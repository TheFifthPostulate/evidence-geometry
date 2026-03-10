# ============================================================
# Evidence Framework v2 (train-fit → val/test-score)
# - per-feature class-conditional likelihoods (EB-ish params)
# - per-row loglik- (l_neg), loglik+ (l_pos), LLR (L), sum (S)
# - pluggable feature weighting (default: mean LLR gap)
# - geometry on weighted L: mu0/mu1, Sigma0, drift v, eigmodes
# - scoring: ||L||, proj_v, maha0, eig coords
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
})

# ---------- utils ----------
.safe_log <- function(x, eps = 1e-12) log(pmax(x, eps))

.inv_cov <- function(S, ridge = 1e-6) {
  S2 <- S + diag(ridge, nrow(S))
  solve(S2)
}

.detect_types <- function(df, features) {
  vapply(df[features], function(col) {
    if (is.numeric(col) || is.integer(col)) "numeric" else "categorical"
  }, character(1))
}

.as_factor_cols <- function(df, cols) {
  df %>% mutate(across(all_of(cols), ~ if (is.factor(.x)) .x else as.factor(.x)))
}

winsorize_vec <- function(x, p = 0.01, na.rm = TRUE) {
  qs <- stats::quantile(x, probs = c(p, 1 - p), na.rm = na.rm, type = 7)
  pmin(pmax(x, qs[[1]]), qs[[2]])
}

winsorize_df <- function(df, cols, p = 0.01) {
  df %>% mutate(across(all_of(cols), ~ winsorize_vec(.x, p = p)))
}

# ============================================================
# 0) Spec object (optional but helps keep things consistent)
# ============================================================

risk_spec <- function(y_col,
                      positive,
                      features = NULL,
                      alpha = 0.5,
                      laplace = 1,
                      numeric_min_sd = 1e-6,
                      ridge = 1e-6,
                      winsor_p = 0.01,
                      weights = FALSE,
                      weight_method = "mean_gap",
                      numeric_candidates = c("gaussian", "lognormal", "gamma"),
                      numeric_val_frac = 0.2,
                      numeric_min_n = 25,
                      llr_cap_quantile = 0.01,
                      mi_nbins = 10) {
  list(
    y_col = y_col,
    positive = positive,
    features = features,
    alpha = alpha,
    laplace = laplace,
    numeric_min_sd = numeric_min_sd,
    ridge = ridge,
    winsor_p = winsor_p,
    weights = weights,
    weight_method = weight_method,
    numeric_candidates = numeric_candidates,
    numeric_val_frac = numeric_val_frac,
    numeric_min_n = numeric_min_n,
    llr_cap_quantile = llr_cap_quantile,
    mi_nbins = mi_nbins
  )
}

validate_spec <- function(df, spec) {
  stopifnot(spec$y_col %in% names(df))
  if (!is.null(spec$features)) {
    missing <- setdiff(spec$features, names(df))
    if (length(missing) > 0) stop("Missing features: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

# ============================================================
# 1) Fit class-conditional models for each feature P(X_i|Y)
#    (Gaussian for numeric; categorical with Laplace smoothing)
# ============================================================

# ============================================================
# Numeric likelihood model selection helpers
# ============================================================

# ============================================================
# Numeric likelihood model selection helpers
# One family per feature, shared across both classes
# ============================================================

.safe_logval <- function(x, floor_log = log(1e-12)) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- floor_log
  pmax(x, floor_log)
}

.split_train_valid <- function(x, y, val_frac = 0.2) {
  ok <- is.finite(x) & !is.na(y)
  x <- x[ok]
  y <- y[ok]
  
  n <- length(x)
  if (n < 8) {
    return(list(
      x_train = x, y_train = y,
      x_valid = x, y_valid = y
    ))
  }
  
  idx <- sample.int(n)
  n_val <- max(3, floor(val_frac * n))
  
  valid_idx <- idx[seq_len(n_val)]
  train_idx <- idx[-seq_len(n_val)]
  
  list(
    x_train = x[train_idx],
    y_train = y[train_idx],
    x_valid = x[valid_idx],
    y_valid = y[valid_idx]
  )
}

.fit_gaussian_model <- function(x, min_sd = 1e-6) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NULL)
  
  mu <- mean(x)
  sdv <- max(sd(x), min_sd)
  
  list(
    family = "gaussian",
    params = list(mean = mu, sd = sdv),
    logpdf = function(z) {
      .safe_logval(dnorm(z, mean = mu, sd = sdv, log = TRUE))
    }
  )
}

.fit_lognormal_model <- function(x, min_sd = 1e-6) {
  x <- x[is.finite(x)]
  if (length(x) < 2 || any(x <= 0)) return(NULL)
  
  lx <- log(x)
  mu <- mean(lx)
  sdv <- max(sd(lx), min_sd)
  
  list(
    family = "lognormal",
    params = list(meanlog = mu, sdlog = sdv),
    logpdf = function(z) {
      out <- rep(log(1e-12), length(z))
      ok <- is.finite(z) & (z > 0)
      vals <- dlnorm(z[ok], meanlog = mu, sdlog = sdv, log = TRUE)
      out[ok] <- .safe_logval(vals)
      out
    }
  )
}

.fit_gamma_model <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 5 || any(x <= 0) || sd(x) < 1e-8) return(NULL)
  
  fit <- tryCatch(
    suppressWarnings(MASS::fitdistr(x, densfun = "gamma")),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  
  shape <- unname(fit$estimate["shape"])
  rate  <- unname(fit$estimate["rate"])
  
  if (!is.finite(shape) || !is.finite(rate) || shape <= 0 || rate <= 0) {
    return(NULL)
  }
  
  list(
    family = "gamma",
    params = list(shape = shape, rate = rate),
    logpdf = function(z) {
      out <- rep(log(1e-12), length(z))
      ok <- is.finite(z) & (z > 0)
      vals <- dgamma(z[ok], shape = shape, rate = rate, log = TRUE)
      out[ok] <- .safe_logval(vals)
      out
    }
  )
}

.fit_family_by_name <- function(x, family, min_sd = 1e-6) {
  switch(
    family,
    gaussian  = .fit_gaussian_model(x, min_sd = min_sd),
    lognormal = .fit_lognormal_model(x, min_sd = min_sd),
    gamma     = .fit_gamma_model(x),
    NULL
  )
}

.score_joint_family <- function(x_train, y_train, x_valid, y_valid,
                                family, pos_label, neg_label,
                                min_sd = 1e-6) {
  model_pos <- .fit_family_by_name(x_train[y_train == pos_label], family, min_sd = min_sd)
  model_neg <- .fit_family_by_name(x_train[y_train == neg_label], family, min_sd = min_sd)
  
  if (is.null(model_pos) || is.null(model_neg)) return(-Inf)
  
  ll_pos <- model_pos$logpdf(x_valid[y_valid == pos_label])
  ll_neg <- model_neg$logpdf(x_valid[y_valid == neg_label])
  
  ll_pos <- ll_pos[is.finite(ll_pos)]
  ll_neg <- ll_neg[is.finite(ll_neg)]
  
  if (length(ll_pos) == 0 || length(ll_neg) == 0) return(-Inf)
  
  mean(ll_pos) + mean(ll_neg)
}

.select_numeric_family_shared <- function(x, y, spec, pos_label, neg_label) {
  ok <- is.finite(x) & !is.na(y)
  x <- x[ok]
  y <- y[ok]
  
  if (length(x) < spec$numeric_min_n) {
    return("gaussian")
  }
  
  split <- .split_train_valid(x, y, val_frac = spec$numeric_val_frac)
  
  fams <- spec$numeric_candidates
  scores <- vapply(
    fams,
    function(fam) .score_joint_family(
      x_train = split$x_train,
      y_train = split$y_train,
      x_valid = split$x_valid,
      y_valid = split$y_valid,
      family = fam,
      pos_label = pos_label,
      neg_label = neg_label,
      min_sd = spec$numeric_min_sd
    ),
    numeric(1)
  )
  
  fams[which.max(scores)]
}

fit_class_models <- function(train_df, spec) {
  validate_spec(train_df, spec)

  y_col <- spec$y_col
  pos_label <- spec$positive

  if (is.null(spec$features)) {
    features <- setdiff(names(train_df), y_col)
  } else {
    features <- spec$features
  }

  y <- train_df[[y_col]]
  if (!is.factor(y)) y <- as.factor(y)
  train_df <- train_df %>% mutate(!!y_col := y)
  
  if (!(pos_label %in% levels(train_df[[y_col]]))) {
    stop("positive label not found in y levels")
  }
  neg_levels <- setdiff(levels(train_df[[y_col]]), pos_label)
  if (length(neg_levels) != 1) {
    stop("Binary Y required. Found levels: ", paste(levels(train_df[[y_col]]), collapse = ", "))
  }
  neg_label <- neg_levels[[1]]

  types <- .detect_types(train_df, features)
  cat_cols <- names(types)[types == "categorical"]
  num_cols <- names(types)[types == "numeric"]

  train_df <- .as_factor_cols(train_df, cat_cols)

  df_pos <- train_df %>% filter(.data[[y_col]] == pos_label)
  df_neg <- train_df %>% filter(.data[[y_col]] == neg_label)

  # Numeric params: one selected family per feature, shared across classes
  num_params <- list()
  if (length(num_cols) > 0) {
    for (cname in num_cols) {
      x_all <- winsorize_vec(train_df[[cname]], p = spec$winsor_p)
      y_all <- train_df[[y_col]]
      
      selected_family <- .select_numeric_family_shared(
        x = x_all,
        y = y_all,
        spec = spec,
        pos_label = pos_label,
        neg_label = neg_label
      )
      
      x_pos <- winsorize_vec(df_pos[[cname]], p = spec$winsor_p)
      x_neg <- winsorize_vec(df_neg[[cname]], p = spec$winsor_p)
      
      pos_model <- .fit_family_by_name(
        x_pos,
        family = selected_family,
        min_sd = spec$numeric_min_sd
      )
      
      neg_model <- .fit_family_by_name(
        x_neg,
        family = selected_family,
        min_sd = spec$numeric_min_sd
      )
      
      # fallback to gaussian if somehow one side fails
      if (is.null(pos_model) || is.null(neg_model)) {
        selected_family <- "gaussian"
        pos_model <- .fit_family_by_name(x_pos, family = selected_family, min_sd = spec$numeric_min_sd)
        neg_model <- .fit_family_by_name(x_neg, family = selected_family, min_sd = spec$numeric_min_sd)
      }
      
      num_params[[cname]] <- list(
        family = selected_family,
        pos = pos_model,
        neg = neg_model
      )
    }
  }

  # Categorical params: categorical distribution per class (smoothed)
  cat_params <- list()
  if (length(cat_cols) > 0) {
    lap <- spec$laplace
    cat_params <- lapply(cat_cols, function(cname) {
      levs <- levels(train_df[[cname]])
      tab_pos <- table(factor(df_pos[[cname]], levels = levs))
      tab_neg <- table(factor(df_neg[[cname]], levels = levs))

      p_pos <- (tab_pos + lap) / (sum(tab_pos) + lap * length(levs))
      p_neg <- (tab_neg + lap) / (sum(tab_neg) + lap * length(levs))

      list(levels = levs, p_pos = as.numeric(p_pos), p_neg = as.numeric(p_neg))
    })
    names(cat_params) <- cat_cols
  }

  list(
    spec = spec,
    features = features,
    types = types,
    num_cols = num_cols,
    cat_cols = cat_cols,
    y_col = y_col,
    pos_label = pos_label,
    neg_label = neg_label,
    num_params = num_params,
    cat_params = cat_params
  )
}

# ============================================================
# 2) Evidence matrix: l_pos, l_neg, LLR L
# ============================================================

loglik_matrices <- function(df, fit, alpha, eps = 1e-12) {
  feats <- fit$features
  out <- df %>% select(all_of(feats))

  # align categorical levels to training
  if (length(fit$cat_cols) > 0) {
    for (cname in fit$cat_cols) {
      levs <- fit$cat_params[[cname]]$levels
      out[[cname]] <- factor(out[[cname]], levels = levs)
    }
  }

  n <- nrow(out)
  d <- length(feats)
  
  l_pos <- matrix(NA_real_, n, d, dimnames = list(NULL, feats))
  l_neg <- matrix(NA_real_, n, d, dimnames = list(NULL, feats))

  # numeric class-conditional logpdf from selected models
  if (length(fit$num_cols) > 0) {
    for (cname in fit$num_cols) {
      x <- out[[cname]]
      
      model_pos <- fit$num_params[[cname]]$pos
      model_neg <- fit$num_params[[cname]]$neg
      
      l_pos[, cname] <- model_pos$logpdf(x)
      l_neg[, cname] <- model_neg$logpdf(x)
    }
  }
  
  # categorical logpmf
  if (length(fit$cat_cols) > 0) {
    for (cname in fit$cat_cols) {
      params <- fit$cat_params[[cname]]
      levs <- params$levels
      idx <- match(as.character(out[[cname]]), levs)

      # unseen/NA -> uniform mass (you can change later)
      ppos <- rep(1 / length(levs), n)
      pneg <- rep(1 / length(levs), n)
      ok <- !is.na(idx)
      ppos[ok] <- params$p_pos[idx[ok]]
      pneg[ok] <- params$p_neg[idx[ok]]

      l_pos[, cname] <- .safe_log(ppos, eps)
      l_neg[, cname] <- .safe_log(pneg, eps)
    }
  }
  
  floor_log <- log(eps)
  l_pos[!is.finite(l_pos)] <- floor_log
  l_neg[!is.finite(l_neg)] <- floor_log
  
  L <- l_pos - l_neg
  L[!is.finite(L)] <- 0
  
  S <- apply(alpha * apply(l_pos, 2, exp) + (1 - alpha) * apply(l_neg, 2, exp), 2, .safe_log, eps)
  t <- l_pos + l_neg
  
  list(
    l_pos = l_pos,
    l_neg = l_neg,
    L = L,
    S = S,
    t = t
  )
}

numeric_family_summary <- function(fit_obj) {
  if (length(fit_obj$fit$num_cols) == 0) return(data.frame())
  
  data.frame(
    feature = fit_obj$fit$num_cols,
    pos_family = vapply(fit_obj$fit$num_cols, function(f) fit_obj$fit$num_params[[f]]$pos$family, character(1)),
    neg_family = vapply(fit_obj$fit$num_cols, function(f) fit_obj$fit$num_params[[f]]$neg$family, character(1)),
    stringsAsFactors = FALSE
  )
}

# ============================================================
# LLR capping / winsorization for stable weighting
# ============================================================

cap_llr_matrix <- function(L, q = 0.01) {
  Lc <- L
  for (j in seq_len(ncol(Lc))) {
    x <- Lc[, j]
    ok <- is.finite(x)
    if (sum(ok) < 5) next
    
    qs <- stats::quantile(x[ok], probs = c(q, 1 - q), na.rm = TRUE, type = 7)
    Lc[ok, j] <- pmin(pmax(x[ok], qs[[1]]), qs[[2]])
  }
  Lc
}

# ============================================================
# Weighting methods
# ============================================================

weights_llr_mean_gap <- function(L, y, positive_label,
                                 nonneg = TRUE,
                                 normalize = TRUE,
                                 cap_q = 0.01,
                                 eps = 1e-12) {
  y <- as.factor(y)
  pos <- positive_label
  neg <- setdiff(levels(y), pos)
  if (length(neg) != 1) stop("Binary y required for weights")
  neg <- neg[[1]]
  
  Lc <- cap_llr_matrix(L, q = cap_q)
  
  L_pos <- Lc[y == pos, , drop = FALSE]
  L_neg <- Lc[y == neg, , drop = FALSE]
  
  w <- colMeans(L_pos) - colMeans(L_neg)
  
  if (nonneg) w <- abs(w)
  
  if (normalize) {
    s <- sum(w)
    if (s < eps) {
      w <- rep(1 / length(w), length(w))
      names(w) <- colnames(L)
    } else {
      w <- w / s
    }
  }
  
  w
}

.mutual_info_disc <- function(x_disc, y) {
  tab <- table(x_disc, y)
  n <- sum(tab)
  if (n == 0) return(0)
  
  pxy <- tab / n
  px <- rowSums(pxy)
  py <- colSums(pxy)
  
  mi <- 0
  for (i in seq_len(nrow(pxy))) {
    for (j in seq_len(ncol(pxy))) {
      if (pxy[i, j] > 0 && px[i] > 0 && py[j] > 0) {
        mi <- mi + pxy[i, j] * log(pxy[i, j] / (px[i] * py[j]))
      }
    }
  }
  mi
}

weights_llr_mutual_info <- function(L, y, positive_label,
                                    normalize = TRUE,
                                    cap_q = 0.01,
                                    nbins = 10,
                                    eps = 1e-12) {
  y <- as.factor(y)
  Lc <- cap_llr_matrix(L, q = cap_q)
  
  w <- numeric(ncol(Lc))
  names(w) <- colnames(Lc)
  
  for (j in seq_len(ncol(Lc))) {
    x <- Lc[, j]
    ok <- is.finite(x) & !is.na(y)
    
    if (sum(ok) < max(10, nbins)) {
      w[j] <- 0
      next
    }
    
    x_ok <- x[ok]
    y_ok <- y[ok]
    
    brks <- unique(stats::quantile(
      x_ok,
      probs = seq(0, 1, length.out = nbins + 1),
      na.rm = TRUE,
      type = 7
    ))
    
    if (length(brks) < 3) {
      w[j] <- 0
      next
    }
    
    x_disc <- cut(x_ok, breaks = brks, include.lowest = TRUE, ordered_result = FALSE)
    w[j] <- .mutual_info_disc(x_disc, y_ok)
  }
  
  if (normalize) {
    s <- sum(w)
    if (s < eps) {
      w <- rep(1 / length(w), length(w))
      names(w) <- colnames(Lc)
    } else {
      w <- w / s
    }
  }
  
  w
}

compute_llr_weights <- function(L, y, fit,
                                method = c("mean_gap", "mutual_info"),
                                cap_q = NULL,
                                nbins = NULL,
                                ...) {
  method <- match.arg(method)
  
  if (is.null(cap_q)) cap_q <- fit$spec$llr_cap_quantile
  if (is.null(nbins)) nbins <- fit$spec$mi_nbins
  
  if (method == "mean_gap") {
    return(weights_llr_mean_gap(
      L = L,
      y = y,
      positive_label = fit$pos_label,
      cap_q = cap_q,
      ...
    ))
  }
  
  if (method == "mutual_info") {
    return(weights_llr_mutual_info(
      L = L,
      y = y,
      positive_label = fit$pos_label,
      cap_q = cap_q,
      nbins = nbins,
      ...
    ))
  }
}

apply_llr_weights <- function(L, w) {
  # columnwise scaling
  sweep(L, 2, w, `*`)
}

# ============================================================
# Weights / Feature-importance utilities (standalone)
# ============================================================

#' Compute LLR weights (feature importance) from either:
#'  - a precomputed evidence matrix L, or
#'  - raw df + fit (computes L via loglik_matrices)
#'
#' Returns a named numeric vector of weights.
compute_llr_weights_any <- function(L = NULL,
                                    df = NULL,
                                    y = NULL,
                                    fit = NULL,
                                    y_col = NULL,
                                    alpha = 0.5,
                                    method = c("mean_gap", "mutual_info"),
                                    normalize = TRUE,
                                    cap_q = NULL,
                                    nbins = NULL,
                                    ...) {
  method <- match.arg(method)
  
  if (is.null(L)) {
    if (is.null(df) || is.null(fit)) {
      stop("Provide either L, or (df + fit) to compute L.")
    }
    ll <- loglik_matrices(df, fit, alpha)
    L <- ll$L
  }
  
  if (is.null(y)) {
    if (!is.null(df) && !is.null(y_col)) {
      y <- df[[y_col]]
    } else if (!is.null(df) && !is.null(fit) && !is.null(fit$y_col)) {
      y <- df[[fit$y_col]]
    } else {
      stop("Provide y, or provide (df + y_col).")
    }
  }
  
  if (is.null(fit) || is.null(fit$pos_label)) {
    stop("fit$pos_label is required to compute weights.")
  }
  
  if (is.null(cap_q)) cap_q <- fit$spec$llr_cap_quantile
  if (is.null(nbins)) nbins <- fit$spec$mi_nbins
  
  w <- switch(
    method,
    mean_gap = weights_llr_mean_gap(
      L = L,
      y = y,
      positive_label = fit$pos_label,
      normalize = normalize,
      cap_q = cap_q,
      ...
    ),
    mutual_info = weights_llr_mutual_info(
      L = L,
      y = y,
      positive_label = fit$pos_label,
      normalize = normalize,
      cap_q = cap_q,
      nbins = nbins,
      ...
    ),
    stop("Unknown method: ", method)
  )
  
  if (is.null(names(w))) names(w) <- colnames(L)
  w
}

#' Convert weights into a tidy "feature importance" table for inspection
weights_to_importance_df <- function(w, top_n = 25) {
  stopifnot(is.numeric(w))
  out <- data.frame(
    feature = names(w),
    weight = as.numeric(w),
    abs_weight = abs(as.numeric(w)),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$abs_weight, decreasing = TRUE), ]
  if (!is.null(top_n)) out <- head(out, top_n)
  rownames(out) <- NULL
  out
}

#' Convenience: compute weights + return top features in one call
feature_importance <- function(L = NULL,
                               df = NULL,
                               y = NULL,
                               fit = NULL,
                               y_col = NULL,
                               method = c("mean_gap"),
                               top_n = 25,
                               normalize = TRUE,
                               ...) {
  w <- compute_llr_weights_any(
    L = L, df = df, y = y, fit = fit, y_col = y_col,
    method = method, normalize = normalize, ...
  )
  weights_to_importance_df(w, top_n = top_n)
}

# ============================================================
# 4) Fit evidence geometry on weighted L
#    - mu0, mu1
#    - Sigma0_inv (for D0)
#    - drift v = mu1 - mu0
#    - eigmodes on overall covariance (train)
# ============================================================


energy_in_subspace <- function(Lw, mu, U) {
  # Lw: n x d
  # mu: length d
  # U:  d x k  (orthonormal columns from eigen())
  centered <- sweep(Lw, 2, mu, "-")
  coords <- centered %*% U
  rowSums(coords^2)
}

fit_evidence_geometry <- function(Lw_train, y_train, positive_label,
                                  ridge = 1e-6,
                                  k_eigen = 2,
                                  pooled = FALSE,
                                  k_energy = k_eigen,
                                  energy_ref = c("pos", "both")) {
  
  energy_ref <- match.arg(energy_ref)
  
  y <- as.factor(y_train)
  pos <- positive_label
  neg <- setdiff(levels(y), pos)
  if (length(neg) != 1) stop("Binary y required")
  neg <- neg[[1]]
  
  L_pos <- Lw_train[y == pos, , drop = FALSE]
  L_neg <- Lw_train[y == neg, , drop = FALSE]
  
  mu1 <- colMeans(L_pos)
  mu0 <- colMeans(L_neg)
  sd1 <- apply(L_pos, 2, sd)
  sd0 <- apply(L_neg, 2, sd)
  
  d <- ncol(Lw_train)
  
  # Drift direction in your v1-compatible style (class-0 standardized)
  v <- colMeans(sweep(sweep(L_pos, 2, mu0, "-"), 2, sd0, "/")) -
    colMeans(sweep(sweep(L_neg, 2, mu0, "-"), 2, sd0, "/"))
  
  # ----- Energy subspaces -----
  # Positive-only covariance eigenmodes
  Sigma_pos <- stats::cov(L_pos) + diag(ridge, d)
  eig_pos <- eigen(Sigma_pos, symmetric = TRUE)
  kp <- min(k_energy, d)
  U_pos <- eig_pos$vectors[, seq_len(kp), drop = FALSE]
  evals_pos <- eig_pos$values[seq_len(kp)]
  
  U_neg <- NULL
  evals_neg <- NULL
  if (energy_ref == "both") {
    Sigma_neg <- stats::cov(L_neg) + diag(ridge, d)
    eig_neg <- eigen(Sigma_neg, symmetric = TRUE)
    kn <- min(k_energy, d)
    U_neg <- eig_neg$vectors[, seq_len(kn), drop = FALSE]
    evals_neg <- eig_neg$values[seq_len(kn)]
  }
  
  # ----- Covariance estimation for d_dist -----
  if (pooled) {
    Sigma <- stats::cov(Lw_train) + diag(ridge, d)
    R <- chol(Sigma)
    
    eig <- eigen(Sigma, symmetric = TRUE)
    k <- min(k_eigen, d)
    eigvecs <- eig$vectors[, seq_len(k), drop = FALSE]
    eigvals <- eig$values[seq_len(k)]
    
    return(list(
      mu0 = mu0,
      mu1 = mu1,
      sd0 = sd0,
      sd1 = sd1,
      pooled = TRUE,
      Sigma = Sigma,
      R = R,
      v = v,
      eigvecs = eigvecs,
      eigvals = eigvals,
      # energy objects
      k_energy = kp,
      U_pos = U_pos,
      evals_pos = evals_pos,
      U_neg = U_neg,
      evals_neg = evals_neg,
      energy_ref = energy_ref,
      positive_label = pos
    ))
  }
  
  # pooled == FALSE: class-specific covariances for d_dist
  Sigma0 <- stats::cov(L_neg) + diag(ridge, d)
  Sigma1 <- stats::cov(L_pos) + diag(ridge, d)
  
  R0 <- chol(Sigma0)
  R1 <- chol(Sigma1)
  
  # Eigenmodes for reporting (your prior convention: pooled reference)
  Sigma_ref <- stats::cov(Lw_train) + diag(ridge, d)
  eig <- eigen(Sigma_ref, symmetric = TRUE)
  k <- min(k_eigen, d)
  eigvecs <- eig$vectors[, seq_len(k), drop = FALSE]
  eigvals <- eig$values[seq_len(k)]
  
  list(
    mu0 = mu0,
    mu1 = mu1,
    pooled = FALSE,
    sd0 = sd0,
    sd1 = sd1,
    Sigma0 = Sigma0,
    Sigma1 = Sigma1,
    R0 = R0,
    R1 = R1,
    v = v,
    eigvecs = eigvecs,
    eigvals = eigvals,
    # energy objects
    k_energy = kp,
    U_pos = U_pos,
    evals_pos = evals_pos,
    U_neg = U_neg,
    evals_neg = evals_neg,
    energy_ref = energy_ref,
    positive_label = pos
  )
}

# ============================================================
# Score risk: uses pooled R if pooled=TRUE,
# else uses R0 for D0 and R1 for D1.
# ============================================================

score_risk <- function(L_pos, L_neg, weights, geom, alpha, eps) {
  
  # ----- Core Evidence Objects -----
  l  <- L_pos - L_neg
  # s  <- apply(alpha * apply(L_pos, 2, exp) + (1 - alpha) * apply(L_neg, 2, exp), 2, .safe_log, eps)
  t <- L_pos + L_neg 
  
  # Apply feature weights to l (LLR)
  Lw <- sweep(l, 2, weights, "*")
  
  # Norm of weighted evidence
  l_norm <- sqrt(rowSums(Lw^2))
  
  # class-0 standardized vector used for proj (diagonal-only scaling)
  z_Lw <- sweep(sweep(Lw, 2, geom$mu0, "-"), 2, geom$sd0, "/")
  
  # ----- Drift projection -----
  proj <- as.numeric(z_Lw %*% geom$v) / as.numeric(t(geom$v) %*% geom$v)
  
  # ----- Mahalanobis distances -----
  centered0 <- sweep(Lw, 2, geom$mu0, "-")
  centered1 <- sweep(Lw, 2, geom$mu1, "-")
  
  if (isTRUE(geom$pooled)) {
    Z0 <- backsolve(geom$R, t(centered0), transpose = TRUE)
    Z1 <- backsolve(geom$R, t(centered1), transpose = TRUE)
  } else {
    Z0 <- backsolve(geom$R0, t(centered0), transpose = TRUE)
    Z1 <- backsolve(geom$R1, t(centered1), transpose = TRUE)
  }
  
  D0 <- colSums(Z0^2)
  D1 <- colSums(Z1^2)
  d_dist <- sqrt(D0) - sqrt(D1)
  
  # ----- Eigenmode projections (reporting axes) -----
  eig_coords <- Lw %*% geom$eigvecs
  colnames(eig_coords) <- paste0("eig_", seq_len(ncol(eig_coords)))
  
  # ----- Energy in positive (and optionally negative) typical subspace -----
  E_pos <- energy_in_subspace(Lw, geom$mu0, geom$U_pos)
  
  E_neg <- NA_real_
  dE <- NA_real_
  if (!is.null(geom$U_neg)) {
    E_neg <- energy_in_subspace(Lw, geom$mu0, geom$U_neg)
    dE <- E_pos - E_neg
  }
  
  # ----- Aggregate totals -----
  l_pos_total <- rowSums(sweep(L_pos, 2, weights, "*"))
  l_neg_total <- rowSums(sweep(L_neg, 2, weights, "*"))
  
  final <- data.frame(
    l_pos  = l_pos_total,
    l_neg  = l_neg_total,
    l      = rowSums(l),
    # s      = rowSums(s),
    t      = rowSums(t),
    proj   = proj,
    d_dist = d_dist,
    l_norm = l_norm,
    E_pos  = E_pos,
    E_neg  = E_neg,
    dE     = dE,
    eig_coords,
    check.names = FALSE
  )
  
  final
}

decompose_eigenmode <- function(eigvecs, feature_names, k = 1, top_n = 10) {
  
  mode_k <- eigvecs[, k]
  
  eigen_decomp <- tibble::tibble(
    feature = feature_names,
    loading = mode_k,
    abs_loading = abs(mode_k)
  ) %>%
    arrange(desc(abs_loading)) %>% head(top_n)
  
  # prop_var <- lambda / sum(lambda)
  # 
  # prop_var_df <- tibble::tibble(
  #   mode = seq_along(lambda),
  #   eigenvalue = lambda,
  #   prop_variance = prop_var,
  #   cum_variance = cumsum(prop_var)
  # )
  # 
  
  eigen_decomp
}

# ============================================================
# 6) One-call fit() that matches your desired pattern
# ============================================================

fit <- function(train_df, spec, k_eigen=2, k_energy=2, energy_ref="pos", weight_args = list()) {
  fit0 <- fit_class_models(train_df, spec)

  # compute train L and weights
  ll_tr <- loglik_matrices(train_df, fit0, spec$alpha)
  L_tr <- ll_tr$L
  y_tr <- train_df[[spec$y_col]]
  
  if (spec$weights) {
    w <- do.call(
      compute_llr_weights,
      c(list(L = L_tr, y = y_tr, fit = fit0, alpha=spec$alpha, method = spec$weight_method), weight_args)
    )
    
    fit0$weights <- w
    
    # weighted evidence for geometry fit
    Lw_tr <- apply_llr_weights(L_tr, w)
    
  } else {
    w <- rep(1, ncol(L_tr))
    fit0$weights <- w
    Lw_tr <- L_tr
  }
  
  if (any(!is.finite(Lw_tr))) {
    bad_cols <- names(which(colSums(!is.finite(Lw_tr)) > 0))
    stop("Non-finite values in weighted evidence matrix. Problematic features: ",
         paste(bad_cols, collapse = ", "))
  }

  geom <- fit_evidence_geometry(
    Lw_train = Lw_tr,
    y_train = y_tr,
    positive_label = fit0$pos_label,
    ridge = spec$ridge,
    k_eigen = k_eigen,
    k_energy = min(k_energy, k_eigen),
    energy_ref = energy_ref
  )

  list(fit = fit0, geom = geom)
}



print_feature_likelihoods <- function(fit_obj,
                                      digits = 4,
                                      top_n_cat = 6) {
  # Accept either:
  #   - fitted model returned by fit()  -> fit_obj$fit
  #   - class-model object returned by fit_class_models()
  fitx <- if (!is.null(fit_obj$fit)) fit_obj$fit else fit_obj
  
  rows <- list()
  
  format_params <- function(model) {
    if (is.null(model)) return(NA_character_)
    
    fam <- model$family
    p <- model$params
    
    if (fam == "gaussian") {
      return(sprintf(
        "mean=%.*f, sd=%.*f",
        digits, p$mean,
        digits, p$sd
      ))
    }
    
    if (fam == "lognormal") {
      return(sprintf(
        "meanlog=%.*f, sdlog=%.*f",
        digits, p$meanlog,
        digits, p$sdlog
      ))
    }
    
    if (fam == "gamma") {
      return(sprintf(
        "shape=%.*f, rate=%.*f",
        digits, p$shape,
        digits, p$rate
      ))
    }
    
    if (fam == "kde") {
      gx <- p$grid_x
      return(sprintf(
        "grid_n=%d, range=[%.*f, %.*f]",
        length(gx),
        digits, min(gx),
        digits, max(gx)
      ))
    }
    
    paste(capture.output(str(p)), collapse = "; ")
  }
  
  fmt_cat_probs <- function(levels, probs, top_n = 6) {
    ord <- order(probs, decreasing = TRUE)
    ord <- head(ord, top_n)
    
    pieces <- sprintf(
      "%s=%.*f",
      levels[ord],
      digits,
      probs[ord]
    )
    
    out <- paste(pieces, collapse = ", ")
    if (length(levels) > top_n) {
      out <- paste0(out, ", ...")
    }
    out
  }
  
  # -----------------------------
  # Numeric features
  # -----------------------------
  if (length(fitx$num_cols) > 0) {
    for (fname in fitx$num_cols) {
      pos_model <- fitx$num_params[[fname]]$pos
      neg_model <- fitx$num_params[[fname]]$neg
      
      rows[[length(rows) + 1]] <- data.frame(
        feature = fname,
        class = "positive",
        feature_type = "numeric",
        likelihood_family = if (!is.null(pos_model)) pos_model$family else NA_character_,
        parameters = format_params(pos_model),
        stringsAsFactors = FALSE
      )
      
      rows[[length(rows) + 1]] <- data.frame(
        feature = fname,
        class = "negative",
        feature_type = "numeric",
        likelihood_family = if (!is.null(neg_model)) neg_model$family else NA_character_,
        parameters = format_params(neg_model),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # -----------------------------
  # Categorical features
  # -----------------------------
  if (length(fitx$cat_cols) > 0) {
    for (fname in fitx$cat_cols) {
      cp <- fitx$cat_params[[fname]]
      
      levs <- cp$levels
      pos_probs <- cp$p_pos
      neg_probs <- cp$p_neg
      
      rows[[length(rows) + 1]] <- data.frame(
        feature = fname,
        class = "positive",
        feature_type = "categorical",
        likelihood_family = "categorical",
        parameters = fmt_cat_probs(levs, pos_probs, top_n = top_n_cat),
        stringsAsFactors = FALSE
      )
      
      rows[[length(rows) + 1]] <- data.frame(
        feature = fname,
        class = "negative",
        feature_type = "categorical",
        likelihood_family = "categorical",
        parameters = fmt_cat_probs(levs, neg_probs, top_n = top_n_cat),
        stringsAsFactors = FALSE
      )
    }
  }
  
  out <- dplyr::bind_rows(rows)
  rownames(out) <- NULL
  out
}

