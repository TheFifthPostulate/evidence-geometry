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
                      alpha=0.5,                      
                      laplace = 1,
                      numeric_min_sd = 1e-6,
                      ridge = 1e-6,
                      winsor_p = 0.01,
                      weights=FALSE,
                      weight_method = "mean_gap") {
  list(
    y_col = y_col,
    positive = positive,
    features = features,
    alpha=0.5,
    laplace = laplace,
    numeric_min_sd = numeric_min_sd,
    ridge = ridge,
    winsor_p = winsor_p,
    weights=weights,
    weight_method = weight_method
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

  # Numeric params
  num_params <- list()
  if (length(num_cols) > 0) {
    
    df_pos_w <- winsorize_df(df_pos, num_cols, p = spec$winsor_p)
    df_neg_w <- winsorize_df(df_neg, num_cols, p = spec$winsor_p)
    
    summ_pos <- df_pos %>%
      summarise(across(all_of(num_cols), list(mu = mean, sd = sd), na.rm = TRUE))
    summ_neg <- df_neg %>%
      summarise(across(all_of(num_cols), list(mu = mean, sd = sd), na.rm = TRUE))

    get_vec <- function(summ, suffix) {
      out <- setNames(numeric(length(num_cols)), num_cols)
      for (c in num_cols) out[[c]] <- summ[[paste0(c, "_", suffix)]]
      out
    }

    num_params <- list(
      mu_pos = get_vec(summ_pos, "mu"),
      sd_pos = pmax(get_vec(summ_pos, "sd"), spec$numeric_min_sd),
      mu_neg = get_vec(summ_neg, "mu"),
      sd_neg = pmax(get_vec(summ_neg, "sd"), spec$numeric_min_sd)
    )
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

  # numeric gaussian logpdf
  if (length(fit$num_cols) > 0) {
    np <- fit$num_params
    for (cname in fit$num_cols) {
      x <- out[[cname]]
      l_pos[, cname] <- .safe_log(dnorm(x, np$mu_pos[[cname]], np$sd_pos[[cname]]))
      l_neg[, cname] <- .safe_log(dnorm(x, np$mu_neg[[cname]], np$sd_neg[[cname]]))
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

  L <- l_pos - l_neg
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

# ============================================================
# 3) Weighting (PLUGGABLE)
#    Default = mean-gap of LLR across classes: w_i ∝ E[L_i|Y=1]-E[L_i|Y=0]
# ============================================================

weights_llr_mean_gap <- function(L, y, positive_label,
                                 nonneg = TRUE,
                                 normalize = TRUE,
                                 eps = 1e-12) {
  y <- as.factor(y)
  pos <- positive_label
  neg <- setdiff(levels(y), pos)
  if (length(neg) != 1) stop("Binary y required for weights")

  L_pos <- L[y == pos, , drop = FALSE]
  L_neg <- L[y == neg, , drop = FALSE]

  w <- colMeans(L_pos) - colMeans(L_neg)

  # if (nonneg) w <- pmax(0, w)
  if (nonneg) w <- abs(w)
  if (normalize) {
    s <- sum(w)
    if (s < eps) {
      # fallback: uniform weights if everything is ~0
      w <- rep(1 / length(w), length(w))
      names(w) <- colnames(L)
    } else {
      w <- w / s
    }
  }
  w
}

compute_llr_weights <- function(L, y, fit,
                                method = c("mean_gap"),
                                ...) {
  method <- match.arg(method)
  if (method == "mean_gap") {
    return(weights_llr_mean_gap(L, y, fit$pos_label, ...))
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
                                    method = c("mean_gap"),
                                    normalize = TRUE,
                                    ...) {
  method <- match.arg(method)
  
  # --- get L ---
  if (is.null(L)) {
    if (is.null(df) || is.null(fit)) {
      stop("Provide either L, or (df + fit) to compute L.")
    }
    ll <- loglik_matrices(df, fit, alpha)  # must return list with $L (your current pattern)
    L <- ll$L
  }
  
  # --- get y ---
  if (is.null(y)) {
    if (!is.null(df) && !is.null(y_col)) {
      y <- df[[y_col]]
    } else if (!is.null(df) && !is.null(fit) && !is.null(fit$y_col)) {
      # optional: if you store y_col in fit
      y <- df[[fit$y_col]]
    } else {
      stop("Provide y, or provide (df + y_col).")
    }
  }
  
  # --- get positive label ---
  if (is.null(fit) || is.null(fit$pos_label)) {
    stop("fit$pos_label is required to compute weights.")
  }
  
  # --- dispatch weighting method ---
  w <- switch(
    method,
    mean_gap = weights_llr_mean_gap(
      L = L,
      y = y,
      positive_label = fit$pos_label,
      normalize = normalize,
      ...
    ),
    stop("Unknown method: ", method)
  )
  
  # enforce names
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

