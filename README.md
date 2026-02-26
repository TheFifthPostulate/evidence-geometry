# Evidence Geometry  
### An Interpretable Evidence-Based Risk Modeling Framework

Evidence Geometry is an interpretable classification framework that transforms heterogeneous features into a unified **log-likelihood ratio (evidence) space**, enabling transparent and decomposed risk analysis.

Instead of producing opaque probability scores, this framework exposes distinct, explainable risk signals for each case.

---

## Core Idea

1. Convert all features (continuous, categorical, binary) into per-feature log-likelihood ratios (evidence).
2. Learn class-conditional geometry in evidence space.
3. Decompose case-level risk into interpretable components.

This approach preserves per-feature likelihood structure while enabling unified geometric analysis.

---

## Interpretable Risk Signals

For each case, the model computes:

### 1️⃣ Distance Contrast (`d_dist`)
Difference in Mahalanobis distance relative to learned class manifolds.  
→ Measures which class explains the case better in a covariance-aware way.

---

### 2️⃣ Drift Projection (`proj`)
Projection onto the learned mean deviation separation direction.  
→ Captures accumulation of marginal evidence deviations toward the positive class.

---

### 3️⃣ Principal Component Bundle Energy (`E_pos`)
Sum of squared projections onto dominant positive-class eigenmodes.  
→ Measures activation of characteristic positive-class feature bundles.

---

Together, these signals capture complementary modes of risk instead of collapsing everything into a single probability score.

---

## Demonstrated On

- Wisconsin Breast Cancer (BCW)
- UCI Heart Disease (Cleveland)

Across both datasets, the framework:

- Supports interpretable triage
- Reduces false negatives relative to baseline discriminative models
- Maintains automation rate via multi threshold-based review policies

---

## Repository Contents

- Fully executable R notebooks  
- End-to-end train/validation/test workflow  
- Feature-level decomposition utilities  
- Principal Component analysis tools  
- Modular likelihood specification framework

**Core Dependencies:** R (4.1 or greater), dplyr, tibble
**Required Packages for Notebooks:** dplyr, ggplot2, caret, patchwork, tibble

---

## Why Not Just Random Forest?

Purely discriminative classifiers produce probability scores without exposing internal structure.

Evidence Geometry:

- Preserves per-feature likelihood information  
- Separates marginal deviation signals from covariance structure  
- Enables threshold-based triage policies grounded in reasoning  
- Provides feature bundle interpretation via eigenmodes  

This makes it particularly suitable for risk-sensitive domains where interpretability and auditability matter.

---

## Status

Prototype v0.1  
Actively expanding to larger clinical datasets (e.g., MIMIC/eICU subsets)

---
