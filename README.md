# Evidence Geometry  
### An Interpretable Evidence-Based Risk Modeling Framework

Evidence Geometry is an experimental framework for interpretable risk modeling in classification problems.  
Instead of producing a single probability score, it decomposes model predictions into structured evidence signals that reveal how risk emerges in the data.  

The framework transforms heterogeneous features into a **unified log-likelihood ratio evidence space, allowing risk to be analyzed geometrically**.

### Repo Link

[https://github.com/TheFifthPostulate/evidence-geometry/tree/main](https://github.com/TheFifthPostulate/evidence-geometry/tree/main)  

### R Notebooks

**BCW dataset** : [https://thefifthpostulate.github.io/evidence-geometry/bcw_analysis.html](https://thefifthpostulate.github.io/evidence-geometry/bcw_analysis.html)  
**UCI Heart Disease Cleveland** : [https://thefifthpostulate.github.io/evidence-geometry/heartdisease_analysis.html](https://thefifthpostulate.github.io/evidence-geometry/heartdisease_analysis.html)  

---

## Statistical Foundation

Each feature is converted into **log-likelihood ratio evidence**

log p(x \| positive class) − log p(x \| negative class)

This transforms heterogeneous inputs into a unified **evidence space**.

In this space:

• Evidence accumulates additively  
• Population geometry becomes analyzable  
• Case-level risk can be decomposed into interpretable components

This interpretation connects the framework to classical **likelihood ratio testing and Bayesian evidence accumulation**.

---

## Framework Overview

Raw Data  
   ↓  
Per-Feature Likelihood Models  
   ↓  
Log-Likelihood Ratio Evidence  
   ↓  
Evidence Space  
   ↓  
Geometric Risk Signals  

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

## Example Interpretation

A case might produce:  
d_dist = -0.3  
proj = +0.7  
E_pos = moderate  

Interpretation:

- globally closer to the benign population
- but drifting toward pathological structure
- with partial activation of disease-related feature bundles

Such cases are natural candidates for **review triage** rather than automated classification.

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
