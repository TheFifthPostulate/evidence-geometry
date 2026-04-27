# Evidence Geometry  
### Likelihood-Based Feature Transformation for Tabular Data

This repository demonstrates a likelihood-based approach for transforming heterogeneous features into a **common, comparable representation** using log-likelihood ratios (LLR).

The goal is to standardize feature values relative to outcome-specific distributions and enable **interpretable analysis and modeling across datasets**.

---
### Repo Link

[https://github.com/TheFifthPostulate/evidence-geometry/tree/main](https://github.com/TheFifthPostulate/evidence-geometry/tree/main)  

### R Notebooks

**BCW dataset** : [https://thefifthpostulate.github.io/evidence-geometry/bcw_analysis.html](https://thefifthpostulate.github.io/evidence-geometry/bcw_analysis.html)  
**UCI Heart Disease Cleveland** : [https://thefifthpostulate.github.io/evidence-geometry/heartdisease_analysis.html](https://thefifthpostulate.github.io/evidence-geometry/heartdisease_analysis.html)  

---

## Overview

For each feature, values are transformed into a log-likelihood ratio:

`log p(x | positive) − log p(x | negative)`

This converts raw inputs into a **common evidence scale**, allowing:

- comparison across heterogeneous variables  
- additive aggregation of feature contributions  
- analysis of samples relative to class-specific distributions  


---

## Derived Signals

The transformed representation can be summarized using complementary signals:

### `d_dist`
Difference in Mahalanobis distance to class-specific distributions  
→ captures relative proximity under covariance structure  

### `proj`
Projection onto the primary separation direction  
→ captures directional accumulation of feature-level deviations  

These provide alternative views of data structure beyond a single prediction score.
---

## Datasets

The framework is demonstrated on:

- Breast Cancer Wisconsin (BCW)  
- UCI Heart Disease (Cleveland)  

These examples illustrate:

- transformation of raw features into a comparable representation  
- extraction of interpretable summary signals  
- applicability across labeled datasets  
---

## Repository Contents

- R notebooks with end-to-end workflows  
- feature transformation utilities (LLR-based)  
- signal extraction and visualization tools  


## Dependencies

install.packages(c(
  "dplyr", "ggplot2", "ranger", "caret",
  "MASS", "patchwork"
))  

---

  
