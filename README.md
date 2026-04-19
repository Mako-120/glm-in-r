# Learning GLM in R — Frequency-Severity Pricing Model

A practical implementation of Generalized Linear Models (GLM) in R for motor insurance pricing, demonstrating Poisson regression (claim frequency) and inverse Gaussian regression (claim severity) with model calibration and lift analysis.

---

## Overview

This script demonstrates the **frequency-severity pricing framework** using base R's `glm()` function, a cornerstone technique in actuarial pricing and risk modeling.

---

## Models

### Frequency Model — Poisson GLM

```r
glm(ClaimNb ~ Gender + DrivAge + VehYear + Area + offset(log(ExposTotal)),
    family = poisson(link = "log"),
    data = train_data)
```

**Formula interpretation:**
- **Response:** `ClaimNb` (claim count)
- **Predictors:** Gender, driver age, vehicle year, area
- **Offset:** `log(ExposTotal)` adjusts for varying exposure periods
- **Link:** log (standard for count data)
- **Family:** Poisson (variance = mean)

**Use case:** Predicts the expected number of claims per policy-year.

### Severity Model — Inverse Gaussian GLM

```r
glm(ClaimAmount ~ Gender + DrivAge + VehYear + Area,
    family = inverse.gaussian(link = "log"),
    data = subset(train_data, ClaimAmount > 0),
    mustart = rep(avg_claim, ...))
```

**Key details:**
- **Subset:** Fitted only on policies with `ClaimAmount > 0` (conditional on claim)
- **Family:** Inverse Gaussian (for right-skewed positive continuous data)
- **Link:** log (suitable for heteroscedastic severity)
- **Initialization:** `mustart` provides starting values for numerical stability

**Use case:** Predicts the expected claim amount conditional on a claim occurring.

---

## Data & Preprocessing

```r

**Source:** [brevhins-B.RData] I found it long ago on `someone's` github repository and `can't` find it now. Please `don't` sue me.

load('brevhins-B.RData')

# Filter: exposure > 0, valid gender, non-missing age
ds <- ds[ds$ExposTotal > 0 & ds$Gender %in% c("Female", "Male"), ]
ds <- ds[!is.na(ds$DrivAge), ]

---

## Practical Notes

### Offset Interpretation
The offset `log(ExposTotal)` means:
```
E[ClaimNb] = ExposTotal · exp(β₀ + β₁Gender + ...)
```

A 6-month policy (ExposTotal = 0.5) has half the expected claims of a full-year policy, all else equal.

### Why Inverse Gaussian for Severity?
- More flexible than Gamma for right-skewed data
- Allows numerical stability via `mustart`
- Standard in European actuarial practice (Solvency II)
