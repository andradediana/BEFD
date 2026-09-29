# Comparison of Business Models on Starbucks' Quarterly Revenues (2009–2025)

Final project for **Business, Economics and Financial Data** (A.Y. 2025/26), Department of Mathematics "Tullio Levi-Civita", University of Padua.

**Lecturer:** Prof. Mariangela Guidolin
**Students:** Diana Andrade (2141895), Mahsa Ekrahi (2141873), Kristina Koleva (2144044)
---

## Overview
This project looks for the best mathematical model to describe and forecast Starbucks' quarterly revenue from 2009 to 2025. Using a range of time-series approaches—including classical models, ARIMA and seasonal methods, as well as innovation-diffusion models such as BM, GBM, and GGM—the study aims to forecast future revenue trajectories, while also considering advertising expenditure as an external factor influencing growth.

### Research questions

1. Which model gives the best solution for the Starbucks revenue series?
2. Does increasing advertising expenses actually help increase revenues?
3. Is fitting the 2020 dip enough to obtain a good model?

### Hypotheses

1. A linear regression model only fits trend and seasonality properly.
2. A GBM with an exponential shock is needed to fit the 2020 dip.
3. ARIMA models give the best solution for a series with an autoregressive structure.
4. GAMs outperform ARIMA thanks to their flexibility, and Prophet handles the dip better through its holiday effects.
5. Advertising expenses help justify growth and the rapid post-pandemic recovery.

---

## Repository contents

| File | Description |
|---|---|
| `FINAL.R` | Full R script: preprocessing, plots, model fitting, diagnostics and comparison |
| `BEFD_-_Project.pdf` | Written report with methodology, results, discussion and conclusions |
| `starbucks.csv` | Quarterly revenue, 2009 Q1 – 2025 Q1 (65 observations, billion USD) |
| `starbucks_advertising.csv` | Yearly advertising spending, 2011–2024 (14 observations, million USD) |
| `statistic_id550719_italy_-consumers-opinion-on-starbucks-positive-aspects-2016.csv` | Survey: positive aspects of Starbucks in Italy (2016) |
| `statistic_id1471037_most-common-reasons-to-love-starbucks-coffee-in-japan-2023.csv` | Survey: reasons to like Starbucks in Japan (2023) |

## Data

All datasets come from [Statista](https://www.statista.com/).

**Quantitative**
- **Quarterly revenue** – Starbucks / U.S. SEC, retrieved 2026. 2025 contains only Q1.
- **Advertising spending** – Starbucks, retrieved 2026. Divided by 1,000 in the script so it matches the revenue scale (billion USD).

**Qualitative** (used for context on brand perception, not for modelling)
- MyGoodMoods (2016), Italy – Wi-Fi (18%) and atmosphere (17%) are the most valued aspects.
- LINE Research (2023), Japan – seasonal menus and fair-trade products are the top reason (41.5%).

---

## Methodology

Models are ranked by **AIC**. Because AIC is unavailable for the non-linear regression models, **MSE** is also reported, and residual diagnostics (residual plots, ACF, PACF) are used to check for overfitting and leftover structure.

### Quarterly data (2009–2025)

| Group | Models |
|---|---|
| Linear regression | Time-series linear model (trend + seasonality) |
| Non-linear regression | Bass Model (BM), Generalized Bass Model (GBM, rectangular and exponential shock), Guseo–Guidolin Model (GGM) |
| ARIMA family | 4 manual ARIMA models, `auto.arima`, SARIMA |
| Short-term forecasting | Simple exponential smoothing, Holt, damped Holt, Holt-Winters (additive/multiplicative), ETS |
| Prophet | Linear vs logistic growth, additive vs multiplicative seasonality, COVID lockdown modelled as a holiday |

### Yearly data (2011–2024, with advertising as an external regressor)

| Group | Models |
|---|---|
| ARIMAX | Advertising spend as exogenous variable |
| GAM | Linear GAM and GAM with smoothing on advertising |
| Prophet | Linear and logistic growth with advertising as an extra regressor |

---

## Key results

### Quarterly models

| Model | AIC | MSE |
|---|---|---|
| TSLM (trend + seasonality) | 99.92 | 0.2264 |
| GBM with exponential shock | – | 0.1203 |
| Auto.ARIMA(0,1,2) | 86.91 | 0.1963 |
| **SARIMA(1,1,1)(1,1,1)[4]** | **82.74** | 0.137 |
| ETS | 142.31 | 0.004 |
| Prophet (linear, additive, lockdown holiday) | – | 0.1458 |

### Yearly models (with advertising)

| Model | AIC | MSE |
|---|---|---|
| auto.ARIMAX(0,1,0) | 58.60 | 3.1079 |
| Linear GAM | 51.33 | 1.2977 |

### Findings

- **Best overall model: SARIMA(1,1,1)(1,1,1)[4].** It has the lowest AIC, a competitive MSE and clean ACF/PACF diagnostics.
- **ETS has the lowest MSE (0.004)** but a high AIC, which points to overfitting rather than a truly better model.
- **Only the GBM with exponential shock** captures the 2020 dip well, but its residuals still show a sinusoidal pattern. No other model fully absorbs the dip.
- **Advertising has at most a small effect.** It is not significant in the ARIMAX model, is marginally significant in the linear GAM (p ≈ 0.026), and has a Prophet coefficient of about 0.05. It does not improve forecasts.
- **Fitting the dip is necessary but not sufficient.** A robust model must capture trend and seasonality without overfitting the single shock event.

See `BEFD_-_Project.pdf` for the full discussion.

---

## Requirements

- R (≥ 4.0 recommended)
- R packages: `DIMORA`, `forecast`, `zoo`, `lmtest`, `readxl`, `plotrix`, `RColorBrewer`, `gam`, `prophet`, `knitr`, `ggplot2`

Install them with:

```r
install.packages(c("DIMORA", "forecast", "zoo", "lmtest", "readxl", "plotrix",
                   "RColorBrewer", "gam", "prophet", "knitr", "ggplot2"))
```

> `prophet` needs a working C++ toolchain (Rtools on Windows, Xcode command line tools on macOS).

## How to run

1. Clone the repository:
   ```bash
   git clone https://github.com/andradediana/BEFD
   cd (https://github.com/andradediana/BEFD
   ```
2. Keep `FINAL.R` and all `.csv` files in the **same folder**. The script reads them with relative paths.
3. Open `FINAL.R` in RStudio, set the working directory to that folder (*Session → Set Working Directory → To Source File Location*), and run the script top to bottom.

The script is organised in sections: **Set up → Data preprocessing → Additional datasets → Models fitting (quarterly) → Yearly models with advertising → Model comparison**.

---

## Limitations

- Only **14 yearly observations** are available for the advertising analysis, so the yearly metrics can be misleading and simpler models are preferable.
- 2025 covers Q1 only.
- The 2020 shock is treated as a one-off event; the models do not forecast any recurrence.
- Model comparison uses in-sample fit (AIC/MSE) and residual diagnostics.

## References

- Guidolin, M. (2025). *Time Series Analysis: ARIMA models* [Lecture slides]. University of Padua.
- Hyndman, R. J., & Athanasopoulos, G. (2018). *Forecasting: Principles and Practice* (2nd ed.). https://otexts.com/fpp2/
- Facebook (2022). *Handling Shocks*. Prophet documentation. https://facebook.github.io/prophet/docs/handling_shocks.html
- Data: Statista – Starbucks quarterly revenue and advertising expenses; MyGoodMoods (2016); LINE Research (2023).
