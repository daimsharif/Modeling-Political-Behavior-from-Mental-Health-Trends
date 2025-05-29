# Modeling Political Behavior from Mental Health Trends

This project investigates the relationship between county-level depression prevalence and voting behavior in the U.S. presidential election, focusing on the proportion of Republican votes across states. Developed as part of the Applied Statistical Modeling master's course at Trinity College Dublin, the analysis applies mixed-effects modeling to uncover geographic heterogeneity and assess the statistical significance of depression as a predictor.

---

## 📁 Contents

| File Name                         | Description                                                                 |
|----------------------------------|-----------------------------------------------------------------------------|
| `ASM_MainAssignment_DaimSharif.pdf` | Final report detailing the modeling strategy, results, and key findings.     |
| `Rcode.R`                         | Complete R code for preprocessing, modeling, diagnostics, and visualization.|

---

## 📊 Project Overview

- **Goal**: Determine whether depression rates are associated with increased Republican vote share in U.S. counties.
- **Data**: U.S. county-level dataset including mental health estimates, racial composition, gender ratios, and voting outcomes.
- **Approach**:
  - Transformed key variables to stabilize variance (e.g., log-transformed votes, standardized race ratios).
  - Built weighted linear, logistic, and hierarchical mixed-effects models.
  - Evaluated model performance via AIC/BIC, ANOVA, and diagnostic plots.
  - Visualized state-specific variation in the depression-vote relationship using `ggplot2`.

---

## 📌 Key Insights

- While simple regressions showed a positive correlation between depression and Republican support, the effect diminished after adjusting for race, gender, and population size.
- Racial composition and urbanization emerged as stronger, more consistent predictors.
- Mixed-effects models revealed substantial state-level variation in how depression related to vote patterns.

---

## 🔧 Tools & Packages

- `R`, `lme4`, `ggplot2`, `dplyr`, `tidyr`, `splines`, `sjPlot`, `marginaleffects`, `ggResidpanel`


