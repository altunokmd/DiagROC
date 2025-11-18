# DiagROC
## ROC Analysis with Diagnostic Accuracy Tools for Jamovi

DiagROC is a Jamovi module for ROC analysis combined with diagnostic accuracy metric tools. 

## Included Analyses
- **ROC Analysis**: Run ROC analysis and assess diagnostic accuracy metrics
- **Optimal Cut-off Selection**: Assess diagnostic accuracy metrics for selected cut-off points
- **Manual Calculator**: Assess diagnostic accuracy metrics based on the given contingency table
- **Sample Size Estimation** for Diagnostic Accuracy Studies


## Features
### 1. ROC Analysis
This module performs ROC analysis using the [**`pROC`**](https://github.com/xrobin/pROC) R package. The classification variable must be **dichotomous**. If the variable is coded as "0" and "1" in the dataset, the level "1" is automatically considered as the positive class.

Unless explicitly specified, the pROC package automatically determines the direction of threshold comparison for univariate ROC curves (which is the type of analysis performed in this module), selecting the direction that **maximizes the AUC**. This module adopts that default automatic selection strategy.
According to the pROC documentation [^pROC-manual]:

> in which direction to make the comparison? “auto” (default for univariate curves): automatically define in which group the median is higher and take the direction accordingly. Not available for multivariate curves. “>” (default for multivariate curves): if the predictor values for the control group are higher than the values of the case group (controls > t >= cases). “<”: if the predictor values for the control group are lower or equal than the values of the case group (controls < t <= cases) 

The confidence interval for the AUC is calculated using DeLong's method.

The summary coordinate table presents selected points based on the local maxima method. Alternatively, users may choose to display all available coordinates. For diagnostic accuracy metrics, the threshold corresponding to the maximum Youden index is selected by default.
(If you wish to evaluate diagnostic accuracy at alternative thresholds, you can use the "Optimal Cutoff Selection" menu.)

You may include **multiple dependent (outcome) variables** in the analysis. In this case, a separate ROC analysis will be performed for each variable, and the **optimal cut-off values** and **diagnostic accuracy measures** will be calculated individually.

ROC curves can be displayed either **in a single combined plot** or as separate plots for each variable. If more than one variable is selected, the **AUC comparison option** becomes available.

For plotting, this module uses the [**`ggplot2`**](https://ggplot2.tidyverse.org/) package instead of the native plotting functions of the **`pROC`** package. This allows for full integration with **Jamovi's theme and visual style**.

### 2. Diagnostic Accuracy Tools

Confidence intervals for diagnostic accuracy measures are calculated using the following methods
| **Measure**                                                       | **Confidence Interval Method**                                         |
| ----------------------------------------------------------------- | ---------------------------------------------------------------------- |
| Sensitivity                                                       | Clopper-Pearson exact binomial confidence interval                     |
| Specificity                                                       | Clopper-Pearson exact binomial confidence interval                     |
| Positive Predictive Value (PPV) / Negative Predictive Value (NPV) | Logit transformation with Wald-type confidence interval (Delta method) |
| Positive Likelihood Ratio (PLR) / Negative Likelihood Ratio (NLR) | Log transformation with Wald-type confidence interval (Delta method)   |
| Prevalence                                                        | Clopper-Pearson exact binomial confidence interval                     |
| Accuracy                                                          | Clopper-Pearson exact binomial confidence interval                     |
| Relative Risk                                                     | Log-transformed confidence interval                                    |
| Odds Ratio                                                        | Log-transformed confidence interval                                    |
| Absolute Risk Reduction (ARR)                                     | Wald-type normal approximation                                         |
| Number Needed to Treat (NNT)                                      | Confidence interval derived by inversion of ARR confidence interval    |

### 3. Optimal Cut-off Selection

Begin the analysis by selecting your **index test or exposure variable** and your **reference standard or disease status** variable. The index variable can be either **numerical** or **nominal**. If a nominal variable is selected, it must be **dichotomous**. For dichotomous variables, a **level selector** will become active; for ordinal or continuous numerical variables, a **cut-off configuration box** will be enabled.

When specifying a threshold, first select the direction of comparison, then enter the desired cut-off value. Based on the chosen direction and cut-off, cases that meet the condition will be labeled as **test/exposure positive**, while those that do not will be considered **negative**. A contingency table is then constructed, and diagnostic accuracy metrics are computed accordingly.

You may define multiple cut-off values. In such cases, diagnostic analyses will be performed separately for each threshold.

### 4. Sample Size Estimation

The sample size calculation tool for diagnostic accuracy studies in this module is adapted from the [online calculator](https://turkjemergmed.com/calculator) developed by **Haldun Akoğlu, MD**.
For methodological background, see [Akoğlu (2022)](https://turkjemergmed.com/abstract/811).[^akoglu]


[^pROC-manual]: Robin X, et al. pROC: Display and Analyze ROC Curves. R package version 1.18.5. [https://cran.r-project.org/web/packages/pROC/pROC.pdf](https://cran.r-project.org/web/packages/pROC/pROC.pdf)
[^akoglu]: Akoğlu H. User's guide to sample size estimation in diagnostic accuracy studies. Turkish Journal of Emergency Medicine. 2016;16(2):57–61. [https://doi.org/10.4103/2452-2473.357348](https://doi.org/10.4103/2452-2473.357348)


## Release History

### 0.9.5 (2025-11-18)
- Added "Distance to Top Left" column to the coordinates table.

### 0.9.4 (2025-06-24)
- Initial public release

### Versioning
Public versioning for this module begins at v0.9.x. Earlier versions (v0.1–v0.8) were used during internal development and were not released, as the module had not yet reached a feature-complete state. Starting from v0.9.x reflects that the module is now largely stable and nearing a full v1.0 release.