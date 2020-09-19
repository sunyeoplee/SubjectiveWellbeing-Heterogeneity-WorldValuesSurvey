# WVS-SubjectiveWellbeing-Heterogeneity

We examined heterogeneity in subjective wellbeing by national income (GDP) and by income inequality (Gini) in World Values Survey - Wave 6 data. 

## Installation 
[R](https://cran.r-project.org/bin/windows/base/) - a programming language  
[MLwiN](http://www.bristol.ac.uk/cmm/software/mlwin/download/) - a specialized software for multilevel modeling

## Data
[World Values Survey - Wave 6](http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp)

## Analytic framework
### Complex level-1 variance model: 
This model is an extension of a multilevel model, which commonly models variance at higher levels than level-1. Complex level-1 variance model adds random effects at level-1 variance. It explicitly models level-1 variance instead of assuming homoscedasticity. It assesses whether variance of an outcome varies by a covariate. In R, [R2MLwiN](https://cran.r-project.org/web/packages/R2MLwiN/R2MLwiN.pdf) is used.

### Multiple imputation:
This is regarded as the best missing data imputation approach in many cases. It imputes missing values multiple times to take uncertainty into account. In R, [mice](https://cran.r-project.org/web/packages/mice/mice.pdf) package is used. 

## Publication
(under review)
