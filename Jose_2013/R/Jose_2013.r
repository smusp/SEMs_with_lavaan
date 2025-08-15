## Chapter 3 (Basic Mediation, pp. 43-92) in:
##   Jose, P. (2013). Doing statistical mediation and moderation.
##   New York, NY: Guilford Press.

## Load packages
library(lavaan)
library(haven)   # To read SPSS data files

## Get the data from Guilford Press web site
url <- "http://www.guilford.com/add/jose/mediation_example.sav"
dataset <- data.frame(haven::read_spss(url))

str(dataset)
head(dataset)
summary(dataset)

## The model from Figure 3.3
model <- "
  # direct effect
  shs ~ cpr * ple

  # effects via the mediator
  grat ~ a * ple
  shs  ~  b * grat

  # indirect effect (a*b)
  indirect := a * b

  # total effect
  total := cpr + (a * b)
"

## Fit the model and get the summary
fit <- sem(model, data = dataset)
summary(fit, rsquare = TRUE, standardized = TRUE)   # Check with Tables 3.2-3.5

## To get intercepts
fit_intercepts <- sem(model, data = dataset, meanstructure = TRUE)
summary(fit_intercepts, rsquare = TRUE, standardized = TRUE)

## To get bootstrap CIs
fit_boot <- sem(model, data = dataset, se = "bootstrap", bootstrap = 2000)
summary(fit_boot, standardized = TRUE, ci = TRUE)
parameterEstimates(fit_boot, boot.ci.type = "perc")




