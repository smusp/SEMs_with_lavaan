

## Kurbanoglu, N. & Takunyaci, M. (2021). A structural equation modeling 
## on relationship between self-efficacy, physics laboratory anxiety
## and attitudes. Journal of Family, Counseling and Education, 6(1), 47-56.


## Load packages
library(lavaan)
library(semmcci)  # For Monte Carlo CIs


## Get the data from Table 1
cor <- c(
   1,
   0.30,  1,
  -0.42, -0.32,  1)
sds <- c(8.81, 7.95, 18.30)
means <- c(56.57, 40.39, 68.22)
n <- 513


## Get the variable names
names <- c("Att", "SE", "Anx")


## Get the variance/covariance matrix
cov <- lavaan::getCov(cor, sds = sds, names = names)


## The model from Figure 1
model <- "
  # direct effect
  Anx ~ cpr * SE   # c prime

  # effects via the mediator
  Att ~ a * SE
  Anx ~ b * Att

  # indirect effect (a * b)
  ab := a * b

  # total effect
  total := cpr + (a * b)
"


## Fit the model and get the summary
fit <- sem(model, sample.cov = cov, sample.nobs = n)
summary(fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)


## To get the intercepts
fit_intercepts <- sem(model, sample.cov = cov, sample.nobs = n, 
   sample.mean = means)
summary(fit_intercepts, rsquare = TRUE, standardized = TRUE)


## To get the Monte Carlo CIs
semmcci::MC(fit, R = 50000, alpha = 0.05)
