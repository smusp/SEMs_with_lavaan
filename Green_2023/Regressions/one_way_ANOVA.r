

## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.


### One-way ANOVA via regression
## Check results with "OLS regression" sections in Table 21.1

## Load packages
library(restriktor)   # To test linear hypotheses
library(car)          # To test linear hypotheses
library(here)         # Relative paths

## Get the data
path <- here::here("Green_2023", "R", "satisfactionI.r")
source(path)

path <- here::here("Green_2023", "R", "ANOVA_data.r")
source(path)
head(df)

# The variables used in this example are:
#  - x - Coping Strategy ("a" - no strategy; "b" - discussion; "c" - exercise)
#  - y - dependent variable ("after" self-satisfaction scores)
#  - a, b, c - dummy coded variables (1, 0) for "Coping Strategy"


# Two versions of OLS regression are shown:
#  - cell-means formulation
#  - dummy variables formulation
# As well, two alternatives are shown:
#  - using iht() from the restriktor package
#  - using linearHypothesis() from the car package
# each of which can be formulated using cell-means or dummy variables.
# The iht() function returns the F test and the restricted and unrestricted means;
# the linearHypothesis() function returns the F test only.


### Cell means formulation


### OLS regression
## Two models:
##    "Less Constained" model - the group means differ;
##    "More Constrained" model - group means constrained to equality

models <- list(
   "More Constrained" = "y ~ 1",
   "Less Constrained" = "y ~ -1 + x"
)

## Fit the models
fit <- lapply(models, lm, data = df)

## Get summaries
# Check Estimates with means in Table 21.1
lapply(fit, summary)

## Pooled error variances
# There is sufficient information in the analysis summaries
# to obtain the pooled error variance: square the residual standard error.
# However, anova() will give residual sums of squares and degrees of freedom,
# from which the mean square error is obtained (by division),
# which is the pooled error variance.
# Check "Mean sq" with pooled error variances in Table 21.1
aovTable <- lapply(fit, anova); aovTable
ErrorVar <- aovTable |>
   lapply("[", c("Df", "Sum Sq", "Mean Sq")) |>      # Extract df, SS, and MS
   lapply(function(x) x[dim(x)[1], ])                # Extract the last row in each data frame
ErrorVar

# Alternatively, the sigma() function gives error standard deviation;
# square the standard deviation to give the error variance:
# lapply(lapply(fit, sigma), "^", 2)

## R square
# Check with Equation 21.4
Rsquare <- ErrorVar |>
   lapply("[[", "Sum Sq") |>                        # Extract SSE
   Reduce(function(mc, lc)  (mc - lc) / mc, x = _)  # Substitute into Eq 21.4
Rsquare

## F test to compare LC and MC models
# To perform the F test (to compare the fit of the two models),
# one could extract the relevant Sums of Squares and degrees of freedom to calculate F
# (as shown in Table 21.1 and Equation 21.3), but it is easier to apply the
# anova() function to the two models.
# Check with F statistic and p-value in Table 21.1
Reduce(anova, fit)


### Alternatives - using the car and restriktor packages
## Begin with the "Less Constrained" model
lc <- lm(y ~ -1 + x, df)
summary(lc)


## Using iht() from restriktor package
# Constrain means to equality
constraints <- "xa = xb = xc"

# Apply contraints to "Less Constrained" model
# and compare LC and MC models
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual    # df for the F test


## Using linearHypothisis() from car package,
# (will return F test results only - no means)
# Constrain means to equality
constraints <- c("xa = xb",
                 "xb = xc")

# Apply contraints to "Less Constrained" model
# and compare LC and MC models
linearHypothesis(lc, constraints)



### Using dummy variables


### OLS regression
## Two models:
##    "Less Constained" model - the group means differ;
##    "More Constrained" model - group means constrained to equality
models <- list(
   "More Constrained" = "y ~ -1 + I(a + b + c)",
   "Less Constrained" = "y ~ -1 + a + b + c"
)

## Get summaries
# Check Estimates with means in Table 21.1
fit <- lapply(models, lm, df)               # Run the models
lapply(fit, summary)                        # Get the summaries - note the means

## Pooled error variances
# Check "Mean sq" with pooled error variances in Table 21.1
aovTable <- lapply(fit, anova); aovTable    # ANOVA tables - Note: SS and MS for Residuals
ErrorVar <- aovTable |>
   lapply("[", c("Df", "Sum Sq", "Mean Sq")) |>  # Extract df, SS, and MS
   lapply(function(x) x[dim(x)[1], ])            # Extract the last row in each data frame
ErrorVar                                         # Mean Sq is pooled error variance

## R square
# Check with Equation 21.4
Rsquare <- ErrorVar |>
   lapply("[[", "Sum Sq") |>                         # Extract SSE
   Reduce(function(mc, lc)  (mc - lc) / mc, x = _)   # Substitute into Eq 21.4
Rsquare

## F test to compare LC and MC models
# Check with F statistic and p-value in Table 21.1
Reduce(anova, fit)


### Alternatives - using the car and restriktor packages
## Begin with the "Less Constrained" model
lc <- lm(y ~ -1 + a + b + c, df)
summary(lc)


## Using iht() from restriktor package
# Constrain means to equality
constraints <- "a = b = c"

# Apply contraints to "Less Constrained" model
# and compare LC and MC models
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual    # df for F test


## Using linearHypothisis() from car package
# Constrain means to equality
constraints <- c("a = b",
                 "b = c")

# Apply contraints to "Less Constrained" model
# and compare LC and MC models
linearHypothesis(lc, constraints)



