

## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling 
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.


### One-way ANCOVA via regression
## Check results with "OLS regression" sections in Table 21.2

## Load packages
library(restriktor)   # To test linear hypotheses
library(here)         # Relative paths

## Get the data
path = here::here("Green_2023", "data", "ANOVA_data.r")
source(path)
head(df)

# The variables used in this example are:
#  - x - Coping Strategy ("a" - no strategy; "b" - discussion; "c" - exercise) 
#  - y - dependent variable ("after" self-satisfaction scores) 
#  - preC - pre-score grand mean centered
#  - x1, x2, x3 - dummy coded variables (1, 0) for "Coping Strategy"

# The steps are the same as with the one_way_ANOVA. 
# The only difference is the addition of the covariate, preC.


### Cell means formulation


### OLS regression
## Two models:
##    "Less Constained" model" - the group means differ;
##    "More Constrained" model - group means constrained to equality

models <- list(
   "More Constrained" = "y ~ 1 + preC",
   "Less Constrained" = "y ~ -1 + preC + x"
)

## Fit the models
fit <- lapply(models, lm, data = df)

## Get summaries
# Check Estimates with means in Table 21.2
lapply(fit, summary)

## Pooled error variances
# Check "Mean sq" with pooled error variances in Table 21.2
aovTable <- lapply(fit, anova); aovTable
ErrorVar <- aovTable |> 
   lapply("[", c("Df", "Sum Sq", "Mean Sq")) |>      # Extract df, SS, and MS
   lapply(function(x) x[dim(x)[1], ])                # Extract the last row in each data frame
ErrorVar

## R square
# Check with Equation 21.9
Rsquare <- ErrorVar |>
   lapply("[[", "Sum Sq") |>                        # Extract SSE
   Reduce(function(mc, lc)  (mc - lc) / mc, x = _)  # Substitute into Eq 21.9
Rsquare

## F test to compare LC and MC models
# Check with F statistic and p-value in Table 21.2
Reduce(anova, fit)


### Alternative - the restriktor package only
## Begin with the "Less Constrained" model
lc = lm(y ~ -1 + preC + x, df)
summary(lc)

# Constrain means to equality
constraints <- "xa = xb = xc"

# Apply contraints to "Less Constrained" model
# and compare LC and MC models
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual



### Using dummy variables


### OLS regression
## Two models:
##    "Less Constained" model" - the group means differ;
##    "More Constrained" model - group means constrained to equality
models <- list(
   "More Constrained" = "y ~ -1 + preC + I(x1 + x2 + x3)",
   "Less Constrained" = "y ~ -1 + preC + x1 + x2 + x3"
)

## Get summaries
# Check Estimates with means in Table 21.2
fit <- lapply(models, lm, df)               # Run the models
lapply(fit, summary)                        # Get the summaries - note the means

## Pooled error variances
# Check "Mean sq" with pooled error variances in Table 21.2
aovTable <- lapply(fit, anova); aovTable    # ANOVA tables - Note: SS and MS for Residuals
ErrorVar <- aovTable |> 
   lapply("[", c("Df", "Sum Sq", "Mean Sq")) |>  # Extract df, SS, and MS
   lapply(function(x) x[dim(x)[1], ])            # Extract the last row in each data frame
ErrorVar                                         # Mean Sq is pooled error variance

## R square
# Check with Equation 21.9
Rsquare <- ErrorVar |>
   lapply("[[", "Sum Sq") |>                         # Extract SSE
   Reduce(function(mc, lc)  (mc - lc) / mc, x = _)   # Substitute into Eq 21.9
Rsquare

## F test to compare LC and MC model
# Check with F statistic and p-value in Table 21.2
Reduce(anova, fit)


### Alternative - the restriktor package
## Begin with the "Less Constrained" model
lc <- lm(y ~ -1 + preC + x1 + x2 + x3, df)
summary(lc)

# Constrain means to equality
constraints <- "x1 = x2 = x3"

# Apply contraints to "Less Constrained" model 
# and compare LC and MC models
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual

