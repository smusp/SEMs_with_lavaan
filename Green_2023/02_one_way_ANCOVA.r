

## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling 
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.


## Load packages
library(lavaan)
library(restriktor)   # to restrict means

## Get the data
source("./data/ANOVA_data.r")
head(df)


### One-way ANCOVA: OLS regression
## Cell means formulation
models <- list(
   "More Constrained" = "y ~ 1 + preC",
   "Less Constrained" = "y ~ -1 + preC + x"
)


## Fit the models
fit <- lapply(models, lm, data = df)

## Get summaries
lapply(fit, summary)


## Pooled error variances
aovTable <- lapply(fit, anova); aovTable
ErrorVar <- aovTable |> 
   lapply("[", c("Df", "Sum Sq", "Mean Sq")) |>      # extract df, SS, and MS
   lapply(function(x) x[dim(x)[1], ])                # extract the last row in each data frame
ErrorVar


## F test to compare fit for LC and MC model
Reduce(anova, fit)


## R square
Rsquare <- ErrorVar |>
   lapply("[[", "Sum Sq") |>                        # Extract SSE
   Reduce(function(mc, lc)  (mc - lc) / mc, x = _)  # Substitute into Eq 21.9
Rsquare


### One-way ANOVA - OLS regression
##  Using dummy variables
models <- list(
   "More Constrained" = "y ~ -1 + preC + I(x1 + x2 + x3)",
   "Less Constrained" = "y ~ -1 + preC + x1 + x2 + x3"
)


## Fit the models and get the results
fit <- lapply(models, lm, df)               # Run the models
lapply(fit, summary)                        # Get the summaries - note the means
aovTable <- lapply(fit, anova); aovTable    # anova tables - Note: SS and MS for Residuals

ErrorVar <- aovTable |> 
   lapply("[", c("Df", "Sum Sq", "Mean Sq")) |>  # extract df, SS, and MS
   lapply(function(x) x[dim(x)[1], ])            # extract the last row in each data frame
ErrorVar                                         # Mean Sq is pooled error variance

Reduce(anova, fit)                    # F test to compare the two fits

Rsquare <- ErrorVar |>
   lapply("[[", "Sum Sq") |>                         # Extract SSE
   Reduce(function(mc, lc)  (mc - lc) / mc, x = _)   # Substitute into Eq 21.9
Rsquare


### One-way ANOVA - OLS regression
## Use the restriktor package - cell means
lc <- lm(y ~ -1 + preC + x, df)   # Less Constrained model
summary(lc)

constraints <- "xa == xb
                xb == xc"  # constrain the means to equality
               
# Compare the fit for the two models           
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test

test$df; test$df.residual


## Use the restriktor package - dummy variables
lc <- lm(y ~ -1 + preC + x1 + x2 + x3, df)  # Less Constrained model
summary(lc)

constraints <- "x1 == x2
                x2 == x3"  # constrain the means to equality
                
# Compare the fit for the two models                 
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test

test$df; test$df.residual


### One-way ANCOVA - SEM
models <- list(
"More Constrained" = 
  "y ~ c(a, a, a)*1           # Means
   y ~ c(b, b, b)*preC        # Covariate
   y ~~ c(e, e, e)*y",        # Variances

"Less Constrained" = 
  "y ~ c(a1, a2, a3)*1
   y ~ c(b, b, b)*preC
   y ~~ c(e, e, e)*y"
)


## Fit the models 
fit <- lapply(models, sem, data = df, group = "x")

## Get model summaries
lapply(fit, summary)


## Get the list of estimates
estimates <- lapply(fit, lavInspect, "est"); estimates    # Means are in element "alpha"

## Extract the means
means <- list()
for (i in names(models)) {
   means[[i]] <- estimates[[i]] |>
      lapply("[[", "alpha") |>        # Means for Y and preC
      lapply("[[", 1) |>              # Means for Y
      unlist()
   }   
means


## Extract error variances from estimates - Variances are in element "psi"
ErrorVar <- estimates |>
   lapply("[[", "a") |>          # Extract group "a" estimates
   lapply("[[", "psi")  |>       # Extract "psi" element
   lapply("[[", 1, 1)            # 1st column, 1st row of 'psi'
ErrorVar


## Contrast model fits
Reduce(anova, fit)


## R square
Rsquare <- ErrorVar |>
   Reduce(function(mc, lc) (mc - lc)/mc, x = _)  # Substitute into Eq 21.9  
c(Rsquare)
