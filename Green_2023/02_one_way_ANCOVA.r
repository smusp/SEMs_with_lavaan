

## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.


## Load packages
library(lavaan)
library(here)         # Relative paths

## Get the data
path <- here::here("Green_2023", "data", "ANOVA_data.r")
source(path)
head(df)


### One-way ANCOVA - SEM
## Check results with "SEM" section of Table 21.2
models <- list(
"More Constrained" =
  "y ~  c(a, a, a)*1        # Means
   y ~  c(b, b, b)*preC     # Regression slopes
   y ~~ c(e, e, e)*y        # Variances",

"Less Constrained" =
  "y ~  c(a1, a2, a3)*1
   y ~  c(b, b, b)*preC
   y ~~ c(e, e, e)*y"
)


## Fit the models
fit <- lapply(models, sem, data = df, group = "x")

## Get model summaries
lapply(fit, summary)


## Extract means, variances, and regression coefficients from list of estimates
# Get list of estimates
estimates <- lapply(fit, lavInspect, "est"); estimates

# Extract means - in element "alpha"
means <- list()
for (i in names(models)){
   means[[i]] <- estimates[[i]] |>
      lapply("[[", "alpha") |>       # Means for Y and preC
      sapply("[[", 1)                # Means for Y
}
means <- do.call(cbind, means); means

# Extract error variances -  in element "psi"
ErrorVar <- list()
for (i in names(models)){
   ErrorVar[[i]] <- estimates[[i]] |>
      lapply("[[", "psi")  |>        # Extract "psi" element
      sapply("[[", 1, 1)             # 1st row, 1st column of "psi"
}
ErrorVar <- do.call(cbind, ErrorVar); ErrorVar

# Extract regression coefficients -  in element "beta"
RegCoef <- list()
for (i in names(models)){
   RegCoef[[i]] <- estimates[[i]] |>
      lapply("[[", "beta")  |>       # Extract "beta" element
      sapply("[[", 1, 2)             # 1st row, 2nd column of "beta"
}
RegCoef <- do.call(cbind, RegCoef); RegCoef


## Contrast model fits
## Check with chi sq statistic and p value in Table 21.2
Reduce(anova, fit)


## R square
## Check with Equation 21.9
Rsquare <- ErrorVar["a", ] |>
   Reduce(function(mc, lc) (mc - lc)/mc, x = _)  # Substitute into Eq 21.9
Rsquare
