

## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling 
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.


## Load packages
library(lavaan)
library(here)         # Relative paths

## Get the data
path = here::here("Green_2023", "data", "ANOVA_data.r")
source(path)
head(df)


### One-way ANCOVA - SEM
## Check results with "SEM" section of Table 21.2
models <- list(
"More Constrained" = 
  "y ~ c(a, a, a)*1         # Means
   y ~ c(b, b, b)*preC      # Covariate
   y ~~ c(e, e, e)*y        # Variances",

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
## Check with means in Table 21.2
means <- list()
for (i in names(models)) {
   means[[i]] <- estimates[[i]] |>
      lapply("[[", "alpha") |>        # Means for Y and preC
      lapply("[[", 1) |>              # Means for Y
      unlist()
   }   
means


## Extract error variances from estimates - Variances are in element "psi"
## Check with pooled error variances in Table 21.2
ErrorVar <- estimates |>
   lapply("[[", "a") |>          # Extract group "a" estimates
   lapply("[[", "psi")  |>       # Extract "psi" element
   lapply("[[", 1, 1)            # 1st column, 1st row of "psi"
ErrorVar


## Contrast model fits
## Check with chi sq statistic and p value in Table 21.2
Reduce(anova, fit)


## R square
## Check with Equation 21.9
Rsquare <- ErrorVar |>
   Reduce(function(mc, lc) (mc - lc)/mc, x = _)  # Substitute into Eq 21.9  
c(Rsquare)
