

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


### One-way ANOVA - SEM
## Check results with "SEM" section of Table 21.1
models <- list(
  "More Constrained" = 
    "y ~ c(a, a, a)*1      # Means
     y ~~ c(e, e, e)*y     # Variances",

  "Less Constrained" = 
    "y ~ c(a1, a2, a3)*1
     y ~~ c(e, e, e)*y"
)


## Fit the models 
fit <- lapply(models, sem, data = df, group = "x")

## Get model summaries
lapply(fit, summary)


## Get the list of estimates
estimates <- lapply(fit, lavInspect, "est"); estimates    # Means are in element "nu"

## Extract the means
## Check with means in Table 21.1
means <- list()
for (i in names(models)) 
   means[[i]] <- estimates[[i]] |>
      lapply("[[", "nu")  |>       # Extract the means
      unlist()
means


## Extract error variances from estimates - Variances are in element "theta"
## Check with pooled error variances in Table 21.1
ErrorVar <- estimates |>
   lapply("[[", "a") |>           # Extract estimates for group "a"
   lapply("[[", "theta")          # Extract "theta" element
ErrorVar


## Contrast model fits
## Check with chi sq statistic and p value in Table 21.1
Reduce(anova, fit)


## Fit for each model - Chi squares
## Check with values on page 390
## First, a function to extract chi squares
GetFit <- function(fit) {
   tab = fitMeasures(fit, c("chisq", "df", "pvalue"))
   tab = round(data.frame(tab), 3) 
}

lapply(fit, GetFit)


## R square
## Check with Equation 21.4
Rsquare <- ErrorVar |>
   Reduce(function(mc, lc) (mc - lc)/mc, x = _)  # Substitute into Eq 21.4  
c(Rsquare)


## Relax homogeneity of variances assumption
models <- list(
  "More Constrained" = 
    "y ~ c(a, a, a)*1          # Means
     y ~~ c(e1, e2, e3)*y      # Variances",

  "Less Constrained" = 
    "y ~ c(a1, a2, a3)*1
     y ~~ c(e1, e2, e3)*y"
)


## Run the model and get the summary
fit <- lapply(models, sem, data = df, group = "x", estimator = "mlm")
lapply(fit, summary)
