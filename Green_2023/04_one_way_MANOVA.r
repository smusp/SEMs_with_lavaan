

## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling 
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.


## Load package
library(lavaan)
library(here)             # Relative paths

## Get the data
path <- here::here("Green_2023", "data", "satisfactionII.csv")
df <- read.csv(path, header = TRUE)
head(df)


## One-way MANOVA - SEM
# Model statements

# Variances and covariances (for both models)
vcov = 
   "y1 ~~ c(e1, e1, e1)*y1
    y2 ~~ c(e2, e2, e2)*y2
    y3 ~~ c(e3, e3, e3)*y3
    y4 ~~ c(e4, e4, e4)*y4

    y1 ~~ c(e12, e12, e12)*y2
    y1 ~~ c(e13, e13, e13)*y3
    y1 ~~ c(e14, e14, e14)*y4
    y2 ~~ c(e23, e23, e23)*y3
    y2 ~~ c(e24, e24, e24)*y4
    y3 ~~ c(e34, e34, e34)*y4"

models <- list(
"More Constrained" = c(
# Means
   "y1 ~ c(a1, a1, a1)*1
    y2 ~ c(a2, a2, a2)*1
    y3 ~ c(a3, a3, a3)*1
    y4 ~ c(a4, a4, a4)*1",
	
	vcov),
	
"Less Constrained" =  c(
# Means
   "y1 ~ c(a1, b1, c1)*1
    y2 ~ c(a2, b2, c2)*1
    y3 ~ c(a3, b3, c3)*1
    y4 ~ c(a4, b4, c4)*1",
	
	vcov)
)


## Check means and chi square test in Table 21.5
# Fit the models 
fit <- lapply(models, sem, data = df, group = "x")

# Get model summaries
lapply(fit, summary)        # Means are "Intercepts"

# Contrast model fits
Reduce(anova, fit)


## To extrat the means
(estimates <- lapply(fit, lavInspect, "est"))    # Note: means are in element "nu"

means <- list()
for (i in names(models)) { 
   means[[i]] = estimates[[i]] |>
      lapply("[[", "nu") |>
      do.call(cbind, args = _) |>
      t() |>
      round(2)
   row.names(means[[i]]) = c("a", "b", "c")
      }
means


## Get the error SSCP matrices by hand
# Note: In the list of estimates, variances and covariances are in element "theta"
   E = estimates |>
   lapply("[[", "a") |>           # Extract estimates for group "a"
   lapply("[[", "theta") |>       # Extract "theta" element
   lapply(matrix, nrow = 4) |>    # Get the full matrix
   lapply("*", 200)               # Multiply by sample size
E


## Relax homogeneity of variances and covariances assumption
## Check chi square on page 401
# Model statements

# Variances and covariances (for both models)
vcov = "
   y1 ~~ y1 + y2 + y3 + y4
   y2 ~~ y2 + y3 + y4
   y3 ~~ y3 + y4
   y4 ~~ y4"

models <- list(

"Less Constrained" =  c(
# Means
   "y1 ~ c(a1, b1, c1)*1
    y2 ~ c(a2, b2, c2)*1
    y3 ~ c(a3, b3, c3)*1
    y4 ~ c(a4, b4, c4)*1",
	
    vcov),

"More Constrained" = c(
# Means
   "y1 ~ c(a1, a1, a1)*1
    y2 ~ c(a2, a2, a2)*1
    y3 ~ c(a3, a3, a3)*1
    y4 ~ c(a4, a4, a4)*1",
	
	vcov)
)

# Fit the models 
fit <- lapply(models, sem, data = df, estimator = "mlm", group = "x")

# Get model summaries
lapply(fit, summary)

# Contrast model fits
Reduce(anova, fit)
