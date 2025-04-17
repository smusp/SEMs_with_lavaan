

## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling 
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.


### One-way MANOVA via regression
## Check results with "OLS regression" sections in Table 21.5

## Load package
library(restriktor)   # To test linear hypotheses
library(car)              # To test linear hypotheses 
library(here)             # Relative paths

## Get the data
path = here::here("Green_2023", "data", "satisfactionII.csv")
df <- read.csv(path, header = TRUE)
head(df)

# The variables used in this example are:

# - x - Coping Strategy ("a" - no strategy; "b" - discussion; "c" - exercise) 
# - x1, x2, x3 - dummy coded variables (1, 0) for "Coping Strategy"
# - y1, y2, y3, y4 - multiple dependent variables (life-satisfaction scores)


### Cell means formulation


### OLS regression
## Two models:
##    "Less Constained" model" - the group means differ;
##    "More Constrained" model - group means constrained to equality
#  The multiple dependent variables (y1, y2, y3, y4) are combined into one object Y. 
Y <- with(df, cbind(y1, y2, y3, y4))
models <- list(
   "More Constrained" = "Y ~ 1",
   "Less Constrained" = "Y ~ -1 + x"
)

## Fit the models
fit <- lapply(models, lm, data = df)

## Get the means
#  Compare with the means in Table 21.5. 
#  I'm fairly sure that there is a typo in the "Less constrained" means 
#  reported in Table 21.5
lapply(fit, coef)

## F test
anova(fit[[2]], fit[[1]], test = "Wilks")

# Or using Reduce()
Reduce(function(mc, lc) anova(lc, mc, test = "Wilks"), x = fit)

## SSCP, lambda, F and df by hand
# The formulas for Wilks' lambda and F are given in Table 21.5. 
# The formulas for the degrees of freedom are not given, 
# but are easily located in standard texts.

# Get error SSCP
E <- fit |>
   lapply(residuals) |>
   lapply(function(x) t(x) %*% x)
E

# Get Wilks' lambda
lambda <- Reduce(function(mc, lc) det(lc) / det(mc), x = E); lambda

# Get df1, df2, F and p
k <- 4      # Number of variables
m <- 3      # Number of groups
n <- 200    # Sample size

# A function to do the calculations
pF <- function(k, m, n) {
   df1 = k*(m - 1)

   a = n - m - (k - m + 2)/2
   b = sqrt((k^2 * (m - 1)^2 - 4) / (k^2 + (m - 1)^2 - 5))
   c = (k * (m - 1) - 2) / 2

   df2 = a * b - c

   F = ((1 - sqrt(lambda))/df1) / (sqrt(lambda)/df2)
   p = pf(F, df1, df2, lower.tail = FALSE)
   tab = round(data.frame(
     "lambda" = lambda, "F" = F, "df1" = df1, "df2" = df2, "p" = p), 3)
   return(tab)
}

pF(k, m, n)



### Using dummy variables


## OLS regression
## Two models:
##    "Less Constained" model" - the group means differ;
##    "More Constrained" model - group means constrained to equality
Y <- with(df, cbind(y1, y2, y3, y4))
models <- list(
   "More Constrained" = "Y ~ -1 + I(x1 + x2 + x3)",
   "Less Constrained" = "Y ~ -1 + x1 + x2 + x3"
)

## Fit the models
fit <- lapply(models, lm, data = df)

# Get the means
lapply(fit, coef)

# F test
anova(fit[[2]], fit[[1]], test = "Wilks")


### Alternative - linearHypothesis() function from car package
### Cell means
## Begin with the "Less Constrained" model
lc = lm(Y ~ -1 + x, df)
summary(lc)

# Constrain means to equality
constraints <- c("xa = xb",
                 "xb = xc")

# Apply contraints to "Less Constrained" model
# and compare LC and MC models
linearHypothesis(lc, constraints, test = "Wilks")


### Dummy variables
## Begin with the "Less Constrained" model
lc <- lm(Y ~ -1 + x1 + x2 + x3, df)
summary(lc)

# Constrain means to equality
constraints <- c("x1 = x2",
                 "x2 = x3")

# Apply contraints to "Less Constrained" model
# and compare LC and MC models
linearHypothesis(lc, constraints, test = "Wilks")
