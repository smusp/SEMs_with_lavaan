

## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.


### Two-way ANOVA via regression
## Check results with "OLS regression" sections in Table 21.4.
## To get Table 21.3 results, see the SEM formulation.

## Load packages
library(restriktor)   # To test linear hypotheses 
library(here)         # Relative paths

## Get the data
path <- here::here("Green_2023", "R", "satisfactionI.r")
source(path)

path <- here("Green_2023", "R", "ANOVA_data.r")
source(path)
head(df)

# The variables used in this example are:
#  - x - Coping Strategy ("a" - no strategy; "b" - discussion; "c" - exercise)
#  - g - Gender
#  - y - dependent variable ("after" self-satisfaction scores)
#  - sg - Gender X Coping Strategy interaction
#  - af, bf, cf, ... cm - dummy coding for interaction

## Cell frequencies
#  Assists in constructing constraints - see below
freq <- table(df$g, df$x); freq


### Overview - using the restriktor package
# The "Less Constrained" model allows the six means in the 
# Gender X Coping Strategy interaction to differ.
# The "Less Constrained" model is set up using the `lm()` function.
# "More Constrained" models constrain the means in such a way that,
# when the fit of a "More Constrained" model is compared to the fit
# of the "Less Constrained" model, it is possible to test for the
# "Coping Strategy" main effect, or for the "Gender" main effect,
# or for the Gender X Coping Strategy interaction. These effects can
# be tested for unweighted means or for weighted means. TLG briefly
# discuss when to use weighted or unweighted means.


### Cell means formulation

## Less Constrained model
lc <- lm(y ~ -1 + sg, df)
summary(lc)


## More constrained models

## Gender main effect - unweighted means
# Mean for females equals mean for males
# (There are three means for females, and three means for males.
#  Add the three female means and add the three male means.)
constraints <- "sgaf + sgbf + sgcf = sgam + sgbm + sgcm"
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual


## Coping Strategy main effect - unweighted means
# Mean for "a" strategy equals mean for "b" strategy; and
# Mean for "b" strategy equals mean for "c" strategy
constraints <- "sgaf + sgam = sgbf + sgbm = sgcf + sgcm"

test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual


## Gender main effect - weighted means
# Mean for females equals mean for males;
# Cell means weighted in proportion to cell frequencies
freq         # Cell frequencies to construct constraints
constraints <- "3/12*sgaf + 3/12*sgbf + 6/12*sgcf = 6/12*sgam + 3/12*sgbm + 3/12*sgcm"
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual


## Coping Strategy main effect - weighted means
# Check with Table 21.4
# Mean for "a" strategy equals mean for "b" strategy; and
# Mean for "b" strategy equals for "c" strategy;
# Cell means weighted in proportion to cell frequencies
freq         # cell frequencies to construct constraints
constraints <- "3/9*sgaf + 6/9*sgam = 3/6*sgbf + 3/6*sgbm = 6/9*sgcf + 3/9*sgcm"
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual


## Interaction: Gender X Coping Strategy
# The difference between "female" mean and "male" mean for the "a" strategy equals 
# the difference between "female" mean and "male" mean for the "b" strategy; and
# the difference between "female" mean and "male" mean for the "b" strategy equals 
#  the difference between "female" mean and "male" mean for the "c" strategy
constraints <- "sgaf - sgam = sgbf - sgbm = sgcf - sgcm"
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual



### Using dummy variables

## Less constrained model
lc <- lm(y ~ -1 + af + bf + cf + am + bm + cm, df)
summary(lc)


## More constrained models
## Gender main effect - unweighted means
constraints <- "af + bf + cf = am + bm + cm"
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual

## Coping Strategy main effect - unweighted means
constraints <- "af + am = bf + bm = cf + cm"
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual

## Gender main effect - weighted means
freq
constraints <- "3/12*af + 3/12*bf + 6/12*cf = 6/12*am + 3/12*bm + 3/12*cm"
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual

## Coping Strategy main effect - weighted means
# Check with Table 21.4
constraints <- "3/9*af + 6/9*am == 3/6*bf + 3/6*bm
                3/6*bf + 3/6*bm == 6/9*cf + 3/9*cm"
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual

## Interaction: Gender X Coping Strategy
constraints <- "af - am = bf - bm = cf - cm"
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test
test$df; test$df.residual
