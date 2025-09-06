# One-Way MANOVA


Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation
modeling approaches for analyzing means. In R. Hoyle (Ed.), *Handbook of
structural equation modeling* (2nd ed., pp. 385-408). New York, NY:
Guilford Press.

This example shows the SEM approach to Part 4: One-way MANOVA. Results
are reported in Table 21.5 (p. 399).

The data are described on pages 397 and 398.

#### Load package and get the data

Load the **lavaan** package, and run `satisfactionII.r` to get the data
(`satisfactionII.r` is available at the end of this post).

``` r
library(lavaan)

source("satisfactionII.r")
head(df)
```

The variables used in this example are:

- x - Coping Strategy (“a” - no strategy; “b” - discussion; “c” -
  exercise)
- y1, y2, y3, y4 - Multiple dependent variables (Life-Satisfaction
  scores)

#### The models

The SEM model for the one-way MANOVA is shown in Fig 21.2 (p. 400), and
is reproduced below. The diagram shows the “Less Constrained” model. The
means are represented by the labels on the arrows connecting the “1” to
the dependent variables. The means for each variable are allowed to
differ across the groups. The residual variances and covariances are
constrained to equality.

<img src="images/one_way_MANOVA.svg" data-fig-align="center" />

The model statements are shown below. The “More Constrained” model
constrains the means to equality. The “Less Constrained” model allows
the means to differ across the groups. In both cases the residual
variances and covariances are constrained to equality. The variancs and
covariances can be set up separately - see `vcov` below. Then, `vcov` is
added back into each model. Saves a little typing.

``` r
# Variances and covariances (for both models)
vcov <-
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
```

#### Fit the models and get the results

``` r
## Fit the models 
fit <- lapply(models, sem, data = df, group = "x")

## Get model summaries
lapply(fit, summary)

## Contrast model fits
Reduce(anova, fit)
```

The “SEM” section of Table 21.5 shows the $\upchi$<sup>2</sup> test.

Scroll through the summaries to find the “Intercepts”, or extract them
from the list of estimates of model parameter.

``` r
## Get list of estimates
estimates <- lapply(fit, lavInspect, "est"); estimates

## Extract means - in element "nu"
means <- list()
for (i in names(models)) {
   means[[i]] = estimates[[i]] |>
      sapply("[[", "nu") |>
      round(2)
   row.names(means[[i]]) = c("Y1", "Y2", "Y3", "Y4")
}
means  # Typos among "Less Constrained" means in Table 21.5)
```

Compare with the means in Table 21.5.

By way of completeness, get the error SSCP matrices. (Thompson, Lie &
Green state that, “the error SSCP matrices were perfectly reproduced by
multiplying the variances and covariances in the SEM output by the total
sample size” p. 398).

``` r
# Note: In the list of estimates, co/variances are in element "theta"
E <- estimates |>
  lapply("[[", "a") |>           # Extract estimates for group "a"
  lapply("[[", "theta") |>       # Extract "theta" element
  lapply(matrix, nrow = 4) |>    # Get the full matrix
  lapply("*", 200)               # Multiply by sample size
E
```

#### Relax homogeneity of variances and covariances assumption

Towards the end of the section headed “Avoiding OLS assumptions for
ANOVA/MANOVA designs using SEM” (pp. 398-401), TLG present the results
for models in which the assumptions of homogeneity and normality are
relaxed. That is, variances and covariances are not constrained to
equality, and a robust ML method of estimation (MLM) is employed. Again,
the variances and covariances are set up separately, then added back
into each model. This time, there are no labels for the variances and
covariances, meaning that **lavaan** will estimate each variance and
covariance for each group.

``` r
## Model statements
# Variances and covariances (for both models)
vcov <- 
  "y1 ~~ y1 + y2 + y3 + y4
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

## Fit the models
fit <- lapply(models, sem, data = df, estimator = "mlm", group = "x")

## Get model summaries
lapply(fit, summary)

## Contrast model fits
Reduce(anova, fit)
```

Compare with the $\upchi$<sup>2</sup> test on page 401.

<br />

<details class="code-fold">
<summary>R code with minimal commenting</summary>

``` r
## One-way MANOVA
##
## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.

## Load package
library(lavaan)

## Get the data
source("satisfactionII.r")
head(df)

## The models
# Variances and covariances (for both models)
vcov <-
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

## Fit the models and get the results
## Check means and chi square test in Table 21.5
## Fit the models
fit <- lapply(models, sem, data = df, group = "x")

## Get model summaries
lapply(fit, summary)

## Contrast model fits
Reduce(anova, fit)

## Extract means from list of estimates
## Get list of estimates
estimates <- lapply(fit, lavInspect, "est"); estimates

## Extract means - in element "nu"
means <- list()
for (i in names(models)) {
   means[[i]] = estimates[[i]] |>
      sapply("[[", "nu") |>
      round(2)
   row.names(means[[i]]) = c("Y1", "Y2", "Y3", "Y4")
}
means  # Typos among "Less Constrained" means in Table 21.5)

## Get the error SSCP matrices by hand
# Note: In the list of estimates, co/variances are in element "theta"
E <- estimates |>
  lapply("[[", "a") |>           # Extract estimates for group "a"
  lapply("[[", "theta") |>       # Extract "theta" element
  lapply(matrix, nrow = 4) |>    # Get the full matrix
  lapply("*", 200)               # Multiply by sample size
E

## Relax homogeneity of variances and covariances assumption
## Check chi square on page 401
## Model statements
# Variances and covariances (for both models)
vcov <-
  "y1 ~~ y1 + y2 + y3 + y4
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

## Fit the models
fit <- lapply(models, sem, data = df, estimator = "mlm", group = "x")

## Get model summaries
lapply(fit, summary)

## Contrast model fits
Reduce(anova, fit)
```

</details>

<details class="code-fold">
<summary>R code to get data file - `satisfactionII.r`</summary>

``` r
### Data for Tables 21.5 and 21.6 ###

df <- structure(list(x = c("a", "a", "a", "a", "a", "a", "a", "a", 
"a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", 
"a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", 
"a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", 
"a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", 
"b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", 
"b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", 
"b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", 
"b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", 
"b", "b", "b", "b", "b", "b", "b", "b", "c", "c", "c", "c", "c", 
"c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", 
"c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", 
"c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", 
"c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", 
"c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", 
"c", "c", "c", "c", "c", "c", "c", "c", "c", "c"), a = c(1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L), b = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L), c = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), y1 = c(18L, 24L, 
21L, 24L, 19L, 22L, 23L, 32L, 24L, 22L, 24L, 23L, 28L, 22L, 22L, 
23L, 19L, 28L, 25L, 25L, 27L, 21L, 33L, 24L, 23L, 28L, 29L, 24L, 
28L, 24L, 26L, 28L, 21L, 26L, 20L, 24L, 22L, 32L, 31L, 22L, 22L, 
27L, 22L, 26L, 24L, 24L, 25L, 27L, 26L, 24L, 22L, 18L, 25L, 27L, 
29L, 24L, 22L, 32L, 23L, 27L, 28L, 24L, 18L, 32L, 27L, 25L, 24L, 
25L, 29L, 21L, 29L, 25L, 25L, 25L, 19L, 32L, 29L, 22L, 18L, 26L, 
23L, 26L, 21L, 18L, 24L, 24L, 17L, 24L, 33L, 21L, 23L, 27L, 26L, 
28L, 20L, 27L, 25L, 25L, 25L, 18L, 27L, 25L, 22L, 23L, 26L, 23L, 
29L, 26L, 24L, 27L, 22L, 24L, 26L, 31L, 27L, 22L, 22L, 26L, 25L, 
21L, 26L, 25L, 24L, 26L, 28L, 27L, 26L, 26L, 19L, 22L, 25L, 26L, 
30L, 22L, 26L, 25L, 27L, 32L, 22L, 27L, 26L, 30L, 32L, 28L, 25L, 
23L, 21L, 14L, 26L, 28L, 29L, 25L, 27L, 25L, 26L, 21L, 23L, 25L, 
26L, 30L, 30L, 26L, 22L, 31L, 28L, 26L, 29L, 25L, 26L, 24L, 28L, 
22L, 35L, 26L, 34L, 29L, 26L, 27L, 32L, 16L, 26L, 22L, 25L, 30L, 
28L, 25L, 22L, 23L, 28L, 23L, 36L, 27L, 24L, 23L, 34L, 31L, 33L, 
26L, 27L, 22L), y2 = c(49L, 50L, 51L, 53L, 44L, 50L, 52L, 55L, 
53L, 48L, 48L, 51L, 57L, 51L, 48L, 51L, 48L, 53L, 59L, 55L, 51L, 
54L, 63L, 49L, 54L, 54L, 52L, 47L, 50L, 49L, 54L, 57L, 51L, 53L, 
49L, 53L, 53L, 57L, 58L, 49L, 53L, 55L, 59L, 57L, 55L, 53L, 55L, 
54L, 47L, 54L, 48L, 47L, 50L, 59L, 52L, 52L, 52L, 60L, 59L, 50L, 
55L, 59L, 55L, 59L, 61L, 48L, 55L, 55L, 60L, 50L, 62L, 54L, 56L, 
61L, 52L, 55L, 51L, 56L, 52L, 56L, 53L, 49L, 59L, 51L, 57L, 55L, 
48L, 54L, 56L, 53L, 47L, 54L, 52L, 54L, 50L, 54L, 52L, 54L, 59L, 
54L, 61L, 54L, 54L, 50L, 56L, 51L, 59L, 50L, 52L, 55L, 57L, 57L, 
62L, 55L, 53L, 51L, 50L, 60L, 51L, 52L, 52L, 56L, 52L, 55L, 56L, 
51L, 64L, 54L, 47L, 51L, 54L, 55L, 55L, 55L, 54L, 55L, 58L, 57L, 
56L, 60L, 55L, 54L, 61L, 55L, 50L, 53L, 60L, 49L, 58L, 61L, 55L, 
51L, 58L, 53L, 55L, 49L, 55L, 53L, 56L, 53L, 55L, 53L, 48L, 59L, 
56L, 52L, 55L, 58L, 54L, 54L, 59L, 49L, 60L, 62L, 57L, 59L, 57L, 
61L, 58L, 53L, 56L, 52L, 53L, 55L, 54L, 53L, 49L, 48L, 59L, 55L, 
61L, 59L, 50L, 55L, 58L, 63L, 53L, 56L, 55L, 54L), y3 = c(42L, 
42L, 46L, 39L, 39L, 37L, 38L, 43L, 36L, 37L, 40L, 45L, 46L, 39L, 
39L, 36L, 38L, 43L, 44L, 42L, 37L, 38L, 41L, 40L, 40L, 48L, 41L, 
37L, 42L, 32L, 38L, 43L, 38L, 41L, 45L, 39L, 40L, 41L, 49L, 40L, 
39L, 40L, 41L, 39L, 41L, 43L, 43L, 37L, 38L, 42L, 44L, 36L, 39L, 
44L, 41L, 38L, 40L, 49L, 41L, 39L, 46L, 45L, 40L, 50L, 45L, 43L, 
40L, 42L, 44L, 34L, 42L, 39L, 46L, 39L, 39L, 42L, 41L, 36L, 42L, 
46L, 39L, 39L, 37L, 36L, 42L, 32L, 37L, 43L, 42L, 42L, 46L, 47L, 
42L, 47L, 39L, 36L, 38L, 43L, 38L, 40L, 47L, 42L, 43L, 42L, 44L, 
42L, 45L, 41L, 39L, 45L, 42L, 41L, 46L, 44L, 43L, 38L, 42L, 44L, 
36L, 37L, 45L, 45L, 37L, 41L, 38L, 42L, 42L, 40L, 35L, 46L, 40L, 
42L, 48L, 42L, 42L, 44L, 44L, 48L, 38L, 43L, 42L, 40L, 48L, 39L, 
40L, 32L, 46L, 34L, 45L, 43L, 42L, 38L, 42L, 35L, 46L, 38L, 42L, 
39L, 43L, 43L, 50L, 41L, 42L, 43L, 44L, 35L, 44L, 42L, 41L, 47L, 
48L, 40L, 46L, 44L, 51L, 43L, 39L, 47L, 51L, 37L, 42L, 38L, 37L, 
38L, 43L, 40L, 36L, 40L, 46L, 43L, 50L, 42L, 42L, 40L, 43L, 46L, 
43L, 40L, 42L, 41L), y4 = c(29L, 31L, 34L, 36L, 26L, 30L, 34L, 
38L, 37L, 31L, 37L, 30L, 38L, 26L, 36L, 27L, 30L, 39L, 37L, 35L, 
39L, 33L, 35L, 32L, 34L, 40L, 32L, 31L, 38L, 38L, 34L, 42L, 30L, 
32L, 27L, 33L, 32L, 35L, 40L, 27L, 31L, 35L, 32L, 37L, 38L, 31L, 
29L, 28L, 33L, 35L, 31L, 22L, 34L, 37L, 27L, 33L, 35L, 47L, 30L, 
39L, 38L, 40L, 29L, 43L, 34L, 34L, 32L, 41L, 34L, 33L, 34L, 34L, 
32L, 32L, 30L, 34L, 32L, 38L, 25L, 35L, 34L, 24L, 34L, 33L, 26L, 
31L, 30L, 35L, 37L, 35L, 35L, 40L, 34L, 33L, 28L, 35L, 36L, 35L, 
40L, 34L, 39L, 33L, 28L, 34L, 31L, 29L, 39L, 40L, 35L, 37L, 36L, 
34L, 38L, 33L, 32L, 26L, 33L, 36L, 30L, 25L, 33L, 35L, 35L, 38L, 
36L, 39L, 32L, 34L, 35L, 34L, 36L, 28L, 35L, 30L, 31L, 38L, 35L, 
40L, 31L, 40L, 37L, 32L, 42L, 35L, 34L, 34L, 35L, 23L, 35L, 41L, 
39L, 37L, 34L, 26L, 35L, 34L, 35L, 33L, 31L, 40L, 38L, 32L, 29L, 
37L, 39L, 34L, 35L, 35L, 28L, 40L, 37L, 35L, 40L, 35L, 42L, 40L, 
42L, 37L, 39L, 32L, 38L, 31L, 34L, 39L, 38L, 35L, 32L, 33L, 39L, 
36L, 43L, 36L, 30L, 36L, 42L, 35L, 32L, 32L, 33L, 35L)), class = "data.frame", row.names = c(NA, 
-200L))



head(df)

## x - Coping Strategy (a - No strategy; b - Discussion; c - Exercise)
## y1, y2, y3, y4 - Multiple dependent variables (life-satisfaction scores)
```

</details>
