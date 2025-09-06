# One-Way ANOVA


Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation
modeling approaches for analyzing means. In R. Hoyle (Ed.), *Handbook of
structural equation modeling* (2nd ed., pp. 385-408). New York, NY:
Guilford Press.

Thompson, Liu & Green (TLG) show an ordinary least squares (OLS)
regression approach and a structural equation modeling (SEM) approach
to:

1.  One-way ANOVA
2.  One-way ANCOVA
3.  Two-way ANOVA
4.  One-way MANOVA
5.  One-way ANOVA of a latent variable

This example shows the SEM approach to Part 1: One-way ANOVA. Results
are reported in Table 21.1 (p. 389).

The data are described at the top of page 388. TLG claim that the data
are available in supplementary materials but I’m unable to locate it.
However, the data are available in Mplus data files in supplementary
materials for the 1st edition. I’ve collected the data into a more
convenient format (see the next section for getting the data), but it is
in a “long” format. It needs to be rearranged from “long” to “wide”.

#### Load package and get the data

Load the **lavaan** package, and run `satisfactionI.r` and
`ANOVA_data.r` to get and rearrange the data (`satisfactionI.r` and
`ANOVA_data.r` are available at the end of this post). The data will be
in the `df` data frame.

``` r
library(lavaan)

source("satisfactionI.r")
head(df)

source("ANOVA_data.r")
head(df)
```

The variables used in this example are:

- x - Coping Strategy (“a” - no strategy; “b” - discussion; “c” -
  exercise)
- y - dependent variable (“after” Life-Satisfaction scores)

#### The models

The SEM model for one-way ANOVA is shown in Fig 21.1 (p. 391), and is
reproduced below. The diagram shows the “Less Constrained” model - the
three means, represented by the labels on the arrows connecting the “1”
to the dependent variable, differ. To be consistent with the ANOVA
assumption of homogeneity of variances, the residual variances are
constrained to be equal.

<img src="images/one_way_ANOVA.svg" data-fig-align="center" />

Two models are fitted. The model statements are shown below. The “More
Constrained” model constrains the means (each with the same label “a”)
to equality. The “Less Constrained” model allows the means (represented
by the labels “a1”, “a2”, and “a3”) to differ across the groups.
(Alternatively, this line could have been written as `y ~ 1`; that is,
with no label, the means are freely estimated in each group. I leave the
labels in to be consistent with the diagram of the model.) In both
models the residual variances (each with the same label “e”) are
constrained to equality.

In what follows, I use lists. The model statements are placed into a
list, then I use the `lapply()` or `sapply()` function to perform
operations on each element in the list (such as `sem()` to run the
analyses, or `summary()` to return summaries of the analyses, or `[[` to
extract elements); and I use the `Reduce()` function when I need to
perform operations across the two models (such as `anova()` to constrast
the fit of the two models).

``` r
models <- list(
  "More Constrained" =
    "y ~  c(a, a, a)*1     # Means
     y ~~ c(e, e, e)*y     # Variances",

  "Less Constrained" =
    "y ~  c(a1, a2, a3)*1
     y ~~ c(e, e, e)*y"
)
```

#### Fit the models and get the results

The `lapply()` function applies the `sem()` function to the two elements
of the `models` list (with `data` set to `df`, and `group` set to the
`"x"` variable).

``` r
fit <- lapply(models, sem, data = df, group = "x")

lapply(fit, summary)
```

The “SEM” sections of Table 21.1 show the means, pooled error variances,
and the $\upchi$<sup>2</sup> test.

The summaries show “Intercepts” (that is, the estimated means) and
“Variances” (that is, the error variances) for each “Coping Strategy”
group for both models. Compare with means and pooled error variances in
the SEM section in Table 21.1.

Rather than, or perhaps, as well as, searching through the model
summaries for the means and variances, they can be extracted from a list
of estimates of model parameters.

``` r
## Get list of estimates
estimates <- lapply(fit, lavInspect, "est"); estimates

## Extract means - in element "nu"
means <- list()
for (i in names(models)) {
   means[[i]] <- estimates[[i]] |>
      sapply("[[", "nu")
}
means <- do.call(cbind, means); means

## Extract error variances - in element "theta"
ErrorVar <- list()
for (i in names(models)) {
   ErrorVar[[i]] <- estimates[[i]] |>
      sapply("[[", "theta")
}
ErrorVar <- do.call(cbind, ErrorVar); ErrorVar
```

To perform the $\upchi$<sup>2</sup> test (to compare the fit of the two
models), apply the `anova()` function to the two models.

``` r
Reduce(anova, fit)
```

Compare with the $\upchi$<sup>2</sup> statistic and p value in Table
21.1.

On page 390, TLG give model fit statistics for both models. These are
available in the anova output above, or they can be extracted separately
from the list of fit measures. First, a function to extract the
$\upchi$<sup>2</sup> statistic, degrees of freedom, and the p value,
then that function is applied to both models.

``` r
GetFit <- function(fit) {
   tab <- fitMeasures(fit, c("chisq", "df", "pvalue"))
   tab <- round(tab, 3)
   return(tab)
}

sapply(fit, GetFit)
```

Note: Neither model fits well.

TLG mention the calculation for R<sup>2</sup> (p. 390). The relevant
SSEs can be obtained from the error variances (see `ErrorVar`) by
multiplying error variance by sample size. However, note that
multiplication is not needed because sample size will cancel out; that
is, substitute the error variances into Equation 21.4.

``` r
Rsquare <- ErrorVar["a", ] |>
   Reduce(function(mc, lc) (mc - lc)/mc, x = _)  # Substitute into Eq 21.4
Rsquare
```

#### Relaxing assumption of homogeneity of variances

TLG do not run these models though they make reference to them. The
model statements when the homogeneity of variances assumption is relaxed
are shown below.

``` r
models <- list(
  "More Constrained" =
    "y ~  c(a, a, a)*1         # Means
     y ~~ c(e1, e2, e3)*y      # Variances",

  "Less Constrained" =
    "y ~  c(a1, a2, a3)*1
     y ~~ c(e1, e2, e3)*y"
)
```

Run the models and get the summaries. In this analysis I use the “mlm”
estimator, a robust ML estimator; that is, the normality assumption is
relaxed also.

``` r
fit <- lapply(models, sem, data = df, group = "x", estimator = "mlm")
lapply(fit, summary)
```

This time, the “Less Constrained” model is just identified - a perfect
fit.

<br />

<details class="code-fold">
<summary>R code with minimal commenting</summary>

``` r
## One-way ANOVA
##
## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.

## Load package
library(lavaan)

## Get the data
source("satisfactionI.r")
head(df)

## Rearrange the data file
source("ANOVA_data.r")
head(df)

## The models
models <- list(
  "More Constrained" =
    "y ~  c(a, a, a)*1     # Means
     y ~~ c(e, e, e)*y     # Variances",

  "Less Constrained" =
    "y ~  c(a1, a2, a3)*1
     y ~~ c(e, e, e)*y"
)

## Fit the models
fit <- lapply(models, sem, data = df, group = "x")

## Get model summaries
## Check results with "SEM" section of Table 21.1
lapply(fit, summary)

## Extract means and variances from list of estimates
## Get list of estimates
estimates <- lapply(fit, lavInspect, "est"); estimates

## Extract means - in element "nu"
means <- list()
for (i in names(models)) {
   means[[i]] <- estimates[[i]] |>
      sapply("[[", "nu")
}
means <- do.call(cbind, means); means

## Extract error variances - in element "theta"
ErrorVar <- list()
for (i in names(models)) {
   ErrorVar[[i]] <- estimates[[i]] |>
      sapply("[[", "theta")
}
ErrorVar <- do.call(cbind, ErrorVar); ErrorVar

## Contrast model fits
## Check with chi sq statistic and p value in Table 21.1
Reduce(anova, fit)

## Fit for each model - Chi squares
## Check with values on page 390
## First, a function to extract chi squares
GetFit <- function(fit) {
   tab <- fitMeasures(fit, c("chisq", "df", "pvalue"))
   tab <- round(tab, 3)
   return(tab)
}

sapply(fit, GetFit)

## R square
## Check with Equation 21.4
Rsquare <- ErrorVar["a", ] |>
   Reduce(function(mc, lc) (mc - lc)/mc, x = _)  # Substitute into Eq 21.4
Rsquare

## Relax homogeneity of variances assumption
models <- list(
  "More Constrained" =
    "y ~  c(a, a, a)*1         # Means
     y ~~ c(e1, e2, e3)*y      # Variances",

  "Less Constrained" =
    "y ~  c(a1, a2, a3)*1
     y ~~ c(e1, e2, e3)*y"
)

## Run the model and get the summary
fit <- lapply(models, sem, data = df, group = "x", estimator = "mlm")
lapply(fit, summary)
```

</details>

<details class="code-fold">
<summary>R code to get data file - `satisfactionI.r`</summary>

``` r
### Data for Tables 21.1, 21.2, 21.3, 21.4 ###

df <- structure(list(x = c("a", "a", "a", "a", "a", "a", "a", "a", 
"a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", 
"b", "b", "b", "b", "b", "b", "b", "b", "b", "c", "c", "c", "c", 
"c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", 
"c"), g = c("m", "m", "m", "m", "m", "m", "f", "f", "f", "m", 
"m", "m", "m", "m", "m", "f", "f", "f", "m", "m", "m", "f", "f", 
"f", "m", "m", "m", "f", "f", "f", "m", "m", "m", "f", "f", "f", 
"f", "f", "f", "m", "m", "m", "f", "f", "f", "f", "f", "f"), 
    c = c("before", "before", "before", "before", "before", "before", 
    "before", "before", "before", "after", "after", "after", 
    "after", "after", "after", "after", "after", "after", "before", 
    "before", "before", "before", "before", "before", "after", 
    "after", "after", "after", "after", "after", "before", "before", 
    "before", "before", "before", "before", "before", "before", 
    "before", "after", "after", "after", "after", "after", "after", 
    "after", "after", "after"), y = c(21, 19, 22, 21, 24, 23, 
    21, 24, 23, 22, 22, 24, 25, 27, 30, 22, 23, 24, 23, 23, 21, 
    19, 22, 21, 30, 26, 22, 25, 26, 27, 27, 25, 24, 25, 23, 22, 
    23, 28, 26, 34, 30, 26, 26, 27, 28, 29, 40, 42)), class = "data.frame", row.names = c(NA, 
-48L))


head(df)

## x - Coping Strategy (a - No strategy; b - Discussion; c - Exercise)
## g - Gender
## c - before/after 
## y - dependent variable (Life Satisfaction)
```

</details>

<details class="code-fold">
<summary>R code to rearrange data file - `ANOVA_data.r`</summary>

``` r
### Data for Tables 21.1, 21.2, 21.3, 21.4 ###

## Reshape data - long to wide
tab <- 0.5 * table(df$x)  # in each condition
df$id <- c(rep(1:tab[1], 2), rep(1:tab[2], 2), rep(1:tab[3], 2))  # id variable 

df <- reshape(df, timevar = "c", idvar = c("id", "x", "g"), varying = c("pre", "y"), 
   direction = "wide")

df <- within(df, {
## Grand mean centered "pre" - the before scores
   preC <- scale(pre, scale = FALSE)

## Drop the id variable
   id <- NULL

## Gender X Coping Strategy interaction
  sg <- interaction(x, g, sep = "")

## Dummy variables to use in regression analysis
## Dummy variables for "Coping Startegy"
   dummies1 <- model.matrix(~ x - 1)

## Dummy variables for interaction
   dummies2 <- model.matrix(~ sg - 1)
})

## Unnest the nested 'dummies' matrices, and rename their colomns
df <- do.call(data.frame, df)
names(df) <- gsub("dummies1.x", "", names(df))
names(df) <- gsub("dummies2.sg", "", names(df))
```

</details>
