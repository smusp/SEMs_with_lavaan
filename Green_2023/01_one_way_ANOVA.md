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

This example shows the approaches to Part 1: One-way ANOVA. Results are
reported in Table 21.1 (p. 389).

The data (`satisfactionI.csv` in the `data` folder) are described at the
top of page 388. The data file needs manipulation before it can be used:
the format needs to be changed from “long” to “wide”; and dummy
variables need to be set up for the groups. These manipulations are
completed in `ANOVA_data.r`.

<br />

#### Load relevant packages and get the data

Load the **lavaan** package (and the **restriktor** package - more
later), and run `ANOVA_data.r` to get the data. The data will be in the
`df` data frame.

``` r
library(lavaan)
library(restriktor)   # to restrict means

source("./data/ANOVA_data.r")
head(df)
```

The variables used in this example are:

- x - Coping Strategy (“a” - no strategy; “b” - discussion; “c” -
  exercise)
- y - dependent variable (“after” self-satisfaction scores)
- x1, x2, x3 - dummy coded variables (1, 0) for “Coping Strategy”

<br />

### OLS Regression

Three versions of OLS regression are shown below:

- cell-means formulation
- using dummy variables
- using the **restriktor** package

In each case, there are two models: a “Less Constrained” model in which
the means are allowed to differ; and a “More Constrained” model in which
the means are constrained to equality.

In what follows, I use lists. The model statements are placed into a
list, then I use the `lapply()` function to perform operations on each
element in the list (such as `lm()` to run the regression analyses, or
`summary()` to return summaries of the analyses, or `[[` to extract
elements); and I use the `Reduce()` function when I need to perform
operations across the two models (such as `anova()` to constrast the fit
of the two models).

<br />

### Regression using cell-means formulation

#### The models

Model statements (as formulas) for the “More Constrained” and “Less
Contrained” models are shown below.

``` r
models <- list(
   "More Constrained" = "y ~ 1",
   "Less Constrained" = "y ~ -1 + x"
)
```

#### Fit the models and get the results

Fit the models by applying `lm()` to each model statement (with `data`
set to `df`). Similarly, get analysis summaries by applying `summary()`
to each analysis.

``` r
fit <- lapply(models, lm, data = df)

lapply(fit, summary)
```

The “OLS regression” sections of Table 21.1 show the means, pooled error
variances, and the F test.

The “Less Constrained” summary shows the “Estimates” (that is, the
estimated means) for each “Coping Strategy” group, while the “More
Constrained” summary shows one estimated mean, that is, the overall
mean, but it is also the estimated mean that would apply to each group
if the three means are constrained to equality. Compare the “Estimates”
with the means in Table 21.1.

There is sufficient information in the analysis summaries to obtain the
pooled error variance: square the residual standard error. However,
`anova()` will give residual sums of squares and degrees of freedom,
from which the mean square error is obtained (by division), which is the
pooled error variance.

``` r
aovTable <- lapply(fit, anova); aovTable
ErrorVar <- aovTable |> 
   lapply("[", c("Df", "Sum Sq", "Mean Sq")) |>      # extract df, SS, and MS
   lapply(function(x) x[dim(x)[1], ])                # extract the last row in each data frame
ErrorVar
```

Compare “Mean Sq” with the pooled error variances in Table 21.1.

(Alternatively, the `sigma()` function gives error standard deviation;
square the standard deviation to give the error variance:
`lapply(lapply(fit, sigma), "^", 2)`)

To perform the F test (to compare the fit of the two models), one could
extract the relevant Sums of Squares and degrees of freedom to calculate
F (as shown in Table 21.1 and Equation 21.3), but it is easier to apply
the `anova()` function to the two models.

``` r
Reduce(anova, fit)
```

Compare with the F statistic and p-value in Table 21.1.

Finally, TLG calculate R<sup>2</sup> (see Equation 21.4) to estimate the
strength of the relationship between “Coping Strategy” and the dependent
variable. The relevant Sums of Squares have already been calculated (in
`ErrorVar`); they just need to be extracted, then combined across the
two models according to Equation 21.4.

``` r
Rsquare <- ErrorVar |>
   lapply("[[", "Sum Sq") |>                        # Extract SSE
   Reduce(function(mc, lc)  (mc - lc) / mc, x = _)  # Substitute into Eq 21.4
Rsquare
```

<br />

### Regression using dummy variables

#### The models

Model statements for the “More Constrained” and “Less Contrained” models
are shown below. Note the use of `I()` in the formula for the “More
Constrained” model. Its effect is to constrain the three means to
equality.

``` r
models <- list(
   "More Constrained" = "y ~ -1 + I(x1 + x2 + x3)",
   "Less Constrained" = "y ~ -1 + x1 + x2 + x3"
)
```

#### Fit the model and get the results

Fitting the models and getting the results proceeds as before.

``` r
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
   Reduce(function(mc, lc)  (mc - lc) / mc, x = _)   # Substitute into Eq 21.4
Rsquare
```

<br />

### Regression using the restriktor package

The **restriktor** package allows the testing of constraints placed on
parameters in regression models. Use `lm()` to fit the “Less
Constrained” model as usual. The models can be specified using the cell
means approach or using dummy variables. Constraints are specified and
passed to the `iht()` function (of the **restriktor** package). If the
cell means approach is used, then the constraints are in terms of the
groups: the mean for group “a” equals the mean for group “b”, and the
mean for group “b” equals the mean for group “c”. If the dummy variables
approach is used, then the constraints are in terms of the dummy
variables: the mean for the group signified by variable “x1” is equal to
the mean for the group signified by variable “x2”, and the the mean for
the group signified by variable “x2” is equal to the mean for the group
signified by variable “x3”. The `iht()` function compares the fit for
the two models.

The `iht()` function returns estimates of the means and the F test. The
degrees of freedom for the F-test can be extrcted.

Using cell means

``` r
lc <- lm(y ~ -1 + x, df)   # Less Constrained model
summary(lc)

constraints <- "xa == xb
                xb == xc"  # constrain the means to equality
               
# Compare the fit for the two models           
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test

test$df; test$df.residual
```

Using dummy variables

``` r
lc <- lm(y ~ -1 + x1 + x2 + x3, df)  # Less Constrained model
summary(lc)

constraints <- "x1 == x2
                x2 == x3"  # constrain the means to equality
                
# Compare the fit for the two models                 
test <- iht(lc, constraints = constraints, type = "A", test = "F"); test

test$df; test$df.residual
```

Compare with Table 21.1. The other information presented in Table 21.1
can be extracted (but not shown here). There might not be much advantage
to using the **restriktor** package as it is applied to the example
here.

<br />

### Structural Equation Modeling

The SEM model for one-way ANOVA is shown in Fig 21.1 (p. 391), and is
reproduced below. The diagram shows the “Less Constrained” model - the
three means, represented by the label on the arrows connecting the “1”
to the dependent variable, differ. To be consistent with the ANOVA
assumption of homogeneity of variances, the residual variances are
constrained to be equal.

<img src="images/one_way_ANOVA.svg" data-fig-align="left" />

The model statements are shown below. The “More Constrained” model
constrains the means (each with the same label “a”) to equality. The
“Less Constrained” model allows the means (represented by the labels
“a1”, “a2”, and “a3”) to differ across the groups. (Alternatively, this
line could have been writted as `y ~ 1`; ie, no label means that the
means are freely estimated in each group. I leave the labels in to be
consistent with the diagram of the model.) In both cases the residual
variances (each with the same label “e”) are constrained to equality.

``` r
models <- list(
"More Constrained" = 
  "y ~ c(a, a, a)*1        # Means
   y ~~ c(e, e, e)*y",     # Variances

"Less Constrained" = 
  "y ~ c(a1, a2, a3)*1
   y ~~ c(e, e, e)*y"
)
```

<br />

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

The summaries show the “Intercepts” (that is, the estimated means) for
each “Coping Strategy” group for both models. Compare with the means in
the SEM section in Table 21.1.

Rather than, or perhaps, as well as, searching through the model
summaries for the means, the means can be extracted from a list of
estimates of model parameters.

``` r
estimates <- lapply(fit, lavInspect, "est"); estimates    # Means are in element "nu"

means <- list()
for (i in names(models)) 
   means[[i]] <- estimates[[i]] |>
      lapply("[[", "nu")  |>       # Extract the means
      unlist()
means
```

The pooled error variances can also be extracted from the list of
estimates. Recall that error variances are constrained to equality, and
therefore, the estimates for one group only (here, group “a”) are
extracted.

``` r
ErrorVar <- estimates |>
   lapply("[[", "a") |>           # Extract estimates for group "a"
   lapply("[[", "theta")          # Extract "theta" element
ErrorVar
```

Compare with pooled error variances in Table 21.1.

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
   tab = fitMeasures(fit, c("chisq", "df", "pvalue"))
   tab = round(data.frame(tab), 3) 
}

lapply(fit, GetFit)
```

Note: Neither model fits well.

TLG mention (on p. 390) the calculation for R<sup>2</sup>. The relevant
SSEs can be obtained from the error variances (see `ErrorVar`) by
multiplying error variance by sample size. However, note that
multiplication is not needed because sample size will cancel out; that
is, substitute the error variances into Equation 21.4.

``` r
Rsquare <- ErrorVar |>
   Reduce(function(mc, lc) (mc - lc)/mc, x = _)  # Substitute into Eq 21.4  
c(Rsquare)
```

<br />

#### Relaxing assumption of homogeneity of variances

TLG do not run these models though they make reference to them. The
model statements when the homogeneity of variances assumption is relaxed
are shown below.

``` r
models <- list(
"More Constrained" = 
  "y ~ c(a, a, a)*1            # Means
   y ~~ c(e1, e2, e3)*y",      # Variances

"Less Constrained" = 
  "y ~ c(a1, a2, a3)*1
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

The R script with minimal commenting is available in
[01_one_way_ANOVA.r](01_one_way_ANOVA.r).
