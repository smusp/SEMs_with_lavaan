# One-Way MANOVA


<style> 
.math.inline .MathJax {
  font-size: 98% !important;
}
.math.display .MathJax {
  font-size: 97% !important;
}
&#10;</style>

Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation
modeling approaches for analyzing means. In R. Hoyle (Ed.), *Handbook of
structural equation modeling* (2nd ed., pp. 385-408). New York, NY:
Guilford Press.

<br />

This example shows the OLS regression approach and the SEM approach to
Part 4: One-way MANOVA. Results are reported in Table 21.5 (p. 399).

The data file (`satisfactionII.csv`) is in the `data` folder.

<br />

#### Load package and get the data

``` r
library(lavaan)

df <- read.csv("./data/satisfactionII.csv", header = TRUE)
head(df)
```

The variables used in this example are:

- x - Coping Strategy (“a” - no strategy; “b” - discussion; “c” -
  exercise)
- x1, x2, x3 - dummy coded variables (1, 0) for “Coping Strategy”
- y1, y2, y3, y4 - multiple dependent variables (life-satisfaction
  scores)

<br />

### OLS regression using cell-means formulation

#### The models

Model statements for the “More Constrained” and “Less Contrained” models
are shown below. The multiple dependent variables (y1, y2, y3, y4) are
combined into one object Y. The “More Constrained” and “Less
Constrained” statement are then similar to the statements for the
one-way ANOVA. In the “More Constrained” model, the means are
constrained to equality; in the “Less Constrained” model, the means are
allowed to differ.

``` r
Y <- with(df, cbind(y1, y2, y3, y4))
models <- list(
   "More Constrained" = "Y ~ 1",
   "Less Constrained" = "Y ~ -1 + x"
)
```

<br />

#### Fit the models and get the results

Fit the models and get the means.

``` r
fit <- lapply(models, lm, data = df)

lapply(fit, coef)
```

Compare with the means in Table 21.5. There could be another typo in
Table 21.5: The mean for third variable in the third groups should be
42.08 (instead of 33.68).

Table 21.5 gives the error SSCP matrices for the two models. The SSCP
matrix is used in the calculation of Wilks $\Lambda$, which in turn is
used in the caculation of F (along with the appropriate degrees of
freedom). All of this can be skipped using the `anova()` function
(provided the default test statistic, Pillai’s trace, is changed to
Wilks’ $\Lambda$).

``` r
anova(fit[[2]], fit[[1]], test = "Wilks")
# or using Reduce
Reduce(function(mc, lc) anova(lc, mc, test = "Wilks"), x = fit)
```

Compare with the F test in Table 21.5.

For the sake of completeness, the quantities shown in Table 21.5 are
easily calculated. To calculate error SSCP, get the matrix of residuals,
then pre-multiply the matrix by its transpose. The formulas for Wilks’
$\Lambda$ and $\mathsf{F}$ are given in Table 21.5. The formulas for the
degrees of freedom are not given, but are easily located in standard
texts. Degrees of freedom depend on:

- $k$ - number of variables - 4
- $m$ - number of groups - 3
- $n$ - sample size - 200

The formulas for numerator ($\mathrm{df_1}$) and denominator
($\mathrm{df_2}$) degrees of freedom are:

$$
\begin{align} 
\mathrm{df_1} &= k(m - 1) \\[.5em]
\mathrm{df_2} &= a(b - c)
\end{align}
$$

where:

$$
\begin{align}
a &= n - m - \frac{k - m + 2}{2} \\[.5em]
b &= \sqrt{\frac{k^2(m-1)^2 - 4}{k^2 + (m - 1)^2 - 5}}  \\[.5em]
c &= \frac{k(m - 1) - 2}{2}
\end{align}
$$

<br />

``` r
# Get error SSCP
E <- fit |>
   lapply(residuals) |>
   lapply(function(x) t(x) %*% x)
E

# Get Wilks' lambda
lambda <- Reduce(function(mc, lc) det(lc) / det(mc), x = E); lambda

# Get df1, df2, F and p
k <- 4
m <- 3
n <- 200

# A function to do the calculations
pF <- function(k, m, n) {
   df1 = k*(m - 1)

   a = n - m - (k - m + 2)/2
   b = sqrt( (k^2 * (m - 1)^2 - 4) / (k^2 + (m - 1)^2 - 5))
   c = (k * (m - 1) - 2) / 2

   df2 = a * b - c

   F = ((1 - sqrt(lambda))/df1) / (sqrt(lambda)/df2)
   p = pf(F, df1, df2, lower.tail = FALSE)
   tab = round(data.frame("lambda" = lambda, "F" = F, "df1" = df1, "df2" = df2, "p" = p), 3)
   return(tab)
}

pF(k, m, n)
```

<br />

### OLS Regression using dummy variables

#### The models

``` r
Y <- with(df, cbind(y1, y2, y3, y4))
models <- list(
   "More Constrained" = "Y ~ -1 + I(x1 + x2 + x3)",
   "Less Constrained" = "Y ~ -1 + x1 + x2 + x3"
)
```

<br />

#### Fit the model and get the results

Fitting the models and getting the results proceeds as before.

``` r
fit <- lapply(models, lm, data = df)

# Get the means
lapply(fit, coef)

# F test
anova(fit[[2]], fit[[1]], test = "Wilks")
```

<br />

### Structural Equation Modeling

The SEM model for the one-way MANOVA is shown in Fig 21.2 (p. 400), and
is reproduced below. The diagram shows the “Less Constrained” model. The
means are represented by the labels on the arrows connecting the “1” to
the dependent variables. The means for each variable are allowed to
differ across the groups. The residual variances and covariances are
constrained to equality.

<img src="images/one_way_MANOVA.svg" data-fig-align="left" />

The model statements are shown below. The “More Constrained” model
constrains the means to equality. The “Less Constrained” model allows
the means to differ across the groups. In both cases the residual
variances and covariances are constrained to equality. The variancs and
covariances can be set up separately - see `vcov` below. Then, `vcoc` is
added back into each model. Saves a little typing.

``` r
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
```

<br />

#### Fit the models and get the results

``` r
fit <- lapply(models, sem, data = df, group = "x")

# Get model summaries
lapply(fit, summary)        # Means are "Intercepts"

# Contrast model fits
Reduce(anova, fit)
```

The “SEM” section of Table 21.5 shows the $\upchi$<sup>2</sup> test.

Scroll through the summaries to find the “Intercepts”, or extract the
means from the list of estimates of model parameter.

``` r
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
```

Campare with the means in Table 21.5.

By way of completeness, get the error SSCP matrices. (TLG state that,
“the error SSCP matrices were perfectly reproduced by multiplying the
variances and covariances in the SEM output by the total sample size”
p. 398).

``` r
# Note: In the list of estimates, variances and covariances are in element "theta"
   E = estimates |>
   lapply("[[", "a") |>           # Extract estimates for group "a"
   lapply("[[", "theta") |>       # Extract "theta" element
   lapply(matrix, nrow = 4) |>    # Get the full matrix
   lapply("*", 200)               # Multiply by sample size
E
```

<br />

### Relax homogeneity of variances and covariances assumption

Towards the end of the section headed “Avoiding OLS assumptions for
ANOVA/MANOVA designs using SEM” (pp. 398-401), TGL present the results
for models in which the assumptions of homogeneity and normality are
relaxed. That is, variances and covariances are not constrained to
equality, and a robust ML method of estimation (MLM) is employed. Again,
the variances and covariances are set up separately, then added back
into each model. This time, there are no labels for the variances and
covariances, meaning that **lavaan** will estimate each variance and
covariance for each group.

``` r
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
```

Compare with the $\upchi$<sup>2</sup> test on p. 401.

<br />

The R script with minimal commenting is available in
[04_one_way_MANOVA.r](04_one_way_MANOVA.r).
