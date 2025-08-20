# One-Way ANOVA of latent variable


Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation
modeling approaches for analyzing means. In R. Hoyle (Ed.), *Handbook of
structural equation modeling* (2nd ed., pp. 385-408). New York, NY:
Guilford Press.

This example shows the SEM approach to Part 5: One-way ANOVA of a latent
variable. Results are reported in Table 21.6 (p. 404).

Any comparison of latent means assumes some level of measurement
invariance. The first part of this example demonstrates SEM assuming
strict measurement invariance. The second part demonstrates SEM under
partial invariance of loadings and intercepts.

#### Load package and get the data

Load the **lavaan** package, and run `satisfactionII.r` to get the data.

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

The SEM model for the one-way ANOVA of a latent variable is shown in Fig
21.3 (p. 403), and is reproduced below. The diagram shows the “Less
Constrained” model.

<img src="images/one_way_LATENT.svg" data-fig-align="center" />

For purposes of identification and scaling, the loading for the first
indicator is constrained to one. (Thompson, Lie & Green (TLG) claim that
the loading for the 4th indicator is constrained to one. However, when
the 4th loading is constrained to one, I do not get the same results as
given in Table 21.6 (in particular the latent variance), nor do I get
the means (given in the discussion on page 405); whereas I get agreement
with the Table and the text when I constrain the 1st loading to one.)

Also for the purposes of identification and scaling, the latent mean for
the first group is constrained to zero. For the “Less Constrained”
model, the latent means for the other groups (a2 and a3) are freely
estimated.

TLG assume strict measurement invariance:

- the loadings ($\uplambda$) are constrained to equality across the
  groups;
- the intercepts ($\uptau$) are constrained to equality across the
  groups;
- the indicator residual variances (e) and covariances are constrained
  to equality (covariances are set to zero by default, and thus they are
  equal);
- TLG impose one last constraint - latent error variances are
  constrained to equality across the groups (they do this to obtain a
  pooled variance to calculate an effect size for the differences
  between latent means).

The model statements are shown below. The only difference between the
“More Constrained” model and the “Less Constrained” model is in the
latent means. For purposess of identification and scaling, the latent
mean for the first group is constrained to zero; in the “More
Constrained” model, in which the means are constrained to equality, all
three are constrained to zero. In the “Less Constrained” model, the
means in the second and third groups are freely estimated.

The common parts of the two models are set up according to the bullet
points above.

``` r
common <- "
   #  Measurement model
   F =~ y1 + c(l2,l2,l2)*y2 + c(l3,l3,l3)*y3 + c(l4,l4,l4)*y4 

   # Indicator intercepts
   y1 ~ c(a1,a1,a1)*1
   y2 ~ c(a2,a2,a2)*1
   y3 ~ c(a3,a3,a3)*1
   y4 ~ c(a4,a4,a4)*1

   # Indicator residual variances
   y1 ~~ c(e1,e1,e1)*y1
   y2 ~~ c(e2,e2,e2)*y2
   y3 ~~ c(e3,e3,e3)*y3
   y4 ~~ c(e4,e4,e4)*y4

   # Latent error variances
   F ~~ c(d,d,d)*F"

models <- list(
  "More Constrained" = c(
    "# Latent means
    F ~ c(m,m,m)*1

     # Constraint
     m == 0",

     common),

  "Less Constrained" =  c(
    "# Latent means
     F ~ c(m1,m2,m3)*1

     # Constraint
     m1 == 0",

    common)
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

Compare with $\upchi$<sup>2</sup> test in the “All measures” row in
Table 21.6.

One could scroll through the model summaries to find the latent means
and error variances for the “Less Constrained” model, and compare them
with Table 21.6. They are also needed to calculate effect sizes (given
in the first column on page 405).

``` r
d1 <- (0.664 - 0) / sqrt(8.135); d1    # "no strategy" vs "discussion"
d2 <- (1.945 - 0) / sqrt(8.135); d2    # "no strategy" vs "exercise"
```

But it is probably safer to extract latent means and error variances
from a list of parameter estimates, then substitute into the formula for
effect size.

``` r
estimates <- lavInspect(fit[["Less Constrained"]], "est"); estimates
   # Note: latent means are in element "alpha"
   #       latent error variances are in element "psi"

LatentMeans <- sapply(estimates, "[[", "alpha"); LatentMeans
LatentVar   <- sapply(estimates, "[[", "psi"); LatentVar

   # "no strategy" vs "discussion"
d1 <- (LatentMeans[2] - LatentMeans[1]) / sqrt(LatentVar[1]); d1

   # "no strategy" vs "exercise"
d2 <- (LatentMeans[3] - LatentMeans[1]) / sqrt(LatentVar[1]); d2
```

Compare the effect sizes with those given on page 405, and the means and
error variances with those in Table 21.6.

### More Flexible Tests of Differences in Means on Latent Variables

The second and third rows of Table 21.6 follow after a discussion in the
section headed “More Flexible Tests of Differences in Means on Latent
Variables” (pp. 406-407).

In this section, TLG assume partial strong invariance:

- Constrain the loading for one indicator to equality across groups
- Constrain that indicator’s intercept to equality across groups
- The other loadings and intercepts are freely estimated

For purposes of identification:

- First loading in each group is constrained to one
- Latent mean in the first group is constrained to zero

Note that the residual variances are freely estimated across groups, as
are the latent error variances. The indicator covariances are by default
set to zero (unless there is good reason to have one or more estimated).

For the “More Constrained” model, the latent means are constrained to
equality across the groups; for the “Less Constrained” model, the latent
means differ.

The purpose of these examples is to demonstrate that “if a latent
variable has only a single referent variable, the means and variances of
the latent variable are a function of only the means and variances of
this variable” (p. 406).

#### Model that applies to the 2nd row in Table 21.6: “One measure - Y$_1$”

The selected indicator is the first - “y1”.

``` r
# Model statements
common <- 
  "# Measurement model
   F =~ y1 + c(l12,l22,l32)*y2 + c(l13,l23,l33)*y3 + c(l14,l24,l34)*y4

   # Indicator intercepts
   y1 ~ c(a1,a1,a1)*1
   y2 ~ c(a12,a22,a32)*1
   y3 ~ c(a13,a23,a33)*1
   y4 ~ c(a14,a24,a34)*1

   # Indicator residual variances
   y1 ~~ c(e11,e21,e31)*y1
   y2 ~~ c(e12,e22,e32)*y2
   y3 ~~ c(e13,e23,e33)*y3
   y4 ~~ c(e14,e24,e34)*y4

   # Latent error variances
   F ~~ c(d1,d2,d3)*F"

models <- list(
  "More Constrained" = c(
    "# Latent means
     F ~ c(m,m,m)*1

     # Constraint
     m == 0",
   
     common),

  "Less Constrained" =  c(
    "# Latent means
     F ~ c(m1,m2,m3)*1

     # Constraint
     m1 == 0",

     common)
)
```

Fit the models and get the latent means, latent error variances, and the
$\upchi$<sup>2</sup> test.

``` r
## Fit the models 
fit <- lapply(models, sem, data = df, group = "x")

## Model summaries
lapply(fit, summary)

## Get the latent means and latent error variances for "Less Constrained" model
estimates <- lavInspect(fit[["Less Constrained"]], "est"); estimates
LatentMeans <- sapply(estimates, "[[", "alpha"); LatentMeans
LatentVar   <- sapply(estimates, "[[", "psi"); LatentVar

## Contrast model fits
Reduce(anova, fit)
```

Compare with the latent means and error variances, and the
$\upchi$<sup>2</sup> test in the 2nd row in Table 21.6.

Consider the three columns of Table 21.6 dealing with means, variances
and residual variances of one measure - in this case, “y1”.

I need sample means and covariances - they can be extract from a list of
sample statistics. I need estimated indicator intercepts and residual
variances - they can be extracted from `estimates`. Also I need
estimated latent means and error variances - they have already been
extracted.

``` r
# Get sample statistics
sampstat <- lavInspect(fit[["Less Constrained"]], "sampstat"); sampstat
   # Means are in element "mean"
   # Variances are the diagonal elements in element "cov"

# Get estimated model parameters
estimates
   # Residual variances for the measures are the diagonal elements in element "theta"
   # Intercepts for the measures are in element "nu"
```

##### Means of the measures for each group (ie, the “y1” means).

- Extract the “y1” means from `sampstats`
- Latent means already extracted in `LatentMeans`
- The differences between the “y1” means are the differences between the
  latent means  
- Alternatively, the “y1” intercepts (which are constrained to equality)
  when added to the latent means give the “y1” means

``` r
# Extract y1 means from sampstats
MeansY1 <- sapply(sampstat, "[[", "mean")
MeansY1 <- MeansY1[1,]; MeansY1   # Compare with 2nd row in Table 21.6

# Differences between y1 means are differencs between latent means
MeansY1[2] - MeansY1[1]
MeansY1[3] - MeansY1[1]

# Compare with latent means
LatentMeans

# Alternatively, the y1 intercepts (which are constrained to equality)
# added to the latent means give the y1 means
intercepts <- sapply(estimates, "[[", "nu")[1,1]; intercepts
intercepts + LatentMeans; MeansY1
```

##### Variances of the measure for each group (ie, “y1” variances)

- Extract the “y1” variances from `sampstats`
- Extract residual variances for “y1” from `estimates`
- Latent error variances already extracted in `LatentVar`
- Differences between “y1” variances and “y1” residual variances are the
  latent error variances

``` r
# Extract y1 variances from sampstats
VarY1 <- sapply(lapply(sampstat, "[[", "cov"), diag)
VarY1 <- VarY1[1,]; VarY1  # Compare with 2nd row in Table 21.6

# Extract residual variances for y1 from estimates
ResidVarY1 <- sapply(lapply(estimates, "[[", "theta"), diag)
ResidVarY1 <- ResidVarY1[1,]; ResidVarY1 # Compare with 2nd row in Table 21.6

# Differences between y1 variances and y1 residual variances are latent error variances
VarY1 - ResidVarY1

# Compare with the latent error variances
LatentVar
```

#### Model that applies to the 3rd row in Table 21.6: “One measure - Y$_2$”

The selected indicator is the second - “y2”.

This is the same as before, except the constraints on the loadings and
intercepts apply to the second indicator.

``` r
# Model statements
common <- 
  "# Measurement model
   F =~ NA*c(l11,l21,l31)*y1 + 1*y2 + c(l13,l23,l33)*y3 + c(l14,l24,l34)*y4 

   # Indicator intercepts 
   y1 ~ c(a11,a21,a31)*1
   y2 ~ c(a2,a2,a2)*1
   y3 ~ c(a13,a23,a33)*1
   y4 ~ c(a14,a24,a34)*1

   # Indicator residual variances
   y1 ~~ c(e11,e21,e31)*y1
   y2 ~~ c(e12,e22,e32)*y2
   y3 ~~ c(e13,e23,e33)*y3
   y4 ~~ c(e14,e24,e34)*y4

   # Latent error variances
   F ~~ c(d1,d2,d3)*F"

models <- list(
  "Less Constrained" =  c(
    "# Latent means
     F ~ c(m1,m2,m3)*1

     # Constraint
     m1 == 0",

     common),

  "More Constrained" = c(
    "# Latent means
     F ~ c(m,m,m)*1

     # Constraint
     m == 0",

     common)
)
```

Fit the models and get the latent means, latent error variances, and the
$\upchi$<sup>2</sup> test.

``` r
# Fit the models
fit <- lapply(models, sem, data = df, group = "x")

# Model summaries
lapply(fit, summary)

# Get the latent means and latent error variances for "Less Constrained" model
estimates <- lavInspect(fit[["Less Constrained"]], "est"); estimates
LatentMeans <- sapply(estimates, "[[", "alpha"); LatentMeans
LatentVar   <- sapply(estimates, "[[", "psi"); LatentVar

# Contrast model fits
Reduce(anova, fit)
```

Compare with the latent means and error variances, and the
$\upchi$<sup>2</sup> test in the 3rd row in Table 21.6.

Consider the three columns of Table 21.6 dealing with means, variances
and residual variances of one measure - in this case, “y2”.

I need sample means and covariances - they can be extract from a list of
sample statistics. I need estimated indicator intercepts and residual
variances - they can be extracted from `estimates`. Also I need
estimated latent means and error variances - they have already been
extracted.

``` r
# Get sample statistics
sampstat <- lavInspect(fit[["Less Constrained"]], "sampstat"); sampstat
   # Means are in element "mean"
   # Variances are the diagonal elements in element "cov"

# Get estimated model parameters
estimates
   # Residual variances for the measures are the diagonal elements in element "theta"
   # Intercepts for the measures are in element "nu"
```

##### The means of the measures for each group (ie, the “y2” means).

- Extract the “y2” means from `sampstats`
- Latent means already extracted in `LatentMeans`
- The differences between the “y2” means are the differences between the
  latent means
- Alternatively, the “y2” intercepts (which are constrained to equality)
  when added to the latent means give the “y2” means

``` r
# Extract y2 Means from sampstats
MeansY2 <- sapply(sampstat, "[[", "mean")
MeansY2 <- MeansY2[2,]; MeansY2   # Compare with 3rd row in Table 21.6

# Differences between y2 means are differencs between latent means
MeansY2[2] - MeansY2[1]
MeansY2[3] - MeansY2[1]

# Compare with latent means
LatentMeans

# Alternatively, the y2 intercepts (which are constrained to equality)
# added to the latent means give the Y2 means
intercepts <- sapply(estimates, "[[", "nu")[2,1]; intercepts
intercepts + LatentMeans; MeansY2
```

##### The variances of the measure for each group (ie, “y2” variances)

- Extract the “y2” variances from `sampstats`
- Extract residual variances for “y2” from `estimates`
- Latent error variances already extracted in `LatentVar`
- Differences between “y2” variances and “y2” residual variances are the
  latent error variances

``` r
# Extract y2 variances from sampstats
VarY2 <- sapply(lapply(sampstat, "[[", "cov"), diag)
VarY2 <- VarY2[2,]; VarY2  # Compare with 3rd row in Table 21.6

# Extract residual variances for y2 from estimates
ResidVarY2 <- sapply(lapply(estimates, "[[", "theta"), diag)
ResidVarY2 <- ResidVarY2[2, ]; ResidVarY2 # Compare with 3rd row in Table 21.6

# Differences between y2 variances and y2 residual variances are latent error variances
VarY2 - ResidVarY2

# Compare with the latent error variances
LatentVar
```

<br />

<details class="code-fold">
<summary>R code with minimal commenting</summary>

``` r
## One-way ANOVA of latent variable
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
common <- "
   #  Measurement model
   F =~ y1 + c(l2,l2,l2)*y2 + c(l3,l3,l3)*y3 + c(l4,l4,l4)*y4

   # Indicator intercepts
   y1 ~ c(a1,a1,a1)*1
   y2 ~ c(a2,a2,a2)*1
   y3 ~ c(a3,a3,a3)*1
   y4 ~ c(a4,a4,a4)*1

   # Indicator residual variances
   y1 ~~ c(e1,e1,e1)*y1
   y2 ~~ c(e2,e2,e2)*y2
   y3 ~~ c(e3,e3,e3)*y3
   y4 ~~ c(e4,e4,e4)*y4

   # Latent error variances
   F ~~ c(d,d,d)*F"

models <- list(
  "More Constrained" = c(
    "# Latent means
    F ~ c(m,m,m)*1

     # Constraint
     m == 0",

     common),

  "Less Constrained" =  c(
    "# Latent means
     F ~ c(m1,m2,m3)*1

     # Constraint
     m1 == 0",

    common)
)

## Fit the models and get the results
## Check results in "All measures" row in Table 21.6
## Fit the models
fit <- lapply(models, sem, data = df, group = "x")

## Get model summaries
lapply(fit, summary)

## Contrast model fits
Reduce(anova, fit)

## Cut-and-paste means and variances to get effect sizes
## Compare with values given on page 405
d1 <- (0.664 - 0) / sqrt(8.135); d1    # "no strategy" vs "discussion"
d2 <- (1.945 - 0) / sqrt(8.135); d2    # "no strategy" vs "exercise"

## Extract latent means and error variances from "Less Constrained" model
## Check with "All measures" row in Table 21.6
estimates <- lavInspect(fit[["Less Constrained"]], "est"); estimates
   # Note: latent means are in element "alpha"
   #       latent error variances are in element "psi"

LatentMeans <- sapply(estimates, "[[", "alpha"); LatentMeans
LatentVar   <- sapply(estimates, "[[", "psi"); LatentVar

## Effect sizes
## Compare with values given on page 405
   # "no strategy" vs "discussion"
d1 <- (LatentMeans[2] - LatentMeans[1]) / sqrt(LatentVar[1]); d1

   # "no strategy" vs "exercise"
d2 <- (LatentMeans[3] - LatentMeans[1]) / sqrt(LatentVar[1]); d2

## Relaxing some constraints
## ANOVA model for 2nd row in Table 21.6
# Model statements
common <-
  "# Measurement model
   F =~ y1 + c(l12,l22,l32)*y2 + c(l13,l23,l33)*y3 + c(l14,l24,l34)*y4

   # Indicator intercepts
   y1 ~ c(a1,a1,a1)*1
   y2 ~ c(a12,a22,a32)*1
   y3 ~ c(a13,a23,a33)*1
   y4 ~ c(a14,a24,a34)*1

   # Indicator residual variances
   y1 ~~ c(e11,e21,e31)*y1
   y2 ~~ c(e12,e22,e32)*y2
   y3 ~~ c(e13,e23,e33)*y3
   y4 ~~ c(e14,e24,e34)*y4

   # Latent error variances
   F ~~ c(d1,d2,d3)*F"

models <- list(
  "More Constrained" = c(
    "# Latent means
     F ~ c(m,m,m)*1

     # Constraint
     m == 0",

     common),

  "Less Constrained" =  c(
    "# Latent means
     F ~ c(m1,m2,m3)*1

     # Constraint
     m1 == 0",

     common)
)

## Fit the model and get the results
## Check with means, error variance, and chi square in 2nd row in Table 21.6
## Fit the models
fit <- lapply(models, sem, data = df, group = "x")

## Model summaries
lapply(fit, summary)

## Get the latent means and latent error variances for "Less Constrained" model
estimates <- lavInspect(fit[["Less Constrained"]], "est"); estimates
LatentMeans <- sapply(estimates, "[[", "alpha"); LatentMeans
LatentVar   <- sapply(estimates, "[[", "psi"); LatentVar

## Contrast model fits
Reduce(anova, fit)

## Columns of Table 21.6 dealing with means, variances and residual variances of "y1"
## Need sample statistics and model estimates
# Get sample statistics
sampstat <- lavInspect(fit[["Less Constrained"]], "sampstat"); sampstat
   # Means are in element "mean"
   # Variances are the diagonal elements in element "cov"

# Get estimated model parameters
estimates
   # Residual variances for the measures are the diagonal elements in element "theta"
   # Intercepts for the measures are in element "nu"

# Extract y1 means from sampstats
MeansY1 <- sapply(sampstat, "[[", "mean")
MeansY1 <- MeansY1[1,]; MeansY1   # Compare with 2nd row in Table 21.6

# Differences between y1 means are differencs between latent means
MeansY1[2] - MeansY1[1]
MeansY1[3] - MeansY1[1]

# Compare with latent means
LatentMeans

# Alternatively, the y1 intercepts (which are constrained to equality)
# added to the latent means give the y1 means
intercepts <- sapply(estimates, "[[", "nu")[1,1]; intercepts
intercepts + LatentMeans; MeansY1

# Extract y1 variances from sampstats
VarY1 <- sapply(lapply(sampstat, "[[", "cov"), diag)
VarY1 <- VarY1[1,]; VarY1  # Compare with 2nd row in Table 21.6

# Extract residual variances for y1 from estimates
ResidVarY1 <- sapply(lapply(estimates, "[[", "theta"), diag)
ResidVarY1 <- ResidVarY1[1,]; ResidVarY1 # Compare with 2nd row in Table 21.6

# Differences between y1 variances and y1 residual variances are latent error variances
VarY1 - ResidVarY1

# Compare with the latent error variances
LatentVar

## Relaxing some constraints
## ANOVA model for 3rd row in Table 21.6
# Model statements
common <-
  "# Measurement model
   F =~ NA*c(l11,l21,l31)*y1 + 1*y2 + c(l13,l23,l33)*y3 + c(l14,l24,l34)*y4

   # Indicator intercepts
   y1 ~ c(a11,a21,a31)*1
   y2 ~ c(a2,a2,a2)*1
   y3 ~ c(a13,a23,a33)*1
   y4 ~ c(a14,a24,a34)*1

   # Indicator residual variances
   y1 ~~ c(e11,e21,e31)*y1
   y2 ~~ c(e12,e22,e32)*y2
   y3 ~~ c(e13,e23,e33)*y3
   y4 ~~ c(e14,e24,e34)*y4

   # Latent error variances
   F ~~ c(d1,d2,d3)*F"

models <- list(
  "Less Constrained" =  c(
    "# Latent means
     F ~ c(m1,m2,m3)*1

     # Constraint
     m1 == 0",

     common),

  "More Constrained" = c(
    "# Latent means
     F ~ c(m,m,m)*1

     # Constraint
     m == 0",

     common)
)

## Fit the model and get the results
## Check with means, error variance, and chi square in 3rd row in Table 21.6
# Fit the models
fit <- lapply(models, sem, data = df, group = "x")

# Model summaries
lapply(fit, summary)

# Get the latent means and latent error variances for "Less Constrained" model
estimates <- lavInspect(fit[["Less Constrained"]], "est"); estimates
LatentMeans <- sapply(estimates, "[[", "alpha"); LatentMeans
LatentVar   <- sapply(estimates, "[[", "psi"); LatentVar

# Contrast model fits
Reduce(anova, fit)

## Columns of Table 21.6 dealing with means, variances and residual variances of "y2"
## Need sample statistics and model estimates
# Get sample statistics
sampstat <- lavInspect(fit[["Less Constrained"]], "sampstat"); sampstat
   # Means are in element "mean"
   # Variances are the diagonal elements in element "cov"

# Get estimated model parameters
estimates
   # Residual variances for the measures are the diagonal elements in element "theta"
   # Intercepts for the measures are in element "nu"

# Extract y2 Means from sampstats
MeansY2 <- sapply(sampstat, "[[", "mean")
MeansY2 <- MeansY2[2,]; MeansY2   # Compare with 3rd row in Table 21.6

# Differences between y2 means are differencs between latent means
MeansY2[2] - MeansY2[1]
MeansY2[3] - MeansY2[1]

# Compare with latent means
LatentMeans

# Alternatively, the y2 intercepts (which are constrained to equality)
# added to the latent means give the Y2 means
intercepts <- sapply(estimates, "[[", "nu")[2,1]; intercepts
intercepts + LatentMeans; MeansY2

# Extract y2 variances from sampstats
VarY2 <- sapply(lapply(sampstat, "[[", "cov"), diag)
VarY2 <- VarY2[2,]; VarY2  # Compare with 3rd row in Table 21.6

# Extract residual variances for y2 from estimates
ResidVarY2 <- sapply(lapply(estimates, "[[", "theta"), diag)
ResidVarY2 <- ResidVarY2[2, ]; ResidVarY2 # Compare with 3rd row in Table 21.6

# Differences between y2 variances and y2 residual variances are latent error variances
VarY2 - ResidVarY2

# Compare with the latent error variances
LatentVar
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
"c", "c", "c", "c", "c", "c", "c", "c", "c", "c"), x1 = c(1L, 
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
0L, 0L, 0L, 0L, 0L, 0L, 0L), x2 = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 
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
0L), x3 = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
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
