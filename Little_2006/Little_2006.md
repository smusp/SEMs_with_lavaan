# Effect Scaling


Little, T., Slegers, D., & Card, N. (2006). A non-arbitrary method of
identifying and scaling latent variables in SEM and MACS models.
*Structural Equation Modeling*, *13*(1), 59-72.

This example shows how to set constraints for different methods of
identification and scaling in latent variable models, and, though
invariance is not a topic of discussion, the example shows how to set
constraints for strong metric invariance in a two-group model. Also, the
example shows how to use summary data (correlations, standard
deviations, and means) in a two-group model.

The methods of identification and scaling discussed by LS&C are:

- Reference Group Method - For each construct, the latent variance is
  fixed to one and the latent mean is fixed to zero in the first group.
  With strong metric invariance (invariance of loadings and intercepts),
  latent means and variances are freely estimated in subsequent groups.
- Marker Variable Method - For each construct, the loading of one
  indicator is fixed to one, and the intercept for the chosen indicator
  is fixed to zero. With strong metric invariance, these constraints
  apply in both groups.
- Effects Coding Method - For each construct, constrain loadings to add
  to the number of indicators, and constrain indicator intercepts to add
  to zero. With strong metric invariance, these constraints apply in
  both groups.

LS&C present a two-group (7th grade and 8th grade), two-construct
(positive affect and negative affact) model. Each construct is assessed
with three manifest indicators. The SEM diagram below shows the model as
it might apply to the whole sample (i.e., ignoring the groups). This
one-group model is presented to explain the symbols used in the paper,
and to show how they apply in the model diagrams. First, POS and NEG are
the constructs; and pos<sub>1</sub>, …, neg<sub>3</sub> are the manifest
indicators. The solid lines represent the covariance structure, and the
gray lines represent the mean structure (ie, the means and intercepts).

The symbols are:

- $\uplambda$ - loadings
- $\upphi$ - latent variances and covariance
- $\uptheta$ - indicator residual variances
- $\upkappa$ - latent means
- $\uptau$ - indicator intercepts

<img src="images/Scaling.svg" data-fig-align="left" />

#### Load relevant packages

Load the **lavaan** package.

``` r
library(lavaan)
```

#### Get the data

LISREL script in Appendix A (pp. 71-72) gives correlations, standard
deviations, means, and sample sizes for each group. From these, the
co/variance matrices for each group can be obtained. The means, standard
deviations, and correlations are entered as vectors. In Appendix A, only
the lower triangle of correlations is presented with ones along the
diagonal; and that is all that is needed here.

``` r
# 7th grade
cor7 <- c(
   1.00000,
   0.75854,  1.00000,
   0.76214,  0.78705,  1.00000,
   0.02766,  0.00973, -0.05762,  1.00000,
  -0.06112, -0.06105, -0.14060,  0.78501,  1.00000,
  -0.02222, -0.05180, -0.10250,  0.81616,  0.81076,  1.00000)

mean7 <- c(3.13552, 2.99061, 3.06945, 1.70069, 1.52705, 1.54483)
sd7   <- c(0.66770, 0.68506, 0.70672, 0.71418, 0.66320, 0.65276)
n7    <- 380

# 8th grade
cor8 <- c(
   1.00000,
   0.81366,  1.00000,
   0.84980,  0.83523,  1.00000,
  -0.18804, -0.15524, -0.21520,  1.00000,
  -0.28875, -0.24951, -0.33769,  0.78418,  1.00000,
  -0.29342, -0.21022, -0.30553,  0.79952,  0.83156,  1.00000)

mean8 <- c(3.07338, 2.84716, 2.97882, 1.71700, 1.57955, 1.55001)
sd8   <- c(0.70299, 0.71780, 0.76208, 0.65011, 0.60168, 0.61420)
n8    <- 379
```

Variable names are also contained in the LISREL script, but I shorten
the names a little.

``` r
names = c("pos1", "pos2", "pos3", "neg1", "neg2", "neg3")
```

Combine the correlations, means, and sample sizes into lists.

``` r
cor  <- list("Grade 7" = cor7, "Grade 8" = cor8)
sd   <- list(sd7, sd8)
mean <- list(mean7, mean8)
n    <- list(n7, n8)
```

Use the `getCov()` function from the **lavaan** package to obtain the
full co/variance matrix for each group (using the `Map()` function to
apply the `getCov()` function to the lists, and to return the two
co/variance matrices in a list).

``` r
cov <- Map(getCov, x = cor, sds = sd, names = list(names, names))
```

## Reference-Group Method

<h4 style="margin-top: 10px;">
The model
</h4>

The model with the constraints is shown below. Some points to note.
There are two groups: Grade 7 and Grade 8. The corresponding loadings
($\uplambda$) and intercepts ($\uptau$) are equal across the groups. The
latent variances
(<span style="white-space: nowrap">$\upphi$<sub>7,11</sub></span> and
<span style="white-space: nowrap">$\upphi$<sub>7,22</sub>)</span> and
latent means ($\upkappa$) are constrained to 1 and 0 respectively in the
first group only. The residual variances ($\uptheta$) are freely
estimated in each group, and the latent covariances
(<span style="white-space: nowrap">$\upphi$<sub>7,12</sub></span> and
<span style="white-space: nowrap">$\upphi$<sub>8,12</sub>)</span> are
freely estimated.

<img src="images/Scaling1.svg" data-fig-align="left" />

When constructing the model statment, there are some points to be
considered.

First, the constraints applied to latent means and variances apply in
the first group only. In the model statement, pre-multiply the mean or
the variance by a vector containing the constraints; like this:
`c(1,NA)` - the `1` forces the parameter in the first group to be
constrained to 1; the `NA` forces the parameter in the second group to
be estimated.

Second, LS&C state that the data display strong metric invariance
(p. 63); that is, the corresponding loadings and intercepts are equal
across the groups. There is no need to be concerned with these
constraints when constructing the model statement - they will be set up
in the next step. (Strictly, the intercepts do not even need to be
mentioned in the model statement - **lavaan** will add them
automatically when `sample.means` are in the model. But they are left in
the statement below because intercepts are implicated in constraints in
models to follow.)

Third, strong metric invariance places no constraints on indicator
variances - they are freely estimated in each group. But again, there is
no need to be concerned with or even to mention them when constructing
the model statement - **lavaan** will add them automatically.

Finally, **lavaan**’s default marker-variable method has to be
explicitely disabled by pre-multiplying the first indicator for both
constructs by `NA`.

``` r
m1 <- "
  # Measurement Model
  #   - Free the 1st loading so it can be estimated
  POS =~ NA*pos1 + pos2 + pos3
  NEG =~ NA*neg1 + neg2 + neg3

  # Latent variances and covariance
  #   - Constrain latent variances to 1 in first group
  POS ~~ c(1,NA)*POS
  NEG ~~ c(1,NA)*NEG
  POS ~~ NEG

  # Indicator intercepts
  pos1 ~ 1
  pos2 ~ 1
  pos3 ~ 1
  neg1 ~ 1
  neg2 ~ 1
  neg3 ~ 1

  # Latent means
  #   - Constrain latent means to 0 in first group
  POS ~ c(0,NA)*1
  NEG ~ c(0,NA)*1
"
```

#### Fit the model and get the results

To deal with strong metric invariance, set
`group.equal = c("loadings", "intercepts")` in the `sem()` function.

``` r
m1_fit <- sem(m1, sample.cov = cov, sample.nobs = n,
   sample.mean = mean, group.equal = c("loadings", "intercepts"))
summary(m1_fit, standardized = TRUE, fit.measures = TRUE)
```

Compare the output with “Method 1” in Table 2 (pp. 64-65).

#### A shortcut

**Lavaan** can do reference-group scaling automatically - set
`std.lv = TRUE` in the `sem()` function. The constraints are the same as
above - in the first group, latent variances are constrained to one, and
latent means are constrained to zero.

``` r
m1_short <- "
  # Measurement Model
  POS =~ pos1 + pos2 + pos3
  NEG =~ neg1 + neg2 + neg3
"

m1_short_fit <- sem(m1_short, sample.cov = cov, sample.nobs = n, 
   sample.mean = mean, std.lv = TRUE,
   group.equal = c("loadings", "intercepts"))
summary(m1_short_fit, standardized = TRUE, fit.measures = TRUE)
```

Check the output. It is the same as before except for one detail. It
might be disconcerting for some that the latent means in the first group
are not reported. Maybe it’s not important because they are zero
(remember the constraint). To see them in the output, set
`remove.unused = FALSE` in the `summary()` function.

``` r
summary(m1_short_fit, remove.unused = FALSE, standardized = TRUE,
   fit.measures = TRUE)
```

## Marker-Variable Method

<h4 style="margin-top: 10px;">
The model
</h4>

The model with the constraints is shown below.

<img src="images/Scaling2.svg" data-fig-align="left" />

Results for three versions of Method 2 are presented in Table 2 - in
each case, constraints are applied to different indicator variables.
Here, only the third is considered - constraints apply to loadings and
intercepts for the third indicator in the POS construct, and to the
first indicator in the NEG construct.

Some points to be considered.

First, with strong metric invariance in place, the constraints applying
to intercepts and loadings apply in both groups. In the model statement,
pre-multiply the loadings by `c(1,1)`, and the intercepts by `c(0,0)`.

Second, again, the point concerning strong metric invariance - it will
be set up in the next step.

Third, again, there is no need to mention indicator variances -
**lavaan** will add them automatically.

Finally, **lavaan**’s default marker-variable method has to be
explicitely disabled for the POS construct by pre-multiplying the first
indicator be `NA`.

``` r
m2c <- "
  # Measurement Model
  #   - Free the 1st loading in POS so it can be estimated
  #   - Constrain 3rd loading in POS to 1 in both groups
  #   - Constrain 1st loading in NEG to 1 in both groups
  POS =~ NA*pos1 + pos2 + c(1,1)*pos3
  NEG =~ c(1,1)*neg1 + neg2 + neg3

  # Latent variances and covariance
  POS ~~ POS
  NEG ~~ NEG
  POS ~~ NEG

  # Indicator intercepts
  #   - Constrain 3rd intercept in POS to 0 in both groups
  #   - Constrain 1st intercept in NEG to 0 in both groups
  pos1 ~ 1
  pos2 ~ 1
  pos3 ~ c(0,0)*1
  neg1 ~ c(0,0)*1
  neg2 ~ 1
  neg3 ~ 1

  # Latent means
  POS ~ 1
  NEG ~ 1
"
```

#### Fit the model and get the results

As before, the `group.equal = c("loadings", "intercepts")` statement in
the `sem()` function forces corresponding loadings and intercepts to be
equal across the groups.

``` r
m2c_fit <- sem(m2c, sample.cov = cov, sample.nobs = n,
   sample.mean = mean, group.equal = c("loadings", "intercepts"))
summary(m2c_fit, standardized = TRUE, fit.measures = TRUE)
```

Compare the output with “Method 2c” in Table 2 (pp. 64-65).

#### **Lavaan** default

This is not **lavaan**’s default method of scaling. The default method
constrains the loadings for the first indicator to one for both
constructs and, because of the strong metric invariance, in both groups.
When there is a mean structure in the model, **lavaan** sets the latent
means to zero (in the first group only). In the `summary()` function set
`remove.unused = FALSE` to see the latent means.

``` r
m2c_default <- "
  # Measurement Model
  POS =~ pos1 + pos2 + pos3
  NEG =~ neg1 + neg2 + neg3
"

m2c_default_fit <- sem(m2c_default, sample.cov = cov, sample.nobs = n,
   sample.mean = mean, group.equal = c("loadings", "intercepts"))
summary(m2c_default_fit, remove.unused = FALSE,
   standardized = TRUE, fit.measures = TRUE)
```

## Effects-Coding Method

<h4 style="margin-top: 10px;">
The model
</h4>

The model with the equality constraints is shown below.

<img src="images/Scaling3.svg" data-fig-align="left" />

In the model statement, the loadings and the intercepts are labelled
(see the “Measurement Model” and the “Indicator intercepts” sections in
the model statement) so that the labels can be used to impose the
constraints. Constraints are imposed on the loadings and the intercepts
using the `==` operator - see the “Constraints” section in the model
statement.

Same points as before: **lavaan** will add indicator variances
automatically; constraints concerning strong metric invariance will be
attended to in the next step; and the default marker-variable method has
to be explicitely disabled.

``` r
m3 <- "
  # Measurement Model
  #   - Free the 1st loading so it can be estimated
  #   - Label the loadings so they can be used in the constraints
  POS =~ NA*p1*pos1 + p2*pos2 + p3*pos3
  NEG =~ NA*n1*neg1 + n2*neg2 + n3*neg3

  # Latent variances and covariance
  POS ~~ POS
  NEG ~~ NEG
  POS ~~ NEG

  # Indicator intercepts
  #   - Label the intercepts so they can be used in the constraints
  pos1 ~ ip1*1
  pos2 ~ ip2*1
  pos3 ~ ip3*1
  neg1 ~ in1*1
  neg2 ~ in2*1
  neg3 ~ in3*1

  # Latent means
  POS ~ 1
  NEG ~ 1

  # Constraints
  # For each construct:
  #   The sum of the loadings equals the number of indicators
  #   The sum of the intercepts equals zero
  p1 + p2 + p3 == 3
  n1 + n2 + n3 == 3

  ip1 + ip2 + ip3 == 0
  in1 + in2 + in3 == 0
"
```

#### Fit the model and get the summary

As before, the `group.equal = c("loadings", "intercepts")` statement in
the `sem()` function forces corresponding loadings and intercepts to be
equal across the groups.

``` r
m3_fit <- sem(m3, sample.cov = cov, sample.nobs = n,
   sample.mean = mean, group.equal = c("loadings", "intercepts"))
summary(m3_fit, standardized = TRUE, fit.measures = TRUE)
```

Compare the output with “Method 3” in Table 2 (pp. 64-65).

#### A shortcut

**Lavaan** can do effects-scaling automatically - set
`effect.coding = TRUE` in the `sem()` function.

``` r
m3_short <- "
  # Measurement Model
  POS =~ pos1 + pos2 + pos3
  NEG =~ neg1 + neg2 + neg3
"

m3_short_fit <- sem(m3_short, sample.cov = cov, sample.nobs = n,
   sample.mean = mean, effect.coding = TRUE,
   group.equal = c("loadings", "intercepts"))
summary(m3_short_fit, standardized = TRUE, fit.measures = TRUE)
```

## Fit measures

LS&C state that the models “produce overall model fit statistics that
are identical” (p. 66). The following shows how to extract fit measures
from all models presented here, and present them in a table.

``` r
#  A function to extract fit measures
GetFit <- function(fit, ...) {
   fitMeasures(fit, ...)
}

#  Add the fitted lavaan objects to a list
models <- list(
   "Method 1"          = m1_fit,
   "Method 1 Shortcut" = m1_short_fit,
   "Method 2c"         = m2c_fit,
   "lavaan Default"    = m2c_default_fit,
   "Method 3"          = m3_fit,
   "Method 3 Shortcut" = m3_short_fit)

#  Select the fit measures
measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
   "rmsea.ci.lower", "rmsea.ci.upper")

#  Get fit measures in a table
tab = sapply(models, GetFit, measures)
tab = t(round(tab, 4)); tab
```

Compare the fit measures with those presented on page 66.

<br />

The R script with minimal commenting is available below:

<details class="code-fold">
<summary>Code</summary>

``` r
## Little, T., Slegers, D., & Card, N. (2006). A non-arbitrary method
## of identifying and scaling latent variables in SEM and MACS models.
## Structural Equation Modeling, 13(1), 59-72.


## Load package
library(lavaan)


## Get the data from Appendix A
# 7th grade
cor7 <- c(
   1.00000,
   0.75854,  1.00000,
   0.76214,  0.78705,  1.00000,
   0.02766,  0.00973, -0.05762,  1.00000,
  -0.06112, -0.06105, -0.14060,  0.78501,  1.00000,
  -0.02222, -0.05180, -0.10250,  0.81616,  0.81076,  1.00000)

mean7 <- c(3.13552, 2.99061, 3.06945, 1.70069, 1.52705, 1.54483)
sd7   <- c(0.66770, 0.68506, 0.70672, 0.71418, 0.66320, 0.65276)
n7    <- 380

# 8th grade
cor8 <- c(
   1.00000,
   0.81366,  1.00000,
   0.84980,  0.83523,  1.00000,
  -0.18804, -0.15524, -0.21520,  1.00000,
  -0.28875, -0.24951, -0.33769,  0.78418,  1.00000,
  -0.29342, -0.21022, -0.30553,  0.79952,  0.83156,  1.00000)

mean8 <- c(3.07338, 2.84716, 2.97882, 1.71700, 1.57955, 1.55001)
sd8   <- c(0.70299, 0.71780, 0.76208, 0.65011, 0.60168, 0.61420)
n8    <- 379


## Get the variable names from Appendix A
names = c("pos1", "pos2", "pos3", "neg1", "neg2", "neg3")


## Combine into lists
cor  <- list("Grade 7" = cor7, "Grade 8" = cor8)
sd   <- list(sd7, sd8)
mean <- list(mean7, mean8)
n    <- list(n7, n8)


## Get the co/variance matrices
cov <- Map(getCov, x = cor, sds = sd, names = list(names, names))


## The model - Reference-Group Method
m1 <- "
  # Measurement Model
  #   - Free the 1st loading so it can be estimated
  POS =~ NA*pos1 + pos2 + pos3
  NEG =~ NA*neg1 + neg2 + neg3

  # Latent variances and covariance
  #   - Constrain latent variances to 1 in first group
  POS ~~ c(1,NA)*POS
  NEG ~~ c(1,NA)*NEG
  POS ~~ NEG

  # Indicator intercepts
  pos1 ~ 1
  pos2 ~ 1
  pos3 ~ 1
  neg1 ~ 1
  neg2 ~ 1
  neg3 ~ 1

  # Latent means
  #   - Constrain latent means to 0 in first group
  POS ~ c(0,NA)*1
  NEG ~ c(0,NA)*1
"


## Fit the model and get the summary
#  Compare with "Method 1" in Table 2
m1_fit <- sem(m1, sample.cov = cov, sample.nobs = n,
   sample.mean = mean, group.equal = c("loadings", "intercepts"))
summary(m1_fit, standardized = TRUE, fit.measures = TRUE)


## Reference-Group Method - Shortcut
m1_short <- "
  # Measurement Model
  POS =~ pos1 + pos2 + pos3
  NEG =~ neg1 + neg2 + neg3
"

m1_short_fit <- sem(m1_short, sample.cov = cov, sample.nobs = n, 
   sample.mean = mean, std.lv = TRUE,
   group.equal = c("loadings", "intercepts"))
summary(m1_short_fit, standardized = TRUE, fit.measures = TRUE)


## To see all means including those set to zero
summary(m1_short_fit, remove.unused = FALSE, standardized = TRUE,
   fit.measures = TRUE)


## The model - Marker-Variable Method
m2c <- "
  # Measurement Model
  #   - Free the 1st loading in POS so it can be estimated
  #   - Constrain 3rd loading in POS to 1 in both groups
  #   - Constrain 1st loading in NEG to 1 in both groups
  POS =~ NA*pos1 + pos2 + c(1,1)*pos3
  NEG =~ c(1,1)*neg1 + neg2 + neg3

  # Latent variances and covariance
  POS ~~ POS
  NEG ~~ NEG
  POS ~~ NEG

  # Indicator intercepts
  #   - Constrain 3rd intercept in POS to 0 in both groups
  #   - Constrain 1st intercept in NEG to 0 in both groups
  pos1 ~ 1
  pos2 ~ 1
  pos3 ~ c(0,0)*1
  neg1 ~ c(0,0)*1
  neg2 ~ 1
  neg3 ~ 1

  # Latent means
  POS ~ 1
  NEG ~ 1
"


## Fit the model and get the summary
#  Compare with "Method 2c" in Table 2
m2c_fit <- sem(m2c, sample.cov = cov, sample.nobs = n,
   sample.mean = mean, group.equal = c("loadings", "intercepts"))
summary(m2c_fit, standardized = TRUE, fit.measures = TRUE)


## Lavaan default method of scaling
m2c_default <- "
  # Measurement Model
  POS =~ pos1 + pos2 + pos3
  NEG =~ neg1 + neg2 + neg3
"

m2c_default_fit <- sem(m2c_default, sample.cov = cov, sample.nobs = n,
   sample.mean = mean, group.equal = c("loadings", "intercepts"))
summary(m2c_default_fit, remove.unused = FALSE,
   standardized = TRUE, fit.measures = TRUE)


## The model - Effects-Scaling Method
m3 <- "
  # Measurement Model
  #   - Free the 1st loading so it can be estimated
  #   - Label the loadings so they can be used in the constraints
  POS =~ NA*p1*pos1 + p2*pos2 + p3*pos3
  NEG =~ NA*n1*neg1 + n2*neg2 + n3*neg3

  # Latent variances and covariance
  POS ~~ POS
  NEG ~~ NEG
  POS ~~ NEG

  # Indicator intercepts
  #   - Label the intercepts so they can be used in the constraints
  pos1 ~ ip1*1
  pos2 ~ ip2*1
  pos3 ~ ip3*1
  neg1 ~ in1*1
  neg2 ~ in2*1
  neg3 ~ in3*1

  # Latent means
  POS ~ 1
  NEG ~ 1

  # Constraints
  # For each construct:
  #   The sum of the loadings equals the number of indicators
  #   The sum of the intercepts equals zero
  p1 + p2 + p3 == 3
  n1 + n2 + n3 == 3

  ip1 + ip2 + ip3 == 0
  in1 + in2 + in3 == 0
"


## Fit the model and get the summary
#  Compare with "Method 3" in Table 2
m3_fit <- sem(m3, sample.cov = cov, sample.nobs = n,
   sample.mean = mean, group.equal = c("loadings", "intercepts"))
summary(m3_fit, standardized = TRUE, fit.measures = TRUE)


## Effects-Scaling Method - Shortcut
m3_short <- "
  # Measurement Model
  POS =~ pos1 + pos2 + pos3
  NEG =~ neg1 + neg2 + neg3
"

m3_short_fit <- sem(m3_short, sample.cov = cov, sample.nobs = n,
   sample.mean = mean, effect.coding = TRUE,
   group.equal = c("loadings", "intercepts"))
summary(m3_short_fit, standardized = TRUE, fit.measures = TRUE)


## Get fit measures
#  A function to extract fit measures
GetFit <- function(fit, ...) {
   fitMeasures(fit, ...)
}

#  Add the fitted lavaan objects to a list
models <- list(
   "Method 1"          = m1_fit,
   "Method 1 Shortcut" = m1_short_fit,
   "Method 2c"         = m2c_fit,
   "lavaan Default"    = m2c_default_fit,
   "Method 3"          = m3_fit,
   "Method 3 Shortcut" = m3_short_fit)

#  Select the fit measures
measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
   "rmsea.ci.lower", "rmsea.ci.upper")

#  Get fit measures in a table
tab = sapply(models, GetFit, measures)
tab = t(round(tab, 4)); tab
```

</details>
