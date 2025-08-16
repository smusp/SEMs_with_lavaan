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




