
#################################################
##
## Effects Scaling
## 
## as presented by Little, Slegers, & Card (2006)
##
## using lavaan 
##
#################################################




## Little, T., Slegers, D., & Card,, N. (2006). A Non-arbitrary method of 
## identifying and scaling latent variables in SEM and MACS models.
## Structural Equation Modeling, 13(1), 59-72.




## Load lavaan
library(lavaan)


## Get the data
# Appendix A (pp. 71-72) gives Lisrel input code which contains vectors of 
# correlations (lower triangle), means, and standard deviations for two groups: 7th grade and 8th grade;
# that is, there is sufficient information to generate the covariance matrices for the two groups.

# 7th grade
vcor7 <- c(
   1.00000,
   0.75854,  1.00000,
   0.76214,  0.78705,  1.00000,
   0.02766,  0.00973, -0.05762,  1.00000,
  -0.06112, -0.06105, -0.14060,  0.78501,  1.00000,
  -0.02222, -0.05180, -0.10250,  0.81616,  0.81076,  1.00000)

means7 <- c(3.13552, 2.99061, 3.06945, 1.70069, 1.52705, 1.54483)
sd7 <- c(0.66770, 0.68506, 0.70672, 0.71418, 0.66320, 0.65276)
n7 <- 380

# 8th grade
vcor8 <- c(
   1.00000,
   0.81366,  1.00000,
   0.84980,  0.83523,  1.00000,
  -0.18804, -0.15524, -0.21520,  1.00000,
  -0.28875, -0.24951, -0.33769,  0.78418,  1.00000,
  -0.29342, -0.21022, -0.30553,  0.79952,  0.83156,  1.00000)

means8 <- c(3.07338, 2.84716, 2.97882, 1.71700, 1.57955, 1.55001)
sd8 <- c(0.70299, 0.71780, 0.76208, 0.65011, 0.60168, 0.61420)
n8 <- 379

labels = c("PosAFF1", "PosAFF2", "PosAFF3", "NegAFF1", "NegAFF2", "NegAFF3")

# Get covariances using getCov() function from lavaan
cov7 <- getCov(vcor7, sds = sd7, names = labels)
cov8 <- getCov(vcor8, sds = sd8, names = labels)

# Combine into lists
cov <- list("Grade 7" = cov7, "Grade 8" = cov8)
means <- list(means7, means8)
n <- list(n7, n8)


## The model - with effects coding identification:
#    Restrict loadings to sum to the number of indicators per factor;
#    Restrict intercepts to sum to zero per factor;
#    See 'Constraints' at the bottom of the model
#
# In lavaan, the default referent loading scaling method has to be explicitely disabled.  
# Let lavaan add the indicator variances automatically.
# A rather verbose model statement follows.
# See m3_short below for a shortcut to get effect scaling automatically.

m3 <- '
   # Measurement Model - Free the first loading so it can be estimated
   #                   - Label the loadings so they can be used in the constraints
   Pos =~ NA*lmda11*PosAFF1 + lmda12*PosAFF2 + lmda13*PosAFF3
   Neg =~ NA*lmda21*NegAFF1 + lmda22*NegAFF2 + lmda23*NegAFF3

   # Factor variances and covariance
   Pos ~~ Pos
   Neg ~~ Neg
   Pos ~~ Neg

   # Indicator intercepts - Label the intercepts so they can be used in the constraints
   PosAFF1 ~ nu11*1
   PosAFF2 ~ nu12*1
   PosAFF3 ~ nu13*1
   NegAFF1 ~ nu21*1
   NegAFF2 ~ nu22*1
   NegAFF3 ~ nu23*1

   # Factor means
   Pos ~ 1
   Neg ~ 1

   # Constraints
   # For each factor: 
   #    The sum of the loadings equals the number of indicators
   #    The sum of the intercepts equals zero
   lmda11 + lmda12 + lmda13 == 3
   lmda21 + lmda22 + lmda23 == 3

   nu11 + nu12 + nu13 == 0
   nu21 + nu22 + nu23 == 0
'


## Run the model
# Little et. al. state that the data display strong metric invariance (p. 63);
# that is, the corresponding loadings and intercepts are equal across the groups,
# but the corresponding indicator residuals vary across the groups.
# Hence the "group.equal" statement in the lavaan() function below.
# The factor variances, covariances, and means also vary across the groups.
# (This could also have been worked out by careful inspection of Table 2).

m3_fit <- sem(m3, sample.cov = cov, sample.nobs = n, sample.mean = means, 
              group.equal = c("loadings", "intercepts"))
summary(m3_fit, standardized = TRUE, fit.measures = TRUE)


## Compare the output with "Method 3" in Table 2 (pp. 64-75).
# In the Table: 
#    phi is a factor (co)variance,
#    kappa is a factor mean,
#    r is a factor correlation.


## A shortcut: lavaan can set effects scaling automatically -  
# set meanstructure and effect.coding to TRUE.

m3_short <- '
   # Measurement Model
   Pos =~ PosAFF1 + PosAFF2 + PosAFF3
   Neg =~ NegAFF1 + NegAFF2 + NegAFF3
'

m3_short_fit <- sem(m3_short, sample.cov = cov, sample.nobs = n, sample.mean = means, 
              meanstructure = TRUE, effect.coding = TRUE,
              group.equal = c("loadings", "intercepts"))
summary(m3_short_fit, standardized = TRUE, fit.measures = TRUE)



### Factor mean scaling and Referent intercept scaling

## Factor mean scaling - factor means and their variances are constrained in one group
# A verbose model statement follows.
# See m1_short below for a shortcut to get factor scaling automatically.

m1 <- '
   # Measurement Model - Free the first loading so it can be estimated
   Pos =~ c(NA,NA)*PosAFF1 + PosAFF2 + PosAFF3
   Neg =~ c(NA,NA)*NegAFF1 + NegAFF2 + NegAFF3

   # Factor variances and covariance - Constrain factor variances to 1 in first group
   Pos ~~ c(1,NA)*Pos
   Neg ~~ c(1,NA)*Neg
   Pos ~~ Neg

   # Indicator intercepts
   PosAFF1 ~ 1
   PosAFF2 ~ 1
   PosAFF3 ~ 1
   NegAFF1 ~ 1
   NegAFF2 ~ 1
   NegAFF3 ~ 1

   # Factor means - Constrain factor means to 0 in first group
   Pos ~ c(0,NA)*1
   Neg ~ c(0,NA)*1
'

m1_fit <- sem(m1, sample.cov = cov, sample.nobs = n, sample.mean = means, 
              group.equal = c("loadings", "intercepts"))
summary(m1_fit, standardized = TRUE, fit.measures = TRUE)

# Compare the output with "Method 1" in Table 2 (pp. 64-75).


## Note: lavaan can set factor scaling automatically - 
#        set std.lv.
# The constraints are the same as above -
#        Factor variances are constrained to one in first group;
#        Factor means are constrained to zero in first group;

m1_short <- '
   # Measurement Model - 
   Pos =~ PosAFF1 + PosAFF2 + PosAFF3
   Neg =~ NegAFF1 + NegAFF2 + NegAFF3
'

m1_short_fit <- sem(m1_short, sample.cov = cov, sample.nobs = n, sample.mean = means, 
               std.lv = TRUE, group.equal = c("loadings", "intercepts"))
summary(m1_short_fit, remove.unused = FALSE, standardized = TRUE, fit.measures = TRUE)


## Referent intercept scaling - one indicator's loading and its intercept in each factor 
#  are constrained to one and zero respectively in both groups.
#  This differs from lavaan's default referent scaling.
#  See m2c_default below.

m2c <- '
   # Measurement Model - Free the first loading so it can be estimated
   #                   - Constrain 3rd indicator in Pos factor to 1 in both groups
   #                   - Constrain 1st indicator is Neg factor to 1 in both groups
   Pos =~ c(NA,NA)*PosAFF1 + PosAFF2 + c(1,1)*PosAFF3
   Neg =~ c(1,1)*NegAFF1 + NegAFF2 + NegAFF3

   # Factor variances and covariance
   Pos ~~ Pos
   Neg ~~ Neg
   Pos ~~ Neg

   # Indicator intercepts - Constrain 3rd residual variance in Pos factor to 0 in both groups
   #                      - Constrain 1st residual variance in Neg factor to 0 in both groups
   PosAFF1 ~ 1
   PosAFF2 ~ 1
   PosAFF3 ~ c(0,0)*1
   NegAFF1 ~ c(0,0)*1
   NegAFF2 ~ 1
   NegAFF3 ~ 1

   # Factor means 
   Pos ~ 1
   Neg ~ 1
'

m2c_fit <- sem(m2c, sample.cov = cov, sample.nobs = n, sample.mean = means, 
               group.equal = c("loadings", "intercepts"))
summary(m2c_fit, standardized = TRUE, fit.measures = TRUE)

# Compare the output with "Method 2c" in Table 2 (pp. 64-75). 


## The methods presented here produce identical model fits.

GetFit <- function(output) {
  fitMeasures(output, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea"))
}

data.frame(rbind(GetFit(m1_fit), GetFit(m1_short_fit),GetFit(m2c_fit), GetFit(m2c_default_fit), GetFit(m3_fit), GetFit(m3_short_fit)), 
           row.names = c("Method 1", "Method 1 Shortcut", "Method 2c", "Method 2c Default", "Method 3", "Method 3 Shortcut")) 



## Note: The default referent scaling in lavaan - 
#        loading for first indicator constrained to one for both factors and in both groups;
#        Factor means constrained to zero for both factors in the first group only.

m2c_default <- '
   # Measurement Model - 
   Pos =~ PosAFF1 + PosAFF2 + PosAFF3
   Neg =~ NegAFF1 + NegAFF2 + NegAFF3
'

m2c_default_fit <- sem(m2c_default, sample.cov = cov, sample.nobs = n, sample.mean = means, 
               meanstructure = TRUE, 
               group.equal = c("loadings", "intercepts"))
summary(m2c_default_fit, standardized = TRUE, remove.unused = FALSE, fit.measures = TRUE)


parameterestimates(m2c_default_fit)