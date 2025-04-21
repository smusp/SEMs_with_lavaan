

## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling 
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.


## Load package
library(lavaan)
library(here)             # Relative paths

## Get the data
path <- here::here("Green_2023", "data", "satisfactionII.csv")
df <- read.csv(path, header = TRUE)
head(df)


## One-way ANOVA of latent variable
# Model statements
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


## Check results in "All measures" row in Table 21.6
# Fit the models 
fit <- lapply(models, sem, data = df, group = "x")

# Get model summaries
lapply(fit, summary)

# Contrast model fits
Reduce(anova, fit)


## Cut-and-paste means and variances to get effect sizes
## Compare with values given on page 405
d1 <- (0.664 - 0) / sqrt(8.135); d1    # "no strategy" vs "discussion"
d2 <- (1.945 - 0) / sqrt(8.135); d2    # "no strategy" vs "exercise"


## Extract latent means and error variances from "Less Constrained" model
## Check with "All measures" row in TAble 21.6
estimates <- lavInspect(fit[["Less Constrained"]], "est"); estimates
   # Note: latent means are in element "alpha"
   #       latent error variances are in element "psi"

LatentMeans <- do.call(rbind, lapply(estimates, "[[", "alpha")); LatentMeans
LatentVar <- do.call(rbind, lapply(estimates, '[[', 'psi')); LatentVar

## Effect sizes
## Compare with values given on page 405
d1 <- (LatentMeans[2] - LatentMeans[1]) / sqrt(LatentVar[1]); d1   # "no strategy" vs "discussion"
d2 <- (LatentMeans[3] - LatentMeans[1]) / sqrt(LatentVar[1]); d2   # "no strategy" vs "exercise"


## ANOVA model for 2nd row in Table 21.6
# Model statements
common <- "
   # Measurement model
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


## Check with means, error variance, and chi square in 2nd row in Table 21.6
# Fit the models 
fit <- lapply(models, sem, data = df, group = "x")

# Model summaries
lapply(fit, summary)

# Get the latent means and latent error variances for "Less Constrained" model
estimates <- lavInspect(fit[["Less Constrained"]], "est"); estimates
LatentMeans <- do.call(rbind, lapply(estimates, "[[", "alpha")); LatentMeans
LatentVar <- do.call(rbind, lapply(estimates, '[[', 'psi')); LatentVar

# Contrast model fits
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
MeansY1 <- do.call(cbind, lapply(sampstat, "[[", "mean"))
MeansY1 <- MeansY1[1,]; MeansY1   # Compare with 2nd row in Table 21.6

# Differences between y1 means are differencs between latent means
MeansY1[2] - MeansY1[1]
MeansY1[3] - MeansY1[1]

# Compare with latent means
LatentMeans

# Alternatively, the y1 intercepts (which are constrained to equality)
# added to the latent means give the y1 means
intercepts <- do.call(cbind, lapply(estimates, "[[", "nu"))[1,1]; intercepts
intercepts + LatentMeans; MeansY1


# Extract y1 variances from sampstats
VarY1 <- do.call(cbind, lapply(lapply(sampstat, "[[", "cov"), diag))
VarY1 <- VarY1[1,]; VarY1  # Compare with 2nd row in Table 21.6

# Extract residual variances for y1 from estimates
ResidVarY1 <- lapply(lapply(estimates, '[[', 'theta'), diag)
ResidVarY1 <- do.call(rbind, lapply(ResidVarY1, '[', 1)); ResidVarY1 # Compare with 2nd row in Table 21.6

# Differences between y1 variances and y1 residual variances are latent error variances
VarY1 - ResidVarY1

# Compare with the latent error variances
LatentVar


## ANOVA model for 3rd row in Table 21.6
# Model statements
common <- "
   # Measurement model
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


## Check with means, error variance, and chi square in 3rd row in Table 21.6
# Fit the models 
fit <- lapply(models, sem, data = df, group = "x")

# Model summaries
lapply(fit, summary)

# Get the latent means and latent error variances for "Less Constrained" model
estimates <- lavInspect(fit[["Less Constrained"]], "est"); estimates
LatentMeans <- do.call(rbind, lapply(estimates, "[[", "alpha")); LatentMeans
LatentVar <- do.call(rbind, lapply(estimates, '[[', 'psi')); LatentVar

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
MeansY2 <- do.call(cbind, lapply(sampstat, "[[", "mean"))
MeansY2 <- MeansY2[2,]; MeansY2   # Compare with 3rd row in Table 21.6

# Differences between y2 means are differencs between latent means
MeansY2[2] - MeansY2[1]
MeansY2[3] - MeansY2[1]

# Compare with latent means
LatentMeans

# Alternatively, the y2 intercepts (which are constrained to equality)
# added to the latent means give the Y2 means
intercepts <- do.call(cbind, lapply(estimates, "[[", "nu"))[2,1]; intercepts
intercepts + LatentMeans; MeansY2


# Extract y2 variances from sampstats
VarY2 <- do.call(cbind, lapply(lapply(sampstat, "[[", "cov"), diag))
VarY2 <- VarY2[2,]; VarY2  # Compare with 3rd row in Table 21.6

# Extract residual variances for y2 from estimates
ResidVarY2 <- lapply(lapply(estimates, '[[', 'theta'), diag)
ResidVarY2 <- do.call(rbind, lapply(ResidVarY2, '[', 2)); ResidVarY2 # Compare with 3rd row in Table 21.6

# Differences between y2 variances and y2 residual variances are latent error variances
VarY2 - ResidVarY2

# Compare with the latent error variances
LatentVar
