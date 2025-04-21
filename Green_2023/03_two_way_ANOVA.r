

## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling 
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.


## Load packages
library(lavaan)
library(here)         # Relative paths
library(DescTools)    # Cramer's V

## Get the data
path <- here::here("Green_2023", "data", "ANOVA_data.r")
source(path)
head(df)


## Cramer's V
## Check with page 394
DescTools::CramerV(df$g, df$x)


## Cramer's V by hand
chisq <- unname(chisq.test(df$g, df$x)$statistic)
n <- length(df$g)           # Sample size
r <- length(unique(df$g))   # Number of rows
c <- length(unique(df$x))   # Number of columns

CV <- sqrt((chisq/n)/min(r-1, c-1)); CV


## Direction of the relationship
chisq.test(df$g, df$x)$stdres 


## Cell means and cell frequencies
## Check cell means and frequencies in Table 21.3
means <- tapply(df$y, list(df$g, df$x), mean); means     # Cell means
freq <- table(df$g, df$x); freq                          # Cell frequencies


## Check unweighted and weighted means in Table 21.3
# Unweighted marginal means
apply(means, 1, mean)      # Gender
apply(means, 2, mean)      # Coping Strategy
 
# Weighted marginal means
tapply(df$y, df$g, mean)     # Gender
tapply(df$y, df$x, mean)     # Coping Strategy   


### Two-way ANOVA - SEM
unique(df$sg)    # Order in which groups appear in lavaan output

# Less Constrained model
lc <- "y ~ c(am, af, bm, bf, cm, cf)*1       # Means
       y ~~ c(e, e, e, e, e, e)*y            # Variances"

lc.fit <- sem(lc, data = df, group = "sg")
summary(lc.fit)


# Gender main effect - unweighted means
constraints <- "af + bf + cf == am + bm + cm"
gend_unw <- c(lc, constraints)

gend_unw.fit <- sem(gend_unw, data = df, group = "sg")
summary(gend_unw.fit)

anova(gend_unw.fit, lc.fit)   # Compare the two models


# Coping Strategy main effect - unweighted means
constraints <- "
   af + am == bf + bm 
   af + am == cf + cm"
strat_unw <- c(lc, constraints)

strat_unw.fit <- sem(strat_unw, data = df, group = "sg")
summary(strat_unw.fit)

anova(strat_unw.fit, lc.fit)   # Compare the two models


# Gender main effect - weighted means
freq                     # To assist with constructing constraints 
constraints <- "(3*af + 3*bf + 6*cf)/12 == (6*am + 3*bm + 3*cm)/12"
gend_w = c(lc, constraints)

gend_w.fit <- sem(gend_w, data = df, group = "sg")
summary(gend_w.fit)

anova(gend_w.fit, lc.fit)   # Compare the two models


# Coping Strategy main effect - weighted means
# Compare with SEM section in Table 21.4
freq
constraints <- "
   (3*af + 6*am)/9 == (3*bf + 3*bm)/6 
   (3*bf + 3*bm)/6 == (6*cf + 3*cm)/9"
strat_w <- c(lc, constraints)

strat_w.fit <- sem(strat_w, data = df, group = "sg")
summary(strat_w.fit)

anova(strat_w.fit, lc.fit)   # Compare the two models


# Gender X Coping Strategy interaction
constraints <- "
   (af - am) == (bf - bm) 
   (bf - bm) == (cf - cm)"
inter <- c(lc, constraints)

inter.fit <- sem(inter, data = df, group = "sg")
summary(inter.fit)

anova(inter.fit, lc.fit)     # Compare the two models
