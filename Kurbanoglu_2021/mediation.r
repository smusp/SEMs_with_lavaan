
library(lavaan)

# Summary statistics are provided in Table 1 (p.50):
# correlations, standard deviations, means, sample size.
vcor <- c(1,
          0.30,  1,
         -0.42, -0.32,  1)
sd <- c(8.81, 7.95, 18.30)
means <- c(56.57, 40.39, 68.22)
n <- 513

# Variables and their labels are:
# Att - Physics lab attitudes
# SE  - Self-efficacy related to learn
# Anx - Physics lab anxiety
labels <- c("Att", "SE", "Anx")

# Get the covriance matrix using the getCov() function (from the lavaan package)
cov = getCov(vcor, sds = sd, names = labels)

# The model is given in Figure 1 (p.51).
# It seems the authors do not include estimaation of the indirect effect  
# in their model, but it is easy enough to include.
model <- "
   # direct effect
   Anx ~ cpr * SE   # c prime

   # via the mediator
   Att ~ a * SE
   Anx ~ b * Att

   # indirect effect (a * b)
   ab := a * b

   # total effect
   total := cpr + (a * b)
"

# Run the model, and get the model summary and fit measures.
# I've asked for R-squared values, standardised estimates (see "std.all" column in the output),
# and the fit measures.
fit <- sem(model, sample.cov = cov, sample.nobs = n)
summary(fit, rsquare = TRUE, standardized = TRUE)
fitMeasures(fit)


# If the intercepts are required, include "sample.mean = means" is the sem() function.
fit <- sem(model, sample.cov = cov, sample.means = means, sample.nobs = n)
