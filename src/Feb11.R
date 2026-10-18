# STATS 4C03/6C03 Winter 2026

# Lecture 15 February 11, 2026

# Example of a Poisson regression using
# a dataset in the faraway package which
# gives some geographical data on the 30 
# Galapagos Islands along with the number
# of distinct plant species on the island.

# number of distinct species 
library(faraway)
# I will delete the second column since
# we will not use it in the analysis.
gala1 = gala[,-2]

# Fitted Model using the log link
gala.glm = glm(Species~., data=gala1,
               family=poisson)
summary(gala.glm)

# For comparison here is the same data
# modelled using the square root link
gala.glm.sq = glm(Species~., data=gala1,
                  family=poisson("sqrt"))
summary(gala.glm.sq)

# Wald tests for the individual Beta
# parameters being zero
summary(gala.glm)$coef

# Likelihood ratio tests for the same
# hypotheses
drop1(gala.glm, test="Chisq")

# These could also be done individually
# For example to test if the beta for 
# Area is equal to zero (when all others
# are included in the model)
gala.glm1 = glm(Species~.-Area, data=gala1, 
                family=poisson)
anova(gala.glm1, gala.glm, test="Chisq")

gala.rd = residuals(gala.glm)
gala.rp = residuals(gala.glm, type="pearson")
gala.fits = fitted(gala.glm)
plot(gala.rd, gala.rp, 
     xlab="Deviance", ylab="Pearson")
abline(0,1)
# Diagnostic plots of residuals
# against fitted values
plot(gala.rd~gala.fits, 
     ylim=range(c(gala.rd, gala.rp)),
     xlab="Fitted", ylab="Residuals")
abline(h=0, lty=2)
points(gala.rp~gala.fits, col="red")

phi.hat = sum(gala.rp^2)/gala.glm$df.residual

summary(gala.glm, dispersion=phi.hat)
drop1(gala.glm, dispersion=phi.hat)

gala.glm1 = glm(Species~., data=gala1,
                family=quasipoisson)
