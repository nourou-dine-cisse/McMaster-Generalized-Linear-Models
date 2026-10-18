# STATS 4C03/6C03 Winter 2026

# Lecture 13 February 5, 2026

# Example of a logistic analysis of
# a binomial dataset
library(faraway)
esoph
?esoph # details of the dataset

# Plot the proportions with cancer
# against the levels of the covariates
esoph$props = esoph$ncases/(esoph$ncases+esoph$ncontrols)
plot(props~agegp, data=esoph)
plot(props~alcgp, data=esoph)
plot(props~tobgp, data=esoph)

# Maximal model with all interactions
esoph.mod1 = glm(cbind(ncases, ncontrols)~agegp*alcgp*tobgp,
                 data=esoph, family=binomial)

summary(esoph.mod1)
# Note that, it is not possible to fit all
# third level interaction parameters.

# Can we drop the third level interaction?
drop1(esoph.mod1, test="Chisq")

# Model with bivariate interactions only
esoph.mod2 = update(esoph.mod1, .~.-agegp:alcgp:tobgp)
summary(esoph.mod2)

# Are the bivariate interactions necessary?
drop1(esoph.mod2)

# Remove both interactions with tobgp
esoph.mod3 = glm(cbind(ncases,ncontrols)~agegp*alcgp+tobgp,
                 data=esoph, family=binomial)
summary(esoph.mod3)
drop1(esoph.mod3, test="Chisq")

# Main effects only model
esoph.mod4 = update(esoph.mod3, .~.-agegp:alcgp)
summary(esoph.mod4)
drop1(esoph.mod4)

# Compare fitted values with and without the
# agegp:alcgp interaction
fit3 = fitted(esoph.mod3, type="response")
fit4 = fitted(esoph.mod4, type="response")
plot(fit4~fit3)
abline(0,1, col="red", lwd=2)

# Residuals
esoph.pear = residuals(esoph.mod4,
                       type="pearson")
esoph.dev = residuals(esoph.mod4,
                       type="deviance")
# Compare Pearson and deviance residuals
plot(esoph.pear, esoph.dev)
abline(0,1, col="red", lwd=2)

# Normal QQ plots of the residuals
par(mfrow=c(1,2))
qqnorm(esoph.pear)
qqnorm(esoph.dev)
par(mfrow=c(1,1))

# What is the outlying point?
esoph[which.max(esoph.pear),]

# Deviance residuals against fitted values
plot(esoph.dev~fit4)
abline(h=0, lty=2, col="red")

