# STATS 4C03 Fall 2026

# Lecture 20 March 2, 2026

# A multiway table
Teens = read.csv("Data/Teens.csv")
View(Teens)

# Construct a contingency table in 
# dataframe format
Teens1 = table(Smoke=Teens$Smoke, Drink=Teens$Drink,
               Drugs=Teens$Drugs, Sexual=Teens$Sexual)
Teens1 = as.data.frame(Teens1)
Teens1

Teens.ind = glm(Freq~., data=Teens1, family=poisson)
Teens.ind
cat(paste("Deviance=", 
          signif(Teens.ind$deviance,4), 
          ", Degrees of Freedom=",
          Teens.ind$df.residual,
          ", p-value=",
          signif(pchisq(Teens.ind$deviance, 
                        Teens.ind$df.residual,
                        lower=FALSE),4), sep=""))


# Is sexual activity related to the other factors?
# We can fit the fully saturated model
Teens.sat = glm(Freq~(Smoke+Drink+Drugs+Sexual)^4,
                data=Teens1, family=poisson)

# Now fit the model where Sexual activity
# is independent of the other factors
Teens.mod2 = glm(Freq~Smoke*Drink*Drugs+Sexual,
                 data=Teens1, family=poisson)
Teens.mod2
summary(Teens.mod2)
anova(Teens.mod2, Teens.sat, test="Chisq")

# Is drug taking independent of the other factors?
# Based on what we already know we retain
# the independence structure between sexual
# activity and all other factors.
Teens.mod3 = glm(Freq~Smoke*Drink+Drugs+Sexual,
                 data=Teens1, family=poisson)
anova(Teens.mod3, Teens.sat, test="Chisq")
anova(Teens.mod3, Teens.mod2, test="Chisq")

# Drug taking is not independent of 
# drinking and smoking.

# Conditional on smoking status is drug use
# independent of drinking?
Teens.mod4 = glm(Freq~Smoke*(Drink+Drugs)+Sexual,
                 data=Teens1, family=poisson)
anova(Teens.mod4, Teens.mod2, test="Chisq")

# Conditional on drinking status, is drug
# use independent of smoking?
Teens.mod5 = glm(Freq~Drink*(Smoke+Drugs)+Sexual,
                 data=Teens1, family=poisson)
anova(Teens.mod5, Teens.mod2, test="Chisq")

# Model 4 seems to fit the data well
# and is quite parsimonious. It only
# has 2 second order interactions.

# Comparing to the saturated model
anova(Teens.mod4, Teens.sat) # Deviance

Teens1$fits=fitted(Teens.mod4)
Teens.Pearson = sum((Teens1$Freq-Teens1$fits)^2/Teens1$fits)
Teens.Pearson
pchisq(Teens.Pearson, Teens.mod4$df.residual,
       lower=F) # Pearson
# In either test we fail to reject the null
# hypothesis so can conclude that the staurated
# model does not fit much better than the
# reduced model (Model 4)

# As always diagnostics are important!
Teens1$rdev=residuals(Teens.mod4) # Deviance Residuals
plot(rdev~fits, data=Teens1,
     xlab="Fitted", ylab="residuals")
abline(h=0, lty=2)

# glm.diag in library(boot) allows us to
# get standardized deviance (or Pearson)
# residuals.
library(boot)
Teens1$rdev.std = glm.diag(Teens.mod4)$rd
plot(rdev.std~fits, data=Teens1,
     xlab="Fitted", ylab="residuals")
abline(h=0, lty=2)

qqnorm(Teens1$rdev.std)
abline(0,1,col="red")



