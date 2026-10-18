# STATS 4C03 Fall 2026

# Lecture 23 March 9, 2026

# An example of multinomial regression
# with multiple observations at each 
# covariate vector.
CarPref = read.csv("Data/CarPref.csv")
CarPref$sex = factor(CarPref$sex, 
                     levels=c("women", "men"))
CarPref$age = factor(CarPref$age, 
                     levels=c("18-23", "24-40", "> 40"))
CarPref$response = factor(CarPref$response,
                          levels=c("no/little", "important", "very important"))

# The response variable is the degree of importance
# attached to air-conditioning and power-steering 
# in a car.  3 levels - no/little importance, important
# and very important

# Here is a graphical look at the observed data 
CarPref$totals = rep(tapply(CarPref$frequency, 
                            CarPref[,2:1], sum), 
                     each=3)
CarPref$prop = CarPref$frequency/CarPref$totals

X11
plot(rep(c(1:3,1:3), each=3), CarPref$prop,
     col=rep(c("red", "blue"), each=9), ylim=c(0,1),
     pch=16, axes=F, xlab="Age", ylab="Proportion")
axis(1, 1:3, levels(CarPref$age))
axis(2, (0:5)/5)
box()
lines(1:3, CarPref$prop[CarPref$sex=="women" &
                          CarPref$response=="no/little"],
      col="red", lty=3)
lines(1:3, CarPref$prop[CarPref$sex=="women" &
                          CarPref$response=="important"],
      col="red", lty=2)
lines(1:3, CarPref$prop[CarPref$sex=="women" &
                          CarPref$response=="very important"],
      col="red", lty=1)
lines(1:3, CarPref$prop[CarPref$sex=="men" &
                          CarPref$response=="no/little"],
      col="blue", lty=3)
lines(1:3, CarPref$prop[CarPref$sex=="men" &
                          CarPref$response=="important"],
      col="blue", lty=2)
lines(1:3, CarPref$prop[CarPref$sex=="men" &
                          CarPref$response=="very important"],
      col="blue", lty=1)
legend("topleft", lty=rep(3:1,2),
       col=rep(c("red","blue"), each=3),
       legend=c("W none", "W imp", "W very",
                "M none", "M imp", "M very"))

library(nnet)
CarPref.mod = multinom(response~sex+age, 
                       weights=frequency,
                       data=CarPref)

CarPref.sat = multinom(response~sex*age, 
                       weights=frequency,
                       data=CarPref)
anova(CarPref.mod, CarPref.sat, test="Chisq")

fitted(CarPref.mod)

CarPref$fit.prob = as.vector(t(fitted(CarPref.mod)[3*(1:6),]))
CarPref$fit.freq = CarPref$fit.prob*CarPref$totals

CarPref$Pearson = (CarPref$frequency-CarPref$fit.freq)/
  sqrt(CarPref$fit.freq)
sum(CarPref$Pearson^2)
