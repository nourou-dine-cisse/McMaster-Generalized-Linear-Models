# STATS 4C03/6C03 Winter 2026

# Lecture 7, January 21, 2026.

Cars.mod1 = lm(Price~Horsepower+Country, data=Cars,
               x=TRUE)
Cars.mod1
# There is much more returned than we see
# on the screen!  
names(Cars.mod1)

# We can extract some parts using accessor functions
coef(Cars.mod1)

plot(fitted(Cars.mod1), residuals(Cars.mod1),
     xlab="Fitted Values", ylab="Residuals")
abline(h=0, lty=2, col="red")

# It is recommended to use standardized
# residuals which all have the same variance
Cars.mod1.r = rstandard(Cars.mod1)
qqnorm(Cars.mod1.r)
abline(0,1, col="red")

# We can get more information from the summary
summary(Cars.mod1)

# Again what is printed is not all there is!
Cars.summ1 = summary(Cars.mod1)
names(Cars.summ1)

# Wald test of whether Country is needed
R=rbind(c(0,0,1,0,0), c(0,0,0,1,0), c(0,0,0,0,1))
q=nrow(R)
Rbhat=R%*%coef(Cars.mod1)
Vbhat=Cars.summ1$cov.unscaled
VRbhat=R%*%Vbhat%*%t(R)
MSE1=Cars.summ1$sigma^2
Fstat1=(t(Rbhat)%*%solve(VRbhat)%*%Rbhat)/(q*MSE1)

c("F"=Fstat1, 
  "p.val"=pf(Fstat1,q,Cars.mod1$df.residual, lower=F))

# Likelihood ratio test for the same hypothesis
Cars.mod2 = lm(Price~Horsepower, data=Cars)
SSE1=sum(residuals(Cars.mod1)^2)
SSE2=sum(residuals(Cars.mod2)^2)
Fstat2=((SSE2-SSE1)/q)/MSE1
c("F"=Fstat2, 
  "p.val"=pf(Fstat2,q,Cars.summ1$df[2], lower=F))

# The anova function simplifies this
anova(Cars.mod2, Cars.mod1)

# The model above has different intercepts 
# for different countries but the same slopes.
# A full model allows the intercepts and slopes
# to be different.
Cars.mod3=lm(Price~Horsepower*Country, 
             data=Cars, x=TRUE)
summary(Cars.mod3)

# We can compare this model with the model having
# parallel lines relating Price to Horsepower
anova(Cars.mod1, Cars.mod3)

# A test of whether Country is needed at all
# would compare the full model with the model
# only having Horsepower
anova(Cars.mod2, Cars.mod3)

# Our conclusion would be that there is no real
# evidence that the lines relating Price to 
# Horsepower differ in slope across countries but
# there is evidence of a difference in
# the intercepts.