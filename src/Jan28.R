# STATS 4C03/6C03 Winter 2026

# Lecture 9-11 January 28-February 2, 2026

# Newton-Raphson for single covariate 
# logistic regression

# We shall consider the (simplified) Space
# Shuttle O-ring data.

shuttle = read.csv("Shuttle.csv")
plot(damage~temp, data=shuttle, 
     ylim=c(-0.5, 1.5))
# Incorrect linear model
shuttle.lm = lm(damage~temp, data=shuttle)
abline(shuttle.lm, col="red")

h.logit = function(beta, y, x) {
  beta0=beta[1]
  beta1=beta[2]
  eta.i = beta0+beta1*x
  pi.i = exp(eta.i)/(1+exp(eta.i))
  c(sum(y)-sum(pi.i), sum(y*x)-sum(x*pi.i))
}

J.logit = function(beta, x){
  beta0=beta[1]
  beta1=beta[2]
  eta.i = beta0+beta1*x
  pi.i = exp(eta.i)/(1+exp(eta.i))
  vi = pi.i*(1-pi.i)
  cbind(c(sum(vi),sum(x*vi)),
        c(sum(x*vi), sum(x^2*vi)))
}

# We need initial values
# Let us consider the null case where there
# is no relationship at all between temperature
# and o-ring damage
pi.hat0 = mean(shuttle$damage)
beta.hat=c(log(pi.hat0/(1-pi.hat0)), 0)
dl = h.logit(beta.hat, shuttle$damage, shuttle$temp)

# Iteration 1
J = J.logit(beta.hat, shuttle$temp)
beta.hat.new = beta.hat+solve(J)%*%dl
dl = h.logit(beta.hat.new, shuttle$damage, shuttle$temp)
beta.hat = beta.hat.new[,1]
cbind(beta.hat, dl)


# Iteration 2
J = J.logit(beta.hat, shuttle$temp)
beta.hat.new = beta.hat+solve(J)%*%dl
dl = h.logit(beta.hat.new, shuttle$damage, shuttle$temp)
beta.hat = beta.hat.new[,1]
cbind(beta.hat, dl)

# Iteration 3
J = J.logit(beta.hat, shuttle$temp)
beta.hat.new = beta.hat+solve(J)%*%dl
dl = h.logit(beta.hat.new, shuttle$damage, shuttle$temp)
beta.hat = beta.hat.new[,1]
cbind(beta.hat, dl)

#Iteration 4
J = J.logit(beta.hat, shuttle$temp)
beta.hat.new = beta.hat+solve(J)%*%dl
dl = h.logit(beta.hat.new, shuttle$damage, shuttle$temp)
beta.hat = beta.hat.new[,1]
cbind(beta.hat, dl)

# Iteration 5
J = J.logit(beta.hat, shuttle$temp)
beta.hat.new = beta.hat+solve(J)%*%dl
dl = h.logit(beta.hat.new, shuttle$damage, shuttle$temp)
beta.hat = beta.hat.new[,1]
cbind(beta.hat, dl)


# Iteratively Reweighted Least squares
# This gives the EXACT same sequence
# of estimates using the lm function.

ginv = function(x) exp(x)/(1+exp(x))
shuttle.X = cbind(1, shuttle$temp)
beta.hat1=c(log(pi.hat0/(1-pi.hat0)), 0)
beta.hat0 = beta.hat1
pi.hat1 = ginv(shuttle.X%*%beta.hat0)[,1]
Wii = pi.hat1*(1-pi.hat1)
Zi = shuttle.X%*%beta.hat0+
  (shuttle$damage-pi.hat1)/Wii
beta.hat1 = coef(lm(Zi~shuttle$temp, 
                    weights=Wii))


beta.hat0 = beta.hat1
pi.hat1 = ginv(shuttle.X%*%beta.hat1)[,1]
Wii = pi.hat1*(1-pi.hat1)
Zi = shuttle.X%*%beta.hat1+
  (shuttle$damage-pi.hat1)/Wii
beta.hat1 = coef(lm(Zi~shuttle$temp,
                    weights=Wii))
sum((beta.hat1-beta.hat0)^2)

beta.hat0 = beta.hat1
pi.hat1 = ginv(shuttle.X%*%beta.hat1)[,1]
Wii = pi.hat1*(1-pi.hat1)
Zi = shuttle.X%*%beta.hat1+
  (shuttle$damage-pi.hat1)/Wii
beta.hat1 = coef(lm(Zi~shuttle$temp,
                    weights=Wii))
sum((beta.hat1-beta.hat0)^2)

beta.hat0 = beta.hat1
pi.hat1 = ginv(shuttle.X%*%beta.hat1)[,1]
Wii = pi.hat1*(1-pi.hat1)
Zi = shuttle.X%*%beta.hat1+
  (shuttle$damage-pi.hat1)/Wii
beta.hat1 = coef(lm(Zi~shuttle$temp,
                    weights=Wii))
sum((beta.hat1-beta.hat0)^2)

beta.hat0 = beta.hat1
pi.hat1 = ginv(shuttle.X%*%beta.hat1)[,1]
Wii = pi.hat1*(1-pi.hat1)
Zi = shuttle.X%*%beta.hat1+
  (shuttle$damage-pi.hat1)/Wii
beta.hat1 = coef(lm(Zi~shuttle$temp,
                    weights=Wii))
sum((beta.hat1-beta.hat0)^2)

# The asymptotic standard errors
# come from the diagnonal entries
# of the final J matrix
V = solve(J.logit(beta.hat, shuttle$temp))
cbind(beta.hat, sqrt(diag(V)))

# Note that when using IRLS we need
# to use the UNSCALED standard errors
# although they should not be too 
# different from the scaled versions
V1 = summary(lm(Zi~shuttle$temp, 
                weights=Wii))$cov.unscaled
cbind(beta.hat1, sqrt(diag(V1)))

# Of course usually we will just use
# the glm function to do all this
summary(glm(damage~temp, data=shuttle,
            family=binomial))
# Note that logit is the default link
# For other links use, for example,
# family=binomial("probit")
# Other possible links are "cloglog",
# or "cauchit" (also "log" but that is 
# generally not recommended)

# Asymptotic Wald Confidence Intervals
shuttle.summ = summary(glm(damage~temp, data=shuttle, 
                           family=binomial))
shuttle.ests = coef(shuttle.summ)
cbind(shuttle.ests[,1]-qnorm(0.975)*shuttle.ests[,2],
      shuttle.ests[,1]+qnorm(0.975)*shuttle.ests[,2])

# Likelihood Ratio Test for beta_1=0
test.stat = shuttle.summ$null.deviance-
  shuttle.summ$deviance
pval = pchisq(test.stat, 1, lower=F)
c(LRT=test.stat, P.Value=pval)

# Alternatively
drop1(shuttle.mod, test="Chisq")
