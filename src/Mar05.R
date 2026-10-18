# STATS 4C03 Fall 2026

# Lecture 22 March 5, 2026

# Example of an analysis of a multinomial
# response variable.

Election96 = read.csv("Data/Election96.csv",
                      stringsAsFactors = TRUE)
Election96$education = factor(Election96$education,
                              levels=c("<HS", "HS", "Deg", "Grad"))
summary(Election96)

# The interest is to examine how party
# voting is related to the other factors

# First we will look at the main effects
# of the covariates without any model fitting

# Education Status
ed.tab = xtabs(~party+education, data=Election96)
ed.tab
ed.Props = sweep(ed.tab, 2, colSums(ed.tab), FUN="/")
X11()
plot(1:4, ed.Props[1,], ylab="proportion", 
     xlab="Education", xaxt="n", ylim=range(ed.Props),
     type="l", col="blue")
axis(1,1:4,colnames(ed.Props))
lines(1:4, ed.Props[2,], col="green")
lines(1:4, ed.Props[3,], col="red")
legend("topright", legend=levels(Election96$party),
       lty=1, col=c("blue","green","red"))

# So that we can examine proportions we will
# use categorical income and ages.

inc.factor = cut(Election96$income,
                 breaks=c(0,15,30,50,75, 100,150),
                 labels=c("<$15K", "$15K-$30K","$30K-$50K",
                          "$50K-$75K", "$75K-$100K", ">$100K"))
Election96$inc.gp = inc.factor
inc.tab = xtabs(~party+inc.gp, data=Election96)
inc.Props = sweep(inc.tab, 2, colSums(inc.tab), FUN="/")
plot(1:ncol(inc.Props), inc.Props[1,], ylab="proportion", 
     xlab="Income", xaxt="n", ylim=range(inc.Props),
     type="l", col="blue")
axis(1,1:ncol(inc.Props),colnames(inc.Props))
lines(1:ncol(inc.Props), inc.Props[2,], col="green")
lines(1:ncol(inc.Props), inc.Props[3,], col="red")
legend("topright", legend=levels(Election96$party),
       lty=1, col=c("blue","green","red"))

age.factor = cut(Election96$age,
                 breaks=c(0,29,39,49, 59, 74, 99),
                 labels=c("18-29", "30-39","40-49",
                          "50-59", "60-74", "75+"))
Election96$age.gp = age.factor
age.tab = xtabs(~party+age.gp, data=Election96)
age.Props = sweep(age.tab, 2, colSums(age.tab), FUN="/")
plot(1:ncol(age.Props), age.Props[1,], ylab="proportion", 
     xlab="Age", xaxt="n", ylim=range(inc.Props),
     type="l", col="blue")
axis(1,1:ncol(age.Props),colnames(age.Props))
lines(1:ncol(age.Props), age.Props[2,], col="green")
lines(1:ncol(age.Props), age.Props[3,], col="red")
legend("topright", legend=levels(Election96$party),
       lty=1, col=c("blue","green","red"))

# Now let us fit some multinomial models
library(nnet)
Elec.mod1 = multinom(party~age*income*education,
                     data=Election96)
Elec.mod1

head(fitted(Elec.mod1))

head(predict(Elec.mod1))

tab1 = table("Observed"=Election96$party, 
             "Predicted"=predict(Elec.mod1))
tab1

# What proportion are correctly identified?
sum(diag(tab1))/sum(tab1)

Elec.mod2 = multinom(party~age+income+education,
                     data=Election96)
anova(Elec.mod2, Elec.mod1, test="Chisq")

tab2 = table("Observed"=Election96$party, 
             "Predicted"=predict(Elec.mod2))
sum(diag(tab2))/sum(tab2)

# drop1 does not work on multinom output but
# we can still do such likelihood ratio tests

Elec.mod3a = multinom(party~income+education,
                      data=Election96)
anova(Elec.mod3a, Elec.mod2, test="Chisq")


Elec.mod3b = multinom(party~age+education,
                      data=Election96)
anova(Elec.mod3b, Elec.mod2, test="Chisq")

Elec.mod3c = multinom(party~age+income,
                      data=Election96)
anova(Elec.mod3c, Elec.mod2, test="Chisq")

# It looks like we can use a model with
# only income
Elec.mod4 = multinom(party~income, data=Election96)
anova(Elec.mod4, Elec.mod1, test="Chisq")

tab4 = table("Observed"=Election96$party, 
             "Predicted"=predict(Elec.mod4))
sum(diag(tab4))/sum(tab4)

# How does the predicted voting change with
# income
predict(Elec.mod4, data.frame(income=0:150))

Elec.mod5 = multinom(party~inc.gp, data=Election96)
tab5 = table("Observed"=Election96$party, 
             "Predicted"=predict(Elec.mod5))
sum(diag(tab5))/sum(tab5)

est.pi =predict(Elec.mod4, data.frame(income=0:150), 
                type="probs")
plot(0:150, est.pi[,1], type="l", col="blue",
     ylim=c(0.10, 0.60), xlab="Income",
     ylab="Estimated Probability")
lines(0:150, est.pi[,2], col="green")
lines(0:150, est.pi[,3], col="red")
legend("topright", legend=levels(Election96$party),
       lty=1, col=c("blue","green","red"))

