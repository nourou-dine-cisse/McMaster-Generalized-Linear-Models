# STATS 4C03 Fall 2026

# Lectures 18-19 February 25-26, 2026

# 2x2 Contingency Table example
genetics = rbind(c(126, 374), c(158, 342))
rownames(genetics)=c("Control", "Case")
colnames(genetics)=c("Mutation", "Normal")
genetics

# Poisson Model using glm and the log link
genetics1 = data.frame(Count=as.vector(genetics),
                       Gene=c(1,1,0,0),
                       Disease=c(0,1,0,1))
# Independence model has no interactions
gen.pois = glm(Count~Gene+Disease, data=genetics1,
               family=poisson())
fitted(gen.pois)
summary(gen.pois)
# Test of independence
with(gen.pois,
     c("LRT"=deviance, "df"=df.residual,
       "p-value"=pchisq(deviance, df.residual, lower=F)))


# Multinomial Model
p1. = sum(genetics[,1])/1000
p.1 = sum(genetics[1,])/1000 # 0.5 in this case by design!

gen.fits.m = 1000*c(p1.*p.1, p1.*(1-p.1),
                    (1-p1.)*p.1, (1-p1.)*(1-p.1))
gen.Dev.m = 2*sum(genetics1$Count*
                    log(genetics1$Count/gen.fits.m))
all.equal(gen.Dev.m, gen.pois$deviance)

# Binomial Model
# Wald test using prop.test
prop.test(genetics)
# Without a continuity correction
prop.test(genetics, correct=FALSE)

gen.bin = glm(genetics~1, family=binomial)
fitted(gen.bin)
pi.hat = c(fitted(gen.bin), 1-fitted(gen.bin))
gen.fits.bin = rowSums(genetics)*pi.hat
gen.bin$deviance
all.equal(gen.bin$deviance, gen.pois$deviance)

# The Pearson test statistic is
Pearson = sum((genetics-gen.fits.bin)^2/gen.fits.bin)
all.equal(Pearson, 
          as.numeric(prop.test(genetics, correct=F)$statistic))

# Fisher's Tea-tasting example:
teatest1 = cbind(c(4,0),c(0,4))
fisher.test(teatest1, alt="greater")
dhyper(4,4,4,4)

# Similar example with 20 cups of tea in
# total, 8 of which had milk put in first.

# The distribution of the number of correctly
# identified cups of tea is
data.frame(y=0:8, 
           prob=dhyper(0:8, 8, 12, 8),
           p.val=phyper(-1:7, 8, 12, 8, lower=F))

# 6 correctly identified is the smallest
# success rate which is significantly (at 5%)
# better than random.

# fisher.test can also be used with two factors
# Consider the following data corresponding to
# getting 6 correct in the above example
truth=factor(rep(c("Milk","Tea"),c(8,12)))
guess=factor(rep(rep(c("Milk","Tea"),2),c(6,2,2,10)))
fisher.test(guess, truth, alt="greater")

# Even though the table did not occur in
# the way specified for the hypergeometric
# we can use Fisher's test on the genetic
# mutation contingency table from Nov 4
fisher.test(genetics)
# We get a (two-sided) p-value of 0.02961 in
# this case which is similar to the asymptotic
# p-value of 0.0247 found earlier by
with(gen.pois,
     c("LRT"=deviance, "df"=df.residual,
       "p-value"=pchisq(deviance, df.residual, lower=F)))

