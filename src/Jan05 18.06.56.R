# STATS 4C03/6C03 Winter 2026

# Lecture 1, January 5, 2026.

# Import a dataset and assign give it a 
# variable name
Cars = read.csv("Cars.csv", 
                stringsAsFactors=TRUE)

# It is important to note that R is CASE
# SENSITIVE so Cars and cars are not the
# same thing. cars is a dataset that comes 
# automatically with R.

# View allows us to look at the data in
# a spreadsheet like format.
View(Cars)

# We can extract individual columns
# Both of these are equivalent
Cars[,1]
Cars$Country

# Or we can access certain elements as in
Cars[1:10,1]
Cars$Country[1:10]

# Logical indexing is very useful.  Here we
# get the data on the German Cars only.
Cars[Cars$Country=="Germany",]

Cars[Cars$Horsepower < 100,]

# We can combine logicals using the & (logical AND) or 
# | (logical OR) operators as in
Cars[(Cars$Horsepower < 100) & (Cars$Country=="Japan"),]
Cars[(Cars$Horsepower < 100) | (Cars$Country=="Japan"),]

# Logicals can be used in mathematical statements since
# R will convert TRUE to 1 and FALSE to 0 when required
2*(Cars$Horsepower < 100)-1

# The sample proportion satisfying a condition can then
# be found as in
mean(Cars$Horsepower < 100)

# Many R functions are generic and call methods
# that do different things depending on the 
# nature of the argument.  One very useful
# generic is summary which gives information
# about its argument.  Here we use it for a 
# data.frame. This gives a summary for each of
# the columns separately
summary(Cars)

# Prior to R 4.0.0, strings were automatically
# converted to factors. Unfortunately this led
# to issues in reproducibility so the default
# has now been changed and we need to
# explicitly set variables that contain
# categorical data to factor.
Cars$Country = factor(Cars$Country,
                      levels=c("USA","Germany",
                               "Japan", "Other"))
# The summary for factors is different and
# gives a frequency table
summary(Cars)

# We can get this directly using table
table(Cars$Country)
100*table(Cars$Country)/nrow(Cars)
round(100*table(Cars$Country)/nrow(Cars),2)

# For numeric columns, summary gives a 6 number 
# summary of the data (the usual 5 number summary
# plus the mean).
summary(Cars$Price)

# We can get all of the standard summary statistics
# separately such as
mean(Cars$Price)
sd(Cars$Price)
median(Cars$Price)
IQR(Cars$Price)

# Simple graphics 
hist(Cars$Horsepower, col="green")
hist(Cars$Horsepower, col="blue", 
     freq = FALSE)
hist(Cars$Price, col="red", freq = FALSE, breaks=20)

barplot(table(Cars$Country), col=rainbow(4),
        axis.lty=1)
barplot(100*table(Cars$Country)/nrow(Cars), 
        col=rainbow(4), ylab="Percentage",
        axis.lty=1)

# In this course we are often interested in the
# relationship between two continuous variables.
# We can get the linear correlation coefficient
cor(Cars$Price, Cars$Horsepower)

# We can also plot the variables against each other
plot(Price~Horsepower, data=Cars)

# It may be useful to use different colours or
# symbols for different Countries.
cols = c("red", "green", "blue", "orange")
syms = 15:18
plot(Price~Horsepower, data=Cars,
     col=cols[Country], pch=syms[Country])


# We can add a legend to explain the colours
# and symbols

# Unfortunately I find that the RStudio 
# Graphics window does not handle legends 
# well!  I prefer to use an external X11
# graphics window instead for such plots.
X11()
plot(Price~Horsepower, data=Cars,
     col=cols[Country], pch=syms[Country])
legend("topleft", legend=levels(Cars$Country), 
       col=cols, pch=syms)
dev.off()

# Boxplots are also often helpful
boxplot(Cars$Price)
boxplot(Price~Country, data=Cars, col=cols)
boxplot(Horsepower~Country, data=Cars, col=cols)

# To produce plots to import into other
# documents (e.g. assignments), it is often
# best to redirect the graphics to the right
# file type (pdf or png work best for LaTeX
# documents)
pdf("Figure1.pdf", width=6, height=6)
plot(Price~Horsepower, data=Cars,
     col=cols[Country], pch=syms[Country])
legend("topleft", legend=levels(Cars$Country), 
       col=cols, pch=syms)
dev.off()

# Note that width and height are measured in 
# inches for pdf but in pixels for png!
# Typically there are 72 pixels per inch
png("Fig2.png", width=72*6, height=72*5)
boxplot(Price~Country, data=Cars, col=cols)
dev.off()