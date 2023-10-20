f<-c(1,2,3)
f
# shows built-in data sets to learn analysis with R
data()
# selecting trees data set
data("trees")
# seeing head of data set
head(trees)
# how many rows we have
nrow(trees)
# see dimensions as number of rows and columns
dim(trees)
# to see structure of data frame we use str()
str(trees)
# seeing mean of column girth
mean(trees$Girth)
# attaching trees data frame to R, less code will be needed
attach(trees)
# now i can find mean , median etc with less code
median(Girth)
# standard deviation
sd(Girth)
# min
min(Girth)
# max
max(Girth)
# fivenum to plot box plot, show min, max, median, 25 and 75 percentile as well
fivenum(Girth)
# have a look data, it shows mean too, as compared to fivenum
summary(Girth)
# summary for whole data frame
summary(trees)
# once you done working with df, you should detach to avoid confusion
# now code lines don't work like this median(Girth), min(Girth), max(Girth)
# because we have detach the df, we need to use it like min(trees$Girth)
detach(trees)
# will install psych package to look at data differently
# install.packages("psych")
# two ways to use installed packages, require(), library()
require(psych)
library(psych)
# to see docs of package , ?psych
?psych
# using describe function from psych package
describe(trees)
# observing another df named iris
data("iris")
# observing df structure
str(iris)
# checking header data
head(iris)
# checking tail of data
tail(iris)
# describing base on species types in species column of df iris
describeBy(iris, iris$Species)