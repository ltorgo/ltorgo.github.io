#############
##  Code of Slides:  Reporting in R
#############




####### Section:  rmarkdown

##
plot(iris$Sepal.Length,iris$Sepal.Width,col=iris$Species)

##
library(rmarkdown)
render("initialDoc.Rmd")
