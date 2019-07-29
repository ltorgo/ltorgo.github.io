#############
##  Code of Slides:  Data Pre-Processing in R
#############




####### Section:  Data Cleaning

##
library(readr)
std <- read_table2("stud2.txt", col_types = cols())
std

##
library(tidyr)
tstd <- gather(std, Math:English,
               key="Subject", value="Grade")
tstd

##
tstd <- separate(tstd, col="DegreeYear", 
                       into=c("Degree","Year"),
                       convert = TRUE)
tstd

##
library(lubridate)
ymd("20151021")
ymd("2015/11/30")
myd("11.2012.3")
dmy_hms("2/12/2013 14:05:01")

##
dates <- c(20120521, "2010-12-12", "2007/01/5", "2015-2-04", 
           "Measured on 2014-12-6", "2013-7+ 25")
dates <- ymd(dates)
dates
data.frame(Dates=dates,WeekDay=wday(dates),nWeekDay=wday(dates,label=TRUE),
           Year=year(dates),Month=month(dates,label=TRUE))

##
date <- ymd_hms("20150823 18:00:05", tz="Europe/Berlin")
date
with_tz(date, tz="Pacific/Auckland")
force_tz(date, tz="Pacific/Auckland")

##
library(readr)
d <- read_lines(url("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/heart-disease.names"))

##
d <- d[127:235]
head(d,2)
tail(d,2)

##
library(stringr)
d <- str_trim(d)

##
## e.g. line (string) starting with the number 26
d[grep("^26",d)] 

##
tgtLines <- sapply(1:76,function(i) d[grep(paste0("^",i),d)[1]])
head(tgtLines,2)

##
nms <- str_split_fixed(tgtLines," ",2)[,2]
head(nms,2)

##
nms <- str_split_fixed(nms,":",2)[,1]
head(nms,2)

##
nms <- str_split_fixed(nms," ",2)[,1]
head(nms,2)
tail(nms,2)

##
library(DMwR)
data(algae)
head(algae[!complete.cases(algae),],3)
nrow(algae[!complete.cases(algae),])
noNA.algae <- na.omit(algae)

##
load("carInsurance.Rdata") # car insurance dataset (get it from class web page)

##
library(DMwR2)
head(ins[!complete.cases(ins),],3)

##
noNA.algae <- centralImputation(algae)
nrow(noNA.algae[!complete.cases(noNA.algae),])
noNA.algae <- knnImputation(algae,k=10)
nrow(noNA.algae[!complete.cases(noNA.algae),])

##
nrow(ins[!complete.cases(ins),])
noNA.ins <- na.omit(ins)  # Option 1
nrow(noNA.ins[!complete.cases(noNA.ins),])
noNA.ins <- centralImputation(ins)  # Option 2
nrow(noNA.ins[!complete.cases(noNA.ins),])
noNA.ins <- knnImputation(ins,k=10)  # Option 3
nrow(noNA.ins[!complete.cases(noNA.ins),])


####### Section:  Transforming Variables

##
data(algae,package="DMwR")
norm.algae <- cbind(algae[,1:3],
                    scale(algae[,-c(1:3,12:18)]),
                    algae[,12:18])

##
load("carInsurance.Rdata") # car insurance data (check course web page)

##
norm.ins <- ins
norm.ins[,c(10:14,17,19:26)] <- scale(norm.ins[,c(10:14,17,19:26)])

##
data(Boston, package="MASS") # The Boston Housing data set
Boston$age <- cut(Boston$age,4)
table(Boston$age)

##
data(Boston, package="MASS") # The Boston Housing data set
Boston$age <- cut(Boston$age,quantile(Boston$age,probs=seq(0,1,.25)))
table(Boston$age)


####### Section:  Creating Variables

##
x <- rnorm(100,mean=100,sd=3)
head(x)
vx <- diff(x)/x[-length(x)]
head(vx)

##
library(quantmod)  # extra package
getSymbols('^GSPC',from='2016-01-01') 
head(GSPC,3)


##
candleChart(GSPC )

##
head(Cl(GSPC))
head(Delt(Cl(GSPC)))

##
library(DMwR2)
library(quantmod)
dat <- getSymbols('^GSPC',from=Sys.Date()-90,auto.assign=FALSE)
ts <- na.omit(Delt(Cl(dat))) # because 1st return is NA
embTS <- createEmbedDS(ts, emb = 3)
head(embTS)
head(ts)


####### Section:  Feature Selection

##
library(CORElearn)
data(iris)
attrEval(Species ~ ., iris, estimator="GainRatio")
attrEval(Species ~ ., iris, estimator="InfGain") 
attrEval(Species ~ ., iris, estimator="Gini")

##
infoCore(what="attrEval")

##
data(algae, package ="DMwR2")
attrEval(a1 ~ ., algae[,1:12], estimator="MSEofMean")
attrEval(a1 ~ ., algae[,1:12], estimator="RReliefFexpRank")

##
infoCore(what="attrEvalReg")

##
data(iris)
pca.data <- iris[,-5] # each case is described by the first 4 variables 
pca <- princomp(pca.data)
loadings(pca)

##
pca$scores[1:5,]
scs <- pca$scores[,1:2]
dadosNovos <- data.frame(pca$scores[,1:2],
                         Species=iris$Species)
head(dadosNovos,3)

##
plot(scs,col=as.numeric(iris$Species),
     pch=as.numeric(iris$Species))
legend('topright',levels(iris$Species),
       pch=1:3,col=1:3)
