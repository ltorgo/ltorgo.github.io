#############
##  Code of Chapter:  Introduction to Data Mining
#############




####### Section:  Data Collection and Business Understanding


#### sub-section:  Data and Datasets


#### sub-section:  Importing Data into R

##
library(readr)
dat <- read_csv("x.csv")
dat
class(dat)

##
d <- read_delim("z.txt", delim=" ", na="???")
d

##
library(DBI)
## The DBMS-specific code starts here
library(RMySQL)
drv <- dbDriver("MySQL")  # Loading the MySQL driver
con <- dbConnect(drv,dbname="transDB",  # connecting to the DBMS
                 username="myuser",password="mypasswd",
                 host="localhost")
## The DBMS-specific code ends here

## getting the results of an SQL query as a data frame
data <- dbGetQuery(con,"SELECT * FROM clients") 

## closing up stuff
dbDisconnect(con)  
dbUnloadDriver(drv)

##
library(DBI)
library(RMySQL)
drv <- dbDriver("MySQL")  # Loading the MySQL driver
con <- dbConnect(drv,dbname="transDB",  # connecting to the DBMS
                 username="myuser",password="mypasswd",
                 host="localhost")

res  <- dbSendQuery(con,"SELECT * FROM transactions") 
while (!dbHasCompleted(res)) {
    # get the next 50 records on a data frame
    someData <- fetch(res, n = 50)  
    # call some function that handles the current chunk
    myProcessingFunction(someData)  
}
dbClearResult(res) # clear the results set

dbDisconnect(con)  # closing up stuff
dbUnloadDriver(drv)

##
library(RMySQL)
library(dplyr)
dbConn <- src_mysql("sonae",
                    host="localhost",user="prodUser",password="myPassword")

##
sensors <- tbl(dbConn,"sensor_values")

##
sensors %>% 
    filter(sid==274,value > 100) %>% 
    select(time,value)

##
d <- read.table("clipboard", header=TRUE)

##
library(readxl)
fc <- "c:\\Documents and Settings\\xpto\\My Documents\\calc.xls"
dat <- read_excel(fc,sheet=1)


####### Section:  Data Pre-Processing


#### sub-section:  Data Cleaning

##
library(readr)
std <- read_delim("stud.txt", delim=" ", 
                  skip=1, col_names=c("StudentName","Math","English"))
std

##
library(tidyr)
stdL <- gather(std, Subject, Grade, Math:English)
stdL

##
spread(stdL, Subject, Grade)

##
std2 <- read_delim("stud2.txt", delim=" ",
                   skip=1, col_names=c("StudentName","Math","English","Degree_Year"))
std2
std2L <- gather(std2, Subject, Grade, Math:English)
std2L <- separate(std2L, Degree_Year, c("Degree","Year"))
std2L

##
library(lubridate)
ymd("20151021")
ymd("2015/11/30")
myd("11.2012.3")
dmy_hms("2/12/2013 14:05:01")
mdy("120112")

##
dates <- c(20120521, "2010-12-12", "2007/01/5", "2015-2-04", 
           "Measured on 2014-12-6", "2013-7+ 25")
dates <- ymd(dates)
dates

##
data.frame(Dates=dates,WeekDay=wday(dates),nWeekDay=wday(dates,label=TRUE),
           Year=year(dates),Month=month(dates,label=TRUE))

##
date <- ymd_hms("20150823 18:00:05",tz="Europe/Berlin")
date

##
with_tz(date,tz="Pacific/Auckland")

##
force_tz(date,tz="Pacific/Auckland")

##
library(dplyr)
library(stringr)
library(readr)
uci.repo <- "https://archive.ics.uci.edu/ml/machine-learning-databases/"
dataset <- "audiology/audiology.standardized"
dataF <- str_c(uci.repo,dataset,".data")
namesF <- str_c(uci.repo,dataset,".names")
## Reading the data file
data <- read_csv(url(dataF), col_names=FALSE, na="?")
data
dim(data)
## Now reading the names file
text <- read_lines(url(namesF))
text[1:3]
length(text)
text[67:70]

##
nms <- str_split_fixed(text[67:135],":",n=2)[,1] # get the names
nms[1:3]
nms <- str_trim(nms) # trim white space
nms[1:3]
nms <- str_replace_all(nms,"\\(|\\)","") # delete invalid chars.
nms[1:3]
colnames(data)[1:69] <- nms
data[1:3,1:10]

##
dat
class(dat$Y)

##
library(readr)
dat$Y <- parse_integer(dat$Y, na="?")
dat
class(dat$Y)


#### sub-section:  Transforming Variables

##
library(dplyr)
data(iris)
iris.stand <- cbind(scale(select(iris,-Species)),select(iris,Species))
summary(iris.stand)

##
mxs <- apply(select(iris,-Species), 2, max, na.rm=TRUE)
mns <- apply(select(iris,-Species), 2, min, na.rm=TRUE)
iris.norm <- cbind(scale(select(iris,-Species), center=mns, scale=mxs-mns),
                   select(iris,Species))
summary(iris.norm)

##
library(Hmisc) # for cut2()
data(Boston, package="MASS") # loading the data
summary(Boston$age) # the numeric variable we are going to discretize

##
Boston$newAge <- cut(Boston$age,5)
table(Boston$newAge)
Boston$newAge <- cut(Boston$age,5, # alternative using our own labels for the bins
                     labels=c("verynew","new","normal","old","veryold"))
table(Boston$newAge)

##
Boston$newAge <- cut2(Boston$age, g=5)
table(Boston$newAge)

##
Boston$newAge <- factor(cut2(Boston$age, g=5),
                        labels=c("verynew","new","normal","old","veryold"))
table(Boston$newAge)


#### sub-section:  Creating Variables

##
library(lubridate)
library(xts)
sp500 <- xts(c(1102.94,1104.49,1115.71,1118.31),
             ymd(c("2010-02-25","2010-02-26","2010-03-01","2010-03-02"),
                 tz=Sys.getenv("TZ"))
             )
sp500

##
sp500["2010-03-02"]
sp500["2010-03"]
sp500["2010-03-01/"]
sp500["2010-02-26/2010-03-01"]

##
library(xts)
data(AirPassengers)
ap <- as.xts(AirPassengers)
apRel <- diff(ap)/ap[-length(ap)]

##
head(ap)
head(embed(ap,4))

##
createEmbedDS <- function(s, emb=4) {
    d <- dim(s)
    if (!is.null(d) && d[2] > 1) stop("Only applicable to uni-variate time series")
    if (emb < 2 || emb > length(s)) stop("Invalid embed size")
    e <- embed(s,emb)
    colnames(e) <- c("T",paste("T",1:(emb-1),sep="_"))
    if (is.xts(s)) return(xts(e,index(s)[emb:length(s)])) else return(e)
}
dataSet <- createEmbedDS(ap,emb=5)
head(dataSet)

##
library(readr)
ff <- read_csv("forestFires.txt")
ff

##
library(sp)
library(dplyr)
spatialCoords <- select(ff,long=x,lat=y) # the contextual data
firesData <- select(ff,ano2000) # the behavioral data
coordRefSys <- CRS("+proj=longlat +ellps=WGS84")
fires2000 <- SpatialPointsDataFrame(spatialCoords,
                                    firesData,
                                    proj4string=coordRefSys)
fires2000[1:3,]

##
bbox(fires2000)
coordinates(fires2000)[1:3,]
summary(fires2000)

##
library(ggmap)
library(tibble)
mapPT <- get_map("Portugal",zoom=7)
d4plot <- as_tibble(cbind(coordinates(fires2000),burnt=fires2000$ano2000))
ggmap(mapPT) + 
    geom_point(data=filter(d4plot, burnt==1),aes(x=long,y=lat),col="orange")

##
library(tm)
docs <- Corpus(DirSource("Documents"))
docs

##
docs[[2]]
content(docs[[2]])[1:3]

##
docs <- docs %>%
        tm_map(removePunctuation) %>%
        tm_map(content_transformer(tolower)) %>%
        tm_map(removeNumbers) %>%
        tm_map(removeWords, stopwords("en")) %>%
        tm_map(stripWhitespace) %>%
        tm_map(stemDocument)
content(docs[[2]])[1:3]

##
data <- DocumentTermMatrix(docs, control=list(weighting=weightTfIdf))
data
inspect(data[1:2,1:5])

##
findFreqTerms(data,0.9)

##
findAssocs(data,"race",0.5)

##
newData <- removeSparseTerms(data,0.7)
newData

##
library(tibble)
as_tibble(as.matrix(newData))


#### sub-section:  Dimensionality Reduction

##
data(iris)
prop2sample <- 0.5
rowIDs <- sample(1:nrow(iris), as.integer(prop2sample*nrow(iris)))
iris.sample <- iris[rowIDs,]

##
data(iris)
prop2sample <- 0.5
rowIDs <- sample(1:nrow(iris), as.integer(prop2sample*nrow(iris)), replace=TRUE)
iris.sample <- iris[rowIDs,]

##
nrLinesFile <- function(f) {
    if (.Platform$OS.type == "unix") 
        as.integer(strsplit(trimws(system(paste("wc -l",f),intern=TRUE)),
                            " ")[[1]][1]) 
    else 
        stop("This function requires unix-based systems")
}

sampleCSV <- function(file, percORn, nrLines, header=TRUE, mxPerc=0.5) {
    if (.Platform$OS.type != "unix") 
        stop("This function requires unix-based systems")
    require(readr, quietly=TRUE)
    
    if (missing(nrLines)) nrLines <- nrLinesFile(file)
    
    if (percORn < 1)
        if (percORn > mxPerc) 
            stop("This function is not adequate for that big samples.")
        else percORn <- as.integer(percORn*nrLines)
    perc <- min(2*percORn/nrLines, mxPerc)
    
    system(paste0("perl -ne 'print if (rand() < ",perc,")' ",file,
                  " > ",file,".tmp.csv"))
    dt <- read_csv(paste0(file,".tmp.csv"),col_names=header, n_max=percORn)
    file.remove(paste0(file,".tmp.csv"))
    if (nrow(dt) != percORn) 
        warning(paste("Expecting",percORn,"rows, but got",nrow(dt)))
    dt
}

##
t <- Sys.time()
d <- sampleCSV("allsensors.csv", 0.01)
Sys.time()-t
nrow(d)

##
library(DBI)
library(RMySQL)
drv <- dbDriver("MySQL")  # Loading the MySQL driver
con <- dbConnect(drv,dbname="transDB",  
                 username="myuser",password="mypassword",
                 host="localhost")

##
sampleDBMS <- function(dbConn, tbl, percORn, mxPerc=0.5) {
    nrRecords <- unlist(dbGetQuery(dbConn, paste("select count(*) from",tbl)))
    
    if (percORn < 1)
        if (percORn > mxPerc) 
            stop("This function is not adequate for that big samples.")
        else percORn <- as.integer(percORn*nrRecords)
    perc <- min(2*percORn/nrRecords, mxPerc)
    
    dt <- dbGetQuery(dbConn,paste("select * from (select * from",tbl,
                                  "where rand() <= ",perc,") as t limit ",percORn))
    if (nrow(dt) != percORn) 
        warning(paste("Expecting",percORn,"rows, but got",nrow(dt)))
    dt
}

##
t1 <- Sys.time()
d <- sampleDBMS(con,"sensor_values",10000)
Sys.time()-t1
nrow(d)

##
dbDisconnect(con)  
dbUnloadDriver(drv)

##
library(CORElearn)
data(iris)
attrEval(Species ~ ., iris, estimator="GainRatio")
attrEval(Species ~ ., iris, estimator="InfGain")
attrEval(Species ~ ., iris, estimator="Gini")
attrEval(Species ~ ., iris, estimator="MDL")

##
infoCore(what="attrEval")

##
data(algae, package ="DMwR2")
attrEval(a1 ~ ., algae[,1:12], estimator="MSEofMean")
attrEval(a1 ~ ., algae[,1:12], estimator="RReliefFexpRank")
infoCore(what="attrEvalReg")

##
data(iris)
pca.data <- iris[,-5] # each case is described by the first 4 variables
pca <- princomp(pca.data)
loadings(pca)

##
pca$scores[1:5,]

##
dim(iris)
reduced.iris <- data.frame(pca$scores[,1:2],Species=iris$Species)
dim(reduced.iris)
head(reduced.iris)

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.7\\textwidth"),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.9\\textwidth"),
                  showFig3=list(fig.width=8,fig.height=8,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.9\\textwidth"),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))
library(dplyr)


####### Section:  Modeling


#### sub-section:  Exploratory Data Analysis

##
data(algae,package="DMwR2")
mean(algae$a1)
mean(algae$NO3)
mean(algae$NO3, na.rm=TRUE)
median(algae$a3)
median(algae$mxPH, na.rm=TRUE)

##
library(dplyr)
alg <- tbl_df(algae)
summarise(alg, avgNO3=mean(NO3,na.rm=TRUE), medA1=median(a1))
select(alg, mxPH:Cl) %>%  
    summarise_each(funs(mean(.,na.rm=TRUE),median(.,na.rm=TRUE)))

##
group_by(alg, season, size) %>% 
    summarize(nObs=n(), mA7=median(a7)) %>% 
        ungroup() %>% arrange(desc(mA7))

##
Mode <- function(x, na.rm = FALSE) {
  if(na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
Mode(algae$mxPH, na.rm=TRUE)
Mode(algae$season)

##
library(DMwR2)
centralValue(algae$a1)
centralValue(algae$speed)

##
var(algae$a1)
sd(algae$Cl, na.rm=TRUE)
IQR(algae$mxPH, na.rm=TRUE)
quantile(algae$a3)
quantile(algae$a3, probs=c(0.2,0.8))
range(algae$a1)
max(algae$a5)-min(algae$a5)

##
select(alg,a1:a7) %>% summarise_each(funs(var))

##
data(iris)
group_by(iris,Species) %>% summarise(qs=quantile(Sepal.Length))

##
aggregate(iris$Sepal.Length, list(Species=iris$Species), quantile)

##
aggregate(Sepal.Length ~ Species, data=iris, quantile)

##
data(algae, package="DMwR2")
nasRow <- apply(algae,1,function(r) sum(is.na(r)))
cat("The Algae dataset contains ",sum(nasRow)," NA values.\n")
cat("There are ",sum(!complete.cases(algae)),
    " rows that have at least one NA value.\n")

##
bpRule <- function(x, const=1.5, positions=FALSE) { 
    x <- x[!is.na(x)]
    qs <- quantile(x,probs = c(0.25,0.75))
    iqr <- qs[2]-qs[1]
    if (!positions) x[x < qs[1]-const*iqr | x > qs[2]+const*iqr]
    else which(x < qs[1]-const*iqr | x > qs[2]+const*iqr)
}
bpRule(algae$a1)
bpRule(algae$NO3)
bpRule(algae$NO3, positions=TRUE)

##
data(iris)
summary(iris)

##
library(Hmisc)
describe(iris)

##
by(algae[,2:5], algae$season,summary)

##
plot(sin(seq(0,10,by=0.1)),type="l")

##
plot(sin(seq(0,10,by=0.1)),type="l")

##
pdf("myplot.pdf")
plot(sin(seq(0,10,by=0.1)),type="l")
dev.off()

##
jpeg("myplot.jpg")
plot(sin(seq(0,10,by=0.1)),type="l")
dev.off()

##
library(ggplot2)
data(iris)
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point(size=4)

##
library(ggplot2)
data(algae, package="DMwR2")
## Plot on the left (standard)
freqOcc <- table(algae$season)
barplot(freqOcc,main='Frequency of the Seasons')
## Plot on the right (ggplot2)
ggplot(algae,aes(x=season)) + geom_bar() + ggtitle("Frequency of the Seasons")

##
library(ggplot2)
library(grid)
data(algae, package="DMwR2")
freqOcc <- table(algae$season)

gh <- ggplot(algae,aes(x=season)) + geom_bar() + 
    ggtitle("Frequency of the Seasons")
par(mfrow=c(1,2))
vp <- viewport(height=unit(1,"npc"),width=unit(0.5,"npc"),
               just=c("left","top"),y=1,x=0.5)
barplot(freqOcc,main='Frequency of the Seasons')
print(gh,vp=vp)

##
ggplot(algae,aes(x=season)) + geom_bar() + 
    ggtitle("Frequency of the Seasons") + coord_flip()

##
library(ggplot2)
data(iris)
## Plot on the left (standard)
hist(iris$Petal.Length,xlab='Petal Length')
## Plot on the right (ggplot2)
ggplot(iris,aes(x=Petal.Length)) + geom_histogram() + xlab("Petal Length")

##
library(ggplot2)
library(grid)
data(iris)
g <- ggplot(iris,aes(x=Petal.Length)) + geom_histogram() +xlab("Petal Length")
par(mfrow=c(1,2))
vp <- viewport(height=unit(1,"npc"),width=unit(0.5,"npc"),
               just=c("left","top"),y=1,x=0.5)
hist(iris$Petal.Length,xlab='Petal Length')
print(g,vp=vp)

##
library(ggplot2)
data(iris)
## Plot on the left (standard)
boxplot(iris$Sepal.Width, ylab='Sepal Width')
## Plot on the right (ggplot2)
ggplot(iris, aes(x=factor(0), y=Sepal.Width)) + geom_boxplot() +
    xlab("") + ylab("Sepal Width") + theme(axis.text.x=element_blank())

##
library(ggplot2)
library(grid)
data(iris)
g <- ggplot(iris,aes(x=factor(0),y=Sepal.Width)) + geom_boxplot() +
    xlab("") + ylab("Sepal Width") + theme(axis.text.x=element_blank())
par(mfrow=c(1,2))
vp <- viewport(height=unit(1,"npc"),width=unit(0.5,"npc"),
               just=c("left","top"),y=1,x=0.5)
boxplot(iris$Sepal.Width,ylab='Sepal Width')
print(g,vp=vp)

##
library(ggplot2)
data(iris)
## Plot on the left (standard)
boxplot(Sepal.Length ~ Species, iris, ylab="Sepal.Length")
## Plot on the right (ggplot2)
ggplot(iris,aes(x=Species,y=Sepal.Length)) + geom_boxplot()

##
library(ggplot2)
library(grid)
data(iris)
g <- ggplot(iris,aes(x=Species,y=Sepal.Length)) + geom_boxplot()
par(mfrow=c(1,2))
vp <- viewport(height=unit(1,"npc"),width=unit(0.5,"npc"),
               just=c("left","top"),y=1,x=0.5)
boxplot(Sepal.Length ~ Species, iris, ylab="Sepal.Length")
print(g,vp=vp)

##
library(ggplot2)
data(algae, package="DMwR2")
ggplot(algae,aes(x=a1)) + geom_histogram() + facet_grid(size ~ speed)

##
library(ggplot2)
data(algae, package="DMwR2")
ggplot(algae,aes(x=a1)) + geom_histogram() + facet_grid(size ~ speed)

##
library(ggplot2)
data(iris)
## Plot on the left (standard)
plot(iris$Sepal.Length,iris$Sepal.Width,
     main="Relationship between Sepal Length and Width", 
     xlab="Sepal Length", ylab="Sepal Width")
## Plot on the right (ggplot2)
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width)) + geom_point() +
    xlab("Sepal Length") + ylab("Sepal Width") + 
    ggtitle("Relationship between Sepal Length and Width")

##
library(ggplot2)
library(grid)
data(iris)
g <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width)) + geom_point() +
    xlab("Sepal Length") + ylab("Sepal Width") + 
    ggtitle("Relationship between Sepal Length and Width")
par(mfrow=c(1,2))
vp <- viewport(height=unit(1,"npc"),width=unit(0.5,"npc"),
               just=c("left","top"),y=1,x=0.5)
plot(iris$Sepal.Length,iris$Sepal.Width,
     main="Relationship between Sepal Length and Width", 
     xlab="Sepal Length", ylab="Sepal Width")
print(g,vp=vp)

##
data(algae, package="DMwR2")
## Plot on the left (standard)
plot(algae$a1, algae$a2, col=algae$season,
     main="Relationship between A1 and A2 by Season", 
     xlab="A1", ylab="A2")
legend("topright",legend=levels(algae$season),
       col=palette()[1:nlevels(algae$season)],pch=1)
## Plot on the right (ggplot2)
plot(algae$a4, algae$a7, pch=as.integer(algae$speed), 
     main="Relationship between A4 and A7 by River Speed", 
     xlab="A4", ylab="A7")
legend("topright",legend=levels(algae$speed),
      pch=1:nlevels(algae$season))

##
par(mfrow=c(1,2))
data(algae, package="DMwR2")
## Plot on the left (standard)
plot(algae$a1, algae$a2, col=algae$season,
     main="Relationship between A1 and A2 by Season", 
     xlab="A1", ylab="A2")
legend("topright",legend=levels(algae$season),
       col=palette()[1:nlevels(algae$season)],pch=1)
## Plot on the right (ggplot2)
plot(algae$a4, algae$a7, pch=as.integer(algae$speed), 
     main="Relationship between A4 and A7 by River Speed", 
     xlab="A4", ylab="A7")
legend("topright",legend=levels(algae$speed),
      pch=1:nlevels(algae$season))

##
data(algae, package="DMwR2")
ggplot(algae, aes(x=a1,y=a2,color=season)) + geom_point() + facet_wrap(~ season)

##
data(algae, package="DMwR2")
pairs(algae[,12:16])

##
data(algae, package="DMwR2")
pairs(algae[,12:16])

##
library(GGally)
ggpairs(algae,columns=12:16)

##
library(GGally)
ggpairs(algae,columns=12:16)

##
library(GGally)
ggpairs(algae,columns=2:5)

##
library(GGally)
ggpairs(algae,columns=2:5)

##
library(GGally)
ggparcoord(algae,columns=12:18,groupColumn="season")

##
library(GGally)
library(dplyr)
data(algae, package="DMwR2")
ggparcoord(algae,columns=12:18,groupColumn="season")


#### sub-section:  Dependency Modeling using Association Rules

##
library(arules)
library(dplyr)
data(Boston,package="MASS")
b <- Boston
b$chas <- factor(b$chas,labels=c("river","noriver"))
b$rad <- factor(b$rad)
b$black <- cut(b$black,breaks=4,labels=c(">31.5%","18.5-31.5%","8-18.5%","<8%"))
discr <- function(x) cut(x,breaks=4, labels=c("low","medLow","medHigh","high"))
b <- select(b,-one_of(c("chas","rad","black"))) %>% 
     mutate_each(funs(discr)) %>% 
     bind_cols(select(b,one_of(c("chas","rad","black"))))
b <- as(b,"transactions")
b

##
summary(b)

##
itemFrequencyPlot(b, support=0.3,cex.names=0.8)

##
itemFrequencyPlot(b, support=0.3,cex.names=0.8)

##
ars <- apriori(b, parameter=list(support=0.025, confidence=0.75))
ars

##
table(discr(Boston$medv))

##
inspect(head(subset(ars, subset=rhs %in% "medv=high"),5,by="confidence"))

##
inspect(head(subset(ars, subset=rhs %in% "medv=low"),5,by="confidence"))

##
inspect(head(subset(ars, subset=lhs %in% "nox=high" | rhs %in% "nox=high"),
             5,by="confidence"))

##
library(arulesViz)
plot(ars)

##
library(arulesViz)
plot(ars)

##
somerules <- subset(ars,
               subset=rhs %in% c("medv=high","medv=medHigh") & confidence>0.75)
plot(somerules, method="matrix", measure="lift")

##
somerules <- subset(ars,subset=rhs %in% c("medv=high","medv=medHigh") & confidence>0.75)
plot(somerules, method="matrix", measure="lift")

##
somerules <- subset(ars, subset=rhs %in% "medv=high" & confidence > 0.95)
plot(somerules, method="graph", control=list(type="itemsets"))


#### sub-section:  Clustering

##
set.seed(1234)
randDat <- matrix(rnorm(50), nrow=5)
dist(randDat) # Euclidean distance (default)
dist(randDat, method="manhattan")
dist(randDat, method="minkowski", p=4)

##
set.seed(1234) # setting a seed for the random number generator
data(iris)
ir3 <- kmeans(iris[,-5], centers=3, iter.max=200) # not using Species info.
ir3

##
table(ir3$cluster, iris$Species)
cm <- table(ir3$cluster, iris$Species)
1-sum(diag(cm))/sum(cm)

##
library(cluster)
s <- silhouette(ir3$cluster, dist(iris[,-5]))

##
plot(s)

##
plot(s)

##
set.seed(1234)
d <- dist(iris[,-5])
avgS <- c()
for(k in 2:6) {
   cl <- kmeans(iris[,-5],centers=k,iter.max=200)
   s <- silhouette(cl$cluster,d)
   avgS <- c(avgS,mean(s[,3]))
}
data.frame(nClus=2:6,Silh=avgS)

##
library(cluster)
set.seed(1234)
pc <- pam(iris[,-5],k=3)
(cm <- table(pc$clustering, iris$Species))
100*(1-sum(diag(cm))/sum(cm))
pc$silinfo$avg.width

##
library(fpc)
data(iris)
sol <- pamk(iris[,-5], krange=2:10, criterion="asw", usepam=TRUE)
sol

##
d <- dist(scale(iris[,-5]))
h <- hclust(d)

##
plot(h,hang=-0.1,labels=iris[["Species"]],cex=0.5)

##
plot(h,hang=-0.1,labels=iris[["Species"]],cex=0.4)

##
clus3 <- cutree(h, 3)
(cm <- table(clus3, iris$Species))
100*(1-sum(diag(cm))/sum(cm))

##
plot(h,hang=-0.1,labels=iris[["Species"]],cex=0.5)
rect.hclust(h,k=3)

##
plot(h,hang=-0.1,labels=iris[["Species"]],cex=0.4)
rect.hclust(h,k=3)

##
set.seed(1234)
d <- dist(scale(iris[,-5]))
methds <- c('complete','single','average')
avgS <- matrix(NA,ncol=3,nrow=5,
               dimnames=list(2:6,methds))
for(k in 2:6) 
  for(m in seq_along(methds)) {
    h <- hclust(d,meth=methds[m])
    c <- cutree(h,k)
    s <- silhouette(c,d)
    avgS[k-1,m] <- mean(s[,3])
  }
avgS

##
di <- diana(iris[,-5], metric='euclidean', stand=TRUE)
di3 <- cutree(di, 3)
(cm <- table(di3, iris$Species))
100*(1-sum(diag(cm))/sum(cm))

##
cm <- cm[c(1,3,2),]
100*(1-sum(diag(cm))/sum(cm))

##
library(fpc)
d <- scale(iris[,-5])
db <- dbscan(d, eps=0.9, MinPts=5)
db
table(db$cluster,iris$Species)


#### sub-section:  Anomaly Detection

##
grubbs.outliers <- function(x, p.thresh=0.05) {
    require(outliers, quietly=TRUE)
    x <- x[!is.na(x)]
    n <- length(x)
    zs <- abs(x - mean(x)) / sd(x)
    outs <- 1 - sapply(zs, function(z) pgrubbs(z, n, type=10))
    posOuts <- which(outs <= p.thresh)
    return(list(zs=zs, 
                pvals=outs, 
                outliers=x[posOuts],
                positions=posOuts))
}
data(algae, package="DMwR2")
grubbs.outliers(algae$a2)$outliers

##
data(algae, package="DMwR2")
table(algae$season)/length(algae$season)

##
dbscan.outliers <- function(data, ...) {
    require(fpc, quietly=TRUE)
    cl <- dbscan(data, ...)
    posOuts <- which(cl$cluster == 0)
    list(positions = posOuts,
         outliers = data[posOuts,], 
         dbscanResults = cl)
}

##
library(dplyr)
library(forcats)
data(Glass, package="mlbench")
count(Glass,Type)  # a dplyr  alternative to "table(Glass$Type)"
g <- mutate(Glass,
            Type=fct_collapse(Type,
                              rare   = as.character(c(3,5,6)),
                              normal = as.character(c(1,2,7))
                              )
            )
g %>% count(Type) %>% mutate(prop=100*n/nrow(g))

##
outs <- dbscan.outliers(g[,-10], eps=1, scale=TRUE)
head(outs$outliers)
nrow(outs$outliers)
slice(g, outs$positions) %>%  count(Type)
count(g, Type)

##
library(DMwR2)
library(dplyr)
og <- outliers.ranking(select(g, -Type))
slice(g, og$rank.outliers[1:40]) %>%  count(Type)

##
library(DMwR2)
library(dplyr)
lof.scores <- lofactor(select(g, -Type),10)
slice(g, order(lof.scores,decreasing=TRUE)[1:40]) %>%  count(Type)

##
library(UBL)
library(dplyr)
count(g,Type)
## Undersampling the largest class
newg <- RandUnderClassif(Type ~ ., g)
count(newg,Type)
## Now specifying the degree of undersampling by hand
newg2 <- RandUnderClassif(Type ~ ., g, list(normal=0.4, rare=1))
count(newg2,Type)
## Oversampling the minority class
newg3 <- RandOverClassif(Type ~ .,g)
count(newg3,Type)

##
library(e1071)
trainD <- filter(g, Type == "normal") %>% select(-Type) 
s <- svm(trainD, y=NULL, type="one-classification", nu=0.5)
(cm <- table(g$Type, predict(s,select(g, -Type))))


#### sub-section:  Predictive Analytics

##
library(DMwR2)
set.seed(1234)
data(iris)
ct1 <- rpartXse(Species ~ ., iris)
ct2 <- rpartXse(Species ~ ., iris, se=0)

##
library(rpart.plot)
prp(ct1, type=0, extra=101)  # left tree
prp(ct2, type=0, extra=101)  # right tree

##
library(rpart.plot)
par( mfrow=c(1,2) )
prp(ct1,type=0,extra=101) 
prp(ct2,type=0,extra=101)
par(mfrow = c(1,1))

##
set.seed(1234)
rndSample <- sample(1:nrow(iris),100)
tr <- iris[rndSample, ]
ts <- iris[-rndSample, ]
ct <- rpartXse(Species ~ ., tr, se=0.5)
ps1 <- predict(ct, ts)
head(ps1)
ps2 <- predict(ct, ts, type="class")
head(ps2)
(cm <- table(ps2, ts$Species))
100*(1-sum(diag(cm))/sum(cm))  # the error rate

##
library(e1071)
data(iris)
set.seed(1234)
rndSample <- sample(1:nrow(iris), 100)
tr <- iris[rndSample, ]
ts <- iris[-rndSample, ]
s <- svm(Species ~ ., tr)
ps <- predict(s, ts)
(cm <- table(ps, ts$Species))
100*(1-sum(diag(cm))/sum(cm))  # the error rate

##
s2 <- svm(Species ~ ., tr, cost=10, kernel="polynomial", degree=3)
ps2 <- predict(s2, ts)
(cm2 <- table(ps2, ts$Species))
100*(1-sum(diag(cm2))/sum(cm2))  # the error rate

##
data(Boston,package='MASS')
set.seed(1234)
sp <- sample(1:nrow(Boston),354)
tr <- Boston[sp,]
ts <- Boston[-sp,]
s1 <- svm(medv ~ ., tr)
ps1 <- predict(s1, ts)
mean(abs(ps1-ts$medv))
s2 <- svm(medv ~ ., tr, kernel="radial", cost=10, epsilon=0.02, gamma=0.01)
ps2 <- predict(s2, ts)
mean(abs(ps2-ts$medv))

##
library(nnet)
data(iris)
set.seed(1234)
rndSample <- sample(1:nrow(iris), 100)
tr <- iris[rndSample, ]
ts <- iris[-rndSample, ]
n <- nnet(Species ~ ., tr, size=6 ,trace=FALSE, maxit=1000)
ps <- predict(n, ts, type="class")
(cm <- table(ps, ts$Species))
100*(1-sum(diag(cm))/sum(cm))  # the error rate

##
data(Boston,package='MASS')
set.seed(1234)
sp <- sample(1:nrow(Boston),354)
tr <- Boston[sp,]
ts <- Boston[-sp,]
nr <- nnet(medv ~ ., tr, linout=TRUE, trace=FALSE, size=6, decay=0.01, maxit=2000)
psnr <- predict(nr, ts)
mean(abs(psnr-ts$medv))

##
library(ggplot2)
library(NeuralNetTools)
## Feature importance (left graph)
garson(nr) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
## Network diagram (rigth graph)
plotnet(nr)    

##
library(h2o)
h2oInstance <- h2o.init(ip="localhost")  # start H2O instance locally

##
data(iris)
set.seed(1234)
rndSample <- sample(1:nrow(iris), 100)
trH  <- as.h2o(iris[rndSample, ],"trH")
tsH <- as.h2o(iris[-rndSample, ],"tsH")
mdl <- h2o.deeplearning(x=1:4, y=5, training_frame=trH)
preds <- h2o.predict(mdl,tsH)[,"predict"]

##
(cm <- table(as.vector(preds), as.vector(tsH$Species)))
100*(1-sum(diag(cm))/sum(cm))

##
library(h2o)
h2oInstance <- h2o.init(ip="localhost") # start H2O instance locally
data(Boston,package="MASS")
rndSample <- sample(1:nrow(Boston), 354)
trH <- as.h2o(Boston[rndSample, ],"trH")
tsH <- as.h2o(Boston[-rndSample, ],"tsH")
mdl <- h2o.deeplearning(x=1:13, y=14, training_frame=trH, 
                        hidden=c(100,100,100, 100), epochs=500)
preds <- as.vector(h2o.predict(mdl,tsH))

##
mean(abs(preds-as.vector(tsH$medv)))

##
library(adabag)
data(iris)
set.seed(1234)
rndSample <- sample(1:nrow(iris), 100)
tr <- iris[rndSample, ]
ts <- iris[-rndSample, ]
 m <- bagging(Species ~ ., tr, mfinal=500)
ps <- predict(m,ts)
names(ps)
ps$confusion
ps$error*100 # percentage of errors

##
library(ipred)
data(Boston,package='MASS')
set.seed(1234)
sp <- sample(1:nrow(Boston),354)
tr <- Boston[sp,]
ts <- Boston[-sp,]
m <- bagging(medv ~ ., tr, nbagg=500)
ps <- predict(m, ts)
mean(abs(ps-ts$medv))

##
library(randomForest)
library(DMwR2)
data(BreastCancer, package="mlbench")
bc <- cbind(knnImputation(BreastCancer[,-c(1,11)]), # column 1 is an ID
            Class=BreastCancer$Class)
set.seed(1234)
rndSample <- sample(1:nrow(bc), 500)
tr <- bc[rndSample, ] 
ts <- bc[-rndSample, ]
m <- randomForest(Class ~ ., tr, ntree=750)
ps <- predict(m, ts)
(cm <- table(ps, ts$Class))
100*(1-sum(diag(cm))/sum(cm))  # the error rate

##
library(adabag)
data(iris)
set.seed(1234)
rndSample <- sample(1:nrow(iris), 100)
tr <- iris[rndSample, ]
ts <- iris[-rndSample, ]
m1 <- boosting(Species ~ ., tr,  mfinal=500) # AdaBoost.M1
ps1 <- predict(m1,ts)
ps1$confusion
ps1$error*100 
m2 <- boosting(Species ~ ., tr, coeflearn="Zhu", mfinal=500) # SAMME
ps2 <- predict(m2,ts)
ps2$confusion
ps2$error*100 

##
library(gbm)
data(Boston,package='MASS')
set.seed(1234)
sp <- sample(1:nrow(Boston),354)
tr <- Boston[sp,]
ts <- Boston[-sp,]
m <- gbm(medv ~ ., data=tr, n.trees=5000)
ps <- predict(m, ts, n.trees=5000)
mean(abs(ps-ts$medv))

##
data(iris)
set.seed(1234)
rndSample <- sample(1:nrow(iris), 100)
tr <- iris[rndSample, ]
ts <- iris[-rndSample, ]

m <- gbm(Species ~ ., data=tr, n.trees=10000, 
         cv.folds=5, n.cores=4)
(best <- gbm.perf(m, plot.it=FALSE, method="cv"))
ps <- predict(m, ts, n.trees=best, type="response")[,,1]
ps <- as.factor(colnames(ps)[max.col(ps)])
(cm <- table(ps, ts$Species))
100*(1-sum(diag(cm))/sum(cm))  # the error rate

##
summary(m, plotit=FALSE)

##
plot(m, i.var=3, type="response")
legend("topleft",c("setosa","versicolor","virginica"),
       col=1:3,lty=1)

##
plot(m, i.var=3, type="response")
legend("topleft",c("setosa","versicolor","virginica"),
       col=1:3,lty=1)

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.7\\textwidth"),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.9\\textwidth"),
                  showFig3=list(fig.width=8,fig.height=8,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.9\\textwidth"),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))


####### Section:  Evaluation


#### sub-section:  The Holdout and Random Subsampling

##
library(performanceEstimation)
library(e1071)
data(iris)
r <- performanceEstimation(PredTask(Species ~ ., iris),
                           Workflow(learner="svm"),
                           EstimationTask(metrics="err",method=Holdout(hldSz=0.3))
                           )
summary(r)

##
library(performanceEstimation)
library(randomForest)
data(Boston, package="MASS")
r <- performanceEstimation(PredTask(medv ~ ., Boston),
                           Workflow(learner="randomForest"),
                           EstimationTask(metrics="mse",
                                          method=Holdout(nReps=3,hldSz=0.3))
                           )
summary(r)


#### sub-section:  Cross Validation

##
library(performanceEstimation)
library(DMwR2)
data(Boston, package="MASS")
r <- performanceEstimation(
         PredTask(medv ~ ., Boston),
         workflowVariants(learner="rpartXse",
                          learner.pars=list(se=c(0,0.25,0.5,1,2))),
         EstimationTask(metrics=c("mse","mae"),
                        method=CV(nReps=3,nFolds=10))
     )
rankWorkflows(r, top=3)

##
getWorkflow("rpartXse.v1", r)

##
plot(r)

##
plot(r)


#### sub-section:  Bootstrap Estimates

##
library(performanceEstimation)
library(DMwR2)
library(e1071)
data(iris)
data(BreastCancer, package="mlbench")
bc <- cbind(knnImputation(BreastCancer[,-c(1,11)]), 
            Class=BreastCancer$Class)
r <- performanceEstimation(
         c(PredTask(Species ~ ., iris), 
           PredTask(Class ~ ., bc)),
         workflowVariants(learner="svm",
                          learner.pars=list(cost=c(1,5,10),
                                            gamma=c(0.01,0.001))),
         EstimationTask(metrics="acc",
                        method=Bootstrap(nReps=200,type=".632"))
    )

##
topPerformers(r , maxs=TRUE)


#### sub-section:  Recommended Procedures


####### Section:  Reporting  and Deployment


#### sub-section:  Reporting Through Dynamic Documents

##
library(rmarkdown)
render("myreport.Rmd")

##
render("myreport.Rmd", output_format="word_document")


#### sub-section:  Deployment Through Web Applications

##
library(shiny)
runApp("myAppFolderName")

##
library(shiny)
data(iris)

shinyUI(fluidPage(
  titlePanel("Exploring the Iris dataset"),
  sidebarLayout(
      sidebarPanel(
          selectInput("var",
                      "Select a Predictor Variable:",
                      choices=colnames(iris)[1:4])
      ),
      mainPanel(
          plotOutput("condBP")
      )
  )
))

##
library(shiny)
data(iris)

shinyServer(
    function(input, output) 
        {
            output$condBP <- renderPlot(
                {
                    form <- as.formula(paste(input$var,"~ Species"))
                    boxplot(form,iris,ylab=input$var,
                            main=paste("Distribution of",input$var,
                                "for the different Species"))
                }
            )  
        }
)
