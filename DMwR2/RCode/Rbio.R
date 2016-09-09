#############
##  Code of Chapter:  Classifying Micro Array Samples
#############




####### Section:  The Available Data

##
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("ALL")

##
library(Biobase)
library(ALL)
data(ALL)

##
ALL

##
pD <- phenoData(ALL)
varMetadata(pD)
table(ALL$BT)
table(ALL$mol.biol)
table(ALL$BT, ALL$mol.bio)

##
featureNames(ALL)[1:10]
sampleNames(ALL)[1:5]

##
tgt.cases <- which(ALL$BT %in% levels(ALL$BT)[1:5] & 
                   ALL$mol.bio %in% levels(ALL$mol.bio)[1:4])
ALLb <- ALL[,tgt.cases]
ALLb

##
ALLb$BT <- factor(ALLb$BT)
ALLb$mol.bio <- factor(ALLb$mol.bio)

##
save(ALLb, file = "myALL.Rdata")

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.7\\textwidth"),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.9\\textwidth"),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))
library(ggplot2)
library(grid)
library(Biobase)
library(ALL)
data(ALL)
load("myALL.Rdata")

##
es <- exprs(ALLb)
dim(es)

##
summary(as.vector(es))

##
source("http://bioconductor.org/biocLite.R")
biocLite("genefilter")

##
library(genefilter)
library(ggplot2)
exprVs <- data.frame(exprVal=as.vector(es))
ds <- data.frame(Stat=c("1stQ","Median","3rdQ","Shorth"),
                 Value=c(quantile(exprVs$exprVal, 
                         probs=c(0.25, 0.5, 0.75)),
                         shorth(exprVs$exprVal)),
                 Color=c("red","green","red","yellow"))
ggplot(exprVs,aes(x=exprVal)) + geom_histogram(fill="lightgrey") + 
    geom_vline(data=ds,aes(xintercept=Value,color=Color)) + 
    geom_text(data=ds,aes(x=Value-0.2,y=0,label=Stat,colour=Color),
              angle=90,hjust="left") +
    xlab("Expression Levels") + guides(colour="none", fill="none") 

##
library(genefilter)
library(ggplot2)
exprVs <- data.frame(exprVal=as.vector(es))
ds <- data.frame(Stat=c("1stQ","Median","3rdQ","Shorth"),
                 Value=c(quantile(exprVs$exprVal, probs=c(0.25, 0.5, 0.75)),
                     shorth(exprVs$exprVal)),
                 Color=c("red","green","red","yellow"))
ggplot(exprVs,aes(x=exprVal)) + geom_histogram(fill="lightgrey") + 
    geom_vline(data=ds,aes(xintercept=Value,color=Color)) + 
    geom_text(data=ds,aes(x=Value-0.2,y=0,label=Stat,colour=Color),angle=90,hjust="left") +
    xlab("Expression Levels") + guides(colour="none", fill="none")    

##
sapply(levels(ALLb$mol.bio), 
       function(x) summary(as.vector(es[, which(ALLb$mol.bio == x)])))

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.7\\textwidth"),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.9\\textwidth"),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))
library(ggplot2)
library(grid)
library(Biobase)
library(ALL)
data(ALL)
load("myALL.Rdata")
es <- exprs(ALLb)


####### Section:  Gene (Feature) Selection

##
source("https://bioconductor.org/biocLite.R")
biocLite("hgu95av2.db")

##
rowIQRs <- function(em) 
    rowQ(em,ceiling(0.75*ncol(em))) - rowQ(em,floor(0.25*ncol(em)))
library(ggplot2)
dg <- data.frame(rowMed=rowMedians(es), rowIQR=rowIQRs(es))
ggplot(dg,aes(x=rowMed, y=rowIQR)) + geom_point() +
    xlab("Median expression level") + ylab("IQR expression level") +
    ggtitle("Main Characteristics of Genes Expression Levels")

##
rowIQRs <- function(em) rowQ(em,ceiling(0.75*ncol(em))) - rowQ(em,floor(0.25*ncol(em)))
library(ggplot2)
dg <- data.frame(rowMed=rowMedians(es), rowIQR=rowIQRs(es))
ggplot(dg,aes(x=rowMed, y=rowIQR)) + geom_point() +
    xlab("Median expression level") + ylab("IQR expression level") +
    ggtitle("Main Characteristics of Genes Expression Levels")

##
library(genefilter)
resFilter <- nsFilter(ALLb,
                 var.func=IQR,
                 var.cutoff=IQR(as.vector(es))/5, 
                 feature.exclude="^AFFX")
resFilter

##
ALLb <- resFilter$eset
es <- exprs(ALLb)
dim(es)

##
f <- Anova(ALLb$mol.bio, p = 0.01)
ff <- filterfun(f)
selGenes <- genefilter(exprs(ALLb), ff)
sum(selGenes)

##
ALLb <- ALLb[selGenes, ]
ALLb
es <- exprs(ALLb)
dim(es)

##
dg <- data.frame(rowMed=rowMedians(es), rowIQR=rowIQRs(es))
ggplot(dg,aes(x=rowMed, y=rowIQR)) + geom_point() +
    xlab("Median expression level") + ylab("IQR expression level") +
    ggtitle("Distribution Properties of the Selected Genes")

##
dg <- data.frame(rowMed=rowMedians(es), rowIQR=rowIQRs(es))
ggplot(dg,aes(x=rowMed, y=rowIQR)) + geom_point() +
    xlab("Median expression level") + ylab("IQR expression level") +
    ggtitle("Distribution Properties of the Selected Genes")

##
library(randomForest)
dt <- data.frame(t(es), Mut = ALLb$mol.bio)
dt$Mut <- droplevels(dt$Mut)
set.seed(1234)
rf <- randomForest(Mut ~ ., dt, importance = TRUE)
imp <- importance(rf)
rf.genes <- rownames(imp)[order(imp[,"MeanDecreaseAccuracy"], 
                                decreasing = TRUE)[1:30]]

##
sapply(rf.genes, function(g) tapply(dt[, g], dt$Mut, median))

##
library(tidyr)
library(dplyr)
d <- gather(dt[,c(rf.genes,"Mut")],Gene,ExprValue,1:length(rf.genes))
dat <- group_by(d,Mut,Gene) %>% 
    summarise(med=median(ExprValue), iqr=IQR(ExprValue))
ggplot(dat, aes(x=med,y=iqr,color=Mut)) +  
    geom_point(size=6) + facet_wrap(~ Gene) + 
    labs(x="MEDIAN expression level",y="IQR expression level",color="Mutation")

##
library(Hmisc)
vc <- varclus(t(es))
clus30 <- cutree(vc$hclust, 30)
table(clus30)

##
getVarsSet <- function(cluster,nvars=30,seed=NULL,verb=FALSE) {
    if (!is.null(seed)) set.seed(seed)
    
    cls <- cutree(cluster,nvars)
    tots <- table(cls)
    vars <- c()
    vars <- sapply(1:nvars,function(clID)
    {
        if (!length(tots[clID])) stop('Empty cluster! (',clID,')')
        x <- sample(1:tots[clID],1)
        names(cls[cls==clID])[x]
    })
    if (verb)  structure(vars,clusMemb=cls,clusTots=tots)
    else       vars
}
getVarsSet(vc$hclust)
getVarsSet(vc$hclust)

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.7\\textwidth"),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.9\\textwidth"),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))
library(ggplot2)
library(grid)
library(Biobase)
library(ALL)
data(ALL)
load("myALL.Rdata")
es <- exprs(ALLb)
rowIQRs <- function(em)  rowQ(em,ceiling(0.75*ncol(em))) - rowQ(em,floor(0.25*ncol(em)))
getVarsSet <- function(cluster,nvars=30,seed=NULL,verb=FALSE) {
    if (!is.null(seed)) set.seed(seed)
    
    cls <- cutree(cluster,nvars)
    tots <- table(cls)
    vars <- c()
    vars <- sapply(1:nvars,function(clID)
    {
        if (!length(tots[clID])) stop('Empty cluster! (',clID,')')
        x <- sample(1:tots[clID],1)
        names(cls[cls==clID])[x]
    })
    if (verb)  structure(vars,clusMemb=cls,clusTots=tots)
    else       vars
}


####### Section:  Predicting Cytogenetic Abnormalities

##
library(performanceEstimation)
library(DMwR2)
data(iris)
exp <- performanceEstimation(
    PredTask(Species ~ ., iris), 
    Workflow(learner="rpartXse", predictor.pars=list(type="class")),
    EstimationTask(metrics="acc",method=Bootstrap(nReps=100)))

##
summary(exp)

##
library(class)
data(iris)
idx <- sample(1:nrow(iris), as.integer(0.7 * nrow(iris)))
tr <- iris[idx, ]
ts <- iris[-idx, ]
preds <- knn(tr[, -5], ts[, -5], tr[, 5], k = 3)
table(preds, ts[, 5])

##
kNN <- function(form, train, test, stand = TRUE, stand.stats = NULL, ...) {
    require(class, quietly = TRUE)
    tgtCol <- which(colnames(train) == as.character(form[[2]]))
    if (stand) {
        if (is.null(stand.stats)) 
            tmp <- scale(train[, -tgtCol], center = TRUE, scale = TRUE)
        else tmp <- scale(train[, -tgtCol], center = stand.stats[[1]], 
                          scale = stand.stats[[2]])
        train[, -tgtCol] <- tmp
        ms <- attr(tmp, "scaled:center")
        ss <- attr(tmp, "scaled:scale")
        test[, -tgtCol] <- scale(test[, -tgtCol], center = ms, scale = ss)
    }
    knn(train[, -tgtCol], test[, -tgtCol], train[, tgtCol], ...)
}

##
preds.stand <- kNN(Species ~ ., tr, ts, k = 3)
table(preds.stand,ts[, 5])
preds.notStand <- kNN(Species ~ ., tr, ts, stand = FALSE, k = 3)
table(preds.notStand, ts[, 5]) 

##
varsEnsemble <- function(tgt,train,test,
                         fs.meth,
                         baseLearner,blPars,
                         predictor,predPars,
                         verb=FALSE)
{
    require(Hmisc,quietly=TRUE)
    v <- varclus(as.matrix(train[,-which(colnames(train)==tgt)]))
    varsSets <- lapply(1:fs.meth[[3]],function(x)
        getVarsSet(v$hclust,nvars=fs.meth[[2]]))
    
    preds <- matrix(NA,ncol=length(varsSets),nrow=NROW(test))
    for(v in seq(along=varsSets)) {
        if (baseLearner=='knn')
            preds[,v] <- do.call("kNN",
                                 c(list(as.formula(paste(tgt,
                                                 paste(varsSets[[v]],
                                                       collapse='+'),
                                                 sep='~')),
                                        train[,c(tgt,varsSets[[v]])],
                                        test[,c(tgt,varsSets[[v]])]),
                                   blPars)
                                 )
        else {
            m <- do.call(baseLearner,
                         c(list(as.formula(paste(tgt,
                                                 paste(varsSets[[v]],
                                                       collapse='+'),
                                                 sep='~')),
                                train[,c(tgt,varsSets[[v]])]),
                           blPars)
                         )
            preds[,v] <- do.call(predictor,
                                 c(list(m,test[,c(tgt,varsSets[[v]])]),
                                   predPars))
        }
    }

    ps <- apply(preds,1,function(x)
        levels(factor(x))[which.max(table(factor(x)))])
    factor(ps,
           levels=1:nlevels(train[,tgt]),
           labels=levels(train[,tgt]))
}

##
ALLb.wf <- function(form, train, test,
                    learner, learner.pars=NULL,
                    predictor="predict",predictor.pars=NULL,
                    featSel.meth = "s2", 
                    available.fsMethods=list(s1=list("all"),s2=list('rf',30),
                                             s3=list('varclus',30,50)),
                    .model=FALSE,
                    ...)
{
    ## The characteristics of the selected feature selection method
    fs.meth <- available.fsMethods[[featSel.meth]] 
    
    ## The target variable
    tgt <- as.character(form[[2]])
    tgtCol <- which(colnames(train)==tgt)

    ## Anova filtering  
    f <- Anova(train[,tgt],p=0.01)
    ff <- filterfun(f)
    genes <- genefilter(t(train[,-tgtCol]),ff)
    genes <- names(genes)[genes]
    train <- train[,c(tgt,genes)]
    test <- test[,c(tgt,genes)]
    tgtCol <- 1

    ## Specific filtering 
    if (fs.meth[[1]]=='varclus') {
      pred <- varsEnsemble(tgt,train,test,fs.meth,
                            learner,learner.pars,
                            predictor,predictor.pars,
                            list(...))

    } else {
      if (fs.meth[[1]]=='rf') {
        require(randomForest,quietly=TRUE)
        rf <- randomForest(form,train,importance=TRUE)
        imp <- importance(rf)
        rf.genes <- rownames(imp)[order(imp[,"MeanDecreaseAccuracy"], 
                                        decreasing = TRUE)[1:fs.meth[[2]]]]
        train <- train[,c(tgt,rf.genes)]
        test <- test[,c(tgt,rf.genes)]
      }

      if (learner == 'knn') 
        pred <- kNN(form,train,test,
                    stand.stats=list(rowMedians(t(as.matrix(train[,-tgtCol]))),
                        rowIQRs(t(as.matrix(train[,-tgtCol])))),
                    ...)
      else {
        model <- do.call(learner,c(list(form,train),learner.pars))
        pred <- do.call(predictor,c(list(model,test),predictor.pars))
      }

    }

    return(list(trues=responseValues(form,test), preds=pred, 
                model=if (.model && learner!="knn") model else NULL))

}

##
vars <- list()
vars$randomForest <- list(learner.pars=list(ntree=c(500,750,1000),
                                            mtry=c(5,15)),
                          preditor.pars=list(type="response"))
vars$svm <- list(learner.pars=list(cost=c(1,100),
                                   gamma=c(0.01,0.001,0.0001)))
vars$knn <- list(learner.pars=list(k=c(3,5,7),
                                   stand=c(TRUE,FALSE)))
vars$featureSel <- list(featSel.meth=c("s1", "s2", "s3"))

##
library(performanceEstimation)
library(class)
library(randomForest)
library(e1071)
library(genefilter)
load('myALL.Rdata')  # loading the previously saved object with the data

es <- exprs(ALLb)

## simple filtering
ALLb <- nsFilter(ALLb,
                 var.func=IQR,var.cutoff=IQR(as.vector(es))/5, 
                 feature.exclude="^AFFX")
ALLb <- ALLb$eset

## the source dataset after the basic filtering
dt <- data.frame(t(exprs(ALLb)),Mut=ALLb$mol.bio)

set.seed(1234)
## The learners to evaluate
TODO <- c('knn','svm','randomForest')
for(td in TODO) {
    assign(td,
         performanceEstimation(
             PredTask(Mut ~ .,dt,'ALL'),
             do.call('workflowVariants',
                     c(list('ALLb.wf',learner=td,varsRootName=td),
                       vars[[td]],
                       vars$featureSel
                       )
                     ),
             EstimationTask(metrics="acc",method=Bootstrap(nReps=100)),
             cluster=TRUE
         )
         )
    save(list=td,file=paste(td,'Rdata',sep='.'))
}

##
## load results of the exps
load("knn.Rdata")
load("svm.Rdata")
load("randomForest.Rdata")

##
rankWorkflows(svm, maxs = TRUE)

##
all.trials <- mergeEstimationRes(svm, knn, randomForest, by ="workflows")

##
rankWorkflows(all.trials, top=10, maxs = TRUE)

##
getWorkflow("svm.v8", all.trials)

##
top10WFnames <- rankWorkflows(all.trials, top=10, 
                              maxs = TRUE)[["ALL"]][["acc"]][,"Workflow"]
sapply(top10WFnames, function(WFid) getWorkflow(WFid,all.trials)@pars$featSel.meth)

##
plot(subset(all.trials,workflows=top10WFnames))

##
plot(subset(all.trials,workflows=top10WFnames))

##
ps <- pairedComparisons(subset(all.trials,workflows=top10WFnames),baseline="svm.v8")
ps$acc$WilcoxonSignedRank.test

##
iteration <- 1  # any number between 1 and 100 in this case
itInfo <- getIterationsInfo(all.trials,workflow="svm.v8",it=iteration)
table(itInfo$trues, itInfo$preds)
