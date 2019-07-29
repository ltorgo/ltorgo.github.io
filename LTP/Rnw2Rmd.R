library(tibble)
tgtDir <- "~/Dropbox/DropboxHome/Ensino/CursosCurtos/CursosR/LTPlabs/WebPage/20192020/"
setwd("~/Dropbox/DropboxHome/Ensino/CursosCurtos/CursosR/DataMiningWithR")

cns <- list(
  list(
    dir= "Reporting/ENG/Beamer",
    fnS= "reporting",
    fnC= "Rreporting",
    title= "Reporting in R",
    fs= c("rmarkdown.Rnw")
  ),
  list(
    dir= "PredAnalytics/ENG/Beamer",
    fnS= "predAnalytics",
    fnC= "RpredAnalytics",
    title= "Predictive Analytics in R",
    fs= c("metrics.Rnw","linDiscr.Rnw","linRegr.Rnw","svms.Rnw","trees.Rnw","rfs.Rnw","UBLexs.Rnw")
  ),
  list(
    dir= "DataPreProcess/ENG/Beamer",
    fnS= "dataPreProc",
    fnC= "RdataPreProc",
    title= "Data Pre-Processing in R",
    fs= c("dataClean2.Rnw","dataTransf.Rnw","varCreation.Rnw","featureSelection.Rnw")
  ),
  list(
    dir= "PredAnalytics/ENG/Beamer",
    fnS= "performanceEstimation",
    fnC= "RperformanceEstimation",
    title= "Performance Estimation in R",
    fs= c("perfEst.Rnw")
  )
  
)
# cns <- as.data.frame(tribble(
#   ~dir, ~fn, ~title,
#   "ChFin" , "Rstocks", ,
#   "ChFraud",  "Rfraud", "Detecting Fraudulent Transactions",
#   "ChBio", "Rbio", "Classifying Micro Array Samples"
# ),strinfsAsFactors=FALSE)



parseRnws <- function(dir,rfn="Rcodes",
                      title="Chapter of Codes",tgt="./",
                      fs=list.files(dir,pattern="Rnw$")) {
  
  res <- list()
#  fs <- list.files(dir,pattern="Rnw$")

  res$code <- c(
    '---',
    paste('title:',title),
    'output:',
    '  html_document:',
    '    toc: true',
    '    toc_depth: 4',
    '    toc_float: true',
    '---',
    '\n',  
    '```{r echo=FALSE}',
    'knitr::opts_chunk$set(eval =FALSE)',
    '```',
    '\n',
    paste0('The following is a [script file](RCode/',rfn,'.R) containing all R code of all sections in this slide set.'),
    '\n'
  )
  res$script <- c(
    "#############",
    paste("##  Code of Slides: ",title,collapse = ""),
    "#############","\n"
  )
  
  for(f in fs) {
    p <- parseRnw(f)
    res$code <- c(res$code,p$code)
    res$script <- c(res$script,p$script)
  }
  writeLines(res$code,con=paste0(tgt,rfn,".Rmd"))
  writeLines(res$script,con=paste0(tgt,"RCode/",rfn,".R"))
}

parseRnw <- function(fn) {
  sh <- "^\\\\section\\{"
#  ssh <- "^\\\\subsection\\{"
  chkI <- "^\\<\\<"
  chkE <- "^\\@"
  shrmd <- "###"
#  sshrmd <- "####"
  chkIrmd <- "```{r}"
  chkErmd <- "```"
  require(stringr)
  txt <- readLines(fn)
  secs <- code <- paste("<!--",fn,"--> ")
  script <- c()
  i <- 1
  while (i <= NROW(txt)) {
    if (str_detect(txt[i],sh)) {
      hd <- str_replace_all(str_split_fixed(str_split_fixed(txt[i],"\\{",2)[2],"\\}",2)[1],"\\\\","")
      code <- c(code,"\n",paste(shrmd,hd))
#      secs <- c(secs,paste('-',hd))
      script <- c(script,paste("\n\n####### Section: ",hd))
#    } else if (str_detect(txt[i],ssh)) {
#      hd <- str_replace_all(str_split_fixed(str_split_fixed(txt[i],"\\{",2)[2],"\\}",2)[1],"\\\\","")
#      code <- c(code,"\n",paste(sshrmd,hd))
#      secs <- c(secs,paste('    +',hd))
#      script <- c(script,paste("\n\n#### sub-section: ",hd))
    } else if (str_detect(txt[i],chkI)) {
      skip <- str_detect(txt[i],"echo=FALSE")
      if (!skip) {
        code <- c(code,chkIrmd)
        script <- c(script,"\n##")
      }
      i <- i+1
      while (!str_detect(txt[i],chkE)) {
          if (!skip) {
            code <- c(code,txt[i])
            script <- c(script,txt[i])
          }
          i <- i+1
      }
      if (!skip) code <- c(code,chkErmd,"\n")
    }
    i <- i+1
  }
  list(#secs=secs,
       code=code, script=script)
}

for(c in cns) {
  cd <- getwd()
  setwd(c$dir)
  parseRnws(".",c$fnC,c$title,tgtDir,c$fs)
  setwd(cd)
}
