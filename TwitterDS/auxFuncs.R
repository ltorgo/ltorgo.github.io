################################################################################
## Some Auxiliary functions used in the reports
## Luis Torgo, 2017
################################################################################



################################################################################
## Loading the Rdata files containing the results of the experiments for both
## the Alpha and F metrics
## The argument of the function specifies the one you wish to load
## The argument can be either "F" or "Alpha", defaulting to the latter
##
## The result of this function are two new objects on your global environment
## containing the results : "Scores" and "res"
################################################################################
loadAndPrepare <- function(metr="Alpha") {
  if (!dir.exists("Graphs")) dir.create("Graphs")
  ## Now loading the results file of the selected metric
  ## with the results objects
  if (metr=="Alpha") {
    load("kScores.Rdata",envir = .GlobalEnv)
  } else if (metr == "F") {
    load("fScores.Rdata",envir = .GlobalEnv) 
  } else {
    cat("Unknown metric! Will default to Alpha!\n")
    load("kScores.Rdata",envir = .GlobalEnv)
  }
  res$Strat2 <<- factor(res$Strat,
                        levels=c("xval91block",
                                 "xval91NSblock",
                                 "xval91rand",
                                 "seq91_20",
                                 "seq91_10",
                                 "seq21semi"
                        ),
                        labels=c("xval(9:1, strat, block)",
                                 "xval(9:1, no-strat, block)",
                                 "xval(9:1, strat, rand)",
                                 "seq(9:1, 20, equi)",
                                 "seq(9:1, 10, equi)",
                                 "seq(2:1, 10, semi-equi)"
                        ))
}



################################################################################
## Creates the files name of a destination PDF storing a graph
################################################################################
gfn <- function(n) paste0("Graphs/",metr,"_",n,".pdf")


################################################################################
## A corrected pairedComparisons function 
## The one on package performanceEstimation has an error that will be corrected
## in new versions of the package. For now we will use this function instead that
## has the error already corrected.
################################################################################
pairedComparisons2 <- function (obj, baseline, 
                                maxs = rep(FALSE, length(metricNames(obj))), 
                                p.value = 0.05) 
{
  if (!inherits(obj, "ComparisonResults")) 
    stop(obj, " is not of class \"ComparisonResults\".\n")
  ts <- taskNames(obj)
  nts <- length(ts)
  ws <- workflowNames(obj)
  nws <- length(ws)
  ms <- metricNames(obj)
  nms <- length(ms)
  if (nws < 2) 
    stop("Paired comparisons only make sense with more than one workflow!")
  if (!missing(baseline)) {
    other <- setdiff(ws, baseline)
    pb <- which(ws == baseline)
  }
  compResults <- vector("list", nms)
  names(compResults) <- ms
  for (p in 1:nms) {
    compResults[[p]] <- list()
    compResults[[p]]$setup <- list(nTasks = nts, nWorkflows = nws)
    compResults[[p]]$avgScores <- t(sapply(obj, function(m) sapply(m, function(i) mean(i@iterationsScores[, p], na.rm = TRUE))))
    compResults[[p]]$medScores <- t(sapply(obj, function(m) sapply(m, function(i) median(i@iterationsScores[, p], na.rm = TRUE))))
    compResults[[p]]$rks <- t(apply(if (maxs[p]) -compResults[[p]]$avgScores else compResults[[p]]$avgScores, 1, rank))
    compResults[[p]]$avgRksWFs <- apply(compResults[[p]]$rks,  2, mean)
    
    if (missing(baseline)) {
      base <- compResults[[p]]$baseline <- names(which.min(compResults[[p]]$avgRksWFs))
      other <- setdiff(ws, base)
      pb <- which(ws == base)
    }
    else base <- compResults[[p]]$baseline <- baseline
    
    compResults[[p]]$t.test <- array(NA, dim = c(nws, 3, nts), 
                                     dimnames = list(c(base, other), c("AvgScore", "DiffAvgScores", "p.value"), ts))
    compResults[[p]]$WilcoxonSignedRank.test <- array(NA, dim = c(nws, 3, nts), 
                                                      dimnames = list(c(base, other), c("AvgScore", "DiffAvgScores", "p.value"), ts))
    
    for (t in ts) {
      compResults[[p]]$WilcoxonSignedRank.test[base, , t] <- NA
      compResults[[p]]$WilcoxonSignedRank.test[base, "AvgScore", t] <- compResults[[p]]$avgScores[t, base]
      compResults[[p]]$t.test[base, , t] <- NA
      compResults[[p]]$t.test[base, "AvgScore", t] <- compResults[[p]]$avgScores[t, base]
      for (o in other) {
        ## Wilcoxon signed rank test
        tst <- try(wilcox.test(obj[[t]][[o]]@iterationsScores[, p], 
                               obj[[t]][[base]]@iterationsScores[, p], 
                               paired = T))
        compResults[[p]]$WilcoxonSignedRank.test[o, "DiffAvgScores", t] <- compResults[[p]]$avgScores[t, base] - compResults[[p]]$avgScores[t, o]
        compResults[[p]]$WilcoxonSignedRank.test[o, "AvgScore", t] <- compResults[[p]]$avgScores[t, o]
        compResults[[p]]$WilcoxonSignedRank.test[o, "p.value", t] <- if (inherits(tst, "try-error"))  NA else tst$p.value
        
        ## t.test
        tst <- try(t.test(obj[[t]][[o]]@iterationsScores[, p], 
                          obj[[t]][[base]]@iterationsScores[, p], 
                          paired = T))
        compResults[[p]]$t.test[o, "DiffAvgScores", t] <- compResults[[p]]$avgScores[t, base] - compResults[[p]]$avgScores[t, o]
        compResults[[p]]$t.test[o, "AvgScore", t] <- compResults[[p]]$avgScores[t, o]
        compResults[[p]]$t.test[o, "p.value", t] <- if (inherits(tst, "try-error"))  NA else tst$p.value
      }
    }
    if (nts > 1) {
      chi <- 12 * nts/(nws * (nws + 1)) * (sum(compResults[[p]]$avgRksWFs^2) - 
                                             (nws * (nws + 1)^2)/4)
      FF <- (nts - 1) * chi/(nts * (nws - 1) - chi)
      critVal <- df(1 - p.value, nws - 1, (nws - 1) * (nts - 
                                                         1))
      rejNull <- FF > critVal
      compResults[[p]]$F.test <- list(chi = chi, FF = FF, 
                                      critVal = critVal, rejNull = rejNull)
      compResults[[p]]$Nemenyi.test <- NULL
      compResults[[p]]$BonferroniDunn.test <- NULL
      if (rejNull) {
        CD.n <- qtukey(1 - p.value, nws, 1e+06)/sqrt(2) * 
          sqrt(nws * (nws + 1)/(6 * nts))
        allRkDifs <- outer(compResults[[p]]$avgRksWFs, 
                           compResults[[p]]$avgRksWFs, function(x, y) abs(x - 
                                                                            y))
        signifDifs <- allRkDifs >= CD.n
        compResults[[p]]$Nemenyi.test <- list(critDif = CD.n, 
                                              rkDifs = allRkDifs, signifDifs = signifDifs)
        CD.bd <- qtukey(1 - (p.value/(nws - 1)), 2, 1e+06)/sqrt(2) * 
          sqrt(nws * (nws + 1)/(6 * nts))
        diffs2base <- abs(compResults[[p]]$avgRksWFs[-pb] - 
                            compResults[[p]]$avgRksWFs[pb])
        signifDifs <- diffs2base >= CD.bd
        compResults[[p]]$BonferroniDunn.test <- list(critDif = CD.bd, 
                                                     baseline = base, rkDifs = diffs2base, signifDifs = signifDifs)
      }
    }
    else {
      compResults[[p]]$F.test <- compResults[[p]]$Nemenyi.test <- compResults[[p]]$BonferroniDunn.test <- NULL
      warning("With less 2 tasks the Friedman, Nemenyi and Bonferroni-Dunn tests are not calculated.")
    }
  }
  compResults
}