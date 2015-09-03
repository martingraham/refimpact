avjac <- function (v1, v2) {
  av <- sapply (seq(1,length(v1)), function(i) {
    sv1 <- v1 [1:i]
    sv2 <- v2 [1:i]
    length (intersect(sv1, sv2)) / length (union(sv1, sv2))
  })
  
  mean (av)
}

# lists need to be same length
avjacMatrix <- function (listOfLists1, listOfLists2) {
  len <- length (listOfLists1)
  m <- sapply (seq(1,len), function(i) {
    row <- sapply (seq(1,len), function (j) {
      avjac (names(listOfLists1[[i]]), names(listOfLists2[[j]]))
    })
  })
  
  m
}

cosin <- function (v1, v2) {
  v1 %*% v2 / sqrt(v1%*%v1 * v2%*%v2)
}

convertToVector <- function (list1, list2) {
  # get names used in lists
  n1 <- names(list1)
  n2 <- names(list2)
  # find differences between names in list
  miss2 <- setdiff (n1, n2)
  miss1 <- setdiff (n2, n1)
  # add the missing names to each list with value zero
  list1[miss1] <- 0
  list2[miss2] <- 0
  # order the lists alphabetically
  list1 <- list1[order(names(list1))]
  list2 <- list2[order(names(list2))]

  # fire them out as vectors which can be compared
  vlist <- list(one=unlist(list1, use.names=F), two=unlist(list2, use.names=F))
  vlist
}

# lists need to be same length
cosinMatrix <- function (listOfLists1, listOfLists2) {
  len <- length (listOfLists1)
  m <- sapply (seq(1,len), function(i) {
    row <- sapply (seq(1,len), function (j) {
      vlist <- convertToVector (listOfLists1[[i]], listOfLists2[[j]])
      cosin (vlist$one, vlist$two)
    })
  })
  
  m
}

hungarian <- function (mat) {
  library (clue)
  # reduce by mins of rows and then columns
  #mat <- t(apply(mat,1,function(r) r - min(r)))
  #mat <- t(apply(mat,2,function(r) r - min(r)))
  # maximum is true cos we want the matches with highest scores, not lowest
  solve <- solve_LSAP (mat, maximum=TRUE)
  vals <- sapply (seq(1,length(solve)), function(i) mat[i, solve[i]])
    
  vals
}


topicStabilityMG <- function (listOfLists1, listOfLists2) {
  m <- cosinMatrix (listOfLists1, listOfLists2)
  hvals <- hungarian (m)
  mean (hvals)
}

getTopWordsWithScores <- function (corpus, docImps, dtm, topicCount=10, settings) {
  stm <- makeSupervisedTopicModel (corpus, docImps, matrix=dtm, topicCount=topicCount, settings=settings)
  topWords <- lapply (seq(1,topicCount), function(i) sort (stm$topics[i,], decreasing=TRUE)[1:10])
  topWords
}


topicStabilityForKTopicsMG <- function (corpus, docImps, dtm, topicCount=10, settings) {
  cat ("calculating for",topicCount,"topics","\n")
  
  results <- lapply (seq(1,settings$iter), function(i) {
    sampleTopWords <- getTopWordsWithScores (corpus, docImps, dtm, topicCount, settings)
  })

  results
}


overallTopicStability <- function (corpus, docImps, krange=seq(2,12), settings=list(iter=6, eit=30, mit=30, alpha = 1.0, trace=0L)) {
  library (parallel)
  
  dtm <- makeTermMatrices(corpus)$direct
  ptm <- proc.time()
  cl <- makeCluster (4)
  clusterEvalQ(cl, {library(topicmodels); library(lda); library(tm); library(clue)})
  clusterExport(cl=cl, varlist=c("corpus", "docImps", "dtm", "getTopWordsWithScores", "topicStabilityMG", 
                                 "makeSupervisedTopicModel", "topicStabilityForKTopicsMG", "settings",
                                 "cosin", "convertToVector", "cosinMatrix", "hungarian"), envir=environment())
  
  results <- parLapply (cl, krange, function (i) topicStabilityForKTopicsMG (corpus, docImps, dtm, topicCount=i, settings=settings))
  
  stopCluster (cl)
  cat (proc.time() - ptm, "ms elapsed")

  #results
  
  uberResult <- sapply (seq(1,length(results)), function(k) {
    topicResults <- results [[k]]
    overallResult <- sapply (seq(2, length(topicResults)), function (m) {
      sapply (seq (1, m-1), function (n) {
        topicStabilityMG (topicResults[[m]], topicResults[[n]])
      })
    })
    
    unlist(overallResult)
  })
  
  uberResult
}



topicAccuracy <- function (corpus, docImps, settings, topicCount=10, stm=NA, lda_obj=NA) {
  # return result of slda for this topic size
  dtm <- makeTermMatrices(corpus)$direct
  
  if (is.na(stm)) {
    stm <- makeSupervisedTopicModel (dtm, docImps, settings, topicCount=topicCount)
  }
  
  # calculate what document scores should be if dog-fed back in
  if (is.na(lda_obj)) {
    lda_obj <- dtm2ldaformat(dtm)
  }
  predict <- slda.predict (lda_obj$documents, stm$topics, stm$model, alpha=settings$alpha, eta=0.1)
  
  # squares of differences
  res <- (as.vector(docImps) - predict[,1])
  res <- res * res
  
  # return results
  list(res=res, predict=predict[,1], mean = sqrt(mean(res)), topicCount=topicCount, corr = cor (docImps, predict[,1]))
}



parUberTopicAccuracy <- function (docImps, dtmdirect, settings = list(eit=50, mit=50, alpha=1.0, trace=0L)) {
  library (parallel)
  library (lda)
  
  lda_obj <- dtm2ldaformat(dtmdirect)
  # parallelise
  cl <- makeCluster (4)
  clusterEvalQ(cl, {library(topicmodels); library(lda); library(tm)})
  clusterExport(cl=cl, varlist=c("docImps", "dtmdirect", "settings", "lda_obj", "topicAccuracy", "makeSupervisedTopicModel"), envir=environment())
  ures <- parLapply (cl, seq(2,15), function (tsize) {
    topicAccuracy (docImps, dtmdirect, settings, topicCount=tsize, lda_obj=lda_obj)
  })
  stopCluster (cl)
  
  ures
}

drawUres <- function (ures) {
  m <- ures[[1]]$topicCount
  x  <- seq (m, m+length(ures)-1)
  y1 <- sapply (ures, function(i) i$mean)
  y2 <- sapply (ures, function(i) i$sd)
  plot(x,y1,type="l",col="blue")
  par(new=T)
  plot(x,y2,type="l",col="green", axes=F)
  axis (side=4)
}

drawStable <- function (data, start) {
  library (ggplot2)
  
  smean <- sapply (seq(1, length(data[1,])), function(i) mean (unlist(data[,i])))
  ssd <- sapply (seq(1, length(data[1,])), function(i) sd (unlist(data[,i])))
  tc <- seq(start, start+length(data[1,])-1)
  dframe <- data.frame (smean, ssd, tc)
  colnames (dframe) <- c("Mean", "SD", "topicCount")
  
  ggplot(dframe, aes(x=topicCount, y=Mean)) + 
    geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.1) +
    geom_line() +
    geom_point()
  
  #dframe
}

#' Produce visualisation of LDA topic corpus using LDAvis package
#'
#' Takes in corpus and supervised topic model and produces interactive web page with topics marked on it
#' @export
#' @param corpus TM document corpus
#' @param slda supervised LDA topic models
#' @return alpha optional SLDA variable, 1.0 if docs contain diverse material which the merged ones do
#' @return eta an even less explanatory optional SLDA variable, never found a plain words explanation for it yet. Best leave to default.
#' @return dir the directory name in which to place generated web pages. optional, default is "vis"
#' @examples
#' \dontrun{
#' makeLDAVisJSON (corpus, topicModel, dir="vis")
#' }
makeLDAVisJSON <- function (corpus, slda, alpha=1.0, eta=0.1, dir="vis") {
  library (LDAvis)
  library (servr)
  library (lda)
  
  theta <- t(apply(slda$document_sums + alpha, 2, function(x) x/sum(x)))
  phi <- t(apply(t(slda$topics) + eta, 2, function(x) x/sum(x)))
  
  matrix <- makeTermMatrices(corpus)$direct
  lda_obj <- dtm2ldaformat(matrix)
  
  docTokenCounts <- as.vector(rowSums(as.matrix(matrix)))
  termTokenCounts <- as.vector(colSums(as.matrix(matrix)))
  
  # create the JSON object to feed the visualization:
  json <- createJSON(phi = phi, 
                     theta = theta, 
                     doc.length = docTokenCounts,
                     vocab = lda_obj$vocab,
                     term.frequency = termTokenCounts
  )
  
  serVis(json, out.dir = dir, open.browser = TRUE)
}