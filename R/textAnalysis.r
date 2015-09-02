#' A Text Loading Function
#'
#' This function chews a load of pdfs in a fixed directory
#' @keywords impact ref text
#' @export
#' @return list containing metric results, and document corpora along with supervised topic models
fromTheTop <- function (mySettings=list()) {
  library (RCurl)
  
  defaultSettings <- list (
    dirName = "C:/Martin/Data/Impact/REF submission documents/Case Studies",
    boilerPlateTerms = c(
      "Page",
      "Impact case study (REF3b)",
      "Title of case study:",
      "(indicative maximum 100 words)",
      "(indicative maximum 500 words)",
      "(indicative maximum of six references)",
      "(indicative maximum 750 words)",
      "(indicative maximum of 10 references)"
    ),
    sectionHeadings = c(
      "Summary of the impact",
      "Underpinning research",
      "References to the research",
      "Details of the impact",
      "Sources to corroborate the impact|Sources to corroborate impact|References to corroborate the impact" 
    ),
    impactSettings = list(uoa=19, profile="Impact"),
    helperFiles = list(buzzFile1="C:/Martin/Data/Impact/buzz.txt", buzzFile2="C:/Martin/Data/Impact/busibuzz.txt"),
    topicCount = 10,
    topicIter = 10,
    keepSpaces = FALSE,
    places = TRUE,
    orgs = TRUE
  )
  
  mergedSettings <- merge.list (mySettings, defaultSettings)
  
  corpus <- loadDocs (mergedSettings$dirName)
  corpus <- primeCaseStudyDocs (corpus, mergedSettings)
  pwordCorpus <- makeProperWordCorpus (corpus, mergedSettings)
  results <- doMetrics (corpus, pwordCorpus, mergedSettings)
  
  write.csv (results, "results.csv", row.names=FALSE, na="")
  
  results
}


#' A Multiple PDF Document Loading Function
#'
#' This function chews a load of pdfs in a given directory
#' @export
#' @param dirName directory PDFs are located in
#' @param isRecursive flag whether sub-directories should be searched
#' @return TM document corpus
#' @examples
#' loadDocs (dirName="C:/Martin/Data/Impact/REF submission documents/Templates", isRecursive=FALSE)
loadDocs <- function (dirName="C:/Martin/Data/Impact/REF submission documents/Case Studies", isRecursive=TRUE) {
  library(tm)
  library(stringr)
  
  dirPath = file.path(dirName)
  docs = Corpus (DirSource (dirPath, recursive=isRecursive), readerControl = list(reader=readPDF))
  
  docs
}


#' Corpus quality control and metadata attachment
#'
#' Takes in original case study corpus, attaches metadata, sections the content, filters boilerplate
#' and removes documents that fail some quality metric (empty sections)
#' Settings list must include three properties:
#' 1) boilerPlate = array of boilerplate strings to strip from docs
#' 2) sectionHeadings = ordered array of sectionHeadings used to split documents up
#' 3) impactSettings = list that describes where impact scores are sourced from, cname=filepath, uoa=unit of assessment number, profile=assessment type (i.e "impact")
#' @param docs TM document corpus
#' @param settings list, must include three properties: boilerPlate, sectionHeadings and impactSettings
#' @return altered TM document corpus, each doc now contains a set number of sections
#
primeCaseStudyDocs <- function (docs, settings) {

  impactSettings <- settings$impactSettings
  docs <- attachMetaToDocs (docs, impactScoreReader (impactSettings))
  docs <- filterOutNA (docs)
  docs <- removeStrings (docs, settings$boilerPlate)
  docs <- sectionise (docs, settings$sectionHeadings)
  docs <- removeDodgyDocs (docs)
  
  docs
}



# remove docs from corpus that are flagged as having dodgy sections
removeDodgyDocs <- function (sectDocs) {
  dodgyDocIndices <- sectionCountSanityCheck (sectDocs, verbose=TRUE)
  a <- 1:length(sectDocs)
  keep <- a[!a%in%dodgyDocIndices]
  sectDocs$content <- sectDocs$content[keep]
  
  sectDocs
}


# which docs have dodgy sections (zero length)?
sectionCountSanityCheck <- function (sectDocs, verbose=FALSE) {
  library (tm)
  
  arr <- matrix(, nrow=5, ncol=length(sectDocs))
  for (i in 1:5) {
    arr[i,] <- sapply (sectDocs, function(doc) nchar(content(doc)[i]))
  }
  
  arr2 <- which (arr == 0, arr.ind=TRUE)
  
  if (verbose) {
    outputZeroSections <- function (i) {
      cat ("Zero Section", i[1], "Doc", i[2], meta(sectDocs[[i[2]]], "name"), meta(sectDocs[[i[2]]], "id"), "\n")
    }
    apply (arr2, 1, outputZeroSections)
    cat ("total", nrow(arr2), "zero length sections\n")
  }
  

  avg <- sapply (seq_along(1:5), function(i) mean (arr[i,]))
  sd <- sapply (seq_along(1:5), function(i) sd (arr[i,]))
  
  copyArr <- matrix(, nrow=5, ncol=length(sectDocs))
  for (i in 1:5) {
    for (j in 1:length(arr[1,])) {
      l <- arr [i,j]
      bool <- l < (avg[i] - (sd[i] * 2.5)) || l > (avg[i] + (sd[i] * 2.5))
      copyArr [i,j] <- bool
    }
  }
  arr4 <- which (copyArr == 1, arr.ind=TRUE)
  
  if (verbose) {
    outputLongerSectionedDocs <- function (i) {
      ccount <- arr[i[1],i[2]]
      cat ("Odd length section", ccount, "characters", ccount / avg[i[1]], "rs", j, meta(sectDocs[[i[2]]], "name"), i[1], meta(sectDocs[[i[2]]], "id"), "\n")
    }
    apply (arr4, 1, outputLongerSectionedDocs)
    cat ("total", nrow(arr4), "odd length sections\n")
  }
  
  dodgyDocs <- unique (arr2[,"col"])
  
  dodgyDocs
}



# this removes all of a set of strings from the corpus collection
# do it here as we can remove multi-word items and phrases
removeStrings <- function (docs, strings=c("")) {
  regStr <- paste (strings, collapse="|")
  regStr <- gsub ("(", "\\(", regStr, fixed=TRUE)
  regStr <- gsub (")", "\\)", regStr, fixed=TRUE)
  
  remove <- function (doc) {
    gsub (regStr, "", content(doc))
  }
  
  for (i in 1:length(docs)) {
    content(docs[[i]]) <- remove (docs[[i]])
  }
  
  docs
}


sectionise <- function (docs, sectionHeadings=c("")) {
  sectionHeadings <- sapply (sectionHeadings, function(h) paste ("(?i:", h, ")", sep="") )
  
  sections <- function (doc) {
    onestr <- paste (content(doc), collapse="\n")
    len <- length(sectionHeadings)
    parts <- rep ("", len + 1)
    for (i in 1:len) {
      split <- strsplit (onestr, sectionHeadings[i])[[1]]
      parts[i] <- if (length(split) == 1) {""} else {split[1]}
      # put other splits back together (may be more than one)
      onestr <- paste (split[min (2, length(split)) : length(split)], collapse="\n")
    }
    parts[len + 1] <- onestr
    # remove first part (before first section header), just template and uoa/uni name we already got
    parts <- parts[-1]
    parts
  }
  
  for (i in 1:length(docs)) {
    content(docs[[i]]) <- sections (docs[[i]])
  }
  
  docs
}



#' Read in impact scores from file
#'
#' This function extracts scores from the ref2014 data set given a uoa and profile type
#' @export
#' @param settings, list containing uoa : the unit of assessment, profile: the profile type to score
#' @return a data.table of scores per university
#' @examples
#' impactScoreReader (uoa=19, profile="Impact")
impactScoreReader <- function (settings) {
  library (data.table)
  
  csv2 <- subset(ref2014, Profile==settings$profile)
  csv3 <- subset(csv2, Unit.of.assessment.number==settings$uoa)
  
  gpacalc <- function (x) { ((as.numeric(x['X4.']) * 4) + (as.numeric(x['X3.']) * 3)  
                             + (as.numeric(x['X2.']) * 2) + (as.numeric(x['X1.']) * 1)) / 100.0
  }
  gpa <- apply (csv3, 1, gpacalc)
  names <- as.character (csv3$Institution.name)
  codes <- as.character (csv3$Institution.code..UKPRN.)
  sFrame <- data.frame (names, codes, gpa);
  names(sFrame) <- c("Name", "Code", "GPA")
  
  sTable <- data.table (sFrame, key=c('Name'))
  sTable
}


# Get rid of docs whose unis have no associated impact score or names for the UOA
filterOutNA <- function (docs, fields=c("impact", "name")) {
  # myFilter tests whether doc has a NA name or NA impact score (see fields above)
  # this produces fvals - a true, false array - which is used to filter docs
  myFilter <- function (doc) { ! any (is.na (sapply (fields, function(i) meta (doc, i)))) }
  fvals <- lapply (docs, myFilter)
  filtDocs <- docs[as.logical(fvals)]
  
  filtDocs
}

# Attach university name and then appropriate impact score to each case study doc
attachMetaToDocs <- function (docs, impScores) {
  library (stringr)
  library (tm)
  
  uniNames <- as.vector(impScores$Name)
  uniCodes <- as.vector(impScores$Code)
  
  knownSynonyms = data.frame (
    synonym=c("birkbeck", "brunel university", "leeds metropolitan", "university of roehampton", "soas", "durham university", "plymouth university", "loughborough", "warwick university", "university of the west of england (uwe), bristol"),
    proper=c("Birkbeck College", "Brunel University London", "Leeds Beckett University", "Roehampton University", "School of Oriental and African Studies", "University of Durham", "University of Plymouth", "Loughborough University", "University of Warwick", "University of the West of England, Bristol")
  )
  # because pmatch is case sensitive
  uniNamesLower <- sapply (uniNames, function(n) tolower(n))
  
  # collapse doc content into 1 string, go to first alphanumeric character after Institution: match,
  # that should be institution name. (tho sometimes it isn't, or the name is subtly changed)
  # then match the rest of the string after that against the list of university names
  # pmatch() returns an array of NA's with one '1' in it, which() returns the index
  # of the university name
  perDoc <- function (doc) {
    s <- tolower (paste (content(doc), collapse=' '))
    instPos <- str_locate (s, "institution:")[2]
    
    if (!is.na (instPos)) {
      sstr <- substr (s, instPos+1, nchar(s))
      firstChar <- regexpr ("[[:alnum:]]", sstr)[1]
      sstr <- substr (sstr, firstChar, nchar(sstr))
      # no the's in title
      if (substr (sstr, 1, 3) == "the") {
        sstr <- substr (sstr, 5, nchar(sstr))
      }
      
      logicMatch <- pmatch (uniNamesLower, sstr)
      index <- which (logicMatch == 1)
      
      if (length(index) == 0) {
        # match against synonyms
        logicMatch <- pmatch (as.vector(knownSynonyms$synonym), sstr)
        index <- which (logicMatch == 1)
        
        if (length(index) == 0) {
          # match against inst code
          eightCode <- substr (sstr, 1, 8)
          index <- which(uniCodes==eightCode)
          if (length(index) > 0) {
            #cat ("Case C. Found unicode", eightCode, uniNames[index], index, "\n")
          }
        } else {
          proper <- as.character (knownSynonyms$proper[index])
          index <- which(uniNames==proper)
          if (length(index) > 0) {
            #cat ("Case B. Found synonym", proper, index, "\n")
          }
        }
      } else {
        #cat ("case A. Name Match found", uniNames[index], index,"\n")
      }
    }

    if (length(index) == 0 || is.na(index)) {
      cat ("Error finding institution in", substr(s, 1, 300), "\n")
    }
    index
  }
  
  for (i in seq_along(docs)) { 
    index <- perDoc(docs[[i]])
    meta(docs[[i]], "name") <- as.character(uniNames[index]) 
    meta(docs[[i]], "impact") <- as.numeric (impScores$GPA[index])
  }
  
  docs
}


#' # Merge documents with same metadata information (i.e name) into single documents
#'
#' Preserve content sectioning, copies over uni names and impact scores
#' @export
#' @param docs unmerged corpus
#' @param field in document meta information to merge on. Default is name.
#' @return merged docs by uni name, with extra meta field docCount indicated number of docs merged per institution
#' @examples
#' mergeByInstitution (docs)
mergeByInstitution <- function (docs, metaProp="name") {
  library (tm)
  
  names <- sapply (docs, function(i) meta(i, metaProp))
  byName <- sapply (unique(names), function(n) which(names==n))
  
  byNameMergedContent <- lapply (byName, function(n) {
    allSectionC <- sapply (seq(1,5), function(chunk) {
      allc <- sapply (n, function(i) {
        c <- content(docs[[i]])[chunk]
      })
      paste (allc, collapse=" \n")
    })
    
    allSectionC
  })
  
  # Turn this into a new corpus
  mergedDocs <- Corpus (VectorSource (byNameMergedContent))
   
  # make list of names and impact scores of unis indexed by uni names
  uniDocIndex <- which(!duplicated(names))
  namesImpScores <- lapply (uniDocIndex, function(i) {
    d <- docs[[i]]
    list(name = meta(d, metaProp), impact = meta(d, "impact"))
  })

  # add this data to new merged corpus documents metadata
  for (i in seq_along(namesImpScores)) {
    name <- namesImpScores[[i]]$name 
    meta(mergedDocs[[i]], "name") <- as.character(name) 
    meta(mergedDocs[[i]], "impact") <- as.numeric (namesImpScores[[i]]$impact)
    meta(mergedDocs[[i]], "docCount") <- as.numeric (length(unlist(byName[i])))
  }
  
  mergedDocs
}


# reduce documents to certain sections
filterCorpusSections <- function (sectionedDocs, sectionNo=c(1,2,4)) {
  library (tm)
  for (i in 1:length(sectionedDocs)) {
    sectionedDocs[[i]]$content <- content(sectionedDocs[[i]])[sectionNo]
  }
  
  sectionedDocs
}


# flatten multiple document content sections to one section
# opposite of sectionise basically
flattenContent <- function (docs) {
  library (tm)
  for (i in 1:length(content(docs))) {
    content(docs[[i]]) <- c (paste (content(docs[[i]]), collapse="\n"))  
  }
  
  docs
}


# Count buzzwords present in corpus documents
buzzCount <- function (docs, cname) {
  library (tm)
  library (RWeka)
  
  # transform docs to lowercase and remove punctuation
  docs <- tm_map (docs, content_transformer (tolower))
  docs <- tm_map (docs, removePunctuation)
  
  # do same for buzzword list
  csv <- read.csv (cname)
  vbuzz <- as.vector (csv$Buzzwords)
  vbuzz <- sapply (vbuzz, function(w) tolower(w))
  vbuzz <- sapply (vbuzz, function(w) gsub("[[:punct:]]", "", w))
  
  singleBuzz <- vbuzz[!grepl("\\s", vbuzz)]
  doubleBuzz <- vbuzz[grepl("^\\w+\\s\\w+$", vbuzz)]
  
  dtm <- DocumentTermMatrix (docs, control = list(dictionary=c(singleBuzz)))
  
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  btm <- DocumentTermMatrix (docs, control = list(tokenize = BigramTokenizer, dictionary=c(doubleBuzz)))
  
  countPerDoc1 <- rowSums(as.matrix(dtm))
  countPerDoc2 <- rowSums(as.matrix(btm))
  bzf <- data.frame(single=countPerDoc1, double=countPerDoc2)
  bzf$total <- bzf$single + bzf$double
  
  # example plot of data
  #drawWeightGPAPlot (docs, "impact", unlist(bzf$total), "#444444", xlab="Buzzword Count")
  #bcorr <- calcCorrelation (docs, "impact", unlist(bzf$total), method="pearson")
  bzf
}

vectorMeta <- function (docs, property) {
  as.vector (sapply (docs, function(doc) meta (doc, property)))
}

#' Input unmerged corpus and run metrics on it
#' 
#' Run readability, length, topic model metrics on documents
#' @export
#' @param docs unmerged corpus
#' @param properCorpus unmerged corpus of proper words (or could be any other filtering of original docs really)
#' @param settings list, must include helperFiles$buzzFile1, helperFiles$buzzFile2 - links to buzzword lists, and topicCount, topicIter - values for supervised topic modelling
#' @return list containing metric results, and document corpora along with supervised topic models
#' @examples
#' doMetrics (corpus, properCorpus, list(helperFiles=list(buzzFile1="C:/Martin/Data/Impact/buzz.txt", buzzFile2="C:/Martin/Data/Impact/busibuzz.txt")), topicCount=10, topicIter=30))
doMetrics <- function (docs, properCorpus, settings) {
  library(stringr)
  library(tm)
  
  # 1. Merge documents by institution, because we don't have individual scores per case study
  docs <- mergeByInstitution (docs)
  properCorpus <- mergeByInstitution (properCorpus)
  
  # basic info for results
  resultFrame <- data.frame (uni = vectorMeta(docs, "name"))
  resultFrame$impact <- vectorMeta (docs, "impact")
  resultFrame$studyCount <- vectorMeta (docs, "docCount")
  
  # some stuff only works before we start picking the docs to bits
  
  # 2. do readability scores before punctuation is stripped out
  readScores <- metricReadability (filterCorpusSections (docs, c(1,2,4)))
  resultFrame$readability <- as.vector(sapply (readScores, function(r) r$Readability$FK_read.ease))
  resultFrame$wordCount <- as.vector(sapply(readScores, function(r) r$Readability$word.count))
  resultFrame$wordCountAvg <- resultFrame$wordCount / resultFrame$studyCount
  
  # 3. find buzzwords and two-word phrases
  suppFiles <- settings$helperFiles
  resultFrame$buzzScores1 <- buzzCount (filterCorpusSections (docs, c(1,2,4)), cname=suppFiles$buzzFile1)$total / resultFrame$wordCount
  resultFrame$buzzScores2 <- buzzCount (filterCorpusSections (docs, c(1,2,4)), cname=suppFiles$buzzFile2)$total / resultFrame$wordCount
  
  # 4. strip out punctuation, whitespace, common words, do stemming etc on both versions
  docs <- docProcessing (docs, c("impact"))
  properCorpus <- docProcessing (properCorpus, c("impact"))
  
  # 5. search for years, strip numbers out of docs afterwards
  yrange <- metricExtractYears (docs)
  avgy <- sapply (yrange, function(r) { r$Year %*% r$Freq / sum(r$Freq) })
  firsty <- sapply (yrange, function(r) { r$Year[1] })
  firsty[is.na(firsty)] <- 2014
  avgy[is.na(avgy)] <- 2014
  lasty <- sapply (yrange, function(r) { if (length(r$Year) > 0) { r$Year[length(r$Year)] } else { 2014 }})
  
  resultFrame$firstYear <- as.vector(firsty)
  resultFrame$lastYear <- as.vector(lasty)
  resultFrame$avgy <- as.vector(avgy)
  
  # 6. remove numbers
  docs <- tm_map (docs, removeNumbers)
  
  #wscores <- metricWordScoresImpactWeighted (docs)

  sharedTopicSettings <- list(eit=settings$topicIter, mit=settings$topicIter, alpha = 1.0, trace=1L)
  
  topicModel <- superTopicModel (docs, topicCount=settings$topiCount, sharedTopicSettings)
  topicPredicted <- topicAccuracy (docs, resultFrame$impact, list(alpha=1.0), topicCount=settings$topicCount, stm=topicModel$stm)
  resultFrame$allWordTopicScore <- topicPredicted$predict
  printTopicWeightings (topicModel$stm)
  
  properNameTopicModel <- superTopicModel (properCorpus, topicCount=settings$topiCount, sharedTopicSettings)
  properTopicPredicted <- topicAccuracy (properCorpus, resultFrame$impact, list(alpha=1.0), topicCount=settings$topicCount, stm=properNameTopicModel$stm)
  resultFrame$properWordTopicScore <- properTopicPredicted$predict
  printTopicWeightings (properNameTopicModel$stm)
  
  list(metricTable=resultFrame,
       corpus=docs,
       properCorpus=properCorpus,
       sTopicModel=topicModel$stm,
       sProperTopicModel=properNameTopicModel$stm
  )
}

plots <- function (docs, metricTable) {
  
  # make a colour scale based on docs impact scores
  tcol <- makeColourScale (docs)
  
  drawWeightGPAPlot (docs, "impact", metricTable$readability, "#444444", xlab="Flesch-Kincaid Ease of Readability")
  drawWeightGPAPlot (docs, "impact", metricTable$firstYear, "#444444", xlab="First Year Mentioned")
  results <- drawPlots (docs, tcol)
  drawWeightGPAPlot (docs, "impact", metricTable$allWordTopicScore, "#444444", xlab="Topic Index")
  drawWeightGPAPlot (docs, "impact", metricTable$allWordTopicScore, "#444444", xlab="Topic Index")
}


topicModelPlot <- function (docs, topicCount=NULL) {
  library (tm)
  
  docImps <- as.vector (unlist (meta (docs, "impact")))
  
  if (is.null(topicCount)) {
    topicCount <- round(sqrt(length(docs) / 2))
  }
  
  topicModel <- makeTopicModel (docs, topicCount)
  tscores <- topicModel@gamma
  tindices <- sapply(seq(1,nrow(tscores)), function(i) which(tscores[i,] == max(tscores[i,])))
  
  sorttindices <- sapply (tindices, function(i) {
    v <- tindices[i]
    which(coorder==v)
  })
  
  #drawWeightGPAPlot (docs, "impact", sorttindices, tcol)
  
  topicModel
}


superTopicModel <- function (docs, topicCount=NULL, settings=list(eit=30, mit=30, alpha = 1.0, trace=1L)) {
  library (tm)
  
  docImps <- as.vector (unlist (meta (docs, "impact")))
  
  if (is.null(topicCount)) {
    topicCount <- round(sqrt(length(docs) / 2))
  }
  
  stm <- makeSupervisedTopicModel (docs, docImps, settings, topicCount=topicCount)
  coefs <- stm$coefs
  coorder <- order (coefs)
  tscores <- stm$document_sums
  tindices <- sapply(seq(1,ncol(tscores)), function(i) max(which(tscores[,i] == max(tscores[,i]))) )
  
  sorttindices <- sapply (tindices, function(i) {
    v <- tindices[i]
    which(coorder==v)
  })
  
  list(stm=stm,order=sorttindices)
}


printTopicWeightings <- function (stm) {
  topWords <- top.topic.words (stm$topics)
  coefs <- stm$coefs
  coorder <- order (coefs)
  tdata <- sapply (coorder, function (i) {
    list (topic=i, coef=coefs[i], topWords=topWords[1:10,i])
  })
  
  for (i in 1:length(tdata[1,])) {
    t <- tdata[,i]
    cat (t$coef, t$topWords, "\n")
  }
}


docProcessing <- function (docs, myStops = c(), stripNumbers=FALSE, stem=TRUE) 
{
  library(tm)
  library(SnowballC)
  
  docs <- filterOutOwnName (docs)
  
  # strip simple words, punctuation, whitespace etc
  docs = tm_map (docs, content_transformer (tolower))
  docs = tm_map (docs, removeWords, stopwords("english"))
  docs = tm_map (docs, removeWords, myStops)
  docs = tm_map (docs, stripWhitespace)
  docs = tm_map (docs, removePunctuation)
  #docs = tm_map (docs, removeNumbers)
  docs = tm_map (docs, removeWords, myStops)
  if (stripNumbers) {
    docs = tm_map (docs, removeNumbers)
  }
  
  # stem document, so globalise, globally both --> global
  if (stem) {
    docs = tm_map (docs, stemDocument, lazy=TRUE)
  }
  
  docs
}



# Strip universities titles' out of documents
filterOutOwnName <- function (docs) {
  perDoc <- function (doc) {
    name <- meta(doc, "name")
    doc$content <- gsub (name, "", doc$content)
    
    doc
  }
  
  docs$content <- lapply (docs, perDoc)
  
  docs
}


#' Extract proper names from corpus
#'
#' Takes in corpus and flags to decide which entity types to parse for
#' @param docs TM document corpus
#' @param settings list of boolean flags, set these to true if entities wanted: people, places, orgs, money, date, percentage
#' @return list of proper names per document
#
properNameExtraction <- function (docs, settings) {
  library (tm)
  library (NLP)
  library (parallel)
  library (stringr)
  library (openNLP)
  library ("openNLPmodels.en")
  library (RCurl)
  
  falseySettings <- list(people=FALSE, places=FALSE, orgs=FALSE, money=FALSE, date=FALSE, percentage=FALSE)
  mergedSettings <- merge.list (settings, falseySettings)
    
  ## Chunking needs word token annotations with POS tags.
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  #pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  person_tag_annotator <- if (mergedSettings$people) Maxent_Entity_Annotator (kind = "person") else NA
  org_tag_annotator <- if (mergedSettings$orgs) Maxent_Entity_Annotator (kind = "organization") else NA
  locality_tag_annotator <- if (mergedSettings$places) Maxent_Entity_Annotator (kind = "location") else NA
  money_tag_annotator <- if (mergedSettings$money) Maxent_Entity_Annotator (kind = "money") else NA
  date_tag_annotator <- if (mergedSettings$date) Maxent_Entity_Annotator (kind = "date") else NA
  pc_tag_annotator <- if (mergedSettings$percentage) Maxent_Entity_Annotator (kind = "percentage") else NA
  # openNLPmodels.en has time package but openNLP doesn't recognise it
  # Error:'arg' should be one of "date", "location", "money", "organization", "percentage", "person", "misc"
  # and following on from that openNLP allows the misc argument but the openNLPModels.en doesn't have a package for it
  # time_tag_annotator <- if (time) Maxent_Entity_Annotator (kind = "time") else NA
  
  # ps. money is returning nothing interesting, occasionally a dollar sign

  pipeline <- list (sent_token_annotator, word_token_annotator,
                    person_tag_annotator, org_tag_annotator, locality_tag_annotator,
                    money_tag_annotator, date_tag_annotator, pc_tag_annotator
  )
  pipeline <- pipeline[!is.na(pipeline)]
  # stuff from https://rpubs.com/lmullen/nlp-chapter
  
  
  entityTrawl <- function (s) {
    anno <- NLP::annotate (s, pipeline)
    entities2 (s, anno)
  }
  
  # start loop timing
  ptm <- proc.time()
  
  # sequential
  entities <- lapply (docs, function(d) { 
    cat ("yo\n")
    l <- sapply (d$content, function(s) {
        entityTrawl (as.String(s)) 
    }) 
    names(l) <- seq(1,length(l))
    
    l
  })
  
  # parallel
  #cl <- makeCluster (mc <- getOption("cl.cores", detectCores()))
  #clusterEvalQ(cl, {library(openNLP); library(NLP); library(methods); library ("openNLPmodels.en")})
  #clusterExport(cl=cl, varlist=c("annotate", "entities2", "pipeline"), envir=environment())
  #entities <- parLapply (cl, docStrings, entityTrawl)
  #stopCluster (cl)
  
  # end, report loop timing
  cat (proc.time() - ptm, "ms elapsed")
  
  entities
}



# Extract entities from an AnnotatedPlainTextDocument
entities2 <- function (s, anno, kind) {
  if(hasArg(kind)) {
    k <- sapply(anno$features, `[[`, "kind")
    s[anno[k == kind]]
  } else {
    l <- anno[anno$type == "entity"]
    if (length(l) > 0) { s[l] } else { c("empty") }
  }
}


makeCorpusFromStrings <- function (strings) {
  library(tm)
  
  strings <- lapply (strings, function(d) { sapply (d, function (s) paste(s, collapse=' ')) })
  
  docs <- Corpus (VectorSource (strings))
  
  docs
}

copyMeta <- function (fromCorpus, toCorpus) {
  for (i in seq_along(fromCorpus)) { 
    meta(toCorpus[[i]], "name") <- meta(fromCorpus[[i]], "name")
    meta(toCorpus[[i]], "impact") <- meta(fromCorpus[[i]], "impact")
    meta(toCorpus[[i]], "id") <- meta(fromCorpus[[i]], "id")
  }
  
  toCorpus
}


makeProperWordCorpusFromList <- function (originalCorpus, properNameList, keepSpaces=FALSE) {
  if (!keepSpaces) {
    properNameList <- lapply (properNameList, function(l) { lapply (l, function (sl) { gsub (" ", "", sl) }) })
  }
  
  properCorpus <- makeCorpusFromStrings (properNameList)
  properCorpus <- copyMeta (originalCorpus, properCorpus)
  
  properCorpus
}

#' Produce corpus of proper names from original corpus
#'
#' Takes in corpus and flags to decide which entity types to parse for
#' @param originalCorpus TM document corpus
#' @param settings list of boolean flags to control what is parsed (people, places, orgs, date). keepSpaces decides concatenation of multi-word terms.
#' @return TM document corpus containing just proper names
#' @examples
#' makeProperWordCorpus (corpus, list(keepSpaces=TRUE, people=TRUE, places=TRUE))
makeProperWordCorpus <- function (originalCorpus, settings) {
  originalCorpus <- filterOutOwnName (originalCorpus)
  properNameList <- properNameExtraction (originalCorpus, settings)
  properCorpus <- makeProperWordCorpusFromList (originalCorpus, properNameList, settings$keepSpaces)
  
  properCorpus
}

makeMergedSection5ProperCorpus <- function (originalCorpus, settings) {
  sect5corpus <- filterCorpusSections (originalCorpus, sectionNo = c(5))
  sect5corpus <- filterOutOwnName (sect5corpus)
  mergedSect5Corpus <- mergeByInstitution (sect5corpus)
  mergedSect5ProperCorpus <- makeProperWordCorpus (mergedSect5Corpus, settings)
  mergedSect5StemmedProperCorpus <- docProcessing (mergedSect5ProperCorpus, myStops = c("impact"))
  mergedSect5StemmedProperCorpus <- tm_map (mergedSect5StemmedProperCorpus, removeNumbers)
  
  mergedSect5StemmedProperCorpus
}


metricReadability <- function (docs) {
  library (qdap)
  library (munsell)
  library (parallel)
  
  # qdapify
  qdocs <- as.data.frame (docs, col1="docs", col2="text")
  
  # Split document text strings up into sentences
  qsplitdocs <- sentSplit (qdocs, "text")
  # now lots of fragments, mapped by filename
  # gather fragments up by same filename and stick them through FK test
  perFile <- unique (qsplitdocs$docs)
  fkread <- function (fname) {
    ftext <- qsplitdocs$text[qsplitdocs$docs==fname]
    flesch_kincaid (ftext)
  }
  #scores <- lapply (perFile, fkread)
  
  # parallelise
  cl <- makeCluster (4)
  clusterExport(cl=cl, varlist=c("fkread", "qsplitdocs", "flesch_kincaid"), envir=environment())
  scores <- parLapply (cl, perFile, fkread)
  stopCluster (cl)
  
  scores
}

# can only use this function if we haven't done removeNumbers with tm_map
metricExtractYears <- function (docs) {
  
  ydict <- c(1960:2040)
  yfunc <- function (doc) {
    # restrict to 4 character words, to searching for values between 1960 and 2040, only return values where there's a hit
    # some of these settings are probably made redundent by the others, but being safe
    l <- termFreq(doc, control=list(wordLengths=c(4,4), dictionary=ydict, bounds = list(local = c(1, Inf))))
    # get min and max of returned vals
    ll <- c(l[1],l[length(l)])
    
    df <- data.frame (as.numeric(names(l)), as.vector(l))
    colnames(df) <- c("Year", "Freq")
    
    df
  }
  
  # do the above function for all the documents, using lapply
 yrange <- lapply (docs, yfunc)
 
 yrange
}


# this uses a termdocumentmatrix as opposed to documenttermmatrix
metricWordScoresImpactWeighted <- function (docs, occurrences=c(2,Inf)) {
  tdm <- TermDocumentMatrix(docs, control = list(bounds = list(global = occurrences), wordLengths=c(2,18), weighting = weightTfIdf))
  tdmm <- as.matrix (tdm)
  docImps <- as.vector(unlist(meta (docs, "impact")))
  # Weight terms that occur in institutions statement by impact score of that institute
  ww <- tdmm %*% docImps
  # Weight those scores by the occurence of those terms across whole corpus
  ww2 <- ww / rowSums(tdmm)
  
  l <- length (ww2)
  cat (l, "terms")
  top <- ww2[order(ww2, decreasing=TRUE),][1:20]
  bottom <- ww2[order(ww2, decreasing = TRUE), ][max(l-19, 1):l]
  
  c(top,bottom)
}



# occurrence is a bandpass filter on the number of documents a term needs to occur in
# to make it in to the matrix.
makeTermMatrices <- function (docs, occurrence = c(2,Inf)) {
  library(tm)
  
  dtm <- DocumentTermMatrix(docs, control = list(bounds = list(global = occurrence), wordLengths=c(2,18)))
  #inspect(dtm[1:12, 5001:5010])
  ## do tfxidf
  dtm_i <- weightTfIdf(dtm)
  
  list (direct=dtm, inverse=dtm_i)
}

# matrix cannot be inverse frequencies (idf)
makeTopicModel <- function (corpus, topicCount, matrix=NA) {
  library (topicmodels)
  
  if (is.na(matrix)) {
    matrix <- makeTermMatrices(corpus)$direct
  }
  # remove documents with no terms from matrix
  rowTotals <- apply(matrix , 1, sum)
  matrix <- matrix [rowTotals>0,]

  lda <- LDA (matrix, k = topicCount)
  
  lda
}

# matrix cannot be inverse frequencies (idf)
makeSupervisedTopicModel <- function (corpus, docImps, settings=list(eit=10, mit=10, alpha = 1.0, trace=1L), topicCount, matrix = NA) {
  library (topicmodels)
  library (lda)
  
  if (is.na(matrix)) {
    matrix <- makeTermMatrices(corpus)$direct
  }
  
  # remove documents with no terms from matrix
  rowTotals <- apply(matrix , 1, sum)
  matrix <- matrix [rowTotals>0,]
  # adjust docImps accordingly
  docImps <- docImps[rowTotals>0]
  
  lda_obj <- dtm2ldaformat(matrix)
  
  # assumptions block
  #
  # 1. params
  # appears to be the assummed regression co-efficients
  # I've put in 3 as that's the average impact score. Importantly, they're all the same.
  #
  # 2. alpha
  # http://stats.stackexchange.com/questions/37405/natural-interpretation-for-lda-hyperparameters
  # For the symmetric distribution, a high alpha-value means that each document is likely to 
  # contain a mixture of most of the topics, and not any single topic specifically. 
  # A low alpha value puts less such constraints on documents and means that it is more likely 
  # that a document may contain mixture of just a few, or even only one, of the topics.
  #
  # Thus, since the case studies typically only cover a particular topic or two, 
  # I've made alpha a low default value
  #
  # 
  
  avg <- mean (docImps)
  var <- var (docImps)
  # shapiro.test on case studies gives W = 0.89704, p almost zero (10e-16), so is not normal
  
  ## Initialize the params
  #params <- sample(c(-0.001, 0.001), topicCount, replace=TRUE)
  params <- rep(3.0, topicCount)
  
  result <- slda.em(documents = lda_obj$documents,
                    K = topicCount,
                    vocab = lda_obj$vocab,
                    num.e.iterations = settings$eit,
                    num.m.iterations = settings$mit,
                    alpha = settings$alpha,
                    eta = 0.1,
                    annotations = docImps,
                    params = params,
                    variance = var,
                    lambda = 1.0,  #no effect, when regularise=FALSE
                    logistic = FALSE,
                    regularise = FALSE,
                    method = "sLDA",
                    trace = settings$trace
  )
  
  result
}


makeColourScale <- function (docs) {
  cscale <- topo.colors (100)
  colFunc = function (doc) {
    cscale [meta(doc,"impact") * 20]
  }
  tcol <- sapply(docs, colFunc)
  
  tcol
}


drawPlots <- function (docs, tcol, invMatrix=NA)
{
  if (is.na(invMatrix)) {
    matrices <- makeTermMatrices (docs, c(5, nrow(docs) * 0.8))
    invMatrix <- matrices$inverse
  }
  
  m <- as.matrix (invMatrix)
  rownames(m) <- 1:nrow(m)
  
  ### don't forget to normalize the vectors so Euclidean makes sense
  norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
  m_norm <- norm_eucl(m)
  

  par(mfrow=c(1, 3))
  
  drawKMeans (m_norm, tcol, docs)
  drawMDS (m_norm, tcol)
  
  
  terms <- c("ceo", "director", "msp", "minister", "president", "councillor")
  tws <- c(4, 2, 2, 5, 7, 0.5)
  wordScores <- metricCalcWeightedScore (docs, terms, tws)
  drawWeightGPAPlot (docs, "impact", unlist(wordScores), "#444444", xlab="Good word count")
  
  
  #terms <- c("letter", "direct", "director", "committee", "ceo", "government", "govern", "speech", "polici", "common", "department")
  #tws <- c(5, 3, 3, 3, 3, 5, 5, 2, 3, 4, 1)
  #wordScores <- metricCalcWeightedScore (docs, terms, tws)
  #drawWeightGPAPlot (docs, "impact", unlist(wordScores), "#444444", xlab="Good word count")
  
  wordScores
}

drawKMeans <- function (normMatrix, colScale, docs) {
  ### cluster into 10 clusters
  ## rool ov fumb
  ## http://en.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set#Rule_of_thumb
  k <- round(sqrt(nrow(normMatrix) / 2))
  cl <- kmeans(normMatrix, k)
  
  tlabel <- sapply(1:length(docs), function(i) meta(docs[[i]], "name"))
  x <- cl$cluster
  y <- sapply (docs, function(doc) meta(doc, "impact"))
  fixr <- (k + 1) / 60; # resolution of 200
  symbols(x, y, circles = rep(fixr,length(x)), inches = FALSE, main="K MEANS", xlab="Cluster Index", ylab="Impact GPA", bg=colScale, fg="white")
  #text(x, y, labels = row.names(normMatrix), pos = 2, cex=.7, col = "#44444480")  
}


### show clusters using the first 2 principal components
drawMDS <- function (normMatrix, colScale) {
  d <- dist(normMatrix) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  
  # plot solution 
  x <- fit$points[,1]
  y <- fit$points[,2]
  # tcol <- sapply(1:corpusSize, colFunc)
  symbols(x, y, circles = rep(0.01,length(x)), inches = FALSE, main="METRIC MDS", xlab="D1", ylab="D2", bg=colScale, fg="white")
  #text(x, y, labels = row.names(normMatrix), pos = 2, cex=.7, col = "#44444480")
}

metricCalcWeightedScore <- function (docs, terms, weights) {
  print (length(weights))
  
  perDoc <- function (doc) {
    freq <- termFreq (doc, control = list(dictionary=terms))
    #print (length(freq))
    #print (freq)
    wfreq <- freq * weights
    wsum <- sum (wfreq)
    wsum
  }
  wscores <- lapply (docs, perDoc)
  
  wscores
}


drawWeightGPAPlot <- function (docs, docField, weightScores, colScale, xlab="WEIGHT", ylab="IMPACT GPA") {
  library (tm)
  x <- weightScores
  y <- sapply (docs, function(doc) meta (doc, docField))
  fixr <- (diff(range(weightScores)) + 0.01) / 200; # resolution of 200
  symbols(x, y, circles = rep (fixr, length(x)), inches = FALSE, main="Var by Impact", xlab=xlab, ylab=ylab, bg=colScale, fg="white")
  #text(x, y, labels = c(1:length(docs)), pos = 2, cex=.7, col = "#44444480")
}


calcCorrelation <- function (docs, docField, scores, method="pearson") {
  library (tm)
  ind <- sapply (docs, function(doc) meta (doc, docField))
  vals <- cor.test (ind, scores, method=method)
  
  vals
}

#' REF 2014 data
#'
#' A dataset containing REF ratings for all UK institutions
#' across multiple aspects and units of assessment
#'
#' @format A data frame with 7644 rows and 18 variables:
#' \describe{
#'   \item{Institution.code..UKPRN.}{uni code}
#'   \item{Institution.name}{university name}
#'   \item{Institution.sort.order}{numerical sort order}
#'   \item{Main.panel}{assessment panel identifier}
#'   \item{Unit.of.assessment.number}{UOA code}
#'   \item{Unit.of.assessment.name}{UOA name}
#'   \item{Multiple.submission.letter}{identifier for multiple entries from one institution}
#'   \item{Multiple.submission.name}{department within university}
#'   \item{Joint.submission}{submission between universities}
#'   \item{Profile}{Aspect of REF}
#'   \item{FTE.Category.A.staff.submitted}{number of staff}
#'   \item{X4.}{4 star rating}
#'   \item{X3.}{3 star rating}
#'   \item{X2.}{2 star rating}
#'   \item{X1.}{1 star rating}
#'   \item{unclassified}{numerical sort order}
#'   \item{Area}{geographic area code within UK}
#'   \item{Post92}{new university flag}
#' }
#' @source ref2014 website
"ref2014"
