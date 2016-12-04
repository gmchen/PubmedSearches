library(tm)
library(SnowballC)
library(readr)
library(stringr)
library(XML)

jamaTexts <- list(title=character(), abstract=character(), keywords=list(), year=numeric(), month=numeric(), publication.types=list())
nejmTexts <- list(title=character(), abstract=character(), keywords=list(), year=numeric(), month=numeric(), publication.types=list())

# Load data from xml
for(year in 1976:2015) {
  yearIndex <- year - 1975
  for(month in 1:12) {
    for(journalName in c("jama", "nejm")) {
      print(paste("Parsing year", year, "and month", month))
      if(journalName == "jama") {
        current.xml.text <- read_file(paste0("Pubmed_JAMA/xml/", year, "_", month, ".xml"))
      } else if(journalName == "nejm") {
        current.xml.text <- read_file(paste0("Pubmed_NEJM/xml/", year, "_", month, ".xml"))
      }
      
      current.xml.text.split <- str_split(current.xml.text,  "(?=\\<\\?xml version\\=)")[[1]]
      current.xml.text.split <- current.xml.text.split[current.xml.text.split != ""]
      xml.text.lists <- lapply(current.xml.text.split, xmlToList)
      xml.text.list <- do.call(c, xml.text.lists)
      
      get_from_list <- function(my.list, my.named.subval) {
          val <- tryCatch({
            my.list[[my.named.subval]]
          }, error = function(e) {
            NA
          })
          return(val)
      }
      current.titles <- unname(sapply(xml.text.list, get_from_list, c("MedlineCitation", "Article", "ArticleTitle")))
      
      current.abstract.lists <- sapply(xml.text.list, get_from_list, c("MedlineCitation", "Article", "Abstract"))
      
      current.abstract.lists <- lapply(xml.text.list, get_from_list, c("MedlineCitation", "Article", "Abstract"))
      # Sometimes $AbstractText is a character vector, sometimes is a list with $text character vectors
      current.abstracts <- sapply(current.abstract.lists, function(x) {
        if(is.null(x)) {
          return(NULL)
        }
        if(is.character(x$AbstractText)) {
          return(x$AbstractText)
        }
        if(is.list(x$AbstractText)) {
          return(paste(sapply(x[(names(x) == "AbstractText")], function(y) y$text), collapse=" "))
        }
        else{
          stop("Found an AbstractText field that is neither null, a character, or a list")
          }
        })
          
      current.keywords <- unname(lapply(xml.text.list, get_from_list, c("MedlineCitation", "MeshHeadingList")))
      current.keywords <- lapply(current.keywords, function(x) sapply(x, get_from_list, c("DescriptorName", "text")))
      
      current.years <- unname(lapply(xml.text.list, get_from_list, c("PubmedData", "History", "PubMedPubDate", "Year")))
      current.years <- as.numeric(current.years)
      current.months <- unname(lapply(xml.text.list, get_from_list, c("PubmedData", "History", "PubMedPubDate", "Month")))
      current.months <- as.numeric(current.months)
      current.publication.types <- unname(lapply(xml.text.list, get_from_list,c("MedlineCitation", "Article", "PublicationTypeList")))
      current.publication.types <- lapply(current.publication.types, function(x) sapply(x, get_from_list,"text"))
      
      # check that all lengths are equal
      current.length <- length(current.titles)
      if(current.length != length(current.abstracts) ||
         current.length != length(current.keywords) || 
         current.length != length(current.years) ||
         current.length != length(current.months) ||
         current.length != length(current.publication.types)) {
          stop("Not all lists are equal length")
        }
      vals.to.add <- list(title=current.titles, abstract=current.abstracts, keywords=current.keywords, year=current.years, month=current.months, publication.types=current.publication.types)
      
      if(journalName == "jama") {
        jamaTexts <- lapply(names(vals.to.add),function(x) c(jamaTexts[[x]],vals.to.add[[x]]))
        names(jamaTexts)<-names(vals.to.add)
      } else if(journalName == "nejm") {
        nejmTexts <- lapply(names(vals.to.add),function(x) c(nejmTexts[[x]],vals.to.add[[x]]))
        names(nejmTexts)<-names(vals.to.add)
      }
    }
  }
}

save(jamaTexts, file="jamaTexts.RData")
save(nejmTexts, file="nejmTexts.RData")

#unique(sort(unlist(texts$publication.types)))
clinical.trial.pubtypes <- c("Clinical Trial",                                
                             "Clinical Trial, Phase I",                               
                             "Clinical Trial, Phase II",                              
                             "Clinical Trial, Phase III",                               
                             "Clinical Trial, Phase IV",
                             "Twin Study",
                             "Randomized Controlled Trial",
                             "Pragmatic Clinical Trial",
                             "Observational Study",
                             "Multicenter Study",
                             "Meta-Analysis",
                             "Evaluation Studies",
                             "Controlled Clinical Trial")
year.counts.with.clinical.trial <- sapply(1976:2015, function(year) {
  mean(sapply(texts$publication.types[texts$year == year], function(pubtypes) any(clinical.trial.pubtypes %in% pubtypes)))
  })
plot(1976:2015, year.counts.with.clinical.trial)

year.counts.with.case.report <- sapply(1976:2015, function(year) {
  mean(sapply(texts$publication.types[texts$year == year], function(pubtypes) "Case Reports" %in% pubtypes))
  })
plot(1976:2015, year.counts.with.case.report)

# create a corpus for each year of title texts of clinical trials

title.texts.with.clinical.trial <- NULL
for(year in 1976:2015) {
  is.clinical.trial <- sapply(texts$publication.types[texts$year == year], function(pubtypes) any(clinical.trial.pubtypes %in% pubtypes))
  current.corpus.texts <- texts$title[texts$year == year][is.clinical.trial]
  toAdd <- Corpus(VectorSource(paste(current.corpus.texts, collapse = " ")))
  meta(toAdd, "id", "local") <- paste0("Title_text_clinical_trial_", year)
  if(is.null(title.texts.with.clinical.trial)) {
    title.texts.with.clinical.trial <- toAdd
  } else {
    title.texts.with.clinical.trial <- c(title.texts.with.clinical.trial, toAdd)
  }
}

# To lowercase
title.texts.case.report <- tm_map(title.texts.case.report, content_transformer(tolower))
# Remove punctuation
title.texts.case.report <-tm_map(title.texts.case.report, removePunctuation)

title.texts.case.report <- NULL
for(year in 1976:2015) {
  is.case.report <- sapply(texts$publication.types[texts$year == year], function(pubtypes) "Case Reports" %in% pubtypes)
  current.corpus.texts <- texts$title[texts$year == year][is.clinical.trial]
  toAdd <- Corpus(VectorSource(paste(current.corpus.texts, collapse = " ")))
  meta(toAdd, "id", "local") <- paste0("Title_text_case_report_", year)
  if(is.null(title.texts.case.report)) {
    title.texts.case.report <- toAdd
  } else {
    title.texts.case.report <- c(title.texts.case.report, toAdd)
  }
}

# To lowercase
title.texts.case.report <- tm_map(title.texts.case.report, content_transformer(tolower))
# Remove punctuation
title.texts.case.report <-tm_map(title.texts.case.report, removePunctuation)

title.texts.excluding.clinical.trials <- NULL
for(year in 1976:2015) {
  is.clinical.trial <- sapply(texts$publication.types[texts$year == year], function(pubtypes) any(clinical.trial.pubtypes %in% pubtypes))
  current.corpus.texts <- texts$title[texts$year == year][!is.clinical.trial]
  toAdd <- Corpus(VectorSource(paste(current.corpus.texts, collapse = " ")))
  meta(toAdd, "id", "local") <- paste0("Title_text_excluding_clinical_trials_", year)
  if(is.null(title.texts.excluding.clinical.trials)) {
    title.texts.excluding.clinical.trials <- toAdd
  } else {
    title.texts.excluding.clinical.trials <- c(title.texts.excluding.clinical.trials, toAdd)
  }
}

# To lowercase
title.texts.excluding.clinical.trials <- tm_map(title.texts.excluding.clinical.trials, content_transformer(tolower))
# Remove punctuation
title.texts.excluding.clinical.trials <-tm_map(title.texts.excluding.clinical.trials, removePunctuation)

abstract.texts.with.clinical.trial <- NULL
for(year in 1976:2015) {
  is.clinical.trial <- sapply(texts$publication.types[texts$year == year], function(pubtypes) any(clinical.trial.pubtypes %in% pubtypes))
  has.abstract <- unlist(sapply(texts$abstract[texts$year == year], function(abstract) paste(unlist(abstract), collapse=" ") != "NA"))
  current.corpus.texts <- paste(unlist(texts$abstract[[is.clinical.trial & has.abstract]]), collapse=" ")
  toAdd <- Corpus(VectorSource(paste(current.corpus.texts, collapse = " ")))
  meta(toAdd, "id", "local") <- paste0("Abstract_text_clinical_trial_", year)
  if(is.null(abstract.texts.with.clinical.trial)) {
    abstract.texts.with.clinical.trial <- toAdd
  } else {
    abstract.texts.with.clinical.trial <- c(abstract.texts.with.clinical.trial, toAdd)
  }
}

title.texts <- NULL
for(year in 1976:2015) {
  toAdd <- Corpus(VectorSource(paste(texts$title[texts$year == year], collapse=" ")))
  meta(toAdd, "id", "local") <- paste0("Title_text_", year)
  if(is.null(title.texts)) {
    title.texts <- toAdd
  } else {
    title.texts <- c(title.texts, toAdd)
  }
}
# To lowercase
title.texts <- tm_map(title.texts, content_transformer(tolower))
# Remove punctuation
title.texts <-tm_map(title.texts, removePunctuation)


# To lowercase
title.texts.with.clinical.trial <- tm_map(title.texts.with.clinical.trial, content_transformer(tolower))
# Remove punctuation
title.texts.with.clinical.trial <-tm_map(title.texts.with.clinical.trial, removePunctuation)
# To lowercase
abstract.texts.with.clinical.trial <- tm_map(abstract.texts.with.clinical.trial, content_transformer(tolower))
# Remove punctuation
abstract.texts.with.clinical.trial <-tm_map(abstract.texts.with.clinical.trial, removePunctuation)



myMat <- DocumentTermMatrix(title.texts)

matrix.colsums <- colSums(inspect(myMat))
matrix.rowsums <- rowSums(inspect(myMat))
freq.count.table <- data.frame(word=colnames(myMat), count=matrix.colsums)

# To avoid divide-by-zero errors, replace zero sums with 1. This variable is only used in the row-wise division.
matrix.rowsums[matrix.rowsums == 0] <- 1
myMat.data <- inspect(myMat)

#myMat.freq.per.1000 <- t(t(myMat.data) * 1000 / matrix.rowsums) # This seems to cause floating point errors!
#myMat.freq.per.1000 <- sweep(myMat.data * 1000, 1, matrix.rowsums, "/")
myMat.freq.per.1000 <- myMat.data * 1000 / matrix.rowsums

# only include terms that have occurred at least 1/10000 (0.01%) of the time
myTDMHiFreq <- myMat.freq.per.1000[,freq.count.table$count > 0.0001 * sum(freq.count.table$count)]

years <- 1976:2015
year.coefficients <- apply(myTDMHiFreq, 2, function(y) coef(lm(y ~ years))["years"])
year.pvals <- apply(myTDMHiFreq, 2, function(y) summary(lm(y ~ years))$coefficients[2,4])

fdr.adj.year.pvals <- p.adjust(year.pvals, method="fdr")

sig.year.coefficients <- year.coefficients[fdr.adj.year.pvals < 0.01]
sort(sig.year.coefficients)

barplot(myTDMHiFreq[,"among"])

# do this barplot in chunks of five years
barplot(tapply(myTDMHiFreq[,"from"], (seq_along(myTDMHiFreq[,"from"])-1) %/% 5, sum))

# find all words after "among"
out <- sapply(title.texts, function(x) unlist(str_extract_all(x$content, '(?<=among\\s)\\w+')))
# Can do some analyses on just these words, can do fancy things like take up to the first noun after "among"

