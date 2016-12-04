library(tm)
library(SnowballC)
library(readr)
library(stringr)
library(XML)
library(RTextTools)

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
          x <- x[(names(x) == "AbstractText")]
          x <- x[sapply(x, function(y) "text" %in% names(y))]
          return(paste(sapply(x, function(y) y$text), collapse=" "))
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

corpusTexts <- list(jama=list(), nejm=list())

for(journalName in c("jama", "nejm")) {
  if(journalName == "jama") {
    texts <- jamaTexts
  } else if (journalName == "nejm") {
    texts <- nejmTexts
  }
  
  is.clinical.trial <- sapply(texts$publication.types, function(pubtypes) any(clinical.trial.pubtypes %in% pubtypes))
  is.not.clinical.trial <- !is.clinical.trial
  is.case.report <- sapply(texts$publication.types, function(pubtypes) "Case Reports" %in% pubtypes)
  
  textsToInclude <- list(
              all=rep(TRUE, length(texts$year)),
              clinical.trials=is.clinical.trial,
              non.clinical.trials=is.not.clinical.trial,
              case.reports=is.case.report)
  
  corpusTexts[[journalName]]  <- lapply(c("title", "abstract"), function(titleOrAbstract){
      sublistToReturn <- lapply(names(textsToInclude), function(textTypeToInclude) {
        includeVector <- textsToInclude[[textTypeToInclude]]
        corpusToReturn <- NULL
        for(year in 1976:2015) {
          if(titleOrAbstract == "title") {
            current.corpus.texts <- texts$title[texts$year == year & includeVector]
          } else if(titleOrAbstract == "abstract") {
            current.corpus.texts <- texts$abstract[texts$year == year & includeVector]
          }
          toAdd <- Corpus(VectorSource(paste(current.corpus.texts, collapse = " ")))
          meta(toAdd, "id", "local") <- paste0(titleOrAbstract, "_", textTypeToInclude, "_", year)
          if(is.null(corpusToReturn)) {
            corpusToReturn <- toAdd
          } else {
            corpusToReturn <- c(corpusToReturn, toAdd)
          }
        }
        
        # To lowercase
        corpusToReturn <- tm_map(corpusToReturn, content_transformer(tolower))
        # Remove punctuation
        corpusToReturn <-tm_map(corpusToReturn, removePunctuation)
        return(corpusToReturn)
      })
      names(sublistToReturn) <- names(textsToInclude)
      return(sublistToReturn)
    })
  names(corpusTexts[[journalName]]) <- c("title", "abstract")
}

myMat <- DocumentTermMatrix(corpusTexts$jama$title$non.clinical.trials)

#
#BigramTokenizer <- function(x) 
#  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#myMat <- TermDocumentMatrix(title.texts.case.report, control = list(tokenize = BigramTokenizer))
#myMat <- t(myMat)

matrix.colsums <- colSums(inspect(myMat))
matrix.rowsums <- rowSums(inspect(myMat))
year.term.count.sums <- matrix.rowsums
freq.count.table <- data.frame(word=colnames(myMat), count=matrix.colsums)

# To avoid divide-by-zero errors, replace zero sums with 1. This variable is only used in the row-wise division.
myTDMCounts <- inspect(myMat)

myMat.freq.per.1000 <- myTDMCounts * 1000 / matrix.rowsums

# only include terms that have occurred at least 1/10000 (0.01%) of the time
myTDMHiFreq.per.1000 <- myMat.freq.per.1000[,freq.count.table$count > 0.0001 * sum(freq.count.table$count)]
myTDMCountsHiFreq <- myTDMCounts[,freq.count.table$count > 0.0001 * sum(freq.count.table$count)]

years <- 1976:2015
year.coefficients <- apply(myTDMHiFreq.per.1000, 2, function(y) coef(lm(y ~ years))["years"])
year.pvals <- apply(myTDMHiFreq.per.1000, 2, function(y) summary(lm(y ~ years))$coefficients[2,4])

fdr.adj.year.pvals <- p.adjust(year.pvals, method="fdr")

sig.year.coefficients <- year.coefficients[fdr.adj.year.pvals < 0.01]
sort(sig.year.coefficients)

barplot(myTDMHiFreq.per.1000[,"patient with"])

current.freq.per.1000 <- myTDMHiFreq.per.1000[,"the elderly"]

barplot.df.to.plot <- data.frame(
                              freq.per.1000 = current.freq.per.1000,
                              year = 1976:2015
                              )

#current.conf.ints <- lapply(1:nrow(myTDMHiFreq.per.1000), function(x) {
#  prop.test.out <- prop.test(myTDMCounts[x,"diabetes"], year.term.count.sums[x])
#  prop.test.conf.ints <- as.vector(prop.test.out$conf.int * 1000)
#  names(prop.test.conf.ints) <- c("lower", "upper")
#  return(prop.test.conf.ints)
#  })
#current.standard.errors <- sapply(1:nrow(myTDMHiFreq.per.1000), function(x) {
#  k <- myTDMCounts[x,"diabetic"]
#  n <- year.term.count.sums[x]
#  pbar <- k / n
#  SE = sqrt(pbar * (1-pbar)/n)
#  return(SE)
#})
#barplot.df.to.plot <- as.data.frame(do.call(rbind, current.conf.ints))
#barplot.df.to.plot$freq.per.1000 <- current.freq.per.1000
#barplot.df.to.plot$year <- 1976:2015
#barplot.df.to.plot$se <- current.standard.errors * 1000

#ggplot(barplot.df.to.plot, aes(x = year, y = freq.per.1000)) +  
#  geom_point(stat="identity", fill="blue") + 
#  geom_errorbar(aes(ymin=freq.per.1000-se, ymax=freq.per.1000+se)) +
#  ggtitle("Bar plot with 95% confidence intervals") + # plot title
#  theme_bw() + # remove grey background (because Tufte said so)
#  theme(panel.grid.major = element_blank())
ggplot(barplot.df.to.plot, aes(x = year, y = freq.per.1000)) +  
  stat_smooth() +
  geom_point(stat="identity", fill="blue") + 
#  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  ggtitle("Frequency of the word \"diabetic\"") + # plot title
  xlab("Year") + 
  ylab("Frequency per 1000 words") +
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank())

# do this barplot in chunks of five years
barplot(tapply(myTDMHiFreq.per.1000[,"from"], (seq_along(myTDMHiFreq.per.1000[,"from"])-1) %/% 5, sum))

# find all words after "among"
out <- sapply(title.texts, function(x) unlist(str_extract_all(x$content, '(?<=among\\s)\\w+')))
# Can do some analyses on just these words, can do fancy things like take up to the first noun after "among"

