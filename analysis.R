library(tm)
library(SnowballC)
library(readr)
library(XML)

jamaTexts <- list(title=NULL, abstract=NULL, keyword=NULL)

# Load data

for(year in 1976:2015) {
  yearIndex <- year - 1975
  
  for(month in 1:12) {
    # textType: 1 for title, 2 for abstract, 3 for keyword
    for(textType in 1:3) {
      if(textType == 1) {
        toAdd <- Corpus(DirSource("Pubmed_BMJ/", pattern = paste0(year, "_", month, "_TitleText\\.txt")))
      } else if (textType == 2) {
        toAdd <- Corpus(DirSource("Pubmed_BMJ/", pattern = paste0(year, "_", month, "_AbstractText\\.txt")))
      } else if(textType == 3) {
        toAdd <- Corpus(DirSource("Pubmed_BMJ/", pattern = paste0(year, "_", month, "_KeywordText\\.txt")))  
      }
      meta(toAdd, "Year", "local") <- year
      meta(toAdd, "Month", "local") <- month
      meta(toAdd, "MonthIndex", "local") <- (year - 1976) * 12 + (month - 1) + 1
      
      if(is.null(jamaTexts[[textType]])) {
        jamaTexts[[textType]] <- toAdd
      } else {
        jamaTexts[[textType]] <- c(jamaTexts[[textType]], toAdd)
      }
    }
  }
}

# Stemming
jamaTexts <- lapply(jamaTexts, function(currentCorpus) tm_map(currentCorpus, stemDocument))

# To lowercase
jamaTexts <- lapply(jamaTexts, function(currentCorpus) tm_map(currentCorpus, content_transformer(tolower)))

# Remove specific words
titleRemoveList <- c("letter: ", "editorial: ")
for(i in 1:length(jamaTexts$title)) {
  for(wordToRemove in titleRemoveList) {
    jamaTexts$title[[i]]$content <- gsub(wordToRemove, "", jamaTexts$title[[i]]$content, fixed=TRUE)
  }
}

# Remove stopwords
jamaTexts <- lapply(jamaTexts, function(currentCorpus) tm_map(currentCorpus, removeWords, stopwords("english")))

# Strip whitespace
jamaTexts <- lapply(jamaTexts, function(currentCorpus) tm_map(currentCorpus, stripWhitespace))

# Remove punctuation
jamaTexts <- lapply(jamaTexts, function(currentCorpus) tm_map(currentCorpus, removePunctuation))

# Term-document matrix

myMat <- DocumentTermMatrix(jamaTexts$title)
matrix.colsums <- colSums(inspect(myMat))
matrix.rowsums <- rowSums(inspect(myMat))
freq.count.table <- data.frame(word=colnames(myMat), count=matrix.colsums)

# To avoid divide-by-zero errors, replace zero sums with 1.
matrix.rowsums[matrix.rowsums == 0] <- 1
myMat.data <- inspect(myMat)

#myMat.freq.per.1000 <- t(t(myMat.data) * 1000 / matrix.rowsums) # This seems to cause floating point errors!
#myMat.freq.per.1000 <- sweep(myMat.data * 1000, 1, matrix.rowsums, "/")
myMat.freq.per.1000 <- myMat.data / matrix.rowsums

# only include terms that have occurred at least 1/1000 (0.01%) of the time
myTDMHiFreq <- myMat.freq.per.1000[,freq.count.table$count > 0.0001 * sum(freq.count.table$count)]

# Go by year
my.new.tdm <- matrix(nrow=40, ncol=ncol(myTDMHiFreq))
for(year in 1976:2015) {
  current.row <- colSums(myTDMHiFreq[startsWith(rownames(myTDMHiFreq), as.character(year)),])
  my.new.tdm[year - 1975,] <- current.row
}
rownames(my.new.tdm) <- paste0("Year-", 1976:2015)
colnames(my.new.tdm) <- colnames(myTDMHiFreq)

years <- 1976:2015
year.coefficients <- apply(my.new.tdm, 2, function(y) coef(lm(y ~ years))["years"])
year.pvals <- apply(my.new.tdm, 2, function(y) summary(lm(y ~ years))$coefficients[2,4])

fdr.adj.year.pvals <- p.adjust(year.pvals, method="fdr")

sig.year.coefficients <- year.coefficients[fdr.adj.year.pvals < 0.01]

barplot(my.new.tdm[,"patient"])

# find all words after "among"
out <- sapply(jamaTexts$abstract, function(x) unlist(str_extract_all(x$content, '(?<=among\\s)\\w+')))
# Can do some analyses on just these words, can do fancy things like take up to the first noun after "among"