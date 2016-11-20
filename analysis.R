library(tm)
library(SnowballC)

titleTexts <- c()
abstractTexts <- c()
keywordTexts <- c()

# Load data

for(year in 1976:2015) {
  yearIndex <- year - 1975
  #titleTexts[[yearIndex]] <- list()
  #abstractTexts[[yearIndex]] <- list()
  #keywordTexts[[yearIndex]] <- list()
  
  for(month in 1:12) {
    #titleTexts[[yearIndex]][[month]] <- Corpus(DirSource("Pubmed_JAMA/", pattern = paste0(year, "_", month, "_KeywordText\\.txt")))
    toAdd <- Corpus(DirSource("Pubmed_JAMA/", pattern = paste0(year, "_", month, "_TitleText\\.txt")))
    meta(toAdd, "Year", "local") <- year
    meta(toAdd, "Month", "local") <- month
    meta(toAdd, "MonthIndex", "local") <- (year - 1976) * 12 + (month - 1) + 1
    if(length(titleTexts) == 0) {
      titleTexts <- toAdd
    } else {
      titleTexts <- c(titleTexts, toAdd)
    }
  }
}

# Stemming
titleTexts <- tm_map(titleTexts, stemDocument)

# To lowercase
titleTexts <- tm_map(titleTexts, content_transformer(tolower))

# Remove specific words
titleRemoveList <- c("letter: ", "editorial: ")
for(i in 1:length(titleTexts)) {
  for(wordToRemove in titleRemoveList) {
    titleTexts[[i]]$content <- gsub(wordToRemove, "", titleTexts[[i]]$content, fixed=TRUE)
  }
}

# Remove stopwords
titleTexts <- tm_map(titleTexts, removeWords, stopwords("english"))

# Strip whitespace
titleTexts <- tm_map(titleTexts, stripWhitespace)

# Remove punctuation
titleTexts <- tm_map(titleTexts, removePunctuation)

# Term-document matrix
myMat <- DocumentTermMatrix(titleTexts)
matrix.colsums <- colSums(inspect(myMat))
matrix.rowsums <- rowSums(inspect(myMat))
freq.table <- data.frame(word=colnames(myMat)[rev(order(matrix.colsums))], count=rev(sort(matrix.colsums)))

myMat.freq.per.1000 <- t(t(inspect(myMat)) * 1000 / matrix.rowsums)

# only include terms that have occurred at least 1% of the time
myTDMHiFreq <- myMat.freq.per.1000[,matrix.colsums >= 100]

# Go by year
my.new.tdm <- matrix(nrow=40, ncol=ncol(myTDMHiFreq))
for(year in 1976:2015) {
  current.row <- colSums(myTDMHiFreq[startsWith(rownames(myTDMHiFreq), as.character(year)),])
  my.new.tdm[year - 1975,] <- current.row
}
rownames(my.new.tdm) <- paste0("Year-", 1976:2015)
colnames(my.new.tdm) <- colnames(myTDMHiFreq)

year.coefficients <- apply(my.new.tdm, 2, function(y) coef(lm(y ~ years))["years"])
year.pvals <- apply(my.new.tdm, 2, function(y) summary(lm(y ~ years))$coefficients[2,4])
p.adjust(year.pvals, method="fdr")

barplot(my.new.tdm[,"patient"])

