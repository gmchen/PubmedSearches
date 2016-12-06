library(tm)
library(SnowballC)
library(readr)
library(stringr)
library(XML)
library(RTextTools)
library(reshape2)
library(wordcloud)
library(scales)

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
  mean(sapply(jamaTexts$publication.types[jamaTexts$year == year], function(pubtypes) any(clinical.trial.pubtypes %in% pubtypes)))
  })
plot(1976:2015, year.counts.with.clinical.trial)

year.counts.with.case.report <- sapply(1976:2015, function(year) {
  mean(sapply(jamaTexts$publication.types[jamaTexts$year == year], function(pubtypes) "Case Reports" %in% pubtypes))
  })
plot(1976:2015, year.counts.with.case.report)

pubtype.fig.df <- data.frame(clinical.trial=year.counts.with.clinical.trial, case.report=year.counts.with.case.report, year=1976:2015)
pubtype.fig.df.m <- melt(pubtype.fig.df, id="year")
colnames(pubtype.fig.df.m) <- c("year", "PublicationType", "Frequency")

pdf("figures/jama_pubtypes.pdf", width=6, height=4.5)
ggplot(pubtype.fig.df.m, aes(x = year, y = Frequency, colour=factor(PublicationType))) +  
  geom_point(stat="identity") + 
  scale_colour_manual(values=c("#045a8d", "#bd0026"), name="Publication Type", labels=c("Clinical Trial", "Case Report")) +
  geom_smooth(aes(colour=factor(PublicationType)),method = loess, method.args = list(family = "symmetric"), show.legend = FALSE) + 
  ggtitle("Clinical Trials vs Case Reports in JAMA") + # plot title
  xlab("Year") + 
  ylab("Frequency") +
  scale_y_continuous(labels = percent_format()) +
  theme_bw() +
  theme(panel.grid.major = element_blank())
dev.off()

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
              case.reports=is.case.report,
              non.case.reports=!is.case.report)
  
  corpusTexts[[journalName]]  <- lapply(c("title", "abstract"), function(titleOrAbstract){
      sublistToReturn <- lapply(names(textsToInclude), function(textTypeToInclude) {
        includeVector <- textsToInclude[[textTypeToInclude]]
        corpusToReturn <- NULL
        for(year in 1976:2015) {
          if(titleOrAbstract == "title") {
            current.corpus.texts <- texts$title[texts$year == year & includeVector]
            # remove duplicates
            current.corpus.texts <- unique(current.corpus.texts)
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


frequencyAnalysis <- lapply(corpusTexts, function(corpusTextByJournal) {
  lapply(corpusTextByJournal, function(corpusTextByTextType) {
      lapply(corpusTextByTextType, function(corpusTextByPubType) {
        out.vals <- lapply(c("monogram", "bigram"), function(gramtype) {
          if(gramtype == "monogram") {
            myMat <- DocumentTermMatrix(corpusTextByPubType)
          } else if(gramtype == "bigram") {
            BigramTokenizer <- function(x) 
              unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
            myMat <- TermDocumentMatrix(corpusTextByPubType, control = list(tokenize = BigramTokenizer))
            myMat <- t(myMat)
          } 
          
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
          
          return(list(TDMHiFreq.per.1000=myTDMHiFreq.per.1000, sig.year.coefficients=sort(sig.year.coefficients)))
        })
        names(out.vals) <- c("monogram", "bigram")
        return(out.vals)
      })
    })
  })



#########################################################

# First, get a list of significant hits in titles. Remove stopwords
title.texts.all.significant.hits <- frequencyAnalysis$jama$title$all$monogram$sig.year.coefficients
title.texts.all.significant.hits <- title.texts.all.significant.hits[!(names(title.texts.all.significant.hits) %in% stopwords())]

# Remove the words from journal-specific headings
# TODO: remove these earlier on
# A piece of my mind
# JAMA patient page
# The cover.
# Council on Scientific Affairs
# From the NIH:
title.texts.words.to.exclude <- c("piece", "mind", "jama", "patient", "page", "cover", "council")
title.texts.all.significant.hits <- title.texts.all.significant.hits[!(names(title.texts.all.significant.hits) %in% title.texts.words.to.exclude)]

# Top 20 positive hits
writeLines(paste(rev(tail(names(title.texts.all.significant.hits), 20)), collapse="\n"))
# Top 20 negative hits
writeLines(paste(head(names(title.texts.all.significant.hits), 20), collapse="\n"))

# Wordcloud for positive hits
cool.colours <- c("#006d2c", "#0868ac", "#045a8d", "#016c59", "#253494")
warm.colours <- c("#b30000", "#980043", "#7a0177", "#993404", "#bd0026")
set.seed(1000)
pdf("figures/jama_title_monogram_wordcloud_positive.pdf", width = 8, height = 8)
wordcloud(
          names(title.texts.all.significant.hits)[title.texts.all.significant.hits>0],
          freq=title.texts.all.significant.hits[title.texts.all.significant.hits>0], 
          min.freq = -Inf, 
          random.color = TRUE, 
          rot.per = 0, 
          colors=cool.colours)
dev.off()

pdf("figures/jama_title_monogram_wordcloud_negative.pdf", width = 8, height = 8)
wordcloud(
  names(title.texts.all.significant.hits)[title.texts.all.significant.hits<0],
  freq=-title.texts.all.significant.hits[title.texts.all.significant.hits<0], 
  min.freq = -Inf, 
  random.color = TRUE, 
  rot.per = 0, 
  colors=warm.colours)
dev.off()

title.texts.bigram.all.significant.hits <- frequencyAnalysis$jama$title$all$bigram$sig.year.coefficients
title.texts.bigram.words.to.exclude <- c("jama patient", "patient page", "of my", "piece of", "my mind", "mind the", "a piece", "the cover", "council on", "on scientific", "scientific affairs")
title.texts.bigram.all.significant.hits <- title.texts.bigram.all.significant.hits[!(names(title.texts.bigram.all.significant.hits) %in% title.texts.bigram.words.to.exclude)]
pdf("figures/jama_title_bigram_wordcloud_positive.pdf", width = 8, height = 8)
wordcloud(
          names(title.texts.bigram.all.significant.hits)[title.texts.bigram.all.significant.hits>0],
          freq=title.texts.bigram.all.significant.hits[title.texts.bigram.all.significant.hits>0], 
          min.freq = -Inf, 
          random.color = TRUE, 
          rot.per = 0, 
          colors=cool.colours)
dev.off()

pdf("figures/jama_title_bigram_wordcloud_negative.pdf", width = 8, height = 8)
wordcloud(
  names(title.texts.bigram.all.significant.hits)[title.texts.bigram.all.significant.hits<0],
  freq=-title.texts.bigram.all.significant.hits[title.texts.bigram.all.significant.hits<0], 
  min.freq = -Inf, 
  random.color = TRUE, 
  rot.per = 0, 
  colors=warm.colours)
dev.off()
# Top 20 positive hits
writeLines(paste(rev(tail(names(title.texts.bigram.all.significant.hits), 20)), collapse="\n"))
# Top 20 negative hits
writeLines(paste(head(names(title.texts.bigram.all.significant.hits), 20), collapse="\n"))

barplot(frequencyAnalysis$jama$title$all$monogram$TDMHiFreq.per.1000[,"diabetes"])

current.freq.per.1000 <- frequencyAnalysis$jama$title$all$monogram$TDMHiFreq.per.1000[,"adults"]

get.scatterplot.with.loess <- function(current.freq.per.1000, termInTitle) {
  barplot.df.to.plot <- data.frame(
                                freq.per.1000 = current.freq.per.1000[,termInTitle],
                                year = 1976:2015
                                )
  
  ggplot(barplot.df.to.plot, aes(x = year, y = freq.per.1000)) +  
    geom_point(stat="identity", fill="#0571b0") + 
    geom_smooth(method = loess, method.args = list(family = "symmetric"), fill="#ca0020", colour="#ca0020") +
  #  geom_errorbar(aes(ymin=lower, ymax=upper)) +
    ggtitle(paste0('"', termInTitle, '"')) + # plot title
    xlab("Year") + 
    ylab("Frequency per 1000 words") +
    theme_bw() +
    theme(panel.grid.major = element_blank())
}
get.barplot <- function(tdm.freq.per.1000, termInTitle, direction=c("positive", "negative")) {
  direction=match.arg(direction)
  barplot.df.to.plot <- data.frame(
                                freq.per.1000 = tdm.freq.per.1000[,termInTitle],
                                year = 1976:2015
                                )
  if(direction=="positive") {
    plot.colour = "#0571b0"
  } else {
    plot.colour = "#b30000"
  }
  
  ggplot(barplot.df.to.plot, aes(x = year, y = freq.per.1000)) +  
    geom_bar(stat="identity", fill=plot.colour) +
  #  geom_errorbar(aes(ymin=lower, ymax=upper)) +
    ggtitle(paste0('"', termInTitle, '"')) + # plot title
    xlab("Year") + 
    ylab("Frequency per 1000 words") +
    theme_bw() +
    theme(panel.grid.major = element_blank())
}

# Epidemiological terms
pdf("figures/jama_title_barplot_randomized.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$monogram$TDMHiFreq.per.1000, "randomized")
dev.off()

pdf("figures/jama_title_barplot_trial.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$monogram$TDMHiFreq.per.1000, "trial")
dev.off()

pdf("figures/jama_title_barplot_risk.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$monogram$TDMHiFreq.per.1000, "risk")
dev.off()

pdf("figures/jama_title_barplot_outcomes.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$monogram$TDMHiFreq.per.1000, "outcomes")
dev.off()

pdf("figures/jama_title_barplot_patient_with.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "patient with", direction="negative")
dev.off()

pdf("figures/jama_title_barplot_patients_with.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "patients with")
dev.off()


pdf("figures/jama_title_barplot_a_patient.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "a patient", direction = "negative")
dev.off()

pdf("figures/jama_title_barplot_in_patients.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "in patients")
dev.off()

pdf("figures/jama_title_barplot_the_elderly.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "the elderly", direction="negative")
dev.off()

pdf("figures/jama_title_barplot_older_patients.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "older patients")
dev.off()

pdf("figures/jama_title_barplot_older_adults.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "older adults")
dev.off()

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

# do this barplot in chunks of five years
barplot(tapply(myTDMHiFreq.per.1000[,"from"], (seq_along(myTDMHiFreq.per.1000[,"from"])-1) %/% 5, sum))

# find all words after "among"
out <- sapply(title.texts, function(x) unlist(str_extract_all(x$content, '(?<=among\\s)\\w+')))
# Can do some analyses on just these words, can do fancy things like take up to the first noun after "among"

