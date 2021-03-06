library(tm)
library(SnowballC)
library(readr)
library(stringr)
library(XML)
library(RTextTools)
library(reshape2)
library(wordcloud)
library(scales)
library(openNLP)

jamaTexts <- list(title=character(), abstract=character(), keywords=list(), year=numeric(), month=numeric(), publication.types=list())
annalsTexts <- list(title=character(), abstract=character(), keywords=list(), year=numeric(), month=numeric(), publication.types=list())
nejmTexts <- list(title=character(), abstract=character(), keywords=list(), year=numeric(), month=numeric(), publication.types=list())
lancetTexts <- list(title=character(), abstract=character(), keywords=list(), year=numeric(), month=numeric(), publication.types=list())
bmjTexts <- list(title=character(), abstract=character(), keywords=list(), year=numeric(), month=numeric(), publication.types=list())

# Load data from xml
for(year in 1976:2015) {
  yearIndex <- year - 1975
  for(month in 1:12) {
    #for(journalName in c("jama", "annals", "nejm", "lancet", "bmj")) {
    for(journalName in c("bmj")) {
      print(paste("Parsing year", year, "and month", month))
      if(journalName == "jama") {
        current.xml.text <- read_file(paste0("Pubmed_JAMA/xml/", year, "_", month, ".xml"))
      } else if(journalName == "annals") {
        current.xml.text <- read_file(paste0("Pubmed_Annals_Intern_Med/xml/", year, "_", month, ".xml"))
      } else if(journalName == "nejm") {
        current.xml.text <- read_file(paste0("Pubmed_NEJM/xml/", year, "_", month, ".xml"))
      } else if(journalName == "lancet") {
        current.xml.text <- read_file(paste0("Pubmed_LANCET/xml/", year, "_", month, ".xml"))
      } else if(journalName == "bmj") {
        current.xml.text <- read_file(paste0("Pubmed_BMJ//xml/", year, "_", month, ".xml"))
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
      } else if(journalName == "annals") {
        annalsTexts <- lapply(names(vals.to.add),function(x) c(annalsTexts[[x]],vals.to.add[[x]]))
        names(annalsTexts)<-names(vals.to.add)
      } else if(journalName == "nejm") {
        nejmTexts <- lapply(names(vals.to.add),function(x) c(nejmTexts[[x]],vals.to.add[[x]]))
        names(nejmTexts)<-names(vals.to.add)
      } else if(journalName == "lancet") {
        lancetTexts <- lapply(names(vals.to.add),function(x) c(lancetTexts[[x]],vals.to.add[[x]]))
        names(lancetTexts)<-names(vals.to.add)
      } else if(journalName == "bmj") {
        bmjTexts <- lapply(names(vals.to.add),function(x) c(bmjTexts[[x]],vals.to.add[[x]]))
        names(bmjTexts)<-names(vals.to.add)
      }
    }
  }
}

if(length(unlist(lancetTexts$title)) != length(lancetTexts$title)) {
  stop("Found Lancet title that is not a list of length 1")  
}
if(length(unlist(bmjTexts$title)) != length(bmjTexts$title)) {
  stop("Found BMJ title that is not a list of length 1")  
}

lancetTexts$title <- unlist(lancetTexts$title)
bmjTexts$title <- unlist(bmjTexts$title)

save(jamaTexts, file="jamaTexts.RData")
save(nejmTexts, file="nejmTexts.RData")
save(annalsTexts, file="annalsTexts.RData")
save(lancetTexts, file="lancetTexts.RData")
save(bmjTexts, file="bmjTexts.RData")

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

clinical.trial.exclusion.pubtypes <- c("Editorial",
                                       "News",
                                       "Historical Article",
                                       "Case Reports",
                                       "Review",
                                       "Congresses",
                                       "Guideline",
                                       "Practice Guideline")

## Save files for manual inspection
for(journalName in c("jama", "nejm", "lancet")) {
  
  if(journalName == "jama") {
    texts <- jamaTexts
  } else if (journalName == "nejm") {
    texts <- nejmTexts
  } else if (journalName == "lancet") {
    texts <- lancetTexts
  }
  # clinical trials texts_for_review
  for(year in 1976:2015) {
      write.table(
        data.frame("", unique(texts$title[texts$year==year])),
        file = paste0("texts_for_review/", journalName, "/", journalName, "_all_titles_", year, ".csv"), 
        col.names = FALSE,
        row.names = FALSE,
        sep=",")
  }
  
  # 5 year intervals
  for(yearstart in c(1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011)) {
    is.clinical.trial <- sapply(texts$publication.types, function(pubtypes) any(clinical.trial.pubtypes %in% pubtypes) & !any(clinical.trial.exclusion.pubtypes %in% pubtypes)  )
    is.case.report <- sapply(texts$publication.types, function(pubtypes) "Case Reports" %in% pubtypes)
    write.table(
      data.frame("", unique(texts$title[texts$year >= yearstart & texts$year <= yearstart+4 & is.clinical.trial])),
      file = paste0("texts_for_review/", journalName, "/", journalName, "_clinical_trial_titles_", yearstart, "-", yearstart+4, ".csv"),
      col.names=FALSE,
      row.names=FALSE,
      sep=",")
    write.table(
      data.frame("", unique(texts$title[texts$year >= yearstart & texts$year <= yearstart+4 & is.case.report])),
      file = paste0("texts_for_review/", journalName, "/", journalName, "_case_report_titles_", yearstart, "-", yearstart+4, ".csv"),
      col.names=FALSE,
      row.names=FALSE,
      sep=",")
    
  }
}

# Publication type plots
for(journalName in c("jama", "annals", "nejm")) {
  if(journalName == "jama") {
    journalNameTitle <- "JAMA"
    texts <- jamaTexts
  } else if (journalName == "annals") {
    journalNameTitle <- "Annals of Internal Medicine"
    texts <- annalsTexts
  } else if (journalName == "nejm") {
    journalNameTitle <- "the New England Journal of Medicine"
    texts <- nejmTexts
  }
  year.counts.with.clinical.trial <- sapply(1976:2015, function(year) {
    mean(sapply(texts$publication.types[texts$year == year], function(pubtypes) any(clinical.trial.pubtypes %in% pubtypes)))
    })
  plot(1976:2015, year.counts.with.clinical.trial)
  
  year.counts.with.case.report <- sapply(1976:2015, function(year) {
    mean(sapply(texts$publication.types[texts$year == year], function(pubtypes) "Case Reports" %in% pubtypes))
    })
  plot(1976:2015, year.counts.with.case.report)
  
  pubtype.fig.df <- data.frame(clinical.trial=year.counts.with.clinical.trial, case.report=year.counts.with.case.report, year=1976:2015)
  pubtype.fig.df.m <- melt(pubtype.fig.df, id="year")
  colnames(pubtype.fig.df.m) <- c("year", "PublicationType", "Frequency")
  
  
  publication.type.plot <- ggplot(pubtype.fig.df.m, aes(x = year, y = Frequency, colour=factor(PublicationType))) +  
    geom_point(stat="identity") + 
    scale_colour_manual(values=c("#045a8d", "#bd0026"), name="Publication Type", labels=c("Clinical Trial", "Case Report")) +
    geom_smooth(aes(colour=factor(PublicationType)),method = loess, method.args = list(family = "symmetric"), show.legend = FALSE) + 
    ggtitle(paste0("Clinical Trials vs Case Reports in ", journalNameTitle)) + # plot title
    xlab("Year") + 
    ylab("Frequency") +
    scale_y_continuous(labels = percent_format()) +
    theme_bw() +
    theme(panel.grid.major = element_blank())
  
  ggsave(paste0("figures/", journalName, "_pubtypes.pdf"), publication.type.plot, width=6, height=4.5)
}

# Make file for raters
rater.file.merged.df <- do.call(rbind, lapply(c("jama", "lancet", "nejm", "annals", "bmj"), function(journalName) {
  if(journalName == "jama") {
    texts <- jamaTexts
  } else if (journalName == "lancet") {
    texts <- lancetTexts
  } else if (journalName == "nejm") {
    texts <- nejmTexts
  } else if (journalName == "annals") {
    texts <- annalsTexts
  } else if (journalName == "bmj") {
    texts <- bmjTexts
  }
  is.clinical.trial <- sapply(texts$publication.types, function(pubtypes) any(clinical.trial.pubtypes %in% pubtypes) & !any(clinical.trial.exclusion.pubtypes %in% pubtypes)  )
  year.range <- c(1976:1980, 2011:2015)
  to.include <- texts$year %in% year.range & is.clinical.trial
  
  current.data.frame <- data.frame(
    rater1="",
    rater2="", 
    title=texts$title[to.include],
    year=texts$year[to.include],
    journal=journalName,
    stringsAsFactors = FALSE)
  return(current.data.frame)
  }))

set.seed(1000)
#rater.file.merged.df <- rater.file.merged.df[sample(nrow(rater.file.merged.df)),]

# TODO: add male female donor
rater.file.merged.df$prediction <- sapply(rater.file.merged.df$title, function(title) 
  grepl(paste(c("patient", "adult", "women", " men[^A-Za-z]", "child", "infant"), collapse = "|"),
        title, ignore.case = TRUE))
rater.file.merged.df$rater1[rater.file.merged.df$prediction] <- "p"

write.table(
  rater.file.merged.df,
  file = "journal_titles.csv",
  row.names=FALSE,
  sep=",")

df.for.barplot <- data.frame(proportion=numeric(), timeframe=factor(levels=c("early", "late")), journal=character())

for(journalName in c("jama", "lancet", "nejm", "annals", "bmj")) {
  numPatientCenteredEarly <- mean(rater.file.merged.df$rater1[rater.file.merged.df$year %in% 1976:1980 & rater.file.merged.df$journal == journalName] == "p")
  numPatientCenteredLate <- mean(rater.file.merged.df$rater1[rater.file.merged.df$year %in% 2011:2015 & rater.file.merged.df$journal == journalName] == "p")
  df.to.add <- data.frame(
    proportion=c(numPatientCenteredEarly, numPatientCenteredLate),
    timeframe=c("early", "late"),
    journal=journalName)
  df.for.barplot <- rbind(df.for.barplot, df.to.add)
  
}

ggplot(data=df.for.barplot, aes(x=journal, y=proportion, fill=timeframe)) + geom_bar(stat='identity', position=position_dodge())

corpusTexts <- list(jama=list(), annals=list(), nejm=list())

for(journalName in c("jama", "annals", "nejm")) {
  if(journalName == "jama") {
    texts <- jamaTexts
  } else if (journalName == "annals") {
    texts <- annalsTexts
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
          toAdd <- Corpus(VectorSource(paste(current.corpus.texts, collapse = "\n")))
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

pdf("figures/jama_title_barplot_diabetes.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$monogram$TDMHiFreq.per.1000, "diabetes")
dev.off()

pdf("figures/jama_title_barplot_diabetic.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$monogram$TDMHiFreq.per.1000, "diabetic", direction = "negative")
dev.off()

article.indices.with.diabetic <- grep("[Dd]iabetic", jamaTexts$title)
article.titles.with.diabetic <- jamaTexts$title[article.indices.with.diabetic]
article.year.with.diabetic <- jamaTexts$year[article.indices.with.diabetic]

writeLines(unique(article.titles.with.diabetic[1:20]))

writeLines(unique(article.titles.with.diabetic)[145:164])

article.indices.with.diabetes <- grep("[Dd]iabetes", jamaTexts$title)
article.titles.with.diabetes <- jamaTexts$title[article.indices.with.diabetes]
article.year.with.diabetes <- jamaTexts$year[article.indices.with.diabetes]

writeLines(head(unique(article.titles.with.diabetes), 20))
writeLines(tail(unique(article.titles.with.diabetes), 22))

# caused by, cause of
pdf("figures/jama_title_barplot_caused_by.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "caused by", direction = "negative")
dev.off()

pdf("figures/jama_title_barplot_cause_of.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "cause of", direction = "negative")
dev.off()

pdf("figures/jama_title_barplot_kidney_disease.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "kidney disease")
dev.off()

pdf("figures/jama_title_barplot_renal_failure.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "renal failure", direction = "negative")
dev.off()

pdf("figures/jama_title_barplot_mental_health.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "mental health")
dev.off()

pdf("figures/jama_title_barplot_poststraumatic_stress.pdf", width = 4, height = 4)
get.barplot(frequencyAnalysis$jama$title$all$bigram$TDMHiFreq.per.1000, "posttraumatic stress")
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

