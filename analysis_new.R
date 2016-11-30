library(tm)
library(SnowballC)
library(readr)
library(XML)

texts <- list(title=character(), abstract=character(), keywords=list(), year=numeric(), month=numeric(), publication.types=list())

# Load data from xml
for(year in 1976:2015) {
  yearIndex <- year - 1975
  for(month in 1:12) {
    for(textType in 1:3) {
      print(paste("Parsing year", year, "and month", month))
      current.xml.text <- read_file(paste0("Pubmed_JAMA/xml/", year, "_", month, ".xml"))
      
      current.xml.text.split <- str_split(my.text,  "(?=\\<\\?xml version\\=)")[[1]]
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
      current.abstracts <- unname(sapply(xml.text.list, get_from_list, c("MedlineCitation", "Article", "Abstract", "AbstractText")))
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
      
      texts <- lapply(names(vals.to.add),function(x) c(texts[[x]],vals.to.add[[x]]))
      names(texts)<-names(vals.to.add)
      #meta(toAdd, "Year", "local") <- 
      #meta(toAdd, "Month", "local") <- 
      #meta(toAdd, "MonthIndex", "local") <- (year - 1976) * 12 + (month - 1) + 1
      
      #if(is.null(jamaTexts[[textType]])) {
      #  jamaTexts[[textType]] <- toAdd
      #} else {
      #  jamaTexts[[textType]] <- c(jamaTexts[[textType]], toAdd)
      #}
    }
  }
}