library(tm)
library(SnowballC)
library(readr)
library(XML)

texts <- list(title=list(), abstract=list(), keyword=list())

# Load data from xml
for(year in 1976:2015) {
  yearIndex <- year - 1975
  for(month in 1:12) {
    print(paste("Parsing year", year, "and month", month))
    current.xml.text <- read_file(paste0("Pubmed_JAMA/xml/", year, "_", month, ".xml"))
    
    current.xml.text.split <- str_split(my.text,  "(?=\\<\\?xml version\\=)")
    
    xml.text.lists <- lapply(my.text2, xmlToList)
    xml.text.list <- do.call(c, out)
    # 1 for title, 2 for abstract, 3 for list
    for(textType in 1:3) {
      
      for(docIndex in 1:length(xml.text.list)) {
        if(textType == 1) {
          current.text <- xml.text.list[[docIndex]]$MedlineCitation$Article$ArticleTitle
        } else if(textType == 2) {
          current.text <- xml.text.list[[docIndex]]$MedlineCitation$Article$Abstract$AbstractText
        } else if(textType ==3) {
          current.text <- xml.text.list[[docIndex]]$MedlineCitation$MeshHeading$MeshHeading$DescriptorName$text
        }
        
        current.year <- xml.text.list[[docIndex]]$PubmedData$History$PubMedPubDate$Year
        current.month <- xml.text.list[[docIndex]]$PubmedData$History$PubMedPubDate$Month
        current.publication.types <- xml.text.list[[docIndex]]$MedlineCitation$Article$PublicationTypeList$PublicationType$text
        
        texts[[textType]] <- c(texts[[textType]], list(text=current.titles, year=current.year, month=current.month, publication.types=current.publication.types))
      }
    }
  }
}