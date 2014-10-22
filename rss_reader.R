#http://www.r-bloggers.com/how-to-build-a-dataset-in-r-using-an-rss-feed-or-web-page/
  
#install.packages("XML") 
#install.packages("tm") 
#install.packages("SnowballC")
library(XML)
library (tm)
library(SnowballC)

prepCorpus <- function (text) {
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, tolower)
  #corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, PlainTextDocument)
  return(corpus)
}

doc<-xmlTreeParse("http://www.huffingtonpost.com/feeds/verticals/parents/index.xml")
src <- xpathApply(xmlRoot(doc), "//item")

for (i in 1:length(src)) {
  foo<-xmlSApply(src[[i]], xmlValue)
  tmp<-data.frame(t(foo), stringsAsFactors=FALSE)
  
  if (i==1) {  DATA<-tmp
  }   else {   DATA<-rbind(DATA, tmp)
  }  
}

rss.text <- prepCorpus(DATA$description)
dtm <- DocumentTermMatrix(rss.text)
concepts <- findFreqTerms(dtm)
concepts2 <- findFreqTerms(dtm, lowfreq=2)
concepts3 <- findFreqTerms(dtm, lowfreq=3)
concepts4 <- findFreqTerms(dtm, lowfreq=4)
concepts9 <- findFreqTerms(dtm, lowfreq=9)
