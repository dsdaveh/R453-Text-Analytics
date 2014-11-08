library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(magrittr)
#library(Rgraphviz) # Correlation plots  #no package available for 3.1.1 from cran

setwd("C:/Users/Dave/Google Drive/Predictive Analytics/453 - Text Analytics/R453 Text Analytics")
corpus_dir <- "EarningsCalls"
cname <- file.path(".", corpus_dir)
nfiles <- length(dir(cname))

docs <- Corpus(DirSource(cname))
class(docs)
class(docs[[1]])
#inspect(docs[1])  #commented out since this output is large

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

# doc style specific transfomration
docs <- tm_map(docs, toSpace, "http:\\S*") #remove URLS
docs <- tm_map(docs, toSpace, "<.*/.*>") #remove page numbers
docs <- tm_map(docs, toSpace, "\\n") #remove newline chars
#TO-DO: remove unprintable characters

getTransformations() # this is the list of things we can do to the corpus

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, content_transformer(removePunctuation))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("question")) #equivalent to SPSS concept delete
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeNumbers)  #may want to reconsider doing this

#docs <- tm_map(docs, stemDocument)  #step words -- not sure we want to do this
#TO-DO: compare the differences of stemming and non stemming

dtm <- DocumentTermMatrix(docs)
dtm
nterms <- dtm$ncol
inspect(dtm[1:5, c(12:16, 1000:1005, seq(nterms-4,nterms))])  #first 11 have unprintable chars

freq <- colSums(as.matrix(dtm))
ord <- order(freq)
freq.most <- freq[tail(ord)]
print ("The most frequent terms found are:")
freq.most
freq.most

terms <- names(freq)
print ("Find terms with string 'product'")
freq[grep("product",terms)]

barplot(freq[grep("^cloud$|^hybrid|^product$|products",terms)])  #word boundaries need work
#
#TO-DOplay around with synonyms
#synonyms "line product,product,product line,product lines,product offering,product offerings"