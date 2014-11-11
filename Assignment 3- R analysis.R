library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(magrittr)
#library(Rgraphviz) # Correlation plots  #no package available for 3.1.1 from cran

setwd("C:/Users/Dave/Google Drive/Predictive Analytics/453 - Text Analytics/R453 Text Analytics")
corpus_dir <- "EarningsCalls"
#corpus_dir <- "EarningsCalls - dbg"
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
docs <- tm_map(docs, content_transformer(function(x) gsub("[^[:print:]]", "", x))) # remove unprintable characters

getTransformations() # this is the list of things we can do to the corpus

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, content_transformer(removePunctuation))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("question")) #equivalent to SPSS concept delete
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeNumbers)  #may want to reconsider doing this

#docs <- tm_map(docs, stemDocument)  #step words -- not sure we want to do this
#TO-DO: compare the differences of stemming and non stemming

mostFrequentTerms <- function (x.dtm, n=6) {
  freq <- colSums(as.matrix(x.dtm))
  ord <- order(freq)
  freq[tail(ord, n)]
} 

findTerms <- function (x.dtm, pattern) {
  freq <- colSums(as.matrix(x.dtm))
  terms <- names(freq)
  freq[grep(pattern, terms)]
}

dtm <- DocumentTermMatrix(docs)
dtm
nterms <- dtm$ncol
inspect(dtm[1:5, c(1:5, seq(nterms-4,nterms))])  

mostFrequentTerms( dtm )
findTerms( dtm, "product")

barplot(freq[grep("^cloud$|^hybrid|^product$|products",terms)])  #word boundaries need work
#
#TO-DOplay around with synonyms
#synonyms "line product,product,product line,product lines,product offering,product offerings"

# THIS DIDNT WORK KEEPING IT HERE BECAUSE THERE WERE SOME THINGS WORTH STEALING
# #create a synonym file
# stems <- tm_map(docs, stemDocument)  
# stems.dtm <- DocumentTermMatrix(stems)
# stems.dict <- colnames(as.matrix(stems.dtm))  #a list of all the stemmed words
# dtm.dict <- colnames(as.matrix(dtm))          #a list of all the terms
# synonym.df <- data.frame( root=as.character(stems.dict), stringsAsFactors=FALSE)  #list of stems
# synonym.df$wordList <- apply(synonym.df, 1, function(x) dtm.dict[grep(x, dtm.dict)]) #find words that match the stem
# synonym.df$n <- apply(synonym.df, 1, function(x) length(unlist(x$wordList)))  #count the number of synonyms for each stem
# synonym.df$csv <- apply(synonym.df, 1, function(x) paste(unlist(x$wordList), collapse=",")) #create a csv string
# 
# write.table( synonym.df[synonym.df$n > 1, ]$csv, file="synonym_seed.csv", 
#              quote=FALSE, row.names=FALSE, col.names=c("primary term"), sep=",")

dtm.dict <- colnames(as.matrix(dtm))          #a list of all the terms
synonym.df <- data.frame( root=as.character(dtm.dict), stringsAsFactors=FALSE) #a processing frame

findWordForms <- function(v) {
   # v is a character vector
   # v[1] is the root used for the search
   # look in v for plural or past tense forms and return them as a vector (with root as first term)
   # -- use this with a sorted list and pass the next 10 (or so) words as part of the vector
   pattern <- sprintf("%se?d$|%se?s$", v[1], v[1])
   strings <- v[2:length(v)]
   c(v[1], strings[grep( pattern, strings)]) 
}

words <- list()
for(i in 1:(nrow(synonym.df))) {
  words[i] <- list( findWordForms( dtm.dict[i:(i+10)]) )
}
synonym.df$wordList <- words
synonym.df$n <- apply(synonym.df, 1, function(x) length(unlist(x$wordList)))  #count the number of synonyms for each root
synonym.df <- subset(synonym.df, n>1)  #reduce to words with synonyms
synonym.df$csv <- apply(synonym.df, 1, function(x) paste(unlist(x$wordList), collapse=",")) #create a csv string

write.table( synonym.df[synonym.df$n > 1, ]$csv, file="synonym_seed.csv", 
             quote=FALSE, row.names=FALSE, col.names=c("primary term"), sep=",")
rm(synonym.df)

#copy the synonym_seed.csv to synonym.csv and edit it as appropriate
#NOTE: You have to save the .csv file in Excel in order for this .csv to work properly
synonyms <- read.csv("synonym.csv", stringsAsFactors=FALSE)

replaceSynonyms <- function(x, syn) {
  #replace synonyms with primary term
  syn
  for (i in 1:nrow(syn)) {
    #if(i%%100 == 0) print(sprintf("%d...", i))
    root <- syn[i,1]
    j <- 2
    while (!is.null(syn[i,j]) && grepl("[a-z]", syn[i,j]) ) {   #first entry that has no chars (most reliable test)
      pattern <- sprintf("%s", syn[i,j])
      x <- tm_map(x, content_transformer(function(y) gsub(pattern, root, y))) 
      j <- j+1
    }
  }
  x
}

dtm.orig <- dtm   # dtm <- dtm.orig to restore
docs.orig <- dtm  # docs <- docs.orig to restore
docs <- replaceSynonyms(docs, synonyms)
dtm <- DocumentTermMatrix(docs)
dtm
nterms <- dtm$ncol

print ("The most frequent terms found are:")
mostFrequentTerms( dtm )   #"think" is interesting here:  synonomous with predict?

psyn <- findTerms(dtm, "predict|forecast|think")
psyn
psyn.words <- names(psyn)[- grep("predictable", names(psyn))]  #remove "predictable"
docs <- replaceSynonyms(docs, data.frame(matrix( psyn.words, nrow=1 )))
dtm <- DocumentTermMatrix(docs)
mostFrequentTerms( dtm )   #"think" is interesting here:  synonomous with predict?

print ("Find terms with string 'product'")
findTerms(dtm, "product")

barplot(findTerms(dtm, "^cloud$|^hybrid|^product$"))  #word boundaries need work
barplot(mostFrequentTerms( dtm, 10 ))
