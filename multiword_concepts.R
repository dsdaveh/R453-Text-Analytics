library(tm)
library(stringr)
library(RWeka)


mostFrequentTerms <- function (x.tdm, n=6) {
  freq <- rowSums(as.matrix(x.tdm))
  ord <- order(freq)
  freq[tail(ord, n)]
} 

findTerms <- function (x.tdm, pattern) {
  freq <- rowSums(as.matrix(x.tdm))
  terms <- names(freq)
  freq[grep(pattern, terms)]
}

replaceSynonyms <- function(x, syn) {
  #replace synonyms with primary term
  syn
  for (i in 1:nrow(syn)) {
    #if(i%%100 == 0) print(sprintf("%d...", i))
    root <- sprintf(" %s ", syn[i,1])
    j <- 2
    while (!is.null(syn[i,j]) && grepl("[a-z]", syn[i,j]) ) {   #first entry that has no chars (most reliable test)
      pattern <- sprintf("\\s%s\\s", syn[i,j])
      x <- tm_map(x, content_transformer(function(y) gsub(pattern, root, y))) 
      j <- j+1
    }
  }
  x
}

findCoverageX <- function( tdm, pct ) {
  freq <- rowSums(as.matrix(tdm))
  x0 <- 0
  xi <- as.integer(.2 * length(freq))  # initialize 20% steps
  c <- sum(mostFrequentTerms(tdm, x0)) / sum(freq)
  pos <- TRUE
  while  (c < pct-.01 || c > pct+.01 ) {
    if (c < pct ) { x0 <- x0+xi ; pos <- TRUE
    } else        { 
      if (pos == TRUE) { x0 <- x0 - xi}
      xi <- as.integer(xi/2) 
      pos <- FALSE
    } 
    c <- sum(mostFrequentTerms(tdm, x0+xi)) / sum(freq)
  }
  x0+xi
}

toConcept <- function( x, pattern, concept) {
  tm_map(x, content_transformer(function(x, p, str) gsub(p, str, x)), pattern, concept)
}


plotCoverage <- function ( tdm, n ) {
  #create a Fibonacci series for xvalues
  title.cp <- sprintf("Coverage Plot for %d-grams", n)
  
  nterms <- tdm$nrow
  freq <- rowSums(as.matrix(tdm))
  
  xFib <- numeric()
  xFib[1] <- xFib[2] <- 1
  while (max(xFib) < nterms) { xFib <- c(xFib, sum(tail(xFib,2)))}
  xFib <- xFib[c(-1,(-length(xFib)))]; #first 2 values are identical, last is out of bounds
  
  ydata <- numeric()
  for (i in seq_along(xFib))  { 
    ydata[i] <- sum(mostFrequentTerms(tdm,xFib[i]))/sum(freq)
  }
  plot( c(xFib, length(freq)), c(ydata,1), type="l", yaxt = "n"
        , xlab="Term Rank (1 is most frequent)"
        , ylab="Occurance in corpus", main=title.cp)
  axis(2, at=pretty(seq(0,1,.2)), lab=paste0(pretty(seq(0,1,.2)) * 100, " %"), las=FALSE)
  abline(h=.5, col="red")
  abline(h=.9, col="red")
  
  #trial and error (and looking at the graph)
  cover.50 <- findCoverageX (tdm, .5)
  cover.90 <- findCoverageX (tdm, .9)
  
  abline(v=cover.50, col="red", lty=3)
  abline(v=cover.90, col="red", lty=3)
  text( x= cover.50, y=0.1, pos=4, sprintf("%d terms", cover.50))
  text( x= cover.90, y=0.2, pos=4, sprintf("%d terms", cover.90))
}

makeNGram <- function( corpus, n ) {
  nTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  x.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = nTokenizer))
  x.tdm
}

corpus_dir <- "EarningsCalls"
cname <- file.path(".", corpus_dir)

dir(corpus_dir)
docs <- Corpus(DirSource(cname))

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

# doc style specific transfomration
docs <- tm_map(docs, toSpace, "http:\\S*") #remove URLS
docs <- tm_map(docs, toSpace, "<.*/.*>") #remove page numbers
docs <- tm_map(docs, toSpace, "\\n") #remove newline chars

# common transformation
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, content_transformer(removePunctuation))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("question")) #equivalent to SPSS concept delete
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeNumbers)  #may want to reconsider doing this

#### Note tdm rather than dtm

#1-Grams
#how many
tdm.1 <- TermDocumentMatrix(docs)
#note how many



tdm.2 <- makeNGram( docs, 2)
tdm.3 <- makeNGram( docs, 3)
tdm.4 <- makeNGram( docs, 4)
tdm.5 <- makeNGram( docs, 5)
tdm.6 <- makeNGram( docs, 6)

makeNGram

tcnt <- c( tdm.1$nrow, tdm.2$nrow, tdm.3$nrow, tdm.4$nrow, tdm.5$nrow , tdm.6$nrow)
plot (tcnt, type="b", ylab = "# of Terms", xlab="N", main="N-gram size")

par(mfrow=c(2,2))
plotCoverage (tdm.1, 1)
plotCoverage (tdm.2, 2)
plotCoverage (tdm.3, 3)
plotCoverage (tdm.4, 4)
par(mfrow=c(1,1))

mar.orig <- par()$mar
par(las=2, mar=c(5,25,4,2))
barplot( mostFrequentTerms(tdm.1, 20), horiz=TRUE )
barplot( mostFrequentTerms(tdm.2, 20), horiz=TRUE  )
barplot( mostFrequentTerms(tdm.3, 20), horiz=TRUE  )
barplot( mostFrequentTerms(tdm.4, 20), horiz=TRUE  )
barplot( mostFrequentTerms(tdm.5, 20), horiz=TRUE  )
barplot( mostFrequentTerms(tdm.6, 20), horiz=TRUE  )
par(las=0, mar=mar.orig)

head(findTerms (tdm.6, "(ste.*){0,1}milligan"))

docs.pre_concept <- docs
docs <- toConcept (docs, "(ste.*){0,1}milligan", "<MILLIGAN>")
docs <- toConcept (docs, "(ste.*){0,1}luczo", "<LUCZO>")
docs <- toConcept (docs, "chief executive officer", "<CEO>")
docs <- toConcept (docs, "chief financial officer", "<CEO>")


findTerms (tdm.6, "(ste.*){0,1}milligan")
findTerms (tdm.6, "(ste.*){0,1}luczo")

toConcept

docs.pre_concept <- docs
docs <- toConcept (docs, "(ste.*){0,1}milligan", "<MILLIGAN>")
docs <- toConcept (docs, "(ste.*){0,1}luczo", "<LUCZO>")
docs <- toConcept (docs, "ch", "<LUCZO>")

infile <- "synonym.csv"
synonyms <- read.csv(infile, stringsAsFactors=FALSE)
docs <- replaceSynonyms(docs, synonyms)

psyn <- findTerms(tdm.1, "predict|forecast|think")
psyn.words <- names(psyn)[- grep("predictable", names(psyn))]  #remove "predictable"
docs <- replaceSynonyms(docs, data.frame(matrix( psyn.words, nrow=1 )))

#repeat analysis (selective plots)
tdm.1 <- TermDocumentMatrix(docs)
tdm.2 <- makeNGram( docs, 2)
tdm.3 <- makeNGram( docs, 3)
tdm.4 <- makeNGram( docs, 4)
tdm.5 <- makeNGram( docs, 5)
tdm.6 <- makeNGram( docs, 6)

mar.orig <- par()$mar
par(las=2, mar=c(5,25,4,2))
barplot( mostFrequentTerms(tdm.1, 20), horiz=TRUE )
barplot( mostFrequentTerms(tdm.2, 20), horiz=TRUE  )
barplot( mostFrequentTerms(tdm.3, 20), horiz=TRUE  )
barplot( mostFrequentTerms(tdm.4, 20), horiz=TRUE  )
barplot( mostFrequentTerms(tdm.5, 20), horiz=TRUE  )
barplot( mostFrequentTerms(tdm.6, 20), horiz=TRUE  )
par(las=0, mar=mar.orig)