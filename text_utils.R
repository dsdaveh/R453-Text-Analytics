library(tm)
library(stringr)
library(RWeka)


predictNext <- function( str) {
    grams <- unlist(str_split(str, "\\s+"))
    n <- length(grams)
    
    guess = NULL
    while (length(guess) < 1) { 
        ss <- sprintf("^%s\\s", paste(grams, collapse="\\s"))
        hits <- findTerms(ngram[[n+1]], ss)
        if ((length(hits) >0)) { 
            best <- names(hits)[which.max(hits)] 
            guess <- gsub(".*\\s(\\S+)?$", "\\1", best)  #strip off last word for guess
        } else {
            grams <- grams[-1]
            n <- length(grams)
            if (length(grams) < 1) { guess <- "the" }
        }
    }
    guess
}

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

eda.by.ngram <- function (corpus, n) {
    
    cat("creating ",n,"-gram TDM...\n")
    if (n > 1) {
        nTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
        tdm <- TermDocumentMatrix(corpus, control = list(tokenize = nTokenizer))
    } else {
        tdm <- TermDocumentMatrix(corpus)
    }
    tdm

    nterms <- tdm$nrow
    inspect(tdm[ c(1:3,seq(nterms-4,nterms)),1:3 ])  
    
    cat("Number of terms: ", nterms, "\n") #raw term count
    
    freq <- rowSums(as.matrix(tdm))
    #hist(freq)            # the first term dominates
    freq[which.max(freq)]  #list the most frequent term
    
    mostFrequentTerms(tdm,10)
    freq.top100 <- mostFrequentTerms(tdm,100)
    hist(freq.top100)
    
    
    sum.top10         <- sum(mostFrequentTerms(tdm,10))
    sum.top10/sum(freq) #perctance of words represented by top 10
    
    #create a Fibonacci series for xvalues
    title.cp <- sprintf("Coverage Plot for %d-grams", n)
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
          , ylab="Percent of corpus cover by most frequent", main=title.cp)
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
    
    
    #table(freq)
    #plot(table(freq))
    plot(table(round(log(freq),2)))
    
    # plot(table(round(log(freq.nonstop),2)))
    
    #count word lengths
    hist(str_length(names(freq)), xlim=c(0,20), breaks=100)
    mode <- names(sort(-table(str_length(names(freq)))))[1] #calculates the mode
    
    #top 10 of highest mode letter words
    freqN <- mostFrequentTerms(tdm,10000)  #assuming there are at least 10 in here
    freqN <- freqN[str_length(names(freqN)) == mode]
    
    freqN.top10 <- tail(freqN,10)
    par.mar <- par()$mar
    par(las=2,mar=c(9,4,4,2))
    barplot(tail(freqN,15))
    par(las=0,mar=par.mar)
    
    sum(mostFrequentTerms(tdm,1))/sum(freq) #
    tdm
    
#     eff <- 0
#     t1Counts <- numeric()  #number of ngrams for the first token in each ngram
#     if ( n > 1) {     #can't calculate for 1grams
#         
#         ptime40 <- proc.time()
#         it1 <- 0; #counter variable for the first tokens 
#         t.xlast <- "?"
#         for(i in 1:length(freq)) {
#             tokens <- unlist(str_split(names(freq[i]), " "))
#             t.x <- paste(tokens[-n], collapse=" ") ; t.y <- tokens[n] 
#             if (i %% 100000 == 0) cat("...", i, " of ", length(freq), " (", t.x, ")\n")
#             if (t.x != t.xlast) { it1 <- it1 + 1; t1Counts[it1] = 0 }
#             t1Counts[it1] = t1Counts[it1] + 1
#             t.xlast <- t.x
#         }
#         eff <- sum(t1Counts==1) / length(freq) 
#         
#         ptime41 <- proc.time()
#         ptime41-ptime40
#     }
#     eda <- list( eff=eff, t1Counts=t1Counts, tdm=tdm )
#     eda
    #eff
}


