library(jsonlite)

# yelp2.json  was created from original tar extract
#             open in vi and remove first line (garbage) wq (write/quit) to convert from binary

json_file <- "../yelp_dataset_challenge_academic_dataset/yelp2b.json"
ptime0 <- proc.time()
json_lines <- readLines(json_file, skipNul=TRUE); ptime1 <- proc.time()-ptime0

#remove the last 47 lines (none JSON)  -- yelp2.json only
#json_lines <- json_lines[1:(length(json_lines)-47)]

json_fix <- paste(json_lines, collapse=",")
yelp <- fromJSON( paste("[", json_fix, "]") ); ptime1 <- proc.time()-ptime1

rm(json_fix, json_lines)

yelp$type <- as.factor(yelp$type)
reduce <- rbinom(nrow(yelp), 1, .001)
syelp <- yelp[reduce,]
#===========

json_file <- "../yelp_dataset_challenge_academic_dataset/review.json"
json_lines <- readLines(json_file, skipNul=TRUE, n=1000)
json_fix <- paste(json_lines, collapse=",")
yrev <- fromJSON( paste("[", json_fix, "]") )


