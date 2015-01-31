library(jsonlite)

#convert object to json
myjson <- toJSON(iris, pretty=TRUE)
cat(myjson)

#convert json back to object
iris2 <- fromJSON(myjson)
print(iris2)

json_file <- "../yelp_dataset_challenge_academic_dataset/small_yelp.json"
json_lines <- readLines(json_file, n=5)

json_data <-fromJSON(json_file)

detach("package:jsonlite", unload=TRUE)

###########################
#install.packages("rjson")
library("rjson")
json_file <- "../yelp_dataset_challenge_academic_dataset/yelp_dataset_challenge_academic_dataset.json"
json_file <- "../yelp_dataset_challenge_academic_dataset/small_yelp.json"
json_file <- "../yelp_dataset_challenge_academic_dataset/tiny2_yelp.json"
json_data <- fromJSON(paste(readLines(json_file, n=100), collapse=""))
sj
json_data <- fromJSON(file=json_file)
jj2 <- toJSON(json_data)

xx<- readLines(json_file, n=100)

detach("package:rjson", unload=TRUE)
