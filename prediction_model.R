## Securing the Data
setwd("~/GitHub/Capstone")

#load libraries
library(tm)
  
# load the model
load("pridictionModel.rda")

# if(input == ''|input == "na na") return('Warning: Just input something')
phrase <- "once upon a"
#phrase <- " with experimental results showing the"
#phrase <- "Since then, something interesting has "
#phrase <- "at the end of the"
#phrase <- "maybe for the first"

# clean up
phrase <- trimws(phrase)
phrase <- tolower(phrase)
phrase <- stripWhitespace(phrase)
phrase <- removePunctuation(phrase)
phrase <- removeNumbers(phrase)

# split into words and find the last 4
phrase <- strsplit(phrase,split=" ")[[1]]

if (length(phrase) >= 3){
  phrase  <- phrase[(length(phrase) - 2):length(phrase)]
  index <- 3
  w1 <- phrase[index-2]
  w2 <- phrase[index-1]
} else {
  index <- length(phrase)
  w2 <- phrase[index-1]
}
w3 <- phrase[index]

# predict results
x4 <- subset(ngram.four, first == w1 & second == w2 & third == w3)
x3 <- subset(ngram.tri, first == w2 & second == w3)
x2 <- subset(ngram.bi, first == w3)
x1 <- ngram.uni[1:3,]

topResults <- function(df){
  results <- df[1:3,c("freq","nextWord")]
  results <- na.omit(results)
  return(results)
}

total <- rbind(topResults(x4), topResults(x3), topResults(x2), topResults(x1)) 
total <- total[!duplicated(total$nextWord),]
total[1:3,c("nextWord")]


unlist(total[1:3,c("nextWord")])


#if(nrow(x4)){
#  results <- x4[1:3,c("freq","nextWord")]
#  results <- na.omit(results)
#}




r1 <- x4[1:3,c("freq","nextWord")]
r1 <- na.omit(r1)
 
r2 <- x3[1:3,c("freq","nextWord")]

total <- rbind(r1, r2) 
total <- total[!duplicated(total$nextWord),]

total[1:3,]

x4[1:3,c("freq","nextWord")]
x3[1:3,c("freq","nextWord")]
x2[1:3,c("freq","nextWord")]
x1
phrase

if (is.na(x4)) {
  print("g")
  }

for(i in index:1) {
    print(i)
}
# checks if empty
if(!nrow(x4)){print("g")}
if(nrow(x4)){print("g")}



#x4 <- ngram.four[which(ngram.four$first == phrase[index-2] & ngram.four$second == phrase[index-1] & ngram.four$third == phrase[index]), ]
#x3 <- ngram.tri[which(ngram.tri$first == phrase[index-1] & ngram.tri$second == phrase[index]), ]
#x2 <- ngram.bi[ngram.bi$first == phrase[index], ]
#x1 <- ngram.uni[1:10,]


#phrase[(length(phrase) - 3):length(phrase)]
#phrase[(length(phrase) - 2):length(phrase)]
#phrase[(length(phrase) - 1):length(phrase)]


#result <- paste(ngram.bi[1,]$second, phrase)
