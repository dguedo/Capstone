## Securing the Data
setwd("~/GitHub/Capstone")

library(tm)

sampleSet <- function(){
  ## Read in the files
  blog    <- readLines("data/final/en_US/en_US.blogs.txt")
  news    <- readLines("data/final/en_US/en_US.news.txt")
  #twitter <- readLines("data/final/en_US/en_US.twitter.txt")
  
  ## Clean and sample the data sets
  #set.seed(90210)
  
  #combined_set <- c(blog, news, twitter)
  combined_set <- c(blog, news)
  sample_set   <- sample(combined_set, size = 1000, replace = TRUE)
  
  # encode
  sample_set <- iconv(sample_set, "latin1", "ASCII", "") 
  
  # create the corpus
  swiftkey = Corpus(VectorSource(sample_set))
  
  # clean up objects
  rm(blog, news, sample_set, combined_set)
  gc()
  
  # clean up
  swiftkey <- tm_map(swiftkey, content_transformer(tolower))
  swiftkey <- tm_map(swiftkey, stripWhitespace)
  swiftkey <- tm_map(swiftkey, removePunctuation)
  swiftkey <- tm_map(swiftkey, removeNumbers)
  gc()

  return(swiftkey)
}

createFrequencyMatrix <- function(sk_corpus, ngram, fileName){
  #
  tdm <- TermDocumentMatrix(sk_corpus, control = list(tokenize = ngram))
  freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  saveRDS(freq, fileName)
  rm(tdm, freq, sk_corpus)
  gc()
}

# use unigram for tie breaks
# create a simple top 3 words list? as last resort

# n-gram functions
unigram  <- function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
bigram   <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
trigram  <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
fourgram <- function(x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

# save to disk
createFrequencyMatrix(sampleSet(), unigram, "data/unigram.rds")
createFrequencyMatrix(sampleSet(), bigram, "data/bigram.rds")
createFrequencyMatrix(sampleSet(), trigram, "data/trigram.rds")
createFrequencyMatrix(sampleSet(), fourgram, "data/fourgram.rds")

# final model
finalModel <- function(file_freq, n){
  
  fm <- data.frame(readRDS(file_freq))
  names(fm)[1] <- "freq"
  fm$phrase <- row.names(fm)
  
  fm$first  <- lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][1])
  if (n == 2){
    fm$second  <- lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][2])
  } else if (n == 3){
    fm$second  <- lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][2])
    fm$third  <- lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][3])
  } else if (n == 4){
    fm$second  <- lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][2])
    fm$third  <- lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][3])
    fm$fourth  <- lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][4])
  }

  return(fm)
}

ngram.uni  <- finalModel("data/unigram.rds", 1)
ngram.bi   <- finalModel("data/bigram.rds", 2)
ngram.tri  <- finalModel("data/trigram.rds", 3)
ngram.four <- finalModel("data/fourgram.rds", 4)

save(ngram.uni, ngram.bi, ngram.tri, ngram.four, file="pridictionModel.rda")

#
load("pridictionModel.rda")
x <- ngram.tri[which(ngram.tri$first == "a" & ngram.tri$second == "couple"), ]
x[,c(1,5)]

rm(ngram.uni, ngram.bi, ngram.tri, ngram.four)
gc()

