## Securing the Data
setwd("~/GitHub/Capstone")

library(tm)

sampleSet <- function(){
  ## Read in the files
  blog    <- readLines("data/final/en_US/en_US.blogs.txt")
  news    <- readLines("data/final/en_US/en_US.news.txt")
  twitter <- readLines("data/final/en_US/en_US.twitter.txt")
  
  ## Clean and sample the data sets
  #set.seed(90210)
  
  combined_set <- c(blog, news, twitter)
  #combined_set <- c(blog, news)
  sample_set   <- sample(combined_set, size = 6000, replace = TRUE)
  
  # encode
  sample_set <- iconv(sample_set, "latin1", "ASCII", "") 
  
  # create the corpus
  swiftkey = Corpus(VectorSource(sample_set))
  
  # clean up objects
  #rm(blog, news, sample_set, combined_set)
  rm(blog, news, twitter, sample_set, combined_set)
  gc()
  
  # clean up
  swiftkey <- tm_map(swiftkey, content_transformer(tolower))
  swiftkey <- tm_map(swiftkey, stripWhitespace)
  swiftkey <- tm_map(swiftkey, removePunctuation)
  swiftkey <- tm_map(swiftkey, removeNumbers)
  gc()

  #getURL("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en")  
  
  return(swiftkey)
}

createFrequencyMatrix <- function(sk_corpus, ngram, fileName){
  #
  tdm <- TermDocumentMatrix(sk_corpus, control = list(tokenize = ngram))
  freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  #return (freq)
  saveRDS(freq, fileName)
  rm(tdm, freq, sk_corpus)
  gc()
}

# n-gram functions
unigram  <- function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
bigram   <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
trigram  <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
fourgram <- function(x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

# final model
finalModel <- function(file_freq, n){
  
  fm <- data.frame(readRDS(file_freq))
  names(fm)[1] <- "freq"
  fm$phrase <- row.names(fm)
  
  
  if (n == 2){
    fm$first     <- unlist(lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][1]))
    fm$nextWord  <- unlist(lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][2]))
  } else if (n == 3){
    fm$first     <- unlist(lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][1]))
    fm$second    <- unlist(lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][2]))
    fm$nextWord  <- unlist(lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][3]))
  } else if (n == 4){
    fm$first     <- unlist(lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][1]))
    fm$second    <- unlist(lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][2]))
    fm$third     <- unlist(lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][3]))
    fm$nextWord  <- unlist(lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][4]))
  } else {
    fm$nextWord  <- unlist(lapply(fm$phrase, function(x) strsplit(x,split=" ")[[1]][1]))
  }
  
  return(fm)
}

for(i in 1:10) {

  start_time <- Sys.time()

  file_uni <- paste("data/unigram", i, ".rds", sep = "")
  file_bi  <- paste("data/bigram", i, ".rds", sep = "")
  file_tri  <- paste("data/trigram", i, ".rds", sep = "")
  file_four <- paste("data/fourgram", i, ".rds", sep = "")

  # create frequency model
  createFrequencyMatrix(sampleSet(), unigram, file_uni)
  createFrequencyMatrix(sampleSet(), bigram, file_bi)
  createFrequencyMatrix(sampleSet(), trigram, file_tri)
  createFrequencyMatrix(sampleSet(), fourgram, file_four)

  # organise
  ngram.uni  <- finalModel(file_uni, 1)
  ngram.bi   <- finalModel(file_bi, 2)
  ngram.tri  <- finalModel(file_tri, 3)
  ngram.four <- finalModel(file_four, 4)

  file_model <- paste("data/pridictionModel", i, ".rda", sep = "") 

  save(ngram.uni, ngram.bi, ngram.tri, ngram.four, file=file_model)

  rm(ngram.uni, ngram.bi, ngram.tri, ngram.four)
  gc()
  
  print(paste("RUNTIME: For loop,", i))
  print(Sys.time() - start_time)
  
}


