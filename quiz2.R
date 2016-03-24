## Securing the Data
setwd("~/GitHub/Capstone")

library(tm)
library(knitr)
library(ggplot2)

## Securing the Data and Preliminary Analyses
## Read in the files
blog    <- readLines("data/final/en_US/en_US.blogs.txt")
news    <- readLines("data/final/en_US/en_US.news.txt")
#twitter <- readLines("data/final/en_US/en_US.twitter.txt")

#kable(data.frame(
#  "Data File"       = c("Blogs", "News", "Twitter"), 
#  "Line Count"      = c(length(blog), length(news), length(twitter)),
#  "Character Count" = c( sum(nchar(blog)), sum(nchar(news)), sum(nchar(twitter)))
#))

## Clean and sample the data sets
set.seed(90210)

#combined_set <- c(blog, news, twitter)
combined_set <- c(blog, news)
sample_set   <- sample(combined_set, size = 1000000, replace = TRUE)

# encode
sample_set <- iconv(sample_set, "latin1", "ASCII", "") 

# create the corpus
swiftkey = Corpus(VectorSource(sample_set))

# clean up objects
rm(blog, news, sample_set, combined_set)
#rm(sample_set)

#rm(blog, news, twitter, sample_set, combined_set)
gc()

swiftkey <- tm_map(swiftkey, content_transformer(tolower))

# remove the most commonly used words in the english language
# swiftkey <- tm_map(swiftkey, removeWords, stopwords("english"))

# reduce inflected (or sometimes derived) words to their word stem
swiftkey <- tm_map(swiftkey, stemDocument)

# clean up
swiftkey <- tm_map(swiftkey, stripWhitespace)
swiftkey <- tm_map(swiftkey, removePunctuation)
swiftkey <- tm_map(swiftkey, removeNumbers)

gc()


# unigram functions
unigram <- function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
bigram  <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
trigram <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

#tdm1 <- TermDocumentMatrix(swiftkey, control = list(tokenize = unigram))
#tdm2 <- TermDocumentMatrix(swiftkey, control = list(tokenize = bigram))

#tdm2Sparse <- removeSparseTerms(tdm2, .99)

#rm(tdm2)
#gc()

tdm3 <- TermDocumentMatrix(swiftkey, control = list(tokenize = trigram))

 #rm(swiftkey)
#gc()

#tdm3Sparse <- removeSparseTerms(tdm3, .999)
#rm(tdm3)
#gc()

#tdm3Sparse

#freq2 <- sort(rowSums(as.matrix(tdm2Sparse)), decreasing = TRUE)
#freq3 <- sort(rowSums(as.matrix(tdm3Sparse)), decreasing = TRUE)


#freq2 <- sort(rowSums(as.matrix(tdm2Sparse)), decreasing = TRUE)

#rm(swiftkey)

#tdm3 <- TermDocumentMatrix(swiftkey, control = list(tokenize = trigram))
#freq3 <- sort(rowSums(as.matrix(tdm3)), decreasing = TRUE)

# cleanup
#rm(swiftkey, tdm3)
#gc()

#freq1 <- sort(rowSums(as.matrix(tdm1)), decreasing = TRUE)[1:15]
#rm(tdm1)

#freq2 <- sort(rowSums(as.matrix(tdm2)), decreasing = TRUE)[1:15]
#rm(tdm2)

#freq3 <- sort(rowSums(as.matrix(tdm3)), decreasing = TRUE)[1:15]
#rm(tdm3)

#t1 <- freq3[grep("i+", names(freq3))]


