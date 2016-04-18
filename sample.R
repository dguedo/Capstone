library(R.utils)

## Securing the Data
setwd("~/GitHub/Capstone")

## Load files
file_blogs = "data/final/en_US/en_US.blogs.txt"
file_news = "data/final/en_US/en_US.news.txt"
file_twitter = "data/final/en_US/en_US.twitter.txt"

#blogs <- read.table(file_blogs, header=F, sep="\n")
#twitter <- read.table(file_twitter, header=F, sep="\n")

#twitter_lines <- length(readLines(file_twitter))

blog <- readLines(file_blogs)
news <- readLines(file_news)
twitter <- readLines(file_twitter)

## 3
#perline_blog <- unlist(lapply(X = blog, FUN = nchar))
#longest_blog <- max(nchar.perline)

#perline_news <- unlist(lapply(X = news, FUN = nchar))
#longest_news <- max(perline_news)

## 4
#love_twitter <- grep("love", twitter)
#length(love_twitter) #90956

#hate_twitter <- grep("hate", twitter)
#length(hate_twitter) #22138

#90956/22138 #4.108592

# 5
twitter[grep(pattern = "biostat", x = twitter)]

# 6
sum(grepl(pattern = "A computer once beat me at chess, but it was no match for me at kickboxing", x = twitter))
