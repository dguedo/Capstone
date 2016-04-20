## Securing the Data
setwd("~/GitHub/Capstone")

#load libraries
library(tm)
  
# load the model
load("pridictionModel.rda")

phrase <- "once upon a"

# predict results
result <- paste(ngram.bi[1,]$second, phrase)


