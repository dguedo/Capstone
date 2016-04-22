## Securing the Data
setwd("~/GitHub/Capstone")

library(tm)

# use unigram for tie breaks

combineDF <- function(fileName1, fileName2, saveName){

  # load the model
  load(fileName1)
  
  ngram1.uni  <- ngram.uni
  ngram1.bi   <- ngram.bi
  ngram1.tri  <- ngram.tri
  ngram1.four <- ngram.four
  # clean up
  rm(ngram.uni, ngram.bi, ngram.tri, ngram.four)
  
  
  load(fileName2)
  
  ngram2.uni  <- ngram.uni
  ngram2.bi   <- ngram.bi
  ngram2.tri  <- ngram.tri
  ngram2.four <- ngram.four
  # clean up
  rm(ngram.uni, ngram.bi, ngram.tri, ngram.four)
  
  gc()
  
  c1 <- aggregate(freq ~ ., data = rbind(ngram1.uni, ngram2.uni), FUN = sum)
  c1 <- c1[with(c1, order(-freq, nextWord)), ]
  
  c2 <- aggregate(freq ~ ., data = rbind(ngram1.bi, ngram2.bi), FUN = sum)
  c2 <- c2[with(c2, order(-freq, nextWord)), ]
  
  c3 <- aggregate(freq ~ ., data = rbind(ngram1.tri, ngram2.tri), FUN = sum)
  c3 <- c3[with(c3, order(-freq, nextWord)), ]
  
  c4 <- aggregate(freq ~ ., data = rbind(ngram1.four, ngram2.four), FUN = sum)
  c4 <- c4[with(c4, order(-freq, nextWord)), ]

  save(c1,c2,c3,c4, file=saveName)
  
  rm(c1,c2,c3,c4)
  gc()
  
}


combineModels <- function(fileName1){

  start_time <- Sys.time()
  
  # load the model
  load(fileName1)
  
  t1 <- c1
  t2 <- c2
  t3 <- c3
  t4 <- c4
  # clean up
  rm(c1, c2, c3, c4)
  gc()
  
  load("data/combinedModel.rda")

  c1 <- aggregate(freq ~ ., data = rbind(c1, t1), FUN = sum)
  c1 <- c1[with(c1, order(-freq, nextWord)), ]
  
  c2 <- aggregate(freq ~ ., data = rbind(c2, t2), FUN = sum)
  c2 <- c2[with(c2, order(-freq, nextWord)), ]
  
  c3 <- aggregate(freq ~ ., data = rbind(c3, t3), FUN = sum)
  c3 <- c3[with(c3, order(-freq, nextWord)), ]
  
  c4 <- aggregate(freq ~ ., data = rbind(c4, t4), FUN = sum)
  c4 <- c4[with(c4, order(-freq, nextWord)), ]
  
  save(c1,c2,c3,c4, file="data/combinedModel.rda")

  print(paste("RUNTIME: ", fileName1))
  print(Sys.time() - start_time)
  
  rm(c1,c2,c3,c4)
  gc()
  
}

# Combine the models
combineDF("data/pridictionModel1.rda", "data/pridictionModel2.rda", "data/combinedModel1.rda")
combineDF("data/pridictionModel3.rda", "data/pridictionModel4.rda", "data/combinedModel2.rda")
combineDF("data/pridictionModel5.rda", "data/pridictionModel6.rda", "data/combinedModel3.rda")
combineDF("data/pridictionModel7.rda", "data/pridictionModel8.rda", "data/combinedModel4.rda")
combineDF("data/pridictionModel9.rda", "data/pridictionModel10.rda", "data/combinedModel5.rda")

combineDF("data/pridictionModel11.rda", "data/pridictionModel12.rda", "data/combinedModel6.rda")
combineDF("data/pridictionModel12.rda", "data/pridictionModel14.rda", "data/combinedModel7.rda")
combineDF("data/pridictionModel15.rda", "data/pridictionModel16.rda", "data/combinedModel8.rda")
combineDF("data/pridictionModel17.rda", "data/pridictionModel18.rda", "data/combinedModel9.rda")
combineDF("data/pridictionModel19.rda", "data/pridictionModel20.rda", "data/combinedModel10.rda")
#
combineModels("data/combinedModel1.rda")
combineModels("data/combinedModel2.rda")
combineModels("data/combinedModel3.rda")
combineModels("data/combinedModel4.rda")
combineModels("data/combinedModel5.rda")

combineModels("data/combinedModel6.rda")
combineModels("data/combinedModel7.rda")
combineModels("data/combinedModel8.rda")
combineModels("data/combinedModel9.rda")
combineModels("data/combinedModel10.rda")

#
load("data/combinedModel.rda")

ngram.uni  <- c1[1:10,]
ngram.bi   <- c2[c2$freq != 1,]
ngram.tri  <- c3[c3$freq != 1,]
ngram.four <- c4[c4$freq != 1,]

ngram.uni$phrase  <- NULL
ngram.bi$phrase  <- NULL
ngram.tri$phrase  <- NULL
ngram.four$phrase  <- NULL


save(ngram.uni, ngram.bi, ngram.tri, ngram.four, file="data/pridictionModel.rda")

#
load("data/pridictionModel.rda")
