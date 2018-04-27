library(tm)
library(tidytext)
library(quanteda)
library(SnowballC)
library(dplyr)
setwd("/home/melabady/course/capstone/Coursera-SwiftKey/")

cleanData <- function(doc) {
  doc <- tolower(doc)
  doc <- removeNumbers(doc)
  doc <- paste(" ", doc, " ")
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't ", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't ", " not ", doc)
  doc <- gsub("'ll ", " will ", doc)
  doc <- gsub("'re ", " are ", doc)
  doc <- gsub("'ve ", " have ", doc)
  doc <- gsub("'m ", " am ", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub(" she's ([a-z]+)ing ", " she is \\1ing ", doc)
  doc <- gsub(" he's ([a-z]+)ing ", " he is \\1ing ", doc)
  doc <- gsub(" it's ([a-z]+)ing ", " it is \\1ing ", doc)
  
  doc <- gsub(" she's ", " she has ", doc)
  doc <- gsub(" he's ", " he has ", doc)
  doc <- gsub(" it's ", " it has ", doc)
  doc <- gsub("'s ", " ", doc)
  doc <- gsub("'d ", " would ", doc)

  doc <- gsub("[^a-z]", " ", doc)
  doc <- gsub(" *\\b[b-hj-z]{1}\\b *", " ", doc)  
  doc <- stripWhitespace(doc)
  return(doc)
}

en_US.news <- readLines("./en_US.news.txt")
en_US.news <- cleanData(en_US.news)
saveRDS(en_US.news, "./model/cleanedData/news.out")
rm(en_US.news)
gc()

en_US.blogs <- readLines("./en_US.blogs.txt")
en_US.blogs <- cleanData(en_US.blogs)
saveRDS(en_US.blogs, "./model/cleanedData/blogs.out")
rm(en_US.blogs)
gc()

en_US.twitter <- readLines("./en_US.twitter.txt")
en_US.twitter <- cleanData(en_US.twitter)
saveRDS(en_US.twitter, "./model/cleanedData/twitter.out")
rm(en_US.twitter)
gc()
