library(quanteda)
library(tm)
library(SnowballC)
library(qdap)
library(textclean)
library(data.table)

setwd("/home/melabady/course/capstone/Coursera-SwiftKey/")

#######################dictionary#######################
########################################################
out <- readRDS("./model/cleanedData/blogs.out")
dict <- unique(words(out))

out <- readRDS("./model/cleanedData/news.out")
dict <- c(dict, unique(words(out)))

out <- readRDS("./model/cleanedData/twitter.out")
dict <- c(dict, unique(words(out)))

dict <- unique(dict)
dict <- sort(dict)
saveRDS(dict,"./model/gramsData/dict")
rm(out)
gc()
########################################################

dict <- readRDS("./model/gramsData/dict")

NGramTokenizer <-
  function(x, n = 3)
    as.data.frame(matrix(match(words(unlist(tokens(x, ngram = n, concatenator = " "), use.names = F)),
                 dict),
           ncol = n, byrow = T))
    #unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)

getFreqTerms <- function(data, nGram = 3, bulksize = 200000, perfix = "news") {
  data_length <- length(data)
  n <- ceiling(data_length/bulksize)
  path <- paste("./model/gramsData/", perfix, nGram, sep = "")
  for(i in 1:n) {
    start_idx <- ((i- 1) * bulksize) + 1
    end_idx <- min(i * bulksize, data_length)
    path2 <- paste(path, i, sep = "_")
    terms <- NGramTokenizer(data[start_idx:end_idx], nGram)
    terms$freq <- 1 
    terms <- aggregate(freq ~ ., data = terms, FUN = sum)
    saveRDS(terms, path2)
    rm(terms)
    gc()
    print(i)
  }
}

out <- readRDS("./model/cleanedData/news.out")
getFreqTerms(out, nGram = 5, bulksize = 10000, perfix = "news")
rm(out)
gc()

out <- readRDS("./model/cleanedData/blogs.out")
getFreqTerms(out, nGram = 5, bulksize = 10000, perfix = "blogs")
rm(out)
gc()

out <- readRDS("./model/cleanedData/twitter.out")
getFreqTerms(out, nGram = 5, bulksize = 10000, perfix = "twitter")
rm(out)
gc()


out <- readRDS("./model/cleanedData/news.out")
getFreqTerms(out, nGram = 4, bulksize = 10000, perfix = "news")
rm(out)
gc()

out <- readRDS("./model/cleanedData/blogs.out")
getFreqTerms(out, nGram = 4, bulksize = 10000, perfix = "blogs")
rm(out)
gc()

out <- readRDS("./model/cleanedData/twitter.out")
getFreqTerms(out, nGram = 4, bulksize = 10000, perfix = "twitter")
rm(out)
gc()


out <- readRDS("./model/cleanedData/news.out")
getFreqTerms(out, nGram = 3, bulksize = 10000, perfix = "news")
rm(out)
gc()

out <- readRDS("./model/cleanedData/blogs.out")
getFreqTerms(out, nGram = 3, bulksize = 10000, perfix = "blogs")
rm(out)
gc()

out <- readRDS("./model/cleanedData/twitter.out")
getFreqTerms(out, nGram = 3, bulksize = 10000, perfix = "twitter")
rm(out)
gc()

out <- readRDS("./model/cleanedData/news.out")
getFreqTerms(out, nGram = 2, bulksize = 10000, perfix = "news")
rm(out)
gc()

out <- readRDS("./model/cleanedData/blogs.out")
getFreqTerms(out, nGram = 2, bulksize = 10000, perfix = "blogs")
rm(out)
gc()

out <- readRDS("./model/cleanedData/twitter.out")
getFreqTerms(out, nGram = 2, bulksize = 10000, perfix = "twitter")
rm(out)
gc()

out <- readRDS("./model/cleanedData/news.out")
getFreqTerms(out, nGram = 1, bulksize = 10000, perfix = "news")
rm(out)
gc()

out <- readRDS("./model/cleanedData/blogs.out")
getFreqTerms(out, nGram = 1, bulksize = 10000, perfix = "blogs")
rm(out)
gc()

out <- readRDS("./model/cleanedData/twitter.out")
getFreqTerms(out, nGram = 1, bulksize = 10000, perfix = "twitter")
rm(out)
gc()

########################################################
#######################load data########################
########################################################
loadData <- function(nGram = 3, n = 3, perfix = "news") {
  path <- paste("./model/gramsData/", perfix, nGram, sep = "")
  for(i in 1:n) {
    path2 <- paste(path, i, sep = "_")
    if(i == 1) {
      out <- data.table(readRDS(path2))
      names <- colnames(out)[-ncol(out)]
    }
    else { 
      out <- rbind(out, readRDS(path2))
      #print(class(out))
      #print(colnames(out))
      #print(nrow(out))
    }
    print(i)
    if(i%%50 == 0)
    {
      out <- out[,list(freq = sum(freq)), by = names]
      gc()
    }
    #gc()
  }
  #out <- aggregate(freq ~ ., data = out, FUN = sum)
  if(i%%50 != 0)
  {
    print("before final aggr.")
    out <- out[,list(freq = sum(freq)), by = names]
    gc()
  }
  saveRDS(out, paste(path, "all", sep = "_"))
  rm(out)
  gc()
}

loadData(nGram = 5, n = 102, perfix = "news")
loadData(nGram = 5, n = 90, perfix = "blogs")
loadData(nGram = 5, n = 237, perfix = "twitter")

loadData(nGram = 4, n = 102, perfix = "news")
loadData(nGram = 4, n = 90, perfix = "blogs")
loadData(nGram = 4, n = 237, perfix = "twitter")

loadData(nGram = 3, n = 102, perfix = "news")
loadData(nGram = 3, n = 90, perfix = "blogs")
loadData(nGram = 3, n = 237, perfix = "twitter")

loadData(nGram = 2, n = 102, perfix = "news")
loadData(nGram = 2, n = 90, perfix = "blogs")
loadData(nGram = 2, n = 237, perfix = "twitter")

loadData(nGram = 1, n = 102, perfix = "news")
loadData(nGram = 1, n = 90, perfix = "blogs")
loadData(nGram = 1, n = 237, perfix = "twitter")
###################################################
out <- readRDS("./model/gramsData/blogs5_all")
out <- out[out$freq > 1,]
colnames(out)[ncol(out)] <- "bg"

out2 <- readRDS("./model/gramsData/news5_all")
out2 <- out2[out2$freq > 1,]
colnames(out2)[ncol(out2)] <- "nw"

out3 <- readRDS("./model/gramsData/twitter5_all")
out3 <- out3[out3$freq > 1,]
colnames(out3)[ncol(out3)] <- "tw"

merged <- merge(out2, out, by = colnames(out2)[-ncol(out2)], all = T)
merged <- merge(merged, out3, by = colnames(out2)[-ncol(out2)], all = T)

merged[is.na(merged)] <- 0
merged$comb <- merged$nw + merged$bg + merged$tw   
saveRDS(data.table(merged),"./model/gramsData/merged5_all")
##########################
##########################
out <- readRDS("./model/gramsData/blogs4_all")
out <- out[out$freq > 2,]
colnames(out)[ncol(out)] <- "bg"

out2 <- readRDS("./model/gramsData/news4_all")
out2 <- out2[out2$freq > 2,]
colnames(out2)[ncol(out2)] <- "nw"

out3 <- readRDS("./model/gramsData/twitter4_all")
out3 <- out3[out3$freq > 2,]
colnames(out3)[ncol(out3)] <- "tw"

merged <- merge(out2, out, by = colnames(out2)[-ncol(out2)], all = T)
merged <- merge(merged, out3, by = colnames(out2)[-ncol(out2)], all = T)

merged[is.na(merged)] <- 0
merged$comb <- merged$nw + merged$bg + merged$tw   
saveRDS(data.table(merged),"./model/gramsData/merged4_all")
##########################
##########################
out <- readRDS("./model/gramsData/blogs3_all")
out <- out[out$freq > 3,]
colnames(out)[ncol(out)] <- "bg"

out2 <- readRDS("./model/gramsData/news3_all")
out2 <- out2[out2$freq > 3,]
colnames(out2)[ncol(out2)] <- "nw"

out3 <- readRDS("./model/gramsData/twitter3_all")
out3 <- out3[out3$freq > 3,]
colnames(out3)[ncol(out3)] <- "tw"

merged <- merge(out2, out, by = colnames(out2)[-ncol(out2)], all = T)
merged <- merge(merged, out3, by = colnames(out2)[-ncol(out2)], all = T)

merged[is.na(merged)] <- 0
merged$comb <- merged$nw + merged$bg + merged$tw   
saveRDS(data.table(merged),"./model/gramsData/merged3_all")
##########################
##########################
out <- readRDS("./model/gramsData/blogs2_all")
out <- out[out$freq > 4,]
colnames(out)[ncol(out)] <- "bg"

out2 <- readRDS("./model/gramsData/news2_all")
out2 <- out2[out2$freq > 4,]
colnames(out2)[ncol(out2)] <- "nw"

out3 <- readRDS("./model/gramsData/twitter2_all")
out3 <- out3[out3$freq > 4,]
colnames(out3)[ncol(out3)] <- "tw"

merged <- merge(out2, out, by = colnames(out2)[-ncol(out2)], all = T)
merged <- merge(merged, out3, by = colnames(out2)[-ncol(out2)], all = T)

merged[is.na(merged)] <- 0
merged$comb <- merged$nw + merged$bg + merged$tw   
saveRDS(data.table(merged),"./model/gramsData/merged2_all")
##########################
##########################
out <- readRDS("./model/gramsData/blogs1_all")
out <- out[out$freq > 5,]
colnames(out)[ncol(out)] <- "bg"

out2 <- readRDS("./model/gramsData/news1_all")
out2 <- out2[out2$freq > 5,]
colnames(out2)[ncol(out2)] <- "nw"

out3 <- readRDS("./model/gramsData/twitter1_all")
out3 <- out3[out3$freq > 5,]
colnames(out3)[ncol(out3)] <- "tw"

merged <- merge(out2, out, by = colnames(out2)[-ncol(out2)], all = T)
merged <- merge(merged, out3, by = colnames(out2)[-ncol(out2)], all = T)

merged[is.na(merged)] <- 0
merged$comb <- merged$nw + merged$bg + merged$tw   
saveRDS(data.table(merged),"./model/gramsData/merged1_all")
##########################
##########################
threshold <- 4
path <- "./model/gramsData/merged2_all"
for(i in 5:2)
{
  path1 <- gsub("2", i, path)
  merged <- readRDS(path1)
  merged <- merged[merged$comb > threshold,]
  saveRDS(merged, gsub("all", "comp", path1))
}