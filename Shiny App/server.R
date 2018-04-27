#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(tidytext)
library(quanteda)
library(SnowballC)
library(data.table)


dict <- readRDS("./dict")
#grams1 = readRDS("./merged1_all")
gramsList <- list(
  grams2 = readRDS("./merged2_comp"),
  grams3 = readRDS("./merged3_comp"),
  grams4 = readRDS("./merged4_comp"),
  grams5 = readRDS("./merged5_comp")
)


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


getExpectedWord <- function(x, retmax = 15) {
  x <- cleanData(x)
  mWords <- match(words(x), dict)
  len <- length(mWords)
  startInd <- min(len, 4)
  if (exists("retOut")) {rm("retOut")}
  for(i in startInd:1) {
    xWords <- tail(mWords, i)
    grams <- as.data.frame(gramsList[[i]])
    cond <- (grams[,1] == xWords[1])
    if(i > 1){
      for(n in 2:i)
      {
        cond <- cond & (grams[,n] == xWords[n])
      }
    }
    out <- grams[cond, (i + 1):ncol(grams)]
    out[,1] <- dict[out[,1]]
    colnames(out)[1] <- "word" 
    out$gram <- rep((i + 1), nrow(out))
    if (!exists("retOut")) 
    {
      retOut <- out
    }
    else
    {
      retOut <- rbind(retOut, out) 
    }
  }
  
  nw <- retOut[,c(1, 2, 6)]
  nw[nw$nw == 0, 3] <- 0
  nw <- head(nw[order(nw$gram, nw$nw, decreasing = T),],retmax)
  
  bg <- retOut[,c(1, 3, 6)]
  bg[bg$bg == 0, 3] <- 0
  bg <- head(bg[order(bg$gram, bg$bg, decreasing = T),],retmax)
  
  tw <- retOut[,c(1, 4, 6)]
  tw[tw$tw == 0, 3] <- 0
  tw <- head(tw[order(tw$gram, tw$tw, decreasing = T),],retmax)
  
  comb <- retOut[,c(1, 5, 6)]
  comb[comb$comb == 0, 3] <- 0
  comb <- head(comb[order(comb$gram, comb$comb, decreasing = T),],retmax)
  
  out <- cbind(nw, bg, tw, comb)
  colnames(out) <- c("nw", "r", "g",   "bg" ,  "r", "g", 
                     "tw" ,   "r", "g","comb", "r", "g")
  out <- out[,c("nw", "bg" , "tw" ,"comb")]
  out <- cbind(1:retmax, out)
  colnames(out) <- c('Order', 'News Source', 'Blogs Source', 
                     'Twitter Source', 'combined results')
  
  out
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$result <- renderDataTable({
    if(nchar(input$text) > 0) {
      tryCatch(getExpectedWord(input$text, input$n), 
               error = function(e) NA) 
    }
  })
  
})
