# construct n-gram datasets from the samples
# Raj Sharma
# rsharma@polestarresearch.com

# invoke the libraries

library("Matrix")
library("quanteda")
library(stringi)
library(stringr)
library("tm")
library(data.table)

# load the sample data

load("sample_data.RData")

# construct corpus from sample data

c_blogs <- corpus(blog_sample)
c_news <- corpus(news_sample)
c_twitter <- corpus(twitter_sample)

c_all <- c(c_blogs, c_news, c_twitter)

c_all1 <- corpus_reshape(c_all, to="sentences")

save(c_all1, file = "c_all1.RData")
load("c_all1.RData")


## Put markers at the beginning and end of the lines, clean up characters

sampletext2 <- gsub("\\W", " ", c_all1)
sampletext11 <- gsub("youve", "you have", sampletext2, ignore.case = TRUE)
sampletext11 <- gsub("youre", "you are", sampletext11, ignore.case = TRUE)
sampletext11 <- gsub("^", "bos ", sampletext11)
sampletext12 <- gsub("$", " eos", sampletext11)

# Tokenize the document using Quanteda by first creating dfm

#remove numbers, remove punctuation and convert to lower case...not eliminating stopwords

tk1 <- tokens(sampletext12, remove_numbers=TRUE, remove_punct=TRUE)

tk2 <- tokens_tolower(tk1)

tk3 <- tokens_remove(tk2, stopwords("english"))

save(tk2, file = "tk2.RData")
save(tk3, file = "tk3.RData")

# tokenize with n-grams

tk21 <- tokens_ngrams(tk2, n = 1L, concatenator = " ")
tk22 <- tokens_ngrams(tk2, n = 2L, concatenator = " ")
tk23 <- tokens_ngrams(tk2, n = 3L, concatenator = " ")

tk31 <- tokens_ngrams(tk3, n = 1L, concatenator = " ")
tk32 <- tokens_ngrams(tk3, n = 2L, concatenator = " ")
tk33 <- tokens_ngrams(tk3, n = 3L, concatenator = " ")

save(tk21, tk22, tk23, file = "tk.RData")
save(tk31, tk32, tk33, file = "tka.RData")


#create dfm for n-grams

dfm1 <- dfm(tk21)
dfm2 <- dfm(tk22)
dfm3 <- dfm(tk23)

dfm1a <- dfm(tk31)
dfm2a <- dfm(tk32)
dfm3a <- dfm(tk33)


## changing dfm to matrix by pulling in frequency of words

library(dplyr)
library(data.table)

dfm11 <- data.frame(ngram = featnames(dfm1), count = colSums(dfm1), 
                 row.names = NULL, stringsAsFactors = FALSE)
dfm22 <- data.frame(ngram = featnames(dfm2), count = colSums(dfm2), 
                    row.names = NULL, stringsAsFactors = FALSE)
dfm33 <- data.frame(ngram = featnames(dfm3), count = colSums(dfm3), 
                    row.names = NULL, stringsAsFactors = FALSE)

dfm11a <- data.frame(ngram = featnames(dfm1a), count = colSums(dfm1a), 
                    row.names = NULL, stringsAsFactors = FALSE)
dfm22a <- data.frame(ngram = featnames(dfm2a), count = colSums(dfm2a), 
                    row.names = NULL, stringsAsFactors = FALSE)
dfm33a <- data.frame(ngram = featnames(dfm3a), count = colSums(dfm3a), 
                    row.names = NULL, stringsAsFactors = FALSE)


            

# save the dfm files to .RData files

save(dfm11, dfm22, dfm33, file = "dfm_data.RData")
save(dfm11a, dfm22a, dfm33a, file = "dfma_data.RData")


dt11 <- data.table(dfm11)
dt22 <- data.table(dfm22)
dt33 <- data.table(dfm33)


dt11a <- data.table(dfm11a)
dt22a <- data.table(dfm22a)
dt33a <- data.table(dfm33a)

dt22[, bigramID := 1:nrow(dt22)]
dt22[, c("first", "second") := tstrsplit(ngram, " ", fixed=TRUE)]

dt22a[, bigramID := 1:nrow(dt22a)]
dt22a[, c("first", "second") := tstrsplit(ngram, " ", fixed=TRUE)]


dt33[, trigramID := 1:nrow(dt33)]
dt33[, c("first", "second", "third") := tstrsplit(ngram, " ", fixed=TRUE)]

dt33a[, trigramID := 1:nrow(dt33a)]
dt33a[, c("first", "second", "third") := tstrsplit(ngram, " ", fixed=TRUE)]


save(dt11, dt22, dt33, file = "dt.RData")
save(dt11a, dt22a, dt33a, file = "dta.RData")
