# KB_nlp.r
# Raj Sharma
# rsharma@polestarresearch.com

# load the libraries

library(e1071)
library("Matrix")
library("quanteda")
library(stringi)
library("tm")
library(data.table)

# load the dt data

load("dt.RData")
load("dta.RData")


## Calculate MLE for unigrams

dt11[, mle := count/sum(count)]
dt11a[, mle := count/sum(count)]

# Calculate MLE for bigrams

setkey(dt11, ngram)
setkey(dt11a, ngram)
setkey(dt22, first)
setkey(dt22a, first)
dt22[, unigramcount := sum(count), by=first]
dt22[,mle := (count/unigramcount)]
dt22a[, unigramcount := sum(count), by=first]
dt22a[,mle := (count/unigramcount)]


## Calculate MLE for trigrams, quadgrams

dt33[,firsttwo := paste(first, second, sep=" ")]
setkey(dt33, firsttwo)
setkey(dt22, ngram)
dt33[1,firsttwocount := dt11["bos", count]]
dt33[2:nrow(dt33),firsttwocount := dt22[.(firsttwo), count]]
dt33[,mle := (count/firsttwocount)]

dt33a[,firsttwo := paste(first, second, sep=" ")]
setkey(dt33a, firsttwo)
setkey(dt22a, ngram)
dt33a[1,firsttwocount := dt11a["bos", count]]
dt33a[2:nrow(dt33a),firsttwocount := dt22a[.(firsttwo), count]]
dt33a[,mle := (count/firsttwocount)]



## Katz-Backoff Method
## Create discounted mle
## unigrams

dt11[, countKB := count - 0.5]
dt11[, mleKB := countKB/sum(count)]
dt11a[, countKB := count - 0.5]
dt11a[, mleKB := countKB/sum(count)]

## bigrams

dt22[, countKB := count - 0.5]
dt22[, mleKB := countKB/sum(count)]
dt22a[, countKB := count - 0.5]
dt22a[, mleKB := countKB/sum(count)]

setkey(dt22, first)
dt22[, unigramcount := sum(count), by=first]
dt22[, unigramcountKB := sum(countKB), by=first]


setkey(dt22a, first)
dt22a[, unigramcount := sum(count), by=first]
dt22a[, unigramcountKB := sum(countKB), by=first]

## trigrams

dt33[, countKB := count - 0.5]
dt33[, mleKB := countKB/sum(count)]

dt33a[, countKB := count - 0.5]
dt33a[, mleKB := countKB/sum(count)]

setkey(dt33, firsttwo)
setkey(dt33a, firsttwo)

dt33[, bigramcount := sum(count), by=firsttwo]
dt33[, bigramcountKB := sum(countKB), by=firsttwo]
dt33a[, bigramcount := sum(count), by=firsttwo]
dt33a[, bigramcountKB := sum(countKB), by=firsttwo]


dt33 <- dt33[,c(1,2,6,7,10:13)]
dt22 <- dt22[,c(1,2,4,5,9)]

dt33a <- dt33a[,c(1,2,6,7,10:13)]
dt22a <- dt22a[,c(1,2,4,5,9)]

dt22 <- dt22[count > 1]
dt33 <- dt33[count > 1]

dt22a <- dt22a[count > 1]
dt33a <- dt33a[count > 1]

setkey(dt22, ngram)
setkey(dt33, ngram)

setkey(dt22a, ngram)
setkey(dt33a, ngram)

save(dt11,file = "dt11.RData")
save(dt11a,file = "dt11a.RData")
save(dt22,file = "dt22.RData")
save(dt22a,file = "dt22a.RData")
save(dt33,file = "dt33.RData")
save(dt33a,file = "dt33a.RData")


saveRDS(dt22, "dt22.rds")
saveRDS(dt33, "dt33.rds")

saveRDS(dt22a, "dt22a.rds")
saveRDS(dt33a, "dt33a.rds")


test1 <- "child might stick her finger or a foreign object into an electrical"







