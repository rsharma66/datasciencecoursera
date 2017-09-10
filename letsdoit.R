# letsdoit.r
# Raj Sharma
# rsharma@polestarresearch.com

library(e1071)
library("quanteda")
library(stringi)
library("tm")
library(data.table)

## Read the n-grams files; dt22/dt33 are with stopwords, dt22a/dt33a are without stopwords

dt22 <- readRDS("dt22.rds")
dt33 <- readRDS("dt33.rds")

dt22a <- readRDS("dt22a.rds")
dt33a <- readRDS("dt33a.rds")


input <- "never mind I'll find"
PredictWords <- function(input) {
        if (input == "") {return(NULL)
        } else {
                ## functions to clean and preprocess the test sentence 
                
                test1 <- char_tolower(input)
                test1 <- removePunctuation(test1)
                
                ## Put markers at the beginning and end of the lines
                test11 <- gsub("\\W", " ", test1)
                test11 <- gsub("0", "", test11)
                test11 <- gsub("youve", "you have", test11)
                test11 <- gsub("youre", "you are", test11)
                test11 <- gsub("^", "bos ", test11)
                test12 <- gsub("$", " eos", test11)
                test12
                
                ## Make the sentences into tokens, to lowercase, remove numbers and punctuation
                
                tk1 <- tokens(test12, remove_numbers=TRUE, remove_punct=TRUE)
                
                tk2 <- tokens_tolower(tk1)
                
                tk3 <- tokens_remove(tk2, stopwords("english"))
                
                tk4 <- tokens_remove(tk3, c("bos","eos"))
                
                ll <- length(tstrsplit(tk4[1][[1]], " ", fixed=TRUE))
                
                if (ll == 0) {
                        ## make trigrams
                        tk23 <- tokens_ngrams(tk2, n = 3L, concatenator = " ")       
                        dfmtest3 <- dfm(tk23)
                        dfmtest13 <- data.frame(ngram = featnames(dfmtest3), count = colSums(dfmtest3), 
                                                row.names = NULL, stringsAsFactors = FALSE)
                        dtest13 <- data.table(dfmtest13)
                        names(dtest13) <- c("sample_ngram", "samplecount")
                        
                        ## Calculate MLEs
                        ## trigrams MLEs
                        
                        setkey(dtest13, sample_ngram)
                        dtest13[, traincount := dt33[.(sample_ngram), count]]
                        dtest13[, traincountKB := dt33[.(sample_ngram), countKB]]
                        dtest13[, c("first", "second", "third") := tstrsplit(sample_ngram, " ", fixed=TRUE)]
                        dtest13[, firsttwo := paste(first, second, sep=" ")]
                        setkey(dtest13, firsttwo)
                        setkey(dt33,firsttwo, count)
                        
                        firsttwolist <- c(dtest13[,firsttwo])
                        third1 <- matrix(ncol = 1)
                        third2 <- matrix(ncol = 1)
                        third3 <- matrix(ncol = 1)
                        
                        for (i in 1:length(firsttwolist)) {
                                third1[i] <- dt33[.(firsttwolist[i]), tail(third, 3)[3]]
                                third2[i] <- dt33[.(firsttwolist[i]), tail(third, 3)[2]]
                                third3[i] <- dt33[.(firsttwolist[i]), tail(third, 3)[1]]
                        } 
                        dtest13$third1 <- third1
                        dtest13$third2 <- third2
                        dtest13$third3 <- third3
                        
                        ## create an NA sample
                        NAtest13 <- dtest13[is.na(traincount)]
                        if (nrow(NAtest13) == 0) {
                                NAtest13 <- NULL
                                
                                ## Find third1 thru third3 for eos in third
                                
                                Pred <- dtest333a[grep("eos", dtest333a[,third])][,c("firsttwo", "third1", "third2", "third3")]
                                PredA <- rbind(Pred[complete.cases(Pred[,third3]), third3], Pred[complete.cases(Pred[,third2]), third2], Pred[complete.cases(Pred[,third1]), third1])
                                Pred1 <- PredA[1,1]
                                Pred2 <- PredA[2,1] 
                                Pred3 <- PredA[3,1]
                                c(Pred1, Pred2, Pred3)   
                        } else {
                                
                                ## assign 1 count to NA traincount
                                NAtest13[is.na(traincount), traincount := 1]
                                NAtest13[is.na(traincountKB), traincountKB := 0.5]
                                
                                ## setkeys
                                setkey(NAtest13, firsttwo)
                                setkey(dt22, first, count)
                                
                                firsttwolist <- c(NAtest13[,firsttwo])
                                secondlist <- c(NAtest13[,second])
                                fslist <- as.data.table(cbind(firsttwolist, secondlist))
                                
                                third1 <- matrix(ncol = 1)
                                third2 <- matrix(ncol = 1)
                                third3 <- matrix(ncol = 1)
                                
                                for (i in 1:nrow(fslist)) {
                                        if (fslist[i, firsttwolist] %in% dt33[,firsttwo]) {
                                                firsttwolist[i] <- fslist[i,firsttwolist]
                                                third1[i] <- dt33[.(firsttwolist[i]), tail(third, 3)[3]]
                                                third2[i] <- dt33[.(firsttwolist[i]), tail(third, 3)[2]]
                                                third3[i] <- dt33[.(firsttwolist[i]), tail(third, 3)[1]]
                                        } else {
                                                if (fslist[i, secondlist] %in% dt22[, first]) {
                                                        secondlist[i] <- fslist[i,secondlist]
                                                        third1[i] <- dt22[.(secondlist[i]), tail(second, 3)[3]]
                                                        third2[i] <- dt22[.(secondlist[i]), tail(second, 3)[2]]
                                                        third3[i] <- dt22[.(secondlist[i]), tail(second, 3)[1]]
                                                } else {
                                                        third1[i] <- dt22[("bos"), tail(second, 3)[3]]
                                                        third2[i] <- dt22[("bos"), tail(second, 3)[2]]
                                                        third3[i] <- dt22[("bos"), tail(second, 3)[1]]
                                                }
                                        }
                                }
                                
                                NAtest13$third1 <- third1
                                NAtest13$third2 <- third2
                                NAtest13$third3 <- third3
                                
                                
                                dtest333a <- merge(dtest13, NAtest13, by = "sample_ngram", all = TRUE)
                                dtest333a <- dtest333a[,c(1,7:8,19:21)]
                                
                                names(dtest333a) <- c("sample_ngram", "third", "firsttwo", "third1", "third2", "third3")
                                
                                ## Find third1 thru third3 for eos in third
                                
                                Pred <- dtest333a[grep("eos", dtest333a[,third])][,c("firsttwo", "third1", "third2", "third3")]
                                PredA <- rbind(Pred[complete.cases(Pred[,third3]), third3], Pred[complete.cases(Pred[,third2]), third2], Pred[complete.cases(Pred[,third1]), third1])
                                Pred1 <- PredA[1,1]
                                Pred2 <- PredA[2,1] 
                                Pred3 <- PredA[3,1]
                                c(Pred1, Pred2, Pred3)   
                        }} else {
                                ## make trigrams
                                tk23 <- tokens_ngrams(tk3, n = 3L, concatenator = " ")       
                                dfmtest3 <- dfm(tk23)
                                dfmtest13 <- data.frame(ngram = featnames(dfmtest3), count = colSums(dfmtest3), 
                                                        row.names = NULL, stringsAsFactors = FALSE)
                                dtest13 <- data.table(dfmtest13)
                                names(dtest13) <- c("sample_ngram", "samplecount")
                                
                                ## Calculate MLEs
                                ## trigrams MLEs
                                
                                setkey(dtest13, sample_ngram)
                                dtest13[, traincount := dt33a[.(sample_ngram), count]]
                                dtest13[, traincountKB := dt33a[.(sample_ngram), countKB]]
                                dtest13[, c("first", "second", "third") := tstrsplit(sample_ngram, " ", fixed=TRUE)]
                                dtest13[, firsttwo := paste(first, second, sep=" ")]
                                setkey(dtest13, firsttwo)
                                setkey(dt33a,firsttwo, count)
                                
                                firsttwolist <- c(dtest13[,firsttwo])
                                third1 <- matrix(ncol = 1)
                                third2 <- matrix(ncol = 1)
                                third3 <- matrix(ncol = 1)
                                
                                for (i in 1:length(firsttwolist)) {
                                        third1[i] <- dt33a[.(firsttwolist[i]), tail(third, 3)[3]]
                                        third2[i] <- dt33a[.(firsttwolist[i]), tail(third, 3)[2]]
                                        third3[i] <- dt33a[.(firsttwolist[i]), tail(third, 3)[1]]
                                } 
                                dtest13$third1 <- third1
                                dtest13$third2 <- third2
                                dtest13$third3 <- third3
                                
                                ## create an NA sample
                                NAtest13 <- dtest13[is.na(traincount)]
                                if (nrow(NAtest13) == 0) {
                                        NAtest13 <- NULL
                                        
                                        ## Find third1 thru third3 for eos in third
                                        
                                        Pred <- dtest333a[grep("eos", dtest333a[,third])][,c("firsttwo", "third1", "third2", "third3")]
                                        PredA <- rbind(Pred[complete.cases(Pred[,third3]), third3], Pred[complete.cases(Pred[,third2]), third2], Pred[complete.cases(Pred[,third1]), third1])
                                        Pred1 <- PredA[1,1]
                                        Pred2 <- PredA[2,1] 
                                        Pred3 <- PredA[3,1]
                                        c(Pred1, Pred2, Pred3) 
                                } else {
                                        
                                        ## assign 1 count to NA traincount
                                        NAtest13[is.na(traincount), traincount := 1]
                                        NAtest13[is.na(traincountKB), traincountKB := 0.5]
                                        
                                        ## setkeys
                                        setkey(NAtest13, firsttwo)
                                        setkey(dt22a, first, count)
                                        
                                        firsttwolist <- c(NAtest13[,firsttwo])
                                        secondlist <- c(NAtest13[,second])
                                        fslist <- as.data.table(cbind(firsttwolist, secondlist))
                                        
                                        third1 <- matrix(ncol = 1)
                                        third2 <- matrix(ncol = 1)
                                        third3 <- matrix(ncol = 1)
                                        
                                        for (i in 1:nrow(fslist)) {
                                                if (fslist[i, firsttwolist] %in% dt33a[,firsttwo]) {
                                                        firsttwolist[i] <- fslist[i,firsttwolist]
                                                        third1[i] <- dt33a[.(firsttwolist[i]), tail(third, 3)[3]]
                                                        third2[i] <- dt33a[.(firsttwolist[i]), tail(third, 3)[2]]
                                                        third3[i] <- dt33a[.(firsttwolist[i]), tail(third, 3)[1]]
                                                } else {
                                                        if (fslist[i, secondlist] %in% dt22a[, first]) {
                                                                secondlist[i] <- fslist[i,secondlist]
                                                                third1[i] <- dt22a[.(secondlist[i]), tail(second, 3)[3]]
                                                                third2[i] <- dt22a[.(secondlist[i]), tail(second, 3)[2]]
                                                                third3[i] <- dt22a[.(secondlist[i]), tail(second, 3)[1]]
                                                        } else {
                                                                third1[i] <- dt22a[("bos"), tail(second, 3)[3]]
                                                                third2[i] <- dt22a[("bos"), tail(second, 3)[2]]
                                                                third3[i] <- dt22a[("bos"), tail(second, 3)[1]]
                                                        }
                                                }
                                        }
                                        
                                        NAtest13$third1 <- third1
                                        NAtest13$third2 <- third2
                                        NAtest13$third3 <- third3
                                        
                                        dtest333a <- merge(dtest13, NAtest13, by = "sample_ngram", all = TRUE)
                                        dtest333a <- dtest333a[,c(1,7:8,19:21)]
                                        
                                        names(dtest333a) <- c("sample_ngram", "third", "firsttwo", "third1", "third2", "third3")
                                        
                                        ## Find third1 thru third3 for eos in third
                                        
                                        Pred <- dtest333a[grep("eos", dtest333a[,third])][,c("firsttwo", "third1", "third2", "third3")]
                                        PredA <- rbind(Pred[complete.cases(Pred[,third3]), third3], Pred[complete.cases(Pred[,third2]), third2], Pred[complete.cases(Pred[,third1]), third1])
                                        Pred1 <- PredA[1,1]
                                        Pred2 <- PredA[2,1] 
                                        Pred3 <- PredA[3,1]
                                        c(Pred1, Pred2, Pred3)  
                                        
                                        
                                }
                        }
                
                
        }
        
}
        