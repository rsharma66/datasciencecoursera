## Capstone - download data, import files, make samples

# Raj Sharma
# rsharma@polestarresearch.com

# invoke libraries

library("Matrix")
library("quanteda")
library(stringi)
library("tm")
library(data.table)

# specify the source and destination of the download

cname <- file.path("~", "Desktop/Coursera/Capstone Project/en_US")
flist <- list.files(path=cname, recursive=T, pattern="*.txt")
uscname <- file.path("~", "Desktop/Coursera/Capstone Project", "en_US")

## Import usdocs 

# Import blog data

usflist <- list.files(path=uscname, recursive = T)
blog_data <- file(paste(uscname, usflist[grep("*.blogs.*", usflist)], sep="/"), "r")
bloglines <- readLines(blog_data)
num_bloglines <- length(bloglines)

# Import news data

news_data <- file(paste(uscname, usflist[grep("*.news.txt", usflist)], sep="/"), "r")
newslines <- readLines(news_data)
num_newslines <- length(newslines)

# Import twitter data

twitter_data <- file(paste(uscname, usflist[grep("*.twitter.txt", usflist)], sep="/"), "r")
twitterlines <- readLines(twitter_data)
num_twitterlines <- length(twitterlines)

# save the data to .RData files

save(bloglines, file = "bloglines.RData")
save(newslines, file = "newslines.RData")
save(twitterlines, file = "twitterlines.RData")


## Create 15% samples of the imported data

# sample of blogs

blog_sample <- bloglines[sample(1:num_bloglines, 0.15*num_bloglines, replace = FALSE)]
close(blog_data)


# sample of news

news_sample <- newslines[sample(1:num_newslines, 0.15*num_newslines, replace = FALSE)]
close(news_data)


# sample of twitter

twitter_sample <- twitterlines[sample(1:num_twitterlines, 0.15*num_twitterlines, replace = FALSE)]
close(twitter_data)


# save the samples to .RData files

save(blog_sample, news_sample, twitter_sample, file = "sample_data.RData")
