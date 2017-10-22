# load_libraries.R
# Created: 29-08-2017
# Last Update: 21-10-2017
# Agustín Suárez
# agustin.suarez@virtual-identity.com
# Data Analytst
# ------------------

## install libraries
install.packages("openNLP")
install.packages("stringi")
install.packages("tm")
install.packages("RWeka")
install.packages("dplyr")
install.packages("caTools")

# require libraries
require("stringi")
require("openNLP")
require("tm")
require("RWeka")
require("dplyr")
require("knitr")
require("caTools")

# Set working directory (works on Linux's operating systems)
# For Windows reference using (C:/capstone/R/switfkey) alike structure
wd <- "~/capstone/R/swiftkey"
setwd(wd)


# Check if file already exists on wd/data folder.
# Please note that the file size is greater than 570Mb!!
# If file already exists, skip the download.
if (!file.exists("./data")) {dir.create("data")}
if (!file.exists("./data/Coursera-SwiftKey.zip")) {
  dataURL <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(dataURL, destfile <- "./data/Coursera-SwiftKey.zip")
}

# Use the unzip function to extract the contents of the downloaded file.
# However check previously that the uncrompress has not been done already!!

blogs <- "./data/final/en_US/en_US.blogs.txt"
news <- "./data/final/en_US/en_US.news.txt"
twitter <- "./data/final/en_US/en_US.twitter.txt"

myfiles <- file.exists(blogs) & file.exists(news) & file.exists(twitter)
if (!myfiles) {
  unzip("./data/Coursera-SwiftKey.zip", exdir = "./data")
}

# Import (read) data
blogsCorpus <- readLines(blogs, warn = FALSE, encoding = "UTF-8")
newsCorpus <- readLines(news, warn = FALSE, encoding = "UTF-8")
twitterCorpus <- readLines(twitter, warn = FALSE, encoding = "UTF-8")

# Get files size
filesSizes <- sapply(list(blogsCorpus,newsCorpus,twitterCorpus), function(x){format(object.size(x),"auto")})

# and number of lines for each file
filesLength <- sapply(list(blogsCorpus,newsCorpus,twitterCorpus), function(x){length(x)})

# Count the number of words in each corpus (this may take some time)
filesWords <- sapply(list(blogsCorpus,newsCorpus,twitterCorpus), function(x){sum(stringi::stri_count_words(x))})

# Calculate number of words per Line for each corpus
filesWordsLine <- filesWords/filesLength

# Calculate total number of characters each corpus
filesTotalChars <- sapply(list(blogsCorpus,newsCorpus,twitterCorpus), function(x){sum(nchar(x))})

# Calculate average word length (in chars) for each corpus
filesAvgWordChar <- filesTotalChars/filesWords

# Create data frame with previous figures
dataSummary <- data.frame(
  Channel = c("Blogs","News","Twitter"),
  Size = filesSizes,
  nLines = filesLength,
  nWords = filesWords,
  nWordsLine = filesWordsLine,
  tChars = filesTotalChars,
  avgCharsWord = filesAvgWordChar
)

# Avoid Scientific Notation
options("scipen" = 100, "digits" = 4)

# Function to transform big numbers into 100K or 100M alike chars
f2si2 <- function(number,rounding = F)
{
  lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06,
           0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21,
           1e+24)
  pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "k",
           "M", "G", "T", "P", "E", "Z", "Y")
  ix <- findInterval(number, lut)
  if (lut[ix] != 1) {
    if (rounding == T) {
      sistring <- paste(round(number/lut[ix]), pre[ix])
    }
    else {
      sistring <- paste(number/lut[ix], pre[ix])
    }
  }
  else {
    sistring <- as.character(number)
  }
  return(sistring)
}

# plot dataSummary (4 plots in 2 rows. 2 cols)
par(mfrow = c(2,2))
plot1 <- barplot(dataSummary$nLines,
                 names.arg = dataSummary$Channel,
                 col = c("orange","lightgreen","cyan"), yaxt = "n",
                 main = "Total Number of Lines of Text")
text(x = plot1, y = max(dataSummary$nLines)*0.05, label = f2si2(dataSummary$nLines, T), pos = 3, cex = 0.8, col = "grey5")
plot2 <- barplot(dataSummary$nWords,
                 names.arg = dataSummary$Channel,
                 col = c("orange","lightgreen","cyan"), yaxt = "n",
                 main = "Total Number of Words")
text(x = plot2, y = max(dataSummary$nWords)*0.05, label = f2si2(dataSummary$nWords, T), pos = 3, cex = 0.8, col = "grey5")
plot3 <- barplot(dataSummary$tChars,
                 names.arg = dataSummary$Channel,
                 col = c("orange","lightgreen","cyan"), yaxt = "n",
                 main = "Total Number of Characters")
text(x = plot3, y = max(dataSummary$tChars)*0.05, label = f2si2(dataSummary$tChars, T), pos = 3, cex = 0.8, col = "grey5")
plot4 <- barplot(dataSummary$avgCharsWord,
                 names.arg = dataSummary$Channel,
                 col = c("orange","lightgreen","cyan"), yaxt = "n",
                 main = "Average Number of Characters per Word")
text(x = plot4, y = max(dataSummary$avgCharsWord)*0.05, label = round(dataSummary$avgCharsWord,2), pos = 3, cex = 0.8, col = "grey5")


# Only needed for Markdown
colnames(dataSummary) <- c("Channels", "Size in bytes", "Number of lines", "Number of Words", "Number of Words per line", "Total Characters", "Average Characters per Word")
kable(dataSummary, format = "markdown", caption = "Summary of Channels")


# Make subsampling reproducible
set.seed(1978)

# Getting only 1% of the data from each corpus
corpusSample <- function(corpus, sampleSize)
{
  return(corpus[as.logical(rbinom(length(corpus),1,sampleSize))])
}

sampleCorpus <- c(corpusSample(blogsCorpus, 0.01),
                  corpusSample(newsCorpus, 0.01),
                  corpusSample(twitterCorpus, 0.01))

# Remove Non ASCII characters
for (i in 1:length(sampleCorpus)) {
  line <- sampleCorpus[i]
  line_temp <- iconv(line, "latin1", "ASCII", sub = "")
  sampleCorpus[i] <- line_temp
}

# Create "real" corpus and do some cleansing
sampleCorpus <- VCorpus(VectorSource(sampleCorpus))
sampleCorpus <- tm_map(sampleCorpus, content_transformer(tolower))
sampleCorpus <- tm_map(sampleCorpus,  stripWhitespace)
sampleCorpus <- tm_map(sampleCorpus, removePunctuation)


# Convert corpus to dataframe for easier tokenization
sampleCorpus.df <- data.frame(lineContent = unlist(sapply(sampleCorpus, '[', 'content')), stringsAsFactors = F)

# Function for creating a NGram data.frame with frequencies
freqNGram <- function(corpus, ngrams, finalDfName)
{
  token <- NGramTokenizer(corpus, Weka_control(min = ngrams, max = ngrams))
  temp <- data.frame(table(token))
  temp2 <- temp[order(temp$Freq, decreasing = TRUE), ]
  names(temp2) <- c("tokens", "freq")
  assign(finalDfName, temp2, envir = .GlobalEnv)
}

## Create frequency "tables" for mono, bi and trigram combinations of sampleCorpus.df
freqNGram(corpus = sampleCorpus.df, ngrams = 1, finalDfName = "monoGramFreq.df")
freqNGram(corpus = sampleCorpus.df, ngrams = 2, finalDfName = "biGramFreq.df")
freqNGram(corpus = sampleCorpus.df, ngrams = 3, finalDfName = "triGramFreq.df")

## Cut frequencies to better plot frequency distribution
monoGramFreq.df$cat1 <- cut(monoGramFreq.df$freq, c(0,1,5,10,100,max(monoGramFreq.df$freq)), labels = c("1","2-5","6-10","10-100","more than 100"))
biGramFreq.df$cat1 <- cut(biGramFreq.df$freq, c(0,1,5,10,100,max(biGramFreq.df$freq)), labels = c("1","2-5","6-10","10-100","more than 100"))
triGramFreq.df$cat1 <- cut(triGramFreq.df$freq, c(0,1,5,10,100,max(triGramFreq.df$freq)), labels = c("1","2-5","6-10","10-100","more than 100"))

unigramTable <- prop.table(table(monoGramFreq.df$cat1))
bigramTable <- prop.table(table(biGramFreq.df$cat1))
trigramTable <- prop.table(table(triGramFreq.df$cat1))
unigramTable2 <- table(monoGramFreq.df$cat1)
bigramTable2 <- table(biGramFreq.df$cat1)
trigramTable2 <- table(triGramFreq.df$cat1)

vectorMono <- head(monoGramFreq.df$freq,6)
names(vectorMono) <- head(monoGramFreq.df$tokens,6)
vectorBi <- head(biGramFreq.df$freq,6)
names(vectorBi) <- head(biGramFreq.df$tokens,6)
vectorTri <- head(triGramFreq.df$freq,6)
names(vectorTri) <- head(triGramFreq.df$tokens,6)

## Plot most frequent tokens and "histograms"
par(mfrow = c(1,2))

# monograms
plot5 <- barplot(unigramTable,col="orange",main="Histogram",xlab="x - Times the word appears in corpus",ylab="f(x)",yaxs="i",xaxs="i")
text(x = plot5, y = max(unigramTable)*0.01, label = unigramTable2, pos = 3, cex = 0.8, col = "grey5")
barplot(vectorMono)

# bigrams
plot6 <- barplot(bigramTable,col="orange",main="Histogram",xlab="x - Times the 2-word combination appears in corpus",ylab="f(x)",yaxs="i",xaxs="i")
text(x = plot6, y = max(bigramTable)*0.01, label = bigramTable2, pos = 3, cex = 0.8, col = "grey5")
barplot(vectorBi)

# trigrams
plot7 <- barplot(trigramTable,col="orange",main="Histogram",xlab="x - Times the 3-word combination appears in corpus",ylab="f(x)",yaxs="i",xaxs="i")
text(x = plot7, y = max(trigramTable)*0.01, label = trigramTable2, pos = 3, cex = 0.8, col = "grey5")
barplot(vectorTri)

save(blogsCorpus,newsCorpus,twitterCorpus,unigramTable,unigramTable2,bigramTable,bigramTable2,trigramTable,trigramTable2,vectorMono,vectorBi,vectorTri, file="ngrams.RData")
