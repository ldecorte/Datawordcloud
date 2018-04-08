options(stringsAsFactors = FALSE)
setwd("/set your directory here/")
library(tm)
library(readr)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

#set location o files
file <- 'insert pdf address here'

#create function named Rpdf to read PDFs files
Rpdf <- readPDF(control = list(text = "-layout"))(elem = list(uri = file),
                                                  language = "en",
                                                  id = "id1")

Rpdf <- readLines(filetxt) # don't mind warning..

Rpdf <- tolower(Rpdf)
Rpdf <- removeWords(Rpdf, c("\\f", stopwords()))
# Remove numbers
Rpdf <- tm_map(Rpdf, removeNumbers)
# specify your stopwords as a character vector
#Rpdf <- tm_map(Rpdf, removeWords, c("word1", "word2")) 


corpus <- Corpus(VectorSource(Rpdf))
corpus <- tm_map(corpus, removePunctuation)
tdm <- TermDocumentMatrix(corpus)

#create matrix of words
m <- as.matrix(tdm)
#d is frequency in decreasing value
d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))

# Stem words
d$stem <- wordStem(row.names(d), language = "english")

# and put words to column, otherwise they would be lost when aggregating
d$word <- row.names(d)

# aggregate freqeuncy by word stem and
# keep first words..
agg_freq <- aggregate(freq ~ stem, data = d, sum)
agg_word <- aggregate(word ~ stem, data = d, function(x) x[1])

d <- cbind(freq = agg_freq[, 2], agg_word)

# sort by frequency
d <- d[order(d$freq, decreasing = T), ]

# print wordcloud:
wordcloud(d$word, d$freq)

write.csv(m, file="DocumentTermMatrix.csv") 
write.csv(d, file="DocumentTermFreq.csv") 

# remove files
#file.remove(dir(tempdir(), full.name=T)) # remove files


