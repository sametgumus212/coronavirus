library(tm)
library(ggplot2)
library(wordcloud)
library(RWeka)

#setwd("C:/Users/MyLife/Desktop/Codes")
apple <- read.csv(file.choose(), header = T)
mydata<-(apple)
#mydata <-read.csv("comments.csv", header = TRUE)
mycorpus <- Corpus(VectorSource(mydata$Title))
# Text Cleaning
# Convert the text to lower case
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
# Remove numbers
mycorpus <- tm_map(mycorpus, removeNumbers)
# Remove english common stopwords
mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
# Remove punctuations
mycorpus <- tm_map(mycorpus, removePunctuation)
# Eliminate extra white spaces
mycorpus <- tm_map(mycorpus, stripWhitespace)

as.character(mycorpus[[3]])

# Bigrams 
minfreq_bigram<-2

token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(mycorpus, Weka_control(min=2,max=2, delimiters = token_delim))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = c(2,0.35),min.freq = minfreq_bigram,colors = brewer.pal(8,"Dark2"),max.words=150)

# Trigrams 

minfreq_trigram <- 5

token_delim <- " \\t\\r\\n.!?,;\"()"
tritoken <- NGramTokenizer(mycorpus, Weka_control(min=3,max=3, delimiters = token_delim))
three_word <- data.frame(table(tritoken))
sort_three <- three_word[order(three_word$Freq,decreasing=TRUE),]
wordcloud(sort_three$tritoken, sort_three$Freq, random.order=FALSE,min.freq = minfreq_trigram,scale = c(1.2,0.35),colors = brewer.pal(8,"Dark2"),max.words=150)
