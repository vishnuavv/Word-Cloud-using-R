library(ggplot2)
library(treemap)

ds = read.csv("loan.csv")
summary(ds)

# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Reading text from data frame
text = ds$title[ds$loan_amnt < 1000] 

# We make a "text corpus" out of the text
docs = Corpus(VectorSource(text))
head(docs)
inspect(docs)

# Replace any special characters with " "
toSpace = content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs = tm_map(docs, toSpace, "/")
docs = tm_map(docs, toSpace, "@")
docs = tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs = tm_map(docs, content_transformer(tolower))
# Remove numbers
docs = tm_map(docs, removeNumbers)
# Remove english common stopwords
docs = tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Build a term matrix
dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)
head(d, 10)

# Build the word cloud
set.seed(1234)
wc <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

text(x=0.5, y=0.1, "Word Cloud of Loan Title with loan amount less than $1,000",cex=1.5,pos=1)