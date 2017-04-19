#install.packages("rvest")
#install.packages('NLP')
#install.packages('Rstem')
#install.packages('SnowballC')
#install.packages('ROAuth')
#install.packages('RCurl')
#install.packages('stringr')
#install.packages('tm')
#install.packages('ggmap')
#install.packages('dplyr')
#install.packages('plyr')
#install.packages('wordcloud')
#install.packages('glmnet')
#install.packages('e1071')
#install.packages('RWeka')
#install.packages('XML')

library(XML)
library(NLP)
library(Rstem)
library(SnowballC)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(wordcloud)
library(glmnet)
library(e1071)
library(rvest)

  
ID <- 'tt0773262'
for (i in seq(0, 100, by=10)){
URL <- paste0("http://www.imdb.com/title/", ID, "/reviews?start=",i)
htmlpage <- read_html(URL)
ex_review1 <- htmlpage %>% html_nodes("#pagecontent") %>%
  html_nodes("div+ p") %>%
  html_text()

ex_review <- c(ex_review,ex_review1)
}

#write.csv(ex_review, file ="IMDB_GOT.csv", row.names=FALSE)

set.seed(100)
ex_review <- sample(ex_review,100)

ex_review <- gsub("[\r\n]", " ", ex_review) # replace line breaks
ex_review <- gsub("u0096", "", ex_review) # replace garbage
ex_review <- gsub("/", "", ex_review) # replace '/' character  
ex_review <- gsub("'\'", "", ex_review) # replace '\' character

corpus=Corpus(VectorSource(ex_review))

# Convert to lower-case
corpus <- tm_map(corpus,tolower)

# Remove numbers
corpus <- tm_map(corpus,removeNumbers)


# Remove stopwords
corpus <- tm_map(corpus,removeWords,stopwords("english"))
corpus[[10]]


#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)


#Strip-off white spaces
corpus <- tm_map(corpus,stripWhitespace)


#Stem the Corpus
corpus <- tm_map(corpus,stemDocument,language="english")
corpus[[1]]

# Convert corpus to plain text document
corpus_clean <- tm_map(corpus, PlainTextDocument)

#Transform Corpus to Document Term Matrix
corpus_dtm <- DocumentTermMatrix(corpus_clean)

rmspa_corpus = removeSparseTerms(corpus_dtm,0.80)
dim(rmspa_corpus)

# show the average frequency of the top 20 most frequent words

mean_corpus <- sort(colMeans(as.matrix(rmspa_corpus)),decreasing = T)
mean_corpus[1:20]

#Barplot of the top 20 words vs frequency
barplot(mean_corpus[1:20],border = NA, las=3,xlab = "top 20 words",ylab = "Frequency",ylim = c(0,3))


#Compare average frequencyif zeros are excluded from the matrix
rmspa_corpus_nozero <- as.matrix(rmspa_corpus)
is.na(rmspa_corpus_nozero) <- rmspa_corpus_nozero==0
mean_corpus_nonzero = sort(colMeans(rmspa_corpus_nozero,na.rm=TRUE),decreasing = T)
mean_corpus_nonzero[1:20]

barplot(mean_corpus_nonzero[1:20],border = NA, las=3,xlab = "top 20 words",ylab = "Frequency",ylim = c(0,4))

#Removing non-interesting words

mystopwords <- c("one","two","hbo","series","well","episode","better")
corpus_clean2 <- tm_map(corpus,removeWords,mystopwords)
corpus_clean2 <- tm_map(corpus_clean2, PlainTextDocument)

corpus_dtm2 <- DocumentTermMatrix(corpus_clean2,control=list(tolower=T,removeNumbers=T,removePunctuation=T,stopwords=T,stripWhitespace=T,stemDocument=T))
rmspa_corpus2 = removeSparseTerms(corpus_dtm2,0.80)
dim(rmspa_corpus2)


# show the average frequency of the top 20 most frequent words
mean_corpus2 <- sort(colMeans(as.matrix(rmspa_corpus2)),decreasing = T)
mean_corpus2[1:20]

#Barplot of the top 20 words vs frequency
barplot(mean_corpus2[1:20],border = NA, las=3,xlab = "top 20 words",ylab = "Frequency",ylim = c(0,3))

#wordcloud of the most frequent words
wordcloud(names(mean_corpus2[1:30]),mean_corpus2[1:30],scale = c(3,1),colors = brewer.pal(8,"Dark2"))

#load up word polarity list and format it
afinn_list <- read.delim(file="C:/Users/Dhaval's Dell/Desktop/ADS/Assignment8/words.txt", header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

#categorize words as very negative to very positive and add some review-specific words

negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1 | afinn_list$score==-5 | afinn_list$score==-4], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
posTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4 | afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious","uproarious", "riveting", "fascinating", "dazzling", "legendary")

#function to calculate number of words in each category within a sentence

sentimentScore <- function(sentences, negTerms, posTerms){
  final_scores <- matrix('', 0, 3)
  scores <- laply(sentences, function(sentence, negTerms, posTerms){
    initial_sentence <- sentence
    
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    
    #build vector with matches between sentence and each category
    posMatches <- match(words, posTerms)
    negMatches <- match(words, negTerms)
    
    #sum up number of words in each category
    posMatches <- sum(!is.na(posMatches))
    negMatches <- sum(!is.na(negMatches))
    
    total <- posMatches + negMatches
    negPercent <- (negMatches/total)*100
    posPercent <- (posMatches/total)*100
    score <- c(negPercent, posPercent)
    
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, negTerms, posTerms)
  return(scores)
}    


#build tables of review with scores
posResult <- as.data.frame(sentimentScore(ex_review, negTerms, posTerms))
colnames(posResult) <- c('sentence', 'neg%', 'pos%')

posResult[,1] <- as.numeric(as.character( posResult[, 1] ))
posResult[,2] <- as.numeric(as.character( posResult[, 2] ))
posResult[,3] <- as.numeric(as.character( posResult[, 3] ))

#save the percent weightage in the dataframe df 

negPercent <- (posResult$neg)
posPercent <- (posResult$pos)
y_name <- "Negative%"
x_name <- "Positive%"

df <- data.frame(posPercent,negPercent)
names(df) <- c(x_name,y_name)
print(df)

meanPercent <- lapply(df, median, na.rm = TRUE)
write.csv(df,file='df8.csv',row.names = FALSE)

postiveAvg <- mean(!is.na(posPercent))
negativeAvg <- mean(!is.na(negPercent))

posResult1 <- subset(posResult, select = -sentence)

#Average Sentiment on the Corpus Review 

barplot(c(meanPercent$`Positive%`,meanPercent$`Negative%`),beside = TRUE, col = c("green","red")
        ,legend.text = c("Positive","Negative"),  
        ylim = c(0,100), xlab = 'Topics(Z)',ylab = 'P(z)', sub = 'Overall Sentiment')

#Sample Plot for Individual Review 1 and 2

barplot(c(df$`Positive%`[1],df$`Negative%`[1]),beside = TRUE, col = c("green","red")
        ,legend.text = c("Positive","Negative"),  
        ylim = c(0,100), xlab = 'Review 1',ylab = 'P(z)', sub = substr(ex_review[1],1,60))

barplot(c(df$`Positive%`[2],df$`Negative%`[2]),beside = TRUE, col = c("green","red")
        ,legend.text = c("Positive","Negative"),  
        ylim = c(0,100), xlab = 'Review 2',ylab = 'P(z)', sub = substr(ex_review[2],1,60))




