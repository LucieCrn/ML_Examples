################################################
### Tweets analysis about the Olympic Games ###
################################################



##########################
## 1 - Install packages ##
##########################

# Packages usefull for Twitter
install.packages("twitteR") 
install.packages("rtweet")
library(twitteR)
library(rtweet)

# Plotting and pipes - tidyverse! 
install.packages("ggplot2") 
library(ggplot2)
install.packages("dplyr")
library(dplyr)

# Text mining library 
install.packages("tidytext") 
library(tidytext)
install.packages("maps")
library(maps)
install.packages("ggmap")
library(ggmap)
install.packages("tm", dependencies=TRUE)
install.packages("stringi")
install.packages("stringr")
library("tm")
library("stringi")
library("stringr")

# To establish a secure connection with Twitter 
#install.packages(c("ROAuth","RCurl"))
#install.packages("ROAuth")
library(ROAuth)
library(RCurl)

#install.packages("base64enc")
library(base64enc)


###################################
## 2 - Connexion to extract data ##
###################################

# Certification schemes for Internet transfers
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="/Users/lena/Desktop/cacert.pem")

# Create R objects from your own consumer information
my.key<-"080X2ef1ZQ97dVFkLuoqodnZW"
my.secret<-"OZgfcXDAj6lCnSx7CwHykm7ISZdu0T4pjkEeOAkYwBaxzEAzuR"
aces_token<-"1075704418346123267-LQIMGKenrF6IsfdK07mTaapAX8QFfP"
access_secret<-"TmyXYyWW3nOqsDM4NsCreaHjqhONSuX0LvUn2hWxSKwPs"

# Authentication process
setup_twitter_oauth(my.key, my.secret,
                    aces_token, access_secret)

twitter_token = create_token(
  consumer_key = my.key,
  consumer_secret = my.secret,
  access_token = aces_token,
  access_secret = access_secret)

# Data extraction from Twitter about the Olympic Games
olympics_data = search_tweets("Olympic Games OR OLYMPIC GAMES OR OLYMPICS OR Olympics OR OLYMPICS OR olympics OR Olympic OR OLYMPIC OR olympic OR olympic games OR Beijing2022 OR OR BEIJING2022 OR BEIJING 2022 OR Beijing 2022 OR Pékin 2022 OR PEKIN 2022 OR pekin 2022 OR pekin2022 OR Pékin2022 OR Pekin2022 OR PEKIN2022 OR Winter Games OR winter games OR WINTERGAMES OR WinterGames", n=5000)
class(olympics_data)
head(olympics_data, n = 2)
summary(olympics_data)


#######################################
## 3 - Data preparation and analysis ##
#######################################

# Remove special characters in non latin langage
olympics_data$location_clean = iconv(olympics_data$location,to = "ASCII", sub="")

# Remplace empty values with "NA" in the "location_clean" column 
olympics_data$location_clean[olympics_data$location_clean==""] = NA
olympics_data$location_clean[olympics_data$location_clean==", "] = NA

### Twitter users - unique locations (using a pip)
olympics_data %>%count(location_clean, sort=TRUE) %>%
  mutate(location_clean=reorder(location_clean,n)) %>%
  na.omit()%>% top_n(10)%>%ggplot(aes(x=location_clean,y=n))+
  geom_bar(stat="identity")+geom_col()+coord_flip() +
  labs(x = "Location", y = "Count",
       title = "Twitter users - unique locations ")+
  theme_light()

# we see that there are some duplicates that we have to correct

olympics_data$location_clean[olympics_data$location_clean=="Tokyo"] = "Tokyo, Japan"
olympics_data$location_clean[olympics_data$location_clean=="JAPAN"] = "Japan"
olympics_data$location_clean[olympics_data$location_clean=="USA"] = "United States"
olympics_data$location_clean[olympics_data$location_clean=="Beijing"] = "Beijing, China"


### Type of tweets

# Reeking only the organic tweets (without retweets and replies)
olympics_data_organic <- olympics_data[olympics_data$is_retweet==FALSE, ] 
olympics_data_organic <- subset(olympics_data_organic, is.na(olympics_data_organic$reply_to_status_id)) 

olympics_data_organic = olympics_data_organic %>% arrange(-favorite_count)
olympics_data_organic[1,5]
olympics_data_organic = olympics_data_organic %>% arrange(-retweet_count)
olympics_data_organic[1,5]

# Keeping only the retweets
olympics_data_retweets = olympics_data[olympics_data$is_retweet==TRUE,]

# Keeping only the replies
olympics_data_replies = subset(olympics_data, !is.na(olympics_data$reply_to_status_id))

# Creating a data frame (numbers have to be changed according to the number of organic tweets, retweets and replies)
data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(951, 141, 3777)
)

# Adding columns 
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))

Type_of_Tweet <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

### Plot time series of tweets 
ts_plot(olympics_data, "hours")+
  ggplot2::theme_minimal()+
  ggplot2::theme(plot.title=ggplot2::element_text(face="bold"))+
  ggplot2::labs(x=NULL,y=NULL,
                title="Frequency of tweets about the Olympics games",
                subtitle="Twitter status counts 1-hour intervals",
                caption="\nSource: Data collected from Twitter's API"
  )


 # Corpus creation and data cleaning
# Removing special characters in non latin language
usableText <- iconv(olympics_data$text, to = "ASCII", sub="")
olympics_data_corpus<-Corpus(VectorSource(usableText))
olympics_data_corpus<-tm_map(olympics_data_corpus,tolower)
olympics_data_corpus<-tm_map(olympics_data_corpus, removePunctuation)
olympics_data_corpus<-tm_map(olympics_data_corpus,removeNumbers)
olympics_data_corpus<-tm_map(olympics_data_corpus,  function(x)removeWords(x, stopwords()))

olympics_data_corpus<-tm_map(olympics_data_corpus, function(x)removeWords(x,
                                                                    stopwords("french")))
olympics_data_corpus<-tm_map(olympics_data_corpus, function(x)removeWords(x,
                                                                    stopwords("italian")))
olympics_data_corpus<-tm_map(olympics_data_corpus, function(x)removeWords(x,
                                                                    stopwords("spanish")))

# We remove URL
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
olympics_data_corpus <- tm_map(olympics_data_corpus, content_transformer(removeURL))

# We remove some evident word that we don't want to do for analysis
olympics_data_corpus <- tm_map(olympics_data_corpus, removeWords, c("beijing", "games", "winter", "olympics", "olympic"))


text_corpus <- tm_map(olympics_data_corpus,
                      content_transformer(function(x) iconv(x,to='ASCII',sub='byte')))

# The document-term matrix
olympics_data.tdm <- TermDocumentMatrix(text_corpus)
m <- as.matrix(olympics_data.tdm)
m[1:5,1:10]

# Remove sparse terms from the term-document matrix
olympics_data.tdm<-removeSparseTerms(olympics_data.tdm, sparse=0.95)

######################
### Terms analysis ###
######################

#install.packages("wordcloud")
library("wordcloud")

# Most frequent terms on our matrix
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Plot of words frequencies
barplot(d[1:20,]$freq, las = 3,
        names.arg = d[1:20,]$word,col ="lightblue",
        main ="Most frequent words",
        ylab = "Word frequencies")

# Terms used at least 100 times
findFreqTerms(olympics_data.tdm, lowfreq=100)[1:10]

# Wordcloud of the most used words 
wordcloud(words = d$word, freq = d$freq, min.freq = 40, max.words=100, random.order=FALSE,colors=brewer.pal(4,"Dark2"))

# Convert the term-document matrix to a data frame
olympics_data.df <- as.data.frame(as.matrix(olympics_data.tdm))

# Scale the data 
olympics_data.df.scale <- scale(olympics_data.df)

# Create the distance matrix
olympics_data.dist <- dist(olympics_data.df.scale, method = "euclidean")

# Cluster the data: tweets are grouped into classes
olympics_data.fit<-hclust(olympics_data.dist, method="ward.D2")

# Visualize the result 
plot(olympics_data.fit, main="Cluster-Olympic-Games")

# Plotting clusters
groups <- cutree(olympics_data.fit, k=5)
plot(olympics_data.fit, main="Cluster-Macron")
rect.hclust(olympics_data.fit, k=5, border="blue")

#########################
### Hashtags analysis ###
#########################

#Wordcloud of the hashtags to most cited
olympics_data$hashtags <- as.character(olympics_data$hashtags)
olympics_data$hashtags <- gsub("c\\(", "", olympics_data$hashtags)
set.seed(1234)
wordcloud(olympics_data$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#############################
### Hashtags relationship ###
#############################

# Define a tag extractor function
tags<-function(x) toupper(grep("#",strsplit(x,
                                            " +")[[1]],value=TRUE))

# create a list of tag sets for each tweet
l <- nrow(olympics_data)
taglist <- vector(mode = "list", l)

# Create an empty vector to store the tweet texts
texts <- vector(mode = "character", length = l)

# Extract the tweet text from each tweet status
for (i in 1:l) texts[i] <- olympics_data$text[i]
texts <- iconv(texts, to = "ASCII", sub="")
# ... and populate it
j<-0
for(i in 1:l){
  if(is.na(str_match(texts[i],"#"))[1,1]==FALSE){
    j<-j+1
    taglist[[j]]<-str_squish(removePunctuation(tags(ifelse(is.na(str_match(texts[i],                                                               "[\n]")[1,1])==TRUE,texts[i],gsub("[\n]"," ",texts[i])))))
  }
}
alltags <- NULL
for (i in 1:l) alltags<-union(alltags,taglist[[i]])

# Create an empty graph
library(igraph)
hash.graph <- graph.empty(directed = T)

# Populate it with nodes
hash.graph <- hash.graph + vertices(alltags)

# Populate it with edges
for (tags in taglist){
  if (length(tags)>1){
    for (pair in combn(length(tags),2,simplify=FALSE,
                       FUN=function(x) sort(tags[x]))){
      if (pair[1]!=pair[2]) {
        if (hash.graph[pair[1],pair[2]]==0)
          hash.graph<-hash.graph+edge(pair[1],
                                      pair[2])
      }
    }
  }
}

# Network construction
V(hash.graph)$color <- "black"
E(hash.graph)$color <- "black"
V(hash.graph)$name <- paste("#",V(hash.graph)$name,
                            sep = "")
V(hash.graph)$label.cex = 0.75
V(hash.graph)$size <- 20
V(hash.graph)$size2 <- 2
hash.graph_simple<-delete.vertices(simplify(hash.graph),
                                   degree(hash.graph)<=7)

# Network construction
plot(hash.graph_simple, edge.width = 2,
     edge.color = "black", vertex.color = "SkyBlue2",
     vertex.frame.color="black", label.color = "black",
     vertex.label.font=2, edge.arrow.size=0.5, alpha = 50)

########################
### Account Analysis ###
########################

#see the accounts that retweet the most

set.seed(1234)
wordcloud(olympics_data_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))


##########################
### Sentiment Analysis ###
##########################

install.packages("sentimentr")
library("sentimentr")
install.packages("syuzhet")
library(syuzhet)

plain.text<-vector()
for(i in 1:dim(olympics_data)[1]){
  plain.text[i]<-olympics_data_corpus[[i]][[1]]
}
sentence_sentiment<-sentiment(get_sentences(plain.text))
sentence_sentiment

average_sentiment<-mean(sentence_sentiment$sentiment)
average_sentiment
sd_sentiment<-sd(sentence_sentiment$sentiment)
sd_sentiment

extract_sentiment_terms(get_sentences(plain.text))


# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(olympics_data, from="UTF-8", to="ASCII", sub="")

# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
# removing mentions, in case needed
tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

# Topic modeling
install.packages("tidytext")
install.packages("topicmodels")
install.packages("tidyverse")
install.packages("rvest")
install.packages("reshape2")

library(tidytext)
library(topicmodels)
library(tidyverse)
library(rvest)
library(reshape2)
library(textdata)

text_corpus2<-text_corpus[1:200]
doc.lengths<-rowSums(as.matrix(DocumentTermMatrix(text_corpus2)))
dtm <- DocumentTermMatrix(text_corpus2[doc.lengths > 0])

# Pick a random seed for replication
SEED = sample(1:1000000, 1)

# Let's start with 2 topics
k = 3
Topics_results<-LDA(dtm, k = k, control = list(seed = SEED))

terms(Topics_results,15)

# To which topic appears each tweet
topics(Topics_results)







