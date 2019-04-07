#List Of Packages Used
library(RColorBrewer) 
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)
library(SnowballC)
library(ggplot2)
library(maps)

consumerKey <- "uo1ePWepjk6H8pyot94FM0YAD"
consumerSecret <- "LU68cuSyaOQ21G6WQ9Lasrgrb6INurIbNsJkvJ0wRl9N8YTMmF"
accessToken <- "901448980038537217-lqq9UnN2DMW4fIfYKw0oNDcDdoZz2Q8"
accessTokenSecret <- "iiTn3MTYNGLt4B7knQtivzoFPQWxbWoXDaMc2u6vFEvAH"

requestURL<- "https://api.twitter.com/oauth/request_token"
accessURL<- "https://api.twitter.com/oauth/access_token"
authURL<- "https://api.twitter.com/oauth/authorize"

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

#search TWitter
users<- searchTwitteR("#modi OR #narendramodi -filter:retweets", resultType="mixed",n=10000, lang="en",since='2019-04-01', until='2019-04-07')
#Converting into Dataframe 
tweet.df = do.call("rbind",lapply(users,as.data.frame))


#Plotting data on map
map('world')
points(tweet.df$longitude,tweet.df$latitude, pch=20, cex=1, col="red")

#Viewing the data
View(tweet.df)

#Reading sentiment analysis data from Txt document
pos.words = scan('./positive-words.txt', what='character', comment.char=';')
neg.words = scan('./negative-words.txt', what='character', comment.char=';')

#Appending some more words to actual words
pos.words = c(pos.words, 'new','nice' ,'good', 'horizon')
neg.words = c(neg.words, 'wtf', 'behind','feels', 'ugly', 'back','worse' , 'shitty', 'bad', 'no','freaking','sucks','horrible')

#converting Into dataFrame
test <-ldply(users,function(t)t$toDataFrame())

#calcuating result
result <- score.sentiment(test$text,pos.words,neg.words)

#summarlizing data
summary(result$score)

#Histogram
hist(result$score,col="darkmagenta", main="Score of tweets",ylab=" No. of tweets",xlab="Score",xlim=c(-5,5))

#Count No of Tweets
count(result$score)

#ploting a percentage pie chart.
sentinum <- c(sum(result$score<0),sum(result$score>0),sum(result$score==0))
piepercent<- round(100*sentinum/sum(sentinum), 1)
pie(sentinum, labels = piepercent, main = "Sentiment pie chart",col = rainbow(length(sentinum)))
legend("topright", c("Negative","Positive","Neutral"), cex = 0.8, fill = rainbow(length(sentinum)))

#score Sentiment function 
#Used to remove all unwanted data 
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
    require(plyr)
    require(stringr)
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}

    
dev.new(width=10, height=10)





