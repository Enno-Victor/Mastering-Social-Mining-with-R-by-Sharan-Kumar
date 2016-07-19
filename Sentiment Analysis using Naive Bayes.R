library(twitteR)

consumer_key = "tAjU2upI4LXGS4LfgtCse7qIi"
consumer_secret = "ngpspBVlNzcA0poW43ntF0zcYSESCuq5NFbPvvVi20bu1O2u4s" 
access_token = "714952412293906437-765iDZGtLnyW84S0EMwTle2cPub2TpO"
access_secret = "5HqVIrSiL2b1jqvkA7e2dCDNLfWsMk0q0WASc2vHTvvep"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#============================================================================================================

## Collecting tweets as a corpus

Meru_tweets = searchTwitter("MeruCabs", n=2000, lang="en")
Ola_tweets = searchTwitter("OlaCabs", n=2000, lang="en")
TaxiForSure_tweets = searchTwitter("TaxiForSure", n=2000, lang="en")
Uber_tweets = searchTwitter("Uber_Delhi", n=2000, lang="en")


length(Meru_tweets)
length(Ola_tweets)
length(TaxiForSure_tweets)
length(Uber_tweets)

 
## Cleaning the corpus
head(Meru_tweets)

MeruTweets <- sapply(Meru_tweets, function(x) x$getText())
OlaTweets = sapply(Ola_tweets, function(x) x$getText())
TaxiForSureTweets = sapply(TaxiForSure_tweets, function(x) x$getText())
UberTweets = sapply(Uber_tweets, function(x) x$getText())



catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # Try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}


cleanTweets<- function(tweet){
  # Clean the tweet for sentiment analysis
  #  remove html links, which are not required for sentiment analysis
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  # First we will remove retweet entities from the stored tweets (text)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Then remove all "#Hashtag"
  tweet = gsub("#\\w+", " ", tweet)
  # Then remove all "@people"
  tweet = gsub("@\\w+", " ", tweet)
  # Then remove all the punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Then remove numbers, we need only text for analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  # if anything else, you feel, should be removed, you can. For example "slang words" etc using the above function and methods.
  # Next we'll convert all the word in lower case. This makes uniform pattern.
  tweet = catch.error(tweet)
  tweet
}

cleanTweetsAndRemoveNAs<- function(Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  # Remove the "NA" tweets from this tweet list
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  # Remove the repetitive tweets from this tweet list
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}


MeruTweetsCleaned = cleanTweetsAndRemoveNAs(MeruTweets)
OlaTweetsCleaned = cleanTweetsAndRemoveNAs(OlaTweets)
TaxiForSureTweetsCleaned <- cleanTweetsAndRemoveNAs(TaxiForSureTweets)
UberTweetsCleaned = cleanTweetsAndRemoveNAs(UberTweets)

#size of each of the cleaned tweet lists
length(MeruTweetsCleaned)
length(OlaTweetsCleaned)
length(TaxiForSureTweetsCleaned)
length(UberTweetsCleaned)

#=====================================================================================================

#Naive Bayes 

install.packages("Rstem_0.4-1.tar.gz" , repos=NULL, type="source")
library(devtools)
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
library(sentiment)
ls("package:sentiment")

# classify_emotion function returns an object of class data frame # with seven columns (anger, disgust, fear, joy, sadness, surprise,  # # best_fit) and one row for each document:
MeruTweetsClassEmo = classify_emotion(MeruTweetsCleaned, algorithm="bayes", prior=1.0)
OlaTweetsClassEmo = classify_emotion(OlaTweetsCleaned, algorithm="bayes", prior=1.0)
TaxiForSureTweetsClassEmo = classify_emotion(TaxiForSureTweetsCleaned, algorithm="bayes", prior=1.0)
UberTweetsClassEmo = classify_emotion(UberTweetsCleaned, algorithm="bayes", prior=1.0)

head(MeruTweetsClassEmo,20)

# substituting these NA values with the word unknown to make further analysis easier

MeruEmotion = MeruTweetsClassEmo[,7]
OlaEmotion = OlaTweetsClassEmo[,7]
TaxiForSureEmotion = TaxiForSureTweetsClassEmo[,7]
UberEmotion = UberTweetsClassEmo[,7]

MeruEmotion[is.na(MeruEmotion)] = "unknown"
OlaEmotion[is.na(OlaEmotion)] = "unknown"
TaxiForSureEmotion[is.na(TaxiForSureEmotion)] = "unknown"
UberEmotion[is.na(UberEmotion)] = "unknown"

head(MeruEmotion,20)


#Using Classify_polarity function to identify positive, negative and neutral tweets
MeruTweetsClassPol = classify_polarity(MeruTweetsCleaned, algorithm="bayes")
OlaTweetsClassPol = classify_polarity(OlaTweetsCleaned, algorithm="bayes")
TaxiForSureTweetsClassPol = classify_polarity(TaxiForSureTweetsCleaned, algorithm="bayes")
UberTweetsClassPol = classify_polarity(UberTweetsCleaned, algorithm="bayes")

head(MeruTweetsClassPol,20)

# we will fetch polarity category best_fit for our analysis purposes,
MeruPol = MeruTweetsClassPol[,4]
OlaPol = OlaTweetsClassPol[,4]
TaxiForSurePol = TaxiForSureTweetsClassPol[,4]
UberPol = UberTweetsClassPol[,4]

# Let us now create a data frame with the above results
MeruSentimentDataFrame = data.frame(text=MeruTweetsCleaned, emotion=MeruEmotion, polarity=MeruPol, stringsAsFactors=FALSE)
OlaSentimentDataFrame = data.frame(text=OlaTweetsCleaned, emotion=OlaEmotion, polarity=OlaPol, stringsAsFactors=FALSE)
TaxiForSureSentimentDataFrame = data.frame(text=TaxiForSureTweetsCleaned, emotion=TaxiForSureEmotion, polarity=TaxiForSurePol, stringsAsFactors=FALSE)
UberSentimentDataFrame = data.frame(text=UberTweetsCleaned, emotion=UberEmotion, polarity=UberPol, stringsAsFactors=FALSE)

# rearrange data inside the frame by sorting it
MeruSentimentDataFrame = within(MeruSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
OlaSentimentDataFrame = within(OlaSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
TaxiForSureSentimentDataFrame = within(TaxiForSureSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
UberSentimentDataFrame = within(UberSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
plotSentiments1<- function (sentiment_dataframe,title) {
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette="Dark2") +
    ggtitle(title) +
    theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')
}

plotSentiments1(MeruSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about MeruCabs')
plotSentiments1(OlaSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about OlaCabs')
plotSentiments1(TaxiForSureSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about TaxiForSure')
plotSentiments1(UberSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about UberIndia')

head(MeruSentimentDataFrame,20)


# Similarly we will plot distribution of polarity in the tweets
plotSentiments2 <- function (sentiment_dataframe,title) {
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle(title) +
    theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories')
}

plotSentiments2(MeruSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about MeruCabs')
plotSentiments2(OlaSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about OlaCabs')
plotSentiments2(TaxiForSureSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about TaxiForSure')
plotSentiments2(UberSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about UberIndia')

#Word CLoud 
removeCustomeWords <- function (TweetsCleaned) {
  for(i in 1:length(TweetsCleaned)){
    TweetsCleaned[i] <- tryCatch({
      TweetsCleaned[i] =  removeWords(TweetsCleaned[i], c(stopwords("english"), "care", "guys", "can", "dis", "didn", "guy" ,"booked", "plz"))
      TweetsCleaned[i]
    }, error=function(cond) {
      TweetsCleaned[i]
    }, warning=function(cond) {
      TweetsCleaned[i]
    })
  }
  return(TweetsCleaned)
}

getWordCloud <- function (sentiment_dataframe, TweetsCleaned, Emotion) {
  emos = levels(factor(sentiment_dataframe$emotion))
  n_emos = length(emos)
  emo.docs = rep("", n_emos)
  TweetsCleaned = removeCustomeWords(TweetsCleaned)
  
  for (i in 1:n_emos){
    emo.docs[i] = paste(TweetsCleaned[Emotion == emos[i]], collapse=" ")
  }
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
  require(wordcloud)
  suppressWarnings(comparison.cloud(tdm, colors = brewer.pal(n_emos, "Dark2"),  scale = c(3,.5), random.order = FALSE, title.size = 1.5))
}
getWordCloud(MeruSentimentDataFrame, MeruTweetsCleaned, MeruEmotion)
getWordCloud(OlaSentimentDataFrame, OlaTweetsCleaned, OlaEmotion)
getWordCloud(TaxiForSureSentimentDataFrame, TaxiForSureTweetsCleaned, TaxiForSureEmotion)
getWordCloud(UberSentimentDataFrame, UberTweetsCleaned, UberEmotion)
