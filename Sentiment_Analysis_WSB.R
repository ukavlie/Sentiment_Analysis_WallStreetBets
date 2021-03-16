# setting working directory
setwd("C:/Users/ukavl/OneDrive/Skrivebord/Hult/02_MsBA/08_Text_Analytics/hult_NLP_student/cases/session II/WallStreetBets")



# Options
options(stringsAsFactors = FALSE, scipen = 999)
Sys.setlocale("LC_ALL","C")


# Libraries
library(pacman)
pacman::p_load(qdap,politeness,tm,lubridate,ggplot2,RTextTools,yardstick,wordcloud,stringr,stringi,
               ggthemes,ggdendro,RColorBrewer,pbapply,plotrix,ggalt,rtweet,tidyverse,quanteda,tidyr,tidytext,lexicon,echarts4r,zoo,scales)


# Functions

# BIGRAM FUNCTION
bigramTokens = function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# TRIGRAM FUNCTION
trigramTokens = function(x){
  unlist(lapply(NLP::ngrams(words(x), 3), paste, collapse = " "), 
         use.names = FALSE)
}

# REPLACE PUNCTUATION INSTEAD OF REMOVE
replacePunctuation = content_transformer(function(x){
  return (gsub("[[:punct:]]"," ", x))})

# CLEAN CORPUS FUNCTION WITH REPLACE INSTEAD OF REMOVE
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(qdap::replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, replacePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# TRY TO LOWER
tryTolower = function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}



# Importing data

# FILE, message = FALSE
wsb_comment = readr::read_csv("CASE_gme.csv")
gme_price   = read.csv("gme_HLOC.csv")


# EDA

# Quick Look at the Structure of Data
head(wsb_comment)


# Table of comments per date
timeframe = as.data.frame(table(wsb_comment$comm_date))
colnames(timeframe) = c("date","comments")
tail(timeframe)


# Table of post title and number of comments
post_comment = as.data.frame(table(wsb_comment$title))
post_comment = post_comment[order(post_comment$Freq, decreasing = TRUE),]
colnames(post_comment) = c("title","comments")
rownames(post_comment) = NULL
head(post_comment, 10)


# Replacing Wierd Values
wsb_comment$comment   = gsub("[^\x20-\x7E]", " ", wsb_comment$comment)
wsb_comment$comment   = gsub("moo*oo", "moon", wsb_comment$comment, ignore.case = TRUE)
wsb_comment$comment   = gsub("hoo*ool", "hold", wsb_comment$comment, ignore.case = TRUE)
wsb_comment$title     = gsub("[^\x20-\x7E]", " ", wsb_comment$title)
wsb_comment$post_text = gsub("[^\x20-\x7E]", " ", wsb_comment$post_text)


# Extracting written text

# filtering on structure to get only original post
wsb_posts = wsb_comment %>% 
  filter(structure == 1)

# combining title and post text
wsb_posts$text = paste(wsb_posts$title, wsb_posts$post_text)

# creating new date column
wsb_posts$date = wsb_posts$post_date

# Comment text and dates
wsb_comment$text = wsb_comment$comment

wsb_comment$date = wsb_comment$comm_date

# merging the two datasets
wsb_comment = rbind(wsb_comment, wsb_posts)

comment = wsb_comment$text



# Creating and Cleaning Corpus

# declearing stopwords
stops   = stopwords("smart")

# creating vcorpus
comment_corpus = VCorpus(VectorSource(wsb_comment$text))

# cleaning corpus
comment_corpus = cleanCorpus(comment_corpus, stops)

# document term matrix to ensure that dates can be attached again
comment_DTM    = DocumentTermMatrix(comment_corpus)

# as matrix
comment_DTMm   = as.matrix(comment_DTM)


# Visual EDA

# Frequency Plot

# summing up all columns
topTerms = colSums(comment_DTMm)

# table
topTerms = data.frame(terms = colnames(comment_DTMm), freq = topTerms)

# resetting rownames
rownames(topTerms) = NULL

# subsetting on 550 to get the highest used
topWords = subset(topTerms, topTerms$freq > 550)

# ordering in increasing order
topWords = topWords[order(topWords$freq, decreasing = FALSE),]


topWords$terms = factor(topWords$terms,
                        levels = unique(as.character(topWords$terms)))

# Plotting the data
ggplot(topWords, aes(x = terms, y = freq)) + 
  geom_bar(stat = "identity", fill = "darkblue") + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label = freq), colour = "white", hjust = 1.25, size = 3.0)+
  labs(x = "Term", y = "Frequency")


# Sampling Data for Bigram and Trigram
set.seed(1234)

# sampling size
ind = sample(1:nrow(wsb_comment), 0.3*nrow(wsb_comment))
bi_tri_data = wsb_comment$text[ind]

# creating and cleaning corpus
bitri_corpus = VCorpus(VectorSource(bi_tri_data))
bitri_corpus = cleanCorpus(bitri_corpus, stops)


# Bigram

# creating bigram document
bi_TDM = TermDocumentMatrix(bitri_corpus, 
                            control=list(tokenize=bigramTokens))

# transformng into matrix
bi_TDMm = as.matrix(bi_TDM)

# sorting the matrix
bi_TDMm = sort(rowSums(bi_TDMm), decreasing = TRUE)
bi_DF = data.frame(word = names(bi_TDMm), freq = bi_TDMm)

# deciding colour palet
pal = brewer.pal(8, "YlOrRd")

# Removing brightest colours 
pal = pal[-(1:2)]

# displaying plot
set.seed(1234)
wordcloud(bi_DF$word,
          bi_DF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,0.5))


# Association Plot With hedge fund

# Declearing associations with melvn capital
association = findAssocs(bi_TDM, "melvin capital", 0.86)

# Creating a dataframe
assocDF = data.frame(terms = names(association[[1]]),
                     value = unlist(association))
assocDF$terms = factor(assocDF$terms, levels = assocDF$terms)

# Removing row names
rownames(assocDF) = NULL

# sampling because to many similar values
ind = sample(1:nrow(assocDF), 0.2*nrow(assocDF))
assocDF = assocDF[ind,]

ggplot(assocDF, aes(y = terms)) +
  geom_point(aes(x = value), data = assocDF, col = '#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x = value,label = value), colour = "red",
            hjust = "inward", vjust = "inward" , size = 3)+
  labs(y = "Term", x = "Association")


# Trigram

# creating bigram document
tri_TDM = TermDocumentMatrix(bitri_corpus, 
                             control=list(tokenize=trigramTokens))

# transformng into matrix
tri_TDMm = as.matrix(tri_TDM)

# sorting the matrix
tri_TDMm = sort(rowSums(tri_TDMm), decreasing = TRUE)
tri_DF   = data.frame(word = names(tri_TDMm), freq = tri_TDMm)

# deciding colour palet
pal = brewer.pal(8, "PuBuGn")

# Removing brightest colours 
pal = pal[-(1:4)]

# displaying plot
set.seed(1234)
wordcloud(tri_DF$word,
          tri_DF$freq,
          max.words    = 40,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,0.4))


# Timeline of Event

# Bar plot of mentioning of GME

# creating dataframe
comment_df = data.frame(comment_DTMm)

# attaching dates from original dataset
comment_df$date = wsb_comment$date

# displaying plot
comment_df %>% 
  filter(year(date) == 2021) %>%  # filtering out anything that isn't 2021
  group_by(date) %>% # grouping by date
  summarise(mentions = sum(gme+gamestop)) %>% # summing mentions of gme and gamestop per date
  ggplot(data = ., aes(x = date, y = mentions))+ # plotting
  geom_col(aplha = 0.7, fill = "blue")+ #chosing aes
  labs(x = "Date", y = "Count of Mentions") #renaming axis labs



# Cumulative Count of Mentions
comment_df %>% 
  filter(year(date) == 2021) %>% # filtering out anything that isn't 2021
  group_by(date) %>% # grouping by date
  summarise(mentions = sum(gme+gamestop)) %>% # suming mentions
  mutate(CumMen = cumsum(mentions)) %>% # cumulative sum of mentions
  ggplot(data = ., aes(x = date, y = CumMen))+ # plotting
  geom_area(fill = "darkgreen")+ # aes
  labs(x = "Date", y = "Cumulative Count of Mentions") # alter axis labs




# Line plot of GME Adjusted close

# changing to date format
gme_price$date = date(gme_price$date)

gme_price %>% 
  filter(year(gme_price$date)== 2021) %>% # selecting 2021
  ggplot(data = ., aes(x = date, y = GME.Adjusted))+ #plotting
  geom_line(color = "red")+ # aes
  labs(x = "Date", y = "Adjusted Close GME") # changing axis labs


# Combined Mentions and GME price

# creating alternative comment_df consisting of dates in 2021 and mentions
alt_comment_df = comment_df %>% 
  filter(year(date) == 2021) %>%  
  group_by(date) %>% 
  summarise(mentions = sum(gme+gamestop))

# creating alternative price_df consisting of dates in 2021 and adj close
alt_gme_price = gme_price %>% 
  filter(year(gme_price$date) == 2021) %>% 
  select(GME.Adjusted,date)

# joining the two alternative dataframes
gme_timeline     = left_join(alt_comment_df, alt_gme_price, by = c("date" = "date"))

# filling blank price values from weekends with prior values
gme_timeline     = gme_timeline %>% fill(GME.Adjusted)

# plotting the result
gme_timeline %>% 
  ggplot(data = .)+
  geom_col(aes(x = date, y = mentions), alpha = 0.7, fill = "blue")+
  geom_line(aes(x = date, y = GME.Adjusted),color = "red", size = 1)+
  ggtitle("BLUE = Mentions of GME, RED = GME price")+
  labs(x = "Date", y = " ")



# Polarity of WallStreetBets

# Creating Polarity Data of WSB
wsb_pol = polarity(as.character(wsb_comment$text))


# Transforming into DataFrame
# creating dataframe
wsbtime_pol = data.frame(text_pol = wsb_pol$all$polarity,
                         year     = year(wsb_comment$date),
                         month    = month(wsb_comment$date),
                         week     = week(wsb_comment$date),
                         day      = day(wsb_comment$date),
                         weekday  = weekdays(wsb_comment$date),
                         id       = wsb_comment$id,
                         date     = wsb_comment$date)

# replacing null-values
wsbtime_pol[is.na(wsbtime_pol)]=0

# sorting by id
wsbtime_pol = wsbtime_pol[order(wsbtime_pol$id, decreasing = FALSE),]

# sorting based on date
wsbtime_pol = wsbtime_pol[order(wsbtime_pol$date, decreasing = FALSE),]



# resetting rownames
rownames(wsbtime_pol) = NULL

# creating a column of index which now follows order of original dataset
wsbtime_pol$ind = as.numeric(rownames(wsbtime_pol))



# Distribution

wsbtime_pol %>% 
  
  ggplot(data = ., aes(x = text_pol))+
  geom_histogram(bins = 15)



# Average Daily Polarity

wsbtime_pol %>% 
  filter(year == 2021) %>%
  group_by(date) %>%
  mutate(Daily_polarity = mean(text_pol)) %>% 
  ggplot(data = .,aes(x = date, y = Daily_polarity))+
  geom_line(color = "blue")+
  geom_point(color = "blue")+
  labs(x = "Date", y = "Average Daily Polarity")


# Polarity by Weekday
weekday_pol <- aggregate(text_pol~weekday, wsbtime_pol, mean)

weekday_pol$weekday <- factor(weekday_pol$weekday,
                              levels= c("Monday",
                                        "Tuesday",
                                        "Wednesday",
                                        "Thursday",
                                        "Friday",
                                        "Saturday",
                                        "Sunday"))

weekday_pol %>% 
  ggplot(data = ., aes(x = weekday, y = text_pol))+
  geom_col(alpha = 0.7, fill = "blue")+
  geom_hline(aes(yintercept = 0))+
  labs(x = "Weekday", y = "Polarity")


# Dividing Dataset into three
wednesday = wsb_comment %>% 
  filter(weekdays(date)=="Wednesday")

sunday = wsb_comment %>% 
  filter(weekdays(date)=="Sunday")

restday = wsb_comment %>% 
  filter(weekdays(date)!="Sunday") %>% 
  filter(weekdays(date)!="Wednesday")


# Creating and Cleaning Corpuses for weekdays
wedn_corpus = VCorpus(VectorSource(wednesday$text))
sund_corpus = VCorpus(VectorSource(sunday$text))
rest_corpus = VCorpus(VectorSource(restday$text))

wedn_corpus = cleanCorpus(wedn_corpus, stops)
sund_corpus = cleanCorpus(sund_corpus, stops)
rest_corpus = cleanCorpus(rest_corpus, stops)


# unlisting content
wedn = unlist(pblapply(wedn_corpus, content))
sund = unlist(pblapply(sund_corpus, content)) 
rest = unlist(pblapply(rest_corpus, content))

# collapsing
wedn = paste(wedn, collapse = " ")
sund = paste(sund, collapse = " ")
rest = paste(rest, collapse = " ")

# combining three sub datasets
all_week = c(wedn, sund, rest)
all_week = VCorpus(VectorSource(all_week))

# making tdm WITH weighting
ctrl      = list(weighting = weightTf)
week_worldTDM  = TermDocumentMatrix(all_week, control = ctrl)
week_worldTDMm = as.matrix(week_worldTDM)

# giving column names
colnames(week_worldTDMm) = c("Wednesday",
                             "Sunday",
                             "Other Days")


# displaying plot
comparison.cloud(week_worldTDMm, 
                 max.words = 100, 
                 random.order = FALSE,
                 title.size = 1,
                 colors = brewer.pal(ncol(week_worldTDMm),"Dark2"),
                 scale = c(3,0.5))




# Cumulative Polarity by Posts in WSB

# Calculating cumulative menstion for each post
wsbtime_pol$CumPol = cumsum(wsbtime_pol$text_pol)


# plotting data
wsbtime_pol %>% 
  ggplot(data = ., aes(x = ind, y = CumPol))+
  geom_area(alpha = 0.6, fill = "red")+
  labs(x = "Post", y = "Cumulative Polarity")



# Cumulative Polarity by Day in WSB

# creating new df of cumulative polarity over days
daily_wsbtime_pol = wsbtime_pol%>% 
  group_by(date) %>% # grouping on days
  summarise(Daily_pol = mean(text_pol)) %>% # summarising mean for each day
  mutate(CumPol = cumsum(Daily_pol)) # calculating cumulative polarity per day

rownames(daily_wsbtime_pol) = NULL # resetting index
daily_wsbtime_pol$ind = as.numeric(rownames(daily_wsbtime_pol)) # creating column based on index

# display plot
daily_wsbtime_pol %>% 
  ggplot(data = ., aes(x = ind, y = CumPol))+ # using index to compress the plot
  geom_area(alpha = 0.7, fill = "red")+ # aes
  geom_vline(aes(xintercept = 47))+ # creating a vline on polarity turningpoint 
  labs(x = "Date", y = "Cumulative Polarity")+ # change axis names
  scale_x_continuous(breaks = seq(10,60,10), # creating customized tickmarks
                     labels = c("2020-03-26",
                                "2020-04-05",
                                "2020-07-13",
                                "2020-12-12",
                                "2021-01-19",
                                "2021-01-29"))


# Date of New Positive Spike
daily_wsbtime_pol %>% 
  filter(ind==47) %>% # filter on 47 from plot
  select(date) # selecting date




# The Sentiment of WallStreetBets


# creating tidy_df
tidyComm = tidy(comment_DTM)


# calling nrc library
nrc = nrc_emotions

# pivoting
nrc = nrc %>% 
  pivot_longer(-term, names_to = "emotion", values_to = "freq")

# dropping terms with no frequency
nrc = subset(nrc, nrc$freq>0)

# resetting index
nrc$freq = NULL

# joining sentiment lib and tidy dataframe
nrcSent = inner_join(tidyComm, nrc, by = c("term" = "term"))



# Radarchart

# table of sentiment emotions
emotions = data.frame(table(nrcSent$emotion))
# renaming columns
names(emotions) = c("emotion","freq")

# plotting chart
emotions %>% 
  e_chart(emotion) %>% 
  e_radar(freq, max = max(emotions$freq), name = "Wall Street Bets Emotions") %>% 
  e_tooltip(trigger = "item")


# Percentage Trust
12814/sum(sapply(gregexpr("\\S+",wsb_comment$text), length))*100


# Average word per Post
mean(sapply(gregexpr("\\S+",wsb_comment$text), length))



# Sentiment Over Time

# making df of dates
comment_dates = as.data.frame(wsb_comment$date)
# renaming col
colnames(comment_dates) = "Date"
# creating col of index values, corresponds with documents
comment_dates$document = rownames(comment_dates)
# joining date and sentiment dataframe
nrcTime = inner_join(nrcSent, comment_dates, by = c("document" = "document"))


# Summarising Emotions by Date
nrcTime_df = nrcTime %>% 
  group_by(Date) %>% # grouping by date
  count(emotion) # summing up emotions by date


# Pivoting Table
nrcTime_df = nrcTime_df %>% 
  group_by(emotion)
colnames(nrcTime_df) = c("Date","Emotion","Count")

# pivoting dataframe
nrcTime_df = nrcTime_df %>% 
  pivot_wider(names_from = Emotion, values_from = Count)


# Calculating Emotional Percentage
pct = nrcTime_df[,2:9]

# renaming columns with added pct
colnames(pct) = paste(colnames(pct), "pct", sep = "_")
# transforming null-values to 0

pct[is.na(pct)]=0

# calculating "percent" or pro mill (easier to compare with the stock price)
pct = pct/rowSums(pct)*1000



# binding together pct_sent and date
pct_long = cbind(pct, nrcTime_df$Date)

# giving column names
colnames(pct_long) = c(colnames(pct_long[1:8]),"Date")

# pivot long
pct_long = pct_long %>% 
  pivot_longer(-Date, names_to = "emotions", values_to = "frequency")

# calculating back to percentage instead of pro mill
pct_long$frequency = pct_long$frequency/10


# Graph of Happy Sentiments in 2021
pct_long %>% 
  filter(year(Date)==2021) %>% 
  filter(emotions != "sadness_pct",
         emotions != "anger_pct",
         emotions != "fear_pct",
         emotions != "disgust_pct") %>% 
  ggplot(data = ., aes(x = Date, y = frequency, color = emotions))+
  geom_line()+
  facet_wrap(~ emotions)



# Graph of Sad Sentiments in 2021
pct_long %>% 
  filter(year(Date)==2021) %>% 
  filter(emotions != "anticipation_pct",
         emotions != "joy_pct",
         emotions != "surprise_pct",
         emotions != "trust_pct") %>% 
  ggplot(data = ., aes(x = Date, y = frequency, color = emotions))+
  geom_line()+
  facet_wrap(~ emotions)



# attaching pct dataframe to original df
nrcTime_df = cbind(nrcTime_df, pct)


# Plotting The Relationship Between rise in Anticipation and rise of GME Price
# adding the GME adj close
nrcTime_df = left_join(nrcTime_df, gme_price, by = c("Date" = "date"))
# imputng weekend-price with last price
nrcTime_df = nrcTime_df %>% fill(GME.Adjusted)

# plotting chart
nrcTime_df %>% 
  filter(year(Date)==2021) %>% # selecting 2021
  ggplot(data = .)+ # calling plot
  geom_line(aes(x = Date, y = trust_pct), color = "blue", size = 1)+ #trust plot
  geom_line(aes(x = Date, y = anticipation_pct),alpha = 0.7, color = "green", size = 1)+ # anticipation plot
  geom_line(aes(x = Date, y = GME.Adjusted), color = "red", size = 1)+ # price plot
  geom_point(aes(x = Date, y = trust_pct), color = "blue", size = 2)+ #trust plot
  geom_point(aes(x = Date, y = anticipation_pct),alpha = 0.7, color = "green", size = 2)+
  ylab(" ")+ # removing name from y-axis
  ggtitle("BLUE     = Trust
GREEN  = Anticipation
RED       = GME price") # giving title


# END
