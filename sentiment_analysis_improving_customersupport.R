#packages list:
install.packages("httr")
install.packages("xml2")
install.packages("rvest")
install.packages("NLP")
install.packages("tm")
install.packages("dplR")
install.packages("tibble")
install.packages("tidytext")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("reshape2")



#webscraping
library(httr)
library(xml2)
library(rvest)

#url of site from where customer reviews to be taken
url1 <- 'https://www.amazon.in/GOQii-Temperature-Tracker-personal-coaching/product-reviews/B087CHCR1N/ref=cm_cr_othr_d_show_all_btm?ie=UTF8&reviewerType=all_reviews'
webpage1 <- read_html(url1)
data1 <- html_nodes(webpage1,'.review-text-content span')
review_data1 <- html_text(data1)
head(review_data1)
class(review_data1)

url2 <- 'https://www.amazon.in/GOQii-Temperature-Tracker-personal-coaching/product-reviews/B087CHCR1N/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber=2'
webpage2 <- read_html(url2)
data2 <- html_nodes(webpage2,'.review-text-content span')
review_data2 <- html_text(data2)
head(review_data2)
class(review_data2)

#merge all review file  into one single data file in form of character arrays
reviews <- c(review_data1,review_data2)
View(reviews)

# Data cleaning and pre-processing
library(NLP)
library(tm)


reviews_clean <- removeNumbers(reviews)
reviews_clean <- removePunctuation(reviews_clean)
reviews_clean <-gsub("[^\x01-\x7F]", "",reviews_clean) # to remove all non-ASCII characters #
View(reviews_clean)

# Converting character arrays to tidytext format
library(dplyr)
library(tibble)
library(tidytext)

reviews_tidy <- tibble(linenumber = 1:20,text = reviews_clean)
reviews_tidy <- reviews_tidy %>% unnest_tokens(word,text)
view(reviews_tidy)


#performing sentiment analysis
library(tidytext)
library(tidyr)

head(sentiments)
#sentiment lexicons present tidytext package : bing,afinn,nrc 
bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
                     

# sentiment for each word of reviews and count

word_count_afinn <- reviews_tidy %>% inner_join(afinn) %>% count(word,value,sort = TRUE)
view(word_count_afinn)

word_count_bing <- reviews_tidy %>% inner_join(bing) %>% count(word,sentiment,sort = TRUE)
view(word_count_bing)

# visualization of sentiment score
library(ggplot2)
word_count_afinn %>% mutate(n = ifelse(value < 0, -n,n)) %>% mutate(sentiment = ifelse(value <0,"negative","positive")) %>% mutate(word = reorder(word,n))  %>% ggplot(mapping = aes(word,n,fill=sentiment)) + geom_col() + coord_flip() + labs(y = "sentiment score")


# sentiment for each review

review_sentiment_bing <- reviews_tidy %>% inner_join(bing) %>% count(Review = linenumber,sentiment) %>%
        spread(sentiment,n,fill = 0) %>% mutate(sentiment = positive - negative)

review_sentiment_afinn <- reviews_tidy %>% inner_join(afinn) %>% group_by(Review = linenumber) %>% summarise(value = sum(value))

# visualisation of sentiment of reviews
review_sentiment_afinn %>% mutate(sentiment = ifelse(value <0,"negative","positive")) %>% ggplot(aes(Review,value,fill=sentiment)) + geom_col() + labs(title = "Sentiment of Reviews")

 


# most recurring words in the reviews
 library(RColorBrewer)
 library(wordcloud)

 reviews_tidy %>% anti_join(stop_words) %>% count(word) %>%
         with(wordcloud(word,n,max.words = 100))
          
# word cloud for sentiment words
 library(reshape2)

 reviews_tidy %>% inner_join(afinn) %>% count(word,value,sort = TRUE) %>% 
         mutate(sentiment = ifelse(value <0,"negative","positive")) %>%
         acast(word~sentiment,value.var = "n",fill = 0) %>%
         comparison.cloud(colors = c("red","green"),max.words = 100)
 




