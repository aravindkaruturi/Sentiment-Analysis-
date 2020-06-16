#packages needed to be installed
install.packages("httr")
install.packages("rvest")
install.packages("NLP")
install.packages("tm")
install.packages("dplyr")
install.packages("tidytext")
install.packages("tibble")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("reshape2")

## web scraping and sentiment analysis with data related to "the impact of 
## baby boomers on healthcare industry"

## web scraping
library(httr)
library(xml2)
library(rvest)
url <-  'https://www.encorepbc.org/blog/baby-boomers-how-they-impact-american-society/'
webpage <- read_html(url)
data <- html_nodes(webpage,'p')
main_data <- html_text(data)
head(main_data)
class(main_data)

url2 <- 'https://www.americanmobile.com/nursezone/nursing-news/the-baby-boomers-massive-impact-on-health-care/'
webpage2 <- read_html(url2)
data1 <- html_nodes(webpage2,'p~p+p')
main_data1 <- html_text(data1)
head(main_data1)
class(main_data1)

## merge all files from these websites in form of character arrays
merge <- c(main_data,main_data1)
View(merge)

## data cleaning and pre-processing (remove numbers,punctuations etc)
library(NLP)
library(tm)
merge <- gsub(",.*","",merge)
merge <- removeNumbers(merge)
merge <- removePunctuation(merge)
View(merge)

## converting character arrays to tidytext format
library(dplyr)
library(tibble)
library(tidytext)
text_df <- tibble(line = 1:26,text = merge)
text_df <- text_df %>% unnest_tokens(word,text)
view(text_df)

## performing sentiment analysis
library(tidytext)
sentiments
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("nrc")

bing_negative <- get_sentiments("bing") %>%
                 filter(sentiment =="negative")
view(bing_negative)

nrc_negative <- get_sentiments("nrc") %>%
                filter(sentiment == "negative")
view(nrc_negative)

main_bing <- text_df %>% inner_join(bing_negative) %>% 
             count(word,sort = TRUE)
view(main_bing)

main_nrc <- text_df %>% inner_join(nrc_negative) %>%
             count(word,sort = TRUE)
view(main_nrc)

## create a word cloud to analyze the most recurring words and with highest 
## sensitivity and importance
library(RColorBrewer)
library(wordcloud)
library(reshape2)

text_df %>% anti_join(get_sentiments("bing")) %>% count(word) %>%
        with(wordcloud(word,n,max.words = 200))
## create a word cloud with both positive and negative words
text_df %>% inner_join(get_sentiments("bing")) %>% count(word,sentiment,sort = TRUE) %>%
        acast(word~sentiment,value.var = "n",fill = 0) %>%
        comparison.cloud(colors = c("red","green"),max.words = 200)









        


