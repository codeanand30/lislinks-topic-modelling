lislinks <- read.csv(file.choose(), header = TRUE )
str(lislinks)

#Build corpus
library(tm)
corpus <- iconv(lislinks$Question)
corpus <- Corpus(VectorSource(corpus))

writeLines(as.character(lislinks$Question[1:5]))


lislinks <- iconv(lislinks$Question, to = "ASCII", sub = " ") 


lislinks <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", lislinks)  # Remove the "RT" (retweet) and usernames 
lislinks = gsub("http.+ |http.+$", " ", lislinks)  # Remove html links
lislinks = gsub("http[[:alnum:]]*", "", lislinks)
lislinks = gsub("[[:punct:]]", " ", lislinks)  # Remove punctuation
lislinks = gsub("[ |\t]{2,}", " ", lislinks)  # Remove tabs
lislinks = gsub("^ ", "", lislinks)  # Leading blanks
lislinks = gsub(" $", "", lislinks)  # Lagging blanks
lislinks = gsub(" +", " ", lislinks) # General spaces 
writeLines(as.character(lislinks[1:5]))


lislinks = tolower(lislinks)
lislinks = unique(lislinks)
writeLines(as.character(lislinks[1:5]))


library(tm)
library(stopwords)
corpus <- Corpus(VectorSource(corpus))
corpus <- VCorpus(x = VectorSource(lislinks),
                    readerControl = list(reader=readPlain,
                                         language="en"))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c('but', 'in', 'it', 'can'))
writeLines(as.character(lislinks[1:5]))

corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)

writeLines(as.character(lislinks[1:5]))



library(RColorBrewer)
library(wordcloud)
set.seed(1234)
palet  = brewer.pal(8, 'Dark2')
wordcloud(corpus, min.freq = 100, max.words = 500, scale = c(6, 0.2) , random.order = TRUE, col = palet)



dtm = DocumentTermMatrix(corpus)
dtm
doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]
dtm




library(dplyr)
freq = colSums(as.matrix(dtm))
length(freq)


ord = order(freq, decreasing = TRUE)
freq[head(ord, n = 20)]


plot = data.frame(words = names(freq), count = freq)
library(ggplot2)
plot = subset(plot, plot$count > 1000) #creating a subset of words having more than 100 frequency
str(plot)
ggplot(data = plot, aes(words, count)) + geom_bar(stat = 'identity') + ggtitle('Words used more than 1000 times')+coord_flip()



library(topicmodels)
k = 10
seed = 1234
lda_fit <- LDA(dtm, k=k, control = list(seed=seed))
lda_fit@alpha
topics(lda_fit, k)
terms(lda_fit, 10)

lda_fit.terms <- as.matrix(terms(lda_fit, 10))
write.csv(lda_fit.terms, file = paste("docstotopics.csv"))

#visulization

library(tidytext)
library(tidyverse)
library(ggplot2)
topics <- tidy(lda_fit, matrix = "beta")
top_terms <-
  topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  labs(title = "LDA of Lislinks", caption = "Top Terms") +
  ylab("") +
  xlab("") +
  coord_flip()









