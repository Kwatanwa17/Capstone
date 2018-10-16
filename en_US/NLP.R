memory.limit(size = 20000)
setwd("C:/Users/Keita/Desktop/DataScienceCapstone")

#Load packeges
require(quanteda)
require(readtext)
require(data.table)
require(dplyr)
require(stringr)


filelist <- list.files(path = "C:/Users/Keita/OneDrive - Universitat de Barcelona/DataScienceCapstone/en_US", pattern = ".*.txt")
# filenames <- gsub(pattern = "^en_US.", replacement = "", filelist)
# filenames <- gsub(pattern = ".txt$", replacement = "", filenames)

sReadLines <- function(fnam) {
  f <- file(fnam, "rb")
  res <- readLines(f, encoding = "UTF-8")
  close(f)
  iconv(res, "UTF-8", "ASCII", sub = "")
}

setwd("C:/Users/Keita/Desktop/DataScienceCapstone/en_US")

txtdata <- lapply(filelist, sReadLines)

# blog_data <- readLines(filelist[1], encoding="UTF-8")
# news_data <- readLines(filelist[2], encoding="UTF-8")
# twitter_data <- readLines(filelist[3], encoding="UTF-8")
# 
# blog_data <- iconv(blog_data, "UTF-8", "ASCII", sub = "")
# news_data <- iconv(news_data, "UTF-8", "ASCII", sub = "")
# twitter_data <- iconv(twitter_data, "UTF-8", "ASCII", sub = "")


#Load as a List

merge_txt <- c(txtdata[[1]], txtdata[[2]], txtdata[[3]])

#Sampling a quarter part of the data set, witch subsequently will be divided traing and test data set.
set.seed(12345)
sample_txt <- sample(merge_txt, length(merge_txt)/4)

sample_size <- floor(0.9 * length(sample_txt))

set.seed(12345)
train_ind <- sample(seq_len(length(sample_txt)), size = sample_size)
train_txt <- sample_txt[train_ind]
test_txt <- sample_txt[-train_ind]

train_corpus <- corpus(train_txt)
test_corpus <- corpus(test_txt)

rm(txtdata)
rm(merge_txt)
rm(sample_txt)
rm(train_txt)
rm(test_txt)

#Craete tokens and calculate frqquency for each token
freq_1gram <- tokens(train_corpus, ngrams = 1, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE,
                      remove_symbols = TRUE, remove_separators = TRUE, remove_hyphens = TRUE, remove_twitter = TRUE) %>% 
  dfm() %>% 
  dfm_trim(min_termfreq = 2) %>% 
  textstat_frequency() %>% 
  select(feature, frequency) %>% 
  data.table() %>% 
  mutate(prob = frequency/sum(frequency))

freq_2gram <- tokens(train_corpus, ngrams = 2, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE,
                      remove_symbols = TRUE, remove_separators = TRUE, remove_hyphens = TRUE, remove_twitter = TRUE) %>% 
  dfm() %>% 
  dfm_trim(min_termfreq = 2) %>% 
  textstat_frequency() %>% 
  select(feature, frequency) %>% 
  data.table() %>% 
  mutate(prob = frequency/sum(frequency))

freq_3gram <- tokens(train_corpus, ngrams = 3, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE,
                     remove_symbols = TRUE, remove_separators = TRUE, remove_hyphens = TRUE, remove_twitter = TRUE) %>% 
  dfm() %>% 
  dfm_trim(min_termfreq = 2) %>% 
  textstat_frequency() %>% 
  select(feature, frequency) %>% 
  data.table() %>% 
  mutate(prob = frequency/sum(frequency))

freq_4gram <- tokens(train_corpus, ngrams = 4, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE,
                      remove_symbols = TRUE, remove_separators = TRUE, remove_hyphens = TRUE, remove_twitter = TRUE) %>% 
  dfm() %>% 
  dfm_trim(min_termfreq = 2) %>% 
  textstat_frequency() %>% 
  select(feature, frequency) %>% 
  data.table() %>% 
  mutate(prob = frequency/sum(frequency))
  
freq_5gram <- tokens(train_corpus, ngrams = 5, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE,
                      remove_symbols = TRUE, remove_separators = TRUE, remove_hyphens = TRUE, remove_twitter = TRUE) %>% 
  dfm() %>% 
  dfm_trim(min_termfreq = 2) %>% 
  textstat_frequency() %>% 
  select(feature, frequency) %>% 
  data.table() %>% 
  mutate(prob = frequency/sum(frequency))


# test_token_2gram <- tokens_ngrams(test_token_1gram, n = 2)
# test_token_3gram <- tokens_ngrams(test_token_1gram, n = 3)
# test_token_4gram <- tokens_ngrams(test_token_1gram, n = 4)
# test_token_5gram <- tokens_ngrams(test_token_1gram, n = 5)


rm(train_corpus)

# 
# DaMergeCorpus
# blog_corpus <- corpus(blog_data)
# news_corpus <- corpus(news_data)
# twitter_corpus <- corpus(twitter_data)
# en_US_corpus <- blog_corpus + news_corpus + twitter_corpus


# Week 1 ------------------------------------------------------------------

# love_counts <- kwic(twitter_corpus, "love", valuetype = "fixed")
# hate_counts <- kwic(twitter_corpus, "hate", valuetype = "fixed")
# biostats <- kwic(twitter_corpus, "biostats")
# sentence <- phrase("A computer once beat me at chess, but it was no match for me at kickboxing")
# q6 <- kwic(twitter_corpus, pattern = sentence, valuetype = "glob")
# q6 <- kwic(twitter_corpus, phrase("A computer once beat me at chess"))
# tweewts <- twitter_corpus[q6$docname]
# kwic(tweewts, "kickboxing")
# en_US_corpus <- blog_corpus + news_corpus + twitter_corpus
# summary(en_US_corpus)
# 
# test <- ntoken(en_US_corpus)
# 
# blog_token_count <- ntoken(blog_corpus)
# news_token_count <- ntoken(news_corpus)


# Week2 -------------------------------------------------------------------

#Explatory analysis and modeling

# rm(blog_data)
# rm(news_data)
# rm(twitter_data)
# 
# rm(blog_corpus)
# rm(news_corpus)
# rm(twitter_corpus)

# blog_token <- tokens(blog_corpus, remove_punct = FALSE)
# news_token <- tokens(news_corpus, remove_punct = FALSE)
# twitter_token <- tokens(twitter_corpus, remove_punct = FALSE)
# en_US_token <- tokens(en_US_corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE, remove_hyphens = FALSE)

# en_US_2gram <- tokens_ngrams(en_US_token, n = 2)
# en_US_3gram <- tokens_ngrams(en_US_token, n = 3)
# en_US_4gram <- tokens_ngrams(en_US_token, n = 4)
# en_US_5gram <- tokens_ngrams(en_US_token, n = 5)
# en_US_6gram <- tokens_ngrams(en_US_token, n = 6)

# test_token_2gram <- tokens_ngrams(test_token_1gram, n = 2)
# test_token_3gram <- tokens_ngrams(test_token_1gram, n = 3)
# test_token_4gram <- tokens_ngrams(test_token_1gram, n = 4)
# test_token_5gram <- tokens_ngrams(test_token_1gram, n = 5)


# #blog_dfm <- dfm(blog_corpus, stem = TRUE, remove = stopwords("english"), remove_punct = TRUE, remove_numbers = TRUE)
# news_dfm <- dfm(news_corpus, stem = TRUE, remove = stopwords("english"), remove_punct = TRUE, remove_numbers = TRUE)
# # twitter_dfm <- dfm(twitter_corpus, stem = TRUE, remove = stopwords("english"), remove_punct = TRUE, remove_numbers = TRUE)
# en_US_dfm <- dfm(en_US_corpus, stem = TRUE, remove = stopwords("english"), remove_punct = TRUE, remove_numbers = TRUE)
# en_US_dfm <- dfm_remove(en_US_dfm, "rt")
# en_US_dfm <- dfm_select(en_US_dfm, min_nchar = 2)

# test_dfm_1gram <- dfm(test_token_1gram) %>% dfm_trim(min_termfreq = 2) %>% textstat_frequency()
# test_dfm_2gram <- dfm(test_token_2gram)
# test_dfm_3gram <- dfm(test_token_3gram)
# test_dfm_4gram <- dfm(test_token_4gram)
# test_dfm_5gram <- dfm(test_token_5gram)
# 
# 
# rm(
#   list = c("test_token_1gram", "test_token_2gram", "test_token_3gram", "test_token_4gram", "test_token_5gram")
# )
# 
# test_freq_1gram <- textstat_frequency(test_dfm_1gram)
# test_freq_2gram <- textstat_frequency(test_dfm_2gram)
# test_freq_3gram <- textstat_frequency(test_dfm_3gram)
# test_freq_4gram <- textstat_frequency(test_dfm_4gram)
# test_freq_5gram <- textstat_frequency(test_dfm_5gram)
# 
# rm(
#   list = c("test_dfm_1gram", "test_dfm_2gram", "test_dfm_3gram", "test_dfm_4gram", "test_dfm_5gram")
# )

#Calculate the probability of each n-gram

# freq_1gram <-dfm(token_1gram) %>% 
#   dfm_trim(min_termfreq = 2) %>% 
#   textstat_frequency() %>% 
#   select(feature, frequency) %>% 
#   data.table() %>% 
#   mutate(prob = frequency/sum(frequency))
# 
# freq_2gram <-dfm(token_2gram) %>% 
#   dfm_trim(min_termfreq = 2) %>% 
#   textstat_frequency() %>% 
#   select(feature, frequency) %>% 
#   data.table() %>% 
#   mutate(prob = frequency/sum(frequency))
# 
# freq_3gram <-dfm(token_3gram) %>% 
#   dfm_trim(min_termfreq = 2) %>% 
#   textstat_frequency() %>% 
#   select(feature, frequency) %>% 
#   data.table() %>% 
#   mutate(prob = frequency/sum(frequency))
# 
# freq_4gram <-dfm(token_4gram) %>% 
#   dfm_trim(min_termfreq = 2) %>% 
#   textstat_frequency() %>% 
#   select(feature, frequency) %>% 
#   data.table() %>% 
#   mutate(prob = frequency/sum(frequency))
# 
# freq_5gram <-dfm(token_5gram) %>% 
#   dfm_trim(min_termfreq = 2) %>% 
#   textstat_frequency() %>% 
#   select(feature, frequency) %>% 
#   data.table() %>% 
#   mutate(prob = frequency/sum(frequency))

cleanSentence <- function(sentence){
  pred_token <- tokens(sentence, remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_separators = TRUE, remove_hyphens = TRUE)
  pred_token <- tokens_tolower(pred_token)
  pred_token <- as.vector(unlist(pred_token))
  if (length(pred_token) > 3) {
  final_4words <- pred_token[(length(pred_token)-3):length(pred_token)]
  four <-  str_c(final_4words, collapse = "_")
  three <-  str_c(final_4words[2:4], collapse = "_")
  two <-  str_c(final_4words[3:4], collapse = "_")
  one <-  final_4words[4]
  c(four, three, two, one)
  } else c(NULL, NULL, NULL, NULL)
}

sepSentence <- function(sentence){
  pred_token <- tokens(sentence, remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_separators = TRUE, remove_hyphens = TRUE)
  pred_token <- tokens_tolower(pred_token)
  pred_token <- as.vector(unlist(pred_token))
  first <- str_c(pred_token[1:length(pred_token)-1], collapse = " ")
  final <- pred_token[length(pred_token)]
  c(first, final)
}

#CREATE TWO BACK MODEL
#1 SIMPLE ONE
#2 ALFA MODEL ALPHA = 0.5??

#else if!
sentence <- test_sentence
#1rm(fr)
predNextWord <- function(sentence) {
  lookup <- cleanSentence(sentence)
  #Strating with 5gram
  result_5gram <- subset(freq_5gram, grepl(paste("^", lookup[1], "_", sep =""), freq_5gram$feature))
  if (nrow(result_5gram) != 0) {
    result <- str_split(result_5gram[, 1][1], pattern = "_")
    result <- as.vector(result[[1]])
    result[5]
  } else {
    #Check 4gram
    result_4gram <- subset(freq_4gram, grepl(paste("^", lookup[2], "_", sep =""), freq_4gram$feature))
    if (nrow(result_4gram) != 0) {
      result <- str_split(result_4gram[, 1][1], pattern = "_")
      result <- as.vector(result[[1]])
      result[4]
    } else {
      #Check 3gram
      result_3gram <- subset(freq_3gram, grepl(paste("^", lookup[3], "_", sep =""), freq_3gram$feature))
      if (nrow(result_3gram) != 0) {
        result <- str_split(result_3gram[, 1][1], pattern = "_")
        result <- as.vector(result[[1]])
        result[3]
      } else {
        #check 2gram
        result_2gram <- subset(freq_2gram, grepl(paste("^", lookup[4], "_", sep =""), freq_2gram$feature))
        if (nrow(result_2gram) != 0) {
          result <- str_split(result_2gram[, 1][1], pattern = "_")
          result <- as.vector(result[[1]])
          result[2]
        } else {
          freq_1gram[, 1][1]
        }
      }
    }
  }
}

#2

predNextWord2 <- function(sentence) {
  lookup <- cleanSentente(sentence)
  #Strating with 5gram
  result_5gram <- subset(freq_5gram, grepl(paste("^", lookup[1], "_", sep =""), freq_5gram$feature))
  if (max(result$frequency) > 30) {
    result_5gram
  } else {
    #Check 4gram
    result_4gram <- subset(freq_4gram, grepl(paste("^", lookup[2], "_", sep =""), freq_4gram$feature))
    if (max(result$frequency) > 30) {
      result_4gram
    } else {
      #Check 3gram
      result_3gram <- subset(freq_3gram, grepl(paste("^", lookup[3], "_", sep =""), freq_3gram$feature))
      if (max(result$frequency) > 100) {
        result_3gram
      } else {
        #check 2gram
        result_2gram <- subset(freq_2gram, grepl(paste("^", lookup[4], "_", sep =""), freq_2gram$feature))
        if (max(result$frequency) > 100) {
          result_2gram
        } else {
          head(freq_1gram, 50)
        }
      }
    }
  }
}

# checkWord <- function(df, word) {
#   subset(df, grepl(paste(word, "$", sep =""), df$feature))
# }

test_sentence <- "Ifm thankful my childhood was filled with imagination and bruises from playing"
predNextWord(test_sentence)

test_sentences <- apply(test_corpus$documents, 1, sepSentence)
a <- head(test_sentences, 5)

b <- unlist(test_sentences)
first_index <- seq(1, length(b), 2)

first <- b[first_index]
last <- b[-first_index]

test_df <- data.frame(test_sentences)

pred_word <- sapply(first[1:30], predNextWord)

sum(pred_word == last[1:30])
# 
# for (x in 1:length(test_words)) {
#   result = checkWord(test_df, test_words[x])
#   print(test_words[x])
#   print(result)
# }
# 
# for (x in 1:length(test_words)) {
#   result = checkWord(freq_1gram, test_words[x])
#   print(test_words[x])
#   print(result)
#   
# }


# firstStirng <- function(string){
#   string_vec = str_split(string, pattern = "_")
#   first = str_c(string_vec[[1]][1:(length(string_vec[[1]])-1)], collapse = "_")
#   first
# }
# 
# lastStirng <- function(string){
#   string_vec = str_split(string, pattern = "_")
#   last = string_vec[[1]][length(string_vec[[1]])]
#   last
# }



# 
# blog_top_100 <- topfeatures(blog_dfm, 100)
# news_top_100 <- topfeatures(news_dfm, 100)
# twitter_top_100 <- topfeatures(twitter_dfm, 100)
# en_US_top_100 <- topfeatures(en_US_dfm, 500)
# en_US_top_100
#Trim blog_dfm
#blog_dfm <- dfm_trim(blog_dfm, min_termfreq = 5000)

#Trim other dfm
#news_dfm <- dfm_trim(news_dfm, min_termfreq = 5)
#twitter_dfm <- dfm_trim(twitter_dfm, min_termfreq = 5)

#fcm
# en_US_fcm <- fcm(en_US_dfm)
#blog_fcm <- fcm(blog_dfm)

# news_fcm <- fcm(news_dfm)
# twitter_fcm <- fcm(twitter_dfm)
# 
# blog_feat <- names(topfeatures(blog_fcm, 100))
# blog_col <- fcm_select(blog_fcm, blog_feat)
# textplot_network(blog_col, min_freq = 0.95, edge_size = 5)
# 
# news_feat <- names(topfeatures(news_fcm, 100))
# news_col <- fcm_select(news_fcm, news_feat)
# textplot_network(news_col, min_freq = 0.95, edge_size = 5)
# 
# twitter_feat <- names(topfeatures(twitter_fcm, 100))
# twitter_col <- fcm_select(twitter_fcm, twitter_feat)
# textplot_network(twitter_col, min_freq = 0.95, edge_size = 5)

# en_US_feat <- names(topfeatures(en_US_fcm, 100))
# en_US_col <- fcm_select(en_US_fcm, en_USr_feat)
# textplot_network(en_US_col, min_freq = 0.95, edge_size = 5)

#Topfeatures
#
#fcm textplot_network
