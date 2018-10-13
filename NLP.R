memory.limit(size = 15000)
setwd("C:/Users/Keita/Desktop/DataScienceCapstone")

require(quanteda)
require(readtext)
require(data.table)
require(dplyr)
require(stringr)

filelist <- list.files(path = "C:/Users/Keita/OneDrive - Universitat de Barcelona/DataScienceCapstone/en_US", pattern = ".*.txt")
filenames <- gsub(pattern = "^en_US.", replacement = "", filelist)
filenames <- gsub(pattern = ".txt$", replacement = "", filenames)

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

merge_corpus <- lapply(txtdata, corpus)
data_corpus <- merge_corpus[[1]] + merge_corpus[[2]] + merge_corpus[[3]]
#Sampling the corpus. Use 1/10 of the data because of the CPU capacity.
set.seed(12345)
test_corpus <- corpus_sample(data_corpus, nrow(data_corpus$documents)/10)
test_token_1gram <- tokens(test_corpus, remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_separators = TRUE, remove_hyphens = TRUE)
rm(txtdata)
rm(merge_corpus)
rm(data_corpus)
rm(test_corpus)
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

test_token_2gram <- tokens_ngrams(test_token_1gram, n = 2)
test_token_3gram <- tokens_ngrams(test_token_1gram, n = 3)
test_token_4gram <- tokens_ngrams(test_token_1gram, n = 4)
test_token_5gram <- tokens_ngrams(test_token_1gram, n = 5)


# #blog_dfm <- dfm(blog_corpus, stem = TRUE, remove = stopwords("english"), remove_punct = TRUE, remove_numbers = TRUE)
# news_dfm <- dfm(news_corpus, stem = TRUE, remove = stopwords("english"), remove_punct = TRUE, remove_numbers = TRUE)
# # twitter_dfm <- dfm(twitter_corpus, stem = TRUE, remove = stopwords("english"), remove_punct = TRUE, remove_numbers = TRUE)
# en_US_dfm <- dfm(en_US_corpus, stem = TRUE, remove = stopwords("english"), remove_punct = TRUE, remove_numbers = TRUE)
# en_US_dfm <- dfm_remove(en_US_dfm, "rt")
# en_US_dfm <- dfm_select(en_US_dfm, min_nchar = 2)

test_dfm_1gram <- dfm(test_token_1gram)
test_dfm_2gram <- dfm(test_token_2gram)
test_dfm_3gram <- dfm(test_token_3gram)
test_dfm_4gram <- dfm(test_token_4gram)
test_dfm_5gram <- dfm(test_token_5gram)


rm(
  list = c("test_token_1gram", "test_token_2gram", "test_token_3gram", "test_token_4gram", "test_token_5gram")
)

test_freq_1gram <- textstat_frequency(test_dfm_1gram)
test_freq_2gram <- textstat_frequency(test_dfm_2gram)
test_freq_3gram <- textstat_frequency(test_dfm_3gram)
test_freq_4gram <- textstat_frequency(test_dfm_4gram)
test_freq_5gram <- textstat_frequency(test_dfm_5gram)

rm(
  list = c("test_dfm_1gram", "test_dfm_2gram", "test_dfm_3gram", "test_dfm_4gram", "test_dfm_5gram")
)

#Calculate the probability of each n-gram

test_freq_1gram <- test_freq_1gram %>% 
  select(feature, frequency) %>% 
  data.table() %>% 
  mutate(prob = frequency/sum(test_freq_1gram[,2]))

test_freq_2gram <- test_freq_2gram %>% 
  select(feature, frequency) %>% 
  data.table() %>% 
  mutate(prob = frequency/sum(test_freq_2gram[,2]))

test_freq_3gram <- test_freq_3gram %>% 
  select(feature, frequency) %>% 
  data.table() %>% 
  mutate(prob = frequency/sum(test_freq_3gram[,2]))

test_freq_4gram <- test_freq_4gram %>% 
  select(feature, frequency) %>% 
  data.table() %>% 
  mutate(prob = frequency/sum(test_freq_4gram[,2]))

test_freq_5gram <- test_freq_5gram %>% 
  select(feature, frequency) %>% 
  data.table() %>% 
  mutate(prob = frequency/sum(test_freq_5gram[,2]))

test_dt <- test_freq_5gram 
test_dt_head <- head(test_dt,100)

pred_sentence <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

cleanSentente <- function(sentence){
  pred_token <- tokens(sentence, remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_separators = TRUE, remove_hyphens = TRUE)
  pred_token <- as.vector(unlist(pred_token))
  final_4words <- pred_token[(length(pred_token)-3):length(pred_token)]
  four <-  str_c(final_4words, collapse = "_")
  three <-  str_c(final_4words[2:4], collapse = "_")
  two <-  str_c(final_4words[3:4], collapse = "_")
  one <-  final_4words[4]
  c(four, three, two, one)
}

lookup <- cleanSentente(pred_sentence)

sentence <- pred_sentence

predNextWord <- function(sentence) {
  lookup <- cleanSentente(sentence)
  #Strating with 5gram
  result <- subset(test_freq_5gram, grepl(paste("^", lookup[1], "_", sep =""), test_freq_5gram$feature))
  if (max(result$frequency) > 10) {
    result
  } else {
    #Check 4gram
    result <- subset(test_freq_4gram, grepl(paste("^", lookup[2], "_", sep =""), test_freq_4gram$feature))
    if (max(result$frequency) > 20) {
      head(result,20)
    } else {
      #Check 3gram
      result <- subset(test_freq_3gram, grepl(paste("^", lookup[3], "_", sep =""), test_freq_3gram$feature))
      if (max(result$frequency) > 20) {
        result
      } else {
        #check 2gram
        result <- subset(test_freq_2gram, grepl(paste("^", lookup[4], "_", sep =""), test_freq_2gram$feature))
        if (max(result$frequency) > 20) {
          result
        } else {
          print("Unseen word")
        }
      }
    }
  }
}


lookup <- subset(test_dt, grepl("^tsasa_end_of_the", test_dt$feature))
nrow(lookup) == 0

  



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
