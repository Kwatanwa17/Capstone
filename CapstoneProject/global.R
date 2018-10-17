load("data/freq_1gram.rda")
load("data/freq_2gram.rda")
load("data/freq_3gram.rda")
load("data/freq_4gram.rda")
load("data/freq_5gram.rda")

#Functions

cleanSentenceUpdated <- function(sentence){
  pred_token <- tokens(sentence, remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_separators = TRUE,
                       remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
  pred_token <- tokens_tolower(pred_token)
  pred_token <- as.vector(unlist(pred_token))
  
  #Check if the length is al least 4
  if (length(pred_token) > 3) {
    final_words <- pred_token[(length(pred_token)-3):length(pred_token)]
    #Check if the length is al least 3
  } else if (length(pred_token) > 2) {
    final_words <- pred_token[(length(pred_token)-2):length(pred_token)]
    #Check if the length is al least 2
  } else if (length(pred_token) > 1) {
    final_words <- pred_token[(length(pred_token)-1):length(pred_token)]
    #Otherwise the length is 1
  } else {
    final_words <- pred_token
  }
  
  if (length(final_words) == 4) {
    four <-  str_c(final_words, collapse = "_")
    three <-  str_c(final_words[2:4], collapse = "_")
    two <-  str_c(final_words[3:4], collapse = "_")
    one <-  final_words[4]
    output <- c(four, three, two, one)
  } else if (length(final_words) == 3) {
    three <-  str_c(final_words, collapse = "_")
    two <-  str_c(final_words[2:3], collapse = "_")
    one <-  final_words[3]
    output <- c(NA, three, two, one)
  } else if (length(final_words) == 2) {
    two <-  str_c(final_words, collapse = "_")
    one <-  final_words[2]
    output <- c(NA, NA, two, one)
  } else {
    one <- final_words
    output <- c(NA, NA, NA, one)
  }
  
  output
  
}


predNextWordUpdated <- function(sentence) {
  lookup <- cleanSentenceUpdated(sentence)
  
  #Check if the length of word is at least 4
  if (is.na(lookup[1]) == FALSE) {
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
    #Check if the length of word is at least 3
  } else if (is.na(lookup[2]) == FALSE) {
    
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
    
    #Check if the length of word is at least 2
  } else if (is.na(lookup[3]) == FALSE) { 
    
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
    
  } else {
    
    #Otherwise word length is 1
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