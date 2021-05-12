library(dplyr)
library(ggplot2)
library(stringr)
library(ngram)
library(tm)
library(RWeka)
library(wordcloud)

uni_df <- readRDS("unigrams.rds")
bi_df_split <- readRDS("bigrams_split.rds")
tri_df_split <- readRDS("trigrams_split.rds")
quad_df_split <- readRDS("quadrigrams_split.rds")



check_uni <- function(unigram){
  for (i in 1:nrow(bi_df_split)){
    if (unigram %in% bi_df_split$beg[i]){
      return(bi_df_split$last_word[i])
    }
  }

  return(sample(head(uni_df$word, 50), 1))
}

check_bi <- function(bigram){
  # check both words in a bigram are in tri_df_split
  for (i in 1:nrow(tri_df_split)){
    if (bigram %in% tri_df_split$beg[i]){
      return(tri_df_split$last_word[i])
    }
  }
  
  return(check_uni(word(bigram, 2, -1)))
  
}  

check_tri <- function(trigram){
  
  # check both words in a bigram are in tri_df_split
  for (i in 1:nrow(quad_df_split)){
    if (trigram %in% quad_df_split$beg[i]){
      return(quad_df_split$last_word[i])
    }  
  }
  
  return(check_bi(word(trigram , 2, -1)))
  
}  


phrase_preprocess <- function(phrase){
  phrase_clean <- phrase %>%
    gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", .) %>% # remove urls
    gsub("\\S+@\\S+", " ", .) %>% # remove email addresses
    gsub("@\\S+", " ", .) %>%  # remove twitter handles
    gsub("#\\S+", " ", .) %>%  # remove hashtags
    removeNumbers() %>%
    removePunctuation() %>%
    str_squish() %>%
    tolower() 
  
  return(phrase_clean)
}


predictNext <- function(phrase){
  phrase_clean <- phrase_preprocess(phrase)
  
  n <- sapply(strsplit(phrase_clean, " "), length)

  if (n == 0){
    return("")
  }
  else if (n == 1){
    return(check_uni(phrase_clean))
  }
  else if (n == 2){
    return(check_bi(phrase_clean))
  }
  else if (n==3){
    return(check_tri(phrase_clean))
  }
  else{
    return(check_tri(word(phrase_clean, -3, -1)))
  }
}


