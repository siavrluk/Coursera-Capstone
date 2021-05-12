library(dplyr)
library(ggplot2)
library(stringr)
library(ngram)
library(tm)
library(RWeka)
library(wordcloud)

blogs_file <- "final/en_US/en_US.blogs.txt"
news_file <- "final/en_US/en_US.news.txt"
twitter_file <- "final/en_US/en_US.twitter.txt"

# File size
blogs_file_size <- file.size(blogs_file)/(2^20)
news_file_size <- file.size(news_file)/(2^20)
twitter_file_size <- file.size(blogs_file)/(2^20)

# Read in the data files and check their length
blogs <-  readLines(blogs_file, skipNul = TRUE) 
blogs_lines_ct <- length(blogs)

news <-  readLines(news_file, skipNul = TRUE) 
news_lines_ct <- length(news)

twitter <-  readLines(twitter_file, skipNul = TRUE) 
twitter_lines_ct <- length(twitter)

# Check number of words per file
blogs_words_ct <- wordcount(blogs, sep = " ")
news_words_ct <- wordcount(news, sep = " ")
twitter_words_ct <- wordcount(twitter, sep = " ")


# Put in a data frame
summary_df <- data.frame(Dataset = c("blogs", "news", "twitter"),
                      FileSizeMB = c(blogs_file_size, news_file_size, twitter_file_size),
                      LinesCt = c(blogs_lines_ct, news_lines_ct, twitter_lines_ct),
                      WordsCt = c(blogs_words_ct, news_words_ct, twitter_words_ct))

names(summary_df) <- c("Dataset", "File size (MB)", "Lines Count", "Words Count")

saveRDS(summary_df, file = "summary.rds")

# Files are too big, will sample 5% of each
set.seed(3213)
sample_size <- 0.05

blogs_small <- sample(blogs, sample_size*length(blogs), replace = FALSE)
news_small <- sample(news, sample_size*length(news), replace = FALSE)
twitter_small <- sample(twitter, sample_size*length(twitter), replace = FALSE)

# Combine into one dataset
data_small <- c(blogs_small, news_small, twitter_small)
length(data_small)
small_words_ct <- wordcount(data_small, sep = " ")


saveRDS(data_small, file = "sampledData.rds")

# Free up memory
rm(blogs, news, twitter, blogs_small, news_small, twitter_small)


data_small_clean <- data_small %>%
  gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", .) %>% # remove urls
  gsub("\\S+@\\S+", " ", .) %>% # remove email addresses
  gsub("@\\S+", " ", .) %>%  # remove twitter handles
  gsub("#\\S+", " ", .) %>%  # remove hashtags
  tolower() %>%
  str_squish()


# Corpus
small_corpus <- data_small_clean %>%   
  VectorSource() %>%                
  VCorpus()

# Remove redundant information such as urls, twitter handles, email addresses, special characters, punctuations, numbers
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

clean_corpus <- small_corpus %>%
#  tm_map(content_transformer(function(x) gsub("(s?)(f|ht)tp(s?)://\\S+\\b", ""))) %>% 
#  tm_map(toSpace, ., "(s?)(f|ht)tp(s?)://\\S+\\b") %>% # remove urls
#  tm_map(toSpace, ., "\\S+@\\S+") %>% # remove email addresses
#  tm_map(toSpace, ., "@[^\\s]+") %>% # remove twitter handles
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) 



# Create tdm
Tokenizer1 <- function (x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdm1 <- TermDocumentMatrix(clean_corpus, control = list(tokenize = Tokenizer1))
tdm1 <- removeSparseTerms(tdm1, 0.9999)
words <- sort(rowSums(as.matrix(tdm1)),decreasing=TRUE) 
uni_df <- data.frame(word = names(words),freq=words)

saveRDS(uni_df, file = "unigrams.rds")

# Number of unique words
length(uni_df$word)

uni_df_coverage <- uni_df %>%
                      mutate(coverage = 100*cumsum(freq)/sum(freq))

word_coverage_plot <- ggplot(uni_df_coverage, aes(coverage)) +
  stat_bin(aes(y = cumsum(..count..)/sum(..count..)*100), geom = "step", bins = 50) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  xlab("Percent Covered") + 
  ylab("Percent of Words") + 
  ggtitle("Word coverage") + 
  coord_flip()

ggsave(word_coverage_plot,file="wordCoverage.png")


# Word cloud
wordCloud <- wordcloud(words = uni_df$word, freq = uni_df$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "RdBu"))


ggsave(wordCloud,file="wordCloud.png")


# Unigram

uni_words90_df <- uni_df_coverage[uni_df_coverage$coverage < 90, ]

uni_plot <- ggplot(uni_words90_df[1:10, ], aes(x = reorder(word, freq), y = freq / sum(uni_words90_df$freq), fill = freq, alpha = 0.1)) +
  geom_bar(stat = "identity", color = "black") +
  xlab("Unigram") +
  ylab("Proportion") +
  ggtitle("Top 10 Unigrams by Proportion") +
  coord_flip() +
  guides(fill = FALSE, alpha = FALSE) 


ggsave(uni_plot,file="unigrams.png")

# Bigram
Tokenizer2 <- function (x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm2 <- TermDocumentMatrix(clean_corpus, control = list(tokenize = Tokenizer2))
tdm2 <- removeSparseTerms(tdm2, 0.9999)
words <- sort(rowSums(as.matrix(tdm2)),decreasing=TRUE) 
bi_df <- data.frame(word = names(words),freq=words)

bi_df_coverage <- bi_df %>%
  mutate(coverage = 100*cumsum(freq)/sum(freq))

bi_words90_df <- bi_df_coverage[bi_df_coverage$coverage < 90, ]

saveRDS(bi_df, file = "bigrams.rds")

bi_plot <- ggplot(bi_words90_df[1:10, ], aes(x = reorder(word, freq), y = freq / sum(bi_words90_df$freq), fill = freq, alpha = 0.1)) +
  geom_bar(stat = "identity", color = "black") +
  xlab("Bigram") +
  ylab("Proportion") +
  ggtitle("Top 10 Bigrams by Proportion") +
  coord_flip() +
  guides(fill = FALSE, alpha = FALSE) 

ggsave(bi_plot,file="bigrams.png")

# Trigram
Tokenizer3 <- function (x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm3 <- TermDocumentMatrix(clean_corpus, control = list(tokenize = Tokenizer3))
tdm3 <- removeSparseTerms(tdm3, 0.9999)
words <- sort(rowSums(as.matrix(tdm3)),decreasing=TRUE) 
tri_df <- data.frame(word = names(words),freq=words)

tri_df_coverage <- tri_df %>%
  mutate(coverage = 100*cumsum(freq)/sum(freq))

tri_words90_df <- tri_df_coverage[tri_df_coverage$coverage < 90, ]

saveRDS(tri_df, file = "trigrams.rds")

tri_plot <- ggplot(tri_words90_df[1:10, ], aes(x = reorder(word, freq), y = freq / sum(tri_words90_df$freq), fill = freq, alpha = 0.1)) +
  geom_bar(stat = "identity", color = "black") +
  xlab("Trigram") +
  ylab("Proportion") +
  ggtitle("Top 10 Trigrams by Proportion") +
  coord_flip() +
  guides(fill = FALSE, alpha = FALSE) 

ggsave(tri_plot,file="trigrams.png")

# Quadrigram
Tokenizer4 <- function (x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
tdm4 <- TermDocumentMatrix(clean_corpus, control = list(tokenize = Tokenizer4))
tdm4 <- removeSparseTerms(tdm4, 0.99999)
words <- sort(rowSums(as.matrix(tdm4)),decreasing=TRUE) 
quad_df <- data.frame(word = names(words),freq=words)

quad_df_coverage <- quad_df %>%
  mutate(coverage = 100*cumsum(freq)/sum(freq))

quad_words90_df <- quad_df_coverage[quad_df_coverage$coverage < 90, ]

saveRDS(quad_df, file = "quadrigrams.rds")

quad_plot <- ggplot(quad_words90_df[1:10, ], aes(x = reorder(word, freq), y = freq / sum(quad_words90_df$freq), fill = freq, alpha = 0.1)) +
  geom_bar(stat = "identity", color = "black") +
  xlab("Quadrigram") +
  ylab("Proportion") +
  ggtitle("Top 10 Quadrigrams by Proportion") +
  coord_flip() +
  guides(fill = FALSE, alpha = FALSE) 

ggsave(quad_plot,file="quadrigrams.png")


# Split n-grams into beginning + last word
bi_df_split <- bi_df %>%
  mutate(beg = word(word , 1  , -2),
         last_word = word(word, -1))

tri_df_split <- tri_df %>%
  mutate(beg = word(word , 1  , -2),
         last_word = word(word, -1))

quad_df_split <- quad_df %>%
  mutate(beg = word(word , 1  , -2),
         last_word = word(word, -1))


saveRDS(bi_df_split, file = "bigrams_split.rds")
saveRDS(tri_df_split, file = "trigrams_split.rds")
saveRDS(quad_df_split, file = "quadrigrams_split.rds")


