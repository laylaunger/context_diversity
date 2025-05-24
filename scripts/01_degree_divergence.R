# Title: Degree & Divergence
# Author: Layla Unger
# Last Updated: 2025-05-22
# R version: 4.3.2
# Packages: readr, plyr, dplyr, data.table, tidyr, tidytext, purrr, stringr, topicmodels, philentropy

#####################################################
#################### DESCRIPTION ####################
#####################################################

# Measures of contextual diversity capture the diversity of
# language contexts in which words appear. The focus of
# this script is to generate multiple measures of contextual
# diversity in corpora of language input to children.
# It can be run for any of the following corpora:
# English, Spanish, French, or German.

# Prior to running this script, you should ensure that you
# have: the following directories from the github repository:
# data/mcdi_files: files containing age of acquisition
# data for each language,
# data/names_file: a file containing a list
# of common first names that will be replaced with a 
# common token.

# To run this script, you need to specify the language
# corpus below. You can also set parameters for calculating
# the contextual diversity measures.

# There are points within the script where there is code
# to save intermediate outputs to .rds files. Once you
# have run these lines, you can comment them out and
# uncomment the lines that load the .rds files.


#####################################################
################### LOAD PACKAGES ###################
#####################################################

# Install any packages not yet installed

library(readr)
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(tidytext)
library(purrr)
library(stringr)
library(topicmodels)
library(philentropy)


#####################################################
################# SPECIFY LANGUAGE ##################
#####################################################

# ---- SPECIFY HERE ---- #

# Specify the language: english, spanish, french or german
# Uncomment the line for the corresponding language

language <- "english"
# language <- "spanish"
# language <- "french"
# language <- "german"


#####################################################
################ SPECIFY PARAMETERS #################
#####################################################

# ---- SPECIFY HERE ---- #

# Set thresholds for moving low frequency words: 
freq_cutoff <- 6 # Threshold for overall frequency
trans_cutoff <- 3 # Threshold for number of transcripts in which word appears
corp_cutoff <- 2 # Threshold for number of corpora in which word appears

# The degree measure of diversity calculates how many words
# a word co-occurs with within a specified window size. This
# script will calculate degree in multiple window sizes. Specify
# the window sizes to use here. 
window_sizes <- c(5, 11, 21)

#####################################################
############## SPECIFY LANGUAGE FILES ###############
#####################################################

# These lines will select the corpus and age of acquisition files for
# the specified language.

# The corpora are available to load from an online repository. All
# urls follow the same naming convention and specify the language.
# This code uses the language specified above to get the url and load
# the corpus
corpus_url <- paste("https://language-corpora-childes.s3.us-east-2.amazonaws.com/childes_lemmatized_", language, ".rds", sep = "")


# MCDI AoA data are saved in the data folder. Get all MCDI
# AoA files and specify just the one for the specified language
mcdi_aoa_paths <- list.files(pattern = "MCDI_AoA", recursive = TRUE, full.names = TRUE)
mcdi_aoa_file <- mcdi_aoa_paths[grepl(language, mcdi_aoa_paths)]

#####################################################
################# GET COMMON NAMES ##################
#####################################################

# The data folder contains a file with a list of common names
# (e.g, Alice, Harry) that appear in the corpora. These will be
# replaced with a common token later in the script.
names_file <- list.files(pattern = "names", recursive = TRUE, full.names = TRUE)
names_to_replace <- readLines(names_file)


#####################################################
############ SET NAMES OF OUTPUT FILES ##############
#####################################################

# Create a directory (folder) in which to store the contextual diversity measures
path_to_diversity <- "data/context_diversity"
if (!dir.exists(path_to_diversity)) dir.create(path_to_diversity)

# These lines generate names for the files that will contain 
# contextual diversity measures.
degree_file <- paste(path_to_diversity, "/childes_degree_total_", language, ".rds", sep = "")
dissim_file <- paste(path_to_diversity, "/childes_dissim_total_childes50_", language, ".rds", sep = "")
LDA_file <- paste(path_to_diversity, "/CHILDES LDA_", language, ".rds", sep = "")
LDA_shuffled_file <- paste(path_to_diversity, "/CHILDES LDA shuffled_", language, ".rds", sep = "")
divergence_file <- paste(path_to_diversity, "/childes_divergence_total_", language, ".rds", sep = "")
multi_file <- paste(path_to_diversity, "/childes_multi_total_", language, ".rds", sep = "")

#####################################################
########## LOAD CHILDES & CALC FREQUENCY ############
#####################################################

# First, load file containing corpus text
# This will load a dataframe in which each row is a full CHILDES 
# transcript. Each transcript is associated with a unique id, the 
# id of the corpus, and the age of the target child.
# The corpus has been preprocessed by: 
# (1) removing punctuation aside from apostrophes in contractions
# (2) removing utterances that did not come from a caregiver
# (3) lemmatizing


# The version with stop words included:
childes_text <- readRDS(url(corpus_url))

# The next steps identify rare words and words that only occur in 
# very few transcripts or corpora (e.g., one family talked a lot
# about Star Wars, but we don't want this to influence our overall
# analysis)

# First, unnest words from transcripts to get one row per word
childes_unnested <- childes_text %>%
  # Convert each transcript into long format where each row is a word in the transcript
  tidytext::unnest_tokens(word, Text) 

# Get word frequencies and log word frequencies
childes_freq <- childes_unnested %>%
  # Count the number of time each word occurs and log transform
  dplyr::count(word, name = "freq") %>%
  dplyr::mutate(log_freq = log(freq))


# Get number of times each word occurs in transcripts and corpora
childes_doc_count <- childes_unnested %>%
  dplyr::group_by(word) %>%
  dplyr::summarise(trans_count = length(unique(id)),
                   corp_count = length(unique(corpus_id)))




low_freq <- childes_freq$word[childes_freq$freq <= freq_cutoff]
low_trans <- childes_doc_count$word[childes_doc_count$trans_count <= trans_cutoff]
low_corp <- childes_doc_count$word[childes_doc_count$corp_count <= corp_cutoff]

num_tokens_total <- nrow(childes_unnested)

childes_unnested <- childes_unnested %>%
  dplyr::filter(!(word %in% low_freq) & !(word %in% low_trans) & !(word %in% low_corp))

prop_exclude <- 1 - nrow(childes_unnested) / num_tokens_total

round(prop_exclude * 100, 2)

# Next, replace people's names with a common token
childes_unnested$word <- ifelse(childes_unnested$word %in% names_to_replace, "pname", childes_unnested$word)

# Generate a shuffled version of the corpora
shuffled_words <- sample(childes_unnested$word, nrow(childes_unnested))
childes_unnested_shuffled <- childes_unnested
childes_unnested_shuffled$word <- shuffled_words


# Re-nest the words in the transcripts back together 
childes_text <- childes_unnested %>%
  group_by(id, corpus_id, target_child_age) %>%
  dplyr::summarise(Text = paste(word, collapse=" ")) %>%
  dplyr::ungroup()

childes_text_shuffled <- childes_unnested_shuffled %>%
  group_by(id, corpus_id, target_child_age) %>%
  dplyr::summarise(Text = paste(word, collapse=" ")) %>%
  dplyr::ungroup()




#####################################################
################## LOAD MCDI DATA ###################
#####################################################

# Load age of acquisition data (calculated from MCDI data)
mcdi_aoa <- read_csv(mcdi_aoa_file, show_col_types = FALSE)


#####################################################
####### MEASURE CO-OCCURRENCE ACROSS CORPORA ########
#####################################################

# This function measures the words that each word co-occurs with
# in sliding windows

# It additionally converts the co-occurrence measure to a binary
# measure, in which words either do or do not co-occur

# This function is used to calculate degree below.

gen_pairwise <- function(window_size, input_corpus) {
  # Divide text into ngrams of window_size. Each row is a 
  # window. There is one column for the first word in a window
  # which is called "word1", and another column for the remaining
  # words which is called "context" 
  childes_windows <- input_corpus %>%
    tidytext::unnest_tokens(ngram, Text, token = "ngrams", n = window_size) %>%
    dplyr::mutate(windowID = row_number()) %>%
    dplyr::mutate(word1 = str_extract(ngram, "^[[:alpha:]]*"), # this regex extracts the first word
                  context = str_replace(ngram, "^[[:alpha:]]* ", "")) %>% # this regex extracts remaining words
    dplyr::select(!ngram) 
  
  # Convert childes_windows into a long format. In the long format,
  # the context is split into individual words, with one word per row.
  # Now, each row has a word1, and a word2 that it co-occurs with
  # within the same window
  childes_windows <- childes_windows %>%
    ungroup() %>%
    tidytext::unnest_tokens(word2, context, token = "words")
  
  # Sometimes there's a problem in which the corpora don't evenly divide
  # into windows of the specified size, so you're left with "windows" that
  # contain just one word or an extra word. The lines below remove these.
  # The code uses data.table, which makes it a lot faster than alternatives
  # though the syntax can be hard to understand. The idea is to count
  # the number of rows for each window, and keep only those that have the
  # correct number. The correct number is window_size - 1, since each 
  # window is in a format where word1 is in one column, and each of its 
  # word2's are in another column.
  childes_windows <- as.data.table(childes_windows)
  childes_windows <- childes_windows[ , num_words := .N, by = .(windowID)][ num_words == window_size - 1 ]
  childes_windows <- childes_windows[ , num_words:=NULL]
  
  # Next, count co-occurrence: i.e., the number of times each word 
  # appears before each other word in the same window. These
  # counts are in a column called co_count
  childes_pairwise <- childes_windows %>%
    dplyr::group_by(word1) %>%
    dplyr::count(word2, name = "co_count")
  
  # Add a column that binarizes co-count: i.e., if co-occurrence is 
  # above a threshold, the value is 1, if it's below, 0
  # First, set threshold
  co_threshold <- 1
  
  childes_pairwise$co_binary <- ifelse(childes_pairwise$co_count >= co_threshold, 1, 0)
  
  return(childes_pairwise)
}

#####################################################
################# CALCULATE DEGREE ##################
#####################################################

# The first measure of diversity is "degree". This is simply
# the number of unique words that a word co-occurs with, within
# a sliding window of a given size.
# It is adapted from Hills et al. (2010)

# The function below calculates degree within a sliding window size,
# for either the original or the shuffled corpus.

gen_degree <- function(window_size, input_corpus) {
  
  # Check whether input_corpus is original or shuffled
  corpus_name <- deparse(substitute(input_corpus))
  corpus_type <- ifelse(grepl("shuffled", corpus_name), "shuffled", "original")
  
  # Measure co-occurrence using function above
  childes_pairwise <- gen_pairwise(window_size = window_size, input_corpus = input_corpus)
  
  # Next, spread into word-word co-occurrence matrix and
  # update NA values to 0
  childes_matrix <- childes_pairwise %>%
    dplyr::select(!co_count) %>%
    pivot_wider(names_from = word2, values_from = co_binary, values_fill = list(co_binary = 0))
  
  # store word1 and word2
  word1 <- childes_matrix$word1
  word2 <- names(childes_matrix[names(childes_matrix) != "word1"])
  
  # Next, get just the binary matrix of 0s and 1s to use for calculating
  # row and col sums. These are the number of unique words that each word
  # co-occurs with.
  binary_matrix <- childes_matrix %>%
    dplyr::ungroup() %>%
    dplyr::select(!word1)
  
  # Calculate rowsums and colsums, the log of these values, then combine
  childes_degree_row <- data.frame(word = word1,
                                   degree_row = rowSums(binary_matrix),
                                   log_degree_row = log(rowSums(binary_matrix)))
  
  childes_degree_col <- data.frame(word = word2, 
                                   degree_col = colSums(binary_matrix),
                                   log_degree_col = log(colSums(binary_matrix)))
  
  childes_degree <- inner_join(childes_degree_row, childes_degree_col)
  
  # Calculate sum of row and columns: i.e., total number of 
  # co-occurring words
  childes_degree$degree_sum <- childes_degree$degree_row + childes_degree$degree_col
  childes_degree$log_degree_sum <- log(childes_degree$degree_sum)
  
  # Analyses focus just on sum of row and col (i.e., total number
  # of co-occurring words). So, remove the row and col values.
  childes_degree <- childes_degree %>%
    dplyr::select(!contains("row") & !contains("col") )
  
  # Add column indicating window size
  childes_degree$window_size <- window_size
  
  # If this value is measured from the shuffled corpus, add
  # this info to the name of the column containing degree values
  if(corpus_type == "shuffled") {
    childes_degree <- rename_with(childes_degree, ~ gsub("sum", "sum_shuffled", .x))
  }
  
  return(childes_degree)
}

# Calculate degree across all window sizes for both the original and shuffled corpora,
# then combine
childes_degree <- purrr::map_dfr(window_sizes, ~gen_degree(.x, input_corpus = childes_text))
childes_degree_shuffled <- purrr::map_dfr(window_sizes, ~gen_degree(.x, input_corpus = childes_text_shuffled))

childes_degree_total <- left_join(childes_degree, childes_degree_shuffled)

# Look:
head(childes_degree_total)

# Store for future use:
saveRDS(childes_degree_total, degree_file)

#####################################################
############ CALCULATE TOPIC DIVERGENCE #############
#####################################################

# For the LDA analysis, we need to create a "document term matrix"
# This is an object that lists each "document", and includes the number 
# of times each word (i.e., "term") occurred in that document
# In Roy et al., documents were 10-minute "episodes" from the
# recordings.
# Here, we are treating individual CHILDES transcripts as documents.

gen_dtm <- function(input_corpus) {

  childes_doc_term <- input_corpus %>%
    dplyr::group_by(id) %>%
    dplyr::count(word, name = "count")


  # There is a special format for document-term-matrices used
  # by LDA functions. We can convert our matrix into this
  # format using cast_dtm
  childes_dtm <- childes_doc_term %>%
    cast_dtm(id, word, count)

  return(childes_dtm)
}

childes_dtm <- gen_dtm(childes_unnested)
childes_dtm_shuffled <- gen_dtm(childes_unnested_shuffled)


# Conduct LDA to identify a specified set of latent "topics"
# The LDA function below creates an object that includes both:
# (1) associations of topics with words
# (2) associations of topics with documents (transcripts)

# This takes a while! Save LDA object to reload later
# so that you don't need to run it each time.
# If you've already run it, comment-out the childes_lda and
# saveRDS lines, and un-comment the readRDS to read in the
# stored files. 

childes_lda <- LDA(childes_dtm, k = 25, control = list(seed = 1234))
saveRDS(childes_lda, LDA_file)

childes_lda_shuffled <- LDA(childes_dtm_shuffled, k = 25, control = list(seed = 1234))
saveRDS(childes_lda_shuffled, LDA_shuffled_file)

# childes_lda <- readRDS(LDA_file)
# childes_lda_shuffled <- readRDS(LDA_shuffled_file)

# The code below implements Roy et al.'s calculation of
# divergence - i.e., the extent to which a word occurs in a 
# wider range of topics than the overall baseline distribution.
# There are two differences here from the original calculation
# because Roy et al. were analyzing input to a single 
# child, whereas we're using CHILDES:
# (1) Calculate across all corpora, not just those before age
#     of first production, because we do not know the age of
#     first production of words for children in CHILDES
# (2) Instead of 10-min episodes, documents are transcripts

# This function calculates KL divergence for one word
calc_word_divergence <- function(input_word) {
  word_probs <- input_word %>%
    dplyr::ungroup()   %>%
    dplyr::select(word_distribution, bg_distribution) 
  
  word_matrix <- t(as.matrix(word_probs))
  
  word_kl <- KL(word_matrix)
  
  output <- data.frame(divergence = word_kl)
  
  return(output)
}


# This function calculates divergence for all words
# It includes an argument (aoa_threshold) in which you can specify 
# whether to calculate divergence across all transcripts, or only
# transcripts of input to children up to the age of acquisition
# of the word 
gen_divergence <- function(input_lda, input_dtm, aoa_threshold = FALSE) {
  
  # Check whether the input_corpus is original or shuffled
  corpus_name <- deparse(substitute(input_lda))
  corpus_type <- ifelse(grepl("shuffled", corpus_name), "shuffled", "original")
  
  # First, get each document's mixture of topics
  doc_topics <- tidy(input_lda, matrix = "gamma") %>%
    dplyr::rename(id = document, topic_prob = gamma) %>%
    dplyr::mutate(id = as.numeric(id))
  
  
  # Next, join with the info about the number of times words
  # occurred in transcripts
  doc_term <- tidy(input_dtm) %>%
    dplyr::rename(id = document, word = term) %>%
    dplyr::mutate(id = as.numeric(id))
  
  doc_word_topics <- left_join(doc_topics, doc_term)
  
  
  # Optional: Remove occurrences of word after aoa
  if(aoa_threshold) {
    
    # Combine with info about target_child_age for each transcript
    age_key <- childes_text %>%
      dplyr::select(id, target_child_age) %>%
      distinct()
    
    doc_word_topics <- left_join(doc_word_topics, age_key)
    
    # Combine with info about aoa
    aoa_key <- mcdi_aoa %>%
      dplyr::select(lemma, aoa_fit) %>%
      dplyr::rename(word = lemma)
    
    doc_word_topics <- left_join(doc_word_topics, aoa_key)
    
    # Remove occurrences of words after aoa
    doc_word_topics <- doc_word_topics %>%
      dplyr::filter(!is.na(aoa_fit)) %>%
      dplyr::filter(target_child_age < aoa_fit)
  }
  
  
  
  # Now, carry out this step from Roy et al: "apportion caregiver uses of 
  # the target word during the episode to topics according to 
  # the episode's topic mixture and then summed and  normalized 
  # to obtain the topic distribution for the word."
  word_topic_distribution <- doc_word_topics %>%
    dplyr::group_by(word, topic) %>%
    dplyr::summarise(word_distribution = sum(topic_prob * count) / sum(count)) %>%
    dplyr::arrange(topic, desc(word_distribution))
  
  
  # Next, follow Roy et al.'s approach to calculating the 
  # "background" distribution for topics:
  
  # The background topic distribution is the weighted average of 
  # all of the episode topic distributions, weighted by the number 
  # of words in each episode. 
  doc_topic_distribution <- doc_word_topics %>%
    dplyr::group_by(id, topic, topic_prob) %>%
    dplyr::summarise(num_words = sum(count))
  
  bg_distribution <- doc_topic_distribution %>%
    dplyr::group_by(topic) %>%
    dplyr::summarise(bg_distribution = sum(topic_prob * num_words) / sum(num_words))
  
  # Combine word and topic distributions
  word_topic_distribution <- left_join(word_topic_distribution, bg_distribution)
  
  # Now you have the topic distribution for each word, and the
  # background distribution
  
  # Apply the KL function to each word
  word_divergence <- word_topic_distribution %>%
    group_by(word) %>% do(calc_word_divergence(.))
  
  # Roy et al use the log of this value
  word_divergence$log_divergence <- log(word_divergence$divergence)
  
  # To align with diversity measures, in which higher values = higher
  # diversity, get the inverse of log_divergence
  word_divergence$log_inv_divergence <- word_divergence$log_divergence * -1
  
  
  # If calculated on the shuffled corpus, add this info to the
  # name of the divergence column
  if(corpus_type == "shuffled") {
    word_divergence <- rename_with(word_divergence, ~ gsub("divergence", "divergence_shuffled", .x))
  }
  
  return(word_divergence)
}

# Calculate divergence across all window sizes for both the original and shuffled corpora,
# then combine
childes_divergence <- gen_divergence(childes_lda, childes_dtm, aoa_threshold = TRUE)
childes_divergence_shuffled <- gen_divergence(childes_lda_shuffled, childes_dtm_shuffled)

childes_divergence_total <- left_join(childes_divergence, childes_divergence_shuffled)

# Store for future use:
saveRDS(childes_divergence_total, divergence_file)

#####################################################
############## MERGE WITH FREQUENCY #################
#####################################################

# Uncomment if you have already generated these files
# childes_degree_total <- readRDS(degree_file)
# childes_divergence_total <- readRDS(divergence_file)

# Combine measures of word frequency and degree
childes_stats <- inner_join(inner_join(childes_degree_total, childes_divergence_total), childes_freq)

childes_stats <- childes_stats %>% 
  mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

# Remove columns used as intermediate steps in calculations
childes_stats <- childes_stats %>%
  dplyr::select(!c(degree_sum, degree_sum_shuffled, divergence, log_divergence, divergence_shuffled, log_divergence_shuffled))


# Simplify names of diversity columns
childes_stats <- childes_stats %>%
  dplyr::rename(degree = log_degree_sum, degree_shuffled = log_degree_sum_shuffled,
                divergence = log_inv_divergence, divergence_shuffled = log_inv_divergence_shuffled)

# Store for future use
saveRDS(childes_stats, multi_file)
