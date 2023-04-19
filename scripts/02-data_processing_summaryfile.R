#### Preamble ####
# Purpose: Creating summary dataset for all 21 datasheets
# Author: Eshta Bhardwaj
# Date: April 21 2023
# Contact: eshta.bhardwaj@mail.utoronto.ca
# License: MIT
# Pre-requisites: Must already individual dataset for each datasheet

#### Workspace set-up ####
library(tidyverse)

#### read in all datasheets datasets ####

datasheet1_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet1.csv",
    show_col_types = FALSE,
  )

datasheet2_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet2.csv",
    show_col_types = FALSE,
  )


datasheet3_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet3.csv",
    show_col_types = FALSE,
  )

datasheet4_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet4.csv",
    show_col_types = FALSE,
  )

datasheet5_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet5.csv",
    show_col_types = FALSE,
  )

datasheet6_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet6.csv",
    show_col_types = FALSE,
  )

# datasheet14_csv <-
#  read_csv(
#    file = "inputs/data/cleaned/datasheet14.csv",
#    show_col_types = FALSE,
#  )

datasheet7_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet7.csv",
    show_col_types = FALSE,
  )


#### define vars for summary dataset  ####

## create list for completion pct
question_completion_numerator_1 = length(which(datasheet1_csv$completion == "Yes"))
question_completion_denominator_1 = 1
if (length(which(datasheet1_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_1 = 50
} else {question_completion_denominator_1 = 57}
question_completion_pct_1 = (question_completion_numerator_1/question_completion_denominator_1)*100

question_completion_numerator_2 = length(which(datasheet2_csv$completion == "Yes"))
question_completion_denominator_2 = 1
if (length(which(datasheet2_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_2 = 50
} else {question_completion_denominator_2 = 57}
question_completion_pct_2 = (question_completion_numerator_2/question_completion_denominator_2)*100

question_completion_numerator_3 = length(which(datasheet3_csv$completion == "Yes"))
question_completion_denominator_3 = 1
if (length(which(datasheet3_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_3 = 50
} else {question_completion_denominator_3 = 57}
question_completion_pct_3 = (question_completion_numerator_3/question_completion_denominator_3)*100

question_completion_numerator_4 = length(which(datasheet4_csv$completion == "Yes"))
question_completion_denominator_4 = 1
if (length(which(datasheet4_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_4 = 50
} else {question_completion_denominator_4 = 57}
question_completion_pct_4 = (question_completion_numerator_4/question_completion_denominator_4)*100

question_completion_numerator_5 = length(which(datasheet5_csv$completion == "Yes"))
question_completion_denominator_5 = 1
if (length(which(datasheet5_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_5 = 50
} else {question_completion_denominator_5 = 57}
question_completion_pct_5 = (question_completion_numerator_5/question_completion_denominator_5)*100

question_completion_numerator_6 = length(which(datasheet6_csv$completion == "Yes"))
question_completion_denominator_6 = 1
if (length(which(datasheet6_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_6 = 50
} else {question_completion_denominator_6 = 57}
question_completion_pct_6 = (question_completion_numerator_6/question_completion_denominator_6)*100


question_completion_numerator_7 = length(which(datasheet7_csv$completion == "Yes"))
question_completion_denominator_7 = 1
if (length(which(datasheet7_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_7 = 50
} else {question_completion_denominator_7 = 57}
question_completion_pct_7 = (question_completion_numerator_7/question_completion_denominator_7)*100

## combine top 5 words for each datasheet

# initialize var
overall_top_5_words_1_list <- list()

# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet1_csv)){
  if (datasheet1_csv$length_words[i] > 10) {
    overall_top_5_words_1_list[i] <- (datasheet1_csv$top_5_frequent_words[i])
    i=i+1
  }
  
  else {i=i+1}
}
# this unlists the list of lists
overall_top_5_words_1_unlist <- unlist(overall_top_5_words_1_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_1_unlist2 <- strsplit(overall_top_5_words_1_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_1_unlist3 <- unlist(overall_top_5_words_1_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_1_table <- sort(table(overall_top_5_words_1_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_1_cols <- names(overall_top_5_words_1_table)
overall_top_5_words_1 <- paste(overall_top_5_words_1_cols[1], overall_top_5_words_1_cols[2], 
                               overall_top_5_words_1_cols[3], overall_top_5_words_1_cols[4], overall_top_5_words_1_cols[5],
                               sep= ', ')


# initialize var
overall_top_5_words_2_list <- list()

# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet2_csv)){
  if (datasheet2_csv$length_words[i] > 10) {
    overall_top_5_words_2_list[i] <- (datasheet2_csv$top_5_frequent_words[i])
    i=i+1
  }
  
  else {i=i+1}
}
# this unlists the list of lists
overall_top_5_words_2_unlist <- unlist(overall_top_5_words_2_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_2_unlist2 <- strsplit(overall_top_5_words_2_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_2_unlsit3 <- unlist(overall_top_5_words_2_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_2_table <- sort(table(overall_top_5_words_2_unlsit3),decreasing=TRUE)[1:5]
overall_top_5_words_2_cols <- names(overall_top_5_words_2_table)
overall_top_5_words_2 <- paste(overall_top_5_words_2_cols[1], overall_top_5_words_2_cols[2], 
                               overall_top_5_words_2_cols[3], overall_top_5_words_2_cols[4], 
                               overall_top_5_words_2_cols[5], sep=", ")

# initialize var
overall_top_5_words_3_list <- list()

# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet3_csv)){
  if (datasheet3_csv$length_words[i] > 10) {
    overall_top_5_words_3_list[i] <- (datasheet3_csv$top_5_frequent_words[i])
    i=i+1
  }
  
  else {i=i+1}
}
# this unlists the list of lists
overall_top_5_words_3_unlist <- unlist(overall_top_5_words_3_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_3_unlist2 <- strsplit(overall_top_5_words_3_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_3_unlist3 <- unlist(overall_top_5_words_3_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_3_table <- sort(table(overall_top_5_words_3_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_3_cols <- names(overall_top_5_words_3_table)
overall_top_5_words_3 <- paste(overall_top_5_words_3_cols[1], overall_top_5_words_3_cols[2], 
                               overall_top_5_words_3_cols[3], overall_top_5_words_3_cols[4], overall_top_5_words_3_cols[5],
                               sep= ', ')

# initialize var
overall_top_5_words_4_list <- list()

# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet4_csv)){
  if (datasheet4_csv$length_words[i] > 10) {
    overall_top_5_words_4_list[i] <- (datasheet4_csv$top_5_frequent_words[i])
    i=i+1
  }
  
  else {i=i+1}
}
# this unlists the list of lists
overall_top_5_words_4_unlist <- unlist(overall_top_5_words_4_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_4_unlist2 <- strsplit(overall_top_5_words_4_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_4_unlist3 <- unlist(overall_top_5_words_4_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_4_table <- sort(table(overall_top_5_words_4_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_4_cols <- names(overall_top_5_words_4_table)
overall_top_5_words_4 <- paste(overall_top_5_words_4_cols[1], overall_top_5_words_4_cols[2], 
                               overall_top_5_words_4_cols[3], overall_top_5_words_4_cols[4], overall_top_5_words_4_cols[5],
                               sep= ', ')

# initialize var
overall_top_5_words_5_list <- list()

# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet5_csv)){
  if (datasheet5_csv$length_words[i] > 10) {
    overall_top_5_words_5_list[i] <- (datasheet5_csv$top_5_frequent_words[i])
    i=i+1
  }
  
  else {i=i+1}
}
# this unlists the list of lists
overall_top_5_words_5_unlist <- unlist(overall_top_5_words_5_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_5_unlist2 <- strsplit(overall_top_5_words_5_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_5_unlist3 <- unlist(overall_top_5_words_5_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_5_table <- sort(table(overall_top_5_words_5_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_5_cols <- names(overall_top_5_words_5_table)
overall_top_5_words_5 <- paste(overall_top_5_words_5_cols[1], overall_top_5_words_5_cols[2], 
                               overall_top_5_words_5_cols[3], overall_top_5_words_5_cols[4], overall_top_5_words_5_cols[5],
                               sep= ', ')

# initialize var
overall_top_5_words_6_list <- list()

# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet6_csv)){
  if (datasheet6_csv$length_words[i] > 10) {
    overall_top_5_words_6_list[i] <- (datasheet6_csv$top_5_frequent_words[i])
    i=i+1
  }
  
  else {i=i+1}
}
# this unlists the list of lists
overall_top_5_words_6_unlist <- unlist(overall_top_5_words_6_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_6_unlist2 <- strsplit(overall_top_5_words_6_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_6_unlist3 <- unlist(overall_top_5_words_6_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_6_table <- sort(table(overall_top_5_words_6_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_6_cols <- names(overall_top_5_words_6_table)
overall_top_5_words_6 <- paste(overall_top_5_words_6_cols[1], overall_top_5_words_6_cols[2], 
                               overall_top_5_words_6_cols[3], overall_top_5_words_6_cols[4], overall_top_5_words_6_cols[5],
                               sep= ', ')

# initialize var
overall_top_5_words_7_list <- list()

# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet7_csv)){
  if (datasheet7_csv$length_words[i] > 10) {
    overall_top_5_words_7_list[i] <- (datasheet7_csv$top_5_frequent_words[i])
    i=i+1
  }
  
  else {i=i+1}
}
# this unlists the list of lists
overall_top_5_words_7_unlist <- unlist(overall_top_5_words_7_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_7_unlist2 <- strsplit(overall_top_5_words_7_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_7_unlist3 <- unlist(overall_top_5_words_7_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_7_table <- sort(table(overall_top_5_words_7_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_7_cols <- names(overall_top_5_words_7_table)
overall_top_5_words_7 <- paste(overall_top_5_words_7_cols[1], overall_top_5_words_7_cols[2], 
                               overall_top_5_words_7_cols[3], overall_top_5_words_7_cols[4], overall_top_5_words_7_cols[5],
                               sep= ', ')


## min lists
# source: https://stackoverflow.com/questions/6347356/creating-a-comma-separated-vector
list1 <- c(datasheet1_csv$qnum[datasheet1_csv$length_words==min(datasheet1_csv$length_words)])
list1_delim <- c(paste(list1, collapse = ", "))
list2 <- c(datasheet2_csv$qnum[datasheet2_csv$length_words==min(datasheet2_csv$length_words)])
list2_delim <- c(paste(list2, collapse = ", "))
list3 <- c(datasheet3_csv$qnum[datasheet3_csv$length_words==min(datasheet3_csv$length_words)])
list3_delim <- c(paste(list3, collapse = ", "))
list4 <- c(datasheet4_csv$qnum[datasheet4_csv$length_words==min(datasheet4_csv$length_words)])
list4_delim <- c(paste(list4, collapse = ", "))
list5 <- c(datasheet5_csv$qnum[datasheet5_csv$length_words==min(datasheet5_csv$length_words)])
list5_delim <- c(paste(list5, collapse = ", "))
list6 <- c(datasheet6_csv$qnum[datasheet6_csv$length_words==min(datasheet6_csv$length_words)])
list6_delim <- c(paste(list6, collapse = ", "))
list7 <- c(datasheet7_csv$qnum[datasheet7_csv$length_words==min(datasheet7_csv$length_words)])
list7_delim <- c(paste(list7, collapse = ", "))

# do this so that any values with only one qnum is also converted to string
list1_delim <- toString(list1_delim)
list2_delim <- toString(list2_delim)
list3_delim <- toString(list3_delim)
list4_delim <- toString(list4_delim)
list5_delim <- toString(list5_delim)
list6_delim <- toString(list6_delim)
list7_delim <- toString(list7_delim)

#### summary df without score ####
summary_df_wo_score <- data.frame(datasheet_ID=c(1,2,3,4,5,6,7),
                         title_short=c("sandbox_dna",
                                       "garments",
                                       "PASS",
                                       "spatial_apartheid",
                                       "CSFCube",
                                       "digital_experiments",
                                       "PROCAT"),
                         total_length_wrds=c(sum(datasheet1_csv$length_words),
                                             sum(datasheet2_csv$length_words),
                                             sum(datasheet3_csv$length_words),
                                             sum(datasheet4_csv$length_words),
                                             sum(datasheet5_csv$length_words),
                                             sum(datasheet6_csv$length_words),
                                             sum(datasheet7_csv$length_words)),
                         question_completion_pct=c(question_completion_pct_1,
                                                   question_completion_pct_2,
                                                   question_completion_pct_3,
                                                   question_completion_pct_4,
                                                   question_completion_pct_5,
                                                   question_completion_pct_6,
                                                   question_completion_pct_7),
                         avg_response_length=c(mean(datasheet1_csv$length_words),
                                               mean(datasheet2_csv$length_words),
                                               mean(datasheet3_csv$length_words),
                                               mean(datasheet4_csv$length_words),
                                               mean(datasheet5_csv$length_words),
                                               mean(datasheet6_csv$length_words),
                                               mean(datasheet7_csv$length_words)),
                         max_response_length=c(max(datasheet1_csv$length_words),
                                               max(datasheet2_csv$length_words),
                                               max(datasheet3_csv$length_words),
                                               max(datasheet4_csv$length_words),
                                               max(datasheet5_csv$length_words),
                                               max(datasheet6_csv$length_words),
                                               max(datasheet7_csv$length_words)),
                         max_response_qnum=c(datasheet1_csv$qnum[datasheet1_csv$length_words==max(datasheet1_csv$length_words)],
                                             datasheet2_csv$qnum[datasheet2_csv$length_words==max(datasheet2_csv$length_words)],
                                             datasheet3_csv$qnum[datasheet3_csv$length_words==max(datasheet3_csv$length_words)],
                                             datasheet4_csv$qnum[datasheet4_csv$length_words==max(datasheet4_csv$length_words)],
                                             datasheet5_csv$qnum[datasheet5_csv$length_words==max(datasheet5_csv$length_words)],
                                             datasheet6_csv$qnum[datasheet6_csv$length_words==max(datasheet6_csv$length_words)],
                                             datasheet7_csv$qnum[datasheet7_csv$length_words==max(datasheet7_csv$length_words)]),
                         min_response_length=c(min(datasheet1_csv$length_words),
                                               min(datasheet2_csv$length_words),
                                               min(datasheet3_csv$length_words),
                                               min(datasheet4_csv$length_words),
                                               min(datasheet5_csv$length_words),
                                               min(datasheet6_csv$length_words),
                                               min(datasheet7_csv$length_words)),
                         min_response_qnum=c(list1_delim, list2_delim,
                                             list3_delim, list4_delim,
                                             list5_delim, list6_delim,
                                             list7_delim),
                         overall_top_5_words=c(overall_top_5_words_1,overall_top_5_words_2,
                                               overall_top_5_words_3, overall_top_5_words_4,
                                               overall_top_5_words_5, overall_top_5_words_6,
                                               overall_top_5_words_7),
                         score_pct=c(1,2,3,4,5,6,7))


## calculate score 
# initialize vars
total_length_score = 0
completion_score = 0
avg_length_score = 0

# loop through df to calculate total score
for (i in 1:nrow(summary_df_wo_score)){
  if (summary_df_wo_score$total_length_wrds[i] < 1000) {
    total_length_score = 0
  }
  else if (summary_df_wo_score$total_length_wrds[i] > 1000 && summary_df_wo_score$total_length_wrds[i] <= 1500) {
    total_length_score = 1
  }
  else if (summary_df_wo_score$total_length_wrds[i] > 1500 && summary_df_wo_score$total_length_wrds[i] <= 2000) {
    total_length_score = 2
  }
  else if (summary_df_wo_score$total_length_wrds[i] > 2000 && summary_df_wo_score$total_length_wrds[i] <= 2500) {
    total_length_score = 3
  }
  else if (summary_df_wo_score$total_length_wrds[i] > 2500 && summary_df_wo_score$total_length_wrds[i] <= 3000) {
    total_length_score = 4
  }
  else {
    total_length_score = 5
  }
  
  if (summary_df_wo_score$question_completion_pct[i] >= 0 && summary_df_wo_score$question_completion_pct[i] <= 50) {
    completion_score = 1
  }
  else if (summary_df_wo_score$question_completion_pct[i] > 50 && summary_df_wo_score$question_completion_pct[i] <= 75) {
    completion_score = 2
  }
  else if (summary_df_wo_score$question_completion_pct[i] > 75 && summary_df_wo_score$question_completion_pct[i] <= 85) {
    completion_score = 3
  }
  else if (summary_df_wo_score$question_completion_pct[i] > 85 && summary_df_wo_score$question_completion_pct[i] <= 95) {
    completion_score = 4
  }
  else {
    completion_score = 5
  }
  
  if (summary_df_wo_score$avg_response_length[i] >=0 && summary_df_wo_score$avg_response_length[i] <= 20) {
    avg_length_score = 1
  }
  else if (summary_df_wo_score$avg_response_length[i] > 20 && summary_df_wo_score$avg_response_length[i] <= 30) {
    avg_length_score = 2
  }
  else if (summary_df_wo_score$avg_response_length[i] > 30 && summary_df_wo_score$avg_response_length[i] <= 40) {
    avg_length_score = 3
  }
  else if (summary_df_wo_score$avg_response_length[i] > 40 && summary_df_wo_score$avg_response_length[i] <= 50) {
    avg_length_score = 4
  }
  else {
    avg_length_score = 5
  }
  summary_df_wo_score$score_pct[i] <- (((total_length_score*0.33)+(completion_score*0.33)+(avg_length_score*0.33))/5)*100
  i=i+1
}

## summary df with score
summary_df <- summary_df_wo_score

## write final df to csv
write.csv(summary_df, file = "./inputs/data/cleaned/summary_all_datasheets.csv", row.names=FALSE)

