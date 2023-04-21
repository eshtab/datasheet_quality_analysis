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

datasheet14_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet14.csv",
    show_col_types = FALSE,
  )

datasheet7_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet7.csv",
    show_col_types = FALSE,
  )

datasheet8_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet8.csv",
    show_col_types = FALSE,
  )

datasheet9_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet9.csv",
    show_col_types = FALSE,
  )

datasheet10_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet10.csv",
    show_col_types = FALSE,
  )

datasheet11_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet11.csv",
    show_col_types = FALSE,
  )


datasheet12_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet12.csv",
    show_col_types = FALSE,
  )

datasheet13_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet13.csv",
    show_col_types = FALSE,
  )

datasheet14_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet14.csv",
    show_col_types = FALSE,
  )

datasheet15_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet15.csv",
    show_col_types = FALSE,
  )

datasheet16_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet16.csv",
    show_col_types = FALSE,
  )

datasheet17_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet17.csv",
    show_col_types = FALSE,
  )

datasheet18_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet18.csv",
    show_col_types = FALSE,
  )

datasheet19_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet19.csv",
    show_col_types = FALSE,
  )

datasheet20_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet20.csv",
    show_col_types = FALSE,
  )

datasheet21_csv <-
  read_csv(
    file = "inputs/data/cleaned/datasheet21.csv",
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

question_completion_numerator_8 = length(which(datasheet8_csv$completion == "Yes"))
question_completion_denominator_8 = 1
if (length(which(datasheet8_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_8 = 50
} else {question_completion_denominator_8 = 57}
question_completion_pct_8 = (question_completion_numerator_8/question_completion_denominator_8)*100

question_completion_numerator_9 = length(which(datasheet9_csv$completion == "Yes"))
question_completion_denominator_9 = 1
if (length(which(datasheet9_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_9 = 50
} else {question_completion_denominator_9 = 57}
question_completion_pct_9 = (question_completion_numerator_9/question_completion_denominator_9)*100

question_completion_numerator_10 = length(which(datasheet10_csv$completion == "Yes"))
question_completion_denominator_10 = 1
if (length(which(datasheet10_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_10 = 50
} else {question_completion_denominator_10 = 57}
question_completion_pct_10 = (question_completion_numerator_10/question_completion_denominator_10)*100

question_completion_numerator_11 = length(which(datasheet11_csv$completion == "Yes"))
question_completion_denominator_11 = 1
if (length(which(datasheet11_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_11 = 50
} else {question_completion_denominator_11 = 57}
question_completion_pct_11 = (question_completion_numerator_11/question_completion_denominator_11)*100

question_completion_numerator_12 = length(which(datasheet12_csv$completion == "Yes"))
question_completion_denominator_12 = 1
if (length(which(datasheet12_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_12 = 50
} else {question_completion_denominator_12 = 57}
question_completion_pct_12 = (question_completion_numerator_12/question_completion_denominator_12)*100

question_completion_numerator_13 = length(which(datasheet13_csv$completion == "Yes"))
question_completion_denominator_13 = 1
if (length(which(datasheet13_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_13 = 50
} else {question_completion_denominator_13 = 57}
question_completion_pct_13 = (question_completion_numerator_13/question_completion_denominator_13)*100

question_completion_numerator_14 = length(which(datasheet14_csv$completion == "Yes"))
question_completion_denominator_14 = 1
if (length(which(datasheet14_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_14 = 50
} else {question_completion_denominator_14 = 57}
question_completion_pct_14 = (question_completion_numerator_14/question_completion_denominator_14)*100

question_completion_numerator_15 = length(which(datasheet15_csv$completion == "Yes"))
question_completion_denominator_15 = 1
if (length(which(datasheet15_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_15 = 50
} else {question_completion_denominator_15 = 57}
question_completion_pct_15 = (question_completion_numerator_15/question_completion_denominator_15)*100

question_completion_numerator_16 = length(which(datasheet16_csv$completion == "Yes"))
question_completion_denominator_16 = 1
if (length(which(datasheet16_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_16 = 50
} else {question_completion_denominator_16 = 57}
question_completion_pct_16 = (question_completion_numerator_16/question_completion_denominator_16)*100

question_completion_numerator_17 = length(which(datasheet17_csv$completion == "Yes"))
question_completion_denominator_17 = 1
if (length(which(datasheet17_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_17 = 50
} else {question_completion_denominator_17 = 57}
question_completion_pct_17 = (question_completion_numerator_17/question_completion_denominator_17)*100

question_completion_numerator_18 = length(which(datasheet18_csv$completion == "Yes"))
question_completion_denominator_18 = 1
if (length(which(datasheet18_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_18 = 50
} else {question_completion_denominator_18 = 57}
question_completion_pct_18 = (question_completion_numerator_18/question_completion_denominator_18)*100

question_completion_numerator_19 = length(which(datasheet19_csv$completion == "Yes"))
question_completion_denominator_19 = 1
if (length(which(datasheet19_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_19 = 50
} else {question_completion_denominator_19 = 57}
question_completion_pct_19 = (question_completion_numerator_19/question_completion_denominator_19)*100

question_completion_numerator_20 = length(which(datasheet20_csv$completion == "Yes"))
question_completion_denominator_20 = 1
if (length(which(datasheet20_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_20 = 50
} else {question_completion_denominator_20 = 57}
question_completion_pct_20 = (question_completion_numerator_20/question_completion_denominator_20)*100

question_completion_numerator_21 = length(which(datasheet21_csv$completion == "Yes"))
question_completion_denominator_21 = 1
if (length(which(datasheet21_csv$datasheet_version[1] == 2018))){
  question_completion_denominator_21 = 50
} else {question_completion_denominator_21 = 57}
question_completion_pct_21 = (question_completion_numerator_21/question_completion_denominator_21)*100

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
# remove null indices from list
overall_top_5_words_1_list <- 
  overall_top_5_words_1_list[lengths(overall_top_5_words_1_list) != 0]
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
# remove null indices from list
overall_top_5_words_2_list <- 
  overall_top_5_words_2_list[lengths(overall_top_5_words_2_list) != 0]

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
# remove null indices from list
overall_top_5_words_3_list <- 
  overall_top_5_words_3_list[lengths(overall_top_5_words_3_list) != 0]

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
# remove null indices from list
overall_top_5_words_4_list <- 
  overall_top_5_words_4_list[lengths(overall_top_5_words_4_list) != 0]

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

# remove null indices from list
overall_top_5_words_5_list <- 
  overall_top_5_words_5_list[lengths(overall_top_5_words_5_list) != 0]

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
# remove null indices from list
overall_top_5_words_6_list <- 
  overall_top_5_words_6_list[lengths(overall_top_5_words_6_list) != 0]

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

# remove null indices from list
overall_top_5_words_7_list <- 
  overall_top_5_words_7_list[lengths(overall_top_5_words_7_list) != 0]

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

# initialize var
overall_top_5_words_8_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet8_csv)){
  if (datasheet8_csv$length_words[i] > 10) {
    overall_top_5_words_8_list[i] <- (datasheet8_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_8_list <- 
  overall_top_5_words_8_list[lengths(overall_top_5_words_8_list) != 0]
# this unlists the list of lists
overall_top_5_words_8_unlist <- unlist(overall_top_5_words_8_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_8_unlist2 <- strsplit(overall_top_5_words_8_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_8_unlist3 <- unlist(overall_top_5_words_8_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_8_table <- sort(table(overall_top_5_words_8_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_8_cols <- names(overall_top_5_words_8_table)
overall_top_5_words_8 <- paste(overall_top_5_words_8_cols[1], overall_top_5_words_8_cols[2], 
                               overall_top_5_words_8_cols[3], overall_top_5_words_8_cols[4], 
                               overall_top_5_words_8_cols[5],
                               sep= ', ')

# initialize var
overall_top_5_words_9_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet9_csv)){
  if (datasheet9_csv$length_words[i] > 10) {
    overall_top_5_words_9_list[i] <- (datasheet9_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_9_list <- 
  overall_top_5_words_9_list[lengths(overall_top_5_words_9_list) != 0]
# this unlists the list of lists
overall_top_5_words_9_unlist <- unlist(overall_top_5_words_9_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_9_unlist2 <- strsplit(overall_top_5_words_9_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_9_unlist3 <- unlist(overall_top_5_words_9_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_9_table <- sort(table(overall_top_5_words_9_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_9_cols <- names(overall_top_5_words_9_table)
overall_top_5_words_9 <- paste(overall_top_5_words_9_cols[1], overall_top_5_words_9_cols[2], 
                               overall_top_5_words_9_cols[3], overall_top_5_words_9_cols[4], 
                               overall_top_5_words_9_cols[5],
                               sep= ', ')

# initialize var
overall_top_5_words_10_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet10_csv)){
  if (datasheet10_csv$length_words[i] > 10) {
    overall_top_5_words_10_list[i] <- (datasheet10_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_10_list <- 
  overall_top_5_words_10_list[lengths(overall_top_5_words_10_list) != 0]
# this unlists the list of lists
overall_top_5_words_10_unlist <- unlist(overall_top_5_words_10_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_10_unlist2 <- strsplit(overall_top_5_words_10_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_10_unlist3 <- unlist(overall_top_5_words_10_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_10_table <- sort(table(overall_top_5_words_10_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_10_cols <- names(overall_top_5_words_10_table)
overall_top_5_words_10 <- paste(overall_top_5_words_10_cols[1], overall_top_5_words_10_cols[2], 
                               overall_top_5_words_10_cols[3], overall_top_5_words_10_cols[4], 
                               overall_top_5_words_10_cols[5],
                               sep= ', ')

# initialize var
overall_top_5_words_11_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet11_csv)){
  if (datasheet11_csv$length_words[i] > 10) {
    overall_top_5_words_11_list[i] <- (datasheet11_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_11_list <- 
  overall_top_5_words_11_list[lengths(overall_top_5_words_11_list) != 0]
# this unlists the list of lists
overall_top_5_words_11_unlist <- unlist(overall_top_5_words_11_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_11_unlist2 <- strsplit(overall_top_5_words_11_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_11_unlist3 <- unlist(overall_top_5_words_11_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_11_table <- sort(table(overall_top_5_words_11_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_11_cols <- names(overall_top_5_words_11_table)
overall_top_5_words_11 <- paste(overall_top_5_words_11_cols[1], overall_top_5_words_11_cols[2], 
                                overall_top_5_words_11_cols[3], overall_top_5_words_11_cols[4], 
                                overall_top_5_words_11_cols[5],
                                sep= ', ')

# initialize var
overall_top_5_words_12_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet12_csv)){
  if (datasheet12_csv$length_words[i] > 10) {
    overall_top_5_words_12_list[i] <- (datasheet12_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_12_list <- 
  overall_top_5_words_12_list[lengths(overall_top_5_words_12_list) != 0]
# this unlists the list of lists
overall_top_5_words_12_unlist <- unlist(overall_top_5_words_12_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_12_unlist2 <- strsplit(overall_top_5_words_12_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_12_unlist3 <- unlist(overall_top_5_words_12_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_12_table <- sort(table(overall_top_5_words_12_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_12_cols <- names(overall_top_5_words_12_table)
overall_top_5_words_12 <- paste(overall_top_5_words_12_cols[1], overall_top_5_words_12_cols[2], 
                                overall_top_5_words_12_cols[3], overall_top_5_words_12_cols[4], 
                                overall_top_5_words_12_cols[5],
                                sep= ', ')

# initialize var
overall_top_5_words_13_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet13_csv)){
  if (datasheet13_csv$length_words[i] > 10) {
    overall_top_5_words_13_list[i] <- (datasheet13_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_13_list <- 
  overall_top_5_words_13_list[lengths(overall_top_5_words_13_list) != 0]
# this unlists the list of lists
overall_top_5_words_13_unlist <- unlist(overall_top_5_words_13_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_13_unlist2 <- strsplit(overall_top_5_words_13_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_13_unlist3 <- unlist(overall_top_5_words_13_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_13_table <- sort(table(overall_top_5_words_13_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_13_cols <- names(overall_top_5_words_13_table)
overall_top_5_words_13 <- paste(overall_top_5_words_13_cols[1], overall_top_5_words_13_cols[2], 
                                overall_top_5_words_13_cols[3], overall_top_5_words_13_cols[4], 
                                overall_top_5_words_13_cols[5],
                                sep= ', ')

# initialize var
overall_top_5_words_14_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet14_csv)){
  if (datasheet14_csv$length_words[i] > 10) {
    overall_top_5_words_14_list[i] <- (datasheet14_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_14_list <- 
  overall_top_5_words_14_list[lengths(overall_top_5_words_14_list) != 0]
# this unlists the list of lists
overall_top_5_words_14_unlist <- unlist(overall_top_5_words_14_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_14_unlist2 <- strsplit(overall_top_5_words_14_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_14_unlist3 <- unlist(overall_top_5_words_14_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_14_table <- sort(table(overall_top_5_words_14_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_14_cols <- names(overall_top_5_words_14_table)
overall_top_5_words_14 <- paste(overall_top_5_words_14_cols[1], overall_top_5_words_14_cols[2], 
                                overall_top_5_words_14_cols[3], overall_top_5_words_14_cols[4], 
                                overall_top_5_words_14_cols[5],
                                sep= ', ')


# initialize var
overall_top_5_words_15_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet15_csv)){
  if (datasheet15_csv$length_words[i] > 10) {
    overall_top_5_words_15_list[i] <- (datasheet15_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_15_list <- 
  overall_top_5_words_15_list[lengths(overall_top_5_words_15_list) != 0]
# this unlists the list of lists
overall_top_5_words_15_unlist <- unlist(overall_top_5_words_15_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_15_unlist2 <- strsplit(overall_top_5_words_15_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_15_unlist3 <- unlist(overall_top_5_words_15_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_15_table <- sort(table(overall_top_5_words_15_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_15_cols <- names(overall_top_5_words_15_table)
overall_top_5_words_15 <- paste(overall_top_5_words_15_cols[1], overall_top_5_words_15_cols[2], 
                                overall_top_5_words_15_cols[3], overall_top_5_words_15_cols[4], 
                                overall_top_5_words_15_cols[5],
                                sep= ', ')


# initialize var
overall_top_5_words_16_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet16_csv)){
  if (datasheet16_csv$length_words[i] > 10) {
    overall_top_5_words_16_list[i] <- (datasheet16_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_16_list <- 
  overall_top_5_words_16_list[lengths(overall_top_5_words_16_list) != 0]
# this unlists the list of lists
overall_top_5_words_16_unlist <- unlist(overall_top_5_words_16_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_16_unlist2 <- strsplit(overall_top_5_words_16_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_16_unlist3 <- unlist(overall_top_5_words_16_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_16_table <- sort(table(overall_top_5_words_16_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_16_cols <- names(overall_top_5_words_16_table)
overall_top_5_words_16 <- paste(overall_top_5_words_16_cols[1], overall_top_5_words_16_cols[2], 
                                overall_top_5_words_16_cols[3], overall_top_5_words_16_cols[4], 
                                overall_top_5_words_16_cols[5],
                                sep= ', ')

# initialize var
overall_top_5_words_17_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet17_csv)){
  if (datasheet17_csv$length_words[i] > 10) {
    overall_top_5_words_17_list[i] <- (datasheet17_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_17_list <- 
  overall_top_5_words_17_list[lengths(overall_top_5_words_17_list) != 0]
# this unlists the list of lists
overall_top_5_words_17_unlist <- unlist(overall_top_5_words_17_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_17_unlist2 <- strsplit(overall_top_5_words_17_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_17_unlist3 <- unlist(overall_top_5_words_17_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_17_table <- sort(table(overall_top_5_words_17_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_17_cols <- names(overall_top_5_words_17_table)
overall_top_5_words_17 <- paste(overall_top_5_words_17_cols[1], overall_top_5_words_17_cols[2], 
                                overall_top_5_words_17_cols[3], overall_top_5_words_17_cols[4], 
                                overall_top_5_words_17_cols[5],
                                sep= ', ')


# initialize var
overall_top_5_words_18_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet18_csv)){
  if (datasheet18_csv$length_words[i] > 10) {
    overall_top_5_words_18_list[i] <- (datasheet18_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_18_list <- 
  overall_top_5_words_18_list[lengths(overall_top_5_words_18_list) != 0]
# this unlists the list of lists
overall_top_5_words_18_unlist <- unlist(overall_top_5_words_18_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_18_unlist2 <- strsplit(overall_top_5_words_18_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_18_unlist3 <- unlist(overall_top_5_words_18_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_18_table <- sort(table(overall_top_5_words_18_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_18_cols <- names(overall_top_5_words_18_table)
overall_top_5_words_18 <- paste(overall_top_5_words_18_cols[1], overall_top_5_words_18_cols[2], 
                                overall_top_5_words_18_cols[3], overall_top_5_words_18_cols[4], 
                                overall_top_5_words_18_cols[5],
                                sep= ', ')


# initialize var
overall_top_5_words_19_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet19_csv)){
  if (datasheet19_csv$length_words[i] > 10) {
    overall_top_5_words_19_list[i] <- (datasheet19_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_19_list <- 
  overall_top_5_words_19_list[lengths(overall_top_5_words_19_list) != 0]
# this unlists the list of lists
overall_top_5_words_19_unlist <- unlist(overall_top_5_words_19_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_19_unlist2 <- strsplit(overall_top_5_words_19_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_19_unlist3 <- unlist(overall_top_5_words_19_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_19_table <- sort(table(overall_top_5_words_19_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_19_cols <- names(overall_top_5_words_19_table)
overall_top_5_words_19 <- paste(overall_top_5_words_19_cols[1], overall_top_5_words_19_cols[2], 
                                overall_top_5_words_19_cols[3], overall_top_5_words_19_cols[4], 
                                overall_top_5_words_19_cols[5],
                                sep= ', ')

# initialize var
overall_top_5_words_20_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet20_csv)){
  if (datasheet20_csv$length_words[i] > 10) {
    overall_top_5_words_20_list[i] <- (datasheet20_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_20_list <- 
  overall_top_5_words_20_list[lengths(overall_top_5_words_20_list) != 0]
# this unlists the list of lists
overall_top_5_words_20_unlist <- unlist(overall_top_5_words_20_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_20_unlist2 <- strsplit(overall_top_5_words_20_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_20_unlist3 <- unlist(overall_top_5_words_20_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_20_table <- sort(table(overall_top_5_words_20_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_20_cols <- names(overall_top_5_words_20_table)
overall_top_5_words_20 <- paste(overall_top_5_words_20_cols[1], overall_top_5_words_20_cols[2], 
                                overall_top_5_words_20_cols[3], overall_top_5_words_20_cols[4], 
                                overall_top_5_words_20_cols[5],
                                sep= ', ')

# initialize var
overall_top_5_words_21_list <- list()
# loop through entire column to get top5words for qnums with more than 10 word length
for (i in 1:nrow(datasheet21_csv)){
  if (datasheet21_csv$length_words[i] > 10) {
    overall_top_5_words_21_list[i] <- (datasheet21_csv$top_5_frequent_words[i])
    i=i+1
  }
  else {i=i+1}
}
# remove null indices from list
overall_top_5_words_21_list <- 
  overall_top_5_words_21_list[lengths(overall_top_5_words_21_list) != 0]
# this unlists the list of lists
overall_top_5_words_21_unlist <- unlist(overall_top_5_words_21_list, recursive = FALSE)
# separates  each list into individual lists
overall_top_5_words_21_unlist2 <- strsplit(overall_top_5_words_21_unlist, split = ", ")
# each word is separate list item
overall_top_5_words_21_unlist3 <- unlist(overall_top_5_words_21_unlist2, recursive = FALSE)
# get 5 most frequent words
# source: https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
overall_top_5_words_21_table <- sort(table(overall_top_5_words_21_unlist3),decreasing=TRUE)[1:5]
overall_top_5_words_21_cols <- names(overall_top_5_words_21_table)
overall_top_5_words_21 <- paste(overall_top_5_words_21_cols[1], overall_top_5_words_21_cols[2], 
                                overall_top_5_words_21_cols[3], overall_top_5_words_21_cols[4], 
                                overall_top_5_words_21_cols[5],
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
list8 <- c(datasheet7_csv$qnum[datasheet8_csv$length_words==min(datasheet8_csv$length_words)])
list8_delim <- c(paste(list8, collapse = ", "))
list9 <- c(datasheet7_csv$qnum[datasheet9_csv$length_words==min(datasheet9_csv$length_words)])
list9_delim <- c(paste(list9, collapse = ", "))
list10 <- c(datasheet7_csv$qnum[datasheet10_csv$length_words==min(datasheet10_csv$length_words)])
list10_delim <- c(paste(list10, collapse = ", "))
list11 <- c(datasheet7_csv$qnum[datasheet11_csv$length_words==min(datasheet11_csv$length_words)])
list11_delim <- c(paste(list11, collapse = ", "))
list12 <- c(datasheet7_csv$qnum[datasheet12_csv$length_words==min(datasheet12_csv$length_words)])
list12_delim <- c(paste(list12, collapse = ", "))
list13 <- c(datasheet7_csv$qnum[datasheet13_csv$length_words==min(datasheet13_csv$length_words)])
list13_delim <- c(paste(list13, collapse = ", "))
list14 <- c(datasheet7_csv$qnum[datasheet14_csv$length_words==min(datasheet14_csv$length_words)])
list14_delim <- c(paste(list14, collapse = ", "))
list15 <- c(datasheet7_csv$qnum[datasheet15_csv$length_words==min(datasheet15_csv$length_words)])
list15_delim <- c(paste(list15, collapse = ", "))
list16 <- c(datasheet7_csv$qnum[datasheet16_csv$length_words==min(datasheet16_csv$length_words)])
list16_delim <- c(paste(list16, collapse = ", "))
list17 <- c(datasheet7_csv$qnum[datasheet17_csv$length_words==min(datasheet17_csv$length_words)])
list17_delim <- c(paste(list17, collapse = ", "))
list18 <- c(datasheet7_csv$qnum[datasheet18_csv$length_words==min(datasheet18_csv$length_words)])
list18_delim <- c(paste(list18, collapse = ", "))
list19 <- c(datasheet7_csv$qnum[datasheet19_csv$length_words==min(datasheet19_csv$length_words)])
list19_delim <- c(paste(list19, collapse = ", "))
list20 <- c(datasheet7_csv$qnum[datasheet20_csv$length_words==min(datasheet20_csv$length_words)])
list20_delim <- c(paste(list20, collapse = ", "))
list21 <- c(datasheet7_csv$qnum[datasheet21_csv$length_words==min(datasheet21_csv$length_words)])
list21_delim <- c(paste(list21, collapse = ", "))


# do this so that any values with only one qnum is also converted to string
list1_delim <- toString(list1_delim)
list2_delim <- toString(list2_delim)
list3_delim <- toString(list3_delim)
list4_delim <- toString(list4_delim)
list5_delim <- toString(list5_delim)
list6_delim <- toString(list6_delim)
list7_delim <- toString(list7_delim)
list8_delim <- toString(list8_delim)
list9_delim <- toString(list9_delim)
list10_delim <- toString(list10_delim)
list11_delim <- toString(list11_delim)
list12_delim <- toString(list12_delim)
list13_delim <- toString(list13_delim)
list14_delim <- toString(list14_delim)
list15_delim <- toString(list15_delim)
list16_delim <- toString(list16_delim)
list17_delim <- toString(list17_delim)
list18_delim <- toString(list18_delim)
list19_delim <- toString(list19_delim)
list20_delim <- toString(list20_delim)
list21_delim <- toString(list21_delim)


#### summary df without score ####
summary_df_wo_score <- data.frame(datasheet_ID=c(1,2,3,4,5,6,7,
                                                 8,9,10,11,12,13,14,15,
                                                 16,17,18,19,20,21),
                                  datasheet_version = c(datasheet1_csv$datasheet_version[1], datasheet2_csv$datasheet_version[1],
                                                        datasheet3_csv$datasheet_version[1], datasheet4_csv$datasheet_version[1],
                                                        datasheet5_csv$datasheet_version[1], datasheet6_csv$datasheet_version[1],
                                                        datasheet7_csv$datasheet_version[1], datasheet8_csv$datasheet_version[1],
                                                        datasheet9_csv$datasheet_version[1], datasheet10_csv$datasheet_version[1],
                                                        datasheet11_csv$datasheet_version[1], datasheet12_csv$datasheet_version[1],
                                                        datasheet13_csv$datasheet_version[1], datasheet14_csv$datasheet_version[1], 
                                                        datasheet15_csv$datasheet_version[1], datasheet16_csv$datasheet_version[1],
                                                        datasheet17_csv$datasheet_version[1], datasheet18_csv$datasheet_version[1],
                                                        datasheet19_csv$datasheet_version[1], datasheet20_csv$datasheet_version[1],
                                                        datasheet21_csv$datasheet_version[1]),
                         title_short=c("sandbox_dna",
                                       "garments",
                                       "PASS",
                                       "spatial_apartheid",
                                       "CSFCube",
                                       "digital_experiments",
                                       "PROCAT",
                                       "OmniPrint",
                                       "recycle",
                                       "wildfire_db",
                                       "docdebt",
                                       "artsheet",
                                       "multi_autoML",
                                       "CREAK",
                                       "CSAW",
                                       "humbug_db",
                                       "model_worlds",
                                       "spoken_words",
                                       "redcaps",
                                       "CPD",
                                       "lifelong_learn"),
                         total_length_wrds=c(sum(datasheet1_csv$length_words),
                                             sum(datasheet2_csv$length_words),
                                             sum(datasheet3_csv$length_words),
                                             sum(datasheet4_csv$length_words),
                                             sum(datasheet5_csv$length_words),
                                             sum(datasheet6_csv$length_words),
                                             sum(datasheet7_csv$length_words),
                                             sum(datasheet8_csv$length_words),
                                             sum(datasheet9_csv$length_words),
                                             sum(datasheet10_csv$length_words),
                                             sum(datasheet11_csv$length_words),
                                             sum(datasheet12_csv$length_words),
                                             sum(datasheet13_csv$length_words),
                                             sum(datasheet14_csv$length_words),
                                             sum(datasheet15_csv$length_words),
                                             sum(datasheet16_csv$length_words),
                                             sum(datasheet17_csv$length_words),
                                             sum(datasheet18_csv$length_words),
                                             sum(datasheet19_csv$length_words),
                                             sum(datasheet20_csv$length_words),
                                             sum(datasheet21_csv$length_words)),
                         question_completion_pct=c(question_completion_pct_1,
                                                   question_completion_pct_2,
                                                   question_completion_pct_3,
                                                   question_completion_pct_4,
                                                   question_completion_pct_5,
                                                   question_completion_pct_6,
                                                   question_completion_pct_7,
                                                   question_completion_pct_8,
                                                   question_completion_pct_9,
                                                   question_completion_pct_10,
                                                   question_completion_pct_11,
                                                   question_completion_pct_12,
                                                   question_completion_pct_13,
                                                   question_completion_pct_14,
                                                   question_completion_pct_15,
                                                   question_completion_pct_16,
                                                   question_completion_pct_17,
                                                   question_completion_pct_18,
                                                   question_completion_pct_19,
                                                   question_completion_pct_20,
                                                   question_completion_pct_21),
                         avg_response_length=c(mean(datasheet1_csv$length_words),
                                               mean(datasheet2_csv$length_words),
                                               mean(datasheet3_csv$length_words),
                                               mean(datasheet4_csv$length_words),
                                               mean(datasheet5_csv$length_words),
                                               mean(datasheet6_csv$length_words),
                                               mean(datasheet7_csv$length_words),
                                               mean(datasheet8_csv$length_words),
                                               mean(datasheet9_csv$length_words),
                                               mean(datasheet10_csv$length_words),
                                               mean(datasheet11_csv$length_words),
                                               mean(datasheet12_csv$length_words),
                                               mean(datasheet13_csv$length_words),
                                               mean(datasheet14_csv$length_words),
                                               mean(datasheet15_csv$length_words),
                                               mean(datasheet16_csv$length_words),
                                               mean(datasheet17_csv$length_words),
                                               mean(datasheet18_csv$length_words),
                                               mean(datasheet19_csv$length_words),
                                               mean(datasheet20_csv$length_words),
                                               mean(datasheet21_csv$length_words)),
                         max_response_length=c(max(datasheet1_csv$length_words),
                                               max(datasheet2_csv$length_words),
                                               max(datasheet3_csv$length_words),
                                               max(datasheet4_csv$length_words),
                                               max(datasheet5_csv$length_words),
                                               max(datasheet6_csv$length_words),
                                               max(datasheet7_csv$length_words),
                                               max(datasheet8_csv$length_words),
                                               max(datasheet9_csv$length_words),
                                               max(datasheet10_csv$length_words),
                                               max(datasheet11_csv$length_words),
                                               max(datasheet12_csv$length_words),
                                               max(datasheet13_csv$length_words),
                                               max(datasheet14_csv$length_words),
                                               max(datasheet15_csv$length_words),
                                               max(datasheet16_csv$length_words),
                                               max(datasheet17_csv$length_words),
                                               max(datasheet18_csv$length_words),
                                               max(datasheet19_csv$length_words),
                                               max(datasheet20_csv$length_words),
                                               max(datasheet21_csv$length_words)),
                         max_response_qnum=c(datasheet1_csv$qnum[datasheet1_csv$length_words==max(datasheet1_csv$length_words)],
                                             datasheet2_csv$qnum[datasheet2_csv$length_words==max(datasheet2_csv$length_words)],
                                             datasheet3_csv$qnum[datasheet3_csv$length_words==max(datasheet3_csv$length_words)],
                                             datasheet4_csv$qnum[datasheet4_csv$length_words==max(datasheet4_csv$length_words)],
                                             datasheet5_csv$qnum[datasheet5_csv$length_words==max(datasheet5_csv$length_words)],
                                             datasheet6_csv$qnum[datasheet6_csv$length_words==max(datasheet6_csv$length_words)],
                                             datasheet7_csv$qnum[datasheet7_csv$length_words==max(datasheet7_csv$length_words)],
                                             datasheet8_csv$qnum[datasheet8_csv$length_words==max(datasheet8_csv$length_words)],
                                             datasheet9_csv$qnum[datasheet9_csv$length_words==max(datasheet9_csv$length_words)],
                                             datasheet10_csv$qnum[datasheet10_csv$length_words==max(datasheet10_csv$length_words)],
                                             datasheet11_csv$qnum[datasheet11_csv$length_words==max(datasheet11_csv$length_words)],
                                             datasheet12_csv$qnum[datasheet12_csv$length_words==max(datasheet12_csv$length_words)],
                                             datasheet13_csv$qnum[datasheet13_csv$length_words==max(datasheet13_csv$length_words)],
                                             datasheet14_csv$qnum[datasheet14_csv$length_words==max(datasheet14_csv$length_words)],
                                             datasheet15_csv$qnum[datasheet15_csv$length_words==max(datasheet15_csv$length_words)],
                                             datasheet16_csv$qnum[datasheet16_csv$length_words==max(datasheet16_csv$length_words)],
                                             datasheet17_csv$qnum[datasheet17_csv$length_words==max(datasheet17_csv$length_words)],
                                             datasheet18_csv$qnum[datasheet18_csv$length_words==max(datasheet18_csv$length_words)],
                                             datasheet19_csv$qnum[datasheet19_csv$length_words==max(datasheet19_csv$length_words)],
                                             datasheet20_csv$qnum[datasheet20_csv$length_words==max(datasheet20_csv$length_words)],
                                             datasheet21_csv$qnum[datasheet21_csv$length_words==max(datasheet21_csv$length_words)]),
                         min_response_length=c(min(datasheet1_csv$length_words),
                                               min(datasheet2_csv$length_words),
                                               min(datasheet3_csv$length_words),
                                               min(datasheet4_csv$length_words),
                                               min(datasheet5_csv$length_words),
                                               min(datasheet6_csv$length_words),
                                               min(datasheet7_csv$length_words),
                                               min(datasheet8_csv$length_words),
                                               min(datasheet9_csv$length_words),
                                               min(datasheet10_csv$length_words),
                                               min(datasheet11_csv$length_words),
                                               min(datasheet12_csv$length_words),
                                               min(datasheet13_csv$length_words),
                                               min(datasheet14_csv$length_words),
                                               min(datasheet15_csv$length_words),
                                               min(datasheet16_csv$length_words),
                                               min(datasheet17_csv$length_words),
                                               min(datasheet18_csv$length_words),
                                               min(datasheet19_csv$length_words),
                                               min(datasheet20_csv$length_words),
                                               min(datasheet21_csv$length_words)),
                         min_response_qnum=c(list1_delim, list2_delim,
                                             list3_delim, list4_delim,
                                             list5_delim, list6_delim,
                                             list7_delim, list8_delim,
                                             list9_delim, list10_delim,
                                             list11_delim, list12_delim,
                                             list13_delim, list14_delim,
                                             list15_delim, list16_delim,
                                             list17_delim, list18_delim,
                                             list19_delim, list20_delim,
                                             list21_delim),
                         overall_top_5_words=c(overall_top_5_words_1,overall_top_5_words_2,
                                               overall_top_5_words_3, overall_top_5_words_4,
                                               overall_top_5_words_5, overall_top_5_words_6,
                                               overall_top_5_words_7, overall_top_5_words_8,
                                               overall_top_5_words_9, overall_top_5_words_10,
                                               overall_top_5_words_11, overall_top_5_words_12,
                                               overall_top_5_words_13, overall_top_5_words_14,
                                               overall_top_5_words_15, overall_top_5_words_16,
                                               overall_top_5_words_17, overall_top_5_words_18,
                                               overall_top_5_words_19, overall_top_5_words_20,
                                               overall_top_5_words_21),
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

