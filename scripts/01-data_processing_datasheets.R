#### Preamble ####
# Purpose: Cleaning and formatting datasheets
# Author: Eshta Bhardwaj
# Date: April 21 2023
# Contact: eshta.bhardwaj@mail.utoronto.ca
# License: MIT
# Pre-requisites: Must already have pdfs of datasheets downloaded with some manual formatting performed.

#### Workspace set-up ####
library(tidyverse)
library(pdftools)

#### reading in blank datasheet to establish baselines ####

# 2021

datasheet_questions_2021 <- pdftools::pdf_text("inputs/data/2021_Datasheet_questionsonly.pdf")
datasheet_questions_2021

datasheet_questions_2021tbl <- tibble(
  raw_text = datasheet_questions_2021,
  page_number = c(1:7)
)

datasheet_questions_2021tbl <-
  separate_rows(datasheet_questions_2021tbl, raw_text, sep = "\\n", convert = FALSE)

# 2018

datasheet_questions_2018 <- pdftools::pdf_text("inputs/data/2018_Datasheet_questionsonly.pdf")
datasheet_questions_2018

datasheet_questions_2018tbl <- tibble(
  raw_text = datasheet_questions_2018,
  page_number = c(1:4)
)

datasheet_questions_2018tbl <-
  separate_rows(datasheet_questions_2018tbl, raw_text, sep = "\\n", convert = FALSE)

#### reading in datasheets ####

#### datasheet1: A sandbox for prediction and integration of DNA, RNA, and proteins in single cells ####

# read in datasheet file
datasheet1 <- pdftools::pdf_text("inputs/data/raw/datasheet1.pdf")

# put data in a tibble
datasheet1_tbl <- tibble(
  raw_text = datasheet1,
  # the following changes for each datasheet
  page_number = c(1:7)
)

# this separates each new line into a separate row
datasheet1_tbl <-
  separate_rows(datasheet1_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet1_tbl <- datasheet1_tbl[-which(datasheet1_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet1_tbl <- datasheet1_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet1_tbl)) {
  if(substr(trimws(datasheet1_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet1_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet1_tbl[i,"raw_text"] == toupper(datasheet1_tbl[i,"raw_text"]) ){
    datasheet1_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet1_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet1_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet1_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet1_tbl)) {
  if(trimws(datasheet1_tbl[i, "identifier"]) == ""){
    datasheet1_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet1_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet1_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2018"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet1_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet1_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet1_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet1_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet1_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet1_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet1_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){

      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet1_tbl_final[question_num,"qnum"] <- question_num
      datasheet1_tbl_final[question_num,"completion"] <- completion_answer
      datasheet1_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet1_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet1_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet1_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet1_tbl_final[question_num,"qnum"] <- question_num
    datasheet1_tbl_final[question_num,"completion"] <- completion_answer
    datasheet1_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet1_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet1_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet1_tbl_final, file = "./inputs/data/cleaned/datasheet1.csv", row.names=FALSE)


#### datasheet2: Generating Datasets of 3D Garments with Sewing Patterns #####

# read in datasheet file
datasheet2 <- pdftools::pdf_text("inputs/data/raw/datasheet2.pdf")

# put data in a tibble
datasheet2_tbl <- tibble(
  raw_text = datasheet2,
  # the following changes for each datasheet
  page_number = c(1:8)
)

# this separates each new line into a separate row
datasheet2_tbl <-
  separate_rows(datasheet2_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet2_tbl <- datasheet2_tbl[-which(datasheet2_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet2_tbl <- datasheet2_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet2_tbl)) {
  if(substr(trimws(datasheet2_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet2_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet2_tbl[i,"raw_text"] == toupper(datasheet2_tbl[i,"raw_text"]) ){
    datasheet2_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet2_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet2_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet2_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet2_tbl)) {
  if(trimws(datasheet2_tbl[i, "identifier"]) == ""){
    datasheet2_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet2_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet2_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet2_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet2_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet2_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet2_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet2_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet2_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet2_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet2_tbl_final[question_num,"qnum"] <- question_num
      datasheet2_tbl_final[question_num,"completion"] <- completion_answer
      datasheet2_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet2_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet2_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet2_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet2_tbl_final[question_num,"qnum"] <- question_num
    datasheet2_tbl_final[question_num,"completion"] <- completion_answer
    datasheet2_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet2_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet2_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet2_tbl_final, file = "./inputs/data/cleaned/datasheet2.csv", row.names=FALSE)

#### datasheet3: PASS An ImageNet replacement for self-supervised pretraining without humans ####


# read in datasheet file
datasheet3 <- pdftools::pdf_text("inputs/data/raw/datasheet3.pdf")

# put data in a tibble
datasheet3_tbl <- tibble(
  raw_text = datasheet3,
  # the following changes for each datasheet
  page_number = c(1:8)
)

# this separates each new line into a separate row
datasheet3_tbl <-
  separate_rows(datasheet3_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet3_tbl <- datasheet3_tbl[-which(datasheet3_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet3_tbl <- datasheet3_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet3_tbl)) {
  if(substr(trimws(datasheet3_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet3_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet3_tbl[i,"raw_text"] == toupper(datasheet3_tbl[i,"raw_text"]) ){
    datasheet3_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet3_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet3_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet3_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet3_tbl)) {
  if(trimws(datasheet3_tbl[i, "identifier"]) == ""){
    datasheet3_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet3_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet3_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet3_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet3_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet3_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet3_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet3_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet3_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet3_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet3_tbl_final[question_num,"qnum"] <- question_num
      datasheet3_tbl_final[question_num,"completion"] <- completion_answer
      datasheet3_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet3_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet3_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet3_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet3_tbl_final[question_num,"qnum"] <- question_num
    datasheet3_tbl_final[question_num,"completion"] <- completion_answer
    datasheet3_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet3_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet3_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet3_tbl_final, file = "./inputs/data/cleaned/datasheet3.csv", row.names=FALSE)

#### datasheet4: Constructing a Visual Dataset to Study the Effects of Spatial Apartheid in South Africa ####

# read in datasheet file
datasheet4 <- pdftools::pdf_text("inputs/data/raw/datasheet4.pdf")

# put data in a tibble
datasheet4_tbl <- tibble(
  raw_text = datasheet4,
  # the following changes for each datasheet
  page_number = c(1:12)
)

# this separates each new line into a separate row
datasheet4_tbl <-
  separate_rows(datasheet4_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet4_tbl <- datasheet4_tbl[-which(datasheet4_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet4_tbl <- datasheet4_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet4_tbl)) {
  if(substr(trimws(datasheet4_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet4_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet4_tbl[i,"raw_text"] == toupper(datasheet4_tbl[i,"raw_text"]) ){
    datasheet4_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet4_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet4_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet4_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet4_tbl)) {
  if(trimws(datasheet4_tbl[i, "identifier"]) == ""){
    datasheet4_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet4_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet4_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet4_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet4_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet4_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet4_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet4_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet4_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet4_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet4_tbl_final[question_num,"qnum"] <- question_num
      datasheet4_tbl_final[question_num,"completion"] <- completion_answer
      datasheet4_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet4_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet4_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet4_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet4_tbl_final[question_num,"qnum"] <- question_num
    datasheet4_tbl_final[question_num,"completion"] <- completion_answer
    datasheet4_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet4_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet4_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet4_tbl_final, file = "./inputs/data/cleaned/datasheet4.csv", row.names=FALSE)

#### datasheet5: CSFCube - A Test Collection of Computer Science Research Articles for Faceted Query by Example ####


# read in datasheet file
datasheet5 <- pdftools::pdf_text("inputs/data/raw/datasheet5.pdf")

# put data in a tibble
datasheet5_tbl <- tibble(
  raw_text = datasheet5,
  # the following changes for each datasheet
  page_number = c(1:8)
)

# this separates each new line into a separate row
datasheet5_tbl <-
  separate_rows(datasheet5_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet5_tbl <- datasheet5_tbl[-which(datasheet5_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet5_tbl <- datasheet5_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet5_tbl)) {
  if(substr(trimws(datasheet5_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet5_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet5_tbl[i,"raw_text"] == toupper(datasheet5_tbl[i,"raw_text"]) ){
    datasheet5_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet5_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet5_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet5_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet5_tbl)) {
  if(trimws(datasheet5_tbl[i, "identifier"]) == ""){
    datasheet5_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet5_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet5_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet5_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet5_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet5_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet5_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet5_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet5_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet5_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet5_tbl_final[question_num,"qnum"] <- question_num
      datasheet5_tbl_final[question_num,"completion"] <- completion_answer
      datasheet5_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet5_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet5_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet5_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet5_tbl_final[question_num,"qnum"] <- question_num
    datasheet5_tbl_final[question_num,"completion"] <- completion_answer
    datasheet5_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet5_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet5_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet5_tbl_final, file = "./inputs/data/cleaned/datasheet5.csv", row.names=FALSE)

#### datasheet6: Datasets for Online Controlled Experiments ####


# read in datasheet file
datasheet6 <- pdftools::pdf_text("inputs/data/raw/datasheet6.pdf")

# put data in a tibble
datasheet6_tbl <- tibble(
  raw_text = datasheet6,
  # the following changes for each datasheet
  page_number = c(1:11)
)

# this separates each new line into a separate row
datasheet6_tbl <-
  separate_rows(datasheet6_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet6_tbl <- datasheet6_tbl[-which(datasheet6_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet6_tbl <- datasheet6_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet6_tbl)) {
  if(substr(trimws(datasheet6_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet6_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet6_tbl[i,"raw_text"] == toupper(datasheet6_tbl[i,"raw_text"]) ){
    datasheet6_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet6_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet6_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet6_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet6_tbl)) {
  if(trimws(datasheet6_tbl[i, "identifier"]) == ""){
    datasheet6_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet6_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet6_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet6_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet6_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet6_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet6_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet6_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet6_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet6_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet6_tbl_final[question_num,"qnum"] <- question_num
      datasheet6_tbl_final[question_num,"completion"] <- completion_answer
      datasheet6_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet6_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet6_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet6_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet6_tbl_final[question_num,"qnum"] <- question_num
    datasheet6_tbl_final[question_num,"completion"] <- completion_answer
    datasheet6_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet6_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet6_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet6_tbl_final, file = "./inputs/data/cleaned/datasheet6.csv", row.names=FALSE)

#### datasheet14: CREAK A Dataset for Commonsense Reasoning over Entity Knowledge ####

# read in datasheet file
datasheet14 <- pdftools::pdf_text("inputs/data/raw/datasheet14.pdf")

# put data in a tibble
datasheet14_tbl <- tibble(
  raw_text = datasheet14,
  # the following changes for each datasheet
  page_number = c(1:6)
)

# this separates each new line into a separate row
datasheet14_tbl <-
  separate_rows(datasheet14_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet14_tbl <- datasheet14_tbl[-which(datasheet14_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet14_tbl <- datasheet14_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet14_tbl)) {
  if(substr(trimws(datasheet14_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet14_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet14_tbl[i,"raw_text"] == toupper(datasheet14_tbl[i,"raw_text"]) ){
    datasheet14_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet14_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet14_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet14_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet14_tbl)) {
  if(trimws(datasheet14_tbl[i, "identifier"]) == ""){
    datasheet14_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet14_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet14_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2018"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet14_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet14_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet14_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet14_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet14_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet14_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet14_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet14_tbl_final[question_num,"qnum"] <- question_num
      datasheet14_tbl_final[question_num,"completion"] <- completion_answer
      datasheet14_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet14_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet14_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet14_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet14_tbl_final[question_num,"qnum"] <- question_num
    datasheet14_tbl_final[question_num,"completion"] <- completion_answer
    datasheet14_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet14_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet14_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet14_tbl_final, file = "./inputs/data/cleaned/datasheet14.csv", row.names=FALSE)

#### datasheet7: PROCAT Product Catalogue Dataset for Implicit Clustering, Permutation Learning and Structure Prediction ####

# read in datasheet file
datasheet7 <- pdftools::pdf_text("inputs/data/raw/datasheet7.pdf")

# put data in a tibble
datasheet7_tbl <- tibble(
  raw_text = datasheet7,
  # the following changes for each datasheet
  page_number = c(1:8)
)

# this separates each new line into a separate row
datasheet7_tbl <-
  separate_rows(datasheet7_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet7_tbl <- datasheet7_tbl[-which(datasheet7_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet7_tbl <- datasheet7_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet7_tbl)) {
  if(substr(trimws(datasheet7_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet7_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet7_tbl[i,"raw_text"] == toupper(datasheet7_tbl[i,"raw_text"]) ){
    datasheet7_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet7_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet7_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet7_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet7_tbl)) {
  if(trimws(datasheet7_tbl[i, "identifier"]) == ""){
    datasheet7_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet7_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet7_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet7_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet7_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet7_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet7_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet7_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet7_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet7_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet7_tbl_final[question_num,"qnum"] <- question_num
      datasheet7_tbl_final[question_num,"completion"] <- completion_answer
      datasheet7_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet7_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet7_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet7_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet7_tbl_final[question_num,"qnum"] <- question_num
    datasheet7_tbl_final[question_num,"completion"] <- completion_answer
    datasheet7_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet7_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet7_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet7_tbl_final, file = "./inputs/data/cleaned/datasheet7.csv", row.names=FALSE)

#### datasheet 8: ####

# read in datasheet file
datasheet8 <- pdftools::pdf_text("inputs/data/raw/datasheet8.pdf")

# put data in a tibble
datasheet8_tbl <- tibble(
  raw_text = datasheet8,
  # the following changes for each datasheet
  page_number = c(1:8)
)

# this separates each new line into a separate row
datasheet8_tbl <-
  separate_rows(datasheet8_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet8_tbl <- datasheet8_tbl[-which(datasheet8_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet8_tbl <- datasheet8_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet8_tbl)) {
  if(substr(trimws(datasheet8_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet8_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet8_tbl[i,"raw_text"] == toupper(datasheet8_tbl[i,"raw_text"]) ){
    datasheet8_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet8_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet8_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet8_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet8_tbl)) {
  if(trimws(datasheet8_tbl[i, "identifier"]) == ""){
    datasheet8_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet8_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet8_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet8_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet8_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet8_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet8_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet8_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet8_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet8_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet8_tbl_final[question_num,"qnum"] <- question_num
      datasheet8_tbl_final[question_num,"completion"] <- completion_answer
      datasheet8_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet8_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet8_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet8_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet8_tbl_final[question_num,"qnum"] <- question_num
    datasheet8_tbl_final[question_num,"completion"] <- completion_answer
    datasheet8_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet8_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet8_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet8_tbl_final, file = "./inputs/data/cleaned/datasheet8.csv", row.names=FALSE)

#### datasheet 9: ####

# read in datasheet file
datasheet9 <- pdftools::pdf_text("inputs/data/raw/datasheet9.pdf")

# put data in a tibble
datasheet9_tbl <- tibble(
  raw_text = datasheet9,
  # the following changes for each datasheet
  page_number = c(1:15)
)

# this separates each new line into a separate row
datasheet9_tbl <-
  separate_rows(datasheet9_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet9_tbl <- datasheet9_tbl[-which(datasheet9_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet9_tbl <- datasheet9_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet9_tbl)) {
  if(substr(trimws(datasheet9_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet9_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet9_tbl[i,"raw_text"] == toupper(datasheet9_tbl[i,"raw_text"]) ){
    datasheet9_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet9_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet9_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet9_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet9_tbl)) {
  if(trimws(datasheet9_tbl[i, "identifier"]) == ""){
    datasheet9_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet9_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet9_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet9_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet9_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet9_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet9_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet9_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet9_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet9_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet9_tbl_final[question_num,"qnum"] <- question_num
      datasheet9_tbl_final[question_num,"completion"] <- completion_answer
      datasheet9_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet9_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet9_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet9_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet9_tbl_final[question_num,"qnum"] <- question_num
    datasheet9_tbl_final[question_num,"completion"] <- completion_answer
    datasheet9_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet9_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet9_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet9_tbl_final, file = "./inputs/data/cleaned/datasheet9.csv", row.names=FALSE)

#### datasheet 10: ####

# read in datasheet file
datasheet10 <- pdftools::pdf_text("inputs/data/raw/datasheet10.pdf")

# put data in a tibble
datasheet10_tbl <- tibble(
  raw_text = datasheet10,
  # the following changes for each datasheet
  page_number = c(1:8)
)

# this separates each new line into a separate row
datasheet10_tbl <-
  separate_rows(datasheet10_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet10_tbl <- datasheet10_tbl[-which(datasheet10_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet10_tbl <- datasheet10_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet10_tbl)) {
  if(substr(trimws(datasheet10_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet10_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet10_tbl[i,"raw_text"] == toupper(datasheet10_tbl[i,"raw_text"]) ){
    datasheet10_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet10_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet10_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet10_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet10_tbl)) {
  if(trimws(datasheet10_tbl[i, "identifier"]) == ""){
    datasheet10_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet10_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet10_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet10_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet10_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet10_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet10_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet10_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet10_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet10_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet10_tbl_final[question_num,"qnum"] <- question_num
      datasheet10_tbl_final[question_num,"completion"] <- completion_answer
      datasheet10_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet10_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet10_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet10_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet10_tbl_final[question_num,"qnum"] <- question_num
    datasheet10_tbl_final[question_num,"completion"] <- completion_answer
    datasheet10_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet10_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet10_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet10_tbl_final, file = "./inputs/data/cleaned/datasheet10.csv", row.names=FALSE)

#### datasheet 11: ####

# read in datasheet file
datasheet11 <- pdftools::pdf_text("inputs/data/raw/datasheet11.pdf")

# put data in a tibble
datasheet11_tbl <- tibble(
  raw_text = datasheet11,
  # the following changes for each datasheet
  page_number = c(1:10)
)

# this separates each new line into a separate row
datasheet11_tbl <-
  separate_rows(datasheet11_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet11_tbl <- datasheet11_tbl[-which(datasheet11_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet11_tbl <- datasheet11_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet11_tbl)) {
  if(substr(trimws(datasheet11_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet11_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet11_tbl[i,"raw_text"] == toupper(datasheet11_tbl[i,"raw_text"]) ){
    datasheet11_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet11_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet11_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet11_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet11_tbl)) {
  if(trimws(datasheet11_tbl[i, "identifier"]) == ""){
    datasheet11_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet11_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet11_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet11_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet11_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet11_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet11_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet11_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet11_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet11_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet11_tbl_final[question_num,"qnum"] <- question_num
      datasheet11_tbl_final[question_num,"completion"] <- completion_answer
      datasheet11_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet11_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet11_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet11_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet11_tbl_final[question_num,"qnum"] <- question_num
    datasheet11_tbl_final[question_num,"completion"] <- completion_answer
    datasheet11_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet11_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet11_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet11_tbl_final, file = "./inputs/data/cleaned/datasheet11.csv", row.names=FALSE)

#### datasheet 12: ####

# read in datasheet file
datasheet12 <- pdftools::pdf_text("inputs/data/raw/datasheet12.pdf")

# put data in a tibble
datasheet12_tbl <- tibble(
  raw_text = datasheet12,
  # the following changes for each datasheet
  page_number = c(1:9)
)

# this separates each new line into a separate row
datasheet12_tbl <-
  separate_rows(datasheet12_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet12_tbl <- datasheet12_tbl[-which(datasheet12_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet12_tbl <- datasheet12_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet12_tbl)) {
  if(substr(trimws(datasheet12_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet12_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet12_tbl[i,"raw_text"] == toupper(datasheet12_tbl[i,"raw_text"]) ){
    datasheet12_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet12_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet12_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet12_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet12_tbl)) {
  if(trimws(datasheet12_tbl[i, "identifier"]) == ""){
    datasheet12_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet12_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet12_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet12_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet12_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet12_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet12_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet12_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet12_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet12_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet12_tbl_final[question_num,"qnum"] <- question_num
      datasheet12_tbl_final[question_num,"completion"] <- completion_answer
      datasheet12_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet12_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet12_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet12_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet12_tbl_final[question_num,"qnum"] <- question_num
    datasheet12_tbl_final[question_num,"completion"] <- completion_answer
    datasheet12_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet12_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet12_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet12_tbl_final, file = "./inputs/data/cleaned/datasheet12.csv", row.names=FALSE)

#### datasheet 13: ####

# read in datasheet file
datasheet13 <- pdftools::pdf_text("inputs/data/raw/datasheet13.pdf")

# put data in a tibble
datasheet13_tbl <- tibble(
  raw_text = datasheet13,
  # the following changes for each datasheet
  page_number = c(1:8)
)

# this separates each new line into a separate row
datasheet13_tbl <-
  separate_rows(datasheet13_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet13_tbl <- datasheet13_tbl[-which(datasheet13_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet13_tbl <- datasheet13_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet13_tbl)) {
  if(substr(trimws(datasheet13_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet13_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet13_tbl[i,"raw_text"] == toupper(datasheet13_tbl[i,"raw_text"]) ){
    datasheet13_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet13_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet13_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet13_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet13_tbl)) {
  if(trimws(datasheet13_tbl[i, "identifier"]) == ""){
    datasheet13_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet13_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet13_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet13_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet13_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet13_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet13_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet13_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet13_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet13_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet13_tbl_final[question_num,"qnum"] <- question_num
      datasheet13_tbl_final[question_num,"completion"] <- completion_answer
      datasheet13_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet13_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet13_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet13_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet13_tbl_final[question_num,"qnum"] <- question_num
    datasheet13_tbl_final[question_num,"completion"] <- completion_answer
    datasheet13_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet13_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet13_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet13_tbl_final, file = "./inputs/data/cleaned/datasheet13.csv", row.names=FALSE)

#### datasheet 15: ####

# read in datasheet file
datasheet15 <- pdftools::pdf_text("inputs/data/raw/datasheet15.pdf")

# put data in a tibble
datasheet15_tbl <- tibble(
  raw_text = datasheet15,
  # the following changes for each datasheet
  page_number = c(1:9)
)

# this separates each new line into a separate row
datasheet15_tbl <-
  separate_rows(datasheet15_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet15_tbl <- datasheet15_tbl[-which(datasheet15_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet15_tbl <- datasheet15_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet15_tbl)) {
  if(substr(trimws(datasheet15_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet15_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet15_tbl[i,"raw_text"] == toupper(datasheet15_tbl[i,"raw_text"]) ){
    datasheet15_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet15_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet15_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet15_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet15_tbl)) {
  if(trimws(datasheet15_tbl[i, "identifier"]) == ""){
    datasheet15_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet15_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet15_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet15_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet15_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet15_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet15_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet15_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet15_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet15_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet15_tbl_final[question_num,"qnum"] <- question_num
      datasheet15_tbl_final[question_num,"completion"] <- completion_answer
      datasheet15_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet15_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet15_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet15_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet15_tbl_final[question_num,"qnum"] <- question_num
    datasheet15_tbl_final[question_num,"completion"] <- completion_answer
    datasheet15_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet15_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet15_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet15_tbl_final, file = "./inputs/data/cleaned/datasheet15.csv", row.names=FALSE)
#### datasheet 16: ####

# read in datasheet file
datasheet16 <- pdftools::pdf_text("inputs/data/raw/datasheet16.pdf")

# put data in a tibble
datasheet16_tbl <- tibble(
  raw_text = datasheet16,
  # the following changes for each datasheet
  page_number = c(1:11)
)

# this separates each new line into a separate row
datasheet16_tbl <-
  separate_rows(datasheet16_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet16_tbl <- datasheet16_tbl[-which(datasheet16_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet16_tbl <- datasheet16_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet16_tbl)) {
  if(substr(trimws(datasheet16_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet16_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet16_tbl[i,"raw_text"] == toupper(datasheet16_tbl[i,"raw_text"]) ){
    datasheet16_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet16_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet16_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet16_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet16_tbl)) {
  if(trimws(datasheet16_tbl[i, "identifier"]) == ""){
    datasheet16_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet16_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet16_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet16_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet16_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet16_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet16_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet16_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet16_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet16_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet16_tbl_final[question_num,"qnum"] <- question_num
      datasheet16_tbl_final[question_num,"completion"] <- completion_answer
      datasheet16_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet16_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet16_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet16_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet16_tbl_final[question_num,"qnum"] <- question_num
    datasheet16_tbl_final[question_num,"completion"] <- completion_answer
    datasheet16_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet16_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet16_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet16_tbl_final, file = "./inputs/data/cleaned/datasheet16.csv", row.names=FALSE)

#### datasheet 17: ####

# read in datasheet file
datasheet17 <- pdftools::pdf_text("inputs/data/raw/datasheet17.pdf")

# put data in a tibble
datasheet17_tbl <- tibble(
  raw_text = datasheet17,
  # the following changes for each datasheet
  page_number = c(1:8)
)

# this separates each new line into a separate row
datasheet17_tbl <-
  separate_rows(datasheet17_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet17_tbl <- datasheet17_tbl[-which(datasheet17_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet17_tbl <- datasheet17_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet17_tbl)) {
  if(substr(trimws(datasheet17_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet17_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet17_tbl[i,"raw_text"] == toupper(datasheet17_tbl[i,"raw_text"]) ){
    datasheet17_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet17_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet17_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet17_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet17_tbl)) {
  if(trimws(datasheet17_tbl[i, "identifier"]) == ""){
    datasheet17_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet17_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet17_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet17_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet17_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet17_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet17_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet17_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet17_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet17_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet17_tbl_final[question_num,"qnum"] <- question_num
      datasheet17_tbl_final[question_num,"completion"] <- completion_answer
      datasheet17_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet17_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet17_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet17_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet17_tbl_final[question_num,"qnum"] <- question_num
    datasheet17_tbl_final[question_num,"completion"] <- completion_answer
    datasheet17_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet17_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet17_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet17_tbl_final, file = "./inputs/data/cleaned/datasheet17.csv", row.names=FALSE)

#### datasheet 18: ####

# read in datasheet file
datasheet18 <- pdftools::pdf_text("inputs/data/raw/datasheet18.pdf")

# put data in a tibble
datasheet18_tbl <- tibble(
  raw_text = datasheet18,
  # the following changes for each datasheet
  page_number = c(1:9)
)

# this separates each new line into a separate row
datasheet18_tbl <-
  separate_rows(datasheet18_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet18_tbl <- datasheet18_tbl[-which(datasheet18_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet18_tbl <- datasheet18_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet18_tbl)) {
  if(substr(trimws(datasheet18_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet18_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet18_tbl[i,"raw_text"] == toupper(datasheet18_tbl[i,"raw_text"]) ){
    datasheet18_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet18_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet18_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet18_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet18_tbl)) {
  if(trimws(datasheet18_tbl[i, "identifier"]) == ""){
    datasheet18_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet18_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet18_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet18_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet18_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet18_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet18_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet18_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet18_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet18_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet18_tbl_final[question_num,"qnum"] <- question_num
      datasheet18_tbl_final[question_num,"completion"] <- completion_answer
      datasheet18_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet18_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet18_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet18_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet18_tbl_final[question_num,"qnum"] <- question_num
    datasheet18_tbl_final[question_num,"completion"] <- completion_answer
    datasheet18_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet18_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet18_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet18_tbl_final, file = "./inputs/data/cleaned/datasheet18.csv", row.names=FALSE)

#### datasheet 19: ####

# read in datasheet file
datasheet19 <- pdftools::pdf_text("inputs/data/raw/datasheet19.pdf")

# put data in a tibble
datasheet19_tbl <- tibble(
  raw_text = datasheet19,
  # the following changes for each datasheet
  page_number = c(1:10)
)

# this separates each new line into a separate row
datasheet19_tbl <-
  separate_rows(datasheet19_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet19_tbl <- datasheet19_tbl[-which(datasheet19_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet19_tbl <- datasheet19_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet19_tbl)) {
  if(substr(trimws(datasheet19_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet19_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet19_tbl[i,"raw_text"] == toupper(datasheet19_tbl[i,"raw_text"]) ){
    datasheet19_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet19_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet19_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet19_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet19_tbl)) {
  if(trimws(datasheet19_tbl[i, "identifier"]) == ""){
    datasheet19_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet19_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet19_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet19_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet19_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet19_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet19_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet19_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet19_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet19_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet19_tbl_final[question_num,"qnum"] <- question_num
      datasheet19_tbl_final[question_num,"completion"] <- completion_answer
      datasheet19_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet19_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet19_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet19_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet19_tbl_final[question_num,"qnum"] <- question_num
    datasheet19_tbl_final[question_num,"completion"] <- completion_answer
    datasheet19_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet19_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet19_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet19_tbl_final, file = "./inputs/data/cleaned/datasheet19.csv", row.names=FALSE)

#### datasheet 20: ####

# read in datasheet file
datasheet20 <- pdftools::pdf_text("inputs/data/raw/datasheet20.pdf")

# put data in a tibble
datasheet20_tbl <- tibble(
  raw_text = datasheet20,
  # the following changes for each datasheet
  page_number = c(1:8)
)

# this separates each new line into a separate row
datasheet20_tbl <-
  separate_rows(datasheet20_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet20_tbl <- datasheet20_tbl[-which(datasheet20_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet20_tbl <- datasheet20_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet20_tbl)) {
  if(substr(trimws(datasheet20_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet20_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet20_tbl[i,"raw_text"] == toupper(datasheet20_tbl[i,"raw_text"]) ){
    datasheet20_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet20_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet20_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet20_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet20_tbl)) {
  if(trimws(datasheet20_tbl[i, "identifier"]) == ""){
    datasheet20_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet20_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet20_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet20_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet20_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet20_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet20_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet20_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet20_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet20_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet20_tbl_final[question_num,"qnum"] <- question_num
      datasheet20_tbl_final[question_num,"completion"] <- completion_answer
      datasheet20_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet20_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet20_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet20_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet20_tbl_final[question_num,"qnum"] <- question_num
    datasheet20_tbl_final[question_num,"completion"] <- completion_answer
    datasheet20_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet20_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet20_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet20_tbl_final, file = "./inputs/data/cleaned/datasheet20.csv", row.names=FALSE)

#### datasheet 21: ####

# read in datasheet file
datasheet21 <- pdftools::pdf_text("inputs/data/raw/datasheet21.pdf")

# put data in a tibble
datasheet21_tbl <- tibble(
  raw_text = datasheet21,
  # the following changes for each datasheet
  page_number = c(1:8)
)

# this separates each new line into a separate row
datasheet21_tbl <-
  separate_rows(datasheet21_tbl, raw_text, sep = "\\n", convert = FALSE)

# remove rows that are blank in first column
# source: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
datasheet21_tbl <- datasheet21_tbl[-which(datasheet21_tbl$raw_text == ""),]


# create column that identifies section header, question rows, and answer rows
# this will enable counting for completion and word count 

# creates a column called identifier with null in the value
datasheet21_tbl <- datasheet21_tbl %>% add_column(identifier = "null")

# loop through each row
for(i in 1:nrow(datasheet21_tbl)) {
  if(substr(trimws(datasheet21_tbl[i,"raw_text"]),0,7) == "[answer" ){
    datasheet21_tbl[i, "identifier"] <-  "Answer"
  }
  else if (datasheet21_tbl[i,"raw_text"] == toupper(datasheet21_tbl[i,"raw_text"]) ){
    datasheet21_tbl[i, "identifier"] <-  "Header"
  } 
  else if( substr(trimws(datasheet21_tbl[i,"raw_text"]),0,9) == "[question" ){
    datasheet21_tbl[i, "identifier"] <-  "Question"
  }
  else{
    datasheet21_tbl[i, "identifier"] <- ""
  }
}

current <- "nothing"
# loop through and downfill
for(i in 1:nrow(datasheet21_tbl)) {
  if(trimws(datasheet21_tbl[i, "identifier"]) == ""){
    datasheet21_tbl[i, "identifier"] <- current
  }
  else{
    current = datasheet21_tbl[i, "identifier"]
  }
}

# create dataset for datasheet summary details # 

# set up new df
datasheet21_tbl_final <- data.frame(qnum=c(0),completion=c(""),length_words=c(0),top_5_frequent_words=c(""),datasheet_version=c(""))

# initiate vars
question_num <- 1
completion_answer <- "nothing"
length_of_words <- 0
top_5 <- ""
datasheet_version <-"2021"
SKIPA <- "NO"
SKIPB <- "YES"
answer_started <- "FALSE"

# loop through the dataframe and grab
for(i in 1:nrow(datasheet21_tbl)) {       # for-loop over rows
  
  if(question_num == 50){
    # print(head(stopwords::stopwords("english"),100))
    print( datasheet21_tbl[i, "raw_text"])
  }
  
  if(trimws(datasheet21_tbl[i, "identifier"]) == "Answer"){
    answer_started <- "TRUE"
    
    # calculates words for each response
    length_of_words <- length_of_words + length(strsplit(trimws(datasheet21_tbl[i, "raw_text"]), " ")[[1]]) 
    top_5 <- paste(top_5, trimws(datasheet21_tbl[i, "raw_text"]), sep=" ")
    
    # stores value into SKIPA 
    if(trimws(datasheet21_tbl[i, "raw_text"]) == "[SkipA] YES"){
      SKIPA <- "YES"
    }
    
    # stores value into SKIPB
    if(trimws(datasheet21_tbl[i, "raw_text"]) == "[SkipB] YES"){
      SKIPB <- "YES"
    }
  }
  else{
    if(answer_started == "TRUE"){
      
      # find top 5 words
      words <- strsplit(top_5, " ", fixed = T)
      words <- unlist(words)
      counts <- table(words)
      
      # order it based on count descending
      counts <- counts[order(counts,decreasing = TRUE)]
      
      # get list of words
      wordList <- names(counts)
      
      # remove stopwords from list
      wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"), 200) ]
      
      # remove blanks and tags
      wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
      
      # select top 5 remaining
      wordList <- wordList[1:5]
      
      top_5 <- paste(wordList, collapse=", ")
      
      
      # check logic of whether a question was completed
      if(length_of_words > 1){
        completion_answer <- "Yes"
      }
      # if  skipa is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      # if  skipb is true then some questions are automatically considered completed
      else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
        completion_answer <- "Yes"
        top_5 <- ""
      }
      else{
        completion_answer <- "No"
        top_5 <- ""
      }
      
      # add to df
      datasheet21_tbl_final[question_num,"qnum"] <- question_num
      datasheet21_tbl_final[question_num,"completion"] <- completion_answer
      datasheet21_tbl_final[question_num,"length_words"] <- length_of_words - 1 # -1 b/c of the tag
      datasheet21_tbl_final[question_num,"top_5_frequent_words"] <- top_5
      datasheet21_tbl_final[question_num,"datasheet_version"] <- datasheet_version
      
      question_num <- question_num + 1
      length_of_words <- 0
      answer_started <- "FALSE"
      completion_answer <- "Nothing"
      top_5 <- ""
    }
    
  }
  
  # if its last row 
  if(nrow(datasheet21_tbl) == i)
  {
    # find top 5 words
    words <- strsplit(top_5, " ", fixed = T)
    words <- unlist(words)
    counts <- table(words)
    
    # order it based on count descending
    counts <- counts[order(counts,decreasing = TRUE)]
    
    # get list of words
    wordList <- names(counts)
    
    # remove stopwords from list
    wordList <- wordList[!wordList %in% head(stopwords::stopwords("english"),200) ]
    
    # remove blanks and tags
    wordList <- wordList[!wordList %in% c("","[answer1]","[answer2]","[answer3]","[answer4]","[answer5]","[answer6]","[answer7]","[answer8]","[answer9]","[answer10]","[answer11]","[answer12]","[answer13]","[answer14]","[answer15]","[answer16]","[answer17]","[answer18]","[answer19]","[answer20]","[answer21]","[answer22]","[answer23]","[answer24]","[answer25]","[answer26]","[answer27]","[answer28]","[answer29]","[answer30]","[answer31]","[answer32]","[answer33]","[answer34]","[answer35]","[answer36]","[answer37]","[answer38]","[answer39]","[answer40]","[answer41]","[answer42]","[answer43]","[answer44]","[answer45]","[answer46]","[answer47]","[answer48]","[answer49]","[answer50]","[answer51]", "[answer52]", "[answer53]", "[answer54]", "[answer55]", "[answer56]", "[answer57]")] 
    
    # select top 5 remaining
    wordList <- wordList[1:5]
    
    top_5 <- paste(wordList, collapse = ", ")
    
    # check logic of completion
    if(length_of_words > 1){
      completion_answer <- "Yes"
    }
    else if (datasheet_version == "2021" && SKIPA == "YES" && question_num %in% c(17, 18, 19, 20) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else if (datasheet_version == "2021" && SKIPB == "YES" && question_num %in% c(27, 28,29, 30, 31, 32) ){
      completion_answer <- "Yes"
      top_5 <- ""
    }
    else{
      completion_answer <- "No"
      top_5 <- ""
    }
    
    # add to df
    datasheet21_tbl_final[question_num,"qnum"] <- question_num
    datasheet21_tbl_final[question_num,"completion"] <- completion_answer
    datasheet21_tbl_final[question_num,"length_words"] <- length_of_words - 1
    datasheet21_tbl_final[question_num,"top_5_frequent_words"] <- top_5
    datasheet21_tbl_final[question_num,"datasheet_version"] <- datasheet_version
    
    
  }
}

write.csv(datasheet21_tbl_final, file = "./inputs/data/cleaned/datasheet21.csv", row.names=FALSE)

