#### Preamble ####
# Purpose: Cleaning and formatting datasheets
# Author: Eshta Bhardwaj
# Date: April 21 2023
# Contact: eshta.bhardwaj@mail.utoronto.ca
# License: MIT
# Pre-requisites: Must already have pdfs of datasheets downloaded. Pdf must be in single column format.

#### Workspace set-up ####
library(tidyverse)
library(pdftools)

#### reading in blank datasheet to establish baselines ####

# 2021

datasheet_questions_2021 <- pdftools::pdf_text("inputs/data/2021_Datasheet_questionsonly.pdf")
datasheet_questions_2021

datasheet_questions_2021tbl <- tibble(
  raw_text = datasheet_questions_2021,
  page_number = c(1:6)
)

datasheet_questions_2021tbl <-
  separate_rows(datasheet_questions_2021tbl, raw_text, sep = "\\n", convert = FALSE)

# 2018

datasheet_questions_2018 <- pdftools::pdf_text("inputs/data/2018_Datasheet_questionsonly.pdf")
datasheet_questions_2018

datasheet_questions_2018tbl <- tibble(
  raw_text = datasheet_questions_2018,
  page_number = c(1:3)
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
