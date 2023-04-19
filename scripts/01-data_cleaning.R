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

datasheet1 <- pdftools::pdf_text("inputs/data/raw/datasheet1.pdf")
datasheet1

datasheet1_tbl <- tibble(
  raw_text = datasheet1,
  page_number = c(1:7)
)

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





#### datasheet2: #####

datasheet2 <- pdftools::pdf_text("inputs/data/raw/datasheet2.pdf")
datasheet2

datasheet2_tbl <- tibble(
  raw_text = datasheet2,
  page_number = c(1:8)
)

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
