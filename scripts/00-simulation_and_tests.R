#### Preamble ####
# Purpose: Simulations for individual datasheets and summary dataset
# Author: Eshta Bhardwaj
# Date: April 21 2023
# Contact: eshta.bhardwaj@mail.utoronto.ca
# License: MIT

#### Workspace set-up ####
library(tidyverse)
# the markheckmann/dissertation package which has the randomWords function

# the below was copied from this source: https://rdrr.io/github/markheckmann/dissertation/src/R/utils-imports.R
# originally functions could be called by installing the following package: remotes::install_github("markheckmann/dissertation")
# but the github page is no longer supported which is why i copied the source code here 

#### source code for stopWords function ####

###############################################################################
### Functions taken from other packages which shall not be loaded due to    ###
### too much overhead or additional dependencies. Hence they are not        ### 
### included as imports.                                                    ###
###############################################################################

#### from maptools ####

#' Label placement for points to avoid overlaps
#' 
#' pointLabel was taken from package maptools. maptools is not imported 
#' or mentioned in DESCRIPTION to reduce dependencies as maptools 
#' requires sp and gpclib. Below is the exact maptools::pointLabel code.
#' See pointLabel in maptools for a description of the arguments
#' 
#' @param x,y,labels,cex,method,allowSmallOverlap,trace,doPlot see maptools::pointLabel
#' @param ... see maptools::pointLabel
#' @export
#' @keywords internal
#'  
pointLabel <- function (x, y = NULL, labels = seq(along = x), cex = 1, 
                        method = c("SANN", "GA"), allowSmallOverlap = FALSE, 
                        trace = FALSE, doPlot = TRUE, ...) 
{
  if (!missing(y) && (is.character(y) || is.expression(y))) {
    labels <- y
    y <- NULL
  }
  labels <- as.graphicsAnnot(labels)
  boundary <- par()$usr
  xyAspect <- par()$pin[1]/par()$pin[2]
  toUnityCoords <- function(xy) {
    list(x = (xy$x - boundary[1])/(boundary[2] - boundary[1]) * 
           xyAspect, y = (xy$y - boundary[3])/(boundary[4] - 
                                                 boundary[3])/xyAspect)
  }
  toUserCoords <- function(xy) {
    list(x = boundary[1] + xy$x/xyAspect * (boundary[2] - 
                                              boundary[1]), y = boundary[3] + xy$y * xyAspect * 
           (boundary[4] - boundary[3]))
  }
  z <- xy.coords(x, y, recycle = TRUE)
  z <- toUnityCoords(z)
  x <- z$x
  y <- z$y
  if (length(labels) < length(x)) 
    labels <- rep(labels, length(x))
  method <- match.arg(method)
  if (allowSmallOverlap) 
    nudgeFactor <- 0.02
  n_labels <- length(x)
  width <- (strwidth(labels, units = "figure", cex = cex) + 
              0.015) * xyAspect
  height <- (strheight(labels, units = "figure", cex = cex) + 
               0.015)/xyAspect
  gen_offset <- function(code) c(-1, -1, -1, 0, 0, 1, 1, 1)[code] * 
    (width/2) + (0+1i) * c(-1, 0, 1, -1, 1, -1, 0, 1)[code] * 
    (height/2)
  rect_intersect <- function(xy1, offset1, xy2, offset2) {
    w <- pmin(Re(xy1 + offset1/2), Re(xy2 + offset2/2)) - 
      pmax(Re(xy1 - offset1/2), Re(xy2 - offset2/2))
    h <- pmin(Im(xy1 + offset1/2), Im(xy2 + offset2/2)) - 
      pmax(Im(xy1 - offset1/2), Im(xy2 - offset2/2))
    w[w <= 0] <- 0
    h[h <= 0] <- 0
    w * h
  }
  nudge <- function(offset) {
    doesIntersect <- rect_intersect(xy[rectidx1] + offset[rectidx1], 
                                    rectv[rectidx1], xy[rectidx2] + offset[rectidx2], 
                                    rectv[rectidx2]) > 0
    pyth <- abs(xy[rectidx1] + offset[rectidx1] - xy[rectidx2] - 
                  offset[rectidx2])/nudgeFactor
    eps <- 1e-10
    for (i in which(doesIntersect & pyth > eps)) {
      idx1 <- rectidx1[i]
      idx2 <- rectidx2[i]
      vect <- (xy[idx1] + offset[idx1] - xy[idx2] - offset[idx2])/pyth[idx1]
      offset[idx1] <- offset[idx1] + vect
      offset[idx2] <- offset[idx2] - vect
    }
    offset
  }
  objective <- function(gene) {
    offset <- gen_offset(gene)
    if (allowSmallOverlap) 
      offset <- nudge(offset)
    if (!is.null(rectidx1)) 
      area <- sum(rect_intersect(xy[rectidx1] + offset[rectidx1], 
                                 rectv[rectidx1], xy[rectidx2] + offset[rectidx2], 
                                 rectv[rectidx2]))
    else area <- 0
    n_outside <- sum(Re(xy + offset - rectv/2) < 0 | Re(xy + 
                                                          offset + rectv/2) > xyAspect | Im(xy + offset - rectv/2) < 
                       0 | Im(xy + offset + rectv/2) > 1/xyAspect)
    res <- 1000 * area + n_outside
    res
  }
  xy <- x + (0+1i) * y
  rectv <- width + (0+1i) * height
  rectidx1 <- rectidx2 <- array(0, (length(x)^2 - length(x))/2)
  k <- 0
  for (i in 1:length(x)) for (j in seq(len = (i - 1))) {
    k <- k + 1
    rectidx1[k] <- i
    rectidx2[k] <- j
  }
  canIntersect <- rect_intersect(xy[rectidx1], 2 * rectv[rectidx1], 
                                 xy[rectidx2], 2 * rectv[rectidx2]) > 0
  rectidx1 <- rectidx1[canIntersect]
  rectidx2 <- rectidx2[canIntersect]
  if (trace) 
    cat("possible intersects =", length(rectidx1), "\n")
  if (trace) 
    cat("portion covered =", sum(rect_intersect(xy, rectv, 
                                                xy, rectv)), "\n")
  GA <- function() {
    n_startgenes <- 1000
    n_bestgenes <- 30
    prob <- 0.2
    mutate <- function(gene) {
      offset <- gen_offset(gene)
      doesIntersect <- rect_intersect(xy[rectidx1] + offset[rectidx1], 
                                      rectv[rectidx1], xy[rectidx2] + offset[rectidx2], 
                                      rectv[rectidx2]) > 0
      for (i in which(doesIntersect)) {
        gene[rectidx1[i]] <- sample(1:8, 1)
      }
      for (i in seq(along = gene)) if (runif(1) <= prob) 
        gene[i] <- sample(1:8, 1)
      gene
    }
    crossbreed <- function(g1, g2) ifelse(sample(c(0, 1), 
                                                 length(g1), replace = TRUE) > 0.5, g1, g2)
    genes <- matrix(sample(1:8, n_labels * n_startgenes, 
                           replace = TRUE), n_startgenes, n_labels)
    for (i in 1:10) {
      scores <- array(0, NROW(genes))
      for (j in 1:NROW(genes)) scores[j] <- objective(genes[j, 
      ])
      rankings <- order(scores)
      genes <- genes[rankings, ]
      bestgenes <- genes[1:n_bestgenes, ]
      bestscore <- scores[rankings][1]
      if (bestscore == 0) {
        if (trace) 
          cat("overlap area =", bestscore, "\n")
        break
      }
      genes <- matrix(0, n_bestgenes^2, n_labels)
      for (j in 1:n_bestgenes) for (k in 1:n_bestgenes) genes[n_bestgenes * 
                                                                (j - 1) + k, ] <- mutate(crossbreed(bestgenes[j, 
                                                                ], bestgenes[k, ]))
      genes <- rbind(bestgenes, genes)
      if (trace) 
        cat("overlap area =", bestscore, "\n")
    }
    nx <- Re(xy + gen_offset(bestgenes[1, ]))
    ny <- Im(xy + gen_offset(bestgenes[1, ]))
    list(x = nx, y = ny)
  }
  SANN <- function() {
    gene <- rep(8, n_labels)
    score <- objective(gene)
    bestgene <- gene
    bestscore <- score
    T <- 2.5
    for (i in 1:50) {
      k <- 1
      for (j in 1:50) {
        newgene <- gene
        newgene[sample(1:n_labels, 1)] <- sample(1:8, 
                                                 1)
        newscore <- objective(newgene)
        if (newscore <= score || runif(1) < exp((score - 
                                                 newscore)/T)) {
          k <- k + 1
          score <- newscore
          gene <- newgene
        }
        if (score <= bestscore) {
          bestscore <- score
          bestgene <- gene
        }
        if (bestscore == 0 || k == 10) 
          break
      }
      if (bestscore == 0) 
        break
      if (trace) 
        cat("overlap area =", bestscore, "\n")
      T <- 0.9 * T
    }
    if (trace) 
      cat("overlap area =", bestscore, "\n")
    nx <- Re(xy + gen_offset(bestgene))
    ny <- Im(xy + gen_offset(bestgene))
    list(x = nx, y = ny)
  }
  if (method == "SANN") 
    xy <- SANN()
  else xy <- GA()
  xy <- toUserCoords(xy)
  if (doPlot) 
    text(xy, labels, cex = cex, ...)
  invisible(xy)
}





#### from my OpenRepGrid package ####

#' Generate a random word
#' 
#' randomWords generates a vector of random words taken from a small 
#' set of words
#' @param n number of words to be generated (integer)
#' @return a string with n words (if length is not constrained)
#' @export
#' @keywords internal
#' @examples
#' randomWords(10)  # 10 random words
randomWords <- function(n)
{
  if (! is.numeric(n))
    stop("n must be an integer")
  words <- c( "the", "novel", "depicts", "Harry", "as", "an", "essentially",
              "good", "man", "who", "is", "forced", "into", "blackmarket", 
              "activity", "by",	"economic", "forces", "beyond", "his", 
              "control", "initially", "his", "fishing", "charter", 
              "customer", "Mr.", "Johnson", "tricks", "Mark", "by", 
              "slipping", "away", "without", "paying", "any", "of", "the",
              "money", "he", "owes", "him", "Brownstone", "then", "flees", 
              "back", "to", "the", "mainland", "by", "airplane", "before", 
              "he", "realizes", "what", "has", "happened", "I", "she")
  sample(words, n, replace=TRUE)
}


#' Generate a random sentence with n words
#'
#' @param n   number of word in sentence
#' @param maxchar   maximal number of characters per sentence. Note that whole 
#'                  words (not part of words) are excluded if the maximal number 
#'                   is exceeded.
#' @return a string with n words (if length is not constrained)
#' @export
#' @keywords internal
#' @examples  
#' randomSentence(10)   # one random sentence with 10 words
randomSentence <- function(n, maxchar=Inf)
{
  x <- paste(randomWords(n), collapse=" ")
  x.split <- strsplit(x, " ")[[1]]
  chars <- as.vector(sapply(x.split, nchar))
  paste(unlist(x.split[cumsum(chars) < maxchar]), collapse = " ")
}


#' Generate n random sentences with a given or random number of words
#'
#' @param n         number of sentences to be generate (integer)
#' @param nwords    number of words per sentence. If vector each sentence
#'           lengths is randomly drawn from the vector
#' @param maxchar   maximal number of characters per sentence. Note that whole 
#'           words (not part of words) are excluded if the maximal number 
#'          is exceeded.
#' @return a vector with n random sentences
#' @export
#' @keywords internal
#' @examples
#' randomSentences(5, 10)     # five random sentences with ten words each
#' randomSentences(5, 1:5)   # five random sentences between two and ten words
randomSentences <- function(n, nwords, maxchar=Inf)
{
  if (length(nwords) == 1)
    nwords <- rep(nwords, n)
  sapply(nwords, randomSentence, maxchar = maxchar)
}

#### my simulations ####

## prep vars ##
set.seed(7)

datasheet_v = c(2018, 2021)
completion = c('Yes', 'No')

# initalize empty list
words_list_21 <- list()
# create list of 21 items with 5 random words
for (i in 1:21) {
  words_list_21[i] = paste(c(randomWords(1), randomWords(1), randomWords(1), 
                             randomWords(1), randomWords(1)), collapse = ', ')
  i = i+1
}
# unlist the list
words_list_unlist_21 <- unlist(words_list_21, recursive = FALSE)

# initalize empty list
words_list_57 <- list()
# create list of 57 items with 5 random words
for (i in 1:57) {
  words_list_57[i] = paste(c(randomWords(1), randomWords(1), randomWords(1), 
                             randomWords(1), randomWords(1)), collapse = ', ')
  i = i+1
}
# unlist the list
words_list_unlist_57 <- unlist(words_list_57, recursive = FALSE)

## simulated tibble 1 ##

simulated_summary_data <- tibble(datasheet_ID = 1:21, # 21 total datasheets
                                 datasheet_version = sample(datasheet_v, 21, replace=TRUE), # version can be 2018 or 2021
                                 title_short = randomWords(n = 21), # random list of 21 words to represent titles
                                 total_length_wrds = sample(x = 500:5000, size = 21), # total length will be b/w 1-5000
                                 question_completion_pct = sample(x = 1:100, size = 21), # pct is b/w 1-100
                                 avg_response_length = sample(x = 10:60, size = 21), # avg length b/w 1-60 words per response
                                 max_response_length = sample(x = 100:600, size = 21), # max response length b/w 100-600 words
                                 max_response_qnum = sample(x = 1:57, size = 21), # there are 57 questions
                                 min_response_length = sample(x = 0:100, size = 21), # max response length b/w 0-100 words
                                 min_response_qnum = sample(x = 1:57, size = 21, replace = FALSE), # there are 57 questions
                                 overall_top_5_words = sample(words_list_unlist_21, 21), # random list of 5 words to represent top 5 words
                                 score_pct = sample(x = 1:100, size = 21)) # pct is b/w 1-100
                                 
## simulated tibble 2 ##

simulated_datasheet_data <- tibble(qnum = 1:57,
                                   completion = sample(completion, 57, replace=TRUE),
                                   length_words = sample(x = 0:600, size = 57),
                                   top_5_words = sample(words_list_unlist_57, 57))

  
#### tests ####

for(i in seq_along(simulated_summary_data)) {
  if (simulated_summary_data$question_completion_pct[i] < 0 || simulated_summary_data$question_completion_pct[i] > 100 ) {
    print("Completion Percentage must be between 0 and 100")
  }
  i = i+1
}

for(i in seq_along(simulated_summary_data)) {
  if (simulated_summary_data$score_pct[i] < 0 || simulated_summary_data$score_pct[i] > 100 ) {
    print("Score Percentage must be between 0 and 100")
  }
  i = i+1
}

for(i in seq_along(simulated_summary_data)) {
  if (simulated_summary_data$datasheet_version[i] != 2018 && simulated_summary_data$datasheet_version[i] != 2021 ) {
    print("Datasheet version must be 2018 or 2021")
  }
  i = i+1
}

for(i in seq_along(simulated_summary_data)) {
  if (simulated_summary_data$datasheet_ID[i] < 1 || simulated_summary_data$datasheet_ID[i] > 21 ) {
    print("Datasheet ID must be between 1 and 21 since there are only 21 datasheets")
  }
  i = i+1
}

for(i in seq_along(simulated_datasheet_data)) {
  if (simulated_datasheet_data$completion[i] != 'Yes' && simulated_datasheet_data$completion[i] != 'No' ) {
    print("Completion must be recorded  as 'Yes' or 'No'")
  }
  i = i+1
}


