# NOTE: this doesn't seem to work correctly yet. After a few iterations, some counts in `r` become negative, and I'm not sure why.


# Implementation of Gibbs Sampling algorithm for Dirichlet Mixture Model, as described in
# "Yin, J., & Wang, J. (2014, August). A dirichlet multinomial mixture model-based approach for short text clustering. [...]" 
# Link to paper: https://dl.acm.org/doi/10.1145/2623330.2623715

# PARAMETERS
# V number of words in the vocabulary
# D number of documents in the corpus
# Lbar average length of documents
# vec(d) documents in the corpus
# vec(z) cluster labels of each document
# I number of iterations
# m_z number of documents in cluster z
# n_z number of words in cluster z
# n_z^w number of occurrences of word w in cluster z
# N_d number of words in document d
# N_d^w number of occurrences of word w in document d

# Libraries
library(tidytext)
library(dplyr)
library(stringr)

# Our test data
setwd("C://Users/tysonp/Desktop/data-science-projects/ice-cream-data-dashboard/data/ice-cream-dataset-v2/combined")
rev_all <- read.csv("reviews.csv",encoding = "UTF-8")
rev <- rev_all %>% filter(brand == "bj")
rev$text = str_squish(rev$text)
allWords <- strsplit(rev$text, split=" ")

# Initialize parameters, note we use different names for variables containing subscripts (i.e. m_z --> m)
V <- length(unique(unlist(allWords))) # vocab size
d <- rev$text # vector of documents
D <- length(d) # number of documents
z <- rep(0, D) # vector of cluster labels
K <- 5 # number of clusters
I <- 5 # number of iterations
alpha <- 0.1
beta <- 0.1
m <- rep(0, K) # m_z -- number of documents belonging to cluster z
n <- rep(0, K) # n_z -- number of words belonging to cluster z
r <- rep(list(NA),K) # n_z.w -- number of words in document d belonging to cluster z
names(r) <- 1:K
for(name in names(r)){
  r[[name]] = list()
}

# -------------- ALGORITHM ----------------
# Initialize clusters
for (dd in 1:D){
  zd <- sample(1:K, size=1)
  z[dd] <- zd
  m[zd] <- m[zd] + 1
  words <- strsplit(d[dd], split=" ")[[1]]
  n[zd] <- n[zd] + length(words)
  word_counts <- as.list(table(words))
  for(w in names(word_counts)){
    if(w %in% names(r[[zd]])){
      r[[zd]][[w]] <- r[[zd]][[w]] + word_counts[[w]]
    }
    else {
      r[[zd]][[w]] <- word_counts[[w]]
    }
  }
}

# Define GSDMM's sampling mechanism
`%notin%` <- Negate(`%in%`)
GSDMM_sample <- function(oldz, m, n, r, words, word_counts, alpha, beta, D, K, V){
  probs <- rep(0,K)
  
  for(newz in 1:K){
    prefactor <- log( (m[newz] + alpha)/(D - 1 + K*alpha) )
    
    # numerator double product
    p1 <- 0
    for(w in names(word_counts)){
      if(w %notin% names(r[[newz]])){
        r[[newz]][[w]] <- 0
      }
      if (r[[newz]][[w]] < 0){
          print(r[[newz]][[w]])
      }
      for(j in 1:word_counts[[w]]){
        # need to fix this. there is an issue with r[[newz]][[w]] giving negative numbers as low as -2.     
        p1 <- p1 + log(r[[newz]][[w]] + beta + j - 1)
      }
    }
    
    # denominator product
    p2 <- 0
    for(i in 1:length(words)){
      p2 <- p2 + log(n[newz] + V*beta + i - 1)
    }
    
    #print(paste(prefactor, p1, p2))
    probs[newz] <- exp(prefactor+p1-p2)
  }

    # if 1 NaN, return this cluster index
  if(sum(is.nan(probs))==1){
    ret <- which(is.nan(probs))
  }
  # if > 1 NaN or all zeros, stay with cluster
  else if((sum(is.nan(probs))>1)|(sum(probs)<=0)){
    ret <- oldz
  }
  # else sample new
  else {
    ret <- sample(1:K, size=1, prob=probs)
  }
  if(ret != oldz){
    # print("Moving clusters")
  }
  
  return(ret)
}

# Iterate through all documents `I` times and sample according to GSDMM mechanism
for(iter in 1:I){
  print(paste("Iteration", iter))
  for(dd in 1:D){
    if(dd %% 200 == 0){
      print(paste("Document", dd))
    }
    
    # remove document dd from cluster z; then update m, n, r (these give the negative subscript versions of m, n, r in the paper)
    zd <- z[dd]
    m[zd] <- m[zd] - 1
    words <- strsplit(d[dd], split=" ")[[1]]
    n[zd] <- n[zd] - length(words)
    word_counts <- as.list(table(words))
    for(w in names(word_counts)){
        r[[zd]][[w]] <- r[[zd]][[w]] - word_counts[[w]]
    }
    
    # choose a new cluster z for document dd; then reupdate m, n, r 
    zd <- GSDMM_sample(zd, m, n, r, words, word_counts, alpha, beta, D, K, V)
    z[dd] <- zd
    m[zd] <- m[zd] + 1
    words <- strsplit(d[dd], split=" ")[[1]]
    n[zd] <- n[zd] + length(words)
    word_counts <- as.list(table(words))
    for(w in names(word_counts)){
      if(w %in% names(r[[zd]])){
        r[[zd]][[w]] <- r[[zd]][[w]] + word_counts[[w]]
      }
      else {
        r[[zd]][[w]] <- word_counts[[w]]
      }
    }
    
  }
}
# ------------- END ALGORITHM ------------------
