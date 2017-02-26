word_prediction <- function(string, trigram, bigram, unigram) {
  last_tokens <- tail(strsplit(string, split = " ", fixed=TRUE)[[1]], 2)
  uniword <- last_tokens[[-1]]
  biword <- paste(last_tokens[[-2]], last_tokens[[-1]])
  trigrams <- grep(paste("^", biword, sep = ""), trigram$word, value = TRUE)
    if (length(trigrams) >= 3) {
      print(tail(strsplit(trigrams[1], split = " ", fixed = TRUE)[[1]], 1))
      print(tail(strsplit(trigrams[2], split = " ", fixed = TRUE)[[1]], 1))
      print(tail(strsplit(trigrams[3], split = " ", fixed = TRUE)[[1]], 1))
      print(tail(strsplit(trigrams[4], split = " ", fixed = TRUE)[[1]], 1))
      print(tail(strsplit(trigrams[5], split = " ", fixed = TRUE)[[1]], 1))
    }
    else if (length(trigrams) == 2) {
      print(tail(strsplit(trigrams[1], split = " ", fixed = TRUE)[[1]], 1))
      print(tail(strsplit(trigrams[2], split = " ", fixed = TRUE)[[1]], 1))
      bigrams <- grep(paste("^", uniword, sep = ""), bigram$word, value = TRUE)
      print(tail(strsplit(bigrams[1], split = " ", fixed = TRUE)[[1]], 1))
      print(tail(strsplit(bigrams[2], split = " ", fixed = TRUE)[[1]], 1))
      print(tail(strsplit(bigrams[3], split = " ", fixed = TRUE)[[1]], 1))
    }
    else if (length(trigrams) == 1) {
      print(tail(strsplit(trigrams[1], split = " ", fixed = TRUE)[[1]], 1))
      bigrams <- grep(paste("^", uniword, sep = ""), bigram$word, value = TRUE)
      print(tail(strsplit(bigrams[1], split = " ", fixed = TRUE)[[1]], 1))
      print(tail(strsplit(bigrams[2], split = " ", fixed = TRUE)[[1]], 1))
      print(tail(strsplit(bigrams[3], split = " ", fixed = TRUE)[[1]], 1))
      print(tail(strsplit(bigrams[4], split = " ", fixed = TRUE)[[1]], 1))
    }
    else if (length(trigrams) == 0) {
        bigrams <- grep(paste("^", uniword, sep = ""), bigram$word, value = TRUE)
        print(tail(strsplit(bigrams[1], split = " ", fixed = TRUE)[[1]], 1))
        print(tail(strsplit(bigrams[2], split = " ", fixed = TRUE)[[1]], 1))
        print(tail(strsplit(bigrams[3], split = " ", fixed = TRUE)[[1]], 1))
        print(tail(strsplit(bigrams[4], split = " ", fixed = TRUE)[[1]], 1))
        print(tail(strsplit(bigrams[5], split = " ", fixed = TRUE)[[1]], 1))
    }
}