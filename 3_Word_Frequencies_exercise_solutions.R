library("stringr")

# setting up: 

url <- "https://raw.githubusercontent.com/OpenITI/0325AH/master/data/0310Tabari/0310Tabari.Tarikh/0310Tabari.Tarikh.Shamela0009783BK1-ara1.completed"
text_v <- scan(url, what="character", sep="\n", encoding="UTF-8")
splitter_index <- which(text_v == "#META#Header#End#")
lines_v <- text_v[(splitter_index+1):length(text_v)]
book_v <- paste(lines_v, collapse = "\n")
book_word_l <- str_split(book_v, "\\W+")
book_word_v <- unlist(book_word_l)
book_freqs_t <- table(book_word_v)
sorted_book_freqs_t <- sort(book_freqs_t, decreasing = TRUE)

## Exercises:

### Print 5th most common token: 
sorted_book_freqs_t[5]


### Print the 10 most common tokens and their frequencies: 
sorted_book_freqs_t[1:10]

###  Find the frequency of the token "الخليفة" in al-Ṭabari's Tarikh:
sorted_book_freqs_t["الخليفة"]

# Relative frequencies: 
all_tokens <- length(book_word_v)
all_tokens

all_token_freqs <- sum(sorted_book_freqs_t)
all_token_freqs
all_token_freqs == all_tokens

all_token_freqs <- sum(sorted_book_freqs_t)
rel_freq <- sorted_book_freqs_t[1] / all_token_freqs
rel_freq

## Exercises:

### 1. Find the relative frequency of the token "الخليفة" in al-Ṭabari's Tarikh:

all_token_freqs <- sum(sorted_book_freqs_t)
sorted_book_freqs_t["الخليفة"] / all_token_freqs

# Recycling

## setting up: 
all_token_freqs <- sum(sorted_book_freqs_t)
most_common <- sorted_book_freqs_t[1:10]
print("10 most common tokens")
most_common
class(most_common)

## calculating relative frequencies
rel_freq <- most_common / all_token_freqs
print("relative frequencies:")
rel_freq

## calculating percentages: 
all_token_freqs <- sum(sorted_book_freqs_t)
rel_freq <- sorted_book_freqs_t[1] / all_token_freqs
print("relative frequency:")
rel_freq
perc <- 100 * rel_freq
print("relative frequency (percentage):")
perc

## Exercise


### 1. Find out what happens when you multiply two vectors of different size, if the length of the longest one is not a multiple of the length of the shortest one:

a <- c(1, 2) 
b <- c(3, 4,5)
a * b

#### works but you get a warning message!

### 2. Find out whether recycling takes places with addition of vectors of different size as well: 
a <- c(1, 2) 
b <- c(3, 4,5)
a + b

#### works but you get a warning message!

### 3. Which percentage of al-Ṭabari's work consists of the twenty most common words?

most_common_20 <- sorted_book_freqs_t[1:10]
sum_most_common_20 <- sum(most_common_20)
word_count <- sum(sorted_book_freqs_t)
100 * sum_most_common_20 / word_count




