# Chapter 5 exercises solution:

## Regular expressions exercises:

### a. Load a new text from OpenITI

url <- "https://raw.githubusercontent.com/OpenITI/0350AH/master/data/0346Istakhri/0346Istakhri.MasalikWaMamalik/0346Istakhri.MasalikWaMamalik.Shamela0011680-ara1.mARkdown"
text_lines_v <- read_lines(url)
splitter_pos <- which(text_lines_v == "#META#Header#End#")
body_lines_v <- text_lines_v[(splitter_pos + 1):length(text_lines_v)]
body_lines_v[150]
book_v <- paste(body_lines_v, collapse = "\n")
book_word_l <- str_split(book_v, "\\W+")
book_word_v <- unlist(book_word_l)

### b. regular expressions exercise 1

### Use a regular expression to find all the lines in the text that contain the 
### word "درهم" (dirham, "silver coin") or its plural "دراهم" (darāhim)

ptrn <- "درا?هم"
dirham_v <- str_detect(book_word_v, ptrn)

# how many times is the word used? 

length(which(dirham_v == TRUE))

# visualize with a dispersion plot where in the text dirham is mentioned: 

plot(dirham_v, type = "h")

### c. regular expressions exercise 2

### Use a regular expression to find all the lines in the text that contain the word "إلى", 
### "to(wards)" (note that the last letter of ilā changes into a yā' ("ي")
### if it is followed by a suffix; we want to include those cases as well)

ptrn <- "إلى|إليه"
ila_v <- str_detect(book_word_v, ptrn)

# how many times is the word used? 

length(which(ila_v == TRUE))

# visualize with a dispersion plot where in the text dirham is mentioned: 

plot(ila_v, type = "h")


### c. regular expressions exercise 2

### Use a regular expression to find all forms derived from the root /rḥl/
### (to travel) in the text. 

ptrn <- "\\w*رت?حل\\w*"
rhl_v <- str_detect(book_word_v, ptrn)

# how many times is the word used? 

length(which(rhl_v == TRUE))

# visualize with a dispersion plot where in the text dirham is mentioned: 

plot(rhl_v, type = "h")

# create a frequency table of the results: 
sort(table(book_word_v[rhl_v]), decreasing = TRUE)




## Loops

### basic loop exercise: 

### Create a vector that contains at least 4 numbers, and print for every 
### number in that vector its index position in the vector, and its value. 

number_v <- c(13543, 1852, 1234, 65432)
for (i in 1:length(number_v)) {
  message("Index number ", i, ": ", number_v[i])
}

### basic loop exercise 2:

### Create a vector `source_v` that contains at least 4 character strings. 
### Create a loop that prints the number of characters in each character string 
### and stores those counts in a new vector called `character_counts_v`.

### You can use the `nchar()` function to count the number of characters in a string. 
### (Ask shaykh Google or the R help function how to use `nchar()`)


source_v <- c("test", "longer", "this is a sentence", "where is this going?")
character_counts_v <- c()
for (i in 1:length(source_v)) {
  message("Index number ", i, " (", source_v[i], "): ", nchar(source_v[i]), " characters")
  character_counts_v <- c(character_counts_v, nchar(source_v[i]))
}
character_counts_v

### basic loop exercise 3: 

### Create a vector `source_v` that contains at least 4 character strings. 
### Use a "for loop" and an "if...else condition" to store the
### strings shorter than 5 characters in a vector `short_v`, and the others in
### a vector called `long_v`. 

source_v <- c("test", "longer", "this is a sentence", "where is this going?")
short_v <- c()
long_v <- c()
for (i in 1:length(source_v)) {
  message("Index number ", i, " (", source_v[i], "): ", nchar(source_v[i]), " characters")
  if (nchar(source_v[i]) < 5) {
    short_v <- c(short_v, source_v[i])
  }
  else {
    long_v <- append(long_v, source_v[i])
  }
}
short_v
long_v

## More advanced regular expressions exercise 3: use the `str_extract_all()` function to find words in context


## The `str_extract_all()` function creates a character vector that contains
## all matches for a search pattern in a given character vector. 

library(stringr)
ex <- c("I love apples", "I like pears", "I hate bananas", "no bananas!")

# find all words in the vector that contain the letter "e"
result_l <- str_extract_all(ex, "\\w*e\\w*")
str(result_l)
# turn the list of results into a character vector:
result_v <- unlist(result_l)
str(result_v)

## What does the output of the `str()` function tell you about the variables `result_l` 
## and `result_v`?

## >>> list contains the results of each string in c separately + includes chr(0) for non-matches, 
## result_v contains only matches and puts them on one level
  

##  Use this same technique to extract all mentions of the word "فارس" from the text. 


library(stringr)
result_l <- str_extract_all(body_lines_v, "فارس")
str(unlist(result_l))

## This is not very informative: you get only a sequence of repetitions of the word Fārs. 

## Would it not be great if we could print the context of the word as well? Say, 
## 20 characters before and 20 characters after the search word? 
  
##  Write a regular expression that will match the word Fārs + 20 characters before
## it and 20 characters after it. Then use a loop to print each of the search results
## in its context. 

## NB: do not use the vector that contains the text split into lines for this, 
## but first collapse the lines into one character string!


book_v <- paste(body_lines_v, collapse = " ")
nchar(book_v[1])
ptrn <- ".{20}فارس.{20}"
result_l <- str_extract_all(book_v, ptrn)
result_v <- unlist(result_l)
for (i in 1:length(result_v)) {
  print(result_v[i])
}


## More advanced loop exercise: 

## OpenITI texts contain milestone markers after every 300 Arabic tokens; they look like this: 
## `ms001` (after the first 300 Arabic tokens), `ms002` (after the second 300 Arabic tokens),
## etc.

## Use Jockers' method from chapter 5 to divide the text not into chapters but 
## into 300-word chunks, and plot how often the words "منبر" (pulpit, for mosque) 
## and مدينة (city) are mentioned in each chunk. 


milestones <- grep("ms\\d+", book_word_v)
minbar_counts_v <- c()
madina_counts_v <- c()
for (i in 1:length(milestones)) {
  chunk_end <- milestones[i]
  if (i < 2) {
    chunk_start = 1
  }
  else {
    chunk_start <- milestones[i-1]
  }
  
  ms_text <- book_word_v[chunk_start:chunk_end]
  
  ptrn <- "منبر"
  minbar_count <- sum(str_count(ms_text, ptrn))
  print(minbar_count)
  minbar_counts_v <- c(minbar_counts_v, minbar_count)
  ptrn <- "مدينة"
  madina_count <- sum(str_count(ms_text, ptrn))
  print(madina_count)
  madina_counts_v <- c(madina_counts_v, madina_count)
}
plot(madina_counts_v, type="h")
plot(minbar_counts_v, type="h")

### Ask shaykh google how to save a plot created by the plot() function as an image file. 
### When you found the solution, create dispersion plots for 4 files using 
### a loop and save each of them to a file.