library(RecordLinkage)
library(hunspell)
library(tm)

#### Preparing word corpus to train the text cleaning model -----

# Reading big.txt to get all english words
raw_text <- paste(readLines("../Norvig Corpus - Big.txt/Norvig_big.txt"), collapse = " ")

# Substituting n't with nt to correct for words like couldn't, didn't
raw_text <- gsub("n't","nt",raw_text)

# Splitting the sentences into individual words
split_text <- strsplit(tolower(raw_text), "[^a-z]+")

# Calculating word occurrence frequency in big.txt
word_count <- table(split_text)
big_wc <- data.frame(word_count)

# Importing Practo specific words with artificial frequency to be added to big.txt
p_dump <- read.csv("../Practo_Words/Practo_Corpus(2).csv", stringsAsFactors = FALSE)
p_wc <- p_dump[,c("Name", "Freq")]
colnames(p_wc) <- c("split_text", "Freq")

# Merging the two word lists to make final training data-set
wc <- rbind(big_wc, p_wc)
colnames(wc) <- c("Words", "Frequency")
wc$Words <- tolower(wc$Words)
wc <- unique(wc)
sorted_words <- wc

# Arranging words in decreaseing order of their occurrence
sw <- sorted_words[order(-sorted_words$Frequency),]
sorted_words <- as.character(sw[,1]) #Preparing list of words from dataframe

#### Writing the model function to correct words in the text -----
correct_final <- function(word)
{
  # Calculating distance of word with each word from training set
  edit_dist <- levenshteinDist(word, sorted_words)
  # Taking all words where distance is favourable (maximum allowed = 2)
  proposals_by_prob <- c(sorted_words[edit_dist <= min(edit_dist, 2)])
  # Including "Not Found" flag for words unknown to training set
  proposals_by_prob <- c(proposals_by_prob, "Not Found")
  # Running Hunspell suggest function & picking the 1st word suggested on unknown words
  print(ifelse(proposals_by_prob[1] == "Not Found", (hunspell_suggest(word))[[1]][1], proposals_by_prob[1]))
}

##### Loading PIPEDRIVE Interaction File -----
dump <- read.csv("../Cosmic_Corpus/pipedrive_dump.csv")

# Notes
temp <- dump[c(1,2)]
s <- strsplit(tolower(temp$Note), "[^a-z]+")
# Converting list to dataframe after splitting sentences
Notes <- data.frame(Activity_id = rep(temp$Activity.ID, sapply(s, length)), Note = unlist(s))

# Converting to vector form
s <- as.character(Notes[['Note']])
all_words <- unlist(s,recursive=F)
# Taking only words where exact match is not found with training set
remaining <- setdiff(all_words,sorted_words)
correct <- data.frame(remaining)

# Applying the correct function on all such words -----
system.time(model <- sapply(remaining, correct_final))
correct[,2] <- model

#### Indexing the corrected words back to the initial set of interaction data -----
Notes$correct <- correct$V2[match(Notes$Note, correct$remaining)]
Notes$correct[is.na(Notes$correct)] <- as.character(Notes$Note)[is.na(Notes$correct)]
write.csv(dump,"../Pipedrive_Clean.csv")






