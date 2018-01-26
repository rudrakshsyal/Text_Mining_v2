# Reading Norvig's Raw_Text -----
raw_text <- paste(readLines("../../../../../../Desktop/Norvig_big.txt"), collapse = " ")
# Text processing function - from CMU Data Mining Course -----

raw_text <- gsub("n't","nt",raw_text)
split_text <- strsplit(tolower(raw_text), "[^a-z]+")
word_count <- table(split_text)

# Force-feeding the frequency of Practo Words -----
big_wc <- data.frame(word_count)
# reading the Practo Words and their frequency from external file
p_dump <- read.csv("../Practo_Words/Practo_Corpus(2).csv", stringsAsFactors = FALSE)
p_wc <- p_dump[,c("Name", "Freq")]
colnames(p_wc) <- c("split_text", "Freq")
# binding the original words with frequencies WITH force-feeded practo words with frequencies
wc <- rbind(big_wc, p_wc)
colnames(wc) <- c("Words", "Frequency")
wc$Words <- tolower(wc$Words)
wc <- unique(wc)

# Sorting the CORPUS in decreasing frequency of words -----
sorted_words <- wc
sw <- sorted_words[order(-sorted_words$Frequency),]
sorted_words <- as.character(sw[,1])

# write.csv(sorted_words, "../sw2.csv")

# Reading the TRAINING DUMP from Cosmic -----
dump <- read.csv("../Cosmic_Corpus/Intr_dump2.csv")
# trained <- read.csv("../Trained_Words.csv")

# Randomly taking a sample from the Dump and Training it from the Model -----
cluster <- dump[sample(nrow(dump), 900),]
cluster1 <- cluster[c(1:300),]
cluster2 <- cluster[c(301:600),]
cluster3 <- cluster[c(601:900),]
# split_cluster1 <- cluster1$id
# split_cluster2 <- strsplit(tolower(cluster1$AP...EN...SB...SP), "[^a-z]+")
# split_cluster <- data.frame(cbind(split_cluster1,split_cluster2))
# colnames(split_cluster) <- c("Interaction_ID", "Interaction")

##
split_cluster1 <- cluster1[c(1,6)]
colnames(split_cluster1) <- c("Interaction_ID", "Interaction")
s <- strsplit(tolower(split_cluster1$Interaction), "[^a-z]+")
temp1 <- data.frame(Interaction_ID = rep(split_cluster1$Interaction_ID, sapply(s, length)), Intr = unlist(s))
temp2 <- as.character(temp1[['Intr']])
##
split_cluster2 <- cluster2[c(1,6)]
colnames(split_cluster2) <- c("Interaction_ID", "Interaction")
s <- strsplit(tolower(split_cluster2$Interaction), "[^a-z]+")
temp3 <- data.frame(Interaction_ID = rep(split_cluster2$Interaction_ID, sapply(s, length)), Intr = unlist(s))
temp4 <- as.character(temp3[['Intr']])
##
split_cluster3 <- cluster3[c(1,6)]
colnames(split_cluster3) <- c("Interaction_ID", "Interaction")
s <- strsplit(tolower(split_cluster3$Interaction), "[^a-z]+")
temp5 <- data.frame(Interaction_ID = rep(split_cluster3$Interaction_ID, sapply(s, length)), Intr = unlist(s))
temp6 <- as.character(temp5[['Intr']])
# temp2 <- temp[,2]

####
system.time(model1 <- sapply(temp2, correct0))
system.time(model2 <- sapply(temp2, correct1))
system.time(model3 <- sapply(temp2, correct2))

temp1[,3] <- model1
temp1[,4] <- model2
temp1[,5] <- model3

# if(temp1[,2] != temp1[,3]) {
#   temp <- temp1[,2]
# }
# system.time(model2 <- sapply(temp2, HSpell))
# temp2 <- as.character(temp1[['Intr']])

# system.time(model2 <- sapply(temp2, HSpell))
# temp1[,4] <- model2
colnames(temp1) <- c("Intr_ID", "Input", "Model0", "Model1", "Model2")
##
system.time(model4 <- sapply(temp4, correct0))
system.time(model5 <- sapply(temp4, correct1))
system.time(model6 <- sapply(temp4, correct2))

temp3[,3] <- model4
temp3[,4] <- model5
temp3[,5] <- model6

# system.time(model4 <- sapply(temp4, HSpell))
# temp3[,4] <- model4
colnames(temp3) <- c("Intr_ID", "Input", "Model0", "Model1", "Model2")
##
system.time(model7 <- sapply(temp6, correct0))
system.time(model8 <- sapply(temp6, correct1))
system.time(model9 <- sapply(temp6, correct2))

temp5[,3] <- model7
temp5[,4] <- model8
temp5[,5] <- model9

# system.time(model6 <- sapply(temp6, HSpell))
# temp5[,4] <- model6
colnames(temp5) <- c("Intr_ID", "Input", "Model0", "Model1", "Model2" )
##
write.csv(temp1, "../v12 - Final Model/Fdf4.csv")
write.csv(temp3, "../v12 - Final Model/Fdf5.csv")
write.csv(temp5, "../v12 - Final Model/Fdf6.csv")
####


####
temp <- data.frame(Input = c(""), Output = c(""), stringsAsFactors = F)
i=j=k=1
for(i in 1:(nrow(trained)))
{
  line <- trained[i,1]
  for(j in 1:length(line[[1]]))
  {
    word <- line[[1]][j]
    temp[k,1] <- as.character(trained[i,1])
    # df[k,2] <- word
    temp[k,2] <- correct(word)
    # df[k,4] <- correct1(word)
    # df[k,5] <- correct2(word)
    # df[k,6] <- correct3(word)
    # df[k,7] <- (hunspell_suggest(word))[[1]][1]
    k <- k+1
    # if(k%%100 == 0)
    {print(paste0(i," - ",k))}
    print(q)
  }
}

# Zipf's law states that given some corpus of natural language utterances, the frequency of any word is inversely proportional to its rank in the frequency table. Thus the most frequent word will occur approximately twice as often as the second most frequent word, three times as often as the third most frequent word, etc.: the rank-frequency distribution is an inverse relation. For example, in the Brown Corpus of American English text, the word "the" is the most frequently occurring word, and by itself accounts for nearly 7% of all word occurrences (69,971 out of slightly over 1 million). True to Zipf's Law, the second-place word "of" accounts for slightly over 3.5% of words (36,411 occurrences), followed by "and" (28,852). Only 135 vocabulary items are needed to account for half the Brown Corpus.[4]