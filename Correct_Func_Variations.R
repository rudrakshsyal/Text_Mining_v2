library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
library(hunspell)
library(RecordLinkage)
clusterEvalQ(cl, library(RecordLinkage))
clusterEvalQ(cl, library(hunspell))
clusterExport(cl, "sorted_words")

system.time(model1 <- sapply(temp2, correct_final))
temp1[,3] <- unlist(model1)


system.time(model7 <- parLapply(cl, temp6, correct_final))
model7 <- unlist(model7)
temp5[,3] <- model7
stopCluster(cl)

correct_final <- function(word) {
  edit_dist <- levenshteinDist(word, sorted_words)
  proposals_by_prob <- c(sorted_words[edit_dist <= min(edit_dist, 2)])
  proposals_by_prob <- c(proposals_by_prob, "Not Found")
  print(ifelse(proposals_by_prob[1] == "Not Found", (hunspell_suggest(word))[[1]][1], proposals_by_prob[1]))
}

correct_final("htink")

correct_final("praco")
system.time(correct_final("praco"))
correct_final("presumtpousne")
system.time(correct_final("presumtpousne"))

q <- list("a", "b", "c", "d", "e")
match("c",q)

correct_final1 <- function(word) {
  edit_dist <- levenshteinDist(word, sorted_words)
  proposals_by_prob <- c(sorted_words[edit_dist <= min(edit_dist, 0)])
  proposals_by_prob <- c(proposals_by_prob, "UNKNOWN")
  proposals_by_prob[1]
  print(match(word, remaining))
}



correct_final2 <- function(word) {
  edit_dist <- levenshteinDist(word, sorted_words)
  proposals_by_prob <- c(sorted_words[edit_dist <= min(edit_dist, 2)])
  proposals_by_prob <- c(proposals_by_prob, "UNKNOWN")
  proposals_by_prob[1]
}
correct_final3 <- function(word) {
  
  (hunspell_suggest(word))[[1]][1]
}

system.time(correct_final1("wek"))
system.time(correct_final2("wek"))
system.time(correct_final3("wek"))
# 
# correct1 <- function(word) {
#   edit_dist <- levenshteinDist(word, sorted_words)
#   proposals_by_prob <- c(sorted_words[edit_dist <= 1 & edit_dist > 0])
#   proposals_by_prob <- c(proposals_by_prob, "UNKNOWN1")
#   proposals_by_prob[1]
# }
# 
# correct1("wednesday")
# 
# correct2 <- function(word) {
#   edit_dist <- levenshteinDist(word, sorted_words)
#   proposals_by_prob <- c(sorted_words[edit_dist <= 2 & edit_dist > 0])
#   proposals_by_prob <- c(proposals_by_prob, "UNKNOWN1")
#   proposals_by_prob[1]
# }
# 
# correct3 <- function(word) {
#   edit_dist <- levenshteinDist(word, sorted_words)
#   proposals_by_prob <- c(sorted_words[edit_dist <= 3 & edit_dist > 0])
#   proposals_by_prob <- c(proposals_by_prob, "UNKNOWN1")
#   proposals_by_prob[1]
# }
# 
# correct2 <- function(word) {
#   edit_dist <- levenshteinDist(word, sorted_words)
#   proposals_by_prob <- c(sorted_words[edit_dist <= min(edit_dist, 2)])
#   proposals_by_prob <- c(proposals_by_prob, "UNKNOWN2")
#   proposals_by_prob[1]
# }
# 
# correct2("softwrea")
# 
# correct3 <- function(word) {
#   edit_dist <- levenshteinDist(word, sorted_words)
#   proposals_by_prob <- c(sorted_words[edit_dist <= min(edit_dist, 3)])
#   proposals_by_prob <- c(proposals_by_prob, "UNKNOWN3")
#   proposals_by_prob[1]
# }
# 
# correct3("softwrea")
# # 
# # correct4 <- function(word) {
# #   edit_dist <- adist(word, sorted_words)
# #   proposals_by_prob <- c(sorted_words[edit_dist <= min(edit_dist, 4) & edit_dist > 3])
# #   proposals_by_prob <- c(proposals_by_prob, "UNKNOWN4")
# #   proposals_by_prob[1]
# # }
# # 
# # correct4("rajashree")
# # 
# 
# # FUNCTION Correct with probability distribution for ED <= 2 Words -----
# correct <- function(word) {
#   # Calculate the edit distance between the word and all other words in sorted_words.
#   edit_dist <- adist(word, sorted_words)
#   # Calculate the minimum edit distance to find a word that exists in big.txt 
#   # with a limit of two edits.
#   min_edit_dist <- min(edit_dist, 2)
#   # Generate a vector with all words with this minimum edit distance.
#   # Since sorted_words is ordered from most common to least common, the resulting
#   # vector will have the most common / probable match first.
#   proposals_by_prob <- c(sorted_words[ edit_dist <= min(edit_dist, 2)])
#   # In case proposals_by_prob would be empty we append the word to be corrected...
#   proposals_by_prob <- c(proposals_by_prob, "UNKNOWN")
#   # ... and return the first / most probable word in the vector.
#   proposals_by_prob[1]
# }
# 
# # FUNCTION Correct without probability distribution for ED = 0 Words -----
# correct <- function(word) {
#   edit_dist <- adist(word, sorted_words)
#   if(edit_dist > 0){
#     proposals_by_prob <- c(sorted_words[edit_dist <= min(edit_dist, 2)])
#     proposals_by_prob <- c(proposals_by_prob, "UN-KNOWN")
#     proposals_by_prob[1]
#   } else{
#     word
#   }
# }
# 
# HSpell <- function(word) {
#   
#   (hunspell_suggest(word))[[1]][1]
# }
# HSpell("wek")
# 
# 
