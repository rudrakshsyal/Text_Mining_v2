C_Sentiment <- read.csv("../Final_532 - Reborn - 2.csv", stringsAsFactors = F)

C_Sentiment$Score[C_Sentiment$Sentiment == "Positive"] <- 100
C_Sentiment$Score[C_Sentiment$Sentiment == "Negative"] <- -100

C_Sentiment_Positive <- subset(C_Sentiment, Sentiment == "Positive")
C_Sentiment_Negative <- subset(C_Sentiment, Sentiment == "Negative")

############################################################

SBT <- read.csv("../../../../../../FINAL_SBT.csv", stringsAsFactors = F)
SBT$X <- SBT$word <- SBT$quad_gram <- NULL

SBT$tri_score_SBT <- C_Sentiment$Score[match(SBT$tri_gram, C_Sentiment$Words)]
SBT[is.na(SBT)] <- 0

SBT$tri_gram[2:length(SBT$tri_gram)][SBT$tri_score_SBT != 0] <- SBT$tri_gram[1:length(SBT$tri_gram)][SBT$tri_score_SBT != 0]
SBT$tri_gram[3:length(SBT$tri_gram)][SBT$tri_score_SBT != 0] <- SBT$tri_gram[1:length(SBT$tri_gram)][SBT$tri_score_SBT != 0]

SBT$concat <- paste0(SBT$interaction_id, SBT$tri_gram)
SBT <- SBT[!duplicated(SBT$concat),]
SBT$concat <- NULL

# SBT$tri_score_SBT <- C_Sentiment$Score[match(SBT$tri_gram, C_Sentiment$Words)]
# SBT[is.na(SBT)] <- 0
# for(i in 1:length(SBT$correct)){
#   SBT$tri_score_SBT[i][SBT$tri_score_SBT[i] == SBT$tri_score_SBT[i-1] | SBT$tri_score_SBT[i] - SBT$tri_score_SBT[i-1] == 200 | SBT$tri_score_SBT[i] - SBT$tri_score_SBT[i-1] == -200] <- 0
#   SBT$tri_score_SBT[i][SBT$tri_score_SBT[i] == SBT$tri_score_SBT[i-2] | SBT$tri_score_SBT[i] - SBT$tri_score_SBT[i-2] == 200 | SBT$tri_score_SBT[i] - SBT$tri_score_SBT[i-2] == -200] <- 0
#   print(i)
# }

SBT$bi_score_SBT[SBT$tri_score_SBT == 0] <- C_Sentiment$Score[match(SBT$bi_gram, C_Sentiment$Words)][SBT$tri_score_SBT == 0]
SBT[is.na(SBT)] <- 0

SBT$bi_gram[2:length(SBT$bi_gram)][SBT$bi_score_SBT != 0] <- SBT$bi_gram[1:length(SBT$bi_gram)][SBT$bi_score_SBT != 0]

SBT$concat <- paste0(SBT$interaction_id, SBT$bi_gram)
SBT <- SBT[!duplicated(SBT$concat),]
SBT$concat <- NULL

# SBT$bi_score_SBT[SBT$tri_score_SBT == 0 & SBT$bi_score_SBT != 0] <- C_Sentiment$Score[match(SBT$bi_gram, C_Sentiment$Words)][SBT$tri_score_SBT == 0 & SBT$bi_score_SBT != 0]
# SBT[is.na(SBT)] <- 0

# SBT$bi_score_SBT[SBT$tri_score_SBT == 0] <- C_Sentiment$Score[match(SBT$bi_gram, C_Sentiment$Words)][SBT$tri_score_SBT == 0]
# SBT[is.na(SBT)] <- 0

# for(i in 1:length(SBT$correct)){
#   SBT$bi_score_SBT[i][SBT$bi_score_SBT[i] == SBT$bi_score_SBT[i-1] | SBT$bi_score_SBT[i] - SBT$bi_score_SBT[i-1] == 200 | SBT$bi_score_SBT[i] - SBT$bi_score_SBT[i-1] == -200] <- 0
#   print(i)
# }
SBT$uni_score_SBT[SBT$bi_score_SBT == 0 & SBT$tri_score_SBT == 0] <- C_Sentiment$Score[match(SBT$correct, C_Sentiment$Words)][SBT$bi_score_SBT == 0 & SBT$tri_score_SBT == 0]
SBT[is.na(SBT)] <- 0

SBT$sum <- SBT$uni_score_SBT + SBT$bi_score_SBT + SBT$tri_score_SBT
SBT_sentiment <- subset(SBT, SBT$sum != 0)
SBT_sentiment$tri_score_SBT[SBT_sentiment$tri_score_SBT != 0] <- SBT_sentiment$tri_gram[SBT_sentiment$tri_score_SBT != 0]
SBT_sentiment$bi_score_SBT[SBT_sentiment$bi_score_SBT != 0] <- SBT_sentiment$bi_gram[SBT_sentiment$bi_score_SBT != 0]
SBT_sentiment$uni_score_SBT[SBT_sentiment$uni_score_SBT != 0] <- SBT_sentiment$correct[SBT_sentiment$uni_score_SBT != 0]
SBT_sentiment$correct <- SBT_sentiment$bi_gram <- SBT_sentiment$tri_gram <- NULL
SBT_sentiment$uni_score_concat[SBT_sentiment$uni_score_SBT != 0] <- paste0(SBT_sentiment$uni_score_SBT, " (", SBT_sentiment$sum, ")")[SBT_sentiment$uni_score_SBT != 0]
SBT_sentiment$bi_score_concat[SBT_sentiment$bi_score_SBT != 0] <- paste0(SBT_sentiment$bi_score_SBT, " (", SBT_sentiment$sum, ")")[SBT_sentiment$bi_score_SBT != 0]
SBT_sentiment$tri_score_concat[SBT_sentiment$tri_score_SBT != 0] <- paste0(SBT_sentiment$tri_score_SBT, " (", SBT_sentiment$sum, ")")[SBT_sentiment$tri_score_SBT != 0]
SBT_sentiment[is.na(SBT_sentiment)] <- 0
SBT_sentiment$concat <- paste0(SBT_sentiment$tri_score_concat, " - ", SBT_sentiment$bi_score_concat, " - ", SBT_sentiment$uni_score_concat)
SBT_sentiment$tri_score_concat <- SBT_sentiment$bi_score_concat <- SBT_sentiment$uni_score_concat <- SBT_sentiment$sum <- NULL

temp <- count(SBT_sentiment$interaction_id)

SBT_sentiment$concat2[1:length(SBT_sentiment$concat)] <- SBT_sentiment$concat[2:length(SBT_sentiment$concat)]
SBT_sentiment$concat3[1:length(SBT_sentiment$concat)] <- SBT_sentiment$concat[3:length(SBT_sentiment$concat)]
SBT_sentiment$concat4[1:length(SBT_sentiment$concat)] <- SBT_sentiment$concat[4:length(SBT_sentiment$concat)]
SBT_sentiment$concat5[1:length(SBT_sentiment$concat)] <- SBT_sentiment$concat[5:length(SBT_sentiment$concat)]
SBT_sentiment$concat6[1:length(SBT_sentiment$concat)] <- SBT_sentiment$concat[6:length(SBT_sentiment$concat)]
SBT_sentiment$concat7[1:length(SBT_sentiment$concat)] <- SBT_sentiment$concat[7:length(SBT_sentiment$concat)]
SBT_sentiment$concat8[1:length(SBT_sentiment$concat)] <- SBT_sentiment$concat[8:length(SBT_sentiment$concat)]
SBT_sentiment$concat9[1:length(SBT_sentiment$concat)] <- SBT_sentiment$concat[9:length(SBT_sentiment$concat)]

SBT_sentiment$freq <- temp$freq[match(SBT_sentiment$interaction_id, temp$x)]

SBT_sentiment <- SBT_sentiment[!duplicated(SBT_sentiment$interaction_id, SBT_sentiment$freq),]

SBT_sentiment$result <- paste0(SBT_sentiment$concat,SBT_sentiment$concat2,SBT_sentiment$concat3,SBT_sentiment$concat4,SBT_sentiment$concat5,SBT_sentiment$concat6,SBT_sentiment$concat7,SBT_sentiment$concat8,SBT_sentiment$concat9) 

SBT_sentiment$concat9 <- SBT_sentiment$concat8 <- SBT_sentiment$concat7 <- SBT_sentiment$concat6 <- SBT_sentiment$concat5 <- SBT_sentiment$concat4 <- SBT_sentiment$concat3 <- SBT_sentiment$concat2 <- SBT_sentiment$concat <- SBT_sentiment$tri_score_SBT <- SBT_sentiment$bi_score_SBT <- SBT_sentiment$uni_score_SBT <- NULL

# SBT_sentiment$result[1:length(SBT_sentiment$interaction_id) == 2:length(SBT_sentiment$interaction_id)] <- paste0(SBT_sentiment$concat, " & ", SBT_sentiment$concat2)[1:length(SBT_sentiment$interaction_id) == 2:length(SBT_sentiment$interaction_id)]

write.csv(SBT,"../SBT_Sentiment.csv")

Intr_Score_SBT <- aggregate(uni_score_SBT~interaction_id, data = SBT, FUN = sum)
temp1 <- aggregate(bi_score_SBT~interaction_id, data = SBT, FUN = sum)
temp2 <- aggregate(tri_score_SBT~interaction_id, data = SBT, FUN = sum)

Intr_Score_SBT$bi_score_SBT <- temp1$`bi_score_SBT`[match(Intr_Score_SBT$`interaction_id`, temp1$`interaction_id`)]
Intr_Score_SBT$tri_score_SBT <- temp2$`tri_score_SBT`[match(Intr_Score_SBT$`interaction_id`, temp2$`interaction_id`)]

colnames(Intr_Score_SBT) <- c("ID", "Uni-grams_SBT", "Bi-grams_SBT", "Tri-grams_SBT")
Intr_Score_SBT$Sentiment_SBT[(Intr_Score_SBT$`Uni-grams` + Intr_Score_SBT$`Bi-grams` + Intr_Score_SBT$`Tri-grams`) < 0] <- "Negative"
Intr_Score_SBT$Sentiment_SBT[(Intr_Score_SBT$`Uni-grams` + Intr_Score_SBT$`Bi-grams` + Intr_Score_SBT$`Tri-grams`) > 0] <- "Positive"
Intr_Score_SBT[is.na(Intr_Score_SBT)] <- "Neutral"
Intr_Score_SBT$Emotion <- Intr_Score_SBT$`Uni-grams_SBT` + Intr_Score_SBT$`Bi-grams_SBT` + Intr_Score_SBT$`Tri-grams_SBT`

############################################################

AP <- read.csv("../../../../../../FINAL_AP.csv", stringsAsFactors = F)
AP$X <- AP$word <- AP$quad_gram <- NULL

AP$tri_score_AP <- C_Sentiment$Score[match(AP$tri_gram, C_Sentiment$Words)]
AP[is.na(AP)] <- 0

AP$tri_gram[2:length(AP$tri_gram)][AP$tri_score_AP != 0] <- AP$tri_gram[1:length(AP$tri_gram)][AP$tri_score_AP != 0]
AP$tri_gram[3:length(AP$tri_gram)][AP$tri_score_AP != 0] <- AP$tri_gram[2:length(AP$tri_gram)][AP$tri_score_AP != 0]

AP$concat <- paste0(AP$interaction_id, AP$tri_gram)
AP <- AP[!duplicated(AP$concat),]
AP$concat <- NULL

# AP$tri_score_AP <- C_Sentiment$Score[match(AP$tri_gram, C_Sentiment$Words)]
# AP[is.na(AP)] <- 0
# for(i in 1:length(AP$correct)){
#   AP$tri_score_AP[i][AP$tri_score_AP[i] == AP$tri_score_AP[i-1] | AP$tri_score_AP[i] - AP$tri_score_AP[i-1] == 200 | AP$tri_score_AP[i] - AP$tri_score_AP[i-1] == -200] <- 0
#   AP$tri_score_AP[i][AP$tri_score_AP[i] == AP$tri_score_AP[i-2] | AP$tri_score_AP[i] - AP$tri_score_AP[i-2] == 200 | AP$tri_score_AP[i] - AP$tri_score_AP[i-2] == -200] <- 0
#   print(i)
# }
AP$bi_score_AP[AP$tri_score_AP == 0] <- C_Sentiment$Score[match(AP$bi_gram, C_Sentiment$Words)][AP$tri_score_AP == 0]
AP[is.na(AP)] <- 0

AP$bi_gram[2:length(AP$bi_gram)][AP$bi_score_AP != 0] <- AP$bi_gram[1:length(AP$bi_gram)][AP$bi_score_AP != 0]

AP$concat <- paste0(AP$interaction_id, AP$bi_gram)
AP <- AP[!duplicated(AP$concat),]
AP$concat <- NULL

# AP$bi_score_AP[AP$tri_score_AP == 0] <- C_Sentiment$Score[match(AP$bi_gram, C_Sentiment$Words)][AP$tri_score_AP == 0]
# AP[is.na(AP)] <- 0
# for(i in 1:length(AP$correct)){
#   AP$tri_score_AP[i][AP$tri_score_AP[i] == AP$tri_score_AP[i-1] | AP$tri_score_AP[i] - AP$tri_score_AP[i-1] == 200 | AP$tri_score_AP[i] - AP$tri_score_AP[i-1] == -200] <- 0
#   print(i)
# }
AP$uni_score_AP[AP$bi_score_AP == 0 & AP$tri_score_AP == 0] <- C_Sentiment$Score[match(AP$correct, C_Sentiment$Words)][AP$bi_score_AP == 0 & AP$tri_score_AP == 0]
AP[is.na(AP)] <- 0

AP$sum <- AP$uni_score_AP + AP$bi_score_AP + AP$tri_score_AP
AP_sentiment <- subset(AP, AP$sum != 0)
AP_sentiment$tri_score_AP[AP_sentiment$tri_score_AP != 0] <- AP_sentiment$tri_gram[AP_sentiment$tri_score_AP != 0]
AP_sentiment$bi_score_AP[AP_sentiment$bi_score_AP != 0] <- AP_sentiment$bi_gram[AP_sentiment$bi_score_AP != 0]
AP_sentiment$uni_score_AP[AP_sentiment$uni_score_AP != 0] <- AP_sentiment$correct[AP_sentiment$uni_score_AP != 0]
AP_sentiment$correct <- AP_sentiment$bi_gram <- AP_sentiment$tri_gram <- NULL
AP_sentiment$uni_score_concat[AP_sentiment$uni_score_AP != 0] <- paste0(AP_sentiment$uni_score_AP, " (", AP_sentiment$sum, ")")[AP_sentiment$uni_score_AP != 0]
AP_sentiment$bi_score_concat[AP_sentiment$bi_score_AP != 0] <- paste0(AP_sentiment$bi_score_AP, " (", AP_sentiment$sum, ")")[AP_sentiment$bi_score_AP != 0]
AP_sentiment$tri_score_concat[AP_sentiment$tri_score_AP != 0] <- paste0(AP_sentiment$tri_score_AP, " (", AP_sentiment$sum, ")")[AP_sentiment$tri_score_AP != 0]
AP_sentiment[is.na(AP_sentiment)] <- 0
AP_sentiment$concat <- paste0(AP_sentiment$tri_score_concat, " - ", AP_sentiment$bi_score_concat, " - ", AP_sentiment$uni_score_concat)
AP_sentiment$tri_score_concat <- AP_sentiment$bi_score_concat <- AP_sentiment$uni_score_concat <- AP_sentiment$sum <- NULL

temp <- count(AP_sentiment$interaction_id)

AP_sentiment$concat2[1:length(AP_sentiment$concat)] <- AP_sentiment$concat[2:length(AP_sentiment$concat)]
AP_sentiment$concat3[1:length(AP_sentiment$concat)] <- AP_sentiment$concat[3:length(AP_sentiment$concat)]
AP_sentiment$concat4[1:length(AP_sentiment$concat)] <- AP_sentiment$concat[4:length(AP_sentiment$concat)]
AP_sentiment$concat5[1:length(AP_sentiment$concat)] <- AP_sentiment$concat[5:length(AP_sentiment$concat)]
AP_sentiment$concat6[1:length(AP_sentiment$concat)] <- AP_sentiment$concat[6:length(AP_sentiment$concat)]
AP_sentiment$concat7[1:length(AP_sentiment$concat)] <- AP_sentiment$concat[7:length(AP_sentiment$concat)]
AP_sentiment$concat8[1:length(AP_sentiment$concat)] <- AP_sentiment$concat[8:length(AP_sentiment$concat)]
AP_sentiment$concat9[1:length(AP_sentiment$concat)] <- AP_sentiment$concat[9:length(AP_sentiment$concat)]

AP_sentiment$freq <- temp$freq[match(AP_sentiment$interaction_id, temp$x)]

AP_sentiment <- AP_sentiment[!duplicated(AP_sentiment$interaction_id, AP_sentiment$freq),]

AP_sentiment$result <- paste0(AP_sentiment$concat,AP_sentiment$concat2,AP_sentiment$concat3,AP_sentiment$concat4,AP_sentiment$concat5,AP_sentiment$concat6,AP_sentiment$concat7,AP_sentiment$concat8,AP_sentiment$concat9) 

AP_sentiment$concat9 <- AP_sentiment$concat8 <- AP_sentiment$concat7 <- AP_sentiment$concat6 <- AP_sentiment$concat5 <- AP_sentiment$concat4 <- AP_sentiment$concat3 <- AP_sentiment$concat2 <- AP_sentiment$concat <- AP_sentiment$tri_score_AP <- AP_sentiment$bi_score_AP <- AP_sentiment$uni_score_AP <- NULL

##

Intr_Score_AP <- aggregate(uni_score_AP~interaction_id, data = AP, FUN = sum)
temp1 <- aggregate(bi_score_AP~interaction_id, data = AP, FUN = sum)
temp2 <- aggregate(tri_score_AP~interaction_id, data = AP, FUN = sum)

Intr_Score_AP$bi_score_AP <- temp1$`bi_score_AP`[match(Intr_Score_AP$`interaction_id`, temp1$`interaction_id`)]
Intr_Score_AP$tri_score_AP <- temp2$`tri_score_AP`[match(Intr_Score_AP$`interaction_id`, temp2$`interaction_id`)]

colnames(Intr_Score_AP) <- c("ID", "Uni-grams_AP", "Bi-grams_AP", "Tri-grams_AP")
Intr_Score_AP$Sentiment_AP[(Intr_Score_AP$`Uni-grams` + Intr_Score_AP$`Bi-grams` + Intr_Score_AP$`Tri-grams`) < 0] <- "Negative"
Intr_Score_AP$Sentiment_AP[(Intr_Score_AP$`Uni-grams` + Intr_Score_AP$`Bi-grams` + Intr_Score_AP$`Tri-grams`) > 0] <- "Positive"
Intr_Score_AP[is.na(Intr_Score_AP)] <- "Neutral"
Intr_Score_AP$Emotion <- Intr_Score_AP$`Uni-grams_AP` + Intr_Score_AP$`Bi-grams_AP` + Intr_Score_AP$`Tri-grams_AP`

############################################################

SP <- read.csv("../../../../../../FINAL_SP.csv", stringsAsFactors = F)
SP$X <- SP$word <- SP$quad_gram <- NULL

SP$tri_score_SP <- C_Sentiment$Score[match(SP$tri_gram, C_Sentiment$Words)]
SP[is.na(SP)] <- 0

SP$tri_gram[2:length(SP$tri_gram)][SP$tri_score_SP != 0] <- SP$tri_gram[1:length(SP$tri_gram)][SP$tri_score_SP != 0]
SP$tri_gram[3:length(SP$tri_gram)][SP$tri_score_SP != 0] <- SP$tri_gram[2:length(SP$tri_gram)][SP$tri_score_SP != 0]

SP$concat <- paste0(SP$interaction_id, SP$tri_gram)
SP <- SP[!duplicated(SP$concat),]
SP$concat <- NULL

# SP$tri_score_SP <- C_Sentiment$Score[match(SP$tri_gram, C_Sentiment$Words)]
# SP[is.na(SP)] <- 0
# for(i in 1:length(SP$correct)){
#   SP$tri_score_SP[i][SP$tri_score_SP[i] == SP$tri_score_SP[i-1] | SP$tri_score_SP[i] - SP$tri_score_SP[i-1] == 200 | SP$tri_score_SP[i] - SP$tri_score_SP[i-1] == -200] <- 0
#   SP$tri_score_SP[i][SP$tri_score_SP[i] == SP$tri_score_SP[i-2] | SP$tri_score_SP[i] - SP$tri_score_SP[i-2] == 200 | SP$tri_score_SP[i] - SP$tri_score_SP[i-2] == -200] <- 0
#   print(i)
# }
SP$bi_score_SP[SP$tri_score_SP == 0] <- C_Sentiment$Score[match(SP$bi_gram, C_Sentiment$Words)][SP$tri_score_SP == 0]
SP[is.na(SP)] <- 0

SP$concat <- paste0(SP$interaction_id, SP$bi_gram)
SP <- SP[!duplicated(SP$concat),]
SP$concat <- NULL

# SP$bi_gram[2:length(SP$bi_gram)][SP$bi_score_SP != 0] <- SP$bi_gram[1:length(SP$bi_gram)][SP$bi_score_SP != 0]

# SP$bi_score_SP[SP$tri_score_SP == 0] <- C_Sentiment$Score[match(SP$bi_gram, C_Sentiment$Words)][SP$tri_score_SP == 0]
# SP[is.na(SP)] <- 0
# for(i in 1:length(SP$correct)){
#   SP$tri_score_SP[i][SP$tri_score_SP[i] == SP$tri_score_SP[i-1] | SP$tri_score_SP[i] - SP$tri_score_SP[i-1] == 200 | SP$tri_score_SP[i] - SP$tri_score_SP[i-1] == -200] <- 0
#   print(i)
# }
SP$uni_score_SP[SP$bi_score_SP == 0 & SP$tri_score_SP == 0] <- C_Sentiment$Score[match(SP$correct, C_Sentiment$Words)][SP$bi_score_SP == 0 & SP$tri_score_SP == 0]
SP[is.na(SP)] <- 0

SP$sum <- SP$uni_score_SP + SP$bi_score_SP + SP$tri_score_SP
SP_sentiment <- subset(SP, SP$sum != 0)
SP_sentiment$tri_score_SP[SP_sentiment$tri_score_SP != 0] <- SP_sentiment$tri_gram[SP_sentiment$tri_score_SP != 0]
SP_sentiment$bi_score_SP[SP_sentiment$bi_score_SP != 0] <- SP_sentiment$bi_gram[SP_sentiment$bi_score_SP != 0]
SP_sentiment$uni_score_SP[SP_sentiment$uni_score_SP != 0] <- SP_sentiment$correct[SP_sentiment$uni_score_SP != 0]
SP_sentiment$correct <- SP_sentiment$bi_gram <- SP_sentiment$tri_gram <- NULL
SP_sentiment$uni_score_concat[SP_sentiment$uni_score_SP != 0] <- paste0(SP_sentiment$uni_score_SP, " (", SP_sentiment$sum, ")")[SP_sentiment$uni_score_SP != 0]
SP_sentiment$bi_score_concat[SP_sentiment$bi_score_SP != 0] <- paste0(SP_sentiment$bi_score_SP, " (", SP_sentiment$sum, ")")[SP_sentiment$bi_score_SP != 0]
SP_sentiment$tri_score_concat[SP_sentiment$tri_score_SP != 0] <- paste0(SP_sentiment$tri_score_SP, " (", SP_sentiment$sum, ")")[SP_sentiment$tri_score_SP != 0]
SP_sentiment[is.na(SP_sentiment)] <- 0
SP_sentiment$concat <- paste0(SP_sentiment$tri_score_concat, " - ", SP_sentiment$bi_score_concat, " - ", SP_sentiment$uni_score_concat)
SP_sentiment$tri_score_concat <- SP_sentiment$bi_score_concat <- SP_sentiment$uni_score_concat <- SP_sentiment$sum <- NULL

temp <- count(SP_sentiment$interaction_id)

SP_sentiment$concat2[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[2:length(SP_sentiment$concat)]
SP_sentiment$concat3[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[3:length(SP_sentiment$concat)]
SP_sentiment$concat4[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[4:length(SP_sentiment$concat)]
SP_sentiment$concat5[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[5:length(SP_sentiment$concat)]
SP_sentiment$concat6[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[6:length(SP_sentiment$concat)]
SP_sentiment$concat7[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[7:length(SP_sentiment$concat)]
SP_sentiment$concat8[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[8:length(SP_sentiment$concat)]
SP_sentiment$concat9[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[9:length(SP_sentiment$concat)]
SP_sentiment$concat10[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[10:length(SP_sentiment$concat)]
SP_sentiment$concat11[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[11:length(SP_sentiment$concat)]
SP_sentiment$concat12[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[12:length(SP_sentiment$concat)]
SP_sentiment$concat13[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[13:length(SP_sentiment$concat)]
SP_sentiment$concat14[1:length(SP_sentiment$concat)] <- SP_sentiment$concat[14:length(SP_sentiment$concat)]

SP_sentiment$freq <- temp$freq[match(SP_sentiment$interaction_id, temp$x)]

SP_sentiment <- SP_sentiment[!duplicated(SP_sentiment$interaction_id, SP_sentiment$freq),]

SP_sentiment$result <- paste0(SP_sentiment$concat,SP_sentiment$concat2,SP_sentiment$concat3,SP_sentiment$concat4,SP_sentiment$concat5,SP_sentiment$concat6,SP_sentiment$concat7,SP_sentiment$concat8,SP_sentiment$concat9, SP_sentiment$concat10, SP_sentiment$concat11, SP_sentiment$concat12, SP_sentiment$concat13, SP_sentiment$concat14) 

SP_sentiment$concat14 <- SP_sentiment$concat13 <- SP_sentiment$concat12 <- SP_sentiment$concat11 <- SP_sentiment$concat10 <- SP_sentiment$concat9 <- SP_sentiment$concat8 <- SP_sentiment$concat7 <- SP_sentiment$concat6 <- SP_sentiment$concat5 <- SP_sentiment$concat4 <- SP_sentiment$concat3 <- SP_sentiment$concat2 <- SP_sentiment$concat <- SP_sentiment$tri_score_SP <- SP_sentiment$bi_score_SP <- SP_sentiment$uni_score_SP <- NULL

##

Intr_Score_SP <- aggregate(uni_score_SP~interaction_id, data = SP, FUN = sum)
temp1 <- aggregate(bi_score_SP~interaction_id, data = SP, FUN = sum)
temp2 <- aggregate(tri_score_SP~interaction_id, data = SP, FUN = sum)

Intr_Score_SP$bi_score_SP <- temp1$`bi_score_SP`[match(Intr_Score_SP$`interaction_id`, temp1$`interaction_id`)]
Intr_Score_SP$tri_score_SP <- temp2$`tri_score_SP`[match(Intr_Score_SP$`interaction_id`, temp2$`interaction_id`)]

colnames(Intr_Score_SP) <- c("ID", "Uni-grams_SP", "Bi-grams_SP", "Tri-grams_SP")
Intr_Score_SP$Sentiment_SP[(Intr_Score_SP$`Uni-grams` + Intr_Score_SP$`Bi-grams` + Intr_Score_SP$`Tri-grams`) < 0] <- "Negative"
Intr_Score_SP$Sentiment_SP[(Intr_Score_SP$`Uni-grams` + Intr_Score_SP$`Bi-grams` + Intr_Score_SP$`Tri-grams`) > 0] <- "Positive"
Intr_Score_SP[is.na(Intr_Score_SP)] <- "Neutral"
Intr_Score_SP$Emotion <- Intr_Score_SP$`Uni-grams_SP` + Intr_Score_SP$`Bi-grams_SP` + Intr_Score_SP$`Tri-grams_SP`

############################################################

EN <- read.csv("../../../../../../FINAL_EN.csv", stringsAsFactors = F)
EN$X <- EN$word <- EN$quad_gram <- NULL

EN$tri_score_EN <- C_Sentiment$Score[match(EN$tri_gram, C_Sentiment$Words)]
EN[is.na(EN)] <- 0

EN$tri_gram[2:length(EN$tri_gram)][EN$tri_score_EN != 0] <- EN$tri_gram[1:length(EN$tri_gram)][EN$tri_score_EN != 0]
EN$tri_gram[3:length(EN$tri_gram)][EN$tri_score_EN != 0] <- EN$tri_gram[2:length(EN$tri_gram)][EN$tri_score_EN != 0]

EN$concat <- paste0(EN$interaction_id, EN$tri_gram)
EN <- EN[!duplicated(EN$concat),]
EN$concat <- NULL

# EN$tri_score_EN <- C_Sentiment$Score[match(EN$tri_gram, C_Sentiment$Words)]
# EN[is.na(EN)] <- 0
# for(i in 1:length(EN$correct)){
#   EN$tri_score_EN[i][EN$tri_score_EN[i] == EN$tri_score_EN[i-1] | EN$tri_score_EN[i] - EN$tri_score_EN[i-1] == 200 | EN$tri_score_EN[i] - EN$tri_score_EN[i-1] == -200] <- 0
#   EN$tri_score_EN[i][EN$tri_score_EN[i] == EN$tri_score_EN[i-2] | EN$tri_score_EN[i] - EN$tri_score_EN[i-2] == 200 | EN$tri_score_EN[i] - EN$tri_score_EN[i-2] == -200] <- 0
#   print(i)
# }
EN$bi_score_EN[EN$tri_score_EN == 0] <- C_Sentiment$Score[match(EN$bi_gram, C_Sentiment$Words)][EN$tri_score_EN == 0]
EN[is.na(EN)] <- 0

EN$concat <- paste0(EN$interaction_id, EN$bi_gram)
EN <- EN[!duplicated(EN$concat),]
EN$concat <- NULL

# EN$bi_gram[2:length(EN$bi_gram)][EN$bi_score_EN != 0] <- EN$bi_gram[1:length(EN$bi_gram)][EN$bi_score_EN != 0]

# EN$bi_score_EN[EN$tri_score_EN == 0] <- C_Sentiment$Score[match(EN$bi_gram, C_Sentiment$Words)][EN$tri_score_EN == 0]
# EN[is.na(EN)] <- 0
# for(i in 1:length(EN$correct)){
#   EN$tri_score_EN[i][EN$tri_score_EN[i] == EN$tri_score_EN[i-1] | EN$tri_score_EN[i] - EN$tri_score_EN[i-1] == 200 | EN$tri_score_EN[i] - EN$tri_score_EN[i-1] == -200] <- 0
#   print(i)
# }
EN$uni_score_EN[EN$bi_score_EN == 0 & EN$tri_score_EN == 0] <- C_Sentiment$Score[match(EN$correct, C_Sentiment$Words)][EN$bi_score_EN == 0 & EN$tri_score_EN == 0]
EN[is.na(EN)] <- 0

EN$sum <- EN$uni_score_EN + EN$bi_score_EN + EN$tri_score_EN
EN_sentiment <- subset(EN, EN$sum != 0)
EN_sentiment$tri_score_EN[EN_sentiment$tri_score_EN != 0] <- EN_sentiment$tri_gram[EN_sentiment$tri_score_EN != 0]
EN_sentiment$bi_score_EN[EN_sentiment$bi_score_EN != 0] <- EN_sentiment$bi_gram[EN_sentiment$bi_score_EN != 0]
EN_sentiment$uni_score_EN[EN_sentiment$uni_score_EN != 0] <- EN_sentiment$correct[EN_sentiment$uni_score_EN != 0]
EN_sentiment$correct <- EN_sentiment$bi_gram <- EN_sentiment$tri_gram <- NULL
EN_sentiment$uni_score_concat[EN_sentiment$uni_score_EN != 0] <- paste0(EN_sentiment$uni_score_EN, " (", EN_sentiment$sum, ")")[EN_sentiment$uni_score_EN != 0]
EN_sentiment$bi_score_concat[EN_sentiment$bi_score_EN != 0] <- paste0(EN_sentiment$bi_score_EN, " (", EN_sentiment$sum, ")")[EN_sentiment$bi_score_EN != 0]
EN_sentiment$tri_score_concat[EN_sentiment$tri_score_EN != 0] <- paste0(EN_sentiment$tri_score_EN, " (", EN_sentiment$sum, ")")[EN_sentiment$tri_score_EN != 0]
EN_sentiment[is.na(EN_sentiment)] <- 0
EN_sentiment$concat <- paste0(EN_sentiment$tri_score_concat, " - ", EN_sentiment$bi_score_concat, " - ", EN_sentiment$uni_score_concat)
EN_sentiment$tri_score_concat <- EN_sentiment$bi_score_concat <- EN_sentiment$uni_score_concat <- EN_sentiment$sum <- NULL

temp <- count(EN_sentiment$interaction_id)

EN_sentiment$concat2[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[2:length(EN_sentiment$concat)]
EN_sentiment$concat3[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[3:length(EN_sentiment$concat)]
EN_sentiment$concat4[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[4:length(EN_sentiment$concat)]
EN_sentiment$concat5[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[5:length(EN_sentiment$concat)]
EN_sentiment$concat6[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[6:length(EN_sentiment$concat)]
EN_sentiment$concat7[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[7:length(EN_sentiment$concat)]
EN_sentiment$concat8[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[8:length(EN_sentiment$concat)]
EN_sentiment$concat9[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[9:length(EN_sentiment$concat)]
EN_sentiment$concat10[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[10:length(EN_sentiment$concat)]
EN_sentiment$concat11[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[11:length(EN_sentiment$concat)]
EN_sentiment$concat12[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[12:length(EN_sentiment$concat)]
EN_sentiment$concat13[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[13:length(EN_sentiment$concat)]
EN_sentiment$concat14[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[14:length(EN_sentiment$concat)]
EN_sentiment$concat15[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[15:length(EN_sentiment$concat)]
EN_sentiment$concat16[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[16:length(EN_sentiment$concat)]
EN_sentiment$concat17[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[17:length(EN_sentiment$concat)]
EN_sentiment$concat18[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[18:length(EN_sentiment$concat)]
EN_sentiment$concat19[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[19:length(EN_sentiment$concat)]
EN_sentiment$concat20[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[20:length(EN_sentiment$concat)]
EN_sentiment$concat21[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[21:length(EN_sentiment$concat)]
EN_sentiment$concat22[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[22:length(EN_sentiment$concat)]
EN_sentiment$concat23[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[23:length(EN_sentiment$concat)]
EN_sentiment$concat24[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[24:length(EN_sentiment$concat)]
EN_sentiment$concat25[1:length(EN_sentiment$concat)] <- EN_sentiment$concat[25:length(EN_sentiment$concat)]

EN_sentiment$freq <- temp$freq[match(EN_sentiment$interaction_id, temp$x)]

EN_sentiment <- EN_sentiment[!duplicated(EN_sentiment$interaction_id, EN_sentiment$freq),]

EN_sentiment$result <- paste0(EN_sentiment$concat,EN_sentiment$concat2,EN_sentiment$concat3,EN_sentiment$concat4,EN_sentiment$concat5,EN_sentiment$concat6,EN_sentiment$concat7,EN_sentiment$concat8,EN_sentiment$concat9, EN_sentiment$concat10, EN_sentiment$concat11, EN_sentiment$concat12, EN_sentiment$concat13, EN_sentiment$concat14, EN_sentiment$concat15, EN_sentiment$concat16, EN_sentiment$concat17, EN_sentiment$concat18, EN_sentiment$concat19, EN_sentiment$concat20, EN_sentiment$concat21, EN_sentiment$concat22, EN_sentiment$concat23, EN_sentiment$concat24, EN_sentiment$concat25) 

EN_sentiment$concat25 <- EN_sentiment$concat24 <- EN_sentiment$concat23 <- EN_sentiment$concat22 <- EN_sentiment$concat21 <- EN_sentiment$concat20 <- EN_sentiment$concat19 <- EN_sentiment$concat18 <- EN_sentiment$concat17 <- EN_sentiment$concat16 <- EN_sentiment$concat15 <- EN_sentiment$concat14 <- EN_sentiment$concat13 <- EN_sentiment$concat12 <- EN_sentiment$concat11 <- EN_sentiment$concat10 <- EN_sentiment$concat9 <- EN_sentiment$concat8 <- EN_sentiment$concat7 <- EN_sentiment$concat6 <- EN_sentiment$concat5 <- EN_sentiment$concat4 <- EN_sentiment$concat3 <- EN_sentiment$concat2 <- EN_sentiment$concat <- EN_sentiment$tri_score_EN <- EN_sentiment$bi_score_EN <- EN_sentiment$uni_score_EN <- NULL

# write.csv(EN,"../EN_Sentiment.csv")

Intr_Score_EN <- aggregate(uni_score_EN~interaction_id, data = EN, FUN = sum)
temp1 <- aggregate(bi_score_EN~interaction_id, data = EN, FUN = sum)
temp2 <- aggregate(tri_score_EN~interaction_id, data = EN, FUN = sum)

Intr_Score_EN$bi_score_EN <- temp1$`bi_score_EN`[match(Intr_Score_EN$`interaction_id`, temp1$`interaction_id`)]
Intr_Score_EN$tri_score_EN <- temp2$`tri_score_EN`[match(Intr_Score_EN$`interaction_id`, temp2$`interaction_id`)]

colnames(Intr_Score_EN) <- c("ID", "Uni-grams_EN", "Bi-grams_EN", "Tri-grams_EN")
Intr_Score_EN$Sentiment_EN[(Intr_Score_EN$`Uni-grams` + Intr_Score_EN$`Bi-grams` + Intr_Score_EN$`Tri-grams`) < 0] <- "Negative"
Intr_Score_EN$Sentiment_EN[(Intr_Score_EN$`Uni-grams` + Intr_Score_EN$`Bi-grams` + Intr_Score_EN$`Tri-grams`) > 0] <- "Positive"
Intr_Score_EN[is.na(Intr_Score_EN)] <- "Neutral"
Intr_Score_EN$Emotion <- Intr_Score_EN$`Uni-grams_EN` + Intr_Score_EN$`Bi-grams_EN` + Intr_Score_EN$`Tri-grams_EN`

############################################################

Intr <- read.csv("../Cosmic_Corpus/FINAL_Interactions_DATA.csv")
Intr <- Intr[1:11]

Intr <- as.data.frame(lapply(Intr[,2:10], FUN = function(x) gsub("n't", "nt", x)))

Intr_Score_EN$Interaction <- Intr$Extra.Notes[match(Intr_Score_EN$ID, Intr$Interaction.ID)]
Intr_Score_SP$Interaction <- Intr$Solutions.Provided[match(Intr_Score_SP$ID, Intr$Interaction.ID)]
Intr_Score_SBT$Interaction <- Intr$Sale.Blockers.Text[match(Intr_Score_SBT$ID, Intr$Interaction.ID)]
Intr_Score_AP$Interaction <- Intr$Action.Plan[match(Intr_Score_AP$ID, Intr$Interaction.ID)]

############################################################

write.csv(Intr_Score_EN, "../Interaction_Score_EN.csv")
write.csv(Intr_Score_SP, "../Interaction_Score_SP.csv")
write.csv(Intr_Score_AP, "../Interaction_Score_AP.csv")
write.csv(Intr_Score_SBT, "../Interaction_Score_SBT.csv")

############################################################

# D_Sentiment <- read.csv("../Dictionary Sentiments.csv", stringsAsFactors = F)
# 
# D_Sentiment$Score[D_Sentiment$Sentiment == "Positive"] <- 100
# D_Sentiment$Score[D_Sentiment$Sentiment == "Negative"] <- -100
# 
# D_Sentiment_Positive <- subset(D_Sentiment, Sentiment == "Positive")
# D_Sentiment_Negative <- subset(D_Sentiment, Sentiment == "Negative")

#Random Sampling of Emotions -----

Intr_Score_AP$type <- "AP"
AP1 <- subset(Intr_Score_AP, Intr_Score_AP$Emotion <= -200)
AP2 <- subset(Intr_Score_AP, Intr_Score_AP$Emotion >= -200 & Intr_Score_AP$Emotion < 0)
AP3 <- subset(Intr_Score_AP, Intr_Score_AP$Emotion >= 200)
AP4 <- subset(Intr_Score_AP, Intr_Score_AP$Emotion == 100)

AP_Extreme <- rbind(AP1, AP3)
AP_Mild <- rbind(AP2, AP4)

AP_Emotion_E <- AP_Extreme[sample(nrow(AP_Extreme), 50),]
AP_Emotion_E$freq <- AP_sentiment$freq[match(AP_Emotion_E$ID, AP_sentiment$interaction_id)]
AP_Emotion_E$split <- AP_sentiment$result[match(AP_Emotion_E$ID, AP_sentiment$interaction_id)]

AP_Emotion_M <- AP_Mild[sample(nrow(AP_Mild), 100),]
AP_Emotion_M$freq <- AP_sentiment$freq[match(AP_Emotion_M$ID, AP_sentiment$interaction_id)]
AP_Emotion_M$split <- AP_sentiment$result[match(AP_Emotion_M$ID, AP_sentiment$interaction_id)]

write.csv(AP_Emotion_M, "../Interaction_Score (WbW) - v4/AP_Emotion_M.csv")
write.csv(AP_Emotion_E, "../Interaction_Score (WbW) - v4/AP_Emotion_E.csv")

############################################################

Intr_Score_SP$type <- "SP"
SP1 <- subset(Intr_Score_SP, Intr_Score_SP$Emotion <= -300)
SP2 <- subset(Intr_Score_SP, Intr_Score_SP$Emotion >= -300 & Intr_Score_SP$Emotion < 0)
SP3 <- subset(Intr_Score_SP, Intr_Score_SP$Emotion >= 200)
SP4 <- subset(Intr_Score_SP, Intr_Score_SP$Emotion == 100)

SP_Extreme <- rbind(SP1, SP3)
SP_Mild <- rbind(SP2, SP4)

SP_Emotion_E <- SP_Extreme[sample(nrow(SP_Extreme), 50),]
SP_Emotion_E$freq <- SP_sentiment$freq[match(SP_Emotion_E$ID, SP_sentiment$interaction_id)]
SP_Emotion_E$split <- SP_sentiment$result[match(SP_Emotion_E$ID, SP_sentiment$interaction_id)]

SP_Emotion_M <- SP_Mild[sample(nrow(SP_Mild), 100),]
SP_Emotion_M$freq <- SP_sentiment$freq[match(SP_Emotion_M$ID, SP_sentiment$interaction_id)]
SP_Emotion_M$split <- SP_sentiment$result[match(SP_Emotion_M$ID, SP_sentiment$interaction_id)]

write.csv(SP_Emotion_M, "../Interaction_Score (WbW) - v4/SP_Emotion_M.csv")
write.csv(SP_Emotion_E, "../Interaction_Score (WbW) - v4/SP_Emotion_E.csv")

############################################################

Intr_Score_SBT$type <- "SBT"
SBT1 <- subset(Intr_Score_SBT, Intr_Score_SBT$Emotion <= -300)
SBT2 <- subset(Intr_Score_SBT, Intr_Score_SBT$Emotion >= -300 & Intr_Score_SBT$Emotion < 0)
SBT3 <- subset(Intr_Score_SBT, Intr_Score_SBT$Emotion >= 100)
SBT4 <- subset(Intr_Score_SBT, Intr_Score_SBT$Emotion == 100)

SBT_Extreme <- rbind(SBT1, SBT3)
SBT_Mild <- rbind(SBT2, SBT4)

SBT_Emotion_E <- SBT_Extreme[sample(nrow(SBT_Extreme), 50),]
SBT_Emotion_E$freq <- SBT_sentiment$freq[match(SBT_Emotion_E$ID, SBT_sentiment$interaction_id)]
SBT_Emotion_E$split <- SBT_sentiment$result[match(SBT_Emotion_E$ID, SBT_sentiment$interaction_id)]

SBT_Emotion_M <- SBT_Mild[sample(nrow(SBT_Mild), 100),]
SBT_Emotion_M$freq <- SBT_sentiment$freq[match(SBT_Emotion_M$ID, SBT_sentiment$interaction_id)]
SBT_Emotion_M$split <- SBT_sentiment$result[match(SBT_Emotion_M$ID, SBT_sentiment$interaction_id)]

write.csv(SBT_Emotion_M, "../Interaction_Score (WbW) - v4/SBT_Emotion_M.csv")
write.csv(SBT_Emotion_E, "../Interaction_Score (WbW) - v4/SBT_Emotion_E.csv")

############################################################

Intr_Score_EN$type <- "EN"
EN1 <- subset(Intr_Score_EN, Intr_Score_EN$Emotion <= -600)
EN2 <- subset(Intr_Score_EN, Intr_Score_EN$Emotion >= -500 & Intr_Score_EN$Emotion < 0)
EN3 <- subset(Intr_Score_EN, Intr_Score_EN$Emotion >= 400)
EN4 <- subset(Intr_Score_EN, Intr_Score_EN$Emotion == 100 | Intr_Score_EN$Emotion == 200)

EN_Extreme <- rbind(EN1, EN3)
EN_Mild <- rbind(EN2, EN4)

EN_Emotion_E <- EN_Extreme[sample(nrow(EN_Extreme), 50),]
EN_Emotion_E$freq <- EN_sentiment$freq[match(EN_Emotion_E$ID, EN_sentiment$interaction_id)]
EN_Emotion_E$split <- EN_sentiment$result[match(EN_Emotion_E$ID, EN_sentiment$interaction_id)]

EN_Emotion_M <- EN_Mild[sample(nrow(EN_Mild), 100),]
EN_Emotion_M$freq <- EN_sentiment$freq[match(EN_Emotion_M$ID, EN_sentiment$interaction_id)]
EN_Emotion_M$split <- EN_sentiment$result[match(EN_Emotion_M$ID, EN_sentiment$interaction_id)]

write.csv(EN_Emotion_M, "../Interaction_Score (WbW) - v4/EN_Emotion_M.csv")
write.csv(EN_Emotion_E, "../Interaction_Score (WbW) - v4/EN_Emotion_E.csv")

############################################################

# write.csv(AP_Emotion_M, "../AP_Emotion_Mild.csv")
# write.csv(AP_Emotion_E, "../AP_Emotion_Extreme.csv")
# 
# write.csv(SP_Emotion_M, "../SP_Emotion_Mild.csv")
# write.csv(SP_Emotion_E, "../SP_Emotion_Extreme.csv")
# 
# write.csv(SBT_Emotion_M, "../SBT_Emotion_Mild.csv")
# write.csv(SBT_Emotion_E, "../SBT_Emotion_Extreme.csv")
# 
# write.csv(EN_Emotion_M, "../EN_Emotion_Mild.csv")
# write.csv(EN_Emotion_E, "../EN_Emotion_Extreme.csv")

############################################################
