# INSTALL PACKAGES:
library(dplyr)
library(tidyverse)

# INPUT WORD:
predict_word = readline(prompt = "Enter any 5-letter word in lowercase:");

# RUN FOLLOWING CODE FOR OUTPUT -----------------------------------
predict_word_df = data.frame(predict_word)
predict_word_df<-predict_word_df%>% 
  separate(col = predict_word, into = c("0","L1","L2","L3","L4","L5"),sep = "")
predict_word_df$repeats_3 <- ifelse(predict_word_df$L1 == predict_word_df$L2 & predict_word_df$L1 == predict_word_df$L3 | 
                                      predict_word_df$L1 == predict_word_df$L2 & predict_word_df$L1 == predict_word_df$L4 | 
                                      predict_word_df$L1 == predict_word_df$L2 & predict_word_df$L1 == predict_word_df$L5 |
                                      predict_word_df$L1 == predict_word_df$L3 & predict_word_df$L1 == predict_word_df$L4 | 
                                      predict_word_df$L1 == predict_word_df$L3 & predict_word_df$L1 == predict_word_df$L5 |
                                      predict_word_df$L1 == predict_word_df$L4 & predict_word_df$L1 == predict_word_df$L5 |
                                      predict_word_df$L2 == predict_word_df$L3 & predict_word_df$L2 == predict_word_df$L4 | 
                                      predict_word_df$L2 == predict_word_df$L3 & predict_word_df$L2 == predict_word_df$L5 | 
                                      predict_word_df$L2 == predict_word_df$L4 & predict_word_df$L2 == predict_word_df$L5 |
                                      predict_word_df$L3 == predict_word_df$L4 & predict_word_df$L3 == predict_word_df$L5,1,0)
predict_word_df$repeats_2 <- ifelse(predict_word_df$L1 == predict_word_df$L2 & predict_word_df$repeats_3!=1| 
                                      predict_word_df$L1 == predict_word_df$L3 & predict_word_df$repeats_3!=1| 
                                      predict_word_df$L1 == predict_word_df$L4 & predict_word_df$repeats_3!=1| 
                                      predict_word_df$L1 == predict_word_df$L5 & predict_word_df$repeats_3!=1|
                                      predict_word_df$L2 == predict_word_df$L3 & predict_word_df$repeats_3!=1| 
                                      predict_word_df$L2 == predict_word_df$L4 & predict_word_df$repeats_3!=1| 
                                      predict_word_df$L2 == predict_word_df$L5 & predict_word_df$repeats_3!=1|
                                      predict_word_df$L3 == predict_word_df$L4 & predict_word_df$repeats_3!=1| 
                                      predict_word_df$L3 == predict_word_df$L5 & predict_word_df$repeats_3!=1|
                                      predict_word_df$L4 == predict_word_df$L5 & predict_word_df$repeats_3!=1,1,0) 
predict_word_df$repeats_0 <- ifelse(predict_word_df$repeats_2 == 0 & 
                                      predict_word_df$repeats_3 == 0, 1, 0)
predict_word_df$vowels_L1 <-ifelse(predict_word_df$L1=='a'|predict_word_df$L1=='e'|predict_word_df$L1=='i'|predict_word_df$L1=='o'|predict_word_df$L1=='u',1,0)
predict_word_df$vowels_L2 <-ifelse(predict_word_df$L2=='a'|predict_word_df$L2=='e'|predict_word_df$L2=='i'|predict_word_df$L2=='o'|predict_word_df$L2=='u',1,0)
predict_word_df$vowels_L3 <-ifelse(predict_word_df$L3=='a'|predict_word_df$L3=='e'|predict_word_df$L3=='i'|predict_word_df$L3=='o'|predict_word_df$L3=='u',1,0)
predict_word_df$vowels_L4 <-ifelse(predict_word_df$L4=='a'|predict_word_df$L4=='e'|predict_word_df$L4=='i'|predict_word_df$L4=='o'|predict_word_df$L4=='u',1,0)
predict_word_df$vowels_L5 <-ifelse(predict_word_df$L5=='a'|predict_word_df$L5=='e'|predict_word_df$L5=='i'|predict_word_df$L5=='o'|predict_word_df$L5=='u',1,0)
vowel_sum<-rowSums(predict_word_df[,c("vowels_L1", "vowels_L2", "vowels_L3","vowels_L4","vowels_L5")])
predict_word_df<-cbind(predict_word_df,vowel_sum)
predict_word_df$L1_rank <-ifelse(predict_word_df$L1=='e'|predict_word_df$L1=='s',1,
                                 ifelse(predict_word_df$L1=='a', 2,
                                        ifelse(predict_word_df$L1=='r', 3,
                                               ifelse(predict_word_df$L1=='i'|predict_word_df$L1=='l'|predict_word_df$L1=='o'|predict_word_df$L1=='t',4,
                                                      ifelse(predict_word_df$L1=='n',5,
                                                             ifelse(predict_word_df$L1=='c'|predict_word_df$L1=='d'|predict_word_df$L1=='p'|predict_word_df$L1=='u', 6,
                                                                    ifelse(predict_word_df$L1=='b'|predict_word_df$L1=='g'|predict_word_df$L1=='h'|predict_word_df$L1=='m'|predict_word_df$L1=='y',7,
                                                                           ifelse(predict_word_df$L1=='f'|predict_word_df$L1=='k'|predict_word_df$L1=='v'|predict_word_df$L1=='w',8,
                                                                                  ifelse(predict_word_df$L1=='j'|predict_word_df$L1=='x'|predict_word_df$L1=='z',9,
                                                                                         ifelse(predict_word_df$L1=='q',10,0))))))))))
predict_word_df$L2_rank <-ifelse(predict_word_df$L2=='e'|predict_word_df$L2=='s',1,
                                 ifelse(predict_word_df$L2=='a', 2,
                                        ifelse(predict_word_df$L2=='r', 3,
                                               ifelse(predict_word_df$L2=='i'|predict_word_df$L2=='l'|predict_word_df$L2=='o'|predict_word_df$L2=='t',4,
                                                      ifelse(predict_word_df$L2=='n',5,
                                                             ifelse(predict_word_df$L2=='c'|predict_word_df$L2=='d'|predict_word_df$L2=='p'|predict_word_df$L2=='u', 6,
                                                                    ifelse(predict_word_df$L2=='b'|predict_word_df$L2=='g'|predict_word_df$L2=='h'|predict_word_df$L2=='m'|predict_word_df$L2=='y',7,
                                                                           ifelse(predict_word_df$L2=='f'|predict_word_df$L2=='k'|predict_word_df$L2=='v'|predict_word_df$L2=='w',8,
                                                                                  ifelse(predict_word_df$L2=='j'|predict_word_df$L2=='x'|predict_word_df$L2=='z',9,
                                                                                         ifelse(predict_word_df$L2=='q',10,0))))))))))
predict_word_df$L3_rank <-ifelse(predict_word_df$L3=='e'|predict_word_df$L3=='s',1,
                                 ifelse(predict_word_df$L3=='a', 2,
                                        ifelse(predict_word_df$L3=='r', 3,
                                               ifelse(predict_word_df$L3=='i'|predict_word_df$L3=='l'|predict_word_df$L3=='o'|predict_word_df$L3=='t',4,
                                                      ifelse(predict_word_df$L3=='n',5,
                                                             ifelse(predict_word_df$L3=='c'|predict_word_df$L3=='d'|predict_word_df$L3=='p'|predict_word_df$L3=='u', 6,
                                                                    ifelse(predict_word_df$L3=='b'|predict_word_df$L3=='g'|predict_word_df$L3=='h'|predict_word_df$L3=='m'|predict_word_df$L3=='y',7,
                                                                           ifelse(predict_word_df$L3=='f'|predict_word_df$L3=='k'|predict_word_df$L3=='v'|predict_word_df$L3=='w',8,
                                                                                  ifelse(predict_word_df$L3=='j'|predict_word_df$L3=='x'|predict_word_df$L3=='z',9,
                                                                                         ifelse(predict_word_df$L3=='q',10,0))))))))))
predict_word_df$L4_rank <-ifelse(predict_word_df$L4=='e'|predict_word_df$L4=='s',1,
                                 ifelse(predict_word_df$L4=='a', 2,
                                        ifelse(predict_word_df$L4=='r', 3,
                                               ifelse(predict_word_df$L4=='i'|predict_word_df$L4=='l'|predict_word_df$L4=='o'|predict_word_df$L4=='t',4,
                                                      ifelse(predict_word_df$L4=='n',5,
                                                             ifelse(predict_word_df$L4=='c'|predict_word_df$L4=='d'|predict_word_df$L4=='p'|predict_word_df$L4=='u', 6,
                                                                    ifelse(predict_word_df$L4=='b'|predict_word_df$L4=='g'|predict_word_df$L4=='h'|predict_word_df$L4=='m'|predict_word_df$L4=='y',7,
                                                                           ifelse(predict_word_df$L4=='f'|predict_word_df$L4=='k'|predict_word_df$L4=='v'|predict_word_df$L4=='w',8,
                                                                                  ifelse(predict_word_df$L4=='j'|predict_word_df$L4=='x'|predict_word_df$L4=='z',9,
                                                                                         ifelse(predict_word_df$L4=='q',10,0))))))))))
predict_word_df$L5_rank <-ifelse(predict_word_df$L5=='e'|predict_word_df$L5=='s',1,
                                 ifelse(predict_word_df$L5=='a', 2,
                                        ifelse(predict_word_df$L5=='r', 3,
                                               ifelse(predict_word_df$L5=='i'|predict_word_df$L5=='l'|predict_word_df$L5=='o'|predict_word_df$L5=='t',4,
                                                      ifelse(predict_word_df$L5=='n',5,
                                                             ifelse(predict_word_df$L5=='c'|predict_word_df$L5=='d'|predict_word_df$L5=='p'|predict_word_df$L5=='u', 6,
                                                                    ifelse(predict_word_df$L5=='b'|predict_word_df$L5=='g'|predict_word_df$L5=='h'|predict_word_df$L5=='m'|predict_word_df$L5=='y',7,
                                                                           ifelse(predict_word_df$L5=='f'|predict_word_df$L5=='k'|predict_word_df$L5=='v'|predict_word_df$L5=='w',8,
                                                                                  ifelse(predict_word_df$L5=='j'|predict_word_df$L5=='x'|predict_word_df$L5=='z',9,
                                                                                         ifelse(predict_word_df$L5=='q',10,0))))))))))
rank_sum<-rowSums(predict_word_df[,c("L1_rank","L2_rank", "L3_rank","L4_rank","L5_rank")])                    
predict_word_df<-cbind(predict_word_df,rank_sum)
difficulty_score<-3.429440 + 0.367526*(as.numeric(predict_word_df$repeats_3)) + 0.402289*(as.numeric(predict_word_df$repeats_2)) + 0.143711*(as.numeric(predict_word_df$vowels_L2)) - 0.172114*(as.numeric(predict_word_df$vowels_L3)) + 0.028613*(as.numeric(predict_word_df$rank_sum))


# OUTPUT:
print(paste("The word", predict_word, "recieved a predicted difficulty score of:", difficulty_score))
if (difficulty_score<=4) print("This word is predicted to be easier than average.") else print("This word is predicted to be harder than average.")
