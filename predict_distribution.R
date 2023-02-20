# INSTALL PACKAGES:
library(dplyr)
library(lubridate)
library(tidyverse)

# INPUT WORD AND DATE:
predict_dist.w = readline(prompt = "Enter any 5-letter word in lowercase:");
predict_dist.d = readline(prompt = "Following the format yyyy-mm-dd enter any date:");


# RUN THE FOLLOWING CODE FOR OUTPUT -----------------------------------
predict_dist_df = data.frame(predict_dist.d, predict_dist.w)
predict_dist_df$`Contest number`<-as.numeric(difftime(predict_dist.d,'2022-01-07',units = "days"))+202
predict_dist_df<-predict_dist_df %>% 
  separate(col = predict_dist.w, into = c("NA","L1","L2","L3","L4","L5"),sep = "")
predict_dist_df$repeats_3 <- ifelse(predict_dist_df$L1 == predict_dist_df$L2 & predict_dist_df$L1 == predict_dist_df$L3 | 
                                      predict_dist_df$L1 == predict_dist_df$L2 & predict_dist_df$L1 == predict_dist_df$L4 | 
                                      predict_dist_df$L1 == predict_dist_df$L2 & predict_dist_df$L1 == predict_dist_df$L5 |
                                      predict_dist_df$L1 == predict_dist_df$L3 & predict_dist_df$L1 == predict_dist_df$L4 | 
                                      predict_dist_df$L1 == predict_dist_df$L3 & predict_dist_df$L1 == predict_dist_df$L5 |
                                      predict_dist_df$L1 == predict_dist_df$L4 & predict_dist_df$L1 == predict_dist_df$L5 |
                                      predict_dist_df$L2 == predict_dist_df$L3 & predict_dist_df$L2 == predict_dist_df$L4 | 
                                      predict_dist_df$L2 == predict_dist_df$L3 & predict_dist_df$L2 == predict_dist_df$L5 | 
                                      predict_dist_df$L2 == predict_dist_df$L4 & predict_dist_df$L2 == predict_dist_df$L5 |
                                      predict_dist_df$L3 == predict_dist_df$L4 & predict_dist_df$L3 == predict_dist_df$L5,1,0)
predict_dist_df$repeats_2 <- ifelse(predict_dist_df$L1 == predict_dist_df$L2 & predict_dist_df$repeats_3!=1 | 
                                      predict_dist_df$L1 == predict_dist_df$L3 & predict_dist_df$repeats_3!=1 | 
                                      predict_dist_df$L1 == predict_dist_df$L4 & predict_dist_df$repeats_3!=1 | 
                                      predict_dist_df$L1 == predict_dist_df$L5 & predict_dist_df$repeats_3!=1 |
                                      predict_dist_df$L2 == predict_dist_df$L3 & predict_dist_df$repeats_3!=1 | 
                                      predict_dist_df$L2 == predict_dist_df$L4 & predict_dist_df$repeats_3!=1 | 
                                      predict_dist_df$L2 == predict_dist_df$L5 & predict_dist_df$repeats_3!=1 |
                                      predict_dist_df$L3 == predict_dist_df$L4 & predict_dist_df$repeats_3!=1 | 
                                      predict_dist_df$L3 == predict_dist_df$L5 & predict_dist_df$repeats_3!=1 |
                                      predict_dist_df$L4 == predict_dist_df$L5 & predict_dist_df$repeats_3!=1,1,0) 
predict_dist_df$repeats_0 <- ifelse(predict_dist_df$repeats_2 == 0 & 
                                      predict_dist_df$repeats_3 == 0, 1, 0)
predict_dist_df$vowels_L1 <-ifelse(predict_dist_df$L1=='a'|predict_dist_df$L1=='e'|predict_dist_df$L1=='i'|predict_dist_df$L1=='o'|predict_dist_df$L1=='u',1,0)
predict_dist_df$vowels_L2 <-ifelse(predict_dist_df$L2=='a'|predict_dist_df$L2=='e'|predict_dist_df$L2=='i'|predict_dist_df$L2=='o'|predict_dist_df$L2=='u',1,0)
predict_dist_df$vowels_L3 <-ifelse(predict_dist_df$L3=='a'|predict_dist_df$L3=='e'|predict_dist_df$L3=='i'|predict_dist_df$L3=='o'|predict_dist_df$L3=='u',1,0)
predict_dist_df$vowels_L4 <-ifelse(predict_dist_df$L4=='a'|predict_dist_df$L4=='e'|predict_dist_df$L4=='i'|predict_dist_df$L4=='o'|predict_dist_df$L4=='u',1,0)
predict_dist_df$vowels_L5 <-ifelse(predict_dist_df$L5=='a'|predict_dist_df$L5=='e'|predict_dist_df$L5=='i'|predict_dist_df$L5=='o'|predict_dist_df$L5=='u',1,0)
vowel_sum<-rowSums(predict_dist_df[,c("vowels_L1", "vowels_L2", "vowels_L3","vowels_L4","vowels_L5")])
predict_dist_df<-cbind(predict_dist_df,vowel_sum)
predict_dist_df$L1_rank <-ifelse(predict_dist_df$L1=='e'|predict_dist_df$L1=='s',1,
                                 ifelse(predict_dist_df$L1=='a', 2,
                                        ifelse(predict_dist_df$L1=='r', 3,
                                               ifelse(predict_dist_df$L1=='i'|predict_dist_df$L1=='l'|predict_dist_df$L1=='o'|predict_dist_df$L1=='t',4,
                                                      ifelse(predict_dist_df$L1=='n',5,
                                                             ifelse(predict_dist_df$L1=='c'|predict_dist_df$L1=='d'|predict_dist_df$L1=='p'|predict_dist_df$L1=='u', 6,
                                                                    ifelse(predict_dist_df$L1=='b'|predict_dist_df$L1=='g'|predict_dist_df$L1=='h'|predict_dist_df$L1=='m'|predict_dist_df$L1=='y',7,
                                                                           ifelse(predict_dist_df$L1=='f'|predict_dist_df$L1=='k'|predict_dist_df$L1=='v'|predict_dist_df$L1=='w',8,
                                                                                  ifelse(predict_dist_df$L1=='j'|predict_dist_df$L1=='x'|predict_dist_df$L1=='z',9,
                                                                                         ifelse(predict_dist_df$L1=='q',10,0))))))))))
predict_dist_df$L2_rank <-ifelse(predict_dist_df$L2=='e'|predict_dist_df$L2=='s',1,
                                 ifelse(predict_dist_df$L2=='a', 2,
                                        ifelse(predict_dist_df$L2=='r', 3,
                                               ifelse(predict_dist_df$L2=='i'|predict_dist_df$L2=='l'|predict_dist_df$L2=='o'|predict_dist_df$L2=='t',4,
                                                      ifelse(predict_dist_df$L2=='n',5,
                                                             ifelse(predict_dist_df$L2=='c'|predict_dist_df$L2=='d'|predict_dist_df$L2=='p'|predict_dist_df$L2=='u', 6,
                                                                    ifelse(predict_dist_df$L2=='b'|predict_dist_df$L2=='g'|predict_dist_df$L2=='h'|predict_dist_df$L2=='m'|predict_dist_df$L2=='y',7,
                                                                           ifelse(predict_dist_df$L2=='f'|predict_dist_df$L2=='k'|predict_dist_df$L2=='v'|predict_dist_df$L2=='w',8,
                                                                                  ifelse(predict_dist_df$L2=='j'|predict_dist_df$L2=='x'|predict_dist_df$L2=='z',9,
                                                                                         ifelse(predict_dist_df$L2=='q',10,0))))))))))
predict_dist_df$L3_rank <-ifelse(predict_dist_df$L3=='e'|predict_dist_df$L3=='s',1,
                                 ifelse(predict_dist_df$L3=='a', 2,
                                        ifelse(predict_dist_df$L3=='r', 3,
                                               ifelse(predict_dist_df$L3=='i'|predict_dist_df$L3=='l'|predict_dist_df$L3=='o'|predict_dist_df$L3=='t',4,
                                                      ifelse(predict_dist_df$L3=='n',5,
                                                             ifelse(predict_dist_df$L3=='c'|predict_dist_df$L3=='d'|predict_dist_df$L3=='p'|predict_dist_df$L3=='u', 6,
                                                                    ifelse(predict_dist_df$L3=='b'|predict_dist_df$L3=='g'|predict_dist_df$L3=='h'|predict_dist_df$L3=='m'|predict_dist_df$L3=='y',7,
                                                                           ifelse(predict_dist_df$L3=='f'|predict_dist_df$L3=='k'|predict_dist_df$L3=='v'|predict_dist_df$L3=='w',8,
                                                                                  ifelse(predict_dist_df$L3=='j'|predict_dist_df$L3=='x'|predict_dist_df$L3=='z',9,
                                                                                         ifelse(predict_dist_df$L3=='q',10,0))))))))))
predict_dist_df$L4_rank <-ifelse(predict_dist_df$L4=='e'|predict_dist_df$L4=='s',1,
                                 ifelse(predict_dist_df$L4=='a', 2,
                                        ifelse(predict_dist_df$L4=='r', 3,
                                               ifelse(predict_dist_df$L4=='i'|predict_dist_df$L4=='l'|predict_dist_df$L4=='o'|predict_dist_df$L4=='t',4,
                                                      ifelse(predict_dist_df$L4=='n',5,
                                                             ifelse(predict_dist_df$L4=='c'|predict_dist_df$L4=='d'|predict_dist_df$L4=='p'|predict_dist_df$L4=='u', 6,
                                                                    ifelse(predict_dist_df$L4=='b'|predict_dist_df$L4=='g'|predict_dist_df$L4=='h'|predict_dist_df$L4=='m'|predict_dist_df$L4=='y',7,
                                                                           ifelse(predict_dist_df$L4=='f'|predict_dist_df$L4=='k'|predict_dist_df$L4=='v'|predict_dist_df$L4=='w',8,
                                                                                  ifelse(predict_dist_df$L4=='j'|predict_dist_df$L4=='x'|predict_dist_df$L4=='z',9,
                                                                                         ifelse(predict_dist_df$L4=='q',10,0))))))))))
predict_dist_df$L5_rank <-ifelse(predict_dist_df$L5=='e'|predict_dist_df$L5=='s',1,
                                 ifelse(predict_dist_df$L5=='a', 2,
                                        ifelse(predict_dist_df$L5=='r', 3,
                                               ifelse(predict_dist_df$L5=='i'|predict_dist_df$L5=='l'|predict_dist_df$L5=='o'|predict_dist_df$L5=='t',4,
                                                      ifelse(predict_dist_df$L5=='n',5,
                                                             ifelse(predict_dist_df$L5=='c'|predict_dist_df$L5=='d'|predict_dist_df$L5=='p'|predict_dist_df$L5=='u', 6,
                                                                    ifelse(predict_dist_df$L5=='b'|predict_dist_df$L5=='g'|predict_dist_df$L5=='h'|predict_dist_df$L5=='m'|predict_dist_df$L5=='y',7,
                                                                           ifelse(predict_dist_df$L5=='f'|predict_dist_df$L5=='k'|predict_dist_df$L5=='v'|predict_dist_df$L5=='w',8,
                                                                                  ifelse(predict_dist_df$L5=='j'|predict_dist_df$L5=='x'|predict_dist_df$L5=='z',9,
                                                                                         ifelse(predict_dist_df$L5=='q',10,0))))))))))
rank_sum<-rowSums(predict_dist_df[,c("L1_rank","L2_rank", "L3_rank","L4_rank","L5_rank")])                    
predict_dist_df<-cbind(predict_dist_df,rank_sum)
predict_dist_df$weekday<-lubridate::wday(predict_dist_df$predict_dist.d,label=TRUE,abbr=FALSE)
predict_dist_df$is.sun <- ifelse(predict_dist_df$weekday=='Sunday', 1, 0)
predict_dist_df$is.mon <- ifelse(predict_dist_df$weekday=='Monday', 1, 0)
predict_dist_df$is.tue <- ifelse(predict_dist_df$weekday=='Tuesday', 1, 0)
predict_dist_df$is.wed <- ifelse(predict_dist_df$weekday=='Wednesday', 1, 0)
predict_dist_df$is.thur <- ifelse(predict_dist_df$weekday=='Thursday', 1, 0)
predict_dist_df$is.fri <- ifelse(predict_dist_df$weekday=='Friday', 1, 0)
predict_dist_df$is.sat <- ifelse(predict_dist_df$weekday=='Saturday', 1, 0)
predict_dist_df$is.weekend<-ifelse(predict_dist_df$is.sun==1 | predict_dist_df$is.sat==1, 1,0)
predicted_difficulty_score<-3.429440 + 0.367526*(as.numeric(predict_dist_df$repeats_3)) + 0.402289*(as.numeric(predict_dist_df$repeats_2)) + 0.143711*(as.numeric(predict_dist_df$vowels_L2)) - 0.172114*(as.numeric(predict_dist_df$vowels_L3)) + 0.028613*(as.numeric(predict_dist_df$rank_sum))
predict_dist_df<-cbind(predict_dist_df,predicted_difficulty_score)
predicted_1_try <- 4.1365176 -0.0018749*(as.numeric(predict_dist_df$`Contest number`)) +0.4975872*(as.numeric(predict_dist_df$repeats_3)) -0.2606656*(as.numeric(predict_dist_df$repeats_2)) -0.2123621*(as.numeric(predict_dist_df$vowels_L1)) -0.0350025*(as.numeric(predict_dist_df$rank_sum)) -0.4934307*(as.numeric(predict_dist_df$predicted_difficulty_score))
predicted_2_tries <- 28.32162 + 2.06686*(as.numeric(predict_dist_df$repeats_3)) -2.02492*(as.numeric(predict_dist_df$repeats_2)) -0.84663*(as.numeric(predict_dist_df$vowels_L1)) -0.27949*(as.numeric(predict_dist_df$rank_sum)) -3.77219*(as.numeric(predict_dist_df$predicted_difficulty_score))
predicted_3_tries <- 74.55338 -5.26544*(as.numeric(predict_dist_df$repeats_3)) -2.88852*(as.numeric(predict_dist_df$repeats_2)) -1.42839*(as.numeric(predict_dist_df$vowel_sum)) -0.34051*(as.numeric(predict_dist_df$rank_sum)) -9.87875*(as.numeric(predict_dist_df$predicted_difficulty_score))
predicted_4_tries <- 47.913414 +0.006372*(as.numeric(predict_dist_df$`Contest number`)) -9.374277*(as.numeric(predict_dist_df$repeats_3)) +1.905534*(as.numeric(predict_dist_df$repeats_2)) +1.695587*(as.numeric(predict_dist_df$vowels_L1)) -1.142326*(as.numeric(predict_dist_df$vowel_sum)) +0.361775*(as.numeric(predict_dist_df$rank_sum)) -5.807345*(as.numeric(predict_dist_df$predicted_difficulty_score))
predicted_5_tries <- -9.8916 +10.2211*(as.numeric(predict_dist_df$repeats_3)) +3.0125*(as.numeric(predict_dist_df$repeats_2)) +1.71876*(as.numeric(predict_dist_df$vowel_sum)) +0.3792*(as.numeric(predict_dist_df$rank_sum)) -2.2896*(as.numeric(predict_dist_df$is.fri)) +5.2094*(as.numeric(predict_dist_df$predicted_difficulty_score))
predicted_6_tries <- -24.400689  -0.007278*(as.numeric(predict_dist_df$`Contest number`)) +9.329512*(as.numeric(predict_dist_df$predicted_difficulty_score))
predicted_7plus_tries <- -17.03534  -0.11677*(as.numeric(predict_dist_df$rank_sum)) +5.39382*(as.numeric(predict_dist_df$predicted_difficulty_score))
l_ci_p1t <- as.numeric(predicted_1_try) - 2.05 * (0.2478)
u_ci_p1t <- as.numeric(predicted_1_try) + 2.05 * (0.2478)
l_ci_p2t <- as.numeric(predicted_2_tries) - 2.05 * (2.767)
u_ci_p2t <- as.numeric(predicted_2_tries) + 2.05 * (2.767)
l_ci_p3t <- as.numeric(predicted_3_tries) - 2.05 * (4.877)
u_ci_p3t <- as.numeric(predicted_3_tries) + 2.05 * (4.877)
l_ci_p4t <- as.numeric(predicted_4_tries) - 2.05 * (4.231)
u_ci_p4t <- as.numeric(predicted_4_tries) + 2.05 * (4.231)
l_ci_p5t <- as.numeric(predicted_5_tries) - 2.05 * (4.396)
u_ci_p5t <- as.numeric(predicted_5_tries) + 2.05 * (4.396)
l_ci_p6t <- as.numeric(predicted_6_tries) - 2.05 * (3.59)
u_ci_p6t <- as.numeric(predicted_6_tries) + 2.05 * (3.59)
l_ci_p7plust <- as.numeric(predicted_7plus_tries) - 2.05 * (3.407)
u_ci_p7plust <- as.numeric(predicted_7plus_tries) + 2.05 * (3.407)
predicted_r_reported <- exp(12.0871960 -0.0037507*(as.numeric(predict_dist_df$`Contest number`)) -0.0322505*(as.numeric(predict_dist_df$is.weekend)))

# OUTPUT:  ------------------------------------------------------------------

print(paste("For the word:", predict_dist.w, "on the date of:", predict_dist.d , "We are 95% confident the true proportion of players who report answering correctly in 1 try is within the interval [", round(l_ci_p1t,3),",", round(u_ci_p1t,3),"]",
            "in 2 tries is within the interval [", round(l_ci_p2t,3),",", round(u_ci_p2t,3),"]",
            "in 3 tries is within the interval [", round(l_ci_p3t,3),",", round(u_ci_p3t,3),"]",
            "in 4 tries is within the interval [", round(l_ci_p4t,3),",", round(u_ci_p4t,3),"]",
            "in 5 tries is within the interval [", round(l_ci_p5t,3),",", round(u_ci_p5t,3),"]",
            "in 6 tries is within the interval [", round(l_ci_p6t,3),",", round(u_ci_p6t,3),"]",
            "and in 7-plus tries is within the interval [ 0 ,",round(u_ci_p7plust,3), "]" ))

print(paste("Furthermore, For the word:", predict_dist.w, "on the date of:", predict_dist.d , "the estimated proportion of players who report answering correctly in 1 try is:", round(predicted_1_try,3),
            "in 2 tries is:", round(predicted_2_tries,3),
            "in 3 tries is:", round(predicted_3_tries,3),
            "in 4 tries is:", round(predicted_4_tries,3),
            "in 5 tries is:", round(predicted_5_tries,3),
            "in 6 tries is:", round(predicted_6_tries,3),
            "and in 7-plus tries is:", round(predicted_7plus_tries,3)))



