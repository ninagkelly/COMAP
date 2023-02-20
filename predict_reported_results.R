library(dplyr)
library(lubridate)
library(tidyverse)

# INPUT:
predict_date = readline(prompt = "Following the format yyyy-mm-dd enter any date:");

# RUN THE FOLLOWING CODE FOR OUTPUT -----------------------------------
predict_date_df = data.frame(predict_date)
predict_date_df$`Contest number`<-as.numeric(difftime(predict_date,'2022-01-07',units = "days"))+202
predict_date_df$weekday<-wday(predict_date_df$predict_date,label=TRUE,abbr=FALSE)
predict_date_df$is.sun <- ifelse(predict_date_df$weekday=='Sunday', 1, 0)
predict_date_df$is.mon <- ifelse(predict_date_df$weekday=='Monday', 1, 0)
predict_date_df$is.tue <- ifelse(predict_date_df$weekday=='Tuesday', 1, 0)
predict_date_df$is.wed <- ifelse(predict_date_df$weekday=='Wednesday', 1, 0)
predict_date_df$is.thur <- ifelse(predict_date_df$weekday=='Thursday', 1, 0)
predict_date_df$is.fri <- ifelse(predict_date_df$weekday=='Friday', 1, 0)
predict_date_df$is.sat <- ifelse(predict_date_df$weekday=='Saturday', 1, 0)
predict_date_df$is.weekend<-ifelse(predict_date_df$is.sun==1 | predict_date_df$is.sat==1, 1,0)
predict_r_reported <- exp(12.0871960 -0.0037507*(as.numeric(predict_date_df$`Contest number`)) -0.0322505*(as.numeric(predict_date_df$is.weekend)))
predict_rr <- 12.0871960 -0.0037507*(as.numeric(predict_date_df$`Contest number`)) -0.0322505*(as.numeric(predict_date_df$is.weekend))
l_ci_prr <- as.numeric(predict_rr) - 2.05 * (0.06988)
u_ci_prr <- as.numeric(predict_rr) + 2.05 * (0.06988)

# OUTPUT:
print(paste("We are 95% confident the true number of reported results on", predict_date ,  "is within the interval [", round(exp(l_ci_prr),3),",", round(exp(u_ci_prr),3),"]"))
print(paste("Furthermore, the estimated number of reported results on:", predict_date, "is:", round(predict_r_reported,0)))
