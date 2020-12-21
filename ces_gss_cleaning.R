#### Preamble ####
# Purpose: Prepare and clean the Canadian Election Study data and General Social Survey data
# Author: Jessica Glustein
# Data: 20 December 2020
# Contact: jessica.glustien@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the CES and GSS data and saved it to inputs/data
# - Don't forget to gitignore it!

library(haven)
library(tidyverse)
library(sjlabelled)
setwd('/Users/jessicag/Desktop/final')

data <- read_dta("CES2019.dta")

ces_cleaned <- data %>% select(gender=cps19_gender, province=cps19_province, education=cps19_education, age=cps19_age, vote=cps19_votechoice) %>% drop_na() %>% filter(vote != 9 && education != 12)
ces_cleaned <- remove_all_labels(ces_cleaned)

gss <- read_csv("gss.csv")
gss_cleaned <- gss %>% select(gender=sex, province, education, age) %>% drop_na() %>% filter(age >= 18)

ces_cleaned <- ces_cleaned %>% mutate(age_factor =  ifelse(age <= 20, "0-20", ifelse(age <= 40, "21-40", ifelse(age <= 60, "41-60", ifelse(age <= 80, "61-80", "81+")))))

gss_cleaned <- gss_cleaned %>% mutate(age_factor =  ifelse(age <= 20, "0-20", ifelse(age <= 40, "21-40", ifelse(age <= 60, "41-60", ifelse(age <= 80, "61-80", "81+")))))

ces_cleaned <- ces_cleaned %>% mutate(province = ifelse(province == 14, "Alberta", ifelse(province == 15, "British Columbia", ifelse(province == 16, "Manitoba", ifelse(province == 17, "New Brunswick", ifelse(province == 18, "Newfoundland and Labrador", ifelse(province == 19, "Northwest Territories", ifelse(province == 20, "Nova Scotia", ifelse(province == 21, "Nunavut", ifelse(province == 22, "Ontario", ifelse(province == 23, "Prince Edward Island", ifelse(province == 24, "Quebec", ifelse(province == 25, "Saskatchewan", "Yukon")))))))))))))

ces_cleaned <- ces_cleaned %>% mutate(gender = ifelse(gender == 1, "Male", "Female"))

ces_cleaned <- ces_cleaned %>% mutate(vote = ifelse(vote == 1, "Liberal Party", ifelse(vote == 2, "Conservative Party", ifelse(vote == 3, "NDP", ifelse(vote == 4, "Bloc Quebecois", ifelse(vote == 5, "Green Party", "Other Party"))))))

ces_cleaned <- ces_cleaned %>% mutate(education = ifelse(education < 5, "Less than highschool", ifelse(education <= 6, "Highschool", ifelse(education == 7, "College", ifelse(education <= 9, "Bachelor's Degree", ifelse(education <= 11, "Above Bachelor's", "Other"))))))

gss_cleaned <- gss_cleaned %>% mutate(education = ifelse(education == "Less than high school diploma or its equivalent", "Less than highschool", ifelse(education == "High school diploma or a high school equivalency certificate", "Highschool", ifelse(education == "Trade certificate or diploma" || education == "College, CEGEP or other non-university certificate or di..." || education == "University certificate or diploma below the bachelor's level", "College", ifelse(education == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)", "Bachelor's Degree", ifelse(education == "University certificate, diploma or degree above the bach...", "Above Bachelor's", "Other"))))))

ces_cleaned <- ces_cleaned %>% mutate(vote_liberal=ifelse(vote == "Liberal Party", 1, 0), vote_conservative=ifelse(vote == "Conservative Party", 1, 0), vote_ndp = ifelse(vote == "NDP", 1, 0))

write.csv(ces_cleaned, "ces_cleaned.csv")
gss_cleaned <- gss_cleaned %>%
  count(gender, province, education, age_factor) %>%
  group_by(gender, province, education, age_factor) 
write.csv(gss_cleaned, "gss_cleaned.csv")