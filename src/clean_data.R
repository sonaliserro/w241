#!/usr/bin/env Rscript
library(data.table)

# Read data
raw = fread(file = './data/raw/W241_Final_Project_Raw.csv')
questions = raw[1,]

# Subset the data and define columns for analysis
d = raw[3:nrow(raw), .(id = .I,
                       age=ifelse(is.na(as.numeric(Q1)), NA, 2020-as.numeric(Q1)),
                       gender = Q2,
                       ethnicity = Q3,
                       eth_other = Q3_6_TEXT,
                       education = Q4,
                       insurance = Q5,
                       health_score = as.numeric(Q6),
                       checkup_freq = Q7,
                       vaccine_belief = as.numeric(Q8),
                       treatment_video = ifelse(is.na(as.numeric(`Q10_Page Submit`)), 0, 1), 
                       treatment_quiz = ifelse(Quiz1 == "", 0, 1),
                       quiz_score = as.numeric(SC0),
                       outcome_nfid = as.numeric(Outcome_1), 
                       outcome_nami = as.numeric(Outcome_2))]

# modify some of the age data - 2 individauls put their age instead of birth year (adjusted below)
d[d$age > 100,]$age = 2020 - d[d$age > 100,]$age

# Another individual fat-fingered 20000 instead of 2000, modified here
d[d$age < 0,]$age = 20

# Define overall treatment indicator(s)
d[, treatment := ifelse(treatment_quiz == 1 | treatment_video == 1, 1, 0)]
d[, treatment_group := ifelse(treatment == 0, 'control', ifelse(treatment_video == 1, 'video', 'quiz'))]

# Bin the age, <40 and >=40
d[, age.bin := ifelse((is.na(age) | age < 40), 0, 1)]

# Bin the Vaccine Beliefs
d[, belief.bin := ifelse(vaccine_belief %in% 0:6, 0, ifelse(vaccine_belief %in% 7:8, 1, 2))]

# Write data out to analysis folder
fwrite(d, file = "./data/analysis/w241_Final_Project_Analysis.csv")