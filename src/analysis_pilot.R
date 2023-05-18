library(data.table)

source("./src/common.R")

# Load the raw file
raw <- fread('./data/raw/w241_Final_Project_Pilot_Raw.csv')
questions <- raw[1,]

# Subset the data -- define columns for analysis
pilot <- raw[3:nrow(raw), .(id = .I,
                            age = ifelse(is.na(as.numeric(Q1)), NA, 2020-as.numeric(Q1)),
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
                            outcome_nfid = as.numeric(Q8_1), 
                            outcome_nami = as.numeric(Q8_2))]

# Somebody wrote their year of birth as 1080 instead of 1980. Fix it.
pilot[pilot$age == 940]$age = 40

# Define overall treatment indicator(s)
pilot[, treatment := ifelse(treatment_quiz == 1 | treatment_video == 1, 1, 0)]
pilot[, treatment_group := ifelse(treatment == 0, 'control', ifelse(treatment_video == 1, 'video', 'quiz'))]

########################################################
# Code for Table of Means
########################################################
# Remove incomplete observations
pilot.complete <- pilot[complete.cases(pilot)]

# Create a dummy placeholder model
pilot_table_of_means <- lm(rnorm(pilot.complete[, .N]) ~ age + as.numeric(factor(gender)) + 
                             as.numeric(factor(ethnicity)) + as.numeric(factor(education)) + 
                             health_score + as.numeric(factor(insurance)) + 
                             as.numeric(factor(checkup_freq)) + vaccine_belief - 1, data = pilot.complete)

# Set the coefficient names
names(pilot_table_of_means$coefficients) <- c('age', 'gender', 'ethnicity', 'education', 
                                              'health_score', 'insurance', 'checkup_freq', 'vaccine_belief')

# Get sample means
pilot_means <- get_means(pilot.complete)

# Get standard deviations
pilot_sd <- get_sd(pilot.complete)

# Get p-values
pilot_p_values <- get_p_values(pilot.complete)
