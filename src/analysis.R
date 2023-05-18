library(data.table)
library(sandwich)
library(lmtest)
library(stats)

source("./src/common.R")

# Load the file
d <- fread('./data/analysis/w241_Final_Project_Analysis.csv')

########################################################
# Code for Table of Means
########################################################
# Create a dummy placeholder model
table_of_means <- lm(rnorm(d[, .N]) ~ age + as.numeric(factor(gender)) + as.numeric(factor(ethnicity)) + 
                       as.numeric(factor(education)) + health_score + as.numeric(factor(insurance)) + 
                       as.numeric(factor(checkup_freq)) + vaccine_belief - 1, data = d)

# Set the coefficient names
names(table_of_means$coefficients) <- c('age', 'gender', 'ethnicity', 'education', 'health_score', 
                                        'insurance', 'checkup_freq', 'vaccine_belief')

# Get sample means
means <- get_means(d)

# Get standard deviations, only for non-factor variables
sd <- get_sd(d)

# Get p-values
p_values <- get_p_values(d)

########################################################
# Helper functions for robust standard errors
########################################################

# Helper method to get robust standard errors
get_robust_se <- function(model) {
  model$vcovHC_ <- vcovHC(model)
  return(model)
}

# Helper method to test coefficients with robust standard errors
coeftest_robust <- function(model)   {
  coeftest(model, vcov. = model$vcovHC_)
}

########################################################
# Relevel the education and checkup_freq factors
########################################################
d[, education := factor(education, 
                        levels = c('Less than a high school diploma', 
                                   'High School degree or equivalent', 
                                   'Bachelor’s degree', 
                                   'Master’s degree', 
                                   'Doctorate', 
                                   'Other (please specify)'))]
d[, checkup_freq := factor(checkup_freq, 
                           levels = c('Never', 
                                      'Only when necessary', 
                                      'Once a year', 
                                      'Once in 6 months', 
                                      'Once in 3 months'))]

########################################################
# Code for covariate balance
########################################################
# Video Treatment
covariate_video = lm(treatment_video ~ age + ethnicity + gender + education + health_score + checkup_freq + vaccine_belief, data = d)
covariate_video <- get_robust_se(covariate_video)
coeftest_robust(covariate_video)
video_cov_pvalue = anova(lm(treatment_video ~ 1, data = d), covariate_video)$"Pr(>F)"[2]

# Quiz Treatment
covariate_quiz = lm(treatment_quiz ~ age + ethnicity + gender + education + health_score + checkup_freq + vaccine_belief, data = d)
covariate_quiz <- get_robust_se(covariate_quiz)
coeftest_robust(covariate_quiz)
quiz_cov_pvalue = anova(lm(treatment_quiz ~ 1, data = d), covariate_quiz)$"Pr(>F)"[2]

########################################################
# Code to find "good" covariates
########################################################
# Helper method that compiles the formula
get_formula <- function(model, covariate) {
  return(as.formula(paste('outcome_nfid', 
                          paste(paste(attr(model$terms, 'term.labels'), collapse = ' + '), 
                                covariate, sep = ' + '),
                          sep = ' ~ ')))
}

# Method that loops through all covariates and returns the one
# with the most significant p-value, and NA if none found.
get_best_covariate <- function(base_model, covariates) {
  stats <- list()
  p.value <- 0.05
  best_covariate = NA
  # Gather stats for each model with the added covariate
  for(covariate in covariates) {
    enhanced_model <- lm(get_formula(base_model, covariate), data = d)
    stats[[covariate]]['p.value'] <- anova(base_model, enhanced_model)[2,6]
  }
  # Determine the 'best covariate' based on the p-value of the anova test
  for(covariate in covariates) {
    if(!is.na(stats[[covariate]]['p.value']) & stats[[covariate]]['p.value'] < p.value) {
      p.value <- stats[[covariate]]['p.value']
      best_covariate <- covariate
    }
  }
  return(best_covariate)
}

# Recursively loops though all covariates and finds the best model
get_best_model <- function(base_model, all_covariates) {
  while(length(all_covariates) != 0) {
    best_covariate <- get_best_covariate(base_model, all_covariates)
    if (is.na(best_covariate)) break
    all_covariates <- all_covariates[-which(all_covariates == best_covariate)]
    base_model <- lm(get_formula(base_model, best_covariate), data = d)
  }
  return(base_model)
}

# Find the best model
all_covariates <- c('age', 'gender', 'education', 'ethnicity', 'health_score', 
                    'checkup_freq', 'vaccine_belief', 'insurance')
base_model <- lm(outcome_nfid ~ treatment_video + treatment_quiz, data = d)
best_model <- get_best_model(base_model, all_covariates)

########################################################
# Code for calcuating ATE
########################################################

# Calculate the treatment-specific ATE(s)
treatment <- lm(outcome_nfid ~ treatment_video + treatment_quiz, data = d)
treatment <- get_robust_se(treatment)
coeftest_robust(treatment)

# Calculate the treatment-specific ATE(s), controlling for covariates
# Best covariates found in previous block: vaccine_belief, gender, health_score, and age
treatment_covariates <- lm(outcome_nfid ~ treatment_video + treatment_quiz + vaccine_belief + gender + health_score + age, data = d)
treatment_covariates <- get_robust_se(treatment_covariates)
coeftest_robust(treatment_covariates)

########################################################
# Code for calcuating HTE(s)
########################################################
# Age(Binned). Excluding the linear age covariate here.
treatment_hte_age <- lm(outcome_nfid ~ treatment_video * factor(age.bin) + treatment_quiz * factor(age.bin) +
                         vaccine_belief + gender + health_score, data = d)
treatment_hte_age <- get_robust_se(treatment_hte_age)
coeftest_robust(treatment_hte_age)
anova(treatment_covariates, treatment_hte_age)

# Checkup-Frequency (as factor)
# Relevel the checkup frequency as its most interesting to compare once a year to only when necessary.
d[, checkup_freq := factor(checkup_freq, levels = c('Only when necessary', 'Once a year', 'Once in 6 months', 'Once in 3 months', 'Never'))]
treatment_hte_checkup <- lm(outcome_nfid ~ treatment_video * checkup_freq + treatment_quiz * checkup_freq +
                             vaccine_belief + gender + health_score + age, data = d)
treatment_hte_checkup <- get_robust_se(treatment_hte_checkup)
coeftest_robust(treatment_hte_checkup)
anova(treatment_covariates, treatment_hte_checkup)

# Vaccine Beliefs (linear)
treatment_hte_belief <- lm(outcome_nfid ~ treatment_video * vaccine_belief + treatment_quiz * vaccine_belief + 
                            gender + health_score + age, data = d)
treatment_hte_belief <- get_robust_se(treatment_hte_belief)
coeftest_robust(treatment_hte_belief)
hte_belief_pvalue <- anova(treatment_covariates, treatment_hte_belief)[2, 6]

########################################################
# Adjust p.values for multiple comparisons
########################################################
get_p_values <- function(model, terms) {
  p_values <- coeftest(model, model$vcovHC_)[terms, 4]
  return(p_values)
}

all_p_values <- c(get_p_values(treatment_hte_age, c('treatment_video:factor(age.bin)1', 'factor(age.bin)1:treatment_quiz')), 
                  get_p_values(treatment_hte_checkup, c('treatment_video:checkup_freqOnce a year', 'treatment_video:checkup_freqOnce in 6 months', 
                                                        'treatment_video:checkup_freqOnce in 3 months', 'treatment_video:checkup_freqNever', 
                                                        'checkup_freqOnce a year:treatment_quiz', 'checkup_freqOnce in 6 months:treatment_quiz', 
                                                        'checkup_freqOnce in 3 months:treatment_quiz', 'checkup_freqNever:treatment_quiz')), 
                  get_p_values(treatment_hte_belief, c('treatment_video:vaccine_belief', 'vaccine_belief:treatment_quiz')))

adjusted_p_values <- all_p_values
adjusted_p_values <- p.adjust(p = adjusted_p_values, method = "fdr")
########################################################
# Misc. Stargazer
########################################################
# Create a stargazer-friendly order of variables
stargazer.vars.order <- c('treatment_video', 
                          'treatment_quiz', 
                          'vaccine_belief', 
                          'genderMale', 
                          'genderNon-Binary', 
                          'health_score',
                          'factor(age.bin)1', 
                          'treatment_video:factor(age.bin)1', 
                          'factor(age.bin)1:treatment_quiz',
                          'checkup_freqOnce a year', 
                          'checkup_freqOnce in 6 months', 
                          'checkup_freqOnce in 3 months',
                          'checkup_freqNever',  
                          'age', 
                          'treatment_video:checkup_freqOnce a year', 
                          'treatment_video:checkup_freqOnce in 6 months',
                          'treatment_video:checkup_freqOnce in 3 months',
                          'treatment_video:checkup_freqNever',
                          'checkup_freqOnce a year:treatment_quiz',
                          'checkup_freqOnce in 6 months:treatment_quiz',
                          'checkup_freqOnce in 3 months:treatment_quiz',
                          'checkup_freqNever:treatment_quiz',
                          'treatment_video:vaccine_belief',
                          'vaccine_belief:treatment_quiz')
