library(data.table)

########################################################
# Helper Methods for generating Table of Means
########################################################

# Calculate covariate means, across all experimental groups.
get_means <- function(d) {
  means <- lapply(list('control', 'video', 'quiz'), function(slice) {
    unlist(
      lapply(d[, .(age,
                   gender = gender == 'Female',
                   ethnicity = ethnicity != 'White',
                   education = education == 'High School degree or equivalent',
                   health_score,
                   insurance = insurance == 'Yes',
                   checkup_freq = checkup_freq == 'Once a year',
                   vaccine_belief)],
             function(subset) {
               mean(subset[d$treatment_group == slice])
             }
      ))})
  return(means)
}

# Calculate covariate standard deviations, only for non-factor variables,
# across all experimental groups.
get_sd <- function(d) {
  sd <- lapply(list('control', 'video', 'quiz'), function(slice) {
  unlist(
    lapply(d[, .(age,
                 gender = gender == 'Female',
                 ethnicity = ethnicity != 'White',
                 education = education == 'High School degree or equivalent',
                 health_score,
                 insurance = insurance == 'Yes',
                 checkup_freq = checkup_freq == 'Once a year', vaccine_belief)],
           function(subset) {
             ifelse(is.numeric(subset[0]), sd(subset[d$treatment_group == slice]), NA)
           }
    ))})
  return(sd)
}

# Calculate p-values from Anova test for covariate difference of means,
# across all experimental groups.
get_p_values <- function(d) {
  p_values <- unlist(
    lapply(d[, .(age,
                 gender = gender == 'Female',
                 ethnicity = ethnicity != 'White',
                 education = education == 'High School degree or equivalent',
                 health_score,
                 insurance = insurance == 'Yes',
                 checkup_freq = checkup_freq == 'Once a year',
                 vaccine_belief)],
           function(covariate) {
            summary(aov(covariate ~ treatment_group, data = d))[[1]][[5]][1]
          }
  ))
  return(p_values)
}
