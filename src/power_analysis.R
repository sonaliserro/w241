library(ggplot2)
# Define a function that would take outcome and treatment vectors and return a treatment effect
est_ate = function(outcome, treatment){
  return(mean(outcome[treatment==1]) - mean(outcome[treatment==0]))
}

# Define a function to randomize treatment assignments for a study of size n, with 2/3 of individuals being assigned to treatment (2 treatment groups and 1 control group)
randomize = function(N){
  return(sample(c(rep(1,round(N*2/3)), rep(0,round(N - N*2/3)))))
}


#define a function to create normal distribution representing potential outcomes (need to restrict to a max & min)
po = function(mean, sd, size, upper){
  samp = rnorm(size + 200, mean, sd)
  samp = samp[samp >= 0 & samp <= upper]
  samp = samp[1:size]
  return(samp)
}

# Next, define function for us to simulate a single study of ours. the function takes the following inputs:
# 1. control_effect = our hypothesized proportion of individuals who would elect to donate to the flu campaign in control group
# 2. treatment_effect_size = our hypothesized treatment_effect. if we think our proportion would increase from 0.15 to 0.20, this would be 0.05
# 3. size = how many subjects are in our experiment

simulate_study = function(control_effect, treatment_effect_size, size) {
  
  #random draws to creat potential control outcomes
  po_control = po(control_effect, 0.26, size, 1-treatment_effect_size)
  
  #create treatment group potential outcomes
  po_treatment = po_control + treatment_effect_size
  
  #randomize treatment assignments
  treatment = randomize(size)
  
  #pull outcomes based on our assignments
  outcomes = po_treatment * treatment + po_control * (1 - treatment)
  
  #calculate the average treatment effect
  ate = est_ate(outcomes, treatment)
  
  #find distribution under the null hypothesis
  distribution_under_sharp_null = replicate(1000, est_ate(outcomes, randomize(size)))
  
  #plot distribution under the null and our estimated ate - commented out for power analysis
  #plot(density(distribution_under_sharp_null),
  #     main = "Density under Sharp Null")
  #abline(v=ate, col = "darkgreen", lwd = 2)
  
  #paste(mean(ate < distribution_under_sharp_null))
  #return the p-value
  return(mean(ate < distribution_under_sharp_null))
}

#baseline proportion of $1 given to NFID = 0.5
#we hypothesize our treatment effect will be approximately a 5 cent increase in average amount allocated to NFID
#we will have 390 subjects
cntrl = 0.5
treat = 0.05
size = 390

simulate_study(cntrl, treat, size)

# can run a power analysis by repeatedly calling the simulate study function with our own defined hypothesis for the control proportions, treament effect,
# and study size

p_values = replicate(1000, simulate_study(cntrl, treat, size)) # distribution of pvalues

plot(density(p_values, from = 0, to = 1), xlim = c(0,1),
     main = paste("Density of p-values \n t = ", toString(treat)))

power = mean(p_values < 0.05)

#Investigate power curve as a function of sample size
#holding size & control effects constant, see relationship between power and treatment effect
sizes = seq(from = 50, to = 600, by = 50)

vary_size = function(cntrl, treat, size_range){
  powers = rep(NA, length(size_range))
  
  for (i in c(1:length(size_range))){
    p_values = replicate(500, simulate_study(cntrl, treat, size_range[i]))
    powers[i] = mean(p_values < 0.05)
  }
  return(powers)
}

powers = vary_size(cntrl, treat, sizes)

p = data.frame(size = sizes, power = powers)
plot1 = ggplot(p, aes(size, power)) + geom_point()
plot1 + geom_smooth(method = "loess") + xlab("Sample Size") + ylab("Power") + ggtitle("Power Curve")

### BELOW FUNCTIONS CAN BE REMOVED, UNLESS @SONALI & @MATT FEEL OTHERWISE
#holding treatment & sample size constant, see relationship between power and control flu probability
#vary_cntrl = function(cntrl_range, treat, size){
#  powers = rep(NA, length(cntrl_range))

#  for (i in c(1:length(cntrl_range))){
#    p_values = replicate(500, simulate_study(cntrl_range[i], treat, size))
#    powers[i] = mean(p_values < 0.05)
#  }

#  scatter.smooth(cntrl_range, powers, main = "Power as function of the control potential outcomes",
#                 xlab = "Control Potential",
#                 ylab = "Power")
#  }

#vary_cntrl(seq(from = 0.05, to = 0.8, by = 0.05), 0.15, 200)#


#holding size & control effects constant, see relationship between power and treatment effect
#vary_treat = function(cntrl, treat_range, size){
#  powers = rep(NA, length(treat_range))

#  for (i in c(1:length(treat_range))){
#    p_values = replicate(500, simulate_study(cntrl, treat_range[i], size))
#    powers[i] = mean(p_values < 0.05)
#  }

#  scatter.smooth(treat_range, powers, main = "Power as function of the treatment effects",
#                 xlab = "Treatment effect",
#                 ylab = "Power")
#}

#vary_treat(0.4, seq(from = 0.05, to = 0.4, by = 0.05), 200)
