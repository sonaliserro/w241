#!/usr/bin/env Rscript
library(data.table)

# Load experiment and pilot data
data <- fread('http://people.ischool.berkeley.edu/~sonaliserro/w241/data/W241%20Final%20Project%20-%20Prime%20Time%20-%20Prolific_March%2017,%202020_14.16.csv')
data_pilot <- fread('http://people.ischool.berkeley.edu/~sonaliserro/w241/data/W241%20Final%20Project%20-%20Pilot_March%2014,%202020_13.41.csv')

# Write to local directory
fwrite(data, './data/raw/W241_Final_Project_Raw.csv')
fwrite(data_pilot, './data/raw/W241_Final_Project_Pilot_Raw.csv')