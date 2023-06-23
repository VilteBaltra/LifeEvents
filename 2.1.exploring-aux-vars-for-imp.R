# LIFE EVENTS STUDY - CORRELATION SCRIPT (informs imp model)

######################################
###             SET UP             ###
######################################
# devtools::install_github("laresbernardo/lares")
library(lares) # for corr_var 
library(gridExtra) # for ggsave plots in grid
library(tidyverse)

# define data path
output_path = "/Volumes/Files/Psychology/ResearchProjects/Ewalton/EarlyCause/WP4/LifeEvents/neuroticism-2023-03-30/"

# read in formatted data (obtained with '0.data-prep.R' script)
dataset_clean <- readRDS(paste0(output_path, 'dataset_clean_LE_2023-06-13.rds')) # read in ALSPAC data with selected variables
dim(dataset_clean) # 3872    95

######################################
###      PLOT TOP CORRELATIONS     ###
######################################

# item names for unweighted LE scores (23 in total)
le_unweighted <- c("ccs2000", "ccs2010", "ccs2020" ,"ccs2030", "ccs2040", "ccs2050", "ccs2060", "ccs2070" ,"ccs2080" ,"ccs2090", "ccs2100",
                   "ccs2110", "ccs2120", "ccs2130", "ccs2140", "ccs2150", "ccs2160", "ccs2170", "ccs2180", "ccs2190", "ccs2200", "ccs2210",
                   "ccs2220")

# corr_var function does not work in a loop, so run this instead and copy-paste
for(item in le_unweighted){
  cat(noquote(paste0("corr_var(dataset_clean, ", item,", top = 15),", '\n')))
}

# First plotting top 15 correlations for each unweighted_le item for the entire dataset
# pdf() did not work, so using ggsave() instead
out1 <- grid.arrange(corr_var(dataset_clean, ccs2000, top = 15),
                     corr_var(dataset_clean, ccs2010, top = 15),
                     corr_var(dataset_clean, ccs2020, top = 15),
                     corr_var(dataset_clean, ccs2030, top = 15),
                     corr_var(dataset_clean, ccs2040, top = 15),
                     corr_var(dataset_clean, ccs2050, top = 15),
                     corr_var(dataset_clean, ccs2060, top = 15),
                     corr_var(dataset_clean, ccs2070, top = 15),
                     corr_var(dataset_clean, ccs2080, top = 15),
                     corr_var(dataset_clean, ccs2090, top = 15),
                     corr_var(dataset_clean, ccs2100, top = 15),
                     corr_var(dataset_clean, ccs2110, top = 15),
                     ncol=3, nrow =4)
ggsave("corr-unweighted-le-part1.jpeg", out1)

out2 <- grid.arrange(corr_var(dataset_clean, ccs2120, top = 15),
                     corr_var(dataset_clean, ccs2130, top = 15),
                     corr_var(dataset_clean, ccs2140, top = 15),
                     corr_var(dataset_clean, ccs2150, top = 15),
                     corr_var(dataset_clean, ccs2160, top = 15),
                     corr_var(dataset_clean, ccs2170, top = 15),
                     corr_var(dataset_clean, ccs2180, top = 15),
                     corr_var(dataset_clean, ccs2190, top = 15),
                     corr_var(dataset_clean, ccs2200, top = 15),
                     corr_var(dataset_clean, ccs2210, top = 15),
                     corr_var(dataset_clean, ccs2220, top = 15),
                     ncol=3, nrow =4)
ggsave("corr-unweighted-le-part2.jpeg", out2)


# Now only plotting top 15 correlations using a subset of data (without LE scores, as these won't be used in LE item imputations)
scores <- c('weighted_LE_mean', 'unweighted_LE_mean')
subset <- dataset_clean %>% select(-c(scores))

out3 <- grid.arrange(corr_var(subset, ccs2000, top = 15),
                     corr_var(subset, ccs2010, top = 15),
                     corr_var(subset, ccs2020, top = 15),
                     corr_var(subset, ccs2030, top = 15),
                     corr_var(subset, ccs2040, top = 15),
                     corr_var(subset, ccs2050, top = 15),
                     corr_var(subset, ccs2060, top = 15),
                     corr_var(subset, ccs2070, top = 15),
                     corr_var(subset, ccs2080, top = 15),
                     corr_var(subset, ccs2090, top = 15),
                     corr_var(subset, ccs2100, top = 15),
                     corr_var(subset, ccs2110, top = 15),
                     ncol=3, nrow =4)
ggsave("subset-corr-unweighted-le-part1.jpeg", out3)


out4 <- grid.arrange(corr_var(subset, ccs2120, top = 15),
                     corr_var(subset, ccs2130, top = 15),
                     corr_var(subset, ccs2140, top = 15),
                     corr_var(subset, ccs2150, top = 15),
                     corr_var(subset, ccs2160, top = 15),
                     corr_var(subset, ccs2170, top = 15),
                     corr_var(subset, ccs2180, top = 15),
                     corr_var(subset, ccs2190, top = 15),
                     corr_var(subset, ccs2200, top = 15),
                     corr_var(subset, ccs2210, top = 15),
                     corr_var(subset, ccs2220, top = 15),
                     ncol=3, nrow =4)
ggsave("subset-corr-unweighted-le-part2.jpeg", out4)


########### end script ################


