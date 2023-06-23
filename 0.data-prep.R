# LIFE EVENTS STUDY - DATA PREPARATION SCRIPT 

######################################
###             SET UP             ###
######################################
library(foreign)
library(tidyverse)
library(openxlsx)
# devtools::install_github("laresbernardo/lares")
library(lares) # for corr_var 
library(gridExtra) # for ggsave plots in grid

# define data and output paths
data_path = "/Volumes/Files/Psychology/ResearchProjects/Ewalton/EarlyCause/data/ALSPAC/"
output_path = "/Volumes/Files/Psychology/ResearchProjects/Ewalton/EarlyCause/WP4/LifeEvents/neuroticism-2023-03-30/"

# read in raw phenotypic data
raw_data <- read.spss(paste0(data_path, 'EarlyCause_AHupdated_CIDB2957_21OCT21.sav'), use.value.labels = FALSE, to.data.frame = TRUE)

######################################
### SELECT ALL RELEVANT VARIABLES ###
######################################

# define ethnicity (White = 1, non-White = 0)
raw_data$ethnicity <- ifelse(is.na(raw_data$c800) & is.na(raw_data$c801), NA, 
                             ifelse((raw_data$c800 == 1 & raw_data$c801 == 1) | 
                                      ((is.na(raw_data$c800) | is.na(raw_data$c801)) & 
                                         (raw_data$c800 == 1 | raw_data$c801 == 1)), 1, 0))

# sex of the child
raw_data$sex <- raw_data$kz021 # 1 = Male; 2 = Female.

# siblings identifier
raw_data$sibling <- raw_data$mult # multiple pregnancies in ALSPAC (exclusion criteria)

# twin identifier
raw_data$twin <- raw_data$qlet # birth order 

# maternal education (university degree during pregnancy)
raw_data$mum_uni <- as.factor(raw_data$c641)

## SDQ 
# SDQ emotional symptoms score at 16y (completed by mother)
raw_data$emot_symp_16y <- raw_data$tc4025a

# SDQ: age of study teenager at completion (months to years)
raw_data$emot_symp_age_16y <- raw_data$tc9991a / 12

## SMFQ 
# Moods and Feelings total score at 17.5 years (child self reported)
raw_data$smfq_17.5y <- raw_data$CCXD917

# Age of study child at completion (months to years)
raw_data$smfq_age_17.5y <- raw_data$CCXD006 / 12 	

# item names for weighted LE scores (23 in total)
le_weighted <- c("ccs2001", "ccs2011", "ccs2021", "ccs2031", "ccs2041","ccs2051", "ccs2061", "ccs2071", "ccs2081", "ccs2091", "ccs2101",
                 "ccs2111", "ccs2121", "ccs2131" ,"ccs2141", "ccs2151", "ccs2161", "ccs2171" ,"ccs2181", "ccs2191", "ccs2201" ,"ccs2211", 
                 "ccs2221")

# item names for unweighted LE scores (23 in total)
le_unweighted <- c("ccs2000", "ccs2010", "ccs2020" ,"ccs2030", "ccs2040", "ccs2050", "ccs2060", "ccs2070" ,"ccs2080" ,"ccs2090", "ccs2100",
                   "ccs2110", "ccs2120", "ccs2130", "ccs2140", "ccs2150", "ccs2160", "ccs2170", "ccs2180", "ccs2190", "ccs2200", "ccs2210",
                   "ccs2220")

## Auxiliary variables 
# select auxiliary variables that will be used to help with the imputation
aux <- c('ACEscore_extended_0_16yrs', 'ACEcat_extended_0_16yrs', 'physical_illness_parent_0_16yrs',
         'physical_illness_child_0_16yrs', 'vlnc_btwn_chld_nd_prtnr_0_16yrs', 'social_support_parent_0_16yrs',
         'neighbourhood_0_16yrs', 'financial_difficulties_0_16yrs', 'social_class_0_16yrs', 'parental_separation_0_16yrs',
         'parent_convicted_offenc_0_16yrs', 'mentl_hlth_prblms_r_scd_0_16yrs', 'substance_household_0_16yrs', 
         'violence_between_parnts_0_16yrs', 'parent_child_bond_0_16yrs', 'bullying_0_16yrs', 'emotional_neglect_0_16yrs',
         'emotional_abuse_0_16yrs', 'sexual_abuse_0_16yrs', 'physical_abuse_0_16yrs',
         'mz028b',  # maternal age at intake (used for imputation) 
         'dw043', # maternal bmi
         'pub803',	# child's height (cm) at 16y
         'pub804', #	child's weight (kg) at 16y
         'a525',  # PRES marital status at 8w gest
         'a551', #  Crowding index at 8w gest
         'bestgest', # # gestational age at birth (used for imputation)
         'kz030', # gestational weight (used for imputation)
         'b032',  # parity (used for imputation)
         'smfq_17.5y',
         'smfq_age_17.5y',
         'fh6878', # generalised anxiety disorder 15y 6m
         'fh6877', # any anxiety disorder
         'fh6870', # any adhd disorder
         'fh6872'  # any behavioural disorder
         )


# subset relevant variables
outcomes <- c('emot_symp_16y', 'smfq_17.5y')
covariates <- c('ethnicity', 'sex', 'mum_uni', 'emot_symp_age_16y', 'smfq_age_17.5y')
other <- c('sibling', 'twin')
dataset_full <- raw_data[, c('cidB2957', outcomes, covariates, other, le_weighted, le_unweighted, aux)]

# # convert to numeric
# dataset_full[, c(outcomes,covariates, 'sibling')] <- apply(dataset_full[, c(outcomes,covariates, 'sibling')], 2, as.numeric)
# str(dataset_full) # check 

######################################
### RECODE and CALCULATE LE MEAN   ###
######################################
# recode weighted response categories so that smaller numbers indicate lower stress
# currently in unweighted LE 1 = yes, 2 = no. In weighted LE, 1 = Very unpleasant and 5 = Very pleasant 
# we want to reverse the coding in weighted LE + add make 0 = did not happen

# Step 1: recode all weighted LE items
dataset_full[, le_weighted] <- abs(dataset_full[, le_weighted] - 6) 
# add 0 = did not happen (to weighted LE)
dataset_full[,le_weighted][dataset_full[, le_unweighted] == 2 ] <- 0
# recode unweighted LE, so that 0 = no, 1 = yes 
dataset_full[,le_unweighted] <- abs(dataset_full[, le_unweighted] - 2) 

# Step 2: calculate mean weighted and unweighted scores 
dataset_full$weighted_LE_mean <- rowMeans(dataset_full[, le_weighted], na.rm = T)  # first calculate the mean weighted LE score
dataset_full$unweighted_LE_mean <- rowMeans(dataset_full[, le_unweighted], na.rm = T)  # and mean unweighted LE score

######################################
###       REMOVE SIBLINGS          ###
######################################
# remove siblings and second born twins
dataset_full <- dataset_full %>% filter(sibling %in% c(0, NA)) # removes siblings but keeps NA (15,090)
dataset_full <- dataset_full %>% filter(twin == "A  ") # only keeps first born twin (14,897)
# 15,645 individuals in raw data and 14,897 after removing siblings and twins

######################################
###       SUBSET ON SDQ            ###
######################################
# only keep individuals that have the outcome available
dataset_full_sub <- dataset_full %>% filter(!is.na(emot_symp_16y)) # 5,356 individuals with outcome 

#  calculate percent missing in LE items 
dataset_full_sub$percent_missing_le_unweighted <- rowSums(is.na(dataset_full_sub[, le_unweighted])) / length(le_unweighted) * 100
dataset_full_sub$percent_missing_le_weighted <- rowSums(is.na(dataset_full_sub[, le_weighted])) / length(le_weighted) * 100
# remove individuals with > 50% missingness in unweighted LE
dataset_miss <- dataset_full_sub %>%  filter(percent_missing_le_unweighted < 50) # 3,926 individuals with under 50% missingness in LE items

######################################
###       IMPOSSIBLE VALUES        ###
######################################

# PART 1: change impossible values
# Identify rows with only NAs or 0s
only_na_0_rows <- apply(dataset_miss[, le_unweighted], 1, function(row) all(is.na(row) | row == 0))
data_subset_na_0_LE <- dataset_miss[only_na_0_rows,]

# Find rows with values outside of NA and 0
outside_values_rows <- apply(data_subset_na_0_LE[, le_weighted], 1, function(row) any(!is.na(row) & row != 0))

# Print the rows with values outside of NA and 0
data_subset_na_0_LE[outside_values_rows, ] 
# there are 5 individuals with unweighted_LE_mean = 0 but with > 0 score in weighted_LE_mean 

# set weighted_LE_mean for these 5 individuals to 0 (because according to unweighted_LE_mean, LE were not experienced) 
IDs_5_individuals <- data_subset_na_0_LE[outside_values_rows, ]$cidB2957
dataset_miss[dataset_miss$cidB2957 %in% IDs_5_individuals, "weighted_LE_mean"] <- 0

# check if weighted_LE_mean for the 5 individuals is now 0 
dataset_miss[dataset_miss$cidB2957 %in% IDs_5_individuals ,  c('cidB2957', 'weighted_LE_mean', 'unweighted_LE_mean')] # yes, it is

# PART 2: remove individuals with impossible values
# there are 54 individuals that had weighted_LE_mean = 0 (0 = none of the events took place) but unweighted_LE_mean was > 0 (which means some events did happen)
subset_54 <- dataset_miss %>% filter(weighted_LE_mean == 0 & unweighted_LE_mean != 0 )
IDs_to_remove <- subset_54$cidB2957
dataset_clean <- dataset_miss %>% filter(!cidB2957 %in% IDs_to_remove)
dim(dataset_clean) # 3,872 individuals remain (was 3,926)


######################################
###            ADD PRS             ###
######################################

# # scale variables
# dataset_clean[, c('emot_symp_16y_z', 'smfq_17.5y_z',  'weighted_LE_mean_z', 
#                  'unweighted_LE_mean_z', 'smfq_age_17.5y_z', 
#                  'emot_symp_age_16y_z')] <- apply(dataset_clean[, c('emot_symp_16y', 
#                                                                    'smfq_17.5y',
#                                                                    'weighted_LE_mean',
#                                                                    'unweighted_LE_mean',
#                                                                    'smfq_age_17.5y',
#                                                                    'emot_symp_age_16y')],2, scale)

# convert sex, mum_uni and ethnicity to factor                                                                                    
dataset_clean$sex <- as.factor(dataset_clean$sex)
dataset_clean$ethnicity <- as.factor(dataset_clean$ethnicity)
dataset_clean$mum_uni <- as.factor(dataset_clean$mum_uni)

# read in PRS for neuroticism (calculated using SDQ outcome)
prs <- read.table(paste0(output_path, "prs.neuroticism.SDQoutcome.best"), header=T)
dataset_clean_prs = dataset_clean 
dataset_clean_prs$IID <- paste0(dataset_clean$cidB2957, dataset_clean$twin) 
dataset_clean_prs$IID <- gsub("  ", "", dataset_clean_prs$IID) # remove spaces 
dataset_clean_prs <- merge(dataset_clean_prs, prs, by = "IID") # 2,914 with PRS
dataset_clean_prs$PRS_z <- scale(dataset_clean_prs$PRS) # standardise

# dataset version for 3,872 individuals without PRS = dataset_clean
# dataset version for 2,874 individuals with PRS = dataset_clean_prs


######################################
###       SAVE DATASET            ###
######################################
# save 
saveRDS(dataset_clean, file = paste0(output_path, "dataset_clean_LE_", Sys.Date(), ".rds"))
saveRDS(dataset_clean_prs, file = paste0(output_path, "dataset_withPRS_LE_", Sys.Date(), ".rds"))


########### end script ################

