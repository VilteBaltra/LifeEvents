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
data_path = "add path to data"
output_path = "add path to output"

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

## SMFQ 16y
# first select SMFQ 16y items as total score not available 
# (17 SMFQ items present, although we will use 13, see comment in line 76)
smfq_16y_items <- c('ccs4500','ccs4501','ccs4502', 'ccs4503', 'ccs4504', 'ccs4505', 'ccs4506', 'ccs4507', 'ccs4508', 'ccs4509', 'ccs4510', 'ccs4511', 'ccs4512', 'ccs4513', 'ccs4514', 'ccs4515','ccs4516')
summary(raw_data[, smfq_16y_items])
# each item is rated on a 3-point scale (3 = not true, 2 = sometimes true, 1 = true)
# total score ranges from 0 to 26. Higher scale scores = greater depressive symptoms

# recode so that 0 = not true, 1 = sometimes true, 2 = true
for(item in smfq_16y_items){
  print(item)
  cat("before:\n")
  print(summary(as.factor(raw_data[, item])))
  raw_data[, item] <- raw_data[, item] - 3 # recode scale
  raw_data[, item] <- abs(raw_data[, item])
  cat("after:\n")
  print(summary(as.factor(raw_data[, item])))
}
summary(raw_data[, smfq_16y_items])

# although 16 items are available in ALSPAC, here we select 13, as only 13 items seem to be used for the Short Mood and Feelings Questionnaire in the literature
smfq_16y_13items <- c('ccs4500','ccs4502', 'ccs4503', 'ccs4504', 'ccs4505', 'ccs4506', 'ccs4508', 'ccs4509', 'ccs4511', 'ccs4512', 'ccs4513', 'ccs4514', 'ccs4515')
raw_data$smfq_16y_sum <- rowSums(raw_data[, smfq_16y_13items])

# age of study child at completion 
raw_data$smfq_age_16y <- raw_data$ccs9991a / 12 

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
         'fh6878', # generalised anxiety disorder 15y 6m
         'fh6877', # any anxiety disorder
         'fh6870', # any adhd disorder
         'fh6872',  # any behavioural disorder
         'emot_symp_16y',
         'emot_symp_age_16y'
         )


# subset relevant variables
outcome <- c('smfq_16y_sum')
covariates <- c('ethnicity', 'sex', 'mum_uni', 'smfq_age_16y')
other <- c('sibling', 'twin')
dataset_full <- raw_data[, c('cidB2957', outcome, covariates, other, smfq_16y_13items, le_weighted, le_unweighted, aux)]
# nrow = 15645 

######################################
### RECODE and CALCULATE LE MEAN   ###
######################################
# recode weighted response categories so that smaller numbers indicate lower stress
# currently in unweighted LE 1 = yes, 2 = no. In weighted LE, 1 = Very unpleasant and 5 = Very pleasant 
# we want to reverse the coding in weighted LE + add make 0 = did not happen

# Step 1: recode all unweighted LE items
# so that 0 = no, 1 = yes 
dataset_full[,le_unweighted] <- abs(dataset_full[, le_unweighted] - 2) 
# Step 2: recode all weighted LE items
# original coding: 1=Very unpleasant,2=A bit unpleasant, 3=No effect, 4=A bit pleasant, 5=Very pleasant
# new coding: 3=Very unpleasant,2=A bit unpleasant, 1=A bit pleasant/Very pleasant/No effect, 0=Did not happen  
dataset_full[, le_weighted] <- dataset_full[, le_weighted] - 4 # this will create -3=Very unpleasant,-2=A bit unpleasant, -1=No effect, 0=A bit pleasant, 1=Very pleasant
# now also collapse 0 and 1 into -1 
for(item in le_weighted) {
  dataset_full[, item] <- ifelse(dataset_full[, item] %in% c(0,1), -1, dataset_full[, item]) # if item is 0 or 1 conert to -1, otherwise keep as is
}
# now make values positive
dataset_full[, le_weighted] <- abs(dataset_full[, le_weighted]) 

# add 0 = did not happen (to weighted LE items)
x=1991 # initialize number
overview <- list() # and empty list (to check output)
# print hist output to pdf
pdf("item_before_after_plots.pdf")
# set up layout 
par(mfrow = c(3, 3))
for(item in le_unweighted) {
  count= as.character(x + 10)
  item2 = paste0("ccs", count)
  cat("Unweighted LE item:", item, "\n")
  cat("Weighted LE item:", item2, "\n")
  # plot histogram of unweighted LE
  hist(dataset_full[, item], main = paste("Unweighted: ", item), xlab = item)
  # plot the before plot (histogram of weighted LE)
  hist(dataset_full[, item2], main = paste("Weighted LE before: ", item2), xlab = item2)
  # print summary of before items
  overview[[item]] <- summary(as.factor(dataset_full[, item]))
  name1=paste0(item2, "_before")
  overview[[name1]] <- summary(as.factor(dataset_full[, item2]))
  dataset_full[, item2] <- ifelse(dataset_full[, item] == 0, 0, dataset_full[, item2]) # if unweighted LE did not happen, add 0 to weighted LE as well 
  # plot the after plot (histogram of weighted LE)
  hist(dataset_full[, item2], main = paste("Weighted LE after: ", item2), xlab = item2)
  name2=paste0(item2, "_after")
  overview[[name2]] <- summary(as.factor(dataset_full[, item2]))
  # print summary of after item
  x=x+10 # weighted LE items change by unit of 10
}
dev.off()
# print overview
print(overview)


# Step 3: calculate mean weighted and unweighted scores 
dataset_full$weighted_LE_mean <- rowMeans(dataset_full[, le_weighted], na.rm = T)  # first calculate the mean weighted LE score
dataset_full$unweighted_LE_mean <- rowMeans(dataset_full[, le_unweighted], na.rm = T)  # and mean unweighted LE score


######################################
###       SUBSET ON SMFQ           ###
######################################
# copy to sub dataset
dataset_full_sub <- dataset_full

# # filter based on 99% missingness
# dataset_full_sub$percent_missing_smfq <- rowSums(is.na(dataset_full_sub[, smfq_16y_13items])) / length(smfq_16y_13items) * 100
# dataset_full_sub <- dataset_full_sub %>% filter(percent_missing_smfq < 99) 
# # 5098 responded to at least 1 SMFQ item

# filter based on 50% missingness
dataset_full_sub$percent_missing_smfq <- rowSums(is.na(dataset_full_sub[, smfq_16y_13items])) / length(smfq_16y_13items) * 100
dataset_full_sub <- dataset_full_sub %>%  filter(percent_missing_smfq < 50) 
# 5095

######################################
###       SUBSET ON LIFE EVENTS    ###
######################################
#  calculate percent missing in LE items 
dataset_full_sub$percent_missing_le_unweighted <- rowSums(is.na(dataset_full_sub[, le_unweighted])) / length(le_unweighted) * 100
dataset_full_sub$percent_missing_le_weighted <- rowSums(is.na(dataset_full_sub[, le_weighted])) / length(le_weighted) * 100
# remove individuals with > 50% missingness in unweighted LE
dataset_full_sub <- dataset_full_sub %>%  filter(percent_missing_le_unweighted < 50) 
# 5,058 individuals with under 50% missingness in LE items


######################################
###       REMOVE SIBLINGS          ###
######################################
# remove siblings and second born twins
dataset_full_sub <- dataset_full_sub %>% filter(sibling %in% c(0, NA)) # removes siblings but keeps NA (4,831 left [107 siblings removed])
dataset_clean <- dataset_full_sub %>% filter(twin == "A  ") # only keeps first born twin (4,791 left [40 twins removed])
# 4,791 individuals left 

# convert sex, mum_uni and ethnicity to factor                                                                                    
dataset_clean$sex <- as.factor(dataset_clean$sex)
dataset_clean$ethnicity <- as.factor(dataset_clean$ethnicity)
dataset_clean$mum_uni <- as.factor(dataset_clean$mum_uni)

# read in PRS for neuroticism (calculated using SDQ outcome)
prs <- read.table(paste0(output_path, "prs.neuroticism.SMFQoutcome.best"), header=T)
dataset_clean_prs = dataset_clean 
dataset_clean_prs$IID <- paste0(dataset_clean$cidB2957, dataset_clean$twin) 
dataset_clean_prs$IID <- gsub("  ", "", dataset_clean_prs$IID) # remove spaces 
dataset_clean_prs <- merge(dataset_clean_prs, prs, by = "IID") # 3,378 with PRS
dataset_clean_prs$PRS_z <- scale(dataset_clean_prs$PRS) # standardise

# dataset version for 4,791 individuals without PRS = dataset_clean
# dataset version for 3,443 individuals with PRS = dataset_clean_prs


######################################
###       SAVE DATASET            ###
######################################
# save 
saveRDS(dataset_clean, file = paste0(output_path, "dataset_clean_LE_", Sys.Date(), ".rds"))
saveRDS(dataset_clean_prs, file = paste0(output_path, "dataset_withPRS_LE_", Sys.Date(), ".rds"))


########### end script ################

