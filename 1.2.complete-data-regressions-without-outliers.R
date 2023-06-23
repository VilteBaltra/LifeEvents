# LIFE EVENTS STUDY - REGRESSION WITHOUT OUTLIERS (SENSITIVITY ANALYSIS) 

######################################
###             SET UP             ###
######################################
# load libraries
library(foreign)
library(tidyverse)
library(openxlsx)

# define data path
data_path = "/Volumes/Files/Psychology/ResearchProjects/Ewalton/EarlyCause/WP4/LifeEvents/neuroticism-2023-03-30/"

# read in formatted data (obtained with '0.data-prep.R' script)
dataset_clean <- readRDS(paste0(data_path, 'dataset_clean_LE_2023-06-13.rds')) # read in ALSPAC data with selected variables
dataset_clean_prs <- readRDS(paste0(data_path, "dataset_withPRS_LE_2023-06-13.rds" )) # read in ALSPAC data with selected variables
dim(dataset_clean) # 3872    95
dim(dataset_clean_prs) # 2874   100

######################################
### SENSITIVITY ANLAYSIS: OUTLIERS ###
######################################
# REMOVE OUTLIERS (with Cook’s distance greater than 3x the mean) AND RERUN REGRESSIONS

# WEIGHTED - no covars
m1_before <- lm(emot_symp_16y~weighted_LE_mean, data = dataset_clean)
summary(m1_before)

cooks_distance <- cooks.distance(m1_before)
influential_rows <- names(cooks_distance[(cooks_distance > (3 * mean(cooks_distance, na.rm = TRUE)))])

data_remove <- dataset_clean[influential_rows,]
dataset_clean_no_outliers <- dataset_clean %>% anti_join(data_remove)

m1_after <- lm(emot_symp_16y~weighted_LE_mean, data = dataset_clean_no_outliers)
summary(m1_after)


# WEIGHTED - with covars
m2_before <- lm(emot_symp_16y~weighted_LE_mean + emot_symp_age_16y + sex + ethnicity + mum_uni, data = dataset_clean)
summary(m2_before)

cooks_distance <- cooks.distance(m2_before)
influential_rows <- names(cooks_distance[(cooks_distance > (3 * mean(cooks_distance, na.rm = TRUE)))])

data_remove <- dataset_clean[influential_rows,]
dataset_clean_no_outliers <- dataset_clean %>% anti_join(data_remove)

m2_after <- lm(emot_symp_16y~weighted_LE_mean + emot_symp_age_16y + sex + ethnicity + mum_uni, data = dataset_clean_no_outliers)
summary(m2_after)

# WEIGHTED - with covars and PRS
m3_before <- lm(emot_symp_16y~weighted_LE_mean + emot_symp_age_16y + sex + ethnicity + mum_uni + PRS, data = dataset_clean_prs)
summary(m3_before)

cooks_distance <- cooks.distance(m3_before)
influential_rows <- names(cooks_distance[(cooks_distance > (3 * mean(cooks_distance, na.rm = TRUE)))])

data_remove <- dataset_clean_prs[influential_rows,]
dataset_clean_no_outliers <- dataset_clean_prs %>% anti_join(data_remove)

m3_after <- lm(emot_symp_16y~weighted_LE_mean + emot_symp_age_16y + sex + ethnicity + mum_uni + PRS, data = dataset_clean_no_outliers)
summary(m3_after)


# UNWEIGHTED - no covars
m4_before <- lm(emot_symp_16y~unweighted_LE_mean, data = dataset_clean)
summary(m4_before)

cooks_distance <- cooks.distance(m4_before)
influential_rows <- names(cooks_distance[(cooks_distance > (3 * mean(cooks_distance, na.rm = TRUE)))])

data_remove <- dataset_clean[influential_rows,]
dataset_clean_no_outliers <- dataset_clean %>% anti_join(data_remove)

m4_after <- lm(emot_symp_16y~unweighted_LE_mean, data = dataset_clean_no_outliers)
summary(m4_after)

# UNWEIGHTED - with covars
m5_before <- lm(emot_symp_16y~unweighted_LE_mean + emot_symp_age_16y + sex + ethnicity + mum_uni, data = dataset_clean)
summary(m5_before)

cooks_distance <- cooks.distance(m5_before)
influential_rows <- names(cooks_distance[(cooks_distance > (3 * mean(cooks_distance, na.rm = TRUE)))])

data_remove <- dataset_clean[influential_rows,]
dataset_clean_no_outliers <- dataset_clean %>% anti_join(data_remove)

m5_after <- lm(emot_symp_16y~unweighted_LE_mean + emot_symp_age_16y + sex + ethnicity + mum_uni, data = dataset_clean_no_outliers)
summary(m5_after)

# UNWEIGHTED - with covars and PRS
m6_before <- lm(emot_symp_16y~unweighted_LE_mean + emot_symp_age_16y + sex + mum_uni + scale(PRS), data = dataset_clean_prs) # no ethnicity as above
summary(m6_before)

cooks_distance <- cooks.distance(m6_before)
influential_rows <- names(cooks_distance[(cooks_distance > (3 * mean(cooks_distance, na.rm = TRUE)))])

data_remove <- dataset_clean_prs[influential_rows,]
dataset_clean_no_outliers <- dataset_clean_prs %>% anti_join(data_remove)

m6_after <- lm(emot_symp_16y~unweighted_LE_mean + emot_symp_age_16y + sex + mum_uni + scale(PRS), data = dataset_clean_no_outliers) # no ethnicity as above
summary(m6_after)

# save models in a list 
output <- list(crude_model_weighted = m1_after, full_model_weighted = m2_after, full_model_prs_weighted = m3_after, 
               crude_model_unweighted = m4_after, full_model_unweighted = m5_after, full_model_prs_unweighted = m6_after)

# function to format table (add CIs and R2)
table_format <- function(model.fit){
  # format table
  table <- cbind(as.data.frame(summary(model.fit)$coefficient), as.data.frame(confint(model.fit)))
  table$`95% CI` <- paste0(round(table[, 5], 3), ", ", round(table[, 6], 3))
  table$r.squared <- summary(model.fit)$r.squared 
  table$adj.r.squared <- summary(model.fit)$adj.r.squared
  table <- table[, c(1,7,3:4,8:9)]
  return(table)
}

# apply above function to all models
output_formatted <- list() # create empty list
for(i in 1:length(output)) {
  model <- output[[i]]
  output_formatted[[i]] <- table_format(model)
  print(output_formatted)
}
names(output_formatted) <- names(output)

# save model output without outliers
openxlsx::write.xlsx(output_formatted, file = paste0("all_SDQ_models_without_outliers_", Sys.Date(),'.xlsx'), rowNames = T, overwrite=T)

########### end script ################
