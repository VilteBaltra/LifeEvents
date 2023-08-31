
######################################
###             SET UP             ###
######################################
# load libraries
require(nnet)
library(mice)
library(openxlsx)

# define data path
data_path = "/Volumes/Files/Psychology/ResearchProjects/Ewalton/EarlyCause/WP4/LifeEvents/neuroticism-2023-03-30/"

# read in imputed data (obtained with '4.life-events-interaction.R' script)
full_imp <- readRDS(file.path(data_path,'LE_imputation_list_with_z_prs_stress_groups.rds'))

######################################
##          SUBGROUP ANALYSES       ##
######################################

# function to model output and save as an excel file
my_table <- function(fit) {
  model <- summary(fit) # extract estimates
  model$sign <- ifelse(model$p.value < 0.05, '*', '') # add a column to highlight significant terms
  model$`95% CI` <- paste0(round((model$estimate - 1.96*model$std.error), 2), ", ", round((model$estimate + 1.96*model$std.error), 2))
  model$`r.squared` <- c(pool.r.squared(fit)[1], rep(NA, nrow(model)-1)) # add a column for R2
  model$`adj.r.squared` <- c(pool.r.squared(fit, adjusted = TRUE)[1], rep(NA, nrow(model)-1)) # adjusted R2
  return(model)
}

# regresion function (full model + covars only)
subgroup.models <- function(data){
  m1 <- with(data, lm( emot_symp_16y ~ unweighted_LE_sum + emot_symp_age_16y + sex + ethnicity + mum_uni)) 
  m2 <- with(data, lm( emot_symp_16y ~ emot_symp_age_16y + sex + ethnicity + mum_uni)) 
  fit_pool1 <- mice::pool(m1)
  fit_pool2 <- mice::pool(m2)
  out1 <- my_table(fit_pool1)
  out2 <- my_table(fit_pool2)
  return(list("full_model" = out1, "covars_model" = out2))
}

# Define low, typical, and high reactivity subgroups
full_imp$data$stress_group_halfsd <- as.numeric(full_imp$data$stress_group_halfsd) # convert to numeric (-1,0,1 changes to 1,2,3 respectively)
full_imp$data$stress_group_onesd <- as.numeric(full_imp$data$stress_group_onesd) # convert to numeric (-1,0,1 changes to 1,2,3 respectively)
imp_long <- complete(full_imp, "long", include = TRUE) # convert to long dataset
imp_long$stress_group_halfsd_mean <- NA 
IDs <- unique(imp_long$cidB2957)
for(i in IDs) { # for each individual obtain the mean stress_group_halfsd value across all imputed datasets 
  individual_values <- imp_long[imp_long$cidB2957 == i, c(".imp", "cidB2957", "stress_group_halfsd")]
  imp_long$stress_group_halfsd_mean[imp_long$cidB2957 == i] <- round(mean(individual_values$stress_group_halfsd, na.rm=T),0)
}
# convert back to mids
full_imp <- as.mids(imp_long)
summary(as.factor(full_imp$data$stress_group_halfsd_mean)) # check categories

# subset into three separate mids objects
low_data <- miceadds::subset_datlist(full_imp, subset = full_imp$data$stress_group_halfsd_mean == 1,  toclass = 'mids') # low
typ_data <- miceadds::subset_datlist(full_imp, subset = full_imp$data$stress_group_halfsd_mean == 2,  toclass = 'mids') # low
high_data <- miceadds::subset_datlist(full_imp, subset = full_imp$data$stress_group_halfsd_mean == 3,  toclass = 'mids') # low

# run regression analysis in each subgroup (full model + covars only model)
high_models <- subgroup.models(high_data)
typical_models <- subgroup.models(typ_data)
low_models <- subgroup.models(low_data)

openxlsx::write.xlsx(list("high_full" = high_models[[1]], "high_covars" = high_models[[2]],
                          "typical_full" = typical_models[[1]], "typical_covars" = typical_models[[2]],
                          "low_full" = low_models[[1]], "low_covars" = low_models[[2]]), file = paste0("stress-subgroup-results-", Sys.Date(),'.xlsx'), rowNames = F, colNames = T, overwrite=T)

## SEX SUBGROUPS ## 

subrgoup.analysis <- function(data, sex){ # I'm using 30th dataset here as only 1 value is missing (and it has been more often classified as a male)
  
  mids <- miceadds::subset_datlist(data, subset = complete(data, 30)$sex == sex,  toclass = 'mids') # males
  model <- with(mids, lm( emot_symp_16y ~ unweighted_LE_sum + emot_symp_age_16y + ethnicity + mum_uni)) 
  fit_pool <- mice::pool(model)
  out <- my_table(fit_pool)
  return(out)
}

# MALES
high_m <- subrgoup.analysis(data = high_data, sex = 1)
typical_m <- subrgoup.analysis(data = typ_data, sex = 1)
low_data$data$sex <- as.numeric(low_data$data$sex) # convert to numeric
low_m <- subrgoup.analysis(data = low_data, sex = 1) # warnings for this model because adj-R2 cannot be pooled (has a negative value) 

# FEMALE
high_f <- subrgoup.analysis(data = high_data, sex = 2)
typical_f <- subrgoup.analysis(data = typ_data, sex = 2)
low_f <- subrgoup.analysis(data = low_data, sex = 2)


## SAVE ALL SUBGROUP MODELS
all_models <- list("low_males" = low_m, "typical_males" = typical_m, "high_males" = high_m,
                   "low_females" = low_f, "typical_females" = typical_f, "high_females" = high_f)

openxlsx::write.xlsx(all_models, file = paste0("sex-subgroup-results-", Sys.Date(),'.xlsx'), rowNames = F, colnames = T, overwrite=T)


