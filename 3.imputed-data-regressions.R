# LIFE EVENTS STUDY - REGRESSION SCRIPT 

######################################
###             SET UP             ###
######################################
# load libraries
library(foreign)
library(tidyverse)
library(openxlsx)
library(mice)

# define data path
data_path = "path to data"

# read in imputed data (obtained with '2.2.data-imputation.R' script)
imp <- readRDS(paste0(data_path, 'LE_imputation_list_with_z.rds'))

######################################
###   DEFINE REGRESSION FUNCTION   ###
######################################

regr_model <- function(crude_fit, adjusted_fit, model_name) {

    # crude model 
    if(!is.null(crude_fit)){
      crude_fit_pool <- mice::pool(crude_fit) # pool estimates from each model 
    }
    
    # full model 
    adjusted_fit_pool <- mice::pool(adjusted_fit) # pool estimates from each model 
    
    my_table <- function(fit) {
      model <- summary(fit) # extract estimates
      model$sign <- ifelse(model$p.value < 0.05, '*', '') # add a column to highlight significant terms
      model$`95% CI` <- paste0(round((model$estimate - 1.96*model$std.error), 3), ", ", round((model$estimate + 1.96*model$std.error), 3))
      model$`r.squared` <- c(pool.r.squared(fit)[1], rep(NA, nrow(model)-1)) # add a column for R2
      model$`adj.r.squared` <- c(pool.r.squared(fit, adjusted = TRUE)[1], rep(NA, nrow(model)-1)) # adjusted R2
      return(model)
    }
    
    if(!is.null(crude_fit)){
      output <- rbind(my_table(crude_fit_pool), my_table(adjusted_fit_pool))
      #save model 
      openxlsx::write.xlsx(output, file = paste0(model_name, Sys.Date(),'.xlsx'), rowNames=F, overwrite=T)
      return(output)
      
    } else {
      output <- my_table(adjusted_fit_pool)
      #save model 
      openxlsx::write.xlsx(output, file = paste0(model_name, Sys.Date(),'.xlsx'), rowNames=F, overwrite=T)
      return(output)
    }
}


######################################
###     SMFQ: RUN REGRESSIONS       ###
######################################

# Running regressions (no standardised variables)

#### using sum LE ####
# same as above but using weighted_LE_sum instead of weighted_LE_mean_imp
# weighted crude and with covars
crude_weighted2 <- with(imp, lm(smfq_16y_sum_imp ~ weighted_LE_sum))
full_weighted2 <- with(imp, lm(smfq_16y_sum_imp ~ weighted_LE_sum + smfq_age_16y + sex + ethnicity + mum_uni))
# get tables and save output in local directory
regr_model(crude_fit=crude_weighted2, adjusted_fit=full_weighted2, model_name='sum_weighted_SMFQ_')

# unweighted crude and with covars
crude_unweighted2 <- with(imp, lm(smfq_16y_sum_imp ~ unweighted_LE_sum))
full_unweighted2 <- with(imp, lm(smfq_16y_sum_imp ~ unweighted_LE_sum + smfq_age_16y + sex + ethnicity + mum_uni))
# get tables and save output in local directory
regr_model(crude_fit=crude_unweighted2, adjusted_fit=full_unweighted2, model_name='sum_unweighted_SMFQ_')

# get R2 for covar only model
covars2 <- with(imp, lm(smfq_16y_sum_imp ~ smfq_age_16y + sex + ethnicity + mum_uni))
covars_fit_pool <- mice::pool(covars2)
# print incremental R2 for unweighted LE
my_table(full_unweighted2)$adj.r.squared[1] - my_table(covars_fit_pool)$adj.r.squared[1] # 0.04530603
# print incremental R2 for weighted LE
my_table(full_weighted2)$adj.r.squared[1] - my_table(covars_fit_pool)$adj.r.squared[1] #  0.06798576

######################################
###  ADD PRS TO IMPUTED DATA       ###
######################################

# read in PRS for neuroticism (calculated using SMFQ outcome)
prs <- read.table(paste0(data_path, "prs.neuroticism.SMFQoutcome.best"), header=T)
prs$cidB2957 <- gsub("A", "", prs$IID) # remove A from prs IDs to match imputed dataset

# # first obtain overlapping IDs 
# ids <- imp$data$cidB2957[which(imp$data$cidB2957 %in% prs$cidB2957)] # 3378 (same as in 0.data-prep.R)
# 
# # subset mids object to individuals with PRS
# imp_prs <- miceadds::subset_datlist(imp, subset = imp$data$cidB2957 %in% ids, toclass = 'mids')
# dim(imp_prs$data)
# # 3443   102

# transform the mids object into regular dataset using 
imp_long <- mice::complete(imp, "long", include = TRUE) 

# select only relevant colums from prs dataset and copy it 31 times to match the imputed dataset (which includes original + 30 imputed datasets)
prs_sub <-  prs[, c("cidB2957", "PRS")]

# adding prs to imp dataset (keeping all rows from imp_long)
imp_long$cidB2957 <- as.character(imp_long$cidB2957) # to match PRS
imp_ALL <- left_join(imp_long, prs_sub, by = "cidB2957") # using left_join instead of merge to keep same row order (required by mids)
any(duplicated(prs_sub$cidB2957)) # false

# convert back to mids
implist <- mice::as.mids(imp_ALL)

# check if PRS nr adds up
NAs_in_PRS <- sum(is.na(implist$data$PRS)) # 1348 NAs 
total_sample <- length(implist$data$PRS)  
cat("Total sample size: ", total_sample) # 4791
cat("Sample size with PRS: ", total_sample - NAs_in_PRS) # 3443 as expected


######################################
### SMFQ: RUN REGRESSIONS WITH PRS  ###
######################################
# run regressions for a subset of individuals with PRS (n = 3,378)
implist.prs <- miceadds::subset_datlist( implist, subset = !is.na(implist$data$PRS),  toclass = 'mids') 

#### using sum LE ####
# weighted
prs_full_weighted2 <- with(implist.prs, lm(smfq_16y_sum_imp ~ weighted_LE_sum + smfq_age_16y + sex + 
                                ethnicity + mum_uni + scale(PRS) ))

# get tables and save output in local directory
regr_model(crude_fit=NULL, adjusted_fit=prs_full_weighted2, model_name='prs_sum_weighted_SMFQ_')

# unweighted
prs_full_unweighted2 <- with(implist.prs, lm(smfq_16y_sum_imp ~ unweighted_LE_sum + smfq_age_16y + sex + 
                                        ethnicity + mum_uni + scale(PRS) ))
# get tables and save output in local directory
regr_model(crude_fit=NULL, adjusted_fit=prs_full_unweighted2, model_name='prs_sum_unweighted_SMFQ_')

######################################
### SAVE IMPUTED DATASET WITH PRS  ###
######################################

saveRDS(implist, file.path(data_path,'LE_imputation_list_with_z_prs.rds'))

########### end script ################
