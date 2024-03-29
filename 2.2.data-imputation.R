# LIFE EVENTS STUDY - IMPUTATION

######################################
###             SET UP             ###
######################################

# Load libraries
library(mice)
library(miceadds)
#install.packages("TAM")

# define data path
data_path = "path to data"

# read in formatted data (obtained with '0.data-prep.R' script)
dataset_clean <- readRDS(paste0(data_path, 'dataset_clean_LE_2023-12-13.rds')) # read in ALSPAC data with selected variables
dim(dataset_clean) # 4791  107

# add exposure variables (for now with NA)
# also add 'smfq_16y_sum_imp' with NA only (as will be imputed using passive imp based on individual items)
dataset_clean[, c('weighted_LE_sum', 'unweighted_LE_sum', 'weighted_LE_mean_imp', 'unweighted_LE_mean_imp', 'smfq_16y_sum_imp')] <- NA


######################################
###             ORGANISE           ###
######################################

# Organize variable names into categories 
# item names for weighted LE scores (23 in total)
le_weighted <- c("ccs2001", "ccs2011", "ccs2021", "ccs2031", "ccs2041","ccs2051", "ccs2061", "ccs2071", "ccs2081", "ccs2091", "ccs2101",
                 "ccs2111", "ccs2121", "ccs2131" ,"ccs2141", "ccs2151", "ccs2161", "ccs2171" ,"ccs2181", "ccs2191", "ccs2201" ,"ccs2211", 
                 "ccs2221")

# item names for unweighted LE scores (23 in total)
le_unweighted <- c("ccs2000", "ccs2010", "ccs2020" ,"ccs2030", "ccs2040", "ccs2050", "ccs2060", "ccs2070" ,"ccs2080" ,"ccs2090", "ccs2100",
                   "ccs2110", "ccs2120", "ccs2130", "ccs2140", "ccs2150", "ccs2160", "ccs2170", "ccs2180", "ccs2190", "ccs2200", "ccs2210",
                   "ccs2220")

exposures <- c('weighted_LE_sum', 'unweighted_LE_sum', 'weighted_LE_mean_imp', 'unweighted_LE_mean_imp')

# outcome items
smfq_16y_13items <- c('ccs4500','ccs4502', 'ccs4503', 'ccs4504', 'ccs4505', 'ccs4506', 'ccs4508', 'ccs4509', 'ccs4511', 'ccs4512', 'ccs4513', 'ccs4514', 'ccs4515')
outcome <- c('smfq_16y_sum', 'smfq_16y_sum_imp')
covariates <- c('ethnicity', 'sex', 'mum_uni', 'smfq_age_16y')

# variable that shouldn't be imputed
no_imputation <- c('percent_missing_le_unweighted', 'percent_missing_le_weighted', 'weighted_LE_mean', 'unweighted_LE_mean') 
# didn't include 'twin' and 'sibling' as already excluded twins + siblings, so that only one related individual per pair remains (twin = all A, sibling = all 0)

# add auxilary variables to help with imputation 
aux <- c('ACEscore_extended_0_16yrs', 'physical_illness_parent_0_16yrs',
         'physical_illness_child_0_16yrs', 'vlnc_btwn_chld_nd_prtnr_0_16yrs',
         'neighbourhood_0_16yrs', 'financial_difficulties_0_16yrs', 'social_class_0_16yrs', 'parental_separation_0_16yrs',
         'parent_convicted_offenc_0_16yrs', 'mentl_hlth_prblms_r_scd_0_16yrs',
         'parent_child_bond_0_16yrs', 'bullying_0_16yrs', 'emotional_neglect_0_16yrs',
         'emotional_abuse_0_16yrs', 'physical_abuse_0_16yrs',
         'mz028b',  # maternal age at intake (used for imputation) 
         'dw043', # maternal bmi
         'pub803',	# child's height (cm) at 16y
         'pub804', #	child's weight (kg) at 16y
         'a525',  # PRES marital status at 8w gest
         'a551', #  Crowding index at 8w gest
         'bestgest', # # gestational age at birth (used for imputation)
         'kz030', # gestational weight (used for imputation)
         'b032',  # parity (used for imputation)
         'fh6877', # any anxiety disorder
         'fh6870', # any adhd disorder
         'fh6872',  # any behavioural disorder
         'emot_symp_16y' # SDQ 
)

dataset_for_imp <- dataset_clean[, c('cidB2957', le_weighted, le_unweighted, exposures, smfq_16y_13items, outcome, covariates, no_imputation, aux)]

# select only numeric variables using subsetting
numeric_vars <- dataset_for_imp[, sapply(dataset_for_imp, is.numeric)]
cor_matrix <- as.data.frame(cor(numeric_vars, use = "complete.obs"))
# save correlation matrix 
pdf("cor-matrix.pdf", width = 14, height = 14)
pheatmap::pheatmap(cor_matrix, cluster_rows = F, cluster_cols = F)
dev.off()


######################################
###        IMPUTATION MODEL        ###
######################################

# empty run to specify default arguments
imp0 <- mice(dataset_for_imp, maxit = 0, defaultMethod = rep('pmm', ncol(dataset_for_imp)))
imp0$loggedEvents
# * PMM imputes a value randomly from a set of observed values whose predicted values 
#   are closest to the predicted value of the specified regression model. PMM has been 
#   said to perform quite well under circumstance where the categorical data is sparse 
#   (Van Buuren, 2018).

meth <- imp0$method

# We use passive imputation for the domain scores. This means that the indicator items  
# are imputed first, and then, using these complete items, mean domain scores are 
# derived by the formula specified below
meth["weighted_LE_sum"]<- "~I( ccs2001 + ccs2011 + ccs2021 + ccs2031 + ccs2041 + ccs2051 + ccs2061 + ccs2071 + ccs2081 + ccs2091 + ccs2101 + ccs2111 + ccs2121 + ccs2131 + ccs2141 + ccs2151 + ccs2161 + ccs2171 + ccs2181 + ccs2191 + ccs2201 + ccs2211 + ccs2221 )"
meth["unweighted_LE_sum"]<- "~I( ccs2000 + ccs2010 + ccs2020 + ccs2030 + ccs2040 + ccs2050 + ccs2060 + ccs2070 + ccs2080 + ccs2090 + ccs2100 + ccs2110 + ccs2120 + ccs2130 + ccs2140 + ccs2150 + ccs2160 + ccs2170 + ccs2180 + ccs2190 + ccs2200 + ccs2210 + ccs2220 )"
meth["weighted_LE_mean_imp"]<- "~I( (ccs2001 + ccs2011 + ccs2021 + ccs2031 + ccs2041 + ccs2051 + ccs2061 + ccs2071 + ccs2081 + ccs2091 + ccs2101 + ccs2111 + ccs2121 + ccs2131 + ccs2141 + ccs2151 + ccs2161 + ccs2171 + ccs2181 + ccs2191 + ccs2201 + ccs2211 + ccs2221) / 23 )"
meth["unweighted_LE_mean_imp"]<- "~I( (ccs2000 + ccs2010 + ccs2020 + ccs2030 + ccs2040 + ccs2050 + ccs2060 + ccs2070 + ccs2080 + ccs2090 + ccs2100 + ccs2110 + ccs2120 + ccs2130 + ccs2140 + ccs2150 + ccs2160 + ccs2170 + ccs2180 + ccs2190 + ccs2200 + ccs2210 + ccs2220) / 23 )"

# passive imputation also applied to outcome
meth["smfq_16y_sum_imp"]<- "~I( ccs4500 + ccs4502 + ccs4503 + ccs4504 + ccs4505 + ccs4506 + ccs4508 + ccs4509 + ccs4511 + ccs4512 + ccs4513 + ccs4514 + ccs4515 )"

# We are going to need a different set of predictors for the different variables we impute 
# so let's define them using the predictormatrix, that gives these instructions to mice
predictormatrix <- imp0$predictorMatrix

# Do not impute nor use these variables
predictormatrix[, c("cidB2957", no_imputation) ] <- 
  predictormatrix[c("cidB2957", no_imputation),] <- 0
# Do not impute the outcome (but it will be used to impute the covariates)
predictormatrix['smfq_16y_sum',] <- 0 # the imputed outcome is called 'smfq_16y_sum_imp', see below
# Do not impute the auxiliary variables 
predictormatrix[aux,] <- 0

# Calculate LE scores using passive imputation (so do not impute)
predictormatrix[c('weighted_LE_sum', 'unweighted_LE_sum', 'weighted_LE_mean_imp', 'unweighted_LE_mean_imp'), ] <- 0
# Do not use LE scores nor outcome as predictors in LE items
predictormatrix[c(le_weighted,# all items for weighted LE 
                  le_unweighted), # all items for unweighted LE 
                c('weighted_LE_sum', 'unweighted_LE_sum', 'weighted_LE_mean_imp', 'unweighted_LE_mean_imp', outcome)]  <- 0

# Impute outcome items
# calculate LE scores using passive imputation (so do not impute)
predictormatrix[c('smfq_16y_sum_imp'), ] <- 0
# items are imputed based on other smfq items, life event sum scores, auxiliary variables, and the covariates 
# so do not use individual LE items nor mean scores 
predictormatrix[smfq_16y_13items,
                c(le_weighted, le_unweighted, 'weighted_LE_mean_imp', 'unweighted_LE_mean_imp')] <- 0

# Do not use ccs2080 to impute ccs2081 as collinear (YP's close friend has died since the age of 12) 
# --> if event occured, all appraised as "Very unpleasant" 
predictormatrix[c('ccs2081'), c('ccs2080')]  <- 
  predictormatrix[c('ccs2080'), c('ccs2081')] <- 0
# Do not use ccs2060 to impute ccs2061 as collinear (YP's parent/brother/sister has died since the age of 12)
predictormatrix[c('ccs2061'), c('ccs2060')]  <- 
  predictormatrix[c('ccs2060'), c('ccs2061')] <- 0

# Do not use ccs2210 to impute sex as only applies to females (YP has become pregnant since the age of 12) 
predictormatrix[c('sex'), c('ccs2210')]  <- 
  predictormatrix[c('ccs2210'), c('sex')] <- 0
# Do not use ccs2211 to impute sex as also only applies to females (Effect of YP becoming pregnant since the age of 12 )
predictormatrix[c('sex'), c('ccs2211')]  <- 
  predictormatrix[c('ccs2211'), c('sex')] <- 0

# Make sure ccs2210 and ccs2211 for males is not imputed as males cannot get pregnant, so value should always be 0 (= did not happen)
dataset_males <- dataset_for_imp %>% filter(sex == 1)
summary(as.factor(dataset_males$ccs2210)) # 1,961 NAs but should be 0 (as males did not get pregnant)
# lets set male NAs to 0 for ccs2210 and ccs2211
ids <- (dataset_for_imp %>% filter(is.na(dataset_for_imp$ccs2210)) %>% filter(sex == 1))$cidB2957
dataset_for_imp$ccs2210[dataset_for_imp$cidB2957 %in% ids] <- 0 # set to 0
dataset_for_imp$ccs2211[dataset_for_imp$cidB2957 %in% ids] <- 0 # set to 0
# check again
dataset_males <- dataset_for_imp %>% filter(sex == 1)
summary(as.factor(dataset_males$ccs2210)) # now 1,961 zeros as expected
summary(as.factor(dataset_males$ccs2211)) # here too

# do not use weighted LE to impute unweighted LE (otherwise results in problems with convergence)
predictormatrix[le_unweighted, le_weighted] <- 0

# check the matrix 
pdf("predictor-matrix.pdf", width = 14, height = 14)
pheatmap::pheatmap(predictormatrix, cluster_rows = F, cluster_cols = F)
dev.off()

# visit the sequence
VisSeq <- imp0$visitSequence

######################################
###        RUN IMPUTATION          ###
######################################

# Run imputation with 60 iterations and 30 imputed datasets
imp <- mice(dataset_for_imp, 
            m = 30, # number of imputed datasets
            maxit = 60, # number of iterations
            seed = 2023, # set seed
            method = meth,
            visitSequence = VisSeq, 
            predictorMatrix = predictormatrix)

# check logged events: 
imp$loggedEvents # NULL

######################################
###       POST-PROCESSING          ###
######################################
# Ensure imputed values for weighted LE make sense
# i.e., if unweighted LE is zero (= event did not happen) and weighted LE is not 0 (i.e., appraisal present), add zero to weighted LE instead
all_imp <- complete(imp, action = "long", include = TRUE)
x=1991 # initiate variable count
for(item in le_unweighted){
  count= as.character(x + 10)
  item2 = paste0("ccs", count)
  cat("Unweighted LE item:", item, "\n")
  cat("Weighted LE item:", item2, "\n")
  print(sum(all_imp[, item] %in% 0 & !(all_imp[, item2] %in% 0)) / 31) # print how many non zero values are imputed per dataset
  all_imp[, item2] <- ifelse(all_imp[, item] %in% 0 & !(all_imp[, item2] %in% 0), 0, all_imp[, item2]) # if unweighted LE is zero and weighted LE is not 0, add zero to weighted LE
  print(sum(all_imp[, item] %in% 0 & !(all_imp[, item2] %in% 0)) / 31) # now should be zero
  print(sum(all_imp[, item2] %in% 0 & !(all_imp[, item] %in% 0)) / 31) # print how many non zero values are imputed per dataset in reverse direction
  all_imp[, item] <- ifelse(all_imp[, item2] %in% 0 & !(all_imp[, item] %in% 0), 0, all_imp[, item])
  print(sum(all_imp[, item2] %in% 0 & !(all_imp[, item] %in% 0)) / 31) # now should be zero
  x=x+10 # weighted LE items change by unit of 10
}

# now recalculate weighted_LE_sum and unweighted_LE_sum scores using the post-processed items
all_imp <- all_imp %>% mutate(weighted_LE_sum = ( ccs2001 + ccs2011 + ccs2021 + ccs2031 + ccs2041 + ccs2051 + ccs2061 + ccs2071 + ccs2081 + ccs2091 + ccs2101 + ccs2111 + ccs2121 + ccs2131 + ccs2141 + ccs2151 + ccs2161 + ccs2171 + ccs2181 + ccs2191 + ccs2201 + ccs2211 + ccs2221) )
all_imp <- all_imp %>% mutate(unweighted_LE_sum = ( ccs2000 + ccs2010 + ccs2020 + ccs2030 + ccs2040 + ccs2050 + ccs2060 + ccs2070 + ccs2080 + ccs2090 + ccs2100 + ccs2110 + ccs2120 + ccs2130 + ccs2140 + ccs2150 + ccs2160 + ccs2170 + ccs2180 + ccs2190 + ccs2200 + ccs2210 + ccs2220 ) )

# plot check
plot(all_imp$unweighted_LE_sum, all_imp$weighted_LE_sum)
cor(all_imp$unweighted_LE_sum,all_imp$weighted_LE_sum, use='complete.obs')

# Convert back to mids
imp2 <- as.mids(all_imp)

# Convert mids to datlist
imp_datlist <- miceadds::mids2datlist(imp2)

# Standardize variables
sdatlist <- miceadds::scale_datlist(imp_datlist, orig_var = c('weighted_LE_sum', 'unweighted_LE_sum', 'weighted_LE_mean_imp',
                                                              'unweighted_LE_mean_imp', 'smfq_16y_sum', 'smfq_age_16y'), 
                                    trafo_var = paste0( c('weighted_LE_sum', 'unweighted_LE_sum', 'weighted_LE_mean_imp',
                                                          'unweighted_LE_mean_imp', 'smfq_16y_sum', 'smfq_age_16y'), "_z") )

# Reconvert back to mids object
imp_full <- miceadds::datlist2mids(sdatlist)


######################################
###             SAVE               ###
######################################

# save the list of imputed datasets (mids object)
saveRDS(imp, file.path(data_path,'LE_imputation_list.rds'))
saveRDS(imp_full, file.path(data_path,'LE_imputation_list_with_z.rds'))

# save the last imputed dataset
last_imputed <- complete(imp_full, 30) 
original_dataset <- complete(imp_full, 0) 
saveRDS(last_imputed,   file.path(data_path, 'LE_last_dataset_imputed30.rds'))
saveRDS(original_dataset, file.path(data_path, 'LE_original_dataset_imputed0.rds'))


########### end script ################





