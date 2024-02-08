# LIFE EVENTS STUDY - IMPUTATION QC

######################################
###             SET UP             ###
######################################
# load libraries
library(mice)

# define data path
data_path = "path to data"
output =  "path to output"

# Load the list of imputed datasets
imp <- readRDS(file.path(data_path,'LE_imputation_list.rds'))
original_set <- readRDS(file.path(data_path, 'LE_original_dataset_imputed0.rds'))

######################################
###         CONVERGENCE            ###
######################################

# EVALUATE THE CONVERGENCE OF THE IMPUTATION
# Extract the means of the imputed values for each iteration # imp$chainMean
# The number of chains is equal to the number of imputed datasets. A chain refers 
# to the chain of regression models that is used to generate the imputed values. 
# The length of each chain is equal to the number of iterations.

# visualise convergence
pdf(file.path(output, "covergence-plots-with-all-aux.pdf")) # convergence looks a little odd for unweighted LE items which have only a few values missing (means and SDs look okay though)
plot(imp)
dev.off()
# healthy convergence = variance within imputation chains should be equal to variance between chains  

######################################
###  IMPUTED VS. OBSERVED VALUES   ###
######################################

# Compare imputed and observed values
for (var in c(le_weighted, le_unweighted, covariates, smfq_16y_13items)) {
   cat(paste('densityplot(imp, ~', var, ')', '\n')) }

pdf(file.path(output, "imputed-vs-observed-with-all-aux.pdf"))
densityplot(imp, ~ ccs2001 ) 
densityplot(imp, ~ ccs2011 ) 
densityplot(imp, ~ ccs2021 ) 
densityplot(imp, ~ ccs2031 ) 
densityplot(imp, ~ ccs2041 ) 
densityplot(imp, ~ ccs2051 ) 
densityplot(imp, ~ ccs2061 ) 
densityplot(imp, ~ ccs2071 ) 
densityplot(imp, ~ ccs2081 ) 
densityplot(imp, ~ ccs2091 ) 
densityplot(imp, ~ ccs2101 ) 
densityplot(imp, ~ ccs2111 ) 
densityplot(imp, ~ ccs2121 ) 
densityplot(imp, ~ ccs2131 ) 
densityplot(imp, ~ ccs2141 ) 
densityplot(imp, ~ ccs2151 ) 
densityplot(imp, ~ ccs2161 ) 
densityplot(imp, ~ ccs2171 ) 
densityplot(imp, ~ ccs2181 ) 
densityplot(imp, ~ ccs2191 ) 
densityplot(imp, ~ ccs2201 ) 
densityplot(imp, ~ ccs2211 ) 
densityplot(imp, ~ ccs2221 ) 
densityplot(imp, ~ ccs2000 ) 
densityplot(imp, ~ ccs2010 ) 
densityplot(imp, ~ ccs2020 ) 
densityplot(imp, ~ ccs2030 ) 
densityplot(imp, ~ ccs2040 ) 
densityplot(imp, ~ ccs2050 ) 
densityplot(imp, ~ ccs2060 ) 
densityplot(imp, ~ ccs2070 ) 
densityplot(imp, ~ ccs2080 ) 
densityplot(imp, ~ ccs2090 ) 
densityplot(imp, ~ ccs2100 ) 
densityplot(imp, ~ ccs2110 ) 
densityplot(imp, ~ ccs2120 ) 
densityplot(imp, ~ ccs2130 ) 
densityplot(imp, ~ ccs2140 ) 
densityplot(imp, ~ ccs2150 ) 
densityplot(imp, ~ ccs2160 ) 
densityplot(imp, ~ ccs2170 ) 
densityplot(imp, ~ ccs2180 ) 
densityplot(imp, ~ ccs2190 ) 
densityplot(imp, ~ ccs2200 ) 
densityplot(imp, ~ ccs2210 ) 
densityplot(imp, ~ ccs2220 ) 
densityplot(imp, ~ ethnicity ) 
densityplot(imp, ~ sex ) 
densityplot(imp, ~ mum_uni ) 
densityplot(imp, ~ ccs4500 ) 
densityplot(imp, ~ ccs4502 ) 
densityplot(imp, ~ ccs4503 ) 
densityplot(imp, ~ ccs4504 ) 
densityplot(imp, ~ ccs4505 ) 
densityplot(imp, ~ ccs4506 ) 
densityplot(imp, ~ ccs4508 ) 
densityplot(imp, ~ ccs4509 ) 
densityplot(imp, ~ ccs4511 ) 
densityplot(imp, ~ ccs4512 ) 
densityplot(imp, ~ ccs4513 ) 
densityplot(imp, ~ ccs4514 ) 
densityplot(imp, ~ ccs4515 ) 
dev.off()

######################################
###.    STRIPPLOT AND XYPLOT       ###
######################################

pdf(file.path(output,"life-event-scores-all-aux.pdf"))
stripplot(imp, ~ ccs2001 ) 
stripplot(imp, ~ ccs2011 ) 
stripplot(imp, ~ ccs2021 ) 
stripplot(imp, ~ ccs2031 ) 
stripplot(imp, ~ ccs2041 ) 
stripplot(imp, ~ ccs2051 ) 
stripplot(imp, ~ ccs2061 ) 
stripplot(imp, ~ ccs2071 ) 
stripplot(imp, ~ ccs2081 ) 
stripplot(imp, ~ ccs2091 ) 
stripplot(imp, ~ ccs2101 ) 
stripplot(imp, ~ ccs2111 ) 
stripplot(imp, ~ ccs2121 ) 
stripplot(imp, ~ ccs2131 ) 
stripplot(imp, ~ ccs2141 ) 
stripplot(imp, ~ ccs2151 ) 
stripplot(imp, ~ ccs2161 ) 
stripplot(imp, ~ ccs2171 ) 
stripplot(imp, ~ ccs2181 ) 
stripplot(imp, ~ ccs2191 ) 
stripplot(imp, ~ ccs2201 ) 
stripplot(imp, ~ ccs2211 ) 
stripplot(imp, ~ ccs2221 ) 
stripplot(imp, ~ ccs2000 ) 
stripplot(imp, ~ ccs2010 ) 
stripplot(imp, ~ ccs2020 ) 
stripplot(imp, ~ ccs2030 ) 
stripplot(imp, ~ ccs2040 ) 
stripplot(imp, ~ ccs2050 ) 
stripplot(imp, ~ ccs2060 ) 
stripplot(imp, ~ ccs2070 ) 
stripplot(imp, ~ ccs2080 ) 
stripplot(imp, ~ ccs2090 ) 
stripplot(imp, ~ ccs2100 ) 
stripplot(imp, ~ ccs2110 ) 
stripplot(imp, ~ ccs2120 ) 
stripplot(imp, ~ ccs2130 ) 
stripplot(imp, ~ ccs2140 ) 
stripplot(imp, ~ ccs2150 ) 
stripplot(imp, ~ ccs2160 ) 
stripplot(imp, ~ ccs2170 ) 
stripplot(imp, ~ ccs2180 ) 
stripplot(imp, ~ ccs2190 ) 
stripplot(imp, ~ ccs2200 ) 
stripplot(imp, ~ ccs2210 ) 
stripplot(imp, ~ ccs2220 ) 
stripplot(imp, unweighted_LE_mean_imp ) 
stripplot(imp, weighted_LE_mean_imp ) 
stripplot(imp, unweighted_LE_sum ) 
stripplot(imp, weighted_LE_sum ) 
stripplot(imp, smfq_16y_sum ) 
stripplot(imp, smfq_age_16y ) 
miss <- is.na(original_set[, "unweighted_LE_sum" ])
xyplot(imp, unweighted_LE_sum ~I ( ccs2000 + ccs2010 + ccs2020 + ccs2030 + ccs2040 + ccs2050 + ccs2060 + ccs2070 + ccs2080 + ccs2090 + ccs2100 + ccs2110 + ccs2120 + ccs2130 + ccs2140 + ccs2150 + ccs2160 + ccs2170 + ccs2180 + ccs2190 + ccs2200 + ccs2210 + ccs2220 ),
       na.groups = miss, cex = c(1, 1), pch = c(1, 20), ylab = "Domain Imputed", xlab = "Domain Calculated", main = "unweighted_LE_sum") 
miss <- is.na(original_set[, "weighted_LE_sum" ])
xyplot(imp, weighted_LE_sum ~I (ccs2001 + ccs2011 + ccs2021 + ccs2031 + ccs2041 + ccs2051 + ccs2061 + ccs2071 + ccs2081 + ccs2091 + ccs2101 + ccs2111 + ccs2121 + ccs2131 + ccs2141 + ccs2151 + ccs2161 + ccs2171 + ccs2181 + ccs2191 + ccs2201 + ccs2211 + ccs2221),
       na.groups = miss, cex = c(1, 1), pch = c(1, 20), ylab = "Domain Imputed", xlab = "Domain Calculated", main = "weighted_LE_sum") 
miss <- is.na(original_set[, "unweighted_LE_mean_imp" ])
xyplot(imp, unweighted_LE_mean_imp ~I ( (ccs2000 + ccs2010 + ccs2020 + ccs2030 + ccs2040 + ccs2050 + ccs2060 + ccs2070 + ccs2080 + ccs2090 + ccs2100 + ccs2110 + ccs2120 + ccs2130 + ccs2140 + ccs2150 + ccs2160 + ccs2170 + ccs2180 + ccs2190 + ccs2200 + ccs2210 + ccs2220)/23 ),
       na.groups = miss, cex = c(1, 1), pch = c(1, 20), ylab = "Domain Imputed", xlab = "Domain Calculated", main = "unweighted_LE_mean_imp") 
miss <- is.na(original_set[, "weighted_LE_mean_imp" ])
xyplot(imp, weighted_LE_mean_imp ~I ( (ccs2001 + ccs2011 + ccs2021 + ccs2031 + ccs2041 + ccs2051 + ccs2061 + ccs2071 + ccs2081 + ccs2091 + ccs2101 + ccs2111 + ccs2121 + ccs2131 + ccs2141 + ccs2151 + ccs2161 + ccs2171 + ccs2181 + ccs2191 + ccs2201 + ccs2211 + ccs2221)/23 ),
       na.groups = miss, cex = c(1, 1), pch = c(1, 20), ylab = "Domain Imputed", xlab = "Domain Calculated", main = "weighted_LE_mean_imp") 
miss <- is.na(original_set[, "smfq_16y_sum_imp" ])
xyplot(imp, smfq_16y_sum_imp ~I ( ccs4500 + ccs4502 + ccs4503 + ccs4504 + ccs4505 + ccs4506 + ccs4508 + ccs4509 + ccs4511 + ccs4512 + ccs4513 + ccs4514 + ccs4515 ),
       na.groups = miss, cex = c(1, 1), pch = c(1, 20), ylab = "SMFQ Imputed", xlab = "SMFQ Calculated", main = "smfq_16y_sum_imp") 
dev.off()

########### end script ################






