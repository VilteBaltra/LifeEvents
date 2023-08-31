
######################################
###             SET UP             ###
######################################
# load libraries
require(nnet)
library(mice)
library(miceadds)
library(openxlsx)
library(psych)

# define data path
data_path = "/Volumes/Files/Psychology/ResearchProjects/Ewalton/EarlyCause/WP4/LifeEvents/neuroticism-2023-03-30/"

# read in imputed data (obtained with '4.life-events-interaction.R' script)
full_imp <- readRDS(file.path(data_path,'LE_imputation_list_with_z_prs_stress_groups.rds'))

######################################
##     DESCRIPTIVES BEFORE IMP      ##
######################################

# Extract the original set (with NAs)
original <- complete(full_imp, 0) 
original$PRS_z <- scale(original$PRS) # standardise PRS as standardised version is used in regression models

## BEFORE IMPUTATION
# continuous variables
# weighted_LE_mean and unweighted_LE_mean were calculated prior to imputation
cont_before <- psych::describe(original[, c('emot_symp_16y', 
                                     'weighted_LE_mean', 'unweighted_LE_mean', # do not include 'weighted_LE_sum', 'unweighted_LE_sum' as these were obtain after imp
                                     'emot_symp_age_16y', 'PRS_z',
                                     'percent_missing_le_unweighted', 'percent_missing_le_weighted') ])
print(cont_before)

# binary variables
binary_before <- t(sapply(original[, c("sex", "ethnicity", "mum_uni")], table))
cat_freq <- round(binary_before/rowSums(binary_before) * 100,2) # get percentages and round
table <- cbind(binary_before, cat_freq)

# initiate an empty data frame with relevant variables
table <- as.data.frame(rbind(Sex = c(NA, NA), '  Males' = c(NA, NA), '  Females' = c(NA, NA),
                            Ethnicity = c(NA, NA), '  non_White' = c(NA, NA), '  White' = c(NA, NA),
                            'Maternal education' = c(NA, NA), '  Degree' = c(NA, NA), '  No degree' = c(NA, NA)) ) 
names(table) <- c("n", "%")

# format table
table[2:3, 1] <- table(original$sex) # add n for males and females
table[5:6, 1] <- table(original$ethnicity) # add n non-white and white
table[8:9, 1] <- table(original$mum_uni) # add n for degree and no degree
table[2:3, 2] <- round(table(original$sex)/sum(table(original$sex)) * 100,2) # add % of males and females
table[5:6, 2] <- round(table(original$ethnicity)/sum(table(original$ethnicity)) * 100,2) # % of non-white and white
table[8:9, 2] <- round(table(original$mum_uni)/sum(table(original$mum_uni)) * 100,2) # % of degree and no degree


# categorical variables
cat_before <- as.data.frame(sapply(original[, c("stress_group_halfsd", "stress_group_onesd")], table))
# initiate empty df with same names
table2 <- as.data.frame(rbind('Stress groups (half SD)' = c(NA, NA), '  -1' = c(NA, NA), '  0' = c(NA, NA), '  1' = c(NA, NA),
                             'Stress groups (one SD)' = c(NA, NA), '  -1grp' = c(NA, NA), '  0grp' = c(NA, NA), '  1grp' = c(NA, NA)))
names(table2) <- c("n", "%")

# combine tables
categorical_table <- rbind(table, table2)
categorical_table[c("  -1", "  0", "  1" ), 1] <- cat_before$stress_group_halfsd # add values for half sd stress groups
categorical_table[c("  -1", "  0", "  1" ), 2] <- round(cat_before$stress_group_halfsd/sum(cat_before$stress_group_halfsd) * 100,2)   # add %
categorical_table[c("  -1grp", "  0grp", "  1grp" ), 1] <- cat_before$stress_group_onesd # add values for one sd stress groups
categorical_table[c("  -1grp", "  0grp", "  1grp" ), 2] <- round(cat_before$stress_group_onesd/sum(cat_before$stress_group_onesd) * 100,2)   # add %
print(categorical_table)

# save output
openxlsx::write.xlsx(list("before imp categorical" = categorical_table, "before imp continuous" = cont_before), file = paste0("descriptives-before-imputation-", Sys.Date(),'.xlsx'), rowNames = T, overwrite=T)

######################################
##     DESCRIPTIVES AFTER IMP      ##
######################################

# Convert mids to datlist
imp_datlist <- miceadds::mids2datlist(full_imp)

# Standardize PRS 
sdatlist <- miceadds::scale_datlist(imp_datlist, orig_var = 'PRS', 
                                    trafo_var = paste0( 'PRS', "_z") )

# Reconvert back to mids object
full_imp <- miceadds::datlist2mids(sdatlist)

# Stack imputed datasets in long format, excluding the original data
impdat <- complete(full_imp, action="long", include = FALSE)

# Pool descriptives
# continuous vars
cont <- impdat[, c('.imp', '.id', 'emot_symp_16y', 
                   'weighted_LE_sum', 'unweighted_LE_sum', 'weighted_LE_mean_imp', 'unweighted_LE_mean_imp',
                   'emot_symp_age_16y', # no need for 'PRS_z' as was not imputed 
                   'percent_missing_le_unweighted', 'percent_missing_le_weighted')]
cont_summary_list <- with(cont, by(cont, .imp, function(x) psych::describe(x[, -c(1, 2)]))) # exclude .imp and .id cols
cont_summary_pool <- round(Reduce("+",cont_summary_list)/length(cont_summary_list),2)

# binary vars
binary <- impdat[, c(".imp", "sex", "ethnicity", "mum_uni")] #  "stress_group_halfsd", "stress_group_onesd"
binary_summary <- with(binary, by(binary, .imp, function(x) sapply(x[, -c(1)], table))) # exclude .imp
out_bin_n <- as.data.frame(round(Reduce("+",binary_summary)/length(binary_summary),2))

# categorical vars 
cate <- impdat[, c(".imp", "stress_group_halfsd", "stress_group_onesd")] # because stress subgroups were defined using 30th dataset, stress_group_halfsd descriptives sligthly differ here from what is presented in descriptive table

for(i in unique(impdat$cidB2957)) { # for each individual obtain the mean stress_group_halfsd value across all imputed datasets 
  individual_values <- impdat[impdat$cidB2957 == i, c(".imp", "cidB2957", "stress_group_halfsd", "stress_group_onesd")]
  impdat$stress_group_halfsd_mean[impdat$cidB2957 == i] <- round(mean(as.numeric(individual_values$stress_group_halfsd), na.rm=T),0)
  impdat$stress_group_onesd_mean[impdat$cidB2957 == i] <- round(mean(as.numeric(individual_values$stress_group_onesd), na.rm=T),0)
}
# store as a dataframe
out_cat_n <- as.data.frame(cbind(stress_group_halfsd = table(impdat$stress_group_halfsd_mean) / 30, # divide by number of imputed datsets
                           stress_group_onesd = table(impdat$stress_group_onesd_mean) / 30))
rownames(out_cat_n) <- c("-1", "0",  "1") # name rows

# format table after imp
categorical_table2 <- categorical_table
categorical_table2[!is.na(categorical_table2)] <- NA # clear df 
categorical_table2[2:3, 1] <- out_bin_n$sex # add n for males and females
categorical_table2[2:3, 2] <- round(out_bin_n$sex/sum(out_bin_n$sex) * 100,2) # add % for males and females
categorical_table2[5:6, 1] <- out_bin_n$ethnicity # add n non-white and white
categorical_table2[5:6, 2] <- round(out_bin_n$ethnicity/sum(out_bin_n$ethnicity)* 100,2) # add % 
categorical_table2[8:9, 1] <- out_bin_n$mum_uni # add n for degree and no degree
categorical_table2[8:9, 2] <- round(out_bin_n$mum_uni/sum(out_bin_n$mum_uni)* 100,2) # % of degree and no degree
categorical_table2[c("  -1", "  0", "  1" ), 1] <- out_cat_n$stress_group_halfsd # add values for half sd stress groups
categorical_table2[c("  -1", "  0", "  1" ), 2] <- round(out_cat_n$stress_group_halfsd/sum(out_cat_n$stress_group_halfsd) * 100,2) # add %
categorical_table2[c("  -1grp", "  0grp", "  1grp" ), 1] <- out_cat_n$stress_group_onesd # add values for one sd stress groups
categorical_table2[c("  -1grp", "  0grp", "  1grp" ), 2] <- round(out_cat_n$stress_group_onesd/sum(out_cat_n$stress_group_onesd) * 100,2)   # add %

# save output 
openxlsx::write.xlsx(list("after imp categorical" = categorical_table2, "after imp continuous" = cont_summary_pool), file = paste0("descriptives-after-imputation-", Sys.Date(),'.xlsx'), rowNames = T, overwrite=T)

# number of unweighted LE per stress group
impdat %>% dplyr::group_by(stress_group_halfsd_mean) %>% dplyr::summarize(mean_unweighted_LE_sum = mean(unweighted_LE_sum), sd_unweighted_LE_sum = sd(unweighted_LE_sum))
ggplot(impdat, aes(x = as.factor(stress_group_halfsd_mean), y = unweighted_LE_sum)) +
  geom_boxplot() +
  labs(x = "Stress Reactivity", y = "Number of Life Events", title = "Box Plot of Number of Life Events by Stress Reactivity")

######################################
##          CORRELATIONS            ##
######################################

# correlations using imputed dataset
vars <- c('emot_symp_16y', 'weighted_LE_sum', 'unweighted_LE_sum', 'emot_symp_age_16y', 'PRS')
imp_correlations <- miceadds::micombine.cor(mi.res=full_imp, variables = vars, method = "spearman")

# initiate empty df and select only r column 
test <- list()
for(name in vars){
  test[[name]] <- imp_correlations %>% filter(variable1 == name) %>% select(variable2, r)
  colnames(test[[name]]) <- c("NA", name)
}

# merge all r columns into a correlation matrix
s1 <- merge(test[[1]], test[[2]], by = "NA", all = TRUE)
s2 <- merge(s1, test[[3]], by = "NA", all = TRUE)
s3 <- merge(s2, test[[4]], by = "NA", all = TRUE)
s4 <- merge(s3, test[[5]], by = "NA", all = TRUE)
final <- s4 %>% select(c('NA', s4$'NA')) # order rows and columns to be the same
final[, 2:ncol(final)] <- apply(final[, 2:ncol(final)], 2, function(column) as.numeric(column, 2)) # convert to numeric
final_rounded <- apply(final[, 2:ncol(final)], 2, function(column) round(column, 3)) # round
diag(final_rounded) <- 1 # set diagonal to 1
final_rounded[upper.tri(final_rounded, diag=FALSE)] <- NA # set top triangle to NA
rownames(final_rounded) <- final[,1] # final tidy corr matrix

# save as above but for p values
# initiate empty df and select only p column 
test2 <- list()
for(name in vars){
  test2[[name]] <- imp_correlations %>% filter(variable1 == name) %>% select(variable2, p)
  colnames(test2[[name]]) <- c("NA", name)
}

# merge all r columns into a correlation matrix
s1 <- merge(test2[[1]], test2[[2]], by = "NA", all = TRUE)
s2 <- merge(s1, test2[[3]], by = "NA", all = TRUE)
s3 <- merge(s2, test2[[4]], by = "NA", all = TRUE)
s4 <- merge(s3, test2[[5]], by = "NA", all = TRUE)
final <- s4 %>% select(c('NA', s4$'NA')) # order rows and columns to be the same
final[, 2:ncol(final)] <- apply(final[, 2:ncol(final)], 2, function(column) as.numeric(column, 2)) # convert to numeric
pvalues_rounded <- apply(final[, 2:ncol(final)], 2, function(column) round(column, 3)) # round
diag(pvalues_rounded) <- 1 # set diagonal to 1
pvalues_rounded[upper.tri(pvalues_rounded, diag=FALSE)] <- NA # set top triangle to NA
rownames(pvalues_rounded) <- final[,1] # final tidy corr matrix

# save correlation matrix
openxlsx::write.xlsx(list("r" = as.data.frame(final_rounded), "p" = as.data.frame(pvalues_rounded)), file = paste0("correlation-matrix-after-imp-", Sys.Date(),'.xlsx'), rowNames = T, overwrite=T)

########### end script ################



