
######################################
###             SET UP             ###
######################################
# load libraries
library(mice)
library(tidyverse)
library(openxlsx)

# define data path
data_path =  "path to data"

# read in imputed data (obtained with '3.imputed-data-regressions.R' script)
imp <- readRDS(file.path(data_path,'LE_imputation_list_with_z_prs.rds'))

######################################
###         OBTAIN RSIDUALS        ###
######################################

# convert mids object into list
datlist <- miceadds::mids2datlist(imp)

# obtain residuals and predicted values for each of the imputed datasets
for(i in 1:length(datlist)) {
  print(i)
  model <- lm(weighted_LE_sum ~ unweighted_LE_sum, data = datlist[[i]]) # specify model
  print(summary(model))
  set.seed(2023)
  datlist[[i]]$residuals <- residuals(model) # save the residual values
  datlist[[i]]$predicted <- predict(model) # save the predicted values
}

# define stress groups 
for(i in 1:length(datlist)) {
  # half SD = threshold
  datlist[[i]]$stress_group_halfsd <- as.factor(ifelse(datlist[[i]]$residuals > (sd(datlist[[i]]$residuals)/2), 1, # high emotional reactivity = 1
                                                        ifelse(datlist[[i]]$residuals < -(sd(datlist[[i]]$residuals)/2), -1,  # low emotional reactivity = -1
                                                               0)))
  # one SD = threshold
  datlist[[i]]$stress_group_onesd <- as.factor(ifelse(datlist[[i]]$residuals > sd(datlist[[i]]$residuals), 1, # high emotional reactivity = 1
                                                      ifelse(datlist[[i]]$residuals < -sd(datlist[[i]]$residuals), -1,  # low emotional reactivity = -1
                                                             0)))
}

# mean center predictor 
for(i in 1:length(datlist)) {
  # half SD = threshold(
  datlist[[i]]$unweighted_LE_sum_centered <- datlist[[i]]$unweighted_LE_sum - mean(datlist[[i]]$unweighted_LE_sum)
}

# convert back to mids object 
full_imp <- miceadds::datlist2mids( datlist )

######################################
###   EXPLORE DATA & ASSUMPTIONS   ###
######################################

# correlate unweighted LE with weighted LE using all imputed datasets
with(full_imp, cor(weighted_LE_sum, unweighted_LE_sum)) # 0.936

# histograms (all are right skewed)
hist(full_imp$data$weighted_LE_sum) 
hist(full_imp$data$unweighted_LE_sum)
hist(full_imp$data$smfq_16y_sum_imp) 

#assumption checks
original <- complete(full_imp, 0) # select original dataset 
model <- lm(weighted_LE_mean~unweighted_LE_mean, data = original)
pdf("life-events-assumption-checks.pdf")
print(summary(model))
par(mfrow=c(3,2))
hist(model$residuals, main="Histogram of Residuals")
plot(model)
plot(model, 4)
mtext("Weighted LE regressed on unweighted LE", outer=TRUE,  cex=1, line=-1.4, font=2, col='blue')
dev.off()

######################################
###        PLOT RESIDUALS          ###
######################################

## Plot all imputed datasets ##
# define stress groups based on half SD residuals
pdf("unweighted-weighted-figure-all-imputed-datasets.pdf")
for(i in 1:length(datlist)) {
p <- ggplot(datlist[[i]], aes(x = unweighted_LE_sum, y = weighted_LE_sum)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = unweighted_LE_sum, yend = predicted), alpha = .2) +
  # color adjustments
  geom_point(aes(color = stress_group_halfsd), size = 2, shape = 20) +  # Color mapped here
  #scale_color_gradient2(low = '#009999', high = "#CC0066") + # "#d7191c"
  scale_color_brewer(palette="Set2") +
  guides(color = 'none') +
  
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  + xlab("Unweighted life events score at 16 years (sum)") + ylab("Weighted life events score at 16 years (sum)")
print(p)
}
dev.off()

## Only plot 30th dataset for manuscript ##
dataset_30 <- complete(full_imp, 30)

p <- ggplot(dataset_30, aes(x = unweighted_LE_sum, y = weighted_LE_sum)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray", linetype = "solid") +
  geom_segment(aes(xend = unweighted_LE_sum, yend = predicted), alpha = 0.2) +
  #geom_point(aes(color = stress_group_halfsd), size = 2, shape = 20, alpha = 0.3) +
  #scale_color_manual(values = c('darkblue', "lightblue", "#FFC20A")) +
  scale_color_manual(values=c('darkblue', "lightblue", "#FFC20A"), name = "Stress reactivity", labels = c('Low', 'Typical', 'High')) + 
  geom_jitter(aes(colour = stress_group_halfsd),width = 0.0,height = 0, alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "top", panel.grid.major = element_line(color = "gray", linetype = "dashed"))
p + xlab("Unweighted life events (sum)") + ylab("Impact-weighted life event score") + theme(legend.position = "bottom")

# save
ggsave(paste0('residuals_plot_30th_imp_dataset_', Sys.Date(), ".png"), scale = 0.7)

# check number of individuals in each group
table(dataset_30$stress_group_halfsd) 
# -1    0    1 
# 1450 2243 1098 


####################################
##     REGR WITH INTERACTION      ## 
####################################

# stress group predicting SMFQ
model1 <- with(full_imp, lm(smfq_16y_sum_imp ~ stress_group_halfsd))
summary(pool(model1))
# unweighted LE predicting SMFQ
model2 <- with(full_imp, lm(smfq_16y_sum_imp ~ unweighted_LE_mean))
summary(pool(model2))

# function for model output 
my_table <- function(fit) {
  model <- summary(fit) # extract estimates
  model$sign <- ifelse(model$p.value < 0.05, '*', '') # add a column to highlight significant terms
  model$`95% CI` <- paste0(round((model$estimate - 1.96*model$std.error), 3), ", ", round((model$estimate + 1.96*model$std.error), 3))
  model$`r.squared` <- c(pool.r.squared(fit)[1], rep(NA, nrow(model)-1)) # add a column for R2
  model$`adj.r.squared` <- c(pool.r.squared(fit, adjusted = TRUE)[1], rep(NA, nrow(model)-1)) # adjusted R2
  return(model)
}

# INTERACTION WITHOUT COVARIATES: HALF SD
# stress group interaction with number of LE predicting emotional symptoms
model <- with(full_imp, lm(smfq_16y_sum_imp ~ relevel(stress_group_halfsd, ref = 1)*unweighted_LE_sum_centered))
fit_pool <- mice::pool(model)
model_no_covars <- my_table(fit_pool) # apply formatting function

# # INTERACTION WITHOUT COVARIATES: ONE SD
# # stress group interaction with number of LE predicting emotional symptoms
# model_onesd <- with(full_imp, lm(smfq_16y_sum_imp ~ relevel(stress_group_onesd, ref = 1)*unweighted_LE_sum_centered))
# fit_pool2 <- mice::pool(model_onesd)
# model_data_onesd <- my_table(fit_pool2) # apply formatting function

# INTERACTION WITH COVARIATES: HAFL SD --> using low reactivity as reference group
# stress group predicting emotional symptoms + covariates
model_covars_low <- with(full_imp, lm(smfq_16y_sum_imp ~ smfq_age_16y + sex + ethnicity + mum_uni + relevel(stress_group_halfsd, ref = 1)*unweighted_LE_sum_centered)) 
fit_pool3_low <- mice::pool(model_covars_low)
model_with_covars_low <- my_table(fit_pool3_low) # apply formatting function

# INTERACTION WITH COVARIATES: HAFL SD --> using typical reactivity as reference group
# stress group predicting emotional symptoms + covariates
model_covars_typical <- with(full_imp, lm(smfq_16y_sum_imp ~ smfq_age_16y + sex + ethnicity + mum_uni + relevel(stress_group_halfsd, ref = 2)*unweighted_LE_sum_centered)) 
fit_pool3_typical <- mice::pool(model_covars_typical)
model_with_covars_typical <- my_table(fit_pool3_typical) # apply formatting function

# INTERACTION WITH COVARIATES: HAFL SD --> using high reactivity as reference group
# stress group predicting emotional symptoms + covariates
model_covars_high <- with(full_imp, lm(smfq_16y_sum_imp ~ smfq_age_16y + sex + ethnicity + mum_uni + relevel(stress_group_halfsd, ref = 3)*unweighted_LE_sum_centered)) 
fit_pool3_high <- mice::pool(model_covars_high)
model_with_covars_high <- my_table(fit_pool3_high) # apply formatting function

# # INTERACTION WITH COVARIATES: ONE SD
# # stress group predicting emotional symptoms + covariates
# model_covars_onesd <- with(full_imp, lm(smfq_16y_sum_imp ~ smfq_age_16y + sex + ethnicity + mum_uni + relevel(stress_group_onesd, ref=3)*unweighted_LE_sum_centered))
# fit_pool4 <- mice::pool(model_covars_onesd) # stress group not a significant predictor on its own, but for high sensitivity individuals greater nr of LE = higher depression score
# model_with_covars_onesd <- my_table(fit_pool4) # apply formatting function

# WITH PRS
# INTERACTION WITH COVARIATES: HAFL SD
# subset to individuals with PRS (n = 3,378)
implist.prs <- miceadds::subset_datlist( full_imp, subset = !is.na(full_imp$data$PRS),  toclass = 'mids') 

# stress group predicting emotional symptoms + covariates
model_prs <- with(implist.prs, lm(smfq_16y_sum_imp ~ smfq_age_16y + sex + ethnicity + mum_uni + scale(PRS) + relevel(stress_group_halfsd, ref=1)*unweighted_LE_sum_centered)) 
fit_pool5 <- mice::pool(model_prs) 
model_with_prs <- my_table(fit_pool5) # apply formatting function

# # INTERACTION WITH COVARIATES: ONE SD
# # stress group predicting emotional symptoms + covariates
# model_prs_onesd <- with(implist.prs, lm(smfq_16y_sum_imp ~ smfq_age_16y + sex + ethnicity + mum_uni  + scale(PRS) + relevel(stress_group_onesd, ref=3)*unweighted_LE_sum_centered)) 
# fit_pool6 <- mice::pool(model_prs_onesd) # stress group not a significant predictor on its own, but for high sensitivity individuals greater nr of LE = higher depression score
# model_with_prs_onesd <- my_table(fit_pool6) # apply formatting function

# SEX STRATIFIED MODELS #
# MALES: INTERACTION WITH COVARIATES: HAFL SD
full_imp$data$sex <- as.numeric(full_imp$data$sex) # convert to numeric
male_data <- miceadds::subset_datlist(full_imp, subset = complete(full_imp, 30)$sex == 1,  toclass = 'mids') # males
male_model <- with(male_data, lm(smfq_16y_sum_imp ~ smfq_age_16y + ethnicity + mum_uni + relevel(stress_group_halfsd, ref = 3)*unweighted_LE_sum_centered)) 
fit_pool7 <- mice::pool(male_model)
male_with_covars <- my_table(fit_pool7) # apply formatting function

# FEMALES: INTERACTION WITH COVARIATES: HAFL SD
female_data <- miceadds::subset_datlist(full_imp, subset = complete(full_imp, 30)$sex == 2,  toclass = 'mids') # females
female_model <- with(female_data, lm(smfq_16y_sum_imp ~ smfq_age_16y + ethnicity + mum_uni + relevel(stress_group_halfsd, ref = 3)*unweighted_LE_sum_centered)) 
fit_pool8 <- mice::pool(female_model)
female_with_covars <- my_table(fit_pool8) # apply formatting function
full_imp$data$sex <- as.factor(full_imp$data$sex) # convert back to factor

# SEX STRATIFIED MODELS with Typical Reference Group #
# MALES: INTERACTION WITH COVARIATES: HAFL SD
full_imp$data$sex <- as.numeric(full_imp$data$sex) # convert to numeric
male_data <- miceadds::subset_datlist(full_imp, subset = complete(full_imp, 30)$sex == 1,  toclass = 'mids') # males
male_model2 <- with(male_data, lm(smfq_16y_sum_imp ~ smfq_age_16y + ethnicity + mum_uni + relevel(stress_group_halfsd, ref = 2)*unweighted_LE_sum_centered)) 
fit_pool9 <- mice::pool(male_model2)
male_with_covars2 <- my_table(fit_pool9) # apply formatting function

# FEMALES: INTERACTION WITH COVARIATES: HAFL SD
female_data <- miceadds::subset_datlist(full_imp, subset = complete(full_imp, 30)$sex == 2,  toclass = 'mids') # females
female_model2 <- with(female_data, lm(smfq_16y_sum_imp ~ smfq_age_16y + ethnicity + mum_uni + relevel(stress_group_halfsd, ref = 2)*unweighted_LE_sum_centered)) 
fit_pool10 <- mice::pool(female_model2)
female_with_covars2 <- my_table(fit_pool10) # apply formatting function
full_imp$data$sex <- as.factor(full_imp$data$sex) # convert back to factor

# SEX STRATIFIED MODELS with Low Reference Group #
# MALES: INTERACTION WITH COVARIATES: HAFL SD
full_imp$data$sex <- as.numeric(full_imp$data$sex) # convert to numeric
male_data <- miceadds::subset_datlist(full_imp, subset = complete(full_imp, 30)$sex == 1,  toclass = 'mids') # males
male_model3 <- with(male_data, lm(smfq_16y_sum_imp ~ smfq_age_16y + ethnicity + mum_uni + relevel(stress_group_halfsd, ref = 1)*unweighted_LE_sum_centered)) 
fit_pool11 <- mice::pool(male_model3)
male_with_covars3 <- my_table(fit_pool11) # apply formatting function

# FEMALES: INTERACTION WITH COVARIATES: HAFL SD
female_data <- miceadds::subset_datlist(full_imp, subset = complete(full_imp, 30)$sex == 2,  toclass = 'mids') # females
female_model3 <- with(female_data, lm(smfq_16y_sum_imp ~ smfq_age_16y + ethnicity + mum_uni + relevel(stress_group_halfsd, ref = 1)*unweighted_LE_sum_centered)) 
fit_pool12 <- mice::pool(female_model3)
female_with_covars3 <- my_table(fit_pool12) # apply formatting function
full_imp$data$sex <- as.factor(full_imp$data$sex) # convert back to factor

## SAVE ALL INTERACTION MODELS
all_models <- list("no_covars_halfsd" = model_no_covars, #"no_covars_onesd" = model_data_onesd, 
                   "covars_halfsd_low" = model_with_covars_low, #"covars_onesd" = model_with_covars_onesd, 
                   "covars_halfsd_typical" = model_with_covars_typical,
                   "covars_halfsd_high" = model_with_covars_high,
                   "prs_halfsd" = model_with_prs, # "prs_onesd" =model_with_prs_onesd, 
                   "males_halfsd_high" = male_with_covars, "females_halfsd_high" = female_with_covars,
                   "males_halfsd_typical" = male_with_covars2, "females_halfsd_typical" = female_with_covars2)

openxlsx::write.xlsx(all_models, file = paste0("all-interaction-results-", Sys.Date(),'.xlsx'), rowNames = F, overwrite=T)


######################################
### ASSUMPTION CHECKS INTERACTION  ###
######################################
# all sample
#assumption checks using means not sums (as sums are NA, calculated post imputation)
pdf("halfsd-interaction-assumption-checks.pdf")
# all sample
model <- lm(smfq_16y_sum_imp ~ smfq_age_16y + sex + ethnicity + mum_uni + stress_group_halfsd*unweighted_LE_mean, data = original,na.action=na.exclude)
print(summary(model))
par(mfrow=c(3,2))
hist(model$residuals, main="Histogram of Residuals")
plot(model)
plot(model, 4)
mtext("All sample: Half SD interaction assumption checks", outer=TRUE,  cex=1, line=-1.4, font=2, col='blue')
# males
model <- lm(smfq_16y_sum_imp ~ smfq_age_16y + ethnicity + mum_uni + stress_group_halfsd*unweighted_LE_mean, data = original[original$sex == 1, ], na.action=na.exclude)
print(summary(model))
par(mfrow=c(3,2))
hist(model$residuals, main="Histogram of Residuals")
plot(model)
plot(model, 4)
mtext("Males: Half SD interaction assumption checks", outer=TRUE,  cex=1, line=-1.4, font=2, col='blue')
#females
model <- lm(smfq_16y_sum_imp ~ smfq_age_16y + ethnicity + mum_uni + stress_group_halfsd*unweighted_LE_mean, data = original[original$sex == 2, ], na.action=na.exclude)
print(summary(model))
par(mfrow=c(3,2))
hist(model$residuals, main="Histogram of Residuals")
plot(model)
plot(model, 4)
mtext("Females: Half SD interaction assumption checks", outer=TRUE,  cex=1, line=-1.4, font=2, col='blue')
dev.off()

####################################
###       PLOT INTERACTION       ### 
####################################

# change reference level
dataset_30$stress_group_halfsd <- relevel(dataset_30$stress_group_halfsd, ref = 3)
dataset_30$stress_group_onesd <- relevel(dataset_30$stress_group_onesd, ref = 3)
males <- dataset_30 %>% filter(sex == 1) # subset males for plotting
females <- dataset_30 %>% filter(sex == 2) # subset females for plotting

interaction.plot <- function(data, group){
  group2 <-  data[, group]
  p <- ggplot(data, aes(x = unweighted_LE_sum, 
                              y = smfq_16y_sum_imp, 
                              color = group2)) +
    geom_point() + 
    #geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method= "lm" , fill="lightgrey", se=TRUE) +
    scale_color_manual(values=c("#FFC20A", "darkblue", "lightblue"), name = "Stress group", labels = c('High reactivity', 'Low reactivity', 'Typical reactivity')) + 
    # scale_color_grey() +
    theme_classic()
  # the shaded region corresponds to the estimated standard error of the fitted values
  
  p + xlab("Unweighted life events (sum)") + ylab("SMFQ emotional symptoms at 16 years")  
}

# HALF SD  
# Plot the interaction slopes with outlier
pdf("all-interaction-plot-with-outlier-halfsd-30th-imp.pdf") # all sample
interaction.plot(data = dataset_30, group = "stress_group_halfsd") 
interaction.plot(data = males, group = "stress_group_halfsd")
interaction.plot(data = females, group = "stress_group_halfsd")
dev.off()

# ONE SD  
# Plot the interaction slopes with outlier
pdf("all-interaction-plot-with-outlier-onesd-30th-imp.pdf") # all sample
interaction.plot(data = dataset_30, group = "stress_group_onesd") 
interaction.plot(data = males, group = "stress_group_onesd")
interaction.plot(data = females, group = "stress_group_onesd")
dev.off()


####################################
###    REMOVE OUTLIER AND PLOT   ### 
####################################

# remove green point that seems an outlier (using the 30th dataset)
dataset_30[dataset_30$unweighted_LE_sum == 22 ,]$cidB2957 # cidB2957 = 13728

outlier_test <- dataset_30 %>% filter(cidB2957 != 13728)
outlier_males <- outlier_test %>% filter(sex == 1)
outlier_females <- outlier_test %>% filter(sex == 2)

# HALF SD
# Plot the interaction slopes without outlier
pdf("all-interaction-plot-without-outlier-halfsd.pdf") 
interaction.plot(data = outlier_test, group = "stress_group_halfsd") # all sample
interaction.plot(data = outlier_males, group = "stress_group_halfsd")
interaction.plot(data = outlier_females, group = "stress_group_halfsd")
dev.off()

# ONE SD
# Plot the interaction slopes without outlier
pdf("all-interaction-plot-without-outlier-onefsd.pdf") 
interaction.plot(data = outlier_test, group = "stress_group_onesd") # all sample
interaction.plot(data = outlier_males, group = "stress_group_onesd")
interaction.plot(data = outlier_females, group = "stress_group_onesd")
dev.off()

# INTERACTION WITH COVARIATES: HAFL SD (without the outlier) --> no meaningful difference
# stress group predicting emotional symptoms + covariates
full_imp_no_outlier <- filter(full_imp, cidB2957 != 13728)
nrow(complete(full_imp)) # 4791 before 
nrow(complete(full_imp_no_outlier)) # 4790 after removing one outlier
model_covars_no_outlier <- with(full_imp_no_outlier, lm(smfq_16y_sum_imp ~ smfq_age_16y + sex + ethnicity + mum_uni + relevel(stress_group_halfsd, ref = 3)*unweighted_LE_sum_centered)) 
fit_pool_no_outlier <- mice::pool(model_covars_no_outlier)
model_with_covars_no_outlier <- my_table(fit_pool_no_outlier) # no meaningful difference


####################################
###              SAVE            ### 
####################################

# save full_imp mids object, which has all imputed datasets + the stress group variables
saveRDS(full_imp, file.path(data_path,'LE_imputation_list_with_z_prs_stress_groups.rds'))

############## end script #################




