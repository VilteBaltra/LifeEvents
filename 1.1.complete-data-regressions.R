# LIFE EVENTS STUDY - REGRESSION SCRIPT WITH COMPLETE DATA (+ ASSUMPTION CHECKS)

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
dim(dataset_clean) # 3872    67

######################################
###   DEFINE REGRESSION FUNCTION   ###
######################################

regr_model <- function(outcome, age_at_outcome, LE, dataset, PRS = NULL, model_name, BS = NULL) {
  
  if(PRS == FALSE){
    # crude model 
    formula <- as.formula(paste(outcome,'~', LE))
    model <- lm(formula, data = dataset)
    
    # define regression for bootstrapping
    boot_regression <- function(data, indices) {
      model <- lm(formula,data=dataset[indices,]) 
      coef(model)
    }
    # set seed and bootstrap
    set.seed(1234)
    boot_results <- boot::boot(dataset, boot_regression, 10000)
    
    # save CIs for intercept and slope
    bs_CI_crude_model_list <- list() # Create empty list
    for(i in 1:ncol(model$model)) {
      bs_CI_crude_model_list[[i]] <- boot::boot.ci(boot_results, index = i, type = "perc")$percent[4:5] # index = 2 (is for slope)
    }
    # add names and convert to df
    names(bs_CI_crude_model_list) <- c("intercept", "slope")
    bs_CI_crude_model <- t(as.data.frame(bs_CI_crude_model_list))
    
    # full model 
    formula_full <- as.formula(paste(outcome,'~', LE, '+', age_at_outcome, ' + sex', '+ ethnicity', '+ mum_uni'))
    model_full <- lm(formula_full,data=dataset)
    
    # define regression with covars for bootstrapping
    boot_regression2 <- function(data, indices) {
      model <- lm(formula_full,data=dataset[indices,]) 
      coef(model)
    }
    # set seed and bootstrap
    set.seed(1234)
    boot_results <- boot::boot(dataset, boot_regression2, 10000)
    
    # save CIs for intercept and slope of all covars
    bs_CI_full_model_list <- list() # Create empty list
    for(i in 1:ncol(model_full$model)) {
      bs_CI_full_model_list[[i]] <- boot::boot.ci(boot_results, index = i, type = "perc")$percent[4:5] # index = 2 (is for slope)
    }
    # add names and convert to df
    names(bs_CI_full_model_list) <- c("intercept", names(model_full$model)[2:ncol(model_full$model)])
    bs_CI_full_model <- t(as.data.frame(bs_CI_full_model_list))
      
    my_table <- function(crude_model, model_full) {
      crude_m <- as.data.frame(summary(crude_model)$coefficient)
      crude_m <- cbind(crude_m, as.data.frame(confint(crude_model)))
      full_m <- rbind(crude_m, cbind(as.data.frame(summary(model_full)$coefficient), confint(model_full)))
      full_m$r.squared <- summary(crude_model)$r.squared 
      full_m$r.squared[3:8] <- summary(model_full)$r.squared 
      full_m$adj.r.squared <- summary(crude_model)$adj.r.squared
      full_m$adj.r.squared[3:8] <- summary(model_full)$adj.r.squared
      # format table
      full_m$`95% CI` <- paste0(round(full_m[, 5], 3), ", ", round(full_m[, 6], 3))
      # add bootstrapped CIs
      full_m$`Bootstrap 95% CI` <- c(paste0(round(bs_CI_crude_model[, 1], 3), ", ", round(bs_CI_crude_model[, 2], 3)), # bs CI from crude model 
                                         paste0(round(bs_CI_full_model[, 1], 3), ", ", round(bs_CI_full_model[, 2], 3)))  # bs CI from full model
      
      table <- full_m[, c('Estimate', '95% CI', 't value', 'Pr(>|t|)', 'r.squared', 'adj.r.squared', 'Bootstrap 95% CI')]
      table[, c('Estimate','t value', 'r.squared', 'adj.r.squared')] <- round(table[, c('Estimate','t value', 'r.squared', 'adj.r.squared')], 3)
      return(table)
    }
    
    output <- my_table(crude_model = model, model_full = model_full)
    #save model 
    openxlsx::write.xlsx(output, file = paste0(model_name, Sys.Date(),'.xlsx'), rowNames=T, overwrite=T)
    
    #assumption checks
    pdf(paste0(model_name, "assumption_checks.pdf"))
    print(summary(model))
    par(mfrow=c(3,2))
    hist(model$residuals, main="Histogram of Residuals")
    plot(model)
    plot(model, 4)
    mtext(paste0(model_name, "crude"), outer=TRUE,  cex=1, line=-1.4, font=2, col='blue')
    hist(model_full$residuals, main="Histogram of Residuals")
    plot(model_full)
    plot(model_full, 4)
    mtext(paste0(model_name, "adjusted"), outer=TRUE,  cex=1, line=-1.4, font=2, col='blue')
    dev.off()
  }
  
  if(PRS == TRUE){
    if(is.null(BS)){ ethnicity = '+ ethnicity' } else { ethnicity = '' }
    # prs - weighted model
    formula_prs <- as.formula(paste(outcome,'~', LE, '+', age_at_outcome, ' + sex', ethnicity, '+ mum_uni', '+ scale(PRS)')) # removed 'ethnicity' as not enough non-white observations for bootstrapping
    model_full_prs <-lm(formula_prs,data=dataset)
    #### sex stratified ####
    formula_prs_nosex <- as.formula(paste(outcome,'~', LE, '+', age_at_outcome,  ethnicity, '+ mum_uni', '+ scale(PRS)')) # removed 'ethnicity' as only 3 non-white in males and 5 in females
    # males
    dataset_clean_males <- dataset %>% filter(sex == 1)
    model_full_prs_males <-lm(formula_prs_nosex,data=dataset_clean_males)
    # females
    dataset_clean_females <- dataset %>% filter(sex == 2)
    model_full_prs_females <-lm(formula_prs_nosex, data=dataset_clean_females)
    
    # combine coeficients with CIs
    prs_table <- function(dataset, formula, model.fit){ # add dataset (all, F, M), formula, model
      if(is.null(BS)) {
        dataset = NULL
        formula = NULL
        # format table
        prs_model <- cbind(as.data.frame(summary(model.fit)$coefficient), as.data.frame(confint(model.fit)))
        prs_model$`95% CI` <- paste0(round(prs_model[, 5], 3), ", ", round(prs_model[, 6], 3))
        prs_model$r.squared <- summary(model.fit)$r.squared 
        prs_model$adj.r.squared <- summary(model.fit)$adj.r.squared
        prs_model <- prs_model[, c('Estimate', '95% CI', 't value', 'Pr(>|t|)', 'r.squared', 'adj.r.squared')] 
      } else {
        # define regression with covars for bootstrapping
        boot_regression <- function(data, indices) {
          model <- lm(formula,data=dataset[indices,])
          coef(model)
        }
        # set seed and bootstrap
        set.seed(1234)
        boot_results <- boot::boot(dataset, boot_regression, 10000)

        # save CIs for intercept and slope of all covars
        bs_CI_prs_model_list <- list() # Create empty list
        for(i in 1:ncol(model.fit$model)) {
          bs_CI_prs_model_list[[i]] <- boot::boot.ci(boot_results, index = i, type = "perc")$percent[4:5] # index = 2 (is for slope)
        }
        # add names and convert to df
        names(bs_CI_prs_model_list) <- c("intercept", names(model.fit$model)[2:ncol(model.fit$model)])
        bs_CI_prs_model <- t(as.data.frame(bs_CI_prs_model_list))

        # format table
        prs_model <- cbind(as.data.frame(summary(model.fit)$coefficient), as.data.frame(confint(model.fit)))
        prs_model$`95% CI` <- paste0(round(prs_model[, 5], 3), ", ", round(prs_model[, 6], 3))
        prs_model$r.squared <- summary(model.fit)$r.squared 
        prs_model$adj.r.squared <- summary(model.fit)$adj.r.squared
        # add bootstrapped CIs
        prs_model$`Bootstrap 95% CI` <- paste0(round(bs_CI_prs_model[, 1], 3), ", ", round(bs_CI_prs_model[, 2], 3))
        prs_model <- prs_model[, c('Estimate', '95% CI', 't value', 'Pr(>|t|)', 'r.squared', 'adj.r.squared', 'Bootstrap 95% CI')]
        }
       }
    
    prs_model = prs_table(dataset = dataset, formula = formula_prs, model.fit = model_full_prs) # all sample
    prs_model_males = prs_table(dataset = dataset_clean_males,formula = formula_prs_nosex,  model.fit = model_full_prs_males) # males
    prs_model_females = prs_table(dataset = dataset_clean_females, formula = formula_prs_nosex,  model.fit = model_full_prs_females) # females
    
    #save model 
    output = list(all = prs_model, males = prs_model_males, females = prs_model_females)
    openxlsx::write.xlsx(output, file = paste0(model_name, Sys.Date(),'.xlsx'), rowNames = T, overwrite=T)
    
    #assumption checks
    pdf(paste0(model_name, "assumption_checks.pdf"))
    print(summary(model_full_prs))
    par(mfrow=c(3,2))
    hist(model_full_prs$residuals, main="Histogram of Residuals")
    plot(model_full_prs)
    plot(model_full_prs, 4)
    mtext(paste0(model_name, "all"), outer=TRUE,  cex=1, line=-1.4, font=2, col='blue')
    dev.off()
  }
  return(output)
}

# Running regressions 
# The weighted and unweighted models will run slower as they include 10,000 bootstrapping
# in PRS models (weighted_prs and unweighted_prs) bootstrapping is optional by changing BS = NULL parameter
# if BS = TRUE, then PRS models will remove 'ethnicity' covar as it has too few non-White individuals
weighted <- regr_model(outcome='emot_symp_16y', 
                       age_at_outcome='emot_symp_age_16y', 
                       LE='weighted_LE_mean', 
                       dataset=dataset_clean, 
                       PRS = FALSE,
                       model_name = 'mean_weighted_SDQ_')

unweighted <- regr_model(outcome='emot_symp_16y', 
                         age_at_outcome='emot_symp_age_16y', 
                         LE='unweighted_LE_mean', 
                         dataset=dataset_clean, 
                         PRS = FALSE,
                         model_name = 'mean_unweighted_SDQ_')

weighted_prs <- regr_model(outcome='emot_symp_16y', 
                           age_at_outcome='emot_symp_age_16y', 
                           LE='weighted_LE_mean', 
                           dataset=dataset_clean_prs, 
                           PRS = TRUE,
                           model_name = 'prs_mean_weighted_SDQ_',
                           BS = NULL) # if bootstrapp is set to TRUE, 10,000 BS will be run and ethnicity covar will be removed

unweighted_prs <- regr_model(outcome='emot_symp_16y', 
                             age_at_outcome='emot_symp_age_16y', 
                             LE='unweighted_LE_mean', 
                             dataset=dataset_clean_prs, 
                             PRS = TRUE,
                             model_name = 'prs_mean_unweighted_SDQ_',
                             BS = NULL) # if bootstrapp is set to TRUE, 10,000 BS will be run and ethnicity covar will be removed

########### end script ################
