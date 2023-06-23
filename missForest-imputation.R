

# missForest
install.packages("missForest")
library(missForest)

# load data 
data("iris")

sapply(iris, class)

# set 10% NA
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)

# imupte missing values using default values
iris.imp <- missForest(iris.mis)

# check imputed values
iris.imp$ximp
summary(iris.imp$ximp)

# check imputation error
iris.imp$OOBerror



# MY DATA TRIAL
summary(dataset_clean)
dataset_clean_sub <- dataset_clean[, 1:56]
summary(dataset_clean_sub)

dataset_clean_sub <- dataset_clean_sub %>% select( -c(twin))
iris.imp <- missForest(dataset_clean_sub)

# check imputed values
iris.imp$ximp
summary(iris.imp$ximp)

# check imputation error
iris.imp$OOBerror

mean(iris.imp$ximp$ccs2001)
mean(dataset_clean_sub$ccs2001, na.rm=T)



# Histogram comparison
hist(iris.imp$ximp$ccs2001, col = "blue", main = "Imputed vs. Observed Values", xlab = "Variable of Interest")
hist(dataset_clean_sub$ccs2001, col = "red", add = TRUE)
legend("topright", c("Imputed", "Observed"), fill = c("blue", "red"))


# Histogram comparison
hist(iris.imp$ximp$ccs2211, col = "blue", main = "Imputed vs. Observed Values", xlab = "Variable of Interest")
hist(dataset_clean_sub$ccs2211, col = "red", add = TRUE)
legend("topright", c("Imputed", "Observed"), fill = c("blue", "red"))


# Histogram comparison
hist(iris.imp$ximp$ccs2110, col = "blue", main = "Imputed vs. Observed Values", xlab = "Variable of Interest")
hist(dataset_clean_sub$ccs2110, col = "red", add = TRUE)
legend("topright", c("Imputed", "Observed"), fill = c("blue", "red"))



# Scatter plot comparison
plot(iris.imp$ximp$ccs2001, dataset_clean_sub$ccs2001, pch = 16, col = "blue",
     xlab = "Imputed Values", ylab = "Observed Values", main = "Imputed vs. Observed Values")
abline(0, 1, col = "red")

# Scatter plot comparison
plot(iris.imp$ximp$ccs2211, dataset_clean_sub$ccs2211, pch = 16, col = "blue",
     xlab = "Imputed Values", ylab = "Observed Values", main = "Imputed vs. Observed Values")
abline(0, 1, col = "red")



dataset_clean_sub$ccs2211
observed_var <- dataset_clean_sub$ccs2211[!is.na(dataset_clean_sub$ccs2211)]


# Density plot comparison
plot(density(round(iris.imp$ximp$ccs2211)), col = "blue", lwd = 2, main = "Imputed vs. Observed Values",
     xlab = "Variable of Interest", xlim = c(min(iris.imp$ximp$ccs2211),
                                             max(iris.imp$ximp$ccs2211)))
lines(density(dataset_clean_sub$ccs2211[!is.na(dataset_clean_sub$ccs2211)]), col = "red", lwd = 2)
legend("topright", c("Imputed", "Observed"), lty = 1, col = c("blue", "red"), lwd = 2)




######################################
###  IMPUTED VS. OBSERVED VALUES   ###
######################################
# my function for density plots of imputed vs observed values
density_plots <- function(item, imputed.data, original.data){
  plot(density(round(as.numeric(imputed.data[, item]))), col = "blue", lwd = 2, main = paste0("Imputed vs. Observed Values - ", item),
       xlab = "Variable of Interest", xlim = c(min(as.numeric(imputed.data[, item])),
                                               max(as.numeric(imputed.data[, item]))))
  lines(density(round(as.numeric(original.data[, item]))[!is.na(as.numeric(original.data[, item]))]), col = "red", lwd = 2)
  legend("topright", c("Imputed", "Observed"), lty = 1, col = c("blue", "red"), lwd = 2)
} # round can be removed if imputed values are rounded before hand (to match original values format)


pdf("missForest-density-all-vars.pdf")
for (var in c(le_weighted, le_unweighted, outcome,covariates)) {
  density_plots(item = var, imputed.data = iris.imp$ximp, original.data = dataset_clean_sub)
}
dev.off()


pdf("mice-30dataset-density-all-vars.pdf")
for (var in c(le_weighted, le_unweighted, outcome,covariates)) {
  density_plots(item = var, last_imputed, dataset_clean)
}
dev.off()


######################################
###      missForest IMPUTATION     ###
######################################
# install.packages("missForest")
# install.packages("foreach")
# install.packages("doParallel")
library(missForest)
library(foreach)
library(doParallel)

# Set the number of CPU cores to be used for parallel computation
num_cores <- 6  # Change this number based on your available CPU cores

# Register the parallel backend
registerDoParallel(cores = num_cores)

dataset_clean_sub <- dataset_clean_sub %>% select( -c(twin))
dataset.imp <- missForest(dataset_clean_sub, maxiter = 60, ntree = 1000,
                          verbose = TRUE, parallelize = 'forests')

# Unregister the parallel backend after the computation is done
stopImplicitCluster()

# check imputed values
dataset.imp$ximp
summary(dataset.imp$ximp)

# check imputation error
dataset.imp$OOBerror
  

pdf("missForest-density-all-vars-60iter-run.pdf")
for (var in c(le_weighted, le_unweighted, outcome,covariates)) {
  density_plots(item = var, imputed.data = dataset.imp$ximp, original.data = dataset_clean_sub)
}
dev.off()

### NEW RUN WITH DIFF PARAMETERS
# Set the number of CPU cores to be used for parallel computation
num_cores <- 6  # Change this number based on your available CPU cores

# Register the parallel backend
registerDoParallel(cores = num_cores)


dataset.new2.imp <- missForest(dataset_clean_sub, maxiter = 30, ntree = 1000,
                              verbose = TRUE, parallelize = 'forests',
                              variablewise = TRUE, mtry = 20)

saveRDS(dataset.new2.imp, paste0(data_path, "forest.imp_30iter_1000trees.imp"))

# Unregister the parallel backend after the computation is done
stopImplicitCluster()

# check imputed values
dataset.new2.imp$ximp
summary(dataset.new2.imp$ximp)

# check imputation error
dataset.new2.imp$OOBerror

library(psych) 

# get the summary table for group with
# subjects to percentage
describeBy(dataset.new2.imp$ximp, group=dataset.new2.imp$ximp$sex, fast=TRUE)



describe(dataset.new2.imp$ximp)



