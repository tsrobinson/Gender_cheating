##########################################
## Paper: Cheating across gender
## Section: Heterogeneous effects models (die game)
## Author code: Denise Laroze
## Additional code: Thomas Robinson
## Year: 2018
##########################################

library(FindIt)
library(BayesTree)
library(randomForest)
library(glmnet)
library(plyr)

source("het_plot_function.r")

#######################
### FindIt analysis
#######################

# Duplicate df
set.seed(89)
vars <- c("numberEntered","perform_high","country","modes","offerdg","gender_lab","subj_id")
df_het <- df[df$die == "Yes",vars]

df_het <- unique(df_het)
df_het <- df_het[complete.cases(df_het[,vars]),]

df_het$gender_lab <- as.factor(df_het$gender_lab)
df_het$modes <- as.factor(df_het$modes)
df_het$perform_high <- ifelse(df_het$perform_high == "High Performance",1,0)

# Audit rate = 0
A0 <- FindIt(model.treat = numberEntered ~ perform_high,
              model.main = ~ gender_lab + country + modes,
              model.int = ~ gender_lab + country + modes,
              data = df_het,
              type = "continuous",
              treat.type = "single")

A0 <- FindIt(model.treat = numberEntered ~ perform_high,
             model.main = ~ gender_lab + country + modes,
             model.int = ~ gender_lab + country + modes,
             data = df_het,
             type = "continuous",
             treat.type = "single",
             search.lambdas = FALSE,
             lambdas = c(2.475,2.480))

A0_pred <- predict(A0)

View(A0_pred$data)

## Stacked density plot
figure <- het_plot(A0_pred$data,"Treatment.effect","FindIt: 0% audit rate", binw = 30)
ggsave(figure, filename = "Figures/die_findit_audit_0.png", device = "png", height = 8, width = 6)


#######################
### BART heterogeneity analysis
#######################

# Refresh data
set.seed(89)
vars <- c("numberEntered","perform_high","country","modes","offerdg","gender_lab","subj_id")
df_het <- df[df$die == "Yes",vars]

df_het <- unique(df_het)
df_het <- df_het[complete.cases(df_het[,vars]),]

df_het$subj_id <-NULL

# Define variables incl. outcome as column 1

y <- as.double(df_het$numberEntered)
train <- df_het[,-1]

# Gen. test data: since treat is continuous, develop full schedule of treatment possibilities
test <- train
test$perform_high <- 1

temp <- train
temp$perform_high <- 0
test <- rbind(test,temp)

rm(temp)

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates
n <- length(bart.out$yhat.train.mean)
CATE_estimates <- train # get covar information
CATE_estimates$y_high <- bart.out$yhat.test.mean[1:n]
CATE_estimates$y_low <- bart.out$yhat.test.mean[(n+1):(2*n)]

CATE_estimates$t1 <- CATE_estimates$y_high - CATE_estimates$y_low


## BART plot - t1
figure <- het_plot(CATE_estimates,"t1","BART estimated CATE - Treatment: Ability", binw=30)
ggsave(figure, filename = "Figures/die_BART_plot_t1.png", device = "png", height = 8, width = 6)

#######################
### Random Forest
#######################

set.seed(89)
vars <- c("numberEntered","perform_high","country","modes","offerdg","gender_lab","subj_id")
df_het <- df[df$die == "Yes",vars]

df_het <- unique(df_het)
df_het <- df_het[complete.cases(df_het[,vars]),]

df_het$gender_lab <- as.factor(df_het$gender_lab)
df_het$modes <- as.factor(df_het$modes)
df_het$perform_high <- ifelse(df_het$perform_high == "High Performance",1,0)

df_het$subj_id <- NULL

# Define variables incl. outcome as column 1
test <- df_het[,-1]
test$perform_high <- 1

temp <- df_het[,-1]
temp$perform_high <- 0
test <- rbind(test,temp)

rm(temp)

a0_rf <- randomForest(numberEntered ~ ., data=df_het)

a0_rf_pred <- predict(a0_rf, newdata = test)

n <- length(test[,1])/2

CATE_estimates <- df_het[,-1] # get covar information
CATE_estimates$y_high <- a0_rf_pred[1:n]
CATE_estimates$y_low <- a0_rf_pred[(n+1):(2*n)]

CATE_estimates$t1 <- CATE_estimates$y_high - CATE_estimates$y_low


## Random Forest plot - t1
figure <- het_plot(CATE_estimates,"t1","Random Forest estimated CATE - Treatment: Ability", binw=30)
ggsave(figure, filename = "Figures/die_RF_plot_t1.png", device = "png", height = 8, width = 6)

#######################
### LASSO
#######################

set.seed(89)

vars <- c("percevaded", "taxrate", "gender_lab", "country", "modes", "offerdg", "perform_high")
df_het <- df[df$auditrate == 0,vars]

# Define variables incl. outcome as column 1
df_het <- df_het[complete.cases(df_het[,vars]),]

# Convert to dummies for glmnet
df_het$gender_lab <- ifelse(df_het$gender_lab == "Female",1,0)
df_het$modes <- ifelse(df_het$modes == "Lab",1,0)
df_het$perform_high <- ifelse(df_het$perform_high == "High Performance",1,0)
df_het$uk <- ifelse(df_het$country == "UK",1,0)
df_het$chile <- ifelse(df_het$country == "Chile",1,0)
df_het$russia <- ifelse(df_het$country == "Russia",1,0)
df_het$country <- NULL

Y <- df_het[,1]
X <- as.matrix(df_het[,-1])

# Run LASSO predictor algorithm, with cross-validated lambda selection
a0_lasso <- cv.glmnet(y = Y, x= X, alpha=1, family='gaussian')

# Output of model (s set to optimal lambda parameter)
coef(a0_lasso, s = a0_lasso$lambda.1se) # gender dropped from model