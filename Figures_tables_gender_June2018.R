
##########################################
## Paper: Cheating across gender
## Author code: Denise Laroze
## Additional code: Thomas Robinson
## Year: 2018
##########################################


library(foreign)
library(ggplot2)
library(ggpubr)
library(readstata13)
library(RColorBrewer)
library(rms)
theme_set(theme_bw())
library(plyr)
library(effsize)
library(gridExtra)
library(FindIt)
library(BayesTree)
library(randomForest)
library(glmnet)
library(clusterSEs)
library(car)
library(Rmisc)
library(texreg)
library(xtable)

# rm(list=ls())
# setwd("C:/Users/Denise Laroze Prehn/Dropbox/CESS-Santiago/Archive/Gender/Data Analysis")

rm(list=ls())
setwd("/Users/tomrobinson/OneDrive/CESS/Gender/Gender_cheating/")


#fig.path <- "Figures"
#fig.path<- "~/GitHub/Gender_cheating/Figures"
#bd<-"~/GitHub/Gender_cheating/"

fig.path<- "Figures"

# bd<-"C:/Users/Denise Laroze Prehn/Dropbox/CESS-Santiago/Archive/Gender/Data Analysis/"
bd<-"/Users/tomrobinson/OneDrive/CESS/Gender/Gender_cheating/"

v<-"20Jul2018"

#dat<-read.csv("Masterfile_2016_Dec.csv", sep=";")
cdat <- read.dta13("Data/mastern_final2018.dta") # Country comparison
modes<-read.csv("Data/Modes_data.csv")

# Source het. effect plot function
source("het_plot_function.r")

#################################
### Data Management country data 
#################################

#cdat$country<-"UK"
#cdat$country[cdat$chile==1]<-"Chile"
#rus$country<-"Russia"

#### UK and Chile identification of treatment

cdat$treatment <- NA
cdat$treatment[cdat$sujeto %in% c(1:72, 1141:1184)] <- 1 # Baseline
cdat$treatment[cdat$sujeto %in% 677:712] <- 1 # Baseline - Chile
cdat$treatment[cdat$sujeto %in% 321:392] <- 2 # Status
cdat$treatment[cdat$sujeto %in% 629:676] <- 2 # Status - Chile
cdat$treatment[cdat$sujeto %in% 521:628] <- 3 # Shock
cdat$treatment[cdat$sujeto %in% 393:476] <- 4 # Redistribution
cdat$treatment[cdat$sujeto %in% 713:896] <- 5 # non-fixed
cdat$treatment[cdat$country=="Russia"] <- 1 # Baseline
cdat$treatment[cdat$country=="Russia" & cdat$session %in% 112:114] <- 2 # Status - Russia
cdat$treatment[cdat$country=="Russia" & cdat$shock==1] <- 3 # shock - Russia
cdat$treatment[cdat$country=="Russia" & cdat$session==108] <- 4 # Redistribution - Russia
cdat$treatment[cdat$country=="Russia" & cdat$non_fixed==1]<-5
cdat$treatment_lab <- factor(cdat$treatment, labels = c("Baseline",
                                                        "Status",
                                                        "Shock",
                                                        "Redistribution",
                                                        "Non-fixed"))

table(cdat$country, cdat$treatment_lab)

cdat$treatment2 <- NA
cdat$treatment2[cdat$sujeto %in% c(1:72, 1141:1184)] <- 1 # Baseline
cdat$treatment2[cdat$sujeto %in% 677:712] <- 1 # Baseline - Chile
cdat$treatment2[cdat$sujeto %in% 321:392 & cdat$highsalary == 0] <- 2 # Status
cdat$treatment2[cdat$sujeto %in% 321:392 & cdat$highsalary == 1] <- 3 # Status
cdat$treatment2[cdat$sujeto %in% 629:676 & cdat$highsalary == 0] <- 2 # Status - Chile
cdat$treatment2[cdat$sujeto %in% 629:676 & cdat$highsalary == 1] <- 3 # Status - Chile
cdat$treatment2[cdat$sujeto %in% 521:628 & cdat$receiveshock == 0] <- 4 # Shock
cdat$treatment2[cdat$sujeto %in% 521:628 & cdat$receiveshock == 1] <- 5 # Shock
cdat$treatment2[cdat$sujeto %in% 393:476] <- 6 # Redistribution
cdat$treatment2[cdat$sujeto %in% 713:896] <- 7# non-fixed UK and Chile
cdat$treatment2[cdat$country=="Russia"] <- 1 # Baseline
cdat$treatment2[cdat$country=="Russia" & cdat$highsalary == 0] <- 2 # Status - Russia
cdat$treatment2[cdat$country=="Russia" & cdat$highsalary == 1] <- 3 # Status - Russia
cdat$treatment2[cdat$country=="Russia" & cdat$receiveshock == 0] <- 4 # Shock - Russia
cdat$treatment2[cdat$country=="Russia" & cdat$receiveshock == 1] <- 5 # Shock - Russia
cdat$treatment2[cdat$country=="Russia" & cdat$session==108] <- 6 # Redistribution - Russia
cdat$treatment2[cdat$country=="Russia" & cdat$non_fixed==1]<-7 # Non-Fixed Russia



cdat$treatment2_lab <- factor(cdat$treatment2, labels = c("Baseline",
                                                          "Status (Low)",
                                                          "Status (High)",
                                                          "Shock (No)",
                                                          "Shock (Yes)",
                                                          "Redistribution",
                                                          "Non-fixed"))

#####  High tax groups
cdat$HighTax  <- as.numeric(apply(cdat[,c("T20","T30", "T40")],1,sum)>0)



##### Gender label

cdat$gender_lab<-cdat$male




################################
### Relevant treatments and Data
################################


# Eliminating irrelevant treatments
cdat<-cdat[complete.cases(cdat$treatment), ]
#cdat<-cdat[cdat$treatment_lab!="Redistribution",]

# Eliminating Modules with 100 audit rate, as the rational decision is that no one cheats
cdat<-cdat[cdat$auditrate!=100, ]

cdat$numberEntered<-cdat$realdie




#################################
### Data Management modes data 
#################################

modes$safechoices<-(1-modes$risk.pref.normal)*10
modes$percevaded<-(1-modes$report.rate)
modes$offerdg<-modes$DictGive
modes$country<-ifelse(modes$sample=="Mturk", "USA",
                      ifelse( modes$sample== "CESS Online Stgo", "Chile", "UK") )


modes$treatment_lab<-as.character(modes$sample)
modes$treatment_lab[modes$sample=="Lab"]<-"Baseline"

#modes$gender_lab[modes$Gender==1]<-"Male"
#modes$gender_lab[modes$Gender==2]<-"Female"
modes$gender_lab[modes$Gender=="M"]<-"Male"
modes$gender_lab[modes$Gender=="F"]<-"Female"

modes$subj_id<- modes$muID
#modes$auditrate[modes$treat==1 & modes$sample=="Lab-DS"]<-0.2
modes<-modes[!(modes$sample == "Lab" & modes$auditrate==0), ]
modes$auditrate[modes$auditrate==0.1]<-10

####################
### Merging
####################

#### Joining datasets
vars<-c("ncorrectret","gender_lab", "safechoices","percevaded", "offerdg", "auditrate",
        "country", "treatment_lab" , "subj_id", "taxrate", "numberEntered")

df1<-modes[, vars]
df2<-cdat[, vars]
class(df2$subj_id)<-"character"

df<-rbind(df1, df2)
df<-df[complete.cases(df$gender_lab), ]

rm(df1, df2)


#########################
### Recoding variables 
#########################

# Cheating as a dummy
df$cheat<-ifelse(df$percevaded>0, 1, 0)

# Dummy for lab v. online experiments
df$modes<-ifelse(df$treatment_lab %in% c("CESS Online Stgo", "CESS Online UK",  "Mturk",  "Online Lab"), "Online", "Lab")
# Dummy for Die
df$die<-ifelse(is.na(df$numberEntered), "No", "Yes")



#Setting base category for factor
df$country <- factor(df$country, levels = c("UK", "Chile", "Russia", "USA"))

### Defining Performance as a dummy of above country median correct responses

m.cl<-median(df$ncorrectret[df$country=="Chile"], na.rm=T)
m.uk<-median(df$ncorrectret[df$country=="UK"], na.rm = T)
m.ru<-median(df$ncorrectret[df$country=="Russia"], na.rm = T)
m.usa<-median(df$ncorrectret[df$country=="USA"], na.rm = T)


df <- ddply(df, c("subj_id"), mutate,
            mean.ncorrectret = mean(ncorrectret, na.rm=T),
            perform_high = if (country=="Chile" && mean.ncorrectret>m.cl) "High Performance" 
            else if (country=="UK" && mean.ncorrectret>m.uk) "High Performance"
            else if (country=="Russia" && mean.ncorrectret>m.ru) "High Performance" 
            else if (country=="USA" && mean.ncorrectret>m.usa) "High Performance"
            else "Low Performance"
)

cdat<- ddply(cdat, c("subj_id"), mutate,
             mean.ncorrectret = mean(ncorrectret, na.rm=T),
             perform_high = if (country=="Chile" && mean.ncorrectret>m.cl) "High Performance"  
             else if (country=="UK" && mean.ncorrectret>m.uk) "High Performance" 
             else if (country=="Russia" && mean.ncorrectret>m.ru) "High Performance"  else "Low Performance"
)

###########################
### Subsets of data
###########################



#### Selecting on audit rate = 0 for robustness
df.0<-df[df$auditrate==0, ]

df.cl<-df.0[df.0$country=="Chile", ]
df.uk<-df.0[df.0$country=="UK", ]
df.ru<-df.0[df.0$country=="Russia", ]



#######################
### Summary statistics
#######################
#### Levels of ability
ability<-ddply(df, c("gender_lab"), summarize,
               mean.gender=mean(ncorrectret, na.rm = T),
               sem=sd(ncorrectret, na.rm = T)/sqrt(length(ncorrectret)),
               #95% confidence intervals of the mean
               lo_ci=mean.gender-1.96*sem,
               up_ci=mean.gender+1.96*sem
)

names(ability) <- c('Gender','Mean','s.e', "Lower c.i.", "Upper c.i." )
xt<-xtable(ability, digits = 2)
print(xt, type="latex", file=(paste0(bd, "Tables/meanXgender.tex")), floating=FALSE, include.rownames=FALSE)


#### Gender percentages
sum.table<-ddply(df, c("country", "modes","treatment_lab"), summarize,
                 #auditrate=as.character(list(unique(auditrate))),
                 #taxtrate=as.character(list(unique(taxrate))),
                 #die=as.character(list(unique(die))),
                 n = length(unique(subj_id)),
                 pc.female = round((length(unique(subj_id[gender_lab=="Female"]))/n)*100, 2),
                 pc.male = round((length(unique(subj_id[gender_lab=="Male"]))/n)*100, 2)
)
sum.table

place<-nrow(sum.table)+1
sum.table$country<-as.character(sum.table$country)
sum.table[place, "country"]<-"All"
sum.table[place, "modes"]<-"All"
sum.table[place, "treatment_lab"]<-"All"

n<-length(unique(df$subj_id))
sum.table[place, "n"]<-n

n.female<-length(unique(df$subj_id[df$gender_lab=="Female"]))
#sum.table[place, "n.female"]<-n.female

n.male<-length(unique(df$subj_id[df$gender_lab=="Male"]))
#sum.table[place, "n.male"]<-n.male

sum.table[place, "pc.female"]<-round((n.female/n)*100, 2)
sum.table[place, "pc.male"]<-round((n.male/n)*100, 2)

names(sum.table) <- c('Country','Mode','Treatment', '\\# Subjects' ,"\\% Female", "\\% Male")

xt<-xtable(sum.table)
print(xt, type="latex", file=(paste0(bd, "Tables/treatmentXcountryXgender.tex")), floating=FALSE, include.rownames=FALSE)
xt 



#### Treatment characteristics
sum.table<-ddply(df, c("country", "modes","treatment_lab"), summarize,
                 DG="Yes",
                 Risk="Yes",
                 auditrate=as.character(list(unique(auditrate))),
                 taxtrate=as.character(list(unique(taxrate))),
                 die=as.character(list(unique(die))),
                 n = length(unique(subj_id))
                 #pc.female = round((length(unique(subj_id[gender_lab=="Female"]))/n)*100, 2),
                 #pc.male = round((length(unique(subj_id[gender_lab=="Male"]))/n)*100, 2)
)
sum.table

place<-nrow(sum.table)+1
sum.table$country<-as.character(sum.table$country)
sum.table[place, "country"]<-"All"
sum.table[place, "modes"]<-"All"
sum.table[place, "treatment_lab"]<-"All"
sum.table[place, "DG"]<-"Yes"
sum.table[place, "Risk"]<-"Yes"


n<-length(unique(df$subj_id))
sum.table[place, "n"]<-n


names(sum.table) <- c('Country','Mode','Treatment', 'DG', 'Risk' ,'Audit Rate' ,"Tax Rate", "Die" ,"\\# Subjects")

xt<-xtable(sum.table)
print(xt, type="latex", file=(paste0(bd, "Tables/treatmentXcountry.tex")), floating=FALSE, include.rownames=FALSE)

xt


########################
### Balance tests
########################

balance1<-lrm(gender_lab ~ ncorrectret + auditrate + taxrate +country + modes + offerdg + safechoices, df,  x=T, y=T)
balance1.cl <- robcov(balance1, df$subj_id)
balance1.cl

texreg(list(balance1.cl))

coeffs<-c("Intercept", "\\# of Additions", #"Weighted Party System Dispersion",
          "Audit Rate", "Tax Rate", "Chile", "Russia", "USA",
          "Online Expt","Offer DG", "Risk Aversion")

texreg(file= paste0(bd, "Tables/balance_test.tex", sep=""), 
       list(balance1.cl),
       custom.model.names = c("Balance test" ), 
       custom.coef.names = coeffs, 
       reorder.coef=c( 2:4, 8:10, 5:7, 1),
       caption = "Logit model on likelihood of being male",
       caption.above = T,
       label="table:balance_test", stars = c(0.001, 0.01, 0.05), #ci.force = T, ci.force.level = 0.95,
       custom.note= "%stars Individual clustered standard s.e.",
       booktabs = F, dcolumn = F,  sideways = F)





########################
### Regression analysis
########################



## Basic model 

# % Evaded
lm1<-ols(percevaded~ gender_lab , df, x=T, y=T)
lm1.cl <- robcov(lm1, df$subj_id)


#Likelihood of cheating
glm1<-lrm(cheat~ gender_lab, df,  x=T, y=T)
glm1.cl <- robcov(glm1, df$subj_id)


## Model with treatment level controls

# % Evaded
lm2<-ols(percevaded~ gender_lab  + ncorrectret, df, x=T, y=T)
lm2.cl <- robcov(lm2, df$subj_id)

#Likelihood of cheating
glm2<-lrm(cheat~ gender_lab  + ncorrectret , df, x=T, y=T)
glm2.cl <- robcov(glm2, df$subj_id)

## Model with treatment level controls

# % Evaded
lm3<-ols(percevaded~ gender_lab  + ncorrectret + auditrate + taxrate + country + modes, df, x=T, y=T)
lm3.cl <- robcov(lm3, df$subj_id)


#Likelihood of cheating
glm3<-lrm(cheat~ gender_lab  + ncorrectret + auditrate + taxrate +country + modes, df,  x=T, y=T)
glm3.cl <- robcov(glm3, df$subj_id)

## Model with treatment  and individual level controls

# % Evaded
lm4<-ols(percevaded~ gender_lab  + ncorrectret + auditrate + taxrate + country + modes + offerdg + safechoices , df, x=T, y=T)
lm4.cl <- robcov(lm4, df$subj_id)

#Likelihood of cheating
glm4<-lrm(cheat~ gender_lab  + ncorrectret + auditrate + taxrate +country + modes+ offerdg + safechoices, df, x=T, y=T)
glm4.cl <- robcov(glm4, df$subj_id)

#######################
### FindIt analysis
#######################

# Duplicate df
set.seed(89)
df_het <- df

df_het$gender_lab <- as.factor(df_het$gender_lab)
df_het$modes <- as.factor(df_het$modes)
df_het$perform_high <- as.factor(df_het$perform_high)

# Audit rate = 0
# A0 <- FindIt(model.treat = percevaded ~ taxrate,
#              model.main = ~ gender_lab + country + modes + offerdg + mean.ncorrectret,
#              model.int = ~ gender_lab + country + modes,
#              data = df_het[df_het$auditrate == 0,],
#              type = "continuous",
#              treat.type = "single")

A0 <- FindIt(model.treat = percevaded ~ taxrate,
             model.main = ~ gender_lab + country + modes + offerdg + mean.ncorrectret,
             model.int = ~ gender_lab + country + modes,
             data = df_het[df_het$auditrate == 0,],
             type = "continuous",
             treat.type = "single",
             search.lambdas = FALSE,
             lambdas = c(-8.858,-8.853))

A0_pred <- predict(A0)


## Stacked density plot
figure <- het_plot(A0_pred$data,"Treatment.effect","FindIt: 0% audit rate")
ggsave(figure, filename = "Figures/findit_audit_0.png", device = "png", height = 8, width = 6)

## Gender effects heterogeneity

a0_gender_hetplot <- ggplot(hist_a0, aes(x = i, y = Treatment.effect)) +
  geom_point(aes(color=gender_lab)) +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = A0_pred$ATE, color = "blue") +
  labs(y = "Treatment Effect", color = "Gender") +
  theme_minimal()

ggsave(a0_gender_hetplot, filename = "Figures/findit_gender_0.png",device = "png",height = 6, width = 6)

# Audit rate = 10
# A10 <- FindIt(model.treat = percevaded ~ taxrate,
#               model.main = ~ gender_lab + country + modes + offerdg + mean.ncorrectret,
#               model.int = ~ gender_lab + country + modes,
#               data = df_het[df_het$auditrate == 10,],
#               type = "continuous",
#               treat.type = "single")

A10 <- FindIt(model.treat = percevaded ~ taxrate,
              model.main = ~ gender_lab + country + modes + offerdg + mean.ncorrectret,
              model.int = ~ gender_lab + country + modes,
              data = df_het[df_het$auditrate == 10,],
              type = "continuous",
              treat.type = "single",
              search.lambdas = FALSE,
              lambdas = c(-6.8795,-6.8745))

A10_pred <- predict(A10)

## Stacked density plot

# Duplicate dataframe and order treatment effects
figure <- het_plot(A10_pred$data, "Treatment.effect", "FindIt: 10% audit rate")
ggsave(figure, filename = "Figures/findit_audit_10.png", device = "png", height = 8, width = 6)

## Gender effects heterogeneity

a10_gender_hetplot <- ggplot(hist_a10, aes(x = i, y = Treatment.effect)) +
  geom_point(aes(color=gender_lab)) +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = A0_pred$ATE, color = "blue") +
  labs(y = "Treatment Effect", color = "Gender") +
  theme_minimal()

plot(a10_gender_hetplot)

ggsave(a10_gender_hetplot, filename = "Figures/findit_gender_10.png",device = "png",height = 6, width = 6)

#######################
### BART heterogeneity analysis
#######################

set.seed(89)

# Refresh data
vars <- c("percevaded", "taxrate", "gender_lab", "country", "modes", "offerdg", "mean.ncorrectret")
df_het <- df[df$auditrate == 0,vars]

# Define variables incl. outcome as column 1

df_het <- df_het[complete.cases(df_het[,vars]),]

y <- df_het$percevaded
train <- df_het[,-1]

# Gen. test data: since treat is continuous, develop full schedule of treatment possibilities
test <- train
test$taxrate <- 10

temp <- train
temp$taxrate <- 20
test <- rbind(test,temp)

temp <- train
temp$taxrate <- 30
test <- rbind(test,temp)

temp <- train
temp$taxrate <- 40
test <- rbind(test,temp)

rm(temp)

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates
n <- length(bart.out$yhat.train.mean)
CATE_estimates <- train # get covar information
CATE_estimates$y10 <- bart.out$yhat.test.mean[1:n]
CATE_estimates$y20 <- bart.out$yhat.test.mean[(n+1):(2*n)]
CATE_estimates$y30 <- bart.out$yhat.test.mean[(2*n+1):(3*n)]
CATE_estimates$y40 <- bart.out$yhat.test.mean[(3*n+1):(4*n)]

CATE_estimates$t1 <- CATE_estimates$y20 - CATE_estimates$y10
CATE_estimates$t2 <- CATE_estimates$y30 - CATE_estimates$y10
CATE_estimates$t3 <- CATE_estimates$y40 - CATE_estimates$y10

## BART plot - t1
figure <- het_plot(CATE_estimates,"t1","CATE = Yi,t=20 - Yi,t=10")
ggsave(figure, filename = "Figures/BART_plot_t1.png", device = "png", height = 8, width = 6)

## BART plot - t2
figure <- het_plot(CATE_estimates,"t2","CATE = Yi,t=30 - Yi,t=10")
ggsave(figure, filename = "Figures/BART_plot_t2.png", device = "png", height = 8, width = 6)

## BART plot - t3
figure <- het_plot(CATE_estimates,"t3","CATE = Yi,t=40 - Yi,t=10")
ggsave(figure, filename = "Figures/BART_plot_t3.png", device = "png", height = 8, width = 6)


#######################
### Random Forest
#######################

set.seed(89)

vars <- c("percevaded", "taxrate", "gender_lab", "country", "modes", "offerdg", "mean.ncorrectret")
df_het <- df[df$auditrate == 0,vars]
df_het$gender_lab <- as.factor(df_het$gender_lab)
df_het$modes <- as.factor(df_het$modes)

# Define variables incl. outcome as column 1
df_het <- df_het[complete.cases(df_het[,vars]),]

test <- df_het[,-1]
test$taxrate <- 10

temp <- df_het[,-1]
temp$taxrate <- 20
test <- rbind(test,temp)

temp <- df_het[,-1]
temp$taxrate <- 30
test <- rbind(test,temp)

temp <- df_het[,-1]
temp$taxrate <- 40
test <- rbind(test,temp)

rm(temp)

a0_rf <- randomForest(percevaded ~ ., data=df_het)

a0_rf_pred <- predict(a0_rf, newdata = test)

n <- length(test[,1])/4

CATE_estimates <- df_het[,-1] # get covar information
CATE_estimates$y10 <- a0_rf_pred[1:n]
CATE_estimates$y20 <- a0_rf_pred[(n+1):(2*n)]
CATE_estimates$y30 <- a0_rf_pred[(2*n+1):(3*n)]
CATE_estimates$y40 <- a0_rf_pred[(3*n+1):(4*n)]

CATE_estimates$t1 <- CATE_estimates$y20 - CATE_estimates$y10
CATE_estimates$t2 <- CATE_estimates$y30 - CATE_estimates$y10
CATE_estimates$t3 <- CATE_estimates$y40 - CATE_estimates$y10

## Random Forest plot - t1
## BART plot - t1
figure <- het_plot(CATE_estimates,"t1","CATE = Yi,t=20 - Yi,t=10")
ggsave(figure, filename = "Figures/RF_plot_t1.png", device = "png", height = 8, width = 6)

#######################
### LASSO
#######################

set.seed(89)

vars <- c("percevaded", "taxrate", "gender_lab", "country", "modes", "offerdg", "mean.ncorrectret")
df_het <- df[df$auditrate == 0,vars]

# Define variables incl. outcome as column 1
df_het <- df_het[complete.cases(df_het[,vars]),]

# Convert to dummies for glmnet
df_het$gender_lab <- ifelse(df_het$gender_lab == "Female",1,0)
df_het$modes <- ifelse(df_het$modes == "Lab",1,0)
df_het$uk <- ifelse(df_het$country == "UK",1,0)
df_het$chile <- ifelse(df_het$country == "Chile",1,0)
df_het$russia <- ifelse(df_het$country == "Russia",1,0)
df_het$country <- NULL

Y <- df_het[,1]
X <- as.matrix(df_het[,-1])

# Run LASSO predictor algorithm, with cross-validated lambda selection
a0_lasso <- cv.glmnet(y = Y, x= X, alpha=1, family='gaussian')

# Output of model (s set to optimal lambda parameter)
coef(a0_lasso, s = a0_lasso$lambda.1se)

# Build test dataset
test <- df_het[,-1]
test$taxrate <- 10

temp <- df_het[,-1]
temp$taxrate <- 20
test <- rbind(test,temp)

temp <- df_het[,-1]
temp$taxrate <- 30
test <- rbind(test,temp)

temp <- df_het[,-1]
temp$taxrate <- 40
test <- rbind(test,temp)

test <- as.matrix(test)
rm(temp)

# Predict on full schedule of possible treatment-covariate profiles
a0_lasso_pred <- predict(a0_lasso, newx = test)

## Set up CATE dataframe for plots

# get covar information
CATE_estimates <- df_het[,-1] 

# Consolidate dummy variables
CATE_estimates$country <- ifelse(CATE_estimates$uk == 1,"UK",
                                 ifelse(CATE_estimates$chile == 1, "Chile",
                                        ifelse(CATE_estimates$russia == 1, "Russia","USA")))

CATE_estimates$uk <- NULL
CATE_estimates$chile <- NULL
CATE_estimates$russia <- NULL

CATE_estimates$gender_lab <- ifelse(CATE_estimates$gender_lab == 1,"Female","Male")
CATE_estimates$modes <- ifelse(CATE_estimates$modes == 1,"Lab","Online")

# Recover CATE estimates
n <- length(test[,1])/4

CATE_estimates$y10 <- a0_lasso_pred[1:n]
CATE_estimates$y20 <- a0_lasso_pred[(n+1):(2*n)]
CATE_estimates$y30 <- a0_lasso_pred[(2*n+1):(3*n)]
CATE_estimates$y40 <- a0_lasso_pred[(3*n+1):(4*n)]

CATE_estimates$t1 <- CATE_estimates$y20 - CATE_estimates$y10
CATE_estimates$t2 <- CATE_estimates$y30 - CATE_estimates$y10
CATE_estimates$t3 <- CATE_estimates$y40 - CATE_estimates$y10

## LASSO plot - t1
figure <- het_plot(CATE_estimates,"t1","CATE = Yi,t=20 - Yi,t=10")
ggsave(figure, filename = "Figures/LASSO_plot_t1.png", device = "png", height = 8, width = 6)


#######################
### Tables main models
#######################

texreg(list(lm1.cl, lm2.cl, lm3.cl, lm4.cl))

coeffs<-c("Intercept", "Male", "\\# of Additions", 
          "Audit Rate", "Tax Rate", "Chile", "Russia", "USA",
          "Online Expt","Offer DG", "Risk Aversion")


texreg(file= paste0(bd, "Tables/main_lm_models.tex", sep=""), 
       list(lm1.cl, lm2.cl, lm3.cl, lm4.cl),
       custom.model.names = c("M1", "M2 ", "M3", "M4" ), 
       custom.coef.names = coeffs, 
       reorder.coef=c( 2, 3, 4,5, 11,10, 9, 6,7, 8, 1),
       caption = "Linear models on percent evaded",
       caption.above = T,
       label="table:main_lm_models", stars = c(0.001, 0.01, 0.05), #ci.force = T, ci.force.level = 0.95,
       custom.note= "%stars Individual clustered standard s.e.",
       booktabs = F, dcolumn = F,  sideways = F)


texreg(list(glm1.cl, glm2.cl, glm3.cl, glm4.cl))


texreg(file= paste0(bd, "Tables/main_glm_models.tex", sep=""), 
       list(glm1.cl, glm2.cl, glm3.cl, glm4.cl),
       custom.model.names = c("M1", "M2 ", "M3", "M4" ), 
       custom.coef.names = coeffs, 
       reorder.coef=c(2, 3, 4,5, 11,10, 9, 6,7, 8, 1),
       caption = "Logit models on cheating",
       caption.above = T,
       label="table:main_glm_models", stars = c(0.001, 0.01, 0.05), #ci.force = T, ci.force.level = 0.95,
       custom.note= "%stars Individual clustered standard s.e.",
       booktabs = F, dcolumn = F,  sideways = F)


###############################
### Gender specific regressions
###############################
## Model with treatment  and individual level controls

# % Evaded - Male
lm4.m<-ols(percevaded~ ncorrectret + auditrate + taxrate + country + modes + offerdg + safechoices , df[df$gender_lab=="Male", ], x=T, y=T)
lm4.m.cl <- robcov(lm4.m, df$subj_id[df$gender_lab=="Male"])

#Likelihood of cheating - Male
glm4.m<-lrm(cheat~ ncorrectret + auditrate + taxrate +country + modes+ offerdg + safechoices, df[df$gender_lab=="Male", ], x=T, y=T)
glm4.m.cl <- robcov(glm4.m, df$subj_id[df$gender_lab=="Male"])


# % Evaded - Female
lm4.f<-ols(percevaded~ ncorrectret + auditrate + taxrate + country + modes + offerdg + safechoices , df[df$gender_lab=="Female", ], x=T, y=T)
lm4.f.cl <- robcov(lm4.f, df$subj_id[df$gender_lab=="Female"])

#Likelihood of cheating - Female
glm4.f<-lrm(cheat~  ncorrectret + auditrate + taxrate +country + modes+ offerdg + safechoices, df[df$gender_lab=="Female", ], x=T, y=T)
glm4.f.cl <- robcov(glm4.f, df$subj_id[df$gender_lab=="Female"])




texreg(list(glm4.m.cl, glm4.f.cl, lm4.m.cl, lm4.f.cl))

coeffs<-c("Intercept", "\\# of Additions", 
          "Audit Rate", "Tax Rate", "Chile", "Russia", "USA",
          "Online Expt","Offer DG", "Risk Aversion")


texreg(file= paste0(bd, "Tables/gender_sep_models.tex", sep=""), 
       list(glm4.m.cl, glm4.f.cl, lm4.m.cl, lm4.f.cl),
       custom.model.names = c("M1 - Male", "M2 - Female ", "M3 - Male", "M4 -Female" ), 
       custom.coef.names = coeffs, 
       reorder.coef=c( 2, 3, 4, 9:10, 8, 5:7, 1),
       caption = "Model M1 is a logit on probability of cheating for a male subset, M2 is the same for a Female subset, 
       M3 is a linear model on percent evaded for a male subset and M4 is the same for a Female Subset",
       caption.above = T,
       label="table:gender_spe_models", stars = c(0.001, 0.01, 0.05), #ci.force = T, ci.force.level = 0.95,
       custom.note= "%stars Individual clustered standard s.e.",
       booktabs = F, dcolumn = F,  sideways = F)




#######################
## Interaction effects
#######################

## Model with treatment  and individual level controls

#audit rate
# % Evaded
int.lm1<-ols(percevaded~ gender_lab*auditrate + ncorrectret , df, x=T, y=T)
int.lm1.cl <- robcov(int.lm1, df$subj_id)

#Likelihood of cheating
int.glm1<-lrm(cheat~ gender_lab*auditrate  + ncorrectret, df, x=T, y=T)
int.glm1.cl <- robcov(int.glm1, df$subj_id)

#tax rate
# % Evaded
int.lm2<-ols(percevaded~ gender_lab*taxrate + ncorrectret , df, x=T, y=T)
int.lm2.cl <- robcov(int.lm2, df$subj_id)

#Likelihood of cheating
int.glm2<-lrm(cheat~ gender_lab*taxrate + ncorrectret , df, x=T, y=T)
int.glm2.cl <- robcov(int.glm2, df$subj_id)

# Country
# % Evaded
int.lm3<-ols(percevaded~ gender_lab*country  + ncorrectret , df, x=T, y=T)
int.lm3.cl <- robcov(int.lm3, df$subj_id)

#Likelihood of cheating
int.glm3<-lrm(cheat~ gender_lab*country + ncorrectret , df, x=T, y=T)
int.glm3.cl <- robcov(int.glm3, df$subj_id)


# offerdg
# % Evaded
int.lm4<-ols(percevaded~ gender_lab*offerdg  + ncorrectret , df, x=T, y=T)
int.lm4.cl <- robcov(int.lm4, df$subj_id)

#Likelihood of cheating
int.glm4<-lrm(cheat~ gender_lab*offerdg  + ncorrectret , df, x=T, y=T)
int.glm4.cl <- robcov(int.glm4, df$subj_id)


# safe choices
# % Evaded
int.lm5<-ols(percevaded~ gender_lab*safechoices  + ncorrectret , df, x=T, y=T)
int.lm5.cl <- robcov(int.lm5, df$subj_id)

#Likelihood of cheating
int.glm5<-lrm(cheat~ gender_lab*safechoices  + ncorrectret , df, x=T, y=T)
int.glm5.cl <- robcov(int.glm5, df$subj_id)



coeffs<-c("Intercept", "Male",  "Audit Rate", "\\# of Additions", 
          "Male*Audit Rate",
          "Tax Rate", "Male*Tax Rate",
          "Chile", "Russia", "USA",  "Male*Chile", "Male*Russia", "Male*USA",
          "Offer DG", "Male*Offer DG", 
          "Risk Aversion",
          "Male*Risk Aversion"
)


texreg(list(int.lm1.cl, int.lm2.cl, int.lm3.cl, int.lm4.cl, int.lm5.cl))

texreg(file= paste0(bd, "Tables/int_models_no_controls.tex", sep=""), 
       list(int.glm1.cl, int.glm2.cl, int.glm3.cl, int.glm4.cl, int.glm5.cl,int.lm1.cl, int.lm2.cl, int.lm3.cl, int.lm4.cl, int.lm5.cl),
       custom.model.names = c("Int GLM1", " Int GLM2 ", "Int GLM3", "Int GLM4", "Int GLM5",
                              "Int LM1", " Int LM2 ", "Int LM3", "Int LM4", "Int LM5"), 
       custom.coef.names = coeffs, 
       reorder.coef=c( 2, 5, 7,  15, 17, 11:13,   4:3, 6, 14, 16 ,8:10 ,1),
       caption = "Empirical models on cheating (Logit GLM) and percent evaded (linear LM) include a control for ability (\\# of Additions)",
       caption.above = T,
       label="table:int_models_no_controls", stars = c(0.001, 0.01, 0.05), #ci.force = T, ci.force.level = 0.95,
       custom.note= "%stars Individual clustered standard s.e.",
       booktabs = F, dcolumn = F,  sideways = F)


## Model with treatment  and individual level controls

#audit rate
# % Evaded
int.lm1<-ols(percevaded~ gender_lab*auditrate  + ncorrectret + auditrate + taxrate + country + modes + offerdg + safechoices , df, x=T, y=T)
int.lm1.cl <- robcov(int.lm1, df$subj_id)

#Likelihood of cheating
int.glm1<-lrm(cheat~ gender_lab*auditrate + ncorrectret + auditrate + taxrate +country + modes+ offerdg + safechoices, df, x=T, y=T)
int.glm1.cl <- robcov(int.glm1, df$subj_id)

#tax rate
# % Evaded
int.lm2<-ols(percevaded~ gender_lab*taxrate  + ncorrectret + auditrate + taxrate + country + modes + offerdg + safechoices , df, x=T, y=T)
int.lm2.cl <- robcov(int.lm2, df$subj_id)

#Likelihood of cheating
int.glm2<-lrm(cheat~ gender_lab*taxrate + ncorrectret + auditrate + taxrate +country + modes+ offerdg + safechoices, df, x=T, y=T)
int.glm2.cl <- robcov(int.glm2, df$subj_id)

# Country
# % Evaded
int.lm3<-ols(percevaded~ gender_lab*country  + ncorrectret + auditrate + taxrate + country + modes + offerdg + safechoices , df, x=T, y=T)
int.lm3.cl <- robcov(int.lm3, df$subj_id)

#Likelihood of cheating
int.glm3<-lrm(cheat~ gender_lab*country + ncorrectret + auditrate + taxrate +country + modes+ offerdg + safechoices, df, x=T, y=T)
int.glm3.cl <- robcov(int.glm3, df$subj_id)


# offerdg
# % Evaded
int.lm4<-ols(percevaded~ gender_lab*offerdg  + ncorrectret + auditrate + taxrate + country + modes + offerdg + safechoices , df, x=T, y=T)
int.lm4.cl <- robcov(int.lm4, df$subj_id)

#Likelihood of cheating
int.glm4<-lrm(cheat~ gender_lab*offerdg  + ncorrectret + auditrate + taxrate +country + modes+ offerdg + safechoices, df, x=T, y=T)
int.glm4.cl <- robcov(int.glm4, df$subj_id)


# safe choices
# % Evaded
int.lm5<-ols(percevaded~ gender_lab*safechoices  + ncorrectret + auditrate + taxrate + country + modes + offerdg + safechoices , df, x=T, y=T)
int.lm5.cl <- robcov(int.lm5, df$subj_id)

#Likelihood of cheating
int.glm5<-lrm(cheat~ gender_lab*safechoices  + ncorrectret + auditrate + taxrate +country + modes+ offerdg + safechoices, df, x=T, y=T)
int.glm5.cl <- robcov(int.glm5, df$subj_id)


texreg(list(int.lm1.cl, int.lm2.cl, int.lm3.cl, int.lm4.cl, int.lm5.cl),
       reorder.coef=c( 2, 12:13, 17:18, 14:16, 4, 3, 5, 11:9  , 6:8, 1))

coeffs<-c("Intercept", "Male",  "Audit Rate", "\\# of Additions", #"Weighted Party System Dispersion",
          "Tax Rate", "Chile", "Russia", "USA",
          "Online Expt","Offer DG", "Risk Aversion",
          "Male*Audit Rate", "Male*Tax Rate", "Male*Chile", "Male*Russia", "Male*USA",
          "Male*Offer DG", "Male*Risk Aversion"
)


texreg(file= paste0(bd, "Tables/int_models.tex", sep=""), 
       list(int.glm1.cl, int.glm2.cl, int.glm3.cl, int.glm4.cl, int.glm5.cl,int.lm1.cl, int.lm2.cl, int.lm3.cl, int.lm4.cl, int.lm5.cl),
       custom.model.names = c("Int GLM6", " Int GLM7 ", "Int GLM8", "Int GLM9", "Int GLM10",
                              "Int LM6", " Int LM7 ", "Int LM8", "Int LM9", "Int LM10"), 
       custom.coef.names = coeffs, 
       reorder.coef=c( 2, 12:13, 17:18, 14:16, 4, 3, 5, 11:9  , 6:8, 1),
       caption = "Empirical models on cheating (Logit GLM) and percent evaded (linear LM) including variables
       to control for treatment and individual level characteristics",
       caption.above = T,
       label="table:int_models", stars = c(0.001, 0.01, 0.05), #ci.force = T, ci.force.level = 0.95,
       custom.note= "%stars Individual clustered standard s.e.",
       booktabs = F, dcolumn = F,  sideways = F)



#####################
## Interaction plots
#####################



#----------------
# Audit rate
#---------------

# Cheating
ggplot(df, aes(x = auditrate , y=cheat*100, group = gender_lab, color = gender_lab))  + scale_color_brewer(palette="Set1") + labs(colour="") +
  geom_smooth(method = lm, se=T)  +
  ylab("% Cheating")  + xlab("Audit Rate")+ facet_wrap(~ perform_high)  + ylim(0,100) +
  theme(legend.position="bottom")

ggsave(paste0("cheat_genderxauditrate", v, ".pdf"), path=fig.path ,width = 5, height =4)


# Percent Evaded
ggplot(df, aes(x = auditrate , y=percevaded*100, group = gender_lab, color = gender_lab))  + scale_color_brewer(palette="Set1") + labs(colour="") +
  geom_smooth(method = lm, se=T)  +
  ylab(" % Evaded")  + xlab("Audit Rate")+ facet_wrap(~ perform_high)  + ylim(0,100) +
  theme(legend.position="bottom")

ggsave(paste0("percevaded_genderxauditrate", v, ".pdf"), path=fig.path ,width = 5, height =4)




#----------------
# N correct RET
#---------------
# Cheating
ggplot(df, aes(x = ncorrectret , y=cheat*100, group = gender_lab, color = gender_lab))  + scale_color_brewer(palette="Set1") + labs(colour="") +
  geom_smooth(method = lm, se=T)  +
  ylab(" % Cheating")  + xlab("Number of correct responses")+ facet_wrap(~ perform_high)  + ylim(0,100) +
  theme(legend.position="bottom")

ggsave(paste0("cheat_genderxncorrectret", v, ".pdf"), path=fig.path ,width = 5, height =4)

# Percent Evaded
ggplot(df, aes(x = ncorrectret , y=percevaded*100, group = gender_lab, color = gender_lab))  + scale_color_brewer(palette="Set1") + labs(colour="") +
  geom_smooth(method = lm, se=T)  +
  ylab(" % Evaded")  + xlab("Number of correct responses")+ facet_wrap(~ perform_high)  + ylim(0,100) +
  theme(legend.position="bottom")

ggsave(paste0("percevaded_genderxncorrectret", v, ".pdf"), path=fig.path ,width = 5, height =4)




#----------------
# N Offer in Dictator Game
#---------------
# Cheating
ggplot(df, aes(x = offerdg , y=cheat*100, group = gender_lab, color = gender_lab))  + scale_color_brewer(palette="Set1") + labs(colour="") +
  geom_smooth(method = lm, se=T)  +
  ylab(" % Cheating")  + xlab("Amount offered in DG")+ facet_wrap(~ perform_high)  + ylim(0,100) +
  theme(legend.position="bottom")

ggsave(paste0("cheat_genderxofferdg", v, ".pdf"), path=fig.path ,width = 5, height =4)

# Percent Evaded
ggplot(df, aes(x = offerdg , y=percevaded*100, group = gender_lab, color = gender_lab))  + scale_color_brewer(palette="Set1") + labs(colour="") +
  geom_smooth(method = lm, se=T)  +
  ylab(" % Evaded")  + xlab("Amount offered in DG")+ facet_wrap(~ perform_high)  + ylim(0,100) +
  theme(legend.position="bottom")

ggsave(paste0("percevaded_genderxofferdg", v, ".pdf"), path=fig.path ,width = 5, height =4)



#-------------
# Safe Choices
#-------------

ggplot(df, aes(x = safechoices , y=cheat*100, group = gender_lab, color = gender_lab))  + scale_color_brewer(palette="Set1") + labs(colour="") +
  geom_smooth(method = lm, se=T)  +
  ylab(" % Cheating")  + xlab("Safe Choices")  + ylim(0,100) +
  theme(legend.position="bottom")

ggsave(paste0("cheat_genderxsafechoices", v, ".pdf"), path=fig.path ,width = 5, height =4)

#  Percent evaded
ggplot(df, aes(x = safechoices , y=percevaded*100, group = gender_lab, color = gender_lab))  + scale_color_brewer(palette="Set1") + labs(colour="") +
  geom_smooth(method = lm, se=T)  +
  ylab(" % Evaded")  + xlab("Safe Choices")  + ylim(0,100) +
  theme(legend.position="bottom")

ggsave(paste0("perevaded_genderxsafechoices", v, ".pdf"), path=fig.path ,width = 5, height =4)



#  Cheating and performance
ggplot(df, aes(x = safechoices , y=cheat*100, group = gender_lab, color = gender_lab))  + scale_color_brewer(palette="Set1") + labs(colour="") +
  geom_smooth(method = lm, se=T)  +
  ylab(" % Cheating")  + xlab("Safe Choices") + facet_grid(~ perform_high)  + ylim(0,100) +
  theme(legend.position="bottom")

ggsave(paste0("cheat_genderxsafechoicesXability", v, ".pdf"), path=fig.path ,width = 10, height =8)

#  Percent evaded and performance
ggplot(df, aes(x = safechoices , y=percevaded*100, group = gender_lab, color = gender_lab))  + scale_color_brewer(palette="Set1") + labs(colour="") +
  geom_smooth(method = lm, se=T)  +
  ylab(" % Evaded")  + xlab("Safe Choices")+ facet_grid(~ perform_high)  + ylim(0,100) +
  theme(legend.position="bottom")

ggsave(paste0("perevaded_genderxsafechoicesxability", v, ".pdf"), path=fig.path ,width = 5, height =4)

#----------------
# Dictator Offer
#----------------

# Visualization of interactions
ggplot(df, aes(x = offerdg , y=percevaded, group = gender_lab, color = gender_lab)) + 
  geom_point() +
  geom_smooth(method = lm, se=T) +
  ylab(" % evaded") + facet_wrap(~ perform_high + country)


#-----------------------
# Gender by country
#----------------------

#Cheating
tgc <- summarySE(df, measurevar="cheat", groupvars=c("gender_lab", "country", "perform_high"), na.rm=T)

ggplot(tgc, aes(x = country, y = cheat*100, colour= gender_lab)) + 
  geom_point(position = position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=(cheat-ci)*100, ymax=(cheat+ci)*100), width=.1, position = position_dodge(width=0.3)) +
  geom_line(position = position_dodge(width=0.3), size = 1.5) + ylab("% Cheating") + xlab("") + ylim(0,100)+
  scale_color_brewer(palette="Set1") + labs(colour="") +
  facet_wrap(~ perform_high)+
  theme(legend.position="bottom")


ggsave(paste0("cheat_genderxcountry", v, ".pdf"), path=fig.path ,width = 5, height =4)




# % Evaded
tgc <- summarySE(df, measurevar="percevaded", groupvars=c("gender_lab", "country", "perform_high"), na.rm=T)

ggplot(tgc, aes(x = country, y = percevaded*100, colour= gender_lab)) + 
  geom_point(position = position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=(percevaded-ci)*100, ymax=(percevaded+ci)*100), width=.1, position = position_dodge(width=0.3)) +
  geom_line(position = position_dodge(width=0.3), size = 1.5) + ylab("% Evaded") + xlab("") +  ylim(0,100)+
  scale_color_brewer(palette="Set1") + labs(colour="") +
  facet_wrap(~ perform_high)+
  theme(legend.position="bottom")


ggsave(paste0("percevaded_genderxcountry", v, ".pdf"), path=fig.path ,width = 5, height =4)





#--------------------------------
# Gender by treatment and country
#--------------------------------

# Cheating
tgc <- summarySE(df, measurevar="cheat", groupvars=c("gender_lab", "treatment_lab" ,"country", "perform_high"), na.rm=T)

ggplot(tgc, aes(x = treatment_lab, y = cheat*100, colour= gender_lab)) + 
  geom_point(position = position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=(cheat-ci)*100, ymax=(cheat+ci)*100), width=.1, position = position_dodge(width=0.3)) +
  geom_line(position = position_dodge(width=0.3), size = 1.5) + ylab("% Cheating") + xlab("") + ylim(0,100)+
  facet_wrap(~  perform_high + country) + scale_color_brewer(palette="Set1") + labs(colour="") +
  theme(legend.position="bottom")


ggsave(paste0("cheat_genderxtreatment", v, ".pdf"), path=fig.path, width = 14, height = 10)



# % Evaded


tgc <- summarySE(df, measurevar="percevaded", groupvars=c("gender_lab", "treatment_lab" ,"country", "perform_high"), na.rm=T)

ggplot(tgc, aes(x = treatment_lab, y = percevaded*100, colour= gender_lab)) + 
  geom_point(position = position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=(percevaded-ci)*100, ymax=(percevaded+ci)*100), width=.1, position = position_dodge(width=0.3)) +
  geom_line(position = position_dodge(width=0.3), size = 1.5) + ylab("% Evaded") + xlab("") + ylim(0,100)+
  facet_wrap(~  perform_high + country) + scale_color_brewer(palette="Set1") + labs(colour="")+
  theme(legend.position="bottom") 


ggsave(paste0("percevaded_genderxtreatment", v, ".pdf"), path=fig.path, width = 14, height = 10)





#####################
### Die analysis
#####################
library(effsize)
library( clinfun)

#General comparison
cdat.p1<-subset(df, !is.na(numberEntered) & !duplicated(subj_id)) # Eliminate missing and duplicate observations

### Proportion declaring each value of the die by gender
prop.t<-prop.table(table(cdat.p1$numberEntered, cdat.p1$gender_lab), 2)

# Female over 5 in die
x<-1/6
(prop.t[5,1]-x) + (prop.t[6,1]-x)

# Male over 5 in die
(prop.t[5,2]-x) + (prop.t[6,2]-x)


cdat.p1$g<-as.numeric(factor(cdat.p1$gender_lab)) #Numeric version of gender variable (2 male, 1 female)

female<- cdat.p1$numberEntered[cdat.p1$gender_lab=="Female"]
male<- cdat.p1$numberEntered[cdat.p1$gender_lab=="Male"]

# Tests 
t.test(female, male)

cohen.d(cdat.p1$numberEntered ~ cdat.p1$gender_lab, paired=FALSE)

ks.test(female, male, alternative = "two.sided")

#wilcox.test(female, male, paired = F, alternative = "two.sided") ## For small samples, does not really apply to a sample of 652


VD.A(cdat.p1$numberEntered ~ cdat.p1$gender_lab)

# For count data, does not really apply to gender  
#jonckheere.test(cdat.p1$numberEntered, cdat.p1$g, alternative = "two.sided", nperm = 1000)


#------------
# Die Figure
#-------------
pt<-prop.table(table(cdat.p1$numberEntered, cdat.p1$gender_lab, cdat.p1$country), c(3,2))

die<-as.data.frame(pt)

names(die)<-c("die", "gender" ,"country", "prop" )


d<-ggplot(die, aes(x = die, y = prop, colour=country)) + geom_bar(stat = "identity") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent , limits = c(0,0.7)) +
  xlab("") + labs(colour="") +
  geom_hline(yintercept = 1/6, lty="dashed", col="red")+
  facet_wrap(~  country + gender, ncol = 2) +  theme(legend.position="bottom")
d

ggsave(paste0("die_result", v,".pdf"), path=fig.path, width = 8, height = 7)










