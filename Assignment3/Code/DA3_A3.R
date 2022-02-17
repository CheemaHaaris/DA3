#----------------------------------

#CLEAR MEMORY
rm(list=ls())

library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
library(gmodels)
library(lspline)
library(sandwich)
library(modelsummary)
library(rattle)
library(caret)
library(pROC)
library(ranger)
library(rpart)
library(partykit)
library(rpart.plot)
library(tidyverse)
library(viridis)
library(rattle)
library(fixest)


# Import Data -------------------------------------------------------------

data <- read_csv('cs_bisnode_panel.csv')

to_filter <- sapply(data, function(x) sum(is.na(x)))
sort(to_filter[to_filter > 0])

#----Drop variables with too many NAs,filtering years and sufficient balance sheet length per year
data <- data %>%
  select(-c(COGS, finished_prod, net_dom_sales, net_exp_sales, wages, D)) %>%
  filter(year >= 2010,
         year <= 2015)

#----Label Engineering -------------------------------------------------------

data <- data %>% complete(year, comp_id)

#----Generate status_alive to check the firm is still alive
data  <- data %>%
  mutate(status_alive = sales > 0 & !is.na(sales) %>%
           as.numeric(.))


#----Size and growth 

#--Sales also in negative so taking log
#summary(data$sales)

data <- data %>%
  mutate(sales = ifelse(sales < 0, 1, sales),
         ln_sales = ifelse(sales > 0, log(sales), 0),
         sales_mil=sales/1000000,
         sales_mil_log = ifelse(sales > 0, log(sales_mil), 0),
         sales_mil_log_sq = ifelse(sales > 0, (sales_mil_log)^2, 0),
         total_assets_bs = intang_assets + curr_assets + fixed_assets)


#--Difference Variables that are additional

data <- data %>%
  group_by(comp_id) %>%
  mutate(d1_sales_mil_log = (sales_mil_log - Lag(sales_mil_log, 1)), #change in sales
         d1_inventories = (inventories - lag(inventories,1))/sales,
         d1_total_assets_bs = (total_assets_bs - lag(total_assets_bs,1))/sales,
         d1_amort = (amort - lag(amort,1))/sales ) %>%
  ungroup()

#--Replace with 0 for new firms + add dummy to capture it
data <- data %>%
  mutate(age = (year - founded_year) %>%
           ifelse(. < 0, 0, .),
         new = as.numeric(age <= 1) %>% #  (age could be 0,1 )
           ifelse(balsheet_notfullyear == 1, 1, .),
         d1_sales_mil_log = ifelse(new == 1, 0, d1_sales_mil_log),
        # d1_inventories = ifelse(new == 1, 0, d1_inventories),
        # d1_total_assets_bs = ifelse(new == 1, 0, d1_total_assets_bs),
        # d1_amort = ifelse(new == 1, 0, d1_amort),
         new = ifelse(is.na(d1_sales_mil_log), 1, new),
         d1_sales_mil_log = ifelse(is.na(d1_sales_mil_log), 0, d1_sales_mil_log),
        # d1_inventories = ifelse(is.na(d1_inventories), 0, d1_inventories),
        # d1_total_assets_bs = ifelse(is.na(d1_total_assets_bs), 0, d1_total_assets_bs),
        # d1_amort = ifelse(is.na(d1_amort), 0, d1_amort)
  )


#---Filter Sales

# look at firms below 10m euro revenues and above 1000 euros
data <- data %>%
  filter(status_alive == 1) %>%
  filter(!(sales_mil > 10)) %>%
  filter(!(sales_mil < 0.001))

# Keep only firms with data for the 3 years
#data <- data %>% group_by(comp_id) %>% filter(n() == 6)

#----Winsorizing-----------------------------------------------

data <- data %>%
  mutate(flag_low_d1_sales_mil_log = ifelse(d1_sales_mil_log < -1.5, 1, 0),
         flag_high_d1_sales_mil_log = ifelse(d1_sales_mil_log > 1.5, 1, 0),
         d1_sales_mil_log_mod = ifelse(d1_sales_mil_log < -1.5, -1.5,
                                       ifelse(d1_sales_mil_log > 1.5, 1.5, d1_sales_mil_log)),
         d1_sales_mil_log_mod_sq = d1_sales_mil_log_mod^2
  )



#----Target Variable: CAGR sales change in the last 2 years
data <- data %>%
  group_by(comp_id) %>%
  mutate(cagr_sales = ((lead(sales_mil,2) / sales_mil)^(1/2)-1)*100)


#----Distribution of target variable
dist_cagr <- ggplot(data=data, aes(x=cagr_sales)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
                 color = "black", fill = "deepskyblue4") +
  coord_cartesian(xlim = c(-100, 200)) +
  labs(x = "CAGR growth",y = "Percent")+
  #scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.03), labels = scales::percent_format(1)) +
  #scale_x_continuous(expand = c(0.00,0.00),limits=c(0,500), breaks = seq(0,500, 50)) +
  theme_bw() 

# Create fast growth dummy
data <- data %>%
  group_by(comp_id) %>%
  mutate(fast_growth = (cagr_sales > 25) %>%
           as.numeric(.)) %>%
  ungroup()

table(data$fast_growth)

#----Sample design-------------------------------------------------------------


#-Cross-sectional Data 
data <- data %>%
  filter(year == 2013,
         status_alive == 1,
         cagr_sales != is.na(cagr_sales))

Hmisc::describe(data$cagr_sales)


#-------Feature engineering----------------------------------------------------------------
sort(unique(data$ind2))
table(data$ind2==61)

#Services - 72, 81
str(data$ind2)
#data <- read_csv(paste0(data_out,"work5.csv"))

library(data.table)

#--Change some industry category codes
data <- data %>%
  mutate(sub_sample = ifelse(ind2 == 31 | ind2 ==32 | ind2==33, "manufacturing", ifelse(ind2==72| ind2==81|ind2==56,"services","other")))

unique(data$sub_sample)
table(data$sub_sample)

#--Change some industry category codes
data <- data %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
  )


table(data$ind2_cat)

# Firm characteristics
data <- data %>%
  mutate(age2 = age^2,
         foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))


#----Look at more financial variables, create ratios


#--Assets can't be negative. Change them to 0 and add a flag.
data <-data  %>%
  mutate(flag_asset_problem=ifelse(intang_assets<0 | curr_assets<0 | fixed_assets<0,1,0  ))
table(data$flag_asset_problem)

data <- data %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))


pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit_loss_year", "personnel_exp")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets" )

#--Divide all pl_names elements by sales and create new column for it
data <- data %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
data <- data %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))


#----Creating flags, and winsorizing tails----------------------------------------------


# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

data <- data %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))


# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "inc_bef_tax_pl", "profit_loss_year_pl", "share_eq_bs")

data <- data %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))


# dropping flags with no variation
variances<- data %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

data <- data %>%
  select(-one_of(names(variances)[variances]))

#---- Additional including some imputation-----------------------------

# CEO age
data <- data %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

data <- data %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))

# number emp, very noisy measure
data <- data %>%
  mutate(labor_avg_mod = ifelse(is.na(labor_avg), mean(labor_avg, na.rm = TRUE), labor_avg),
         flag_miss_labor_avg = as.numeric(is.na(labor_avg)))

summary(data$labor_avg)
summary(data$labor_avg_mod)

data <- data %>%
  select(-labor_avg)

# create factors
data <- data %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(data$ind2_cat))))

data <- data %>%
  mutate(fast_growth_f = factor(fast_growth, levels = c(0,1)) %>%
           recode(., `0` = 'no_fast_growth', `1` = "fast_growth"))

# no more imputation, drop obs if key vars missing
data <- data %>%
  filter(!is.na(liq_assets_bs),!is.na(foreign), !is.na(ind))

# drop missing
data <- data %>%
  filter(!is.na(age),!is.na(foreign), !is.na(material_exp_pl), !is.na(m_region_loc),
         !is.na(d1_total_assets_bs),!is.na(d1_inventories),!is.na(d1_amort))

# drop unused factor levels
data <- data %>%
  mutate_at(vars(colnames(data)[sapply(data, is.factor)]), funs(fct_drop))

#----Sales---------------------------------------------------------- 

ggplot(data = data, aes(x=sales_mil_log, y=as.numeric(fast_growth))) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method = "gam", formula = y ~ poly(x,2), color="navyblue", se = F, size=1)+
  geom_smooth(method="gam", se=F, colour="red", size=1.5, span=0.9) +
  labs(x = "sales_mil_log",y = "fast growth") +
  theme_bw()

ols_s <- lm(fast_growth~sales_mil_log+sales_mil_log_sq,
            data = data)
summary(ols_s)

#---- Graph--------------------------------------------------------
summary(data$inc_bef_tax_pl)
ggplot(data = data, aes(x=inc_bef_tax_pl, y=as.numeric(fast_growth))) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method="loess", se=F, colour="black", size=1.5, span=0.9) +
  labs(x = "Income before taxes",y = "Fast Growth distribution") +
  theme_bw() +
  scale_x_continuous(limits = c(-1.5,1.5), breaks = seq(-1.5,1.5, 0.5))


# lowess
Hmisc::describe(data$d1_sales_mil_log) # no missing

ggplot(data = data, aes(x=d1_sales_mil_log, y=as.numeric(fast_growth))) +
  geom_point(size=0.1,  shape=20, stroke=2, fill='blue', color='black') +
  geom_smooth(method="loess", se=F, colour='red', size=1.5, span=0.9) +
  labs(x = "Growth rate (Diff of ln sales)",y = "default") +
  theme_bw() +
  scale_x_continuous(limits = c(-6,10), breaks = seq(-5,10, 5))

ggplot(data = data, aes(x=d1_sales_mil_log_mod, y=as.numeric(fast_growth))) +
  geom_point(size=0.1,  shape=20, stroke=2, fill='blue', color='black') +
  geom_smooth(method="loess", se=F, colour='red', size=1.5, span=0.9) +
  labs(x = "Growth rate (Diff of ln sales)",y = "fast growth") +
  theme_bw() +
  scale_x_continuous(limits = c(-1.5,1.5), breaks = seq(-1.5,1.5, 0.5))

ggplot(data = data, aes(x=d1_sales_mil_log, y=d1_sales_mil_log_mod)) +
  geom_point(size=0.1,  shape=20, stroke=2, fill='blue', color='black') +
  labs(x = "Growth rate (Diff of ln sales) (original)",y = "Growth rate (Diff of ln sales) (winsorized)") +
  theme_bw() +
  scale_x_continuous(limits = c(-5,5), breaks = seq(-5,5, 1)) +
  scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3, 1))



#--------------------------------------------MODELS---------------------------------------------


#----Run a script from github page
library(devtools)
devtools::source_url('https://raw.githubusercontent.com/regulyagoston/BA21_Coding/main/Class_4/codes/seminar_5_auxfuncs.R')

ggplot( data = data , aes( x = fast_growth ) ) +
  geom_histogram( aes( y = ..count.. / sum( count ) ) , size = 0.1 , fill = 'navyblue',
                  bins = 3)+
  labs(y='Probabilities',x='0: No Fast Growth, 1: Fast Growth')+
  ylim(0,1) +
  theme_bw()


#-------Model Building

#----Define variable sets for modelling

#--Main firm variables
rawvars <-  c("curr_assets", "curr_liab", "extra_exp", "extra_inc", "extra_profit_loss", "fixed_assets",
              "inc_bef_tax", "intang_assets", "inventories", "liq_assets", "material_exp", "personnel_exp",
              "profit_loss_year", "sales", "share_eq", "subscribed_cap")

#--Further financial variables
qualityvars <- c("balsheet_flag", "balsheet_length", "balsheet_notfullyear")

engvar <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs",
            "share_eq_bs", "subscribed_cap_bs", "intang_assets_bs", "extra_exp_pl",
            "extra_inc_pl", "extra_profit_loss_pl", "inc_bef_tax_pl", "inventories_pl",
            "material_exp_pl", "profit_loss_year_pl", "personnel_exp_pl")

engvar2 <- c("extra_profit_loss_pl_quad", "inc_bef_tax_pl_quad",
             "profit_loss_year_pl_quad", "share_eq_bs_quad")

#--Flag variables
engvar3 <- c(grep("*flag_low$", names(data), value = TRUE),
             grep("*flag_high$", names(data), value = TRUE),
             grep("*flag_error$", names(data), value = TRUE),
             grep("*flag_zero$", names(data), value = TRUE))

#--Growth variables
d1 <-  c("d1_sales_mil_log_mod", "d1_sales_mil_log_mod_sq",
         "flag_low_d1_sales_mil_log", "flag_high_d1_sales_mil_log")

#--Human capital related variables
hr <- c("female", "ceo_age", "flag_high_ceo_age", "flag_low_ceo_age",
        "flag_miss_ceo_age", "ceo_count", "labor_avg_mod",
        "flag_miss_labor_avg", "foreign_management")

#--Firms history related variables
firm <- c("age", "age2", "new", "ind2_cat", "m_region_loc", "urban_m")

#--Interactions for logit, LASSO
interactions1 <- c("ind2_cat*age", "ind2_cat*age2",
                   "ind2_cat*d1_sales_mil_log_mod", "ind2_cat*sales_mil_log",
                   "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                   "ind2_cat*female",   "ind2_cat*urban_m", "ind2_cat*labor_avg_mod")
interactions2 <- c("sales_mil_log*age", "sales_mil_log*female",
                   "sales_mil_log*profit_loss_year_pl", "sales_mil_log*foreign_management")

#----Model setups

#--Simple logit models 
X1 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl", "ind2_cat")
X2 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl",
        "fixed_assets_bs","share_eq_bs","curr_liab_bs ",   "curr_liab_bs_flag_high ", 
        "curr_liab_bs_flag_error",  "age","foreign_management" , "ind2_cat")
X3 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, d1)
X4 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3, d1, hr, qualityvars)
X5 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3, d1, hr, qualityvars, interactions1, interactions2)

#--Logit+LASSO
logitvars <- c("sales_mil_log", "sales_mil_log_sq", engvar, engvar2, engvar3, d1, hr, firm, qualityvars, interactions1, interactions2)

#--CART and RF (no interactions, no modified features)
rfvars  <-  c("sales_mil", "d1_sales_mil_log", rawvars, hr, firm, qualityvars)

#-----------------------------------------------------------------------------------------
#---Quick reminder about probability models:


#--Linear probability model
ols_modelx2 <- feols(formula(paste0("fast_growth ~", paste0(X2, collapse = " + "))),
                     data = data , vcov = 'hetero')
summary(ols_modelx2)

# Logit model
glm_modelx2 <- glm(formula(paste0("fast_growth ~", paste0(X2, collapse = " + "))),
                   data = data, family = "binomial")
summary(glm_modelx2)


# With Logit we need to calculate average marginal effects (dy/dx)
#   to be able to interpret the coefficients (under some assumptions...)
# Note: vce="none" makes it run much faster, here we do not need variances
#   but if you do need it (e.g. prediction intervals -> take the time!)
mx2 <- margins(glm_modelx2, vce = "none")

sum_table <- summary(glm_modelx2) %>%
  coef() %>%
  as.data.frame() %>%
  select(Estimate) %>%
  mutate(factor = row.names(.)) %>%
  merge(summary(mx2)[,c("factor","AME")])
sum_table

#------------------------------------------------------------------------------------------


######################
# STEP 0)
#   separate datasets

set.seed(13505)
# Create train and holdout samples
train_indices <- as.integer(createDataPartition(data$fast_growth, p = 0.8, list = FALSE))
data_train    <- data[train_indices, ]
data_holdout  <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)



#######################################################
# Step 1) Predict probabilities 
#   with logit + logit and LASSO models
#     using CV


# 5 fold cross-validation:
#   check the summary function
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)

####
# a) Cross-Validate Logit Models 
logit_model_vars <- list("X1" = X1, "X2" = X2, "X3" = X3, "X4" = X4, "X5" = X5)

CV_RMSE_folds <- list()
logit_models <- list()

for (model_name in names(logit_model_vars)) {
  
  # setting the variables for each model
  features <- logit_model_vars[[model_name]]
  
  # Estimate logit model with 5-fold CV
  set.seed(13505)
  glm_model <- train(
    formula(paste0("fast_growth_f ~", paste0(features, collapse = " + "))),
    method    = "glm",
    data      = data_train,
    family    = binomial,
    trControl = train_control
  )
  
  # Save the results to list
  logit_models[[model_name]] <- glm_model
  # Save RMSE on test for each fold
  CV_RMSE_folds[[model_name]] <- glm_model$resample[,c("Resample", "RMSE")]
  
}

#####
# b) Logit + LASSO

# Set lambda parameters to check
lambda <- 10^seq(-1, -4, length = 10)
grid <- expand.grid("alpha" = 1, lambda = lambda)

# Estimate logit + LASSO with 5-fold CV to find lambda
set.seed(13505)
system.time({
  logit_lasso_model <- train(
    formula(paste0("fast_growth_f ~", paste0(logitvars, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    family = "binomial",
    trControl = train_control,
    tuneGrid = grid,
    na.action=na.exclude
  )
})

# Save the results
tuned_logit_lasso_model <- logit_lasso_model$finalModel
best_lambda <- logit_lasso_model$bestTune$lambda
logit_models[["LASSO"]] <- logit_lasso_model
lasso_coeffs <- as.matrix(coef(tuned_logit_lasso_model, best_lambda))
CV_RMSE_folds[["LASSO"]] <- logit_lasso_model$resample[,c("Resample", "RMSE")]

#logit_models - LASSO is giving the lowest RMSE after checking

#############################################
# Step 2)
#  Calibration Curve, Confusion Matrix,
#     ROC, AUC, 

### 
# a) For demonstration, let us use Logit Model 4 (which will turn out to be the 'best')
#     Estimate RMSE on holdout sample

best_logit_no_loss <- logit_models[["X4"]]

logit_predicted_probabilities_holdout    <- predict(best_logit_no_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_logit_no_loss_pred"] <- logit_predicted_probabilities_holdout[,"fast_growth"]
RMSE(data_holdout[, "best_logit_no_loss_pred", drop=TRUE], data_holdout$fast_growth)

####
# a) Old friend: calibration curve:
# how well do estimated vs actual event probabilities relate to each other?

calibration_X4 <- create_calibration_plot(data_holdout, 
                        prob_var = "best_logit_no_loss_pred", 
                        actual_var = "fast_growth",
                        n_bins = 10)

####
# b) Confusion Matrix, with (arbitrarily) chosen threshold(s)

# default: the threshold 0.5 is used to convert probabilities to binary classes
logit_class_prediction <- predict(best_logit_no_loss, newdata = data_holdout)
summary(logit_class_prediction)

# Confusion matrix: summarize different type of errors and successfully predicted cases
# positive = "yes": explicitly specify the positive case
cm_object1 <- confusionMatrix(logit_class_prediction, data_holdout$fast_growth_f, positive = "fast_growth")
cm_object1
cm1 <- cm_object1$table
cm1

# we can apply different thresholds:
# 0.5 same as before
holdout_prediction <-
  ifelse(data_holdout$best_logit_no_loss_pred < 0.5, "no_fast_growth", "fast_growth") %>%
  factor(levels = c("no_fast_growth", "fast_growth"))
cm_object1b <- confusionMatrix(holdout_prediction,data_holdout$fast_growth_f)
cm1b <- cm_object1b$table
cm1b

# calculating the probabilities
cm1b_p <- cm1b / sum(cm1b)
cm1b_p <- cbind( cm1b_p , c( sum(cm1b_p[1,]), sum(cm1b_p[2,]) ) )
cm1b_p <- rbind( cm1b_p , c( sum(cm1b_p[,1]), sum(cm1b_p[,2]), sum(cm1b_p[,3]) ) )
cm1b_p <- round( cm1b_p*100 , 1 )
cm1b_p


# a sensible choice: mean of predicted probabilities
mean_predicted_default_prob <- mean(data_holdout$best_logit_no_loss_pred)
mean_predicted_default_prob
holdout_prediction <-
  ifelse(data_holdout$best_logit_no_loss_pred < mean_predicted_default_prob, "no_fast_growth", "fast_growth") %>%
  factor(levels = c("no_fast_growth", "fast_growth"))
cm_object2 <- confusionMatrix(holdout_prediction,data_holdout$fast_growth_f)
cm2 <- cm_object2$table

cm1
cm2

# c) Visualize ROC (with thresholds in steps) on holdout
#     what if we want to compare multiple thresholds?
#   Get AUC - how good our model is in terms of classification error?

thresholds <- seq(0.05, 0.75, by = 0.05)

# pre allocate lists
cm <- list()
true_positive_rates <- c()
false_positive_rates <- c()
for (thr in thresholds) {
  # get holdout prediction
  holdout_prediction <- ifelse(data_holdout[,"best_logit_no_loss_pred"] < thr, "no_fast_growth", "fast_growth") %>%
    factor(levels = c("no_fast_growth", "fast_growth"))
  # create confusion Martrix
  cm_thr <- confusionMatrix(holdout_prediction,data_holdout$fast_growth_f)$table
  cm[[as.character(thr)]] <- cm_thr
  # Categorize to true positive/false positive
  true_positive_rates <- c(true_positive_rates, cm_thr["fast_growth", "fast_growth"] /
                             (cm_thr["fast_growth", "fast_growth"] + cm_thr["no_fast_growth", "fast_growth"]))
  false_positive_rates <- c(false_positive_rates, cm_thr["fast_growth", "no_fast_growth"] /
                              (cm_thr["fast_growth", "no_fast_growth"] + cm_thr["no_fast_growth", "no_fast_growth"]))
}

# create a tibble for results
tpr_fpr_for_thresholds <- tibble(
  "threshold" = thresholds,
  "true_positive_rate"  = true_positive_rates,
  "false_positive_rate" = false_positive_rates
)

# Plot discrete ROC
ROC_1 <- ggplot(
  data = tpr_fpr_for_thresholds,
  aes(x = false_positive_rate, y = true_positive_rate, color = threshold)) +
  labs(x = "False positive rate (1 - Specificity)", y = "True positive rate (Sensitivity)") +
  geom_point(size=2, alpha=0.8) +
  scale_color_viridis(option = "D", direction = -1) +
  scale_x_continuous(expand = c(0.01,0.01), limit=c(0,1), breaks = seq(0,1,0.1)) +
  scale_y_continuous(expand = c(0.01,0.01), limit=c(0,1), breaks = seq(0,1,0.1)) +
  theme_bw() +
  theme(legend.position ="right") +
  theme(legend.title = element_text(size = 4), 
        legend.text = element_text(size = 4),
        legend.key.size = unit(.4, "cm")) 


# Or with a fairly easy commands, we can plot, the
#   continuous ROC on holdout with Logit 4
roc_obj_holdout <- roc(data_holdout$fast_growth, data_holdout$best_logit_no_loss_pred, quiet = T)
# use aux function
ROC_2 <- createRocPlot(roc_obj_holdout, "best_logit_no_loss_roc_plot_holdout")
# and quantify the AUC (Area Under the (ROC) Curve)
roc_obj_holdout$auc


# d) Calculate the ROC Curve and calculate AUC for each folds
#   We can compare different competing models based on AUC measure

# d1) calculate AUC for each fold
CV_AUC_folds <- list()
for (model_name in names(logit_models)) {
  
  auc <- list()
  model <- logit_models[[model_name]]
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    # get the prediction from each fold
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    # calculate the roc curve
    roc_obj <- roc(cv_fold$obs, cv_fold$fast_growth, quiet = TRUE)
    # save the AUC value
    auc[[fold]] <- as.numeric(roc_obj$auc)
  }
  
  CV_AUC_folds[[model_name]] <- data.frame("Resample" = names(auc),
                                           "AUC" = unlist(auc))
}

####
# d2) for each model: 
#     average RMSE and average AUC for each models

CV_RMSE <- list()
CV_AUC <- list()
mean(CV_RMSE_folds$X4$RMSE)

for (model_name in names(logit_models)) {
  CV_RMSE[[model_name]] <- mean(CV_RMSE_folds[[model_name]]$RMSE)
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}

# We have 6 models, (5 logit and the logit lasso). For each we have a 5-CV RMSE and AUC.
# We pick our preferred model based on that.

nvars <- lapply(logit_models, FUN = function(x) length(x$coefnames))
# quick adjustment for LASSO
nvars[["LASSO"]] <- sum(lasso_coeffs != 0)

logit_summary1 <- data.frame("Number of predictors" = unlist(nvars),
                             "CV RMSE" = unlist(CV_RMSE),
                             "CV AUC" = unlist(CV_AUC))

# Summary for average RMSE and AUC for each model on the test sample
logit_summary1


#############################################x
# Step 3)

# We have a loss function
#

# Introduce loss function
# relative cost of of a false negative classification (as compared with a false positive classification)
FP=1
FN=3
cost = FN/FP
# the prevalence, or the proportion of cases in the population (n.cases/(n.controls+n.cases))
prevelance = sum(data_train$fast_growth)/length(data_train$fast_growth)

# Draw ROC Curve and find optimal threshold WITH loss function

best_tresholds <- list()
expected_loss <- list()
logit_cv_rocs <- list()
logit_cv_threshold <- list()
logit_cv_expected_loss <- list()

# Iterate through:
#  a) models
#  b) Folds
for (model_name in names(logit_models)) {
  
  model <- logit_models[[model_name]]
  colname <- paste0(model_name,"_prediction")
  
  best_tresholds_cv <- list()
  expected_loss_cv <- list()
  
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$fast_growth, quiet = TRUE)
    # Add the weights (costs) here!
    best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                            best.method="youden", best.weights=c(cost, prevelance))
    # save best treshold for each fold and save the expected loss value
    best_tresholds_cv[[fold]] <- best_treshold$threshold
    expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$fast_growth)
  }
  
  # average
  best_tresholds[[model_name]] <- mean(unlist(best_tresholds_cv))
  expected_loss[[model_name]]  <- mean(unlist(expected_loss_cv))
  
  # for fold #5
  logit_cv_rocs[[model_name]] <- roc_obj
  logit_cv_threshold[[model_name]] <- best_treshold
  logit_cv_expected_loss[[model_name]] <- expected_loss_cv[[fold]]
  
}

logit_summary2 <- data.frame("Avg of optimal thresholds" = unlist(best_tresholds),
                             "Threshold for Fold5" = sapply(logit_cv_threshold, function(x) {x$threshold}),
                             "Avg expected loss" = unlist(expected_loss),
                             "Expected loss for Fold5" = unlist(logit_cv_expected_loss))
logit_summary1
logit_summary2

#####
# Example:
#   Create plots for Logit M4 - training sample

# get the ROC properties
r <- logit_cv_rocs[["X4"]]
# get coordinates and properties of the choosen threshold
best_coords <- logit_cv_threshold[["X4"]]
# plot for Loss function
createLossPlot(r, best_coords,
               paste0("X4", "_loss_plot"))
# Plot for optimal ROC
createRocPlotWithOptimal(r, best_coords,
                         paste0("X4", "_roc_plot"))


####
# Pick best model based on average expected loss
#   Calculate the expected loss on holdout sample

# Get model with optimal threshold
best_logit_with_loss <- logit_models[["X4"]]
best_logit_optimal_treshold <- best_tresholds[["X4"]]

# Predict the probabilities on holdout
logit_predicted_probabilities_holdout      <- predict(best_logit_with_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_logit_with_loss_pred"] <- logit_predicted_probabilities_holdout[,"fast_growth"]

# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout$fast_growth, data_holdout[, "best_logit_with_loss_pred", drop=TRUE],quiet = TRUE)

# Get expected loss on holdout:
holdout_treshold <- coords(roc_obj_holdout, x = best_logit_optimal_treshold, input= "threshold",
                           ret="all", transpose = FALSE)
# Calculate the expected loss on holdout sample
expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$fast_growth)
expected_loss_holdout

# Confusion table on holdout with optimal threshold
holdout_prediction <-
  ifelse(data_holdout$best_logit_with_loss_pred < best_logit_optimal_treshold, "no_fast_growth", "fast_growth") %>%
  factor(levels = c("no_fast_growth", "fast_growth"))
cm_object3 <- confusionMatrix(holdout_prediction,data_holdout$fast_growth_f)
cm3 <- cm_object3$table
cm3
# in pctg
round( cm3 / sum(cm3) * 100 , 1 )

#################################################

#----Step 4)
#--PREDICTION WITH RANDOM FOREST (and CART)

#-warm up with CART

data_for_graph <- data_train
levels(data_for_graph$fast_growth_f) <- list("no growth" = "no_fast_growth", "growth" = "fast_growth")

# First a simple CART (with pre-set cp and minbucket)
set.seed(13505)
rf_for_graph <-
  rpart(
    formula = fast_growth_f ~ sales_mil + profit_loss_year+ foreign_management,
    data = data_for_graph,
    control = rpart.control(cp = 0.0028, minbucket = 100)
  )

rpart.plot(rf_for_graph, tweak=1, digits=2, extra=107, under = TRUE)



#################################################
# A) Probability forest
#     Split by gini, ratio of 1's in each tree, 
#      and average over trees


# 5 fold cross-validation
train_control <- trainControl(
  method = "cv",
  n = 5,
  classProbs = TRUE, # same as probability = TRUE in ranger
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)
train_control$verboseIter <- TRUE

# Tuning parameters -> now only check for one setup, 
#     but you can play around with the rest, which is uncommented
tune_grid <- expand.grid(
  .mtry = 5, # c(5, 6, 7),
  .splitrule = "gini",
  .min.node.size = 15 # c(10, 15)
)

# By default ranger understoods that the outcome is binary, 
#   thus needs to use 'gini index' to decide split rule
# getModelInfo("ranger")
set.seed(13505)
rf_model_p <- train(
  formula(paste0("fast_growth_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control
)

rf_model_p$results

best_mtry <- rf_model_p$bestTune$mtry
best_min_node_size <- rf_model_p$bestTune$min.node.size

# Get average (ie over the folds) RMSE and AUC
CV_RMSE_folds[["rf_p"]] <- rf_model_p$resample[,c("Resample", "RMSE")]

auc <- list()
for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <-
    rf_model_p$pred %>%
    filter(Resample == fold)
  
  roc_obj <- roc(cv_fold$obs, cv_fold$fast_growth, quiet = TRUE)
  auc[[fold]] <- as.numeric(roc_obj$auc)
}
CV_AUC_folds[["rf_p"]] <- data.frame("Resample" = names(auc),
                                     "AUC" = unlist(auc))

CV_RMSE[["rf_p"]] <- mean(CV_RMSE_folds[["rf_p"]]$RMSE)
CV_AUC[["rf_p"]] <- mean(CV_AUC_folds[["rf_p"]]$AUC)


###
# Now use loss function and search for best thresholds and expected loss over folds
best_tresholds_cv <- list()
expected_loss_cv <- list()

for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <-
    rf_model_p$pred %>%
    filter(mtry == best_mtry,
           min.node.size == best_min_node_size,
           Resample == fold)
  
  roc_obj <- roc(cv_fold$obs, cv_fold$fast_growth, quiet = TRUE)
  best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                          best.method="youden", best.weights=c(cost, prevelance))
  best_tresholds_cv[[fold]] <- best_treshold$threshold
  expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$fast_growth)
}

# average
best_tresholds[["rf_p"]] <- mean(unlist(best_tresholds_cv))
expected_loss[["rf_p"]] <- mean(unlist(expected_loss_cv))


rf_summary <- data.frame("CV RMSE" = CV_RMSE[["rf_p"]],
                         "CV AUC" = CV_AUC[["rf_p"]],
                         "Avg of optimal thresholds" = best_tresholds[["rf_p"]],
                         "Threshold for Fold5" = best_treshold$threshold,
                         "Avg expected loss" = expected_loss[["rf_p"]],
                         "Expected loss for Fold5" = expected_loss_cv[[fold]])

rf_summary

###
# Create plots - this is for Fold5

createLossPlot(roc_obj, best_treshold, "rf_p_loss_plot")
createRocPlotWithOptimal(roc_obj, best_treshold, "rf_p_roc_plot")

####
# Take model to holdout and estimate RMSE, AUC and expected loss
rf_predicted_probabilities_holdout <- predict(rf_model_p, newdata = data_holdout, type = "prob")
data_holdout$rf_p_prediction <- rf_predicted_probabilities_holdout[,"fast_growth"]
RMSE(data_holdout$rf_p_prediction, data_holdout$fast_growth)

# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout$fast_growth, data_holdout[, "rf_p_prediction", drop=TRUE], quiet=TRUE)

# AUC
as.numeric(roc_obj_holdout$auc)

# Get expected loss on holdout with optimal threshold
holdout_treshold <- coords(roc_obj_holdout, x = best_tresholds[["rf_p"]] , input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss_holdout_rf <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$fast_growth)
expected_loss_holdout_rf

# Confusion matrix
holdout_prediction_rf <-
  ifelse(data_holdout$rf_p_prediction < best_tresholds[["rf_p"]],  "no_fast_growth", "fast_growth") %>%
  factor(levels = c("no_fast_growth", "fast_growth"))
cm_object_rf <- confusionMatrix(holdout_prediction_rf,data_holdout$fast_growth_f)
cm_rf <- cm_object_rf$table
cm_rf
# in pctg
round( cm_rf / sum(cm_rf) * 100 , 1 )


# Summary results

nvars[["rf_p"]] <- length(rfvars)

summary_results <- data.frame("Number of predictors" = unlist(nvars),
                              "CV RMSE" = unlist(CV_RMSE),
                              "CV AUC" = unlist(CV_AUC),
                              "CV threshold" = unlist(best_tresholds),
                              "CV expected Loss" = unlist(expected_loss))

model_names <- c("Logit X1", "Logit X4",
                 "Logit LASSO","RF probability")
summary_results <- summary_results %>%
  filter(rownames(.) %in% c("X1", "X4", "LASSO", "rf_p"))
rownames(summary_results) <- model_names

summary_results




#################################################
# B) Classification forest
# Split by Gini, majority vote in each tree, 
#     majority vote over trees
#   Difference: before we have averaged the probabilities
#       now, we grow a tree, make a prediction, based on that we classify each observation
#       to be 0 or 1. We do it for each tree, then check which classification has more 'vote'
#       if there is more 0 then it is going to be classified as 0 and vica-versa.
#
# USE ONLY IF AGNOSTIC ABOUT THE EXPECTED LOSS!!! 

train_control <- trainControl(
  method = "cv",
  n = 5
)
train_control$verboseIter <- TRUE

set.seed(13505)
rf_model_f <- train(
  formula(paste0("fast_growth_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control
)

# Predict on both samples
data_train$rf_f_prediction_class   <- predict(rf_model_f, type = "raw")
data_holdout$rf_f_prediction_class <- predict(rf_model_f, newdata = data_holdout, type = "raw")

# We use predicted classes to calculate expected loss based on our loss fn
fp <- sum(data_holdout$rf_f_prediction_class == "fast_growth"    & data_holdout$fast_growth_f == "no_fast_growth")
fn <- sum(data_holdout$rf_f_prediction_class == "no_fast_growth" & data_holdout$fast_growth_f == "fast_growth")
(fp*FP + fn*FN)/length(data_holdout$fast_growth)




####################################################################

#Extra Task 

#Keeping loss function same as before

#________________________________________________________________________________________________________________


data_manu <- subset(data, data$sub_sample=="manufacturing")
data_service <- subset(data, data$sub_sample=="services")


#---------------------------Separate data-sets-----------------------------------------

set.seed(13505)
# Create train and holdout samples
train_indices_manu <- as.integer(createDataPartition(data_manu$fast_growth, p = 0.8, list = FALSE))
data_train_manu    <- data_manu[train_indices_manu, ]
data_holdout_manu  <- data_manu[-train_indices_manu, ]



set.seed(13505)
# Create train and holdout samples
train_indices_service <- as.integer(createDataPartition(data_service$fast_growth, p = 0.8, list = FALSE))
data_train_service   <- data_service[train_indices_service, ]
data_holdout_service  <- data_service[-train_indices_service, ]

#-------------------------------Running Models---------------------------------------------

# 5 fold cross-validation
train_control <- trainControl(
  method = "cv",
  n = 5,
  classProbs = TRUE, # same as probability = TRUE in ranger
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)
train_control$verboseIter <- TRUE

# Tuning parameters -> now only check for one setup, 
#     but you can play around with the rest, which is uncommented
tune_grid <- expand.grid(
  .mtry = 5, # c(5, 6, 7),
  .splitrule = "gini",
  .min.node.size = 15 # c(10, 15)
)


set.seed(13505)
rf_model_p_manu <- train(
  formula(paste0("fast_growth_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train_manu,
  tuneGrid = tune_grid,
  trControl = train_control
)


set.seed(13505)
rf_model_p_service <- train(
  formula(paste0("fast_growth_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train_service,
  tuneGrid = tune_grid,
  trControl = train_control
)

rf_model_p_manu$results
rf_model_p_service$results

#----Classification Manufacturing---------------------------------------------------------

best_mtry <- rf_model_p_manu$bestTune$mtry
best_min_node_size <- rf_model_p_manu$bestTune$min.node.size

# Get average (ie over the folds) RMSE and AUC
CV_RMSE_folds_manu <- list()
CV_RMSE_folds_manu[["rf_p_manu"]] <- rf_model_p_manu$resample[,c("Resample", "RMSE")]
CV_AUC_folds_manu <- list()
CV_RMSE_manu <- list()
CV_AUC_manu <- list()
auc_manu <- list()

for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold_manu <-
    rf_model_p_manu$pred %>%
    filter(Resample == fold)
  
  roc_obj_manu <- roc(cv_fold_manu$obs, cv_fold_manu$fast_growth, quiet = TRUE)
  auc[[fold]] <- as.numeric(roc_obj_manu$auc)
}
CV_AUC_folds_manu[["rf_p_manu"]] <- data.frame("Resample" = names(auc),
                                     "AUC" = unlist(auc))

CV_RMSE_manu[["rf_p_manu"]] <- mean(CV_RMSE_folds_manu[["rf_p_manu"]]$RMSE)
CV_AUC_manu[["rf_p_manu"]] <- mean(CV_AUC_folds_manu[["rf_p_manu"]]$AUC)




###
# Now use loss function and search for best thresholds and expected loss over folds
best_tresholds_cv_manu <- list()
expected_loss_cv_manu <- list()
best_tresholds_manu <- list()
expected_loss_manu <- list()

for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold_manu <-
    rf_model_p_manu$pred %>%
    filter(mtry == best_mtry,
           min.node.size == best_min_node_size,
           Resample == fold)
  
  roc_obj_manu <- roc(cv_fold_manu$obs, cv_fold_manu$fast_growth, quiet = TRUE)
  best_treshold_manu <- coords(roc_obj_manu, "best", ret="all", transpose = FALSE,
                          best.method="youden", best.weights=c(cost, prevelance))
  best_tresholds_cv_manu[[fold]] <- best_treshold_manu$threshold
  expected_loss_cv_manu[[fold]] <- (best_treshold_manu$fp*FP + best_treshold_manu$fn*FN)/length(cv_fold_manu$fast_growth)
}

# average
best_tresholds_manu[["rf_p_manu"]] <- mean(unlist(best_tresholds_cv_manu))
expected_loss_manu[["rf_p_manu"]] <- mean(unlist(expected_loss_cv_manu))


rf_summary_manu <- data.frame("CV RMSE" = CV_RMSE_manu[["rf_p_manu"]],
                         "CV AUC" = CV_AUC_manu[["rf_p_manu"]],
                         "Avg of optimal thresholds" = best_tresholds_manu[["rf_p_manu"]],
                         "Threshold for Fold5" = best_treshold_manu$threshold,
                         "Avg expected loss" = expected_loss_manu[["rf_p_manu"]],
                         "Expected loss for Fold5" = expected_loss_cv_manu[[fold]])

rf_summary_manu

###
# Create plots - this is for Fold5

createLossPlot(roc_obj_manu, best_treshold_manu, "rf_p_loss_plot_manu")
createRocPlotWithOptimal(roc_obj_manu, best_treshold_manu, "rf_p_roc_plot_manu")

####
# Take model to holdout and estimate RMSE, AUC and expected loss
rf_predicted_probabilities_holdout_manu <- predict(rf_model_p_manu, newdata = data_holdout_manu, type = "prob")
data_holdout_manu$rf_p_prediction_manu <- rf_predicted_probabilities_holdout_manu[,"fast_growth"]
RMSE(data_holdout_manu$rf_p_prediction_manu, data_holdout_manu$fast_growth)

# ROC curve on holdout
roc_obj_holdout_manu <- roc(data_holdout_manu$fast_growth, data_holdout_manu[, "rf_p_prediction_manu", drop=TRUE], quiet=TRUE)

# AUC
as.numeric(roc_obj_holdout_manu$auc)

# Get expected loss on holdout with optimal threshold
holdout_treshold_manu <- coords(roc_obj_holdout_manu, x = best_tresholds_manu[["rf_p_manu"]] , input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss_holdout_manu <- (holdout_treshold_manu$fp*FP + holdout_treshold_manu$fn*FN)/length(data_holdout_manu$fast_growth)
expected_loss_holdout_manu


# Confusion table on holdout with optimal threshold
holdout_prediction_manu <-
  ifelse(data_holdout_manu$rf_p_prediction_manu < best_tresholds_manu,  "no_fast_growth", "fast_growth") %>%
  factor(levels = c("no_fast_growth", "fast_growth"))
cm_object_manu <- confusionMatrix(holdout_prediction_manu,data_holdout_manu$fast_growth_f)
cm_object_manu
cm_manu <- cm_object_manu$table
cm_manu
# in pctg
round( cm_manu / sum(cm_manu) * 100 , 1 )


#----Classification Services---------------------------------------------------------

best_mtry <- rf_model_p_service$bestTune$mtry
best_min_node_size <- rf_model_p_service$bestTune$min.node.size

# Get average (ie over the folds) RMSE and AUC
CV_RMSE_folds_service <- list()
CV_RMSE_folds_service[["rf_p_service"]] <- rf_model_p_service$resample[,c("Resample", "RMSE")]
CV_AUC_folds_service <- list()
CV_RMSE_service <- list()
CV_AUC_service <- list()
auc_service <- list()

for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold_service <-
    rf_model_p_service$pred %>%
    filter(Resample == fold)
  
  roc_obj_service <- roc(cv_fold_service$obs, cv_fold_service$fast_growth, quiet = TRUE)
  auc[[fold]] <- as.numeric(roc_obj_service$auc)
}
CV_AUC_folds_service[["rf_p_service"]] <- data.frame("Resample" = names(auc),
                                               "AUC" = unlist(auc))

CV_RMSE_service[["rf_p_service"]] <- mean(CV_RMSE_folds_service[["rf_p_service"]]$RMSE)
CV_AUC_service[["rf_p_service"]] <- mean(CV_AUC_folds_service[["rf_p_service"]]$AUC)


###
# Now use loss function and search for best thresholds and expected loss over folds
best_tresholds_cv_service <- list()
expected_loss_cv_service <- list()
best_tresholds_service <- list()
expected_loss_service <- list()

for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold_service <-
    rf_model_p_service$pred %>%
    filter(mtry == best_mtry,
           min.node.size == best_min_node_size,
           Resample == fold)
  
  roc_obj_service <- roc(cv_fold_service$obs, cv_fold_service$fast_growth, quiet = TRUE)
  best_treshold_service <- coords(roc_obj_service, "best", ret="all", transpose = FALSE,
                               best.method="youden", best.weights=c(cost, prevelance))
  best_tresholds_cv_service[[fold]] <- best_treshold_service$threshold
  expected_loss_cv_service[[fold]] <- (best_treshold_service$fp*FP + best_treshold_service$fn*FN)/length(cv_fold_service$fast_growth)
}

# average
best_tresholds_service[["rf_p_service"]] <- mean(unlist(best_tresholds_cv_service))
expected_loss_service[["rf_p_service"]] <- mean(unlist(expected_loss_cv_service))


rf_summary_service <- data.frame("CV RMSE" = CV_RMSE_service[["rf_p_service"]],
                              "CV AUC" = CV_AUC_service[["rf_p_service"]],
                              "Avg of optimal thresholds" = best_tresholds_service[["rf_p_service"]],
                              "Threshold for Fold5" = best_treshold_service$threshold,
                              "Avg expected loss" = expected_loss_service[["rf_p_service"]],
                              "Expected loss for Fold5" = expected_loss_cv_service[[fold]])

rf_summary_service

###
# Create plots - this is for Fold5

createLossPlot(roc_obj_service, best_treshold_service, "rf_p_loss_plot_service")
createRocPlotWithOptimal(roc_obj_service, best_treshold_service, "rf_p_roc_plot_service")

####
# Take model to holdout and estimate RMSE, AUC and expected loss
rf_predicted_probabilities_holdout_service <- predict(rf_model_p_service, newdata = data_holdout_service, type = "prob")
data_holdout_service$rf_p_prediction_service <- rf_predicted_probabilities_holdout_service[,"fast_growth"]
RMSE(data_holdout_service$rf_p_prediction_service, data_holdout_service$fast_growth)

# ROC curve on holdout
roc_obj_holdout_service <- roc(data_holdout_service$fast_growth, data_holdout_service[, "rf_p_prediction_service", drop=TRUE], quiet=TRUE)

# AUC
as.numeric(roc_obj_holdout_service$auc)

# Get expected loss on holdout with optimal threshold
holdout_treshold_service <- coords(roc_obj_holdout_service, x = best_tresholds_service[["rf_p_service"]] , input= "threshold",
                                ret="all", transpose = FALSE)
expected_loss_holdout_service <- (holdout_treshold_service$fp*FP + holdout_treshold_service$fn*FN)/length(data_holdout_service$fast_growth)
expected_loss_holdout_service


# Confusion table on holdout with optimal threshold
holdout_prediction_service <-
  ifelse(data_holdout_service$rf_p_prediction_service < best_tresholds_service,  "no_fast_growth", "fast_growth") %>%
  factor(levels = c("no_fast_growth", "fast_growth"))
cm_object_service <- confusionMatrix(holdout_prediction_service,data_holdout_service$fast_growth_f)
cm_service <- cm_object_service$table
cm_service
# in pctg
round( cm_service / sum(cm_service) * 100 , 1 )

#------------------------------------------------------------------------------------------------------------------------

