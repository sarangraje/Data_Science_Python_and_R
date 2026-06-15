library(ggplot2)
library(magrittr)
library(pander)


# DATA CLEANING AND MANIPULATION 

library(dplyr)


library(readxl)
df <- read_xlsx("df_final.xlsx")
# View(df)
summary(df)
head(df)

class(df$diag_1)
df$diag_1 <- as.numeric(df$diag_1)

cols <- c("diag_2", "diag_3")
df[,cols] <- lapply(df[,cols], as.numeric)
# View(df)
colnames(df)
cols <- c()

# Convert to Factors for the model
df$race <- as.factor(df$race)
df$gender <- as.factor(df$gender)
df$age <- as.factor(df$age)


df$diag_1 <- as.numeric(as.character(df$diag_1))

cols_2 <- c("max_glu_serum", "A1Cresult", "metformin", "repaglinide", "nateglinide", "chlorpropamide", "glimepiride",             
          "acetohexamide", "glipizide", "glyburide", "tolbutamide", "pioglitazone", "rosiglitazone", "acarbose", "miglitol", "troglitazone", "tolazamide", "examide", "citoglipton", "insulin", "glyburide-metformin", "glipizide-metformin", "glimepiride-pioglitazone", "metformin-rosiglitazone", "metformin-pioglitazone", "change", "diabetesMed", "readmitted")
df[, cols_2] <- lapply(df[, cols_2], factor)

summary(df)
dim(df)

library(dplyr)
library(ggplot2)

colnames(df)

colss <- c("admission_type_id", "discharge_disposition_id", "admission_source_id", "time_in_hospital", "num_lab_procedures", 
           "num_procedures", "num_medications", "number_outpatient", "number_emergency", "number_inpatient", "diag_1", 
           "diag_2", "diag_3", "number_diagnoses")

corr_data <- df[,colss]
round(cor(corr_data),2)
str(corr_data)

ppt_col <- c("time_in_hospital", "number_diagnoses", "num_medications", "num_lab_procedures", "num_procedures", "number_inpatient", "number_emergency")
cor_num <- df[, ppt_col]
round(cor(cor_num), 2)


library(ggplot2)
library(reshape2)

# 1. Prepare data
cor_matrix <- cor(df[, c("time_in_hospital", "number_diagnoses", "num_medications", 
                         "num_lab_procedures", "num_procedures", "number_inpatient", 
                         "number_emergency")])
melted_cor <- melt(cor_matrix)

# 2. Plot with diagonal starting top-left
ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits = rev) + # This reverses the Y-axis order
  scale_fill_gradient(low = "lightyellow", high = "red") +
  geom_text(aes(label = round(value, 2))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL)



#########################################################################################

# MULTIPLE LINEAR REGRESSION


# 1. Drop the zero-variance columns: These columns only have one type of level which is "No"
df_clean <- df[, !(names(df) %in% c("examide", "citoglipton"))]

df <- df_clean

model <- lm(formula = time_in_hospital ~ ., data = df)
summary(model)

res <- residuals(model)
plot(res)
abline(h = 0, col = "red")
qqnorm(res, main = "Residuals in reference with the QQ-line")
qqline(res, col = "blue")


model_log <- lm(formula = log(time_in_hospital) ~ ., data = df)
summary(model_log)

res_log <- residuals(model_log)
plot(res_log)
abline(h = 0, col = "red")
qqnorm(res_log, main = "QQ-line & the residuals with Log-transformation on the outcome")
qqline(res_log, col = "blue")


# Model with only statistically significant variables.

model_sta_sig <- lm(log(time_in_hospital) ~ race + gender + admission_type_id + 
                      discharge_disposition_id + admission_source_id + 
                      num_lab_procedures + num_procedures + num_medications + 
                      number_outpatient + number_emergency + number_inpatient + 
                      diag_1 + diag_2 + diag_3 + number_diagnoses + 
                      max_glu_serum + A1Cresult + metformin + repaglinide + 
                      glimepiride + glipizide + glyburide + pioglitazone + 
                      rosiglitazone + insulin + `glyburide-metformin` + 
                      change + diabetesMed + readmitted, 
                    data = df)

summary(model_sta_sig)
AIC(model_log, model_sta_sig)

##########################################################################################

# STEPWISE VARIABLE SELECTION --- forward and backward

2^45

(1 + 45*(45+1)/2)

library(leaps)
  
  
mod_fwd <- regsubsets(df$time_in_hospital ~ ., data = df, nvmax = 45, method = "forward")
summ_fwd <- summary(mod_fwd)
names(summ_fwd)

summ_fwd$adjr2
which.max(summ_fwd$adjr2)                                                        # Check max adjr2
coef(mod_fwd, 45)


mod_bwd <- regsubsets(df$time_in_hospital ~ ., data = df, nvmax = 45, method = "backward")
summ_bwd <- summary(mod_bwd)
names(summ_bwd)

summ_bwd$adjr2
which.max(summ_bwd$adjr2)
coef(mod_bwd, 45)

# The adjr2 of Backward (0.3224033) is very slightly higher than that of the Forward (0.3218724) subset selection methods and for both of them it is for the 45th model.


# PREDICTION

index <- sample(1:nrow(df), nrow(df)*0.6)

train_df <- df[index,]
colnames(train_df)
train_clean <- train_df[, !(names(train_df) %in% c("examide", "citoglipton"))]
train_df_c <- train_clean



test_df <- df[-index,]
colnames(test_df)
test_clean <- test_df[, !(names(test_df) %in% c("examide", "citoglipton"))]
test_df_c <- test_clean


model_train <- regsubsets(train_df_c$time_in_hospital ~ ., data = train_df_c, nvmax = 45, method = "forward")
summ_train <- summary(model_train)
which.max(summ_train$adjr2)

predictions <- predict(model_train, newdata = test_df_c, interval = "confidence")            # Error, not allowing to predict using a regsubset model
predictions




#############################################################################################

# PENALISED MODELS ---> LASSO        


library(glmnet)

x <- model.matrix(time_in_hospital ~ ., data = df)[, -1]
y <- df[row.names(x), "time_in_hospital"]
y_vector <- as.numeric(unlist(y))

keep_index <- !is.na(y_vector)
x_final <- x[keep_index, ]
y_final <- y_vector[keep_index]

set.seed(123)
train_idx <- sample(1:nrow(x_final), nrow(x_final) * 0.60)

x_train <- x_final[train_idx, ] 
y_train <- y_final[train_idx]
x_test <- x_final[-train_idx, ]
y_test <- y_final[-train_idx]

cv_out <- cv.glmnet(x_train, y_train, alpha = 1)
plot(cv_out)

bestlam <- cv_out$lambda.min
bestlam

lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = bestlam)
coef(lasso_model)

predictions <- predict(lasso_model, s = bestlam, newx = x_test)
predictions


############################################ CALCULATING ADJR2 ###########################################
mse <- mean((y_test - predictions)^2)
rmse <- sqrt(mse)
rmse

y_test_mean <- mean(y_test)
sse <- sum((y_test - predictions)^2)
sst <- sum((y_test - y_test_mean)^2)

r2_test <- 1 - (sse / sst)

n <- length(y_test)
k <- ncol(x_test)
adj_r2_test <- 1 - ((1 - r2_test) * (n - 1) / (n - k - 1))

r2_test
adj_r2_test




###############################################################################################################


# Ridge Regression

library(glmnet)

x <- model.matrix(time_in_hospital ~ ., data = df)[, -1]
y <- df[row.names(x), "time_in_hospital"]
y_vector <- as.numeric(unlist(y))

keep_index <- !is.na(y_vector)
x_final <- x[keep_index, ]
y_final <- y_vector[keep_index]

set.seed(123)
train_idx <- sample(1:nrow(x_final), nrow(x_final) * 0.60)

x_train_r <- x_final[train_idx, ]
y_train_r <- y_final[train_idx]
x_test_r <- x_final[-train_idx, ]
y_test_r <- y_final[-train_idx]

cv_out_r = cv.glmnet(x_train_r, y_train_r, alpha = 0)
plot(cv_out_r)

bestlam_r <- cv_out_r$lambda.min
bestlam_r


ridge_model <- glmnet(x_train_r, y_train_r, alpha = 0, lambda = bestlam_r)
ridge_model

coef(ridge_model)

predictions_r <- predict(ridge_model, newx = x_test_r, s = bestlam_r)
predictions_r

##################################################################################3

# adjr2 for Ridge 

mse_r <- mean((y_test_r - predictions_r)^2)
rmse_r <- sqrt(mse_r)
rmse_r

y_test_mean_r <- mean(y_test_r)
sse_r <- sum((y_test_r - predictions_r)^2)
sst_r <- sum((y_test_r - y_test_mean_r)^2)

r2_test_r <- 1 - (sse_r / sst_r)

n <- length(y_test_r)
k <- ncol(x_test_r)
adj_r2_test_r <- 1 - ((1 - r2_test_r) * (n - 1) / (n - k - 1))

r2_test_r
adj_r2_test_r

# Compare the adjr2 of LASSO and Ridge

adj_r2_test
adj_r2_test_r


#####################################################################################


# Decision Trees - Regression Tree for predicting time_in_hospital

library(dplyr)       # for data wrangling
library(DescTools)   # for descriptive statistics and visualization
library(rpart)       # for decision tree application
library(caret)       # for decision tree application
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance


regress_tr_mod <- rpart(formula = time_in_hospital ~ ., data = df, method = "anova")
regress_tr_mod
summary(regress_tr_mod)

rpart.plot(regress_tr_mod)

vip(regress_tr_mod)

plotcp(regress_tr_mod)

printcp(regress_tr_mod)                      

# To get adjr2 for decision tree, check the value of the last line of "rel error", using this function
# Adjr2 = 1 - "rel error" value of last line



# Random Forest

# library(randomForest)


# We are removing the variables with a hyphen in their name. So, total variables came down to 39 from 44
# Random forest with those variables also but its adjr2 was lesser than with 39-variable model below:

clean_vars <- c("race", "gender", "age", "admission_type_id", 
                "discharge_disposition_id", "admission_source_id", 
                "time_in_hospital", "num_lab_procedures", "num_procedures", 
                "num_medications", "number_outpatient", "number_emergency", 
                "number_inpatient", "diag_1", "diag_2", "diag_3", 
                "number_diagnoses", "max_glu_serum", "A1Cresult", 
                "metformin", "repaglinide", "nateglinide", "chlorpropamide", 
                "glimepiride", "acetohexamide", "glipizide", "glyburide", 
                "tolbutamide", "pioglitazone", "rosiglitazone", "acarbose", 
                "miglitol", "troglitazone", "tolazamide", "insulin", "change", "diabetesMed", "readmitted")

# 2. Subset your dataframe
df_clean <- df[, clean_vars]

# random_for_mod <- randomForest(formula = time_in_hospital ~ ., data = df_clean, mtry = 5, importance = TRUE)
# random_for_mod
# summary(random_for_mod)
# varImpPlot(random_for_mod)
# importance(random_for_mod)

# Rename specifically the 5 hyphenated variables
# names(df)[names(df) == "glyburide-metformin"]  <- "glyburide_metformin"
# names(df)[names(df) == "glipizide-metformin"]  <- "glipizide_metformin"
# names(df)[names(df) == "glimepiride-pioglitazone"] <- "glimepiride_pioglitazone"
# names(df)[names(df) == "metformin-rosiglitazone"] <- "metformin_rosiglitazone"
# names(df)[names(df) == "metformin-pioglitazone"]  <- "metformin_pioglitazone"

# Verify that they have changed
# grep("_", names(df), value = TRUE)

# install.packages("ranger")                                                     # This package handles huge datasets and so many trees well and quickly.
library(ranger)                                                                  # "ranger" means Random Forest Generator

set.seed(31)

rf_ranger_mod <- ranger(
  formula         = time_in_hospital ~ ., 
  data            = df_clean,                                                    # This is df_clean (without variables containing hyphen in their names)
  num.trees       = 500,
  mtry            = 13,
  importance      = 'impurity', 
  verbose         = TRUE
)

print(rf_ranger_mod)

vip(rf_ranger_mod)

imp <- sort(importance(rf_ranger_mod), decreasing = TRUE)
imp


# Outcome of Random Forest

For Random Forest, m = p^0.5 = sqrt(39) = 6.24 ~ 6 or 7 variables 

So, for mtry = 6, adjr2 = 0.4276                          (m = p^0.5)            # This is for Classification tree of Random forest
    for mtry = 7, adjr2 = 0.4305
    for mtry = 13, adjr2 = 0.4350                         (m = p/3)              # p/3 is for Regression Tree of Random Forest.
                                                          






# Tested code for 44-variable model (all variables)


# Running Ranger on the full dataset (all columns)
# rf_all_vars_mod <- ranger(
#  formula         = time_in_hospital ~ ., 
#  data            = df,                                                         # This is the original df
#  num.trees       = 500,
#  mtry            = 13,
#  importance      = 'impurity', 
#  verbose         = TRUE
# )

# Check the final results
# print(rf_all_vars_mod)                                                         # Adjr2 was less than rf_ranger_mod


vip(rf_ranger_mod)
imp <- sort(importance(rf_ranger_mod), decreasing = TRUE)
imp

# Variable diag_3 at the top shows that the complexity of the patient's illness is more important 
# than the primary reason they were admitted.


######################################################################################################################333333



# Comparison of models 


# In terms of Adjusted R-squared:

# Multiple Linear Regression: 0.3221
# Forward stepwise: 0.3218
# Backward stepwise: 0.3224
# Single Decision Tree (rpart): 0.2624
# Random Forest (ranger): 0.435

View(df)
