# Biostats 680 - Project 


df <- read.csv("heart_failure_clinical_records_dataset.csv")
View(df)
head(df)
summary(df)
colnames(df)




# Data Manipulation and Data Types
cols <- c("anaemia", "diabetes", "high_blood_pressure", "sex", "smoking", "DEATH_EVENT")
df[, cols] <- lapply(df[, cols], factor)

str(df)
summary(df)





# Correlation
cols_to_numeric <- c("creatinine_phosphokinase", "ejection_fraction", "serum_sodium", "time")
df[, cols_to_numeric] <- lapply(df[, cols_to_numeric], as.numeric)

col_for_corr <- c("age", "creatinine_phosphokinase", "ejection_fraction", 
                  "platelets", "serum_creatinine", "serum_sodium", "time")


cor(df[, col_for_corr])
# No multicollinearity

library(ggplot2)

melted_corr <- as.data.frame(as.table(cor(df[, col_for_corr])))

ggplot(melted_corr, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_y_discrete(limits = rev) + # This reverses the Y-axis order
  geom_text(aes(label = round(Freq, 2))) +
  scale_fill_gradient2(low = "yellow", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Feature Correlation")


# Logistic Regression
glm_mod <- glm(formula = DEATH_EVENT ~ .-time, data = df, family = binomial(link = "logit"))    # We can't keep time as a predictor because we don't know when the person will die or leave the study.
glm_mod
summary(glm_mod)
# Only age, ejection_fraction, serum_creatinine, and time are statistically significant at alpha = 0.05
# INTERCEPT IS NOT STATISTICALLY SIGNIFICANT. "Female" is the baseline for the variable "sex"
# A non-significant intercept just means the baseline starting point isn't statistically different from a log-odds of zero => odds = 1 and p = 0.5

# Reduced model
glm_significant <- glm(DEATH_EVENT ~ age + ejection_fraction + serum_creatinine, 
                   family = binomial(link = "logit"), data = df)
summary(glm_significant)

  
# Comparing them
library(pander)
pander(anova(glm_mod, glm_significant, test = "Chisq"))

AIC(glm_mod, glm_significant)
BIC(glm_mod, glm_significant)


# Training and Testing split
library(caret)
set.seed(31)

training_index <- createDataPartition(df$DEATH_EVENT, p = 0.70, list = FALSE)

train_df <- df[training_index, ]
test_df <- df[-training_index, ]

glm_train_df <- glm(formula = DEATH_EVENT ~ age + ejection_fraction + serum_creatinine, data = train_df, family = binomial(link = "logit"))
glm_train_df
summary(glm_train_df)

# Making predictions
predictions <- predict(glm_train_df, newdata = test_df, type = "response")
predictions

plot(sort(predictions), type = "l",
     xlab = "Patient Rank (from lowest to highest risk)", 
     ylab = "Probability of Death Event",
     col = "blue")
abline(h= 0.3, col = "red", lty = 2)                                              # h = 3 because probability is reduced for classifying the outcome.


predictions_binary <- ifelse(predictions > 0.3, 1, 0)


table(test_df$DEATH_EVENT, predictions_binary)

18/(18+15) # Precision

18/(18+10)  # Recall

(45+18)/(45+10+15+18)  # Accuracy




# Survival Analysis
library(survival)

train_df$DEATH_EVENT <- as.numeric(train_df$DEATH_EVENT)

cox_mod <- coxph(Surv(time, DEATH_EVENT) ~ age + ejection_fraction + serum_creatinine, data = train_df)
summary(cox_mod)
# Concordance calculates the probability of who will die first from any 2 randomly chosen patients.  
# Also, if I gave the model 10 pairs of patients, it would correctly pick "who dies first from that pair", 7 times (here)


# Plotting the survival curves
# install.packages("survminer")
# library(survminer)                        -------- for ggsurvplot() ---------

plot(survfit(cox_mod), xlab = "Days", ylab = "Survival Probability", main = "Heart Failure Survival", col = "red")

library(survminer)
ggsurvplot(survfit(cox_mod), data = train_df, col = "red", ggtheme = theme_minimal())

# They create mean curves for the whole data (mean of all predictors)


# Comparing hazard risk associated with smokers and non-smokers.

df_smoke <- df[df$smoking == 1,]
dim(df_smoke)

df_no_smoke <- df[df$smoking == 0,]
dim(df_no_smoke)


df_smoke$DEATH_EVENT <- as.numeric(df_smoke$DEATH_EVENT)
df_no_smoke$DEATH_EVENT <- as.numeric(df_no_smoke$DEATH_EVENT)


library(survival)
cox_mod_smo <- coxph(Surv(time, DEATH_EVENT) ~ age + ejection_fraction + serum_creatinine, data = df_smoke)
summary(cox_mod_smo)

cox_mod_no_smo <- coxph(Surv(time, DEATH_EVENT) ~ age + ejection_fraction + serum_creatinine, data = df_no_smoke)
summary(cox_mod_no_smo)


# To check the model assumption of constant hazard ratios over time;
cox.zph(cox_mod_smo)
cox.zph(cox_mod_no_smo)

t.test(age ~ smoking, data = df)

# While smoking is a known risk factor, my analysis revealed that it fundamentally changes the predictive 
# power of clinical markers. For smokers, Age was a more aggressive predictor of mortality, whereas for non-smokers, 
# Kidney Function (Serum Creatinine) was the dominant hazard driver. This suggests that smoking status dictates 
# which clinical pathways lead to heart failure mortality, in this dataset.





# If you want to see the total causal effect of smoking in one single number, run this on your full dataset:




# Predicting time to event ith probability of survival for new patients using our Cox model:

# Define a new patient (e.g., 65 years old, high creatinine, low ejection fraction)

#  new_patient <- data.frame(                                                     # Only 1 new patient
#  age = 97,
#  ejection_fraction = 16,
#  serum_creatinine = 1.85
# )

test_patients <- data.frame(
  patient_id = 1:5,
  age = c(22, 35, 45, 51, 85),
  ejection_fraction = c(60, 55, 40, 35, 45),
  serum_creatinine = c(0.7, 0.9, 1.1, 2.1, 1.0)
)

# Preview the data
View(test_patients)


# Generate the individualized survival curve
patient_survival_mod <- survfit(cox_mod, newdata = test_patients)

# Plot their specific survival trajectory
plot(patient_survival_mod, 
     col = 1:5,
     lty = 1,
     xlab = "Days", 
     ylab = "Survival Probability", 
     main = "Individualized Survival Forecast")

colors <- c("red", "blue", "green", "black", "orange")


ggsurvplot(patient_survival_mod, data = test_patients, palette = colors, 
           ggtheme = theme_minimal(), conf.int = FALSE, censor = FALSE) 
############################################################################################################################
# INVESTIGATE THIS PROPERLY :           Check Cox Proportional Hazards Assumption ------ It says that the hazard ratio associated with every predictor should be constant over time
cox.zph(cox_mod)                        # p-value should be greater than 0.05  # Null Hyp. is that the Hazard Ratio of that variable is constant over time. 
############################################################################################################################

library(survival)
library(survminer)

df$time <- as.numeric(df$time)
df$DEATH_EVENT <- as.numeric(as.character(df$DEATH_EVENT))
df_clean <- na.omit(df[, c("time", "DEATH_EVENT")])

# The Kaplan-Meier Curve          ---------------   It just plots the raw events or censoring from the data without any predictors. (It is a non-parametric model)
km_fit_final <- survfit(Surv(time, DEATH_EVENT) ~ 1, data = df_clean)

# Plot
ggsurvplot(
  km_fit_final, 
  data = df_clean, 
  fun = NULL, 
  conf.int = TRUE, 
  palette = "blue",
  title = "Kaplan-Meier Survival Probability",
  xlab = "Days",
  ylab = "Survival Probability"
)

# Comparing Kaplan-Meier with Mean Survival Curve of coxph model

library(survival)
library(survminer)

km_fit <- survfit(Surv(time, DEATH_EVENT) ~ 1, data = df_clean)
cox_fit <- survfit(cox_mod)

fit_list <- list(Kaplan_Meier = km_fit, Cox_Baseline = cox_fit)

ggsurvplot_combine(
  fit_list, 
  data = df_clean,
  palette = c("blue", "red"),
  legend.title = "Method",
  legend.labs = c("Actual (KM)", "Model Adjusted (Cox)"),
  ggtheme = theme_minimal(),
  title = "Comparison: Observed vs. Predicted Survival"
)

# Even though the red curve is the coxph curve which shows the average survival probability (with all the average values), it still shows
# vertical lines of censoring because it depicts the real-time events that happened in the dataset. BUT IT IS THE CURVE WHICH SHOWS THE 
# AVERAGE FOR THE WHOLE DATASET.

# Kaplan-Meier curve (blue) shows all the observations in the dataset without considering any predictors. They are just raw observations of events by time.


# Check and compare survival probability at Day 200 for all 10 patients
prob <- summary(patient_survival_mod, times = 200)$surv
barplot(prob, ylim = c(0,1), col = "lightgreen", main = "Survival Probability at day 200", xlab = "Patient ID", ylab = "Probability")
abline(h = 0.5, col = "red", lty = 2)

library(magrittr)
library(dplyr)
head(df)

x <- df %>% group_by(diabetes) %>% summarize(Avg_platelets = mean(platelets))
x

y <- df %>% group_by(sex) %>% summarize(Avg_Serum_sodium = sprintf("%.2f", mean(serum_sodium)), 
                                        Avg_serum_crea = sprintf("%.2f", mean(serum_creatinine)))
y

library(ggplot2)

ggplot(data = df, aes(diabetes, serum_creatinine)) + 
  stat_summary(fun = "mean", geom = "bar", fill = "green") +
  ggtitle("Scatter Plot") + 
  xlab("Diabetes") + 
  ylab("Mean Serum Creatinine") +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "black"),
    panel.grid.minor.y = element_line(color = "grey")
  )

