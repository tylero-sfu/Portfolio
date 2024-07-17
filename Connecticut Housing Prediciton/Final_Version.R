# Required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(randomForest)
library(xgboost)
library(pROC)

# Reading the data
salary <- read.csv('salary.csv')
awards <- read.csv('awards.csv')
accused <- read.csv('accused.csv')
complaints <- read.csv('complaints.csv')
Xte <- read.csv('Xte.csv')
pred0 <- read.csv('pred0.csv')

# Histogram of Officers Ages
ggplot(accused, aes(x = birth_year)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Officer Ages",
       x = "Age",
       y = "Count")

# Define a function to extract the mode of character data
get_mode_char <- function(x) {
  unique_x <- na.omit(x)
  if (length(unique_x) == 0) {
    return(NA)
  }
  table_x <- table(unique_x)
  mode_val <- names(table_x)[which.max(table_x)]
  return(mode_val)
}

# Features
# - Salary: Salary by Year (2011-2012, 2013-2014, 2015-2016)
# - Probation: Probational Status from Salary Dataset
# - Previous complaints against police officer: Y1112, Y1314
# - Previous complaints against police officers in same department: D1112, D1314
# - Awards: Factor variable whether they are awarded in 2011-2012, 2013-2014, 2015-2016

# Forcing values to NAs where just have space instead of date
awards$appointed_date <- ifelse(awards$appointed_date == "", NA, awards$appointed_date)
salary$start_date <- ifelse(salary$start_date == "", NA, salary$start_date)

salary_preprocessing <- salary[Xte$name %in% salary$name,]
salary_preprocessing <- salary_preprocessing %>%
  group_by(name) %>%
  mutate(
    start_date = ifelse(is.na(start_date), get_mode_char(start_date), start_date)
  )

TEMP <- data.frame(name = Xte[,2])

# Merge TEMP with other data
TEMP = left_join(TEMP, accused[c('name','appointed_date')])
TEMP = left_join(TEMP, salary_preprocessing[c('name','start_date')], multiple = "first", by = 'name')
TEMP = left_join(TEMP, awards[c('name','appointed_date')], multiple = "first", by = 'name')

# Imputing missing start_date using appointed_date from accused and awards
missing_appointed_date <- which(is.na(TEMP$start_date))
TEMP$start_date[missing_appointed_date] <- TEMP$appointed_date.x[missing_appointed_date]
missing_appointed_date <- which(is.na(TEMP$start_date))
TEMP$start_date[missing_appointed_date] <- TEMP$appointed_date.y[missing_appointed_date]

# Removing unnecessary columns
TEMP$appointed_date.x <- NULL
TEMP$appointed_date.y <- NULL

# Keeping only unique entries
TEMP <- unique(TEMP)

# Calculating and creating new features W1516
TEMP$start_date <- as.Date(TEMP$start_date)
TEMP$work_year_2015 <- 2015 - as.numeric(format(TEMP$start_date, "%Y"))
TEMP$work_year_2016 <- 2016 - as.numeric(format(TEMP$start_date, "%Y"))

TEMP <- left_join(Xte, TEMP, by='name',multiple="first")

# Imputing if Working year is still missing by averaging the whole working year
TEMP$work_year_2015 <- ifelse(is.na(TEMP$work_year_2015), mean(TEMP[!is.na(TEMP$work_year_2015),]$work_year_2015), TEMP$work_year_2015)
TEMP$work_year_2016 <- ifelse(is.na(TEMP$work_year_2016), mean(TEMP[!is.na(TEMP$work_year_2016),]$work_year_2016), TEMP$work_year_2016)

TEMP$W1516 <- (TEMP$work_year_2015 + TEMP$work_year_2016)/2

# Creating variables
N = dim(Xte)[1]
Y1112 = rep(NA, N)
Y1314 = rep(NA, N)
A1112 = rep(NA, N)
A1314 = rep(NA, N)
A1516 = rep(NA, N)
S1112 = rep(NA, N)
S1314 = rep(NA, N)
S1516 = rep(NA, N)
P1112 = rep(NA, N)
P1314 = rep(NA, N)
P1516 = rep(NA, N)
D1112 = rep(NA, N)
D1314 = rep(NA, N)

# Extract year from the incident date
complaints$year = substr(complaints$complaint_date, 1, 4)

# Make two subsets of the complaints
complaints1314 = complaints[complaints$year == "2013" | complaints$year == "2014", ]
cr_ids1314 = complaints1314$cr_id

complaints1112 = complaints[complaints$year == "2011" | complaints$year == "2012", ]
cr_ids_1112 = complaints1112$cr_id

# Loop through the names
for (i in 1:N) {
  # Print progress through this for loop (to ensure compute time is fine)
  if (i %% 500 == 0) {cat(sprintf('%d / %d\n', i, N))}
  
  # Get the i-th name
  name = Xte[i, 2]
  
  # Get all cr_ids in which the i-th name is accused
  cr_ids_i = accused$cr_id[accused$name==name]
  
  # If any of the cr_ids for this name are in cr_ids1314, then name was accused in 2013/2014
  if (any(cr_ids_i %in% cr_ids1314)) {Y1314[i] = 1} else {Y1314[i] = 0}
  if (any(cr_ids_i %in% cr_ids_1112)) {Y1112[i] = 1} else {Y1112[i] = 0}
}

# Creating features that how many awards each police officers received in certain period 
awards$award_request_date = as.Date(awards$award_request_date)

awards_1112 = awards[as.numeric(format(awards$award_request_date, "%Y")) == "2011" | as.numeric(format(awards$award_request_date, "%Y")) == "2012",]
name_awards_1112 = awards_1112[awards_1112$current_award_status == "FINAL",]$name

awards_1314 = awards[as.numeric(format(awards$award_request_date, "%Y")) == "2013" | as.numeric(format(awards$award_request_date, "%Y")) == "2014",]
name_awards_1314 = awards_1314[awards_1112$current_award_status == "FINAL",]$name

awards_1516 = awards[as.numeric(format(awards$award_request_date, "%Y")) == "2015" | as.numeric(format(awards$award_request_date, "%Y")) == "2016",]
name_awards_1516 = awards_1516[awards_1112$current_award_status == "FINAL",]$name

awards_1112 <- awards_1112 %>% group_by(name) %>% summarize(total_awards = n())
awards_1314 <- awards_1314 %>% group_by(name) %>% summarize(total_awards = n())
awards_1516 <- awards_1516 %>% group_by(name) %>% summarize(total_awards = n())

awards_1112 <- left_join(awards_1112, Xte, by ="name")
awards_1314 <- left_join(awards_1314, Xte, by ="name")
awards_1516 <- left_join(awards_1516, Xte, by ="name")

A1112[awards_1112$ID] <- awards_1112$total_awards
A1314[awards_1314$ID] <- awards_1314$total_awards
A1516[awards_1516$ID] <- awards_1516$total_awards

A1112 = ifelse(is.na(A1112), 0, A1112)
A1314 = ifelse(is.na(A1314), 0, A1314)
A1516 = ifelse(is.na(A1516), 0, A1516)

# Creating features of police officers salary in certain period

salary_1112 = salary[salary$year == "2011" | salary$year == "2012",]
salary_1314 = salary[salary$year == "2013" | salary$year == "2014",]
salary_1516 = salary[salary$year == "2015" | salary$year == "2016",]

salary_1112 <- salary_1112 %>% group_by(name) %>% summarize(avg_salary = mean(salary))
salary_1314 <- salary_1314 %>% group_by(name) %>% summarize(avg_salary = mean(salary))
salary_1516 <- salary_1516 %>% group_by(name) %>% summarize(avg_salary = mean(salary))

salary_1112 <- left_join(salary_1112, Xte, by ="name")
salary_1314 <- left_join(salary_1314, Xte, by ="name")
salary_1516 <- left_join(salary_1516, Xte, by ="name")

S1112[salary_1112$ID] <- salary_1112$avg_salary
S1314[salary_1314$ID] <- salary_1314$avg_salary
S1516[salary_1516$ID] <- salary_1516$avg_salary

# Imputing if there are NAs with average of salary in the period
Smu1112 <- mean(salary[salary$year %in% c("2011", "2012"),]$salary)
Smu1314 <- mean(salary[salary$year %in% c("2013", "2014"),]$salary)
Smu1516 <- mean(salary[salary$year %in% c("2015", "2016"),]$salary)

S1112 = ifelse(is.na(S1112), Smu1112, S1112)
S1314 = ifelse(is.na(S1314), Smu1314, S1314)
S1516 = ifelse(is.na(S1516), Smu1516, S1516)

# Creating features whether they are on probationary
salary_1112 = salary[salary$year == "2011" | salary$year == "2012",]
salary_1314 = salary[salary$year == "2013" | salary$year == "2014",]
salary_1516 = salary[salary$year == "2015" | salary$year == "2016",]

salary_1112$Probational <- ifelse(salary_1112$employee_status == "PROBATIONARY CAREER SERVICE", 1, 0)
salary_1314$Probational <- ifelse(salary_1314$employee_status == "PROBATIONARY CAREER SERVICE", 1, 0)
salary_1516$Probational <- ifelse(salary_1516$employee_status == "PROBATIONARY CAREER SERVICE", 1, 0)

salary_1112 <- left_join(salary_1112, Xte, by ="name")
salary_1314 <- left_join(salary_1314, Xte, by ="name")
salary_1516 <- left_join(salary_1516, Xte, by ="name")

P1112[salary_1112$ID] <- salary_1112$Probational
P1314[salary_1314$ID] <- salary_1314$Probational
P1516[salary_1516$ID] <- salary_1516$Probational

P1112 = ifelse(is.na(P1112), 0, P1112)
P1314 = ifelse(is.na(P1314), 0, P1314)
P1516 = ifelse(is.na(P1516), 0, P1516)

# Creating features how many complaints occurred in each department in certain period
complaints_accused <- left_join(complaints, accused, by = "cr_id")

complaints_accused <- complaints_accused[(complaints_accused['complaint_date'] >= '2009-01-01') &
                                           (complaints_accused['complaint_date'] <= '2014-12-31'),]

complaints_accused$beat <- ifelse(complaints_accused$beat == "", NA, complaints_accused$beat)
complaints_accused$beat <- as.numeric(complaints_accused$beat)

beat1112 <- complaints_accused[(complaints_accused['complaint_date'] >= '2011-01-01') &
                                 (complaints_accused['complaint_date'] <= '2012-12-31'),]
beat1314 <- complaints_accused[(complaints_accused['complaint_date'] >= '2013-01-01') &
                                 (complaints_accused['complaint_date'] <= '2014-12-31'),]

beat1112 <- beat1112[c('beat','name','cr_id')]
beat1314 <- beat1314[c('beat','name','cr_id')]

n_by_beat1112 <- beat1112 %>%
  select('beat','cr_id') %>%
  distinct() %>%
  group_by(beat) %>%
  summarize(n_complaints = n())

n_by_beat1314 <- beat1314 %>%
  select('beat','cr_id') %>%
  distinct() %>%
  group_by(beat) %>%
  summarize(n_complaints = n())

beat1112 <- left_join(beat1112, n_by_beat1112, by="beat")
beat1314 <- left_join(beat1314, n_by_beat1314, by="beat")

same_beat1112 <- beat1112[Xte$name %in% beat1112$name,]
same_beat1314 <- beat1314[Xte$name %in% beat1314$name,]

same_beat1112 <- same_beat1112[!is.na(same_beat1112$name),]
same_beat1314 <- same_beat1314[!is.na(same_beat1314$name),]

same_beat1112 <- left_join(same_beat1112[c('name','n_complaints')], Xte, by = "name")
same_beat1314 <- left_join(same_beat1314[c('name','n_complaints')], Xte, by = "name")

D1112[same_beat1112$ID] <- same_beat1112[same_beat1112$ID,]$n_complaints
D1314[same_beat1314$ID] <- same_beat1314[same_beat1314$ID,]$n_complaints

# Imputing the average of the number of complaints in the period
Dmu1112 <- mean(same_beat1112$n_complaints)
Dmu1314 <- mean(same_beat1314$n_complaints)

D1112 = ifelse(is.na(D1112), Dmu1112, D1112)
D1314 = ifelse(is.na(D1314), Dmu1314, D1314)

# Merging every column and creating a new dataset for training and testing
XY <- data.frame(X = Xte[,2], Y1112 = Y1112, Y1314 = Y1314,
                 A1112 = A1112, A1314 = A1314, A1516 = A1516,
                 S1112 = S1112, S1314 = S1314, S1516 = S1516,
                 P1112 = P1112, P1314 = P1314, P1516 = P1516,
                 D1112 = D1112, D1314 = D1314, W1516 = TEMP$W1516)

XY <- rename(XY, name = X)

# Make specific columns as factors
XY$Y1112 <- as.factor(XY$Y1112)
XY$Y1314 <- as.factor(XY$Y1314)
XY$P1112 <- as.factor(XY$P1112)
XY$P1314 <- as.factor(XY$P1314)
XY$P1516 <- as.factor(XY$P1516)

# Define selected columns and prepare features and target
selected_columns <- c("Y1112", "A1112", "A1314", "A1516", "S1112", "S1314", "S1516", "P1112", "P1314", "P1516", "D1112", "D1314", "W1516")

training_features <- XY[, selected_columns]
training_target <- Y1314  # Assuming Y1314 is the target variable

# Setting seed for reproducibility
set.seed(440)

# Train the Random Forest model
model_rf <- randomForest(x = training_features,
                         y = training_target,
                         ntree = 300,
                         mtry = 2,
                         node_size = 3,
                         importance=TRUE)

test_features <- XY_rf[, selected_columns]

# Make predictions on test data
predictions_rf <- predict(model_rf, newdata = test_features)

# Add predictions to your prediction dataframe
pred0$score <- predictions_rf

# Save the results
write.csv(pred0, file = 'pred1.csv', row.names = FALSE)

# Train Random Forest model and compute ROC AUC
set.seed(440)

data <- data.frame(training_features, training_target)
folds <- createFolds(data$training_target, k = 5, list = TRUE, returnTrain = FALSE)

# Function to train Random Forest model and calculate ROC AUC for each fold
rf_roc_auc <- function(train_index, test_index) {
  train_data <- data[train_index, ]
  test_data <- data[test_index, ]
  
  rf_model <- randomForest(
    x = as.matrix(train_data[, selected_columns]),
    y = train_data$training_target,
    ntree = 300,
    mtry = 2,
    node_size = 3
  )
  
  # Predict probabilities on test set
  rf_pred <- predict(rf_model, as.matrix(test_data[, selected_columns]))
  
  # Calculate ROC AUC
  roc_auc <- roc(test_data$training_target, rf_pred)
  return(auc(roc_auc))
}

# Apply 5-fold cross-validation and store ROC AUC results for Random Forest
rf_roc_auc_scores <- sapply(folds, function(fold_indices) {
  rf_roc_auc(train_index = fold_indices, test_index = setdiff(seq_len(nrow(data)), fold_indices))
})

# Compute mean ROC AUC for Random Forest
mean_rf_roc_auc <- mean(rf_roc_auc_scores)

# Define parameters for XGBoost
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta = 0.1,
  max_depth = 9,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  alpha = 1,
  lambda = 1
)
nrounds <- 200

# Convert target variable to numeric for XGBoost
data$training_target <- as.numeric(as.factor(data$training_target)) - 1
data$Y1112 <- as.numeric(as.factor(data$Y1112)) - 1
data$P1112 <- as.numeric(as.factor(data$P1112)) - 1
data$P1314 <- as.numeric(as.factor(data$P1314)) - 1
data$P1516 <- as.numeric(as.factor(data$P1516)) - 1

# Function to train XGBoost model and calculate ROC AUC for each fold
xgb_roc_auc <- function(train_index, test_index) {
  train_data <- data[train_index, ]
  test_data <- data[test_index, ]
  
  xgb_model <- xgboost(
    data = as.matrix(train_data[, selected_columns]),
    label = train_data$training_target,
    params = params,
    nrounds = nrounds,
    verbose = 0
  )
  
  # Predict probabilities on test set
  xgb_pred <- predict(xgb_model, as.matrix(test_data[, selected_columns]))
  
  # Calculate ROC AUC
  roc_auc <- roc(test_data$training_target, xgb_pred)
  return(auc(roc_auc))
}

# Apply 5-fold cross-validation and store ROC AUC results for XGBoost
xgb_roc_auc_scores <- sapply(folds, function(fold_indices) {
  xgb_roc_auc(train_index = fold_indices, test_index = setdiff(seq_len(nrow(data)), fold_indices))
})

# Compute mean ROC AUC for XGBoost
mean_xgb_roc_auc <- mean(xgb_roc_auc_scores)

# Compare Random Forest and XGBoost models
cat("Mean ROC AUC for Random Forest:", mean_rf_roc_auc, "\n")
cat("Mean ROC AUC for XGBoost:", mean_xgb_roc_auc, "\n")
