library(tidyverse)
library(leaps)
library(forecast)
library(corrplot)

#load dataset
loans_df <- read.csv("loans_full_schema.csv")
summary(loans_df)

#visualize NA values in columns
loans_df  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank())

# Remove columns which contain character values that cannot be encoded and the columns which are dependent on interest rate
columns <- c("emp_title","loan_purpose" , "state", "installment", "loan_status", "grade", "sub_grade", "issue_month", "loan_status", "initial_listing_status", "disbursement_method", "balance", "paid_total","paid_principal","paid_interest","paid_late_fees")
loan_lr_df <- loans_df[,!(names(loans_df) %in% columns)]
view(loan_lr_df)

#Identify unique values in home ownership column and encode them
unique(loan_lr_df["homeownership"])
loan_lr_df['MORTGAGE_HOME'] = ifelse(loan_lr_df['homeownership'] == "MORTGAGE", 1, 0)
loan_lr_df['RENT_HOME'] = ifelse(loan_lr_df['homeownership'] == "RENT", 1, 0)

#Identify unique values in income verificcation column and encode them
unique(loan_lr_df["verified_income"])
loan_lr_df['Income_verified'] = ifelse(loan_lr_df['verified_income'] == "Verified", 1, 0)
loan_lr_df['Income_source_verified'] = ifelse(loan_lr_df['verified_income'] == "Source Verified", 1, 0)

#Identify unique values in joint income verification column column and encode them
#unique(loan_lr_df["verification_income_joint"])
#loan_lr_df['income_verified_joint'] = ifelse(loan_lr_df['verification_income_joint'] == "Verified", 1, 0)
#loan_lr_df['income_source_verified_joint'] = ifelse(loan_lr_df['verification_income_joint'] == "Verified", 1, 0)

#encoding application type
unique(loan_lr_df["application_type"])
loan_lr_df['individual_application'] = ifelse(loan_lr_df['application_type'] == "individual", 1, 0)

#Remove Joint records from the dataframe
#Removing records where joint annual income is N/A as these records are for joint applications
library(tidyr)
loan_lr_df[loan_lr_df=='NA'] <- NA
loan_lr_df<-loan_lr_df[is.na(loan_lr_df$annual_income_joint),]
#loan_lr_df<-loan_lr_df[(loan_lr_df$individual_application != 0),]
View(loan_lr_df)

#remove homeownership, income verification, joint income verification columns
col <- c("homeownership","verified_income", "verification_income_joint", "application_type","debt_to_income_joint","annual_income_joint")
loan_lr_df <- loan_lr_df[,!(names(loan_lr_df) %in% col)]
View(loan_lr_df)

#looking at outliers
boxplot(loan_lr_df$annual_income, ylab = "annual_income")
loan_lr_df <- loan_lr_df[loan_lr_df$annual_income < 1500000,]

boxplot(loan_lr_df$debt_to_income, ylab = "debt_to_income")

#to visualize correlation between the variables and keeping only the highly correlated variables to reduce noise
corr_simple <- function(data=loan_lr_df,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}
corr_simple()


#Splitting dataset into train test and validation 
set.seed(1)
numberOfRows <- nrow(loan_lr_df)
train_index <- sample(numberOfRows, numberOfRows*0.5)
train_df <- loan_lr_df[train_index,]
view(train_df)

valid_index <- sample(numberOfRows, numberOfRows*0.3)
valid_df <- loan_lr_df[valid_index,]


test_index <- sample(numberOfRows, numberOfRows*0.2)
test_df <- loan_lr_df[test_index,]

#Do search to get best variables
search <- regsubsets(interest_rate ~ ., data = train_df, nbest = 1, nvmax = dim(train_df)[2],
                     method = "exhaustive")
sum <- summary(search)
summary(search)
# show models
sum$which
# show metrics
sum$rsq
sum$adjr2
par(mfrow = c(1, 1))
plot(search, scale="r2")

# Choose a model by AIC in a Stepwise Algorithm
# Forward regression Start with no predictors add them one by one (add the one with largest contribution based on AIC)
train.df = na.omit(train_df)
loans.lm <- lm(formula = interest_rate ~ ., data = train_df)
loans.lm.fwd <- step(loans.lm, direction = "forward", trace = 1)

options(scipen = TRUE)
summary(loans.lm.fwd)
loans.lm.fwd.pred <- predict(loans.lm.fwd, valid_df)
accuracy(loans.lm.fwd.pred, valid_df$interest_rate)

library(dplyr)
library(Hmisc)
library(ggplot2)

###plot predicted price vs target price for range of prices
df <- data.frame("Predicted" = loans.lm.fwd.pred, "Actual" = valid_df$interest_rate)
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)
df

# %>% is a pipe operator.  Pipes let you pipe a value forward in an expression.
bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

bin_stats

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 

p1

# backward regression Start with all predictors and Successively eliminate least useful predictors one by one
# Stop when all remaining predictors have statistically significant contribution
train.df = na.omit(train_df)
valid.df = na.omit(valid_df)
loans.lm <- lm(formula = interest_rate ~ ., data = train.df)
loans.lm.bwd <- step(loans.lm, direction = "backward", trace = 1)
summary(loans.lm.bwd)
loans.lm.bwd.pred <- predict(loans.lm.bwd, valid.df)
accuracy(loans.lm.bwd.pred, valid.df$interest_rate)


df <- data.frame("Predicted" = loans.lm.bwd.pred, "Actual" = valid.df$interest_rate)
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)
df

# %>% is a pipe operator.  Pipes let you pipe a value forward in an expression.
bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

bin_stats

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 

p1




#Using lasso regression to minimize the insignificant variables to zero and fit the regression model to predict the interest rate
library(glmnet)

train.df

y = train.df$interest_rate
#y = train.df[, c('emp_length', 'annual_income', 'debt_to_income', 'delinq_2y', 'months_since_last_delinq','earliest_credit_line','inquiries_last_12m','total_credit_lines','open_credit_lines','total_credit_limit','total_credit_utilized','num_collections_last_12m','num_historical_failed_to_pay','months_since_90d_late','current_accounts_delinq','total_collection_amount_ever','current_installment_accounts','accounts_opened_24m','months_since_last_credit_inquiry','num_satisfactory_accounts','num_accounts_120d_past_due','num_accounts_30d_past_due','num_active_debit_accounts','total_debit_limit','num_total_cc_accounts','num_open_cc_accounts','num_cc_carrying_balance','num_mort_accounts','account_never_delinq_percent','tax_liens','public_record_bankrupt','loan_amount','term','homeownership','verified_income','verified_income','application_type')]
c <- c('interest_rate')
x <- data.matrix(train.df[,!(names(train.df) %in% c)])
#x <- data.matrix(y)

#fit lasso regression model using k-fold cross-validation to find best lambda 
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda

#view plot of test MSE's vs. lambda values
plot(cv_model)

#to fit the the model on best lambda value
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#make a prediction for the response value 
lasso_pred = predict(best_model, s = best_lambda, newx = x)


#To find R squared of lasso over training data
sst <- sum((y - mean(y))^2)
sse <- sum((lasso_pred - y)^2)

rsq <- 1 - sse/sst
rsq

#predict validation split
val.df = data.matrix(valid.df[,!(names(valid.df) %in% c)])
lasso_pred_valid = predict(best_model, s = best_lambda, newx = val.df)
#df <- data.frame("Predicted" = lasso_pred, "Actual" = valid.df$interest_rate)

#To find R squared of lasso over validation data
sst <- sum((valid.df$interest_rate - mean(valid.df$interest_rate))^2)
sse <- sum((lasso_pred_valid - valid.df$interest_rate)^2)

rsq <- 1 - sse/sst
rsq

