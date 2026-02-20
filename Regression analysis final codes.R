# PART 2: EXPLORATORY DATA ANALYSIS
# Load the dataset
hr <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Inspect the structure of the dataset
str(hr)

# Basic Dataset Overview

# Dimensions of the dataset (number of rows and columns)
dim(hr)

# Display the first few observations
head(hr)

# Summary statistics for all variables
summary(hr)

# Missing Values Check
colSums(is.na(hr)) # Count missing values for each variable
# as we see, there is no missing value in our data

# Remove Non-informative Variables
# Remove variables with zero variance or no analytical value
library(dplyr)
hr <- hr %>%
  select(-Over18, -EmployeeCount, -StandardHours)

# Identify and Convert Variable Types
# Convert categorical variables to factors
hr <- hr %>%
  mutate(
    Attrition        = factor(Attrition),
    Gender           = factor(Gender),
    OverTime         = factor(OverTime),
    BusinessTravel   = factor(BusinessTravel),
    Department       = factor(Department),
    EducationField   = factor(EducationField),
    JobRole          = factor(JobRole),
    MaritalStatus    = factor(MaritalStatus)
  )

# Check structure again to confirm changes
str(hr)

# Numerical Summary (ALL continuous variables)
num_vars <- c("Age","MonthlyIncome","DailyRate","HourlyRate",
"MonthlyRate","TotalWorkingYears",
"YearsAtCompany","DistanceFromHome")

summary_table <- hr %>%
  summarise(across(all_of(num_vars),
                   list(
                     mean   = ~mean(.x, na.rm = TRUE),
                     median = ~median(.x, na.rm = TRUE),
                     sd     = ~sd(.x, na.rm = TRUE),
                     min    = ~min(.x, na.rm = TRUE),
                     max    = ~max(.x, na.rm = TRUE)
                   )))
summary_table

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

# Visualizations
# 1. Distribution of Monthly Income (Histogram)
ggplot(hr, aes(x = MonthlyIncome)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Monthly Income",
       x = "Monthly Income",
       y = "Number of Employees")

# 2. Income vs Experience by Job Level (MULTIVARIATE EDA)
ggplot(hr, aes(x = TotalWorkingYears, y = MonthlyIncome,
               color = factor(JobLevel))) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Monthly Income vs Experience by Job Level",
    x = "Total Working Years",
    y = "Monthly Income",
    color = "Job Level"
  )

# 3. Monthly Income by Job Role (Boxplot)
ggplot(hr, aes(x = JobRole, y = MonthlyIncome)) +
  geom_boxplot(fill = "darkgreen") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Monthly Income by Job Role",
    x = "Job Role",
    y = "Monthly Income"
  )

# 4. CORRELATION
ggplot(cor_long, aes(x = var1, y = var2, fill = r)) +
  geom_tile(color = "white", linewidth = 0.3) +
  coord_fixed() +
  
  # Contrasting palette
  scale_fill_gradient2(
    low = "#0D47A1",  
    mid = "white",  
    high = "#B71C1C", 
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  
  labs(
    title = "Correlation Heatmap (Numeric Variables)",
    x = "",
    y = ""
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    
    # key strings for width and captions
    axis.text.x = element_text(
      angle = 60,
      hjust = 1,
      vjust = 1,
      size = 10
    ),
    axis.text.y = element_text(size = 10),
    
    panel.grid = element_blank()
  )

# 5.Age distribution
library(ggplot2)

ggplot(hr, aes(x = Age, fill = Attrition)) +
  geom_histogram(
    bins = 30,
    position = "identity",
    alpha = 0.7,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("No" = "#E5739A", "Yes" = "grey60")
  ) +
  labs(
    title = "Age Distribution by Attrition",
    x = "Age",
    y = "Count",
    fill = "Attrition"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# 6.Years at company distribution
library(ggplot2)

ggplot(hr, aes(x = YearsAtCompany, fill = Attrition)) +
  geom_histogram(
    bins = 30,
    position = "identity",
    alpha = 0.7,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("No" = "#7FCDBB", "Yes" = "#F4A582")
  ) +
  labs(
    title = "Years at Company Distribution by Attrition",
    x = "Years at Company",
    y = "Count",
    fill = "Attrition"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# 7.Attrition by business travel frequency
library(ggplot2)

ggplot(hr, aes(x = BusinessTravel, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(
    values = c("Yes" = "#4C72B0", "No" = "#DD8452")
  ) +
  labs(
    title = "Attrition by Business Travel Frequency",
    x = "Business Travel",
    y = "Number of Employees",
    fill = "Attrition"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )
# 8.Distance from Home distribution
library(ggplot2)

ggplot(hr, aes(x = DistanceFromHome, fill = Attrition)) +
  geom_histogram(
    bins = 30,
    position = "identity",
    alpha = 0.75,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("No" = "#2C9FD6", "Yes" = "#F04B3E")
  ) +
  labs(
    title = "Distance From Home Distribution by Attrition",
    x = "Distance From Home",
    y = "Count",
    fill = "Attrition"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )


# PART 3: MODELING

# SALARY MODELING
# Model 1: Career basics
m1 <- lm(MonthlyIncome ~ JobLevel + TotalWorkingYears,
         data = train)

pred1 <- predict(m1, newdata = test)
rmse1 <- sqrt(mean((test$MonthlyIncome - pred1)^2))
mae1  <- mean(abs(test$MonthlyIncome - pred1))
r2_1  <- 1 - sum((test$MonthlyIncome - pred1)^2) /
  sum((test$MonthlyIncome - mean(test$MonthlyIncome))^2)

# Model 2: Add JobRole
m2 <- lm(MonthlyIncome ~ JobLevel + TotalWorkingYears + JobRole,
         data = train)

pred2 <- predict(m2, newdata = test)
rmse2 <- sqrt(mean((test$MonthlyIncome - pred2)^2))
mae2  <- mean(abs(test$MonthlyIncome - pred2))
r2_2  <- 1 - sum((test$MonthlyIncome - pred2)^2) /
  sum((test$MonthlyIncome - mean(test$MonthlyIncome))^2)

# Model 3: Add interaction ONLY
m3 <- lm(
  MonthlyIncome ~ JobLevel + TotalWorkingYears + JobRole +
    JobLevel:JobRole,
  data = train
)

pred3 <- predict(m3, newdata = test)
rmse3 <- sqrt(mean((test$MonthlyIncome - pred3)^2))
mae3  <- mean(abs(test$MonthlyIncome - pred3))
r2_3  <- 1 - sum((test$MonthlyIncome - pred3)^2) /
  sum((test$MonthlyIncome - mean(test$MonthlyIncome))^2)

# Comparison table
income_incremental <- data.frame(
  Model = c(
    "M1: JobLevel + TotalWorkingYears",
    "M2: + JobRole",
    "M3: + JobLevel×JobRole"
  ),
  RMSE = c(rmse1, rmse2, rmse3),
  MAE  = c(mae1,  mae2,  mae3),
  R2   = c(r2_1,  r2_2,  r2_3)
)
income_incremental

# ATTRITION MODEL
pkgs <- c("dplyr","caret","pROC","readr","tibble")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(caret)
library(pROC)
library(readr)
library(tibble)

hr <- hr %>%
  mutate(
    Attrition = as.character(Attrition),
    Attrition = ifelse(Attrition %in% c("Yes","No"), Attrition, NA),
    Attrition = factor(Attrition, levels = c("No","Yes"))
  )

num_candidates <- c(
  "Age","YearsAtCompany","JobLevel","DistanceFromHome",
  "MonthlyIncome","PerformanceRating","TotalWorkingYears"
)
num_candidates <- intersect(num_candidates, names(hr))

hr[num_candidates] <- lapply(hr[num_candidates], function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) x <- gsub(",", "", x)
  suppressWarnings(as.numeric(x))
})

cat_candidates <- c(
  "JobRole","Department","Education",
  "OverTime","WorkLifeBalance",
  "JobSatisfaction","EnvironmentSatisfaction"
)
cat_candidates <- intersect(cat_candidates, names(hr))
hr[cat_candidates] <- lapply(hr[cat_candidates], factor)

vars_m1 <- c("Attrition","Age","TotalWorkingYears","YearsAtCompany")
vars_m2 <- c(vars_m1,"JobLevel","JobRole","Department","MonthlyIncome")
vars_m3 <- c(vars_m2,"OverTime","WorkLifeBalance","JobSatisfaction","EnvironmentSatisfaction")

needed_vars <- unique(c(vars_m1, vars_m2, vars_m3))
needed_vars <- intersect(needed_vars, names(hr))

hr_model <- hr |>
  select(all_of(needed_vars)) |>
  filter(!is.na(Attrition)) |>
  na.omit()

sapply(hr_model[, intersect(c("Age","MonthlyIncome","TotalWorkingYears","Attrition"),
                            names(hr_model))], class)
table(hr_model$Attrition)

set.seed(123)
train_index <- createDataPartition(hr_model$Attrition, p = 0.7, list = FALSE)
train <- hr_model[train_index, ]
test  <- hr_model[-train_index, ]

eval_logit <- function(model, test_df, threshold = 0.5) {
  prob <- predict(model, newdata = test_df, type = "response")
  pred <- factor(ifelse(prob > threshold, "Yes","No"),
                 levels = c("No","Yes"))
  cm <- confusionMatrix(pred, test_df$Attrition)
  roc_obj <- roc(test_df$Attrition, prob,
                 levels = c("No","Yes"), direction = "<")
  list(cm = cm, roc = roc_obj, auc = as.numeric(auc(roc_obj)))
}

model1 <- glm(
  Attrition ~ Age + TotalWorkingYears + YearsAtCompany,
  data = train,
  family = binomial
)
summary(model1)
res1 <- eval_logit(model1, test)
res1$cm
res1$auc

model2 <- glm(
  Attrition ~ Age + TotalWorkingYears + YearsAtCompany +
    JobLevel + JobRole + Department + MonthlyIncome,
  data = train,
  family = binomial
)
summary(model2)
res2 <- eval_logit(model2, test)
res2$cm
res2$auc

model3 <- glm(
  Attrition ~ Age + TotalWorkingYears + YearsAtCompany +
    JobLevel + JobRole + Department + MonthlyIncome +
    OverTime + WorkLifeBalance + JobSatisfaction +
    EnvironmentSatisfaction,
  data = train,
  family = binomial
)
summary(model3)
res3 <- eval_logit(model3, test)
res3$cm
res3$auc

plot(res1$roc, col = "grey60", lwd = 2,
     main = "ROC Curves for Attrition Models")
plot(res2$roc, col = "steelblue", lwd = 2, add = TRUE)
plot(res3$roc, col = "firebrick", lwd = 2, add = TRUE)

legend(
  "bottomright",
  legend = c(
    paste0("Model 1 (AUC=", round(res1$auc, 3), ")"),
    paste0("Model 2 (AUC=", round(res2$auc, 3), ")"),
    paste0("Model 3 (AUC=", round(res3$auc, 3), ")")
  ),
  col = c("grey60","steelblue","firebrick"),
  lwd = 2,
  bty = "n"
)

AIC(model1, model2, model3)

# STEP 4: DIAGNOSTICS 

# FOR SALARY MODEL
pkgs <- c("ggplot2","broom","dplyr","scales","ggrepel",
          "RColorBrewer","ggsci","scico")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if(length(to_install)) install.packages(to_install)

library(ggplot2); library(broom); library(dplyr)
library(scales); library(ggrepel)
library(RColorBrewer); library(ggsci); library(scico)

# Model
m_final <- m3

# Theme
theme_set(theme_minimal(base_size = 13))
base_theme <- theme(
  plot.title = element_text(face = "bold"),
  panel.grid.minor = element_blank(),
  axis.title = element_text(face = "bold")
)

# Diagnostics df
diag <- augment(m_final) %>% mutate(id = row_number())
n <- nrow(diag)
p <- length(coef(m_final))

diag <- diag %>%
  mutate(abs_std = sqrt(abs(.std.resid)),
         lev_cut = 2 * p / n)

top_infl <- diag %>% arrange(desc(.cooksd)) %>% slice(1:8)

# Colors
cols_brew <- brewer.pal(8, "Dark2")
cols_npg  <- ggsci::pal_npg("nrc")(10)

# A) Residuals vs Fitted  (Brewer)
p1 <- ggplot(diag, aes(.fitted, .resid)) +
  geom_point(alpha = 0.35, color = viridis(6)[2]) +
  geom_smooth(method = "loess", se = TRUE, color = viridis(6)[5], linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey10") +
  labs(title = "Residuals vs Fitted (Linear model)",
       x = "Fitted values", y = "Residuals") +
  base_theme
p1

# B) Q–Q plot (NPG)
p2 <- ggplot(diag, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.35, color = cols_npg[5]) +
  stat_qq_line(color = cols_npg[6], linewidth = 1.1) +
  labs(title = "Normal Q–Q Plot (Linear model)",
       x = "Theoretical quantiles", y = "Standardized residuals") +
  base_theme
p2

# C) Scale–Location (Brewer)
p3 <- ggplot(diag, aes(.fitted, abs_std)) +
  geom_point(alpha = 0.35, color = cols_brew[2]) +
  geom_smooth(method = "loess", se = TRUE, color = cols_brew[7], linewidth = 1.1) +
  labs(title = "Scale–Location (Linear model)",
       x = "Fitted values", y = "sqrt(|standardized residuals|)") +
  base_theme
p3

# D) Influence (scico)
p4 <- ggplot(diag, aes(.hat, .std.resid)) +
  geom_vline(xintercept = unique(diag$lev_cut), linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = c(-2, 0, 2),
             linetype = c("dashed","solid","dashed"),
             color = c("grey40","grey20","grey40")) +
  geom_point(aes(size = .cooksd, color = .cooksd), alpha = 0.55) +
  scale_color_scico(palette = "batlow", name = "Cook's D") +
  scale_size_continuous(range = c(1.5, 10), name = "Cook's D") +
  geom_text_repel(data = top_infl, aes(label = id),
                  size = 3.5, max.overlaps = 20) +
  labs(title = "Influence (Linear model)",
       x = "Leverage (hat values)", y = "Standardized residuals") +
  base_theme
p4

# E) Observed vs Predicted (NPG)
pred_test <- predict(m_final, newdata = test)
plot_df <- data.frame(y = test$MonthlyIncome, yhat = pred_test)

p5 <- ggplot(plot_df, aes(yhat, y)) +
  geom_point(alpha = 0.35, color = cols_npg[5]) +
  geom_smooth(method = "lm", se = FALSE, color = cols_npg[6], linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  labs(title = "Observed vs Predicted (Test set, Linear model)",
       x = "Predicted MonthlyIncome", y = "Observed MonthlyIncome") +
  base_theme
p5

# With log transformation
# Fit log-income model
m_log <- lm(
  log(MonthlyIncome) ~ JobLevel + TotalWorkingYears + JobRole + JobLevel:JobRole,
  data = train
)

# Predict on test (log scale)
pred_log <- predict(m_log, newdata = test)

# (1) Metrics on LOG scale

y_log <- log(test$MonthlyIncome)

rmse_log <- sqrt(mean((y_log - pred_log)^2))
mae_log  <- mean(abs(y_log - pred_log))

r2_log <- 1 - sum((y_log - pred_log)^2) / sum((y_log - mean(y_log))^2)

c(RMSE_log = rmse_log, MAE_log = mae_log, R2_log = r2_log)

# (2) Back-transform to ORIGINAL $ scale

pred_income <- exp(pred_log)

rmse_orig <- sqrt(mean((test$MonthlyIncome - pred_income)^2))
mae_orig  <- mean(abs(test$MonthlyIncome - pred_income))

r2_orig <- 1 - sum((test$MonthlyIncome - pred_income)^2) /
  sum((test$MonthlyIncome - mean(test$MonthlyIncome))^2)

c(RMSE_original = rmse_orig, MAE_original = mae_orig, R2_original = r2_orig)

# (3) bias-corrected back-transform
# (log-normal correction using residual variance)

sigma2 <- mean(residuals(m_log)^2)  # approx Var(error) on log scale
pred_income_bc <- exp(pred_log + 0.5 * sigma2)

rmse_orig_bc <- sqrt(mean((test$MonthlyIncome - pred_income_bc)^2))
mae_orig_bc  <- mean(abs(test$MonthlyIncome - pred_income_bc))

r2_orig_bc <- 1 - sum((test$MonthlyIncome - pred_income_bc)^2) /
  sum((test$MonthlyIncome - mean(test$MonthlyIncome))^2)

c(RMSE_original_biascorrected = rmse_orig_bc,
  MAE_original_biascorrected  = mae_orig_bc,
  R2_original_biascorrected   = r2_orig_bc)

# Approach for heteroskedasticity
library(splines)

m_base <- lm(MonthlyIncome ~ JobLevel + TotalWorkingYears + JobRole + JobLevel:JobRole,
             data = train)

pred_base <- predict(m_base, newdata = test)
rmse_base <- sqrt(mean((test$MonthlyIncome - pred_base)^2))
mae_base  <- mean(abs(test$MonthlyIncome - pred_base))
r2_base   <- 1 - sum((test$MonthlyIncome - pred_base)^2) /
  sum((test$MonthlyIncome - mean(test$MonthlyIncome))^2)

# Spline TotalWorkingYears (mild nonlinearity)
m_spline <- lm(MonthlyIncome ~ JobLevel + ns(TotalWorkingYears, df = 4) +
                 JobRole + JobLevel:JobRole,
               data = train)

pred_spline <- predict(m_spline, newdata = test)
rmse_spline <- sqrt(mean((test$MonthlyIncome - pred_spline)^2))
mae_spline  <- mean(abs(test$MonthlyIncome - pred_spline))
r2_spline   <- 1 - sum((test$MonthlyIncome - pred_spline)^2) /
  sum((test$MonthlyIncome - mean(test$MonthlyIncome))^2)

data.frame(
  Model = c("Base linear", "Spline(TotalWorkingYears, df=4)"),
  RMSE  = c(rmse_base, rmse_spline),
  MAE   = c(mae_base,  mae_spline),
  R2    = c(r2_base,   r2_spline)
)

# Sensitivity check (Influence)
m_base <- m_spline   # <- or m3 if needed

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))
r2   <- function(y, yhat){
  1 - sum((y - yhat)^2) / sum((y - mean(y))^2)
}

# Influence measures on TRAIN
n <- nrow(train)
p <- length(coef(m_base))

cooks <- cooks.distance(m_base)
lev   <- hatvalues(m_base)
rstd  <- rstandard(m_base)

# Cutoffs
cooks_cut <- 4 / n
lev_cut   <- 2 * p / n
rstd_cut  <- 3

# Flag influential observations
infl_id <- which(cooks > cooks_cut | lev > lev_cut | abs(rstd) > rstd_cut)

# Refit after removing influential points
train_sens <- train[-infl_id, , drop = FALSE]
m_sens <- lm(formula(m_base), data = train_sens)

# Compare TEST prediction error
pred_base <- predict(m_base, newdata = test)
pred_sens <- predict(m_sens, newdata = test)

sens_compare <- data.frame(
  Model = c("Original", "Sensitivity refit (removed influential)"),
  RMSE  = c(rmse(test$MonthlyIncome, pred_base),
            rmse(test$MonthlyIncome, pred_sens)),
  MAE   = c(mae(test$MonthlyIncome, pred_base),
            mae(test$MonthlyIncome, pred_sens)),
  R2    = c(r2(test$MonthlyIncome, pred_base),
            r2(test$MonthlyIncome, pred_sens))
)

sens_compare


# For ATTRITION MODEL
hr$JobRole <- factor(hr$JobRole)
hr$Department <- factor(hr$Department)
hr$OverTime <- factor(hr$OverTime)

library(car)
library(caret)
library(pROC)
library(glmnet)

predicted_classes <- ifelse(prob3 > 0.5, "Yes", "No") |>
  factor(levels = c("No", "Yes"))

vars3 <- all.vars(formula(model3))

test3 <- na.omit(test[, vars3])

prob3 <- predict(model3, newdata = test3, type = "response")

predicted_classes <- factor(
  ifelse(prob3 > 0.5, "Yes", "No"),
  levels = c("No", "Yes")
)

conf_matrix <- confusionMatrix(predicted_classes, test3$Attrition)
conf_matrix

roc_curve <- roc(test$Attrition, prob3)
plot(roc_curve, main = "ROC Curve for Model 3")
auc_value <- auc(roc_curve)
auc_value

precision <- conf_matrix$byClass["Precision"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
F1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
F1_score

vif(model3)

summary(model3)

library(glmnet)

ridge_vars <- c(
  "Attrition","Age","TotalWorkingYears","YearsAtCompany",
  "JobLevel","JobRole","Department","MonthlyIncome",
  "OverTime","WorkLifeBalance","JobSatisfaction","EnvironmentSatisfaction"
)
ridge_vars <- intersect(ridge_vars, names(train))

selected_vars <- train[, ridge_vars]
selected_vars <- na.omit(selected_vars)

x <- model.matrix(Attrition ~ . , data = selected_vars)[, -1]
y <- ifelse(selected_vars$Attrition == "Yes", 1, 0)

fit_attrition_logistic_ridge <- glmnet(
  x, y,
  family = "binomial",
  alpha = 0
)

coef(fit_attrition_logistic_ridge)

stepwise_model <- step(model3, direction = "both", trace = 0)
summary(stepwise_model)

model3_updated <- glm(
  Attrition ~ Age + YearsAtCompany + JobLevel + MonthlyIncome +
    WorkLifeBalance + JobSatisfaction + EnvironmentSatisfaction +
    OverTime + Department,
  data = train,
  family = binomial
)

summary(model3_updated)

vars_upd <- all.vars(formula(model3_updated))

test_upd <- test[, vars_upd]
test_upd <- na.omit(test_upd)

prob_updated <- predict(model3_updated, newdata = test_upd, type = "response")

pred_updated <- factor(
  ifelse(prob_updated > 0.5, "Yes", "No"),
  levels = c("No", "Yes")
)

conf_matrix_updated <- confusionMatrix(pred_updated, test_upd$Attrition)
conf_matrix_updated


roc_curve_updated <- roc(test$Attrition, prob_updated)
plot(roc_curve_updated, main = "ROC Curve for Updated Model")

auc_updated <- auc(roc_curve_updated)
auc_updated

sensitivity_updated <- conf_matrix_updated$byClass["Sensitivity"]
specificity_updated <- conf_matrix_updated$byClass["Specificity"]
sensitivity_updated
specificity_updated

precision_updated <- conf_matrix_updated$byClass["Precision"]
F1_score_updated <- 2 * (precision_updated * sensitivity_updated) /
  (precision_updated + sensitivity_updated)
F1_score_updated

vif(model3_updated)


# PART 5: Final Models

# SALARY REGRESSION
library(lmtest)
library(sandwich)
library(car)
library(dplyr)

# Final salary model
m3 <- lm(
  MonthlyIncome ~ JobLevel + TotalWorkingYears + JobRole + JobLevel:JobRole,
  data = train
)

# White heteroskedasticity-robust covariance
V_white <- vcovHC(m3, type = "HC1")

# Robust coefficients and confidence intervals(p values)
robust_tab <- coeftest(m3, vcov. = V_white)
ci_white <- coefci(m3, vcov. = V_white, level = 0.95)

coef_table <- data.frame(
  term      = rownames(robust_tab),
  estimate  = robust_tab[,1],
  robust_se = robust_tab[,2],
  p_value   = robust_tab[,4],
  CI_low    = ci_white[,1],
  CI_high   = ci_white[,2],
  row.names = NULL
)

coef_table

# Robust F-tests
joblevel_test <- linearHypothesis(m3, "JobLevel = 0", vcov. = V_white, test = "F")
twy_test      <- linearHypothesis(m3, "TotalWorkingYears = 0", vcov. = V_white, test = "F")

role_terms <- grep("^JobRole", names(coef(m3)), value = TRUE)
int_terms  <- grep("^JobLevel:JobRole", names(coef(m3)), value = TRUE)

role_test <- linearHypothesis(m3, paste0(role_terms, " = 0"), vcov. = V_white, test = "F")
int_test  <- linearHypothesis(m3, paste0(int_terms,  " = 0"), vcov. = V_white, test = "F")

data.frame(
  Block = c("JobLevel", "TotalWorkingYears", "JobRole (overall)", "JobLevel × JobRole (overall)"),
  F_stat = c(joblevel_test$F[2], twy_test$F[2], role_test$F[2], int_test$F[2]),
  p_value = c(joblevel_test$`Pr(>F)`[2], twy_test$`Pr(>F)`[2], role_test$`Pr(>F)`[2], int_test$`Pr(>F)`[2])
)

# 10-fold cross-validation RMSE
cv_rmse <- function(formula, data, K = 10, seed = 123){
  set.seed(seed)
  n <- nrow(data)
  fold <- sample(rep(1:K, length.out = n))
  rmses <- numeric(K)
  for(k in 1:K){
    fit <- lm(formula, data = data[fold != k, ])
    pred <- predict(fit, newdata = data[fold == k, ])
    rmses[k] <- sqrt(mean((data$MonthlyIncome[fold == k] - pred)^2))
  }
  c(mean = mean(rmses), sd = sd(rmses))
}

cv_rmse(
  MonthlyIncome ~ JobLevel + TotalWorkingYears + JobRole + JobLevel:JobRole,
  data = train
)

# ATTRITION REGRESSION

hr_clean$JobRole <- factor(hr$JobRole)
hr_clean$Department <- factor(hr$Department)
hr_clean$OverTime <- factor(hr$OverTime)

# 1. P-Values for model3_updated (Already done using summary)
summary(model3_updated)

# 2. Confidence Intervals for model3_updated
# Using the confint() function to get 95% confidence intervals for each coefficient
confint(model3_updated)

# Perform the Likelihood Ratio Test
anova(model3_updated, model3, test = "LRT")

# 4. F-Statistics from Likelihood Ratio Test
# Extract the likelihood ratio test results
lrt_results <- anova(model3_updated, model3, test = "LRT")
lrt_results






