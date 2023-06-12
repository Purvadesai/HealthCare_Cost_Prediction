library(ggplot2)
library(dplyr)
library(Hmisc)
library(cowplot)
library(WVPlots)
set.seed(123)
insurance_data <- read.csv("~/Downloads/insurance.csv")
insurance_data
sample_n(insurance_data, 5)

#Exploratary Data Analysis

ages_vs_charges <- ggplot(insurance_data, aes(age, charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

bmi_vs_charges <- ggplot(insurance_data, aes(bmi, charges)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

p <- plot_grid(ages_vs_charges, bmi_vs_charges) 
title <- ggdraw() + draw_label("1. Correlation between Charges and Age / BMI", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
#----------------------------------------------
sex_vs_charges <- ggplot(insurance_data, aes(sex, charges)) +
  geom_jitter(aes(color = sex), alpha = 0.5) +
  theme_light()

children_vs_charges <- ggplot(insurance_data, aes(children, charges)) +
  geom_jitter(aes(color = children), alpha = 0.5) +
  theme_classic()

p <- plot_grid(sex_vs_charges, children_vs_charges)
title <- ggdraw() + draw_label("2. Correlation between Charges and Sex / Children covered by insurance", fontface = 'bold')
plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1))
#---------------------------------------------
smoker_vs_charges <- ggplot(insurance_data, aes(smoker, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.5) +
  theme_light()

region_vs_charges <- ggplot(insurance_data, aes(region, charges)) +
  geom_jitter(aes(color = region), alpha = 0.5) +
  theme_light()

p <- plot_grid(smoker_vs_charges, region_vs_charges) 
title <- ggdraw() + draw_label("3. Correlation between Charges and Smoker / Region", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

#Linear regression model
# first we will divide the data into train and test.
train <- round(0.8 * nrow(insurance_data))
train_indices <- sample(1:nrow(insurance_data), train)
insurance_data_train <- insurance_data[train_indices, ]
insurance_data_test <- insurance_data[-train_indices, ]

# 1st model
model_1 <- lm("charges ~ age + bmi + children + smoker + region", data = insurance_data_train)
summary(model_1)
#Saving R-squared
r_sq_1 <- summary(model_1)$r.squared

#Now we will predict data on test set
pred1 <- predict(model_1, newdata = insurance_data_test)
#calculating the residuals
residuals_1 <- insurance_data_test$charges - pred1
residuals_1
#calculating Root Mean Squared Error
rmse_1 <- sqrt(mean(residuals_1^2))
rmse_1

#2nd model
model_2 <- lm("charges ~ age + sex + bmi + children + smoker + region", data = insurance_data_train)
summary(model_2)
#Saving R-squared
r_sq_2 <- summary(model_2)$r.squared
#Now we will predict data on test set
pred2 <- predict(model_2, newdata = insurance_data_test)
#calculating the residuals
residuals_2 <- insurance_data_test$charges - pred2
residuals_2
#calculating Root Mean Squared Error
rmse_2 <- sqrt(mean(residuals_2^2))
rmse_2


print(paste0("R-squared for 1st model:", round(r_sq_1, 4)))
print(paste0("R-squared for 2nd model: ", round(r_sq_2, 4)))
print(paste0("RMSE for 1st model: ", round(rmse_1, 2)))
print(paste0("RMSE for 2nd model: ", round(rmse_2, 2)))

insurance_data_test$prediction <- predict(model_2, newdata = insurance_data_test)
ggplot(insurance_data_test, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue", alpha = 0.5) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")

insurance_data_test$residuals <- insurance_data_test$charges - insurance_data_test$prediction

ggplot(data = insurance_data_test, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "red") +
  ggtitle("Residuals vs. Linear model prediction")





