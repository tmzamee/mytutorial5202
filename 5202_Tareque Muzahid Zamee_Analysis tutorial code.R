library(tidyverse)
require(tidyverse)
head(heart)
tail(heart)
heart[1,5]
heart[2,5]
model <- lm(trestbps ~ age * chol, data = heart)
summary(model)

library(dplyr)
data_mean <- heart %>%
  group_by(age, chol) %>%
  summarise(mean_blood_pressure = mean(trestbps))

library(ggplot2)
ggplot(data_mean, aes(x = age, y = mean_blood_pressure, fill = factor(chol))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Correlation between Cholesterol and Blood presure based on Age",
       x = "Age",
       y = "Mean Blood Pressure",
       fill = "Cholesterol Level") +
  theme_minimal()

# Plot comparison between male and female blood pressure based on age
library(ggplot2)

ggplot(heart, aes(x = age, y = trestbps, color = sex)) +
  geom_point() +  
  labs(title = "Comparison of Blood Pressure between Male and Female based on Age",
       x = "Age",
       y = "Blood Pressure",
       color = "Sex") +
  theme_minimal()

# Load the required libraries
library(readr)
library(dplyr)
library(ggplot2)

# Plot heart disease frequency by age group and sex
ggplot(heart_freq, aes(x = age_group, y = count, fill = target)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~sex, scales = "free", nrow = 1) +
  labs(title = "Heart Disease Frequency by Age Group and Sex",
       x = "Age Group",
       y = "Frequency",
       fill = "Heart Disease") +
  scale_x_discrete(labels = function(x) gsub("\\,", " -", x)) +
  theme_minimal()

# Load the required libraries
library(readr)
library(dplyr)
library(ggplot2)

# Create distribution plots for resting ecg and resting blood pressure based on age
ggplot(heart, aes(x = age, y = trestbps)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_density_2d() +
  labs(title = "Distribution of Resting Blood Pressure by Age",
       x = "Age",
       y = "Resting Blood Pressure") +
  theme_minimal()

ggplot(heart, aes(x = age, fill = factor(restecg))) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7) +
  labs(title = "Distribution of Resting ECG by Age",
       x = "Age",
       y = "Count",
       fill = "Resting ECG") +
  theme_minimal()

# Load the required libraries
library(readr)
library(ggplot2)

# Fit logistic regression model
model <- glm(target ~ age + sex + chol + thalach, data = heart, family = binomial)

# Predict probabilities
heart$predicted_prob <- predict(model, type = "response")

# Plot logistic regression curve
ggplot(heart, aes(x = age, y = predicted_prob, color = sex)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Logistic Regression Plot",
       x = "Age",
       y = "Predicted Probability",
       color = "Sex") +
  theme_minimal()

# Load the required libraries
library(readr)
library(ggplot2)

# Convert 'sex' to factor
heart$sex <- factor(heart$sex)

# Fit logistic regression model
model <- glm(target ~ age + sex + chol + thalach, data = heart, family = binomial)

# Create a data frame for plotting
plot_data <- expand.grid(age = seq(min(heart$age), max(heart$age), length.out = 100),
                         sex = levels(heart$sex),
                         chol = mean(heart$chol),
                         thalach = mean(heart$thalach))

# Predict probabilities using the model
plot_data$predicted_prob <- predict(model, newdata = plot_data, type = "response")
# Plot logistic regression curve
ggplot(plot_data, aes(x = age, y = predicted_prob, color = sex)) +
  geom_line() +
  labs(title = "Logistic Regression Plot",
       x = "Age",
       y = "Predicted Probability",
       color = "Sex") +
  theme_minimal()



