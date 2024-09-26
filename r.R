# Load necessary libraries
library(ggplot2)

# Load the Longley dataset
data("longley")
longley_data <- longley

# Plot Employed against each variable
par(mfrow = c(3, 2))
for (var in names(longley_data)[-which(names(longley_data) == "Employed")]) {
  plot(longley_data[[var]], longley_data$Employed,
       main = paste("Employed vs", var),
       xlab = var, ylab = "Employed", pch = 19, col = "blue")
}

# Calculate correlation
correlation_matrix <- cor(longley_data)
cor_employed <- correlation_matrix["Employed",]
cor_employed <- sort(cor_employed, decreasing = TRUE)

# Select the 3 variables most correlated with Employed
most_correlated_vars <- names(cor_employed)[2:4]

# Create regression models for selected variables
models <- list()
for (var in most_correlated_vars) {
  formula <- as.formula(paste("Employed ~", var))
  models[[var]] <- lm(formula, data = longley_data)
}

# Compare models based on adjusted R-squared
adjusted_r_squared <- sapply(models, function(m) summary(m)$adj.r.squared)
best_model_name <- names(which.max(adjusted_r_squared))
best_model <- models[[best_model_name]]

# Create model matrices
model_matrix <- model.matrix(best_model)

# Calculate regression parameters from model matrices
beta <- solve(t(model_matrix) %*% model_matrix) %*% t(model_matrix) %*% longley_data$Employed

# Calculate predicted values
predicted_values <- model_matrix %*% beta

# Combine actual and predicted values in a data frame
results <- data.frame(Actual = longley_data$Employed, Predicted = predicted_values)

# Print results
head(results)
