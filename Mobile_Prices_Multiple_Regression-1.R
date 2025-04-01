library(readxl)

# Load the dataset
data = read_excel("C:/Users/shreyash/OneDrive/Desktop/Projects/mobile prices/updated_mobile_prices - Copy.xlsx")

# Define independent and dependent variables
x1 = data$Price  # Dependent variable
x2 = data$RAM    # Independent variable 1
x3 = data$Battery  # Independent variable 2

# Check lengths
n1 = length(x1)
n2 = length(x2)
n3 = length(x3)

# Build multiple regression model
mur = lm(x1 ~ x2 + x3)

# Extract coefficients
cr = coefficients(mur)
mcr = matrix(cr)
a = mcr[1,1]  # Intercept
b = mcr[2,1]  # Coefficient for RAM
c = mcr[3,1]  # Coefficient for Battery

# Predicted values
x1est = fitted(mur)
meanx1 = mean(x1)

# Sum of squared errors
sum1 = sum((x1 - x1est)^2)
sum2 = sum((x1 - meanx1)^2)

# Mean residual sum of squares
mrss = sum1 / n1

# Residual standard deviation
residual_sd = sqrt(mrss)

# Create dataframe with actual and estimated prices
d1 = data.frame("Actual Price" = x1, "RAM" = x2, "Battery" = x3, "Estimated Price" = x1est)

# Coefficient of determination (R²)
cod = summary(mur)$r.squared

# Adjusted R²
adj_r2 = summary(mur)$adj.r.squared

# Correlation coefficients
r12 = cor(x1, x2)
r13 = cor(x1, x3)
r23 = cor(x2, x3)

# Multiple correlation coefficient
ne1 = r12^2 + r13^2 - 2 * r12 * r13 * r23
de1 = 1 - r23^2
R1.23 = sqrt(ne1 / de1)

# Rename columns
colnames(d1) = c("Actual_Price", "RAM", "Battery", "Estimated_Price")

# Scatter plot for RAM vs. Price
plot(x2, x1, main = "Price vs. RAM", xlab = "RAM", ylab = "Price", col = "blue", pch = 19)
abline(lm(x1 ~ x2), col = "red", lwd = 2)

# Residuals vs. Fitted Plot
plot(fitted(mur), residuals(mur), main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals", col = "purple", pch = 19)
abline(h = 0, col = "black", lwd = 2)

# Histogram of residuals
hist(residuals(mur), main = "Histogram of Residuals", xlab = "Residuals", col = "orange", breaks = 20)

# Model summary
summary(mur)

# Model Evaluation
cat("\n================ Model Evaluation ================\n")
cat("Multiple Regression Model: Price on RAM and Battery:\n")
cat("Price =", a, "+", b, "* RAM +", c, "* Battery\n")
cat("Mean Price:", meanx1, "\n")
cat("Sum of Squared Errors (SSE):", sum1, "\n")
cat("Total Sum of Squares (TSS):", sum2, "\n")
cat("Mean Residual Sum of Squares (MRSS):", mrss, "\n")
cat("Residual Standard Deviation:", residual_sd, "\n")
cat("Coefficient of Determination (R²):", cod, "\n")
cat("Adjusted R²:", adj_r2, "\n")
cat("Multiple Correlation Coefficient R1.23:", R1.23, "\n")

# Display if the model fit is poor
if (cod < 0.5) {
  cat("In given dataset cod value is 0.1042793, that does not vary or match with our data, so R^2 value is less than 0.5,it indicates RAM and Battery does not explain variation in price effectively\n")
} else {
  cat("\nConclusion: The model is a good fit with a reasonable R² value.\n")
}

