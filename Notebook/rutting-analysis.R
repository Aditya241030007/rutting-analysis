# Pavement Rutting Progression Analysis

# Step 1: Simulate dataset
set.seed(42)
traffic_load <- seq(1000, 10000, length.out = 30) # Traffic load in ESALs
rut_depth <- 2 + 0.0004 * traffic_load + rnorm(length(traffic_load), mean = 0, sd = 0.5)

# Combine into data frame
df <- data.frame(Traffic_Load_ESAL = traffic_load, Rut_Depth_mm = rut_depth)

# Step 2: Fit Linear Regression
model <- lm(Rut_Depth_mm ~ Traffic_Load_ESAL, data = df)

# Step 3: Predictions
df$Predicted_Rut <- predict(model, newdata = df)

# Step 4: Plot Results
plot(df$Traffic_Load_ESAL, df$Rut_Depth_mm, 
     main = "Pavement Rutting Progression",
     xlab = "Traffic Load (ESALs)", ylab = "Rut Depth (mm)",
     pch = 19, col = "blue")
lines(df$Traffic_Load_ESAL, df$Predicted_Rut, col = "red", lwd = 2)
legend("topleft", legend = c("Observed Data", "Regression Line"), 
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1))

# Step 5: Show Regression Equation
summary(model)

# Step 6: Save Results
write.csv(df, "simulated_rut_data.csv", row.names = FALSE)
png("rutting_plot.png")
plot(df$Traffic_Load_ESAL, df$Rut_Depth_mm, 
     main = "Pavement Rutting Progression",
     xlab = "Traffic Load (ESALs)", ylab = "Rut Depth (mm)",
     pch = 19, col = "blue")
lines(df$Traffic_Load_ESAL, df$Predicted_Rut, col = "red", lwd = 2)
dev.off()

