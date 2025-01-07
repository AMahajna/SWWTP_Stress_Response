# Load the global variable 
mae <- readRDS("mae.rds")
tse = mae[[1]]
tse_bacteria = mae[[2]]
tse_active = mae[[3]]
tse_pathway = mae[[4]]
tse_enzyme = mae[[5]]
tse_gene = mae[[6]]

################################################################################
#All subsystems classes of stress 
# Stress Response 
#selected_stress <- rowData(tse_pathway)$Kingdom %in% c("Stress Response") &
#  !is.na(rowData(tse_pathway)$Kingdom)
#tse_stress <- tse_pathway[selected_stress, ]

#tse_stress = agglomerateByRank(tse_stress, rank = "Phylum")

#tse_stress = transformAssay(
#  x = tse_stress, assay.type = "relabundance", method = "clr", pseudocount = TRUE,
#  name = "clr")

#environmental data is standardized and abundance data is CLR transformed
#df_env_stress = cbind(as.data.frame(colData(tse_stress))[ ,10:22],t(as.data.frame(assay(tse_stress, "relabundance"))))
################################################################################

tse_func_cat = agglomerateByRank(tse_pathway, rank = "Kingdom")
tse_func_cat = transformAssay(
  x = tse_func_cat, assay.type = "relabundance", method = "clr", pseudocount = TRUE,
  name = "clr")

df_env_func_cat = cbind(as.data.frame(colData(tse_func_cat))[ ,10:22],t(as.data.frame(assay(tse_func_cat, "relabundance"))))

################################################################################
set.seed(102020023)

control_LOOCV = trainControl( method="LOOCV", returnResamp = 'all')
#control_CV = trainControl( method = "cv", number = 5)

y <- df_env_func_cat[ ,"Stress Response"]
X = df_env_func_cat[ ,1:13]
data_combined = cbind(X,y)

rf_model <- train(
  y ~ ., data = data_combined,
  method = "rf",
  trControl = control_LOOCV,
  importance = TRUE
)

# Get feature importance
importance <- varImp(rf_model, scale = TRUE)
print(importance)

importance_plot = plot(importance)
# Plot feature importance
png(filename="figures/feature_importance_rf_loocv_relabundance_stress_response.png" ,units = 'in',width=9, height=6, res=1000)
plot(importance_plot)
dev.off()





# Print all available metrics from the results
model_results <- rf_model$results
print(model_results)

# Extract and plot other metrics, such as MAE or R-squared (if applicable)
mae_values <- model_results$MAE
rsq_values <- model_results$Rsquared

# Plot MAE and R-squared
mae_plot <- ggplot(model_results, aes(x = 1:nrow(model_results), y = MAE)) +
  geom_line() + 
  geom_point() + 
  labs(title = "MAE for Model Prediction",
       x = "Fold Number (LOOCV)", 
       y = "MAE") +
  theme_minimal()

rsq_plot <- ggplot(model_results, aes(x = 1:nrow(model_results), y = Rsquared)) +
  geom_line() + 
  geom_point() + 
  labs(title = "R-squared for Model Prediction",
       x = "Fold Number (LOOCV)", 
       y = "R-squared") +
  theme_minimal()

# Save the plots
png(filename="figures/mae_plot_rf_loocv_stress_response.png", units = 'in', width = 9, height = 6, res = 1000)
print(mae_plot)
dev.off()

png(filename="figures/rsq_plot_rf_loocv_stress_response.png", units = 'in', width = 9, height = 6, res = 1000)
print(rsq_plot)
dev.off()
###############################################################################

# Step 1: Get the predicted values from the random forest model
predictions <- predict(rf_model, newdata = data_combined)

# Actual values
actuals <- y

# Step 2: Calculate RMSE and MAE for the prediction accuracy
rmse <- sqrt(mean((predictions - actuals)^2))  # Root Mean Squared Error
mae <- mean(abs(predictions - actuals))        # Mean Absolute Error

# R-squared (R²) for evaluating model fit
rsquared <- 1 - sum((predictions - actuals)^2) / sum((actuals - mean(actuals))^2)

# Step 3: Create Actual vs. Predicted plot
pred_vs_actual_plot <- ggplot(data.frame(Actual = actuals, Predicted = predictions), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
       x = "Actual Values", 
       y = "Predicted Values") +
  theme_minimal()

pred_vs_actual_plot_single <- ggplot(data.frame(Actual = actuals, Predicted = predictions), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = paste("Prediction vs Actual (RMSE:", round(rmse, 2), ", MAE:", round(mae, 2), ", R²:", round(rsquared, 2), ")"),
       x = "Actual Values", 
       y = "Predicted Values") +
  theme_minimal()

# Save the plot as PNG
png(filename="figures/prediction_accuracy_plot.png", units = 'in', width = 9, height = 6, res = 1000)
print(pred_vs_actual_plot)
dev.off()

# Step 4: Create Residuals plot
residuals <- actuals - predictions

residuals_plot <- ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Residuals for Model Prediction",
       x = "Residuals", 
       y = "Frequency") +
  theme_minimal()

# Save the residuals plot
png(filename="figures/residuals_plot.png", units = 'in', width = 9, height = 6, res = 1000)
print(residuals_plot)
dev.off()

# Optionally, print the RMSE, MAE, and R² values
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")

