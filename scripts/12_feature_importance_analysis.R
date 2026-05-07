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
set.seed(999)

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

residuals_df <- data.frame(
  Predicted = predictions,
  Residuals = residuals
)

residuals_plot <- ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Residuals for Model Prediction",
       x = "Residuals", 
       y = "Frequency") +
  theme_minimal()


ggplot(residuals_df, aes(x = Predicted, y = Residuals)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  theme_minimal()


# Save the residuals plot
png(filename="figures/residuals_plot.png", units = 'in', width = 9, height = 6, res = 1000)
print(residuals_plot)
dev.off()

# Optionally, print the RMSE, MAE, and R² values
cat("RMSE:", rmse, "\n")
cat("MAE:", mae$value, "\n")
cat("R-squared:", rsquared, "\n")



###############################################################################
# Step 4: Create Residuals plot
residuals <- actuals - predictions

residuals_params <- data.frame(
  x = 1:length(residuals(rf_model)),
  y = residuals(rf_model)
)

residuals_df <- data.frame(
  Predicted = predictions,
  Residuals = residuals
)

tiff("Figures/residuals_diagnostics.tiff",
    width = 2000,
    height = 2400,
    res = 300)


layout(matrix(c(1,2,
                3,4,
                5,5), nrow = 3, byrow = TRUE))

par(mar = c(4, 4, 2, 1),
    oma = c(1, 1, 1, 1),
    cex.main = 0.9,
    cex.lab = 1.0)

# A
plot(residuals_params$x, residuals_params$y, type = "l",
     main = "Residuals over time",
     xlab = "Time (ordered observations)",
     ylab = "Residuals")
abline(h = 0, col = "red")
mtext("A", side = 3, line = 0.5, adj = 0, font = 2)

# B
plot(residuals_df$Predicted, residuals_df$Residuals,
     main = "Residuals vs Predicted",
     xlab = "Predicted values",
     ylab = "Residuals")
abline(h = 0, col = "red")
mtext("B", side = 3, line = 0.5, adj = 0, font = 2)

# C
hist(residuals_df$Residuals,
     main = "Residuals distribution",
     xlab = "Residuals",
     ylab = "Frequency")
mtext("C", side = 3, line = 0.5, adj = 0, font = 2)

# D
qqnorm(residuals_df$Residuals,
       main = "Normal Q-Q plot",
       xlab = "Theoretical quantiles",
       ylab = "Sample quantiles")
qqline(residuals_df$Residuals, col = "red")
mtext("D", side = 3, line = 0.5, adj = 0, font = 2)

# E (ACF)
par(mar = c(4, 4, 3, 1))  # extra top space for title
acf(residuals_df$Residuals,
    main = "Autocorrelation function (ACF)",
    xlab = "Lag",
    ylab = "Autocorrelation")
mtext("E", side = 3, line = 0.5, adj = 0, font = 2)

dev.off()

###############################################################################
# Stress Response 
selected_stress <- rowData(tse_pathway)$Kingdom %in% c("Stress Response") &
  !is.na(rowData(tse_pathway)$Kingdom)
tse_stress <- tse_pathway[selected_stress, ]


tse_func_subcat = agglomerateByRank(tse_stress, rank = "Phylum")

df_env_func_subcat = cbind(as.data.frame(colData(tse_func_subcat))[ ,10:22],t(as.data.frame(assay(tse_func_subcat, "relabundance"))))


# ---------------------------
# Data setup
# ---------------------------
X <- df_env_func_subcat[, 1:13]
response_vars <- colnames(df_env_func_subcat)[14:ncol(df_env_func_subcat)]

# ---------------------------
# Storage objects
# ---------------------------
importance_list <- list()
results_list <- list()
plot_list <- list()

# ---------------------------
# Loop over response variables
# ---------------------------
for (i in seq_along(response_vars)) {
  
  # Set a different seed for each response variable
  set.seed(102020023 + i)
  
  y_name <- response_vars[i]
  y <- df_env_func_subcat[[y_name]]
  
  data_combined <- cbind(X, y)
  
  # Random Forest model (LOOCV)
  rf_model <- train(
    y ~ ., 
    data = data_combined,
    method = "rf",
    trControl = control_LOOCV,
    importance = TRUE
  )
  
  # ---------------------------
  # Feature Importance
  # ---------------------------
  importance <- varImp(rf_model, scale = TRUE)
  
  imp_df <- importance$importance
  imp_df$Variable <- rownames(imp_df)
  imp_df$Response <- y_name
  
  importance_list[[i]] <- imp_df
  
  # ---------------------------
  # Performance metrics
  # ---------------------------
  pred <- rf_model$pred$pred
  obs  <- rf_model$pred$obs
  
  r2   <- R2(pred, obs)
  rmse <- RMSE(pred, obs)
  mae  <- MAE(pred, obs)
  
  results_list[[i]] <- data.frame(
    Response = y_name,
    R2 = r2,
    RMSE = rmse,
    MAE = mae
  )
  
  # ---------------------------
  # Observed vs Predicted plot
  # ---------------------------
  p <- ggplot(data.frame(obs, pred), aes(x = obs, y = pred)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(
      title = y_name,
      x = "Observed",
      y = "Predicted"
    )
  
  plot_list[[i]] <- p
}

# ---------------------------
# 1. PERFORMANCE TABLE
# ---------------------------
performance_df <- bind_rows(results_list)

print(performance_df)

# Save table
write.csv(performance_df, 
          "output_data/model_performance.csv", 
          row.names = FALSE)

write_xlsx(performance_df,
           path = "output_data/model_performance.xlsx")
# ---------------------------
# 2. HEATMAP OF FEATURE IMPORTANCE
# ---------------------------
importance_all <- bind_rows(importance_list)

# Rename importance column (caret dependent)
colnames(importance_all)[1] <- "Importance"

p_heatmap <- ggplot(importance_all, 
                    aes(x = Response, 
                        y = Variable, 
                        fill = Importance)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Feature Importance Across Response Variables",
    x = "Response Variables",
    y = "Predictors"
  )

# Print heatmap
print(p_heatmap)

# Save heatmap
ggsave("Figures/Feature_Importance_Heatmap.tiff",
       plot = p_heatmap,
       width = 10,
       height = 8,
       dpi = 300)

# ---------------------------
# 3. OBSERVED vs PREDICTED FIGURE
# ---------------------------
png("Figures/Observed_vs_Predicted.tiff",
    width = 3000,
    height = 2000,
    res = 300)

grid.arrange(grobs = plot_list, ncol = 3)

dev.off()
###############
