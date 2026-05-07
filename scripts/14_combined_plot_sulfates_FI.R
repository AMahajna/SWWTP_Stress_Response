combined_plot <- plot_grid(
  importance_plot,pred_vs_actual_plot ,  # Top row: Feature importance and sulfates plot
  sulfates_plot, stress_response_time_series,            # Bottom row: Prediction vs Actual plot
  labels = c("A", "B", "C", "D"),      # Labels for the plots
  nrow = 2,                       # Arrange the plots in 2 rows
  rel_heights = c(1, 0.8),        # Adjust relative heights of the rows
  label_size = 10                 # Size of the labels
)

png("figures/combined_plot_sulfates_FI.png", width = 8, height = 6, units = "in", res = 1000)

#tiff("figures/combined_plot_sulfates_FI.tiff", width = 8, height = 6, units = "in", res = 1000)
print(combined_plot)
dev.off()

