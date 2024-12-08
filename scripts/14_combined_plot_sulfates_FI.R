# Combine plots with cowplot
combined_plot_sulfates_FI <- plot_grid(
  importance_plot, sulfates_plot, 
  labels = c("A", "B"), # Labels for the plots
  nrow = 1,             # Correct argument: nrow (not nrows)
  label_size = 14       # Size of the labels
)

png("figures/combined_plot_sulfates_FI.png", width = 8, height = 6, units = "in", res = 1000)

#tiff("figures/combined_plot_sulfates_FI.tiff", width = 8, height = 6, units = "in", res = 1000)
print(combined_plot_sulfates_FI)
dev.off()
