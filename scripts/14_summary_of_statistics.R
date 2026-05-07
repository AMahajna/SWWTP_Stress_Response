# convert to long format
# convert to long format
df_long <- process_data |>
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

# remove zeros (important for log scale)
df_long <- df_long[df_long$value > 0, ]

#Distribution of Variables (Log-transformed)
ggplot(df_long, aes(x = variable, y = log10(value), fill = variable)) +
  geom_violin(trim = FALSE, alpha = 0.6, color = "black", linewidth = 0.2) +
  geom_jitter(width = 0.15, size = 0.5, alpha = 0.6) +
  theme_classic(base_size = 14) +
  labs(
    x = NULL,
    y = "log10(Value)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
