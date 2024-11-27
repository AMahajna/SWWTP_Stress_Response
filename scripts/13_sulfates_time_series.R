#Load Data
df =  read_excel("input_data/ZAWZI_INF_lab_date.xlsx")
sulfates =  na.omit(df[ ,"INF_SO4_µg_per_l"])

# Get the maximum value from the 'Concentration' column
max_value <- max(sulfates)

# Get the minimum value from the 'Concentration' column
min_value <- min(sulfates)

print(summary(df$INF_SO4_µg_per_l))

# Print results
cat("Max Sulfates in µg_per_l :", max_value, "\n")
cat("Min Sulfates in µg_per_l:", min_value, "\n")
################################################################################

# Apply this function to the 'Date' column
df$Date_updated <- as.Date(df$Date_updated)
df$sulfate_load_mg_per_l = df[ ,"INF_SO4_µg_per_l"]/1000
# Extract numeric values from the embedded data frame
df$sulfate_load_mg_per_l <- df$sulfate_load_mg_per_l$INF_SO4_µg_per_l

# Check the structure
str(df)


df_cleaned = df[ ,c("Date_updated","sulfate_load_mg_per_l" )]
df_cleaned <- df_cleaned %>% drop_na(sulfate_load_mg_per_l)
df_cleaned = as.data.frame(df_cleaned)


# Create the interactive plot
plot <- plot_ly(df_cleaned, 
                x = ~Date_updated, 
                y = ~sulfate_load_mg_per_l, 
                type = 'scatter', 
                mode = 'lines+markers',
                marker = list(color = 'blue', size = 8),
                line = list(color = 'darkblue', width = 2)) %>%
  layout(
         xaxis = list(title = "Date"),
         yaxis = list(title = "Sulfate Load [mg/L]"),
         template = "plotly_white")


print(plot)


# Create a time series plot
sulfates_plot = ggplot(df_cleaned, aes(x = Date_updated, y = sulfate_load_mg_per_l)) +
  geom_line(color = "steelblue", size = 1.2) +  # Line with custom color and thickness
  geom_point(color = "darkorange", size = 3) +  # Points with custom color and size
  labs(
    x = "Date",
    y = "Sulfate Load (mg/L)"
  ) +
  theme_minimal(base_size = 14) +  # Minimal theme with larger base font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centered, bold title
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold")
  )


png(filename="figures/sulfates_time_series.png" ,units = 'in',width=9, height=6, res=1000)
print(sulfates_plot)
dev.off()
