# Generate x values
x_vals <- seq(0, 40, length.out = 500)

# Compute the chi-squared density with 19 degrees of freedom
y_vals <- dchisq(x_vals, df = df_diff)

# Create a data frame for plotting
chi_sq_data <- data.frame(x = x_vals, density = y_vals)

# Plot the chi-squared distribution
ggplot(chi_sq_data, aes(x = x, y = density)) +
  geom_line(color = "blue", size = 1) +
  theme_minimal() +
  labs(
    title = "Goodness-of-Fit BIC Reduced Model",
    x = "Chi-squared Value",
    y = "Density"
  ) +
  annotate(
    "text",
    x = 35,
    y = 0.01,  # Adjust label height
    label = paste("Chi-sq =", round(chisq_stat, 2), "\np =", signif(p_value, 3)),
    color = "red",
    hjust = 0.5
  ) +
  annotate(
    "segment", 
    x = 37, xend = 40,  # Starting and ending x-coordinates for the arrow
    y = 0.0005, yend = 0.00005,    # Starting and ending y-coordinates for the arrow
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),  # Arrow settings
    color = "red",       # Arrow color
    size = 1             # Arrow thickness
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )