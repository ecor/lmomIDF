# Create example data
set.seed(123)
x <- c(rep(1, 5), rep(2, 7), rep(3, 10))
y <- x^2 + rnorm(length(x))

# Calculate confidence intervals for groups with sufficient observations
ci <- tapply(y, x, function(x) if (length(x) > 1) t.test(x)$conf.int else NA)

# Convert confidence intervals to a data frame
ci_df <- data.frame(
  x = as.numeric(names(ci)),
  ymin = sapply(ci, function(x) if (!is.na(x)) x[1] else NA),
  ymax = sapply(ci, function(x) if (!is.na(x)) x[2] else NA)
)

# Create plot with confidence intervals
ggplot() +
  geom_point(aes(x, y)) +
  geom_ribbon(data = na.omit(ci_df), aes(x = x, ymin = ymin, ymax = ymax), alpha = 0.3) +
  labs(x = "x", y = "y", title = "Plot with Confidence Intervals")