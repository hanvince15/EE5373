library(ggplot2)

# Load the data from "crab.txt"
data <- read.table("C:/Users/1hanv/OneDrive/Documents/EE5373/Lab2/crab.txt", header = TRUE)

# Plotting two variables using ggplot
ggplot(data, aes(x = width, y = weight)) +
  geom_point() +
  labs(x = "Width", y = "Weight", title = "Scatter Plot of Width vs. Weight") +
  theme_minimal()
