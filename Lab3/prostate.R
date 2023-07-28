# install.packages("genridge")

# Load the required package
library(genridge)

# Load the 'prostate' dataset
data(prostate)

# Set the input and output variables
input_var <- prostate$lcavol
output_var <- prostate$lpsa

# Fit the linear regression model using genridge()
model <- genridge(output_var, input_var)

# Print the model summary
summary(model)
