source("https://raw.githubusercontent.com/igormanojlovic/TimeSeriesR/main/TimeSeriesR.R")
# Download and extract one of the datasets from the link provided in the README file at github.

folder = "path to dataset folder"

# Example 1: Run Time Series Grouping Algorithm (TSGA)
TSF(folder, "TSGA")
# Check results under folder/Test/TSGA

# Example 2: Run Deep Centroid Learning (DCL)
TSF(folder, c("Forecast", "Accuracy"), "DCL") 
# Check results under folder/Test/DCL