# canopyexplorer  
The Canopy Project is a collaborative national effort involving hundreds of organizations and schools that identifies innovative learning environments and their impact. This package provides helper functions for cleaning, analyzing, and visualizing Canopy data.  

You can install the development version from GitHub:  

```r
# install.packages("devtools")  
devtools::install_github("jdavelar/canopyexplorer")  
library(canopyexplorer)  

# Example use case  

# Step 1: Load Canopy data for year of interest
raw_data <- load_canopy_data("2024")

# Step 2: Prepare the data for modeling  
cleaned_data <- tag_bayes_prep(  
  data = raw_data,   
  prefix = "practices_",   
  id = "school_id",  
  covariates = c("leader_race", "pct_bipoc")  
)  

# Step 3: Run Bayesian models  
results <- tag_bayes(  
  data = cleaned_data,  
  covariates = c("leader_race", "pct_bipoc"),  
  exponentiate = TRUE  
)  

# Step 4: Plot the results  
plot <- tag_bayes_plot(  
  data = results,  
  n = 10,  
  theme = "crpe"  
)  
print(plot)
```

If you have suggestions or find any issues, please open an issue or submit a pull request!

This package is licensed under the MIT License.
