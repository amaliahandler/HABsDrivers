# Cyanobacteria and Microcystin Predictions for 125,000 Lakes in the United States

Project citation: Handler, A., Reynolds, M., Dumelle, M., Compton, J., Hill, R., Jansen, L., Sabo, R.,  Weber, M., Brehob, M., and Peninno, M.(2025). Spatial Modeling of Cyanobacteria Bloom Risk to Disentangle Nutrients and Other Drivers for 125,000 Lakes in the USA. In review.

For correspondance, please contact Amalia Handler [amhandle\@asu.edu](mailto:amhandle@asu.edu))

### Abstract

Cyanobacterial harmful algal blooms (cyanoHABs) are an environmental threat to freshwater ecosystems. Understanding which waterbodies are at-risk for blooms remains challenging. The goal of this study is to investigate how in-lake conditions, watershed nutrient inputs, geography, hydrology, and lake morphology explain patterns in cyanobacteria abundance and microcystin detection across the conterminous US. We used a statistical modeling approach incorporating spatial dependence (i.e., autocorrelation) and national survey datasets to generate predictions for 124,529 lakes. Our models performed well even without in-lake data: the spatial model for cyanobacteria abundance had a predictive R2 of 0.46 and the spatial microcystin detection model had an AUC of 0.87. These models explained more variation in cyanobacteria abundance and microcystin detection than regional and national scale studies to date. We found 62.2% of lakes are expected to reach a high level of cyanobacteria abundance above 100,000 cell/mL and 14.8% of lakes have at least a 50% probability of detecting microcystin at least once at the surface water near the deepest point in the lake during the summer. Higher nutrient inputs and concentrations, warmer and drier regions of the country, and shallow lakes with longer fetch are key drivers of higher cyanoHAB risk across the US. Lower mean annual precipitation, reduced baseflow relative to stormflow, and higher lake area relative to depth increased cyanoHAB risk even in lakes where nutrient inputs associated with farms and developed land cover were lower. This finding underscores the importance of characterizing other environmental factors and quantifying their effect on cyanoHABs. These spatially explicit predictions are a critical tool for environmental managers, facilitating prioritization of resources for monitoring, education, and mitigation.

### Package Overview

Run the following to install the supplementary R package

```{r}
install.packages("remotes") # if you don't already have remotes installed
remotes::install_github("amaliahandler/HABsDrivers")
```
### Data Availability

All compiled data for this manuscript are available and documented within the package. A glimpse of the data components and the help pages for each dataset can be viewed by running

```{r}
# Data used to fit the models
dplyr::glimpse(habs); ?habs

# Prediction dataset
dplyr::glimpse(pred); ?pred

```
### Analysis and Results

The code required to reproduce the results section of the paper including the figures and tables are available at the file path found by running

```{r}
system.file("inst/manuscript/HABsDrivers_Results.qmd", package = "HABsDrivers")
```

The code used to fit the models is available at the file path found by running

```{r}
system.file("inst/scripts/drivers_analysis.Rmd", package = "HABsDrivers")
```

### Disclaimer

The views expressed in this manuscript are those of the authors and do not necessarily represent the views or policies of the U.S. Environmental Protection Agency. Any mention of trade names, products, or services does not imply an endorsement by the U.S. government or the U.S. Environmental Protection Agency. The U.S. Environmental Protection Agency does not endorse any commercial products, services, or enterprises.


