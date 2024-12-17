## Loading libraries

inst_pkgs <- rownames(installed.packages())

# Install pak package manager (allows to install directly from
# CRAN or github):
if(!("pak" %in% inst_pkgs)) install.packages("pak")

pkg_list <-
  list(
    "tidyverse",               # Basic data manipulation and visualization
    "magrittr",
    "lubridate",               # Basic dates manipulation
    "readxl",                  # Export xlsx data
    "tseries",                 # For complex time series functions
    "tseriesChaos",            # For complex non-linear time series functions
    "mvgam",                   # Multivariate GAM models for community ecology
    "rEDM",                    # Empirical Dynamic Modelling
    "multispatialCCM",         # Clark's package for causal inference
    "pttstability",            # Clark's package for filtering 
    "adespatial",              # For Legendre's beta-diversity
    "ecopart",                 # Tatsumi's beta-diversity
    "comstab",                 # Segrestin's synchrony
    "mFD",                     # Villeger's Multi Dimensional Functional Diversity
    "ggpmisc",     # Basic plotting options
    "scales",      # Basic plotting options
    "vegan"        # General vegetation ecology
  )

invisible(
  lapply(
  pkg_list,
  function(x) if(!(x %in% inst_pkgs)) pak::pkg_install(x)
  )
)

invisible(
  lapply(
    pkg_list,
    library, character.only = TRUE
  )
)

rm(list = ls())

print("All packages installed and loaded successfully!")