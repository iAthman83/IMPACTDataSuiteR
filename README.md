# IMPACTDataSuiteR

A package designed for humanitarian needs and survey data workflows, from data‑checking and cleaning to analysis, tailored for datasets (e.g., KOBO) and for use in organisations such as Impact Initiatives.

## Features

- Read and validate survey and choices sheets (e.g., from KOBO) for constraints, invalid values, duplicates etc.   
- Easy workflow to “initialize” environment, load tools, perform checks, and proceed to analysis.  
- Built with production‑use in mind (humanitarian data contexts), but flexible for other datasets.

## Installation

You can install the development version from GitHub using `devtools`:

```r
# Install devtools if you haven’t already
install.packages("devtools")

# Then install the package from GitHub
devtools::install_github("iAthman83/IMPACTDataSuiteR")

# Then load the package
library(IMPACTDataSuiteR)