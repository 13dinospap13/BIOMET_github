# Thermos

Thermos is a private `R` package for bioclimatic preprocessing and thermal
comfort simulations.

It is designed so the scientific workflow lives in reusable functions, while
the GUI acts only as an interface for selecting inputs and launching the
workflow.

## Package structure

```text
Thermos/
|-- DESCRIPTION
|-- LICENSE
|-- NAMESPACE
|-- R/
|   |-- thermos-rasterize.R
|   |-- thermos-svf.R
|   |-- thermos-thermal.R
|   |-- thermos-validation.R
|   |-- thermos-utils.R
|   `-- thermos-gui.R
|-- inst/
|   |-- examples/
|   `-- extdata/
`-- docs/
```

## Main exported functions

- `thermos_rasterize_landcover()`
- `thermos_calculate_svf()`
- `thermos_thermal_comfort()`
- `thermos_check_inputs()`
- `thermos_run_pipeline()`
- `thermos_gui()`

## Workflow

1. Rasterize land-cover and obstacle attributes to the DEM grid.
2. Compute Sky View Factor from DEM and DSM.
3. Compute thermal-comfort outputs such as `Tmrt`, `PET`, `mPET`, `PMV`,
   `SET`, `UTCI`, and `UTCI class`.

The GUI provides two analysis modes:

- **Full pipeline** runs rasterization, SVF, and thermal analysis.
- **Thermal from existing rasters** reuses existing land-cover and SVF folders
  and runs only the thermal stage.

Existing rasters must use matching plot suffixes. For example, `plot01`
requires `svf_plot01.tif` and land-cover files such as
`albedo_plot01.tif`, `emis_plot01.tif`, and the remaining generated layers.
For multiple plots, the GUI can run one detected suffix or all complete plots.

## Data policy

Sample and test datasets are intentionally not included in this repository.

Users must provide their own:

- land-cover vector file
- obstacles vector file
- DEM raster folder
- DSM raster folder
- meteorological Excel file

See [inst/extdata/README.md](inst/extdata/README.md) and
[docs/input_requirements.md](docs/input_requirements.md) for the expected input
structure.

## Documentation

- [Thermos manual](docs/Thermos_manual.md)
- [Input requirements](docs/input_requirements.md)
- [Install and run](docs/install_and_run.md)

## Installation

### Option 1: install from GitHub

```r
install.packages("remotes")
remotes::install_github("your-org-or-username/Thermos", dependencies = TRUE)
```

### Option 2: install from a local folder

```r
install.packages("remotes")
remotes::install_local("path/to/Thermos", dependencies = TRUE)
```

### Quick local install helper

If the user has downloaded or cloned the repository, they can also run:

```r
source("install.R")
```

## Launch GUI

After installation:

```r
library(Thermos)
thermos_gui()
```

Or from the repository folder:

```r
source("launch_gui.R")
```

## Example usage in R

```r
library(Thermos)

checks <- thermos_check_inputs(
  lc_path = "path/to/landcover.gpkg",
  obs_path = "path/to/obstacles.gpkg",
  dem_dir = "path/to/dem_folder",
  dsm_dir = "path/to/dsm_folder",
  met_xlsx = "path/to/met_inputs.xlsx"
)

print(checks)
```
