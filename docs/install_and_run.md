# Install and Run

## Minimum user steps

1. Open `R` or `RStudio`
2. Set the working directory to the repository folder
3. Run:

```r
source("install.R")
source("launch_gui.R")
```

## Notes on dependencies

The package declares these `R` dependencies:

- `terra`
- `readxl`
- `lubridate`
- `solartime`
- `comf`
- `shiny`

In most Windows and macOS setups, `remotes::install_local(..., dependencies = TRUE)`
or `remotes::install_github(..., dependencies = TRUE)` should install them
automatically.

`terra` is the main heavy dependency and may be the hardest one to install on
some systems.

According to the current CRAN package page for `terra`, it needs compilation
and lists system requirements including `GDAL`, `GEOS`, `PROJ`, `TBB`, and
`sqlite3`, although CRAN provides Windows and macOS binaries for current R
releases. The current CRAN page for `solartime` shows it is available on CRAN
as well.

## If installation fails

Try installing the dependencies manually first:

```r
install.packages(c("terra", "readxl", "lubridate", "solartime", "shiny", "comf", "remotes"))
```

Then run:

```r
remotes::install_local(".", dependencies = FALSE)
```
