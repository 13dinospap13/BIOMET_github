# Input Requirements

## 1. Land-cover vector

Expected attributes:

- `lc_class`
- `albedo`
- `emissivity`
- `z0`
- `et_scale`

## 2. Obstacles vector

Expected attributes:

- `obs_type`
- `lai`
- `canopy_cover`
- `k_ext`
- `wall_albedo`
- `wall_emissivity`

Common vegetation values expected by the default workflow:

- `tree`
- `shrub`
- `hedge`

Building features are expected to use:

- `building`

## 3. DEM folder

Expected contents:

- one or more `.tif` raster files used as the base analysis grid

## 4. DSM folder

Expected contents:

- one or more `.tif` raster files matched to the DEM files by filename suffix

## 5. Meteorological Excel

Expected columns:

- `date`
- `hour_utc`
- `Ta`
- `Td`
- `u10`
- `v10`
- `ssrd`
- `strd`
- `slhf`

## 6. Output folders

The workflow writes outputs to user-provided directories such as:

- rasterized land-cover outputs
- SVF outputs
- thermal-comfort outputs

## Reusing existing intermediate rasters

Select **Thermal from existing rasters** in the GUI to skip land-cover
rasterization and SVF calculation.

Choose:

- a folder containing the rasterized land-cover layers
- a folder containing the SVF rasters
- the matching DEM and DSM folders
- the meteorological Excel file
- a parent output folder for the new thermal results

For every plot suffix, the SVF folder must contain:

```text
svf_<plot>.tif
```

The rasterized land-cover folder must contain:

```text
landcover_<plot>.tif
albedo_<plot>.tif
emis_<plot>.tif
z0_<plot>.tif
et_scale_<plot>.tif
lai_<plot>.tif
canopy_cover_<plot>.tif
k_ext_<plot>.tif
gai_<plot>.tif
wall_albedo_<plot>.tif
wall_emis_<plot>.tif
```

All files belonging to one plot must use the same suffix, such as `plot01`.
Land-cover rasters must be aligned with the matching DEM. DSM and SVF rasters
may use a different resolution when they share the DEM coordinate system and
spatially overlap it; the thermal stage resamples them to the DEM grid.

When several complete plot suffixes are available, select an individual plot
or **All detected plots**. Missing or incompatible files stop the analysis
before processing and are reported in the GUI status area.
