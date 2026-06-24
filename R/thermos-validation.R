#' Validate Thermos inputs
#'
#' Checks whether required files, folders, and expected vector fields exist.
#'
#' @param lc_path Path to land-cover GeoPackage.
#' @param obs_path Path to obstacles GeoPackage.
#' @param dem_dir Directory with DEM rasters.
#' @param dsm_dir Optional directory with DSM rasters.
#' @param svf_dir Optional directory with SVF rasters.
#' @param lc_dir Optional directory with rasterized land-cover outputs.
#' @param met_xlsx Optional meteorological Excel file.
#'
#' @return A list with `ok`, `messages`, and `details`.
#' @export
thermos_check_inputs <- function(lc_path = NULL,
                                obs_path = NULL,
                                dem_dir = NULL,
                                dsm_dir = NULL,
                                svf_dir = NULL,
                                lc_dir = NULL,
                                met_xlsx = NULL) {
  messages <- character()
  details <- list()
  ok <- TRUE

  add_message <- function(msg, is_ok = TRUE) {
    if (!is_ok) {
      ok <<- FALSE
    }
    messages <<- c(messages, msg)
  }

  if (!is.null(lc_path)) {
    if (!file.exists(lc_path)) {
      add_message(paste("Missing land-cover vector:", lc_path), FALSE)
    } else {
      lc_vec <- terra::vect(lc_path)
      required <- c("lc_class", "albedo", "emissivity", "z0", "et_scale")
      missing <- setdiff(required, names(lc_vec))
      details$landcover_fields <- names(lc_vec)
      if (length(missing) > 0) {
        add_message(
          paste("Land-cover vector is missing fields:", paste(missing, collapse = ", ")),
          FALSE
        )
      } else {
        add_message("Land-cover vector looks valid.")
      }
    }
  }

  if (!is.null(obs_path)) {
    if (!file.exists(obs_path)) {
      add_message(paste("Missing obstacles vector:", obs_path), FALSE)
    } else {
      obs_vec <- terra::vect(obs_path)
      required <- c(
        "obs_type", "lai", "canopy_cover", "k_ext",
        "wall_albedo", "wall_emissivity"
      )
      missing <- setdiff(required, names(obs_vec))
      details$obstacle_fields <- names(obs_vec)
      if (length(missing) > 0) {
        add_message(
          paste("Obstacles vector is missing fields:", paste(missing, collapse = ", ")),
          FALSE
        )
      } else {
        add_message("Obstacles vector looks valid.")
      }
    }
  }

  for (entry in list(
    list(path = dem_dir, label = "DEM", pattern = "\\.tif$"),
    list(path = dsm_dir, label = "DSM", pattern = "\\.tif$"),
    list(path = svf_dir, label = "SVF", pattern = "\\.tif$")
  )) {
    if (is.null(entry$path)) {
      next
    }
    if (!dir.exists(entry$path)) {
      add_message(paste(entry$label, "directory not found:", entry$path), FALSE)
      next
    }
    count <- length(list.files(entry$path, pattern = entry$pattern, full.names = TRUE))
    details[[paste0(tolower(entry$label), "_file_count")]] <- count
    if (count == 0) {
      add_message(paste("No", entry$label, "rasters found in:", entry$path), FALSE)
    } else {
      add_message(paste(entry$label, "directory contains", count, "raster(s)."))
    }
  }

  if (!is.null(lc_dir)) {
    required_layers <- c(
      "albedo", "emis", "z0", "et_scale", "gai",
      "k_ext", "wall_emis", "wall_albedo"
    )
    if (!dir.exists(lc_dir)) {
      add_message(paste("Rasterized land-cover directory not found:", lc_dir), FALSE)
    } else {
      available <- list.files(lc_dir, pattern = "\\.tif$", full.names = FALSE)
      details$lc_raster_files <- available
      missing_prefixes <- required_layers[
        !vapply(required_layers, function(prefix) {
          any(startsWith(available, paste0(prefix, "_")))
        }, logical(1))
      ]
      if (length(missing_prefixes) > 0) {
        add_message(
          paste(
            "Rasterized land-cover directory is missing expected layers:",
            paste(missing_prefixes, collapse = ", ")
          ),
          FALSE
        )
      } else {
        add_message("Rasterized land-cover directory looks valid.")
      }
    }
  }

  if (!is.null(met_xlsx)) {
    if (!file.exists(met_xlsx)) {
      add_message(paste("Meteorological Excel file not found:", met_xlsx), FALSE)
    } else {
      cols <- names(readxl::read_xlsx(met_xlsx, n_max = 0))
      required <- c("date", "hour_utc", "Ta", "Td", "u10", "v10", "ssrd", "strd", "slhf")
      missing <- setdiff(required, cols)
      details$meteo_columns <- cols
      if (length(missing) > 0) {
        add_message(
          paste("Meteorological Excel is missing columns:", paste(missing, collapse = ", ")),
          FALSE
        )
      } else {
        add_message("Meteorological Excel looks valid.")
      }
    }
  }

  list(ok = ok, messages = messages, details = details)
}

thermos_required_landcover_layers <- function() {
  c(
    "landcover",
    "albedo",
    "emis",
    "z0",
    "et_scale",
    "lai",
    "canopy_cover",
    "k_ext",
    "gai",
    "wall_albedo",
    "wall_emis"
  )
}

thermos_find_suffix_file <- function(dir_path, suffix, label) {
  files <- list.files(dir_path, pattern = "\\.tif$", full.names = TRUE)
  matches <- files[vapply(
    files,
    function(path) identical(thermos_extract_suffix(path), suffix),
    logical(1)
  )]

  if (length(matches) == 0) {
    return(NULL)
  }
  if (length(matches) > 1) {
    stop(
      "Multiple ", label, " rasters match plot '", suffix, "': ",
      paste(basename(matches), collapse = ", "),
      call. = FALSE
    )
  }
  matches[[1]]
}

thermos_compare_raster_geometry <- function(reference_path, candidate_path, label) {
  reference <- terra::rast(reference_path)
  candidate <- terra::rast(candidate_path)
  same_geometry <- isTRUE(terra::compareGeom(
    reference,
    candidate,
    crs = TRUE,
    ext = TRUE,
    rowcol = TRUE,
    res = TRUE,
    stopOnError = FALSE
  ))

  if (!same_geometry) {
    stop(
      label, " is not aligned with DEM '", basename(reference_path),
      "': ", basename(candidate_path),
      ". CRS, extent, resolution, rows, and columns must match.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

thermos_check_raster_compatibility <- function(reference_path, candidate_path, label) {
  reference <- terra::rast(reference_path)
  candidate <- terra::rast(candidate_path)

  if (!isTRUE(terra::same.crs(reference, candidate))) {
    stop(
      label, " has a different CRS from DEM '", basename(reference_path),
      "': ", basename(candidate_path), ".",
      call. = FALSE
    )
  }

  ref_ext <- terra::ext(reference)
  candidate_ext <- terra::ext(candidate)
  overlaps <- max(terra::xmin(ref_ext), terra::xmin(candidate_ext)) <
    min(terra::xmax(ref_ext), terra::xmax(candidate_ext)) &&
    max(terra::ymin(ref_ext), terra::ymin(candidate_ext)) <
    min(terra::ymax(ref_ext), terra::ymax(candidate_ext))

  if (!overlaps) {
    stop(
      label, " does not overlap DEM '", basename(reference_path),
      "': ", basename(candidate_path), ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

thermos_validate_existing_thermal_inputs <- function(dem_dir,
                                                     dsm_dir,
                                                     svf_dir,
                                                     lc_dir,
                                                     met_xlsx,
                                                     plot_suffix = "__auto__") {
  thermos_dir_must_exist(dem_dir, "DEM")
  thermos_dir_must_exist(dsm_dir, "DSM")
  thermos_dir_must_exist(svf_dir, "SVF")
  thermos_dir_must_exist(lc_dir, "Rasterized land-cover")

  if (!file.exists(met_xlsx)) {
    stop("Meteorological Excel file not found: ", met_xlsx, call. = FALSE)
  }

  met_cols <- names(readxl::read_xlsx(met_xlsx, n_max = 0))
  required_met_cols <- c("date", "hour_utc", "Ta", "Td", "u10", "v10", "ssrd", "strd", "slhf")
  missing_met_cols <- setdiff(required_met_cols, met_cols)
  if (length(missing_met_cols) > 0) {
    stop(
      "Meteorological Excel is missing columns: ",
      paste(missing_met_cols, collapse = ", "),
      call. = FALSE
    )
  }

  plot_suffixes <- thermos_resolve_plot_suffixes(
    plot_suffix = plot_suffix,
    dem_dir = dem_dir,
    dsm_dir = dsm_dir,
    svf_dir = svf_dir,
    lc_dir = lc_dir
  )
  if (length(plot_suffixes) == 0) {
    stop("No matching plots were detected in the supplied raster folders.", call. = FALSE)
  }

  required_layers <- thermos_required_landcover_layers()
  details <- vector("list", length(plot_suffixes))
  names(details) <- plot_suffixes

  for (suffix in plot_suffixes) {
    dem_path <- thermos_find_suffix_file(dem_dir, suffix, "DEM")
    dsm_path <- thermos_find_suffix_file(dsm_dir, suffix, "DSM")
    svf_path <- file.path(svf_dir, paste0("svf_", suffix, ".tif"))
    layer_paths <- stats::setNames(
      file.path(lc_dir, paste0(required_layers, "_", suffix, ".tif")),
      required_layers
    )

    missing <- c(
      if (is.null(dem_path)) paste0("DEM (*_", suffix, ".tif)") else character(),
      if (is.null(dsm_path)) paste0("DSM (*_", suffix, ".tif)") else character(),
      if (!file.exists(svf_path)) basename(svf_path) else character(),
      basename(layer_paths[!file.exists(layer_paths)])
    )
    if (length(missing) > 0) {
      stop(
        "Plot '", suffix, "' is incomplete. Missing: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }

    thermos_check_raster_compatibility(dem_path, dsm_path, "DSM")
    thermos_check_raster_compatibility(dem_path, svf_path, "SVF")
    for (layer_name in names(layer_paths)) {
      thermos_compare_raster_geometry(
        dem_path,
        layer_paths[[layer_name]],
        paste("Land-cover layer", layer_name)
      )
    }

    details[[suffix]] <- list(
      dem = dem_path,
      dsm = dsm_path,
      svf = svf_path,
      landcover = layer_paths
    )
  }

  list(
    ok = TRUE,
    plot_suffixes = plot_suffixes,
    messages = paste(
      "Existing rasters are valid for",
      length(plot_suffixes),
      "plot(s):",
      paste(plot_suffixes, collapse = ", ")
    ),
    details = details
  )
}
