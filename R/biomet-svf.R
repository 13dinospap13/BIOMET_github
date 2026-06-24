biomet_compute_svf_matrix <- function(dsm_r,
                                      dem_r,
                                      num_directions,
                                      max_distance,
                                      observer_height) {
  r <- terra::res(dsm_r)[1]
  dsm_mat <- as.matrix(dsm_r, wide = TRUE)
  dem_mat <- as.matrix(dem_r, wide = TRUE)
  nr <- nrow(dsm_mat)
  nc <- ncol(dsm_mat)
  svf_mat <- matrix(NA_real_, nrow = nr, ncol = nc)
  max_steps <- ceiling(max_distance / r)
  steps <- seq_len(max_steps)
  angles <- seq(0, 2 * pi, length.out = num_directions + 1)[-(num_directions + 1)]
  angle_step <- 2 * pi / num_directions
  valid_cells <- which(!is.na(dem_mat), arr.ind = TRUE)
  valid_cells <- valid_cells[order(valid_cells[, 1], valid_cells[, 2]), , drop = FALSE]

  for (i in seq_len(nrow(valid_cells))) {
    row <- valid_cells[i, 1]
    col <- valid_cells[i, 2]
    h0 <- dem_mat[row, col] + observer_height
    max_angle <- 0
    angles_px <- angles + stats::runif(1, 0, angle_step)

    for (az in angles_px) {
      dx <- sin(az)
      dy <- -cos(az)
      rr <- round(row + dy * steps)
      cc <- round(col + dx * steps)
      in_bounds <- rr >= 1 & rr <= nr & cc >= 1 & cc <= nc

      if (!all(in_bounds)) {
        first_out <- which(!in_bounds)[1]
        if (first_out == 1) {
          next
        }
        keep <- seq_len(first_out - 1)
        rr <- rr[keep]
        cc <- cc[keep]
        s <- steps[keep]
      } else {
        s <- steps
      }

      h_obs <- dsm_mat[cbind(rr, cc)]
      if (anyNA(h_obs)) {
        first_na <- which(is.na(h_obs))[1]
        if (first_na == 1) {
          next
        }
        keep <- seq_len(first_na - 1)
        h_obs <- h_obs[keep]
        s <- s[keep]
      }

      if (length(h_obs) > 0) {
        ray_max <- max(atan2(h_obs - h0, r * s))
        if (ray_max > max_angle) {
          max_angle <- ray_max
        }
      }
    }

    svf_mat[row, col] <- 1 - sin(max_angle)
  }

  out <- terra::setValues(terra::rast(dsm_r), as.vector(t(svf_mat)))
  terra::mask(out, dem_r)
}

#' Calculate Sky View Factor rasters
#'
#' Matches DEM and DSM rasters by suffix and computes one SVF raster per plot.
#'
#' @param dem_dir Directory with DEM rasters.
#' @param dsm_dir Directory with DSM rasters.
#' @param svf_dir Output directory for SVF rasters.
#' @param num_directions Number of ray directions.
#' @param max_distance Maximum search distance in meters.
#' @param observer_height Observer height above ground in meters.
#'
#' @return A summary data frame with plot status and SVF statistics.
#' @export
biomet_calculate_svf <- function(dem_dir,
                                 dsm_dir,
                                 svf_dir,
                                 num_directions = 72,
                                 max_distance = 30,
                                 observer_height = 1.5) {
  biomet_dir_must_exist(dem_dir, "DEM")
  biomet_dir_must_exist(dsm_dir, "DSM")
  dir.create(svf_dir, showWarnings = FALSE, recursive = TRUE)

  dem_files <- list.files(dem_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(dem_files) == 0) {
    stop("No DEM files found in: ", dem_dir, call. = FALSE)
  }

  results <- data.frame(
    plot = character(),
    status = character(),
    svf_min = numeric(),
    svf_mean = numeric(),
    svf_max = numeric(),
    stringsAsFactors = FALSE
  )

  for (dem_path in dem_files) {
    suffix <- biomet_extract_suffix(dem_path)
    dsm_candidates <- list.files(
      dsm_dir,
      pattern = paste0(suffix, "\\.tif$"),
      full.names = TRUE
    )

    if (length(dsm_candidates) == 0) {
      results <- rbind(
        results,
        data.frame(
          plot = suffix,
          status = "skipped - no DSM match",
          svf_min = NA_real_,
          svf_mean = NA_real_,
          svf_max = NA_real_,
          stringsAsFactors = FALSE
        )
      )
      next
    }

    dem <- terra::rast(dem_path)
    dsm <- terra::rast(dsm_candidates[1])

    if (!all(terra::res(dsm) == terra::res(dem)) ||
        !all(dim(dsm)[1:2] == dim(dem)[1:2])) {
      dsm <- terra::resample(dsm, dem, method = "bilinear")
    }

    svf <- biomet_compute_svf_matrix(
      dsm_r = dsm,
      dem_r = dem,
      num_directions = num_directions,
      max_distance = max_distance,
      observer_height = observer_height
    )

    out_path <- file.path(svf_dir, paste0("svf_", suffix, ".tif"))
    terra::writeRaster(svf, out_path, overwrite = TRUE)
    stats <- terra::global(svf, c("min", "mean", "max"), na.rm = TRUE)

    results <- rbind(
      results,
      data.frame(
        plot = suffix,
        status = "ok",
        svf_min = round(stats[1, "min"], 3),
        svf_mean = round(stats[1, "mean"], 3),
        svf_max = round(stats[1, "max"], 3),
        stringsAsFactors = FALSE
      )
    )
  }

  utils::write.csv(results, file.path(svf_dir, "svf_batch_summary.csv"), row.names = FALSE)
  results
}
