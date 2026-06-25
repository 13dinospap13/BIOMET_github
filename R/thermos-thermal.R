thermos_compute_shadow <- function(dsm_r, dem_r, zenith_deg, azimuth_deg, observer_height = 1.1) {
  r <- terra::res(dsm_r)[1]
  tan_elev <- tan((90 - zenith_deg) * pi / 180)
  az_rad <- azimuth_deg * pi / 180
  dx <- sin(az_rad)
  dy <- -cos(az_rad)
  mat <- as.matrix(dsm_r, wide = TRUE)
  dem_mat <- as.matrix(dem_r, wide = TRUE)
  nr <- nrow(mat)
  nc <- ncol(mat)
  shd <- matrix(1L, nrow = nr, ncol = nc)
  max_steps <- ceiling(150 / r)
  steps <- seq_len(max_steps)
  valid_cells <- which(!is.na(mat) & !is.na(dem_mat), arr.ind = TRUE)

  for (i in seq_len(nrow(valid_cells))) {
    row <- valid_cells[i, 1]
    col <- valid_cells[i, 2]
    h0 <- dem_mat[row, col] + observer_height

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

    h_obs <- mat[cbind(rr, cc)]
    if (anyNA(h_obs)) {
      first_na <- which(is.na(h_obs))[1]
      if (first_na == 1) {
        next
      }
      keep <- seq_len(first_na - 1)
      h_obs <- h_obs[keep]
      s <- s[keep]
    }

    if (any(h_obs > h0 + tan_elev * r * s)) {
      shd[row, col] <- 0L
    }
  }

  out <- terra::setValues(terra::rast(dsm_r), as.vector(t(shd)))
  ov <- terra::values(out, mat = FALSE)
  ov[is.na(terra::values(dem_r, mat = FALSE))] <- NA
  terra::values(out) <- ov
  out
}

thermos_compute_I0 <- function(doy, hour_utc, lat_deg) {
  Isc <- 1367
  B <- 2 * pi * (doy - 1) / 365
  Ecc <- 1.00011 + 0.034221 * cos(B) + 0.00128 * sin(B) +
    0.000719 * cos(2 * B) + 0.000077 * sin(2 * B)
  decl <- 0.006918 - 0.399912 * cos(B) + 0.070257 * sin(B) -
    0.006758 * cos(2 * B) + 0.000907 * sin(2 * B)
  hour_angle <- (hour_utc - 12) * pi / 12
  lat_rad <- lat_deg * pi / 180
  cos_z <- sin(lat_rad) * sin(decl) + cos(lat_rad) * cos(decl) * cos(hour_angle)
  Isc * Ecc * pmax(cos_z, 0)
}

thermos_save_rast <- function(vec, name, ref, file_id, out_dir, dem_vals) {
  r <- terra::setValues(terra::rast(ref), vec)
  rv <- terra::values(r, mat = FALSE)
  rv[is.na(dem_vals)] <- NA
  terra::values(r) <- rv
  path <- file.path(out_dir, paste0(name, "_", file_id, ".tif"))
  terra::writeRaster(r, path, overwrite = TRUE)
  r
}

thermos_calc_pet <- function(ta, tr, v, vp, M, icl, ht, mbody) {
  Adu <- 0.203 * mbody^0.425 * ht^0.725
  M_w2 <- M / Adu
  fcl <- 1 + 0.31 * icl / 0.155
  hc <- pmax(2.38 * abs(tr - ta)^0.25, 12.1 * sqrt(v))
  hr <- 4 * 0.97 * 5.67e-8 * ((tr + ta) / 2 + 273)^3
  Tsk <- 34 - 0.065 * (M_w2 - 83)
  Tcl <- (Tsk + (hc * ta + hr * tr) * icl * fcl) / (1 + icl * fcl * (hc + hr))
  E_sw <- pmax(0.42 * (M_w2 - 58.15), 0)
  E_re <- 1.7e-5 * M_w2 * (5867 - vp * 100) + 0.0014 * M_w2 * (34 - ta)
  R_cl <- fcl * hr * (Tcl - tr)
  C_cl <- fcl * hc * (Tcl - ta)
  ta + (M_w2 - E_sw - E_re - R_cl - C_cl - 58.15) / (hc + hr)
}

thermos_calc_pmv <- function(ta, tr, v, vp, Met, Clo, ht, mbody) {
  Adu <- 0.203 * mbody^0.425 * ht^0.725
  M_w2 <- Met / Adu
  icl <- Clo * 0.155
  fcl <- ifelse(icl <= 0.078, 1 + 1.29 * icl, 1.05 + 0.645 * icl)
  pa <- vp * 100

  tcl <- ta + (35.5 - ta) / (3.5 * (6.45 * icl + 0.1))
  for (iter in seq_len(10)) {
    hcf <- pmax(2.38 * abs(tcl - ta)^0.25, 12.1 * sqrt(v))
    f_val <- tcl - 35.7 + 0.028 * M_w2 +
      icl * (3.96e-8 * fcl * ((tcl + 273)^4 - (tr + 273)^4) +
        fcl * hcf * (tcl - ta))
    f_drv <- 1 + icl * (3.96e-8 * fcl * 4 * (tcl + 273)^3 + fcl * hcf)
    f_drv[abs(f_drv) < 1e-6] <- 1e-6
    tcl_next <- tcl - f_val / f_drv
    if (max(abs(tcl_next - tcl), na.rm = TRUE) < 1e-4) {
      tcl <- tcl_next
      break
    }
    tcl <- tcl_next
  }

  hcf <- pmax(2.38 * abs(tcl - ta)^0.25, 12.1 * sqrt(v))
  hl2 <- ifelse(M_w2 > 58.15, 0.42 * (M_w2 - 58.15), 0)
  L <- M_w2 - 3.05e-3 * (5733 - 6.99 * M_w2 - pa) -
    hl2 - 1.7e-5 * M_w2 * (5867 - pa) -
    0.0014 * M_w2 * (34 - ta) -
    3.96e-8 * fcl * ((tcl + 273)^4 - (tr + 273)^4) -
    fcl * hcf * (tcl - ta)

  (0.303 * exp(-0.036 * M_w2) + 0.028) * L
}

thermos_calc_set_one <- function(ta, tr, vel, rh, clo, met, sa, ht, wt) {
  wme <- 0
  pb <- 760
  ltime <- 60
  csw <- 170
  cdil <- 120
  cstr <- 0.5

  m <- met * 58.2
  w <- wme * 58.2
  kclo <- 0.25
  tskn <- 33.7
  tcrn <- 36.8
  tbn <- 36.49
  skbfn <- 6.3
  sbc <- 5.6697e-08

  vel <- max(vel, 0.1)
  tsk <- tskn
  tcr <- tcrn
  skbf <- skbfn
  mshiv <- 0
  alfa <- 0.1
  rmm <- m
  esk <- 0.1 * met

  atm <- pb / 760
  rcl <- 0.155 * clo
  facl <- 1 + 0.15 * clo
  lr <- 2.2 / atm
  pa <- rh * exp(18.6686 - (4030.183 / (ta + 235))) / 100

  if (clo <= 0) {
    wcrit <- 0.38 * vel^(-0.29)
    icl <- 1
  } else {
    wcrit <- 0.59 * vel^(-0.08)
    icl <- 0.45
  }

  chc <- 3 * atm^0.53
  if (rmm / 58.2 < 0.85) {
    chca <- 0
  } else {
    chca <- 5.66 * ((rmm / 58.2 - 0.85) * atm)^0.39
  }
  chcv <- 8.600001 * (vel * atm)^0.53
  if (chc <= chca) {
    chc <- chca
  }
  chc <- max(chc, chcv)

  chr <- 4.7
  ctc <- chr + chc
  ra <- 1 / (facl * ctc)
  top <- (chr * tr + chc * ta) / ctc
  tcl <- top + (tsk - top) / (ctc * (ra + rcl))
  tclold <- tcl
  flag <- FALSE

  fnsvp <- function(T) exp(18.6686 - 4030.183 / (T + 235))

  for (tim in seq_len(ltime)) {
    if (flag) {
      tcl <- (ra * tsk + rcl * top) / (ra + rcl)
      if (abs(tcl - tclold) > 0.01) {
        flag <- FALSE
        tclold <- tcl
      } else {
        flag <- TRUE
      }
    }

    while (!flag) {
      chr <- 4.0 * 0.95 * sbc * (((tcl + tr) / 2.0 + 273.15)^3.0) * 0.73
      ctc <- chr + chc
      ra <- 1 / (facl * ctc)
      top <- (chr * tr + chc * ta) / ctc
      tcl <- (ra * tsk + rcl * top) / (ra + rcl)
      if (abs(tcl - tclold) > 0.01) {
        flag <- FALSE
        tclold <- tcl
      } else {
        flag <- TRUE
      }
    }

    dry <- (tsk - top) / (ra + rcl)
    hfcs <- (tcr - tsk) * (5.28 + 1.163 * skbf)
    eres <- 0.0023 * m * (44 - pa)
    cres <- 0.0014 * m * (34 - ta)
    scr <- m - hfcs - eres - cres - w
    ssk <- hfcs - dry - esk
    tcsk <- 0.97 * alfa * wt
    tccr <- 0.97 * (1 - alfa) * wt
    dtsk <- (ssk * sa) / tcsk / 60
    dtcr <- scr * sa / tccr / 60
    tsk <- tsk + dtsk
    tcr <- tcr + dtcr
    tb <- alfa * tsk + (1 - alfa) * tcr

    if (tsk > tskn) {
      warms <- tsk - tskn
      colds <- 0
    } else {
      colds <- tskn - tsk
      warms <- 0
    }

    if (tcr > tcrn) {
      warmc <- tcr - tcrn
      coldc <- 0
    } else {
      coldc <- tcrn - tcr
      warmc <- 0
    }

    if (tb > tbn) {
      warmb <- tb - tbn
    } else {
      warmb <- 0
    }

    skbf <- (skbfn + cdil * warmc) / (1 + cstr * colds)
    if (skbf > 90) {
      skbf <- 90
    }
    if (skbf < 0.5) {
      skbf <- 0.5
    }

    regsw <- csw * warmb * exp(warms / 10.7)
    if (regsw > 500) {
      regsw <- 500
    }
    ersw <- 0.68 * regsw
    rea <- 1 / (lr * facl * chc)
    recl <- rcl / (lr * icl)
    emax <- (fnsvp(tsk) - pa) / (rea + recl)
    prsw <- ersw / emax
    pwet <- 0.06 + 0.94 * prsw
    edif <- pwet * emax - ersw
    esk <- ersw + edif

    if (pwet > wcrit) {
      pwet <- wcrit
      prsw <- wcrit / 0.94
      ersw <- prsw * emax
      edif <- 0.06 * (1 - prsw) * emax
      esk <- ersw + edif
    }

    if (emax < 0) {
      edif <- 0
      ersw <- 0
      pwet <- wcrit
      prsw <- wcrit
      esk <- emax
    }

    mshiv <- 19.4 * colds * coldc
    m <- rmm + mshiv
    alfa <- 0.0417737 + 0.7451833 / (skbf + 0.585417)
  }

  hsk <- dry + esk
  pssk <- fnsvp(tsk)
  chrs <- chr
  if (met < 0.85) {
    chcs <- 3
  } else {
    chcs <- 5.66 * (met - 0.85)^0.39
    if (chcs < 3) {
      chcs <- 3
    }
  }
  ctcs <- chcs + chrs
  rclos <- 1.52 / ((met - wme) + 0.6944) - 0.1835
  rcls <- 0.155 * rclos
  facls <- 1 + kclo * rclos
  fcls <- 1 / (1 + 0.155 * facls * ctcs * rclos)
  ims <- 0.45
  icls <- (ims * chcs / ctcs * (1 - fcls)) / (chcs / ctcs - fcls * ims)
  ras <- 1 / (facls * ctcs)
  reas <- 1 / (lr * facls * chcs)
  recls <- rcls / (lr * icls)
  hd_s <- 1 / (ras + rcls)
  he_s <- 1 / (reas + recls)

  fnerrs <- function(x) {
    hsk - hd_s * (tsk - x) - pwet * he_s * (pssk - 0.5 * fnsvp(x))
  }

  delta <- 0.0001
  xold <- tsk - hsk / hd_s
  flag2 <- FALSE
  while (!flag2) {
    err1 <- fnerrs(xold)
    err2 <- fnerrs(xold + delta)
    x <- xold - delta * err1 / (err2 - err1)
    if (abs(x - xold) > 0.01) {
      xold <- x
      flag2 <- FALSE
    } else {
      flag2 <- TRUE
    }
  }
  x
}

thermos_calc_set <- function(ta, tr, v, rh, M, icl, ht, mbody) {
  ht_cm <- ht * 100
  sa <- ((ht_cm * mbody) / 3600)^0.5
  met_units <- M / (58.2 * sa)
  clo_units <- icl / 0.155

  vapply(
    seq_along(ta),
    function(i) {
      thermos_calc_set_one(
        ta = ta[i],
        tr = tr[i],
        vel = v[i],
        rh = rh[i],
        clo = clo_units,
        met = met_units,
        sa = sa,
        ht = ht_cm,
        wt = mbody
      )
    },
    numeric(1)
  )
}

thermos_lc_load <- function(name, plot_suffix, lc_dir, dem, dem_vals) {
  p <- file.path(lc_dir, paste0(name, "_", plot_suffix, ".tif"))
  if (!file.exists(p)) {
    return(NULL)
  }
  r <- terra::rast(p)
  if (!all(terra::res(r) == terra::res(dem))) {
    r <- terra::resample(r, dem, method = "bilinear")
  }
  vals <- terra::values(r, mat = FALSE)
  vals[is.na(dem_vals)] <- NA
  terra::values(r) <- vals
  r
}

thermos_lc_load_safe <- function(name, default, plot_suffix, lc_dir, dem, dem_vals) {
  r <- thermos_lc_load(name, plot_suffix, lc_dir, dem, dem_vals)
  if (is.null(r)) {
    return(thermos_make_const_rast(dem, default))
  }
  rv <- terra::values(r, mat = FALSE)
  rv[is.nan(rv)] <- NA
  if (sum(!is.na(rv)) == 0) {
    return(thermos_make_const_rast(dem, default))
  }
  terra::values(r) <- rv
  thermos_fill_default_in_mask(r, dem, default)
}

thermos_thermal_comfort_one_plot <- function(dem_dir,
                                            dsm_dir,
                                            svf_dir,
                                            lc_dir,
                                            out_dir,
                                            plot_suffix,
                                            met_df,
                                            alpha_k = 0.70,
                                            eps_p = 0.97,
                                            Met = 80,
                                            Clo = 0.9,
                                            ht = 1.75,
                                            mbody = 75) {
  dem_path <- thermos_first_match(dem_dir, paste0(plot_suffix, "\\.tif$"), "DEM")
  dsm_path <- thermos_first_match(dsm_dir, paste0(plot_suffix, "\\.tif$"), "DSM")
  svf_path <- thermos_first_match(svf_dir, paste0(plot_suffix, "\\.tif$"), "SVF")

  dem <- terra::rast(dem_path)
  dsm <- terra::rast(dsm_path)
  svf <- terra::rast(svf_path)

  if (!all(terra::res(dsm) == terra::res(dem))) {
    dsm <- terra::resample(dsm, dem, method = "bilinear")
  }
  if (!all(terra::res(svf) == terra::res(dem))) {
    svf <- terra::resample(svf, dem, method = "bilinear")
  }

  dem_vals <- terra::values(dem, mat = FALSE)
  dem_vals[is.nan(dem_vals)] <- NA
  svf_vals <- terra::values(svf, mat = FALSE)
  svf_vals[is.na(dem_vals)] <- NA
  terra::values(svf) <- svf_vals

  albedo_r <- thermos_lc_load_safe("albedo", 0.15, plot_suffix, lc_dir, dem, dem_vals)
  emis_r <- thermos_lc_load_safe("emis", 0.95, plot_suffix, lc_dir, dem, dem_vals)
  gai_r <- thermos_lc_load_safe("gai", 0.0, plot_suffix, lc_dir, dem, dem_vals)
  k_ext_r <- thermos_lc_load_safe("k_ext", 0.5, plot_suffix, lc_dir, dem, dem_vals)
  et_scale_r <- thermos_lc_load_safe("et_scale", 0.0, plot_suffix, lc_dir, dem, dem_vals)
  z0_r <- thermos_lc_load_safe("z0", 0.5, plot_suffix, lc_dir, dem, dem_vals)
  wall_emis_r <- thermos_lc_load_safe("wall_emis", 0.92, plot_suffix, lc_dir, dem, dem_vals)
  wall_alb_r <- thermos_lc_load_safe("wall_albedo", 0.0, plot_suffix, lc_dir, dem, dem_vals)

  centroid_wgs84 <- terra::project(
    terra::vect(
      matrix(
        c(
          mean(c(terra::xmin(terra::ext(dem)), terra::xmax(terra::ext(dem)))),
          mean(c(terra::ymin(terra::ext(dem)), terra::ymax(terra::ext(dem))))
        ),
        ncol = 2
      ),
      crs = terra::crs(dem)
    ),
    "EPSG:4326"
  )
  lon_site <- terra::geom(centroid_wgs84)[, "x"]
  lat_site <- terra::geom(centroid_wgs84)[, "y"]

  summary_df <- data.frame(
    plot = character(),
    ts_id = character(),
    date = character(),
    hour_utc = integer(),
    Ta = numeric(),
    RH = numeric(),
    va_11m = numeric(),
    Tmrt_mean = numeric(),
    PET_mean = numeric(),
    mPET_mean = numeric(),
    PMV_mean = numeric(),
    SET_mean = numeric(),
    UTCI_mean = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(met_df))) {
    row <- met_df[i, ]
    hour_utc <- as.integer(row$hour_utc)
    Ta_val <- as.numeric(row$Ta)
    Td_val <- as.numeric(row$Td)
    u10_val <- as.numeric(row$u10)
    v10_val <- as.numeric(row$v10)
    ssrd_val <- as.numeric(row$ssrd)
    strd_val <- as.numeric(row$strd)
    slhf_val <- as.numeric(row$slhf)

    date_str <- format(as.Date(row$date), "%Y-%m-%d")
    ts <- as.POSIXct(paste(date_str, sprintf("%02d:00:00", hour_utc)), tz = "UTC")
    ts_id <- format(ts, "%Y%m%d_%H")
    file_id <- paste0(plot_suffix, "_", ts_id)
    doy <- lubridate::yday(ts)

    va_val <- sqrt(u10_val^2 + v10_val^2)
    es_val <- 6.112 * exp(17.67 * Ta_val / (Ta_val + 243.5))
    ea_val <- 6.112 * exp(17.67 * Td_val / (Td_val + 243.5))
    rh_val <- min(max((ea_val / es_val) * 100, 0), 100)

    sol <- solartime::computeSunPosition(timestamp = ts, latDeg = lat_site, longDeg = lon_site)
    solar_elev <- as.numeric(sol[, "elevation"]) * 180 / pi
    solar_az <- as.numeric(sol[, "azimuth"]) * 180 / pi
    solar_zenith <- 90 - solar_elev

    if (solar_zenith >= 90) {
      next
    }

    Ta_r <- thermos_make_const_rast(dem, Ta_val)
    va_r <- thermos_make_const_rast(dem, va_val)
    ssrd <- thermos_make_const_rast(dem, ssrd_val)
    strd <- thermos_make_const_rast(dem, strd_val)
    rh_r <- thermos_make_const_rast(dem, rh_val)
    slhf_r <- thermos_make_const_rast(dem, slhf_val)

    I0 <- thermos_compute_I0(doy, hour_utc, lat_site)
    if (I0 < 1) {
      next
    }

    kt <- terra::clamp(ssrd / I0, 0, 1)
    fd <- terra::ifel(
      kt <= 0.22,
      1 - 0.09 * kt,
      terra::ifel(
        kt <= 0.80,
        0.9511 - 0.1604 * kt + 4.388 * kt^2 - 16.638 * kt^3 + 12.336 * kt^4,
        0.165
      )
    )
    K_diff <- ssrd * fd
    K_dir <- ssrd * (1 - fd)

    rho_air <- 1.2
    Cp <- 1005
    h_mix <- 50
    dTa_cool <- terra::clamp((slhf_r * et_scale_r) / (rho_air * Cp * h_mix), 0, 5)
    Ta_r <- Ta_r - dTa_cool

    z_ref <- 10.0
    z_target <- 1.1
    va_11m_r <- terra::clamp(va_r * log(z_target / z0_r) / log(z_ref / z0_r), 0.5, 17)

    shadow <- thermos_compute_shadow(dsm, dem, solar_zenith, solar_az, observer_height = z_target)
    shd_v <- terra::values(shadow, mat = FALSE)
    shd_v[is.na(dem_vals)] <- NA
    terra::values(shadow) <- shd_v

    tau_sw <- exp(-k_ext_r * gai_r)
    tau_lw <- exp(-0.9 * gai_r)
    sigma <- 5.67e-8
    Tc <- Ta_r - terra::ifel(gai_r > 0, 3.0, 0)
    L_canopy <- 0.96 * sigma * (Tc + 273.15)^4

    K_up <- ssrd * albedo_r
    K_down <- K_diff * svf * tau_sw
    K_dir_att <- K_dir * shadow * tau_sw
    L_up <- emis_r * sigma * (Ta_r + 273.15)^4
    L_wall <- wall_emis_r * sigma * (Ta_r + 273.15)^4
    L_down <- (strd * svf + L_wall * (1 - svf)) * tau_lw + L_canopy * (1 - tau_lw)
    K_wall <- ssrd * wall_alb_r * (1 - svf)
    sum_K <- (K_dir_att + K_down + K_up + 4 * 0.5 * (K_down + K_up + K_wall)) / 6
    sum_L <- (L_down + L_up + 4 * (0.5 * L_down + 0.5 * L_up)) / 6

    Tmrt_C <- ((alpha_k / eps_p * sum_K + sum_L) / sigma)^0.25 - 273.15
    terra::writeRaster(
      Tmrt_C,
      file.path(out_dir, paste0("Tmrt_", file_id, ".tif")),
      overwrite = TRUE
    )

    ta_v <- terra::values(Ta_r, mat = FALSE)
    tr_v <- terra::values(Tmrt_C, mat = FALSE)
    rh_v <- terra::values(rh_r, mat = FALSE)
    v_v <- terra::values(va_11m_r, mat = FALSE)
    v10_v <- terra::values(va_r, mat = FALSE)
    n <- length(ta_v)
    valid <- !is.na(ta_v) & !is.na(tr_v) & !is.na(v_v) & !is.na(rh_v) & !is.na(v10_v)

    ta <- ta_v[valid]
    tr <- tr_v[valid]
    rh <- rh_v[valid]
    v <- pmax(v_v[valid], 0.1)
    v10 <- pmax(v10_v[valid], 0.5)
    vp <- rh / 100 * 6.112 * exp(17.67 * ta / (ta + 243.5))

    icl <- Clo * 0.155
    pmv_tmp <- thermos_calc_pmv(ta, tr, v, vp, Met, Clo, ht, mbody)

    D_Tmrt <- pmin(pmax(tr - ta, -30), 70)
    v_utci <- pmin(pmax(v10, 0.5), 17)
    pa_utci <- pmin(pmax(vp / 10, 0), 5)
    ta_u <- pmin(pmax(ta, -50), 50)

    utci_tmp <- ta_u +
      (6.07562052e-01) + (-2.27712343e-02) * ta_u + (8.06470249e-04) * ta_u^2 +
      (-1.54271372e-04) * ta_u^3 + (-3.24651995e-06) * ta_u^4 + (7.32602852e-08) * ta_u^5 +
      (1.35959073e-09) * ta_u^6 + (-2.25836520e+00) * v_utci + (8.80326035e-02) * ta_u * v_utci +
      (2.16844454e-03) * ta_u^2 * v_utci + (-1.53347087e-05) * ta_u^3 * v_utci +
      (-5.72983704e-07) * ta_u^4 * v_utci + (-2.55090145e-09) * ta_u^5 * v_utci +
      (-7.51269505e-01) * v_utci^2 + (-4.08350271e-03) * ta_u * v_utci^2 +
      (-5.21670675e-05) * ta_u^2 * v_utci^2 + (1.94544667e-06) * ta_u^3 * v_utci^2 +
      (1.14099205e-08) * ta_u^4 * v_utci^2 + (1.58137256e-01) * v_utci^3 +
      (-6.57263143e-04) * ta_u * v_utci^3 + (2.22697524e-05) * ta_u^2 * v_utci^3 +
      (-4.16117031e-08) * ta_u^3 * v_utci^3 + (-1.27762753e-02) * v_utci^4 +
      (9.66891875e-05) * ta_u * v_utci^4 + (2.52785852e-06) * ta_u^2 * v_utci^4 +
      (4.56306672e-04) * v_utci^5 + (-1.74202546e-06) * ta_u * v_utci^5 +
      (-5.91491269e-06) * v_utci^6 + (3.98374109e-01) * D_Tmrt +
      (1.83945314e-04) * ta_u * D_Tmrt + (-1.73754510e-04) * ta_u^2 * D_Tmrt +
      (-7.60781159e-07) * ta_u^3 * D_Tmrt + (3.77830287e-08) * ta_u^4 * D_Tmrt +
      (5.43079673e-10) * ta_u^5 * D_Tmrt + (-2.00518269e-02) * v_utci * D_Tmrt +
      (8.92859837e-04) * ta_u * v_utci * D_Tmrt + (3.45433048e-06) * ta_u^2 * v_utci * D_Tmrt +
      (-3.77925774e-07) * ta_u^3 * v_utci * D_Tmrt + (-1.69699377e-09) * ta_u^4 * v_utci * D_Tmrt +
      (1.69992415e-04) * v_utci^2 * D_Tmrt + (-4.99204314e-05) * ta_u * v_utci^2 * D_Tmrt +
      (2.47417178e-07) * ta_u^2 * v_utci^2 * D_Tmrt + (1.07596466e-08) * ta_u^3 * v_utci^2 * D_Tmrt +
      (8.49242932e-05) * v_utci^3 * D_Tmrt + (1.35191328e-06) * ta_u * v_utci^3 * D_Tmrt +
      (-6.21531254e-09) * ta_u^2 * v_utci^3 * D_Tmrt + (-4.99410301e-06) * v_utci^4 * D_Tmrt +
      (-1.89489258e-08) * ta_u * v_utci^4 * D_Tmrt + (8.15300114e-08) * v_utci^5 * D_Tmrt +
      (7.55043090e-04) * D_Tmrt^2 + (-5.65095215e-05) * ta_u * D_Tmrt^2 +
      (-4.52166564e-07) * ta_u^2 * D_Tmrt^2 + (2.46688878e-08) * ta_u^3 * D_Tmrt^2 +
      (2.42674348e-10) * ta_u^4 * D_Tmrt^2 + (1.54547250e-04) * v_utci * D_Tmrt^2 +
      (5.24110970e-06) * ta_u * v_utci * D_Tmrt^2 + (-8.75874982e-08) * ta_u^2 * v_utci * D_Tmrt^2 +
      (-1.50743064e-09) * ta_u^3 * v_utci * D_Tmrt^2 + (-1.56236307e-05) * v_utci^2 * D_Tmrt^2 +
      (-1.33895614e-07) * ta_u * v_utci^2 * D_Tmrt^2 + (2.49709824e-09) * ta_u^2 * v_utci^2 * D_Tmrt^2 +
      (6.51711721e-07) * v_utci^3 * D_Tmrt^2 + (1.94960053e-09) * ta_u * v_utci^3 * D_Tmrt^2 +
      (-1.00361113e-08) * v_utci^4 * D_Tmrt^2 + (-1.21206673e-05) * D_Tmrt^3 +
      (-2.18203660e-07) * ta_u * D_Tmrt^3 + (7.51269482e-09) * ta_u^2 * D_Tmrt^3 +
      (9.79063848e-11) * ta_u^3 * D_Tmrt^3 + (1.25411264e-08) * v_utci * D_Tmrt^3 +
      (-6.67742461e-10) * ta_u * v_utci * D_Tmrt^3 + (2.20927476e-10) * ta_u^2 * v_utci * D_Tmrt^3 +
      (-4.06016116e-09) * v_utci^2 * D_Tmrt^3 + (3.14168468e-11) * ta_u * v_utci^2 * D_Tmrt^3 +
      (2.33146963e-11) * v_utci^3 * D_Tmrt^3 + (-6.85399141e-08) * D_Tmrt^4 +
      (3.55298533e-10) * ta_u * D_Tmrt^4 + (1.40272308e-11) * ta_u^2 * D_Tmrt^4 +
      (-1.85492662e-11) * v_utci * D_Tmrt^4 + (-1.22442541e-12) * ta_u * v_utci * D_Tmrt^4 +
      (-1.90539528e-12) * v_utci^2 * D_Tmrt^4 + (1.13901640e-11) * D_Tmrt^5 +
      (1.00716234e-13) * ta_u * D_Tmrt^5 + (-1.20268494e-13) * v_utci * D_Tmrt^5 +
      (-8.72647004e-14) * D_Tmrt^6 + (2.66836399e+00) * pa_utci +
      (1.59762500e-02) * ta_u * pa_utci + (-7.93531032e-03) * ta_u^2 * pa_utci +
      (-4.75059040e-04) * ta_u^3 * pa_utci + (1.47966723e-05) * ta_u^4 * pa_utci +
      (1.11982569e-07) * ta_u^5 * pa_utci + (3.83469289e-02) * v_utci * pa_utci +
      (-2.35033737e-03) * ta_u * v_utci * pa_utci + (-4.78156212e-05) * ta_u^2 * v_utci * pa_utci +
      (1.22206323e-06) * ta_u^3 * v_utci * pa_utci + (-1.15789920e-04) * v_utci^2 * pa_utci +
      (1.52462354e-05) * ta_u * v_utci^2 * pa_utci + (-5.34898432e-07) * ta_u^2 * v_utci^2 * pa_utci +
      (2.10793499e-06) * v_utci^3 * pa_utci + (-2.31382535e-07) * ta_u * v_utci^3 * pa_utci +
      (1.67677569e-08) * v_utci^4 * pa_utci

    pet_tmp <- thermos_calc_pet(ta, tr, v, vp, Met, icl, ht, mbody)
    Top <- 0.5 * ta + 0.5 * tr
    icl_auto <- pmax(pmin((-0.1635 * Top + 4.9431) * 0.155, 0.9 * 0.155), 0.1 * 0.155)
    mpet_tmp <- thermos_calc_pet(ta, tr, v, vp, Met, icl_auto, ht, mbody)
    set_tmp <- thermos_calc_set(ta, tr, v, rh, Met, icl, ht, mbody)

    pet_v <- rep(NA_real_, n)
    mpet_v <- rep(NA_real_, n)
    pmv_v <- rep(NA_real_, n)
    set_v <- rep(NA_real_, n)
    utci_v <- rep(NA_real_, n)
    pet_v[valid] <- pet_tmp
    mpet_v[valid] <- mpet_tmp
    pmv_v[valid] <- pmv_tmp
    set_v[valid] <- set_tmp
    utci_v[valid] <- utci_tmp

    pet_r <- thermos_save_rast(pet_v, "PET", Ta_r, file_id, out_dir, dem_vals)
    mpet_r <- thermos_save_rast(mpet_v, "mPET", Ta_r, file_id, out_dir, dem_vals)
    pmv_r <- thermos_save_rast(pmv_v, "PMV", Ta_r, file_id, out_dir, dem_vals)
    set_r <- thermos_save_rast(set_v, "SET", Ta_r, file_id, out_dir, dem_vals)
    utci_r <- thermos_save_rast(utci_v, "UTCI", Ta_r, file_id, out_dir, dem_vals)

    utci_class <- terra::classify(utci_r, matrix(c(
      -Inf, -40, 1, -40, -27, 2, -27, -13, 3, -13, 0, 4, 0, 9, 5,
      9, 26, 6, 26, 32, 7, 32, 38, 8, 38, 46, 9, 46, Inf, 10
    ), ncol = 3, byrow = TRUE))
    terra::writeRaster(
      utci_class,
      file.path(out_dir, paste0("UTCI_class_", file_id, ".tif")),
      overwrite = TRUE
    )

    gmean <- function(r) round(terra::global(r, "mean", na.rm = TRUE)[1, 1], 2)
    summary_df <- rbind(
      summary_df,
      data.frame(
        plot = plot_suffix,
        ts_id = ts_id,
        date = date_str,
        hour_utc = hour_utc,
        Ta = round(Ta_val, 1),
        RH = round(rh_val, 1),
        va_11m = round(max(va_val * (1.1 / 10.0)^0.2, 0.1), 2),
        Tmrt_mean = gmean(Tmrt_C),
        PET_mean = gmean(pet_r),
        mPET_mean = gmean(mpet_r),
        PMV_mean = gmean(pmv_r),
        SET_mean = gmean(set_r),
        UTCI_mean = gmean(utci_r),
        stringsAsFactors = FALSE
      )
    )
  }

  utils::write.csv(summary_df, file.path(out_dir, paste0("summary_", plot_suffix, ".csv")), row.names = FALSE)
  summary_df
}

#' Run Thermos thermal comfort workflow
#'
#' Computes Tmrt and thermal comfort indices for each meteorological timestep.
#'
#' @param dem_dir Directory with DEM rasters.
#' @param dsm_dir Directory with DSM rasters.
#' @param svf_dir Directory with SVF rasters.
#' @param lc_dir Directory with rasterized land-cover layers.
#' @param out_dir Output directory for results.
#' @param plot_suffix Plot identifier used to match raster filenames. You can
#'   pass one suffix, a comma-separated list, `"auto"`, or `"all"`.
#' @param met_xlsx Meteorological Excel file.
#' @param alpha_k Shortwave absorptivity of the human body.
#' @param eps_p Longwave emissivity of the human body.
#' @param Met Metabolic rate in watts.
#' @param Clo Clothing insulation in clo units.
#' @param ht Body height in meters.
#' @param mbody Body mass in kilograms.
#'
#' @return A data frame summarizing mean thermal metrics by timestep.
#' @export
thermos_thermal_comfort <- function(dem_dir,
                                   dsm_dir,
                                   svf_dir,
                                   lc_dir,
                                   out_dir,
                                   plot_suffix,
                                   met_xlsx,
                                   alpha_k = 0.70,
                                   eps_p = 0.97,
                                   Met = 80,
                                   Clo = 0.9,
                                   ht = 1.75,
                                   mbody = 75) {
  validation <- thermos_validate_existing_thermal_inputs(
    dem_dir = dem_dir,
    dsm_dir = dsm_dir,
    svf_dir = svf_dir,
    lc_dir = lc_dir,
    met_xlsx = met_xlsx,
    plot_suffix = plot_suffix
  )
  plot_suffixes <- validation$plot_suffixes
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  met_cols <- names(readxl::read_xlsx(met_xlsx, n_max = 0))
  met_df <- as.data.frame(readxl::read_xlsx(met_xlsx, skip = 2, col_names = met_cols))
  summaries <- lapply(
    plot_suffixes,
    function(current_suffix) {
      thermos_thermal_comfort_one_plot(
        dem_dir = dem_dir,
        dsm_dir = dsm_dir,
        svf_dir = svf_dir,
        lc_dir = lc_dir,
        out_dir = out_dir,
        plot_suffix = current_suffix,
        met_df = met_df,
        alpha_k = alpha_k,
        eps_p = eps_p,
        Met = Met,
        Clo = Clo,
        ht = ht,
        mbody = mbody
      )
    }
  )

  summary_df <- do.call(rbind, summaries)
  rownames(summary_df) <- NULL

  if (length(plot_suffixes) > 1) {
    utils::write.csv(summary_df, file.path(out_dir, "summary_all_plots.csv"), row.names = FALSE)
  }

  summary_df
}

#' Run the full Thermos pipeline
#'
#' Convenience wrapper that executes input checks, land-cover rasterization,
#' SVF calculation, and thermal comfort modeling in sequence.
#'
#' @inheritParams thermos_rasterize_landcover
#' @inheritParams thermos_calculate_svf
#' @inheritParams thermos_thermal_comfort
#'
#' @return A named list with validation results and summaries from all steps.
#' @export
thermos_run_pipeline <- function(lc_path,
                                obs_path,
                                dem_dir,
                                dsm_dir,
                                lc_dir,
                                svf_dir,
                                out_dir,
                                plot_suffix,
                                met_xlsx,
                                veg_types = c("tree", "shrub", "hedge"),
                                num_directions = 72,
                                max_distance = 30,
                                observer_height = 1.5,
                                alpha_k = 0.70,
                                eps_p = 0.97,
                                Met = 80,
                                Clo = 0.9,
                                ht = 1.75,
                                mbody = 75) {
  checks <- thermos_check_inputs(
    lc_path = lc_path,
    obs_path = obs_path,
    dem_dir = dem_dir,
    dsm_dir = dsm_dir,
    met_xlsx = met_xlsx
  )
  if (!checks$ok) {
    stop(paste(checks$messages, collapse = "\n"), call. = FALSE)
  }

  raster_summary <- thermos_rasterize_landcover(
    lc_path = lc_path,
    obs_path = obs_path,
    dem_dir = dem_dir,
    out_dir = lc_dir,
    veg_types = veg_types
  )
  svf_summary <- thermos_calculate_svf(
    dem_dir = dem_dir,
    dsm_dir = dsm_dir,
    svf_dir = svf_dir,
    num_directions = num_directions,
    max_distance = max_distance,
    observer_height = observer_height
  )
  thermal_summary <- thermos_thermal_comfort(
    dem_dir = dem_dir,
    dsm_dir = dsm_dir,
    svf_dir = svf_dir,
    lc_dir = lc_dir,
    out_dir = out_dir,
    plot_suffix = plot_suffix,
    met_xlsx = met_xlsx,
    alpha_k = alpha_k,
    eps_p = eps_p,
    Met = Met,
    Clo = Clo,
    ht = ht,
    mbody = mbody
  )

  list(
    checks = checks,
    rasterize = raster_summary,
    svf = svf_summary,
    thermal = thermal_summary
  )
}
