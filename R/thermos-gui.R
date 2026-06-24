#' Launch the Thermos GUI
#'
#' Starts a Shiny application that gathers inputs and calls the package's core
#' functions. The GUI does not implement the science itself.
#'
#' @return Invisibly returns the Shiny app object.
#' @export
thermos_gui <- function() {
  asset_dir <- system.file("app/www", package = "Thermos")
  if (nzchar(asset_dir) && dir.exists(asset_dir)) {
    shiny::addResourcePath("thermos-assets", asset_dir)
  }

  path_input_ui <- function(text_id,
                            button_id,
                            label,
                            button_label = "Browse",
                            value = "") {
    shiny::tagList(
      shiny::tags$div(
        class = "thermos-file-card",
        shiny::tags$div(
          class = "thermos-file-icon",
          shiny::icon("folder-open")
        ),
        shiny::tags$div(
          class = "thermos-file-body",
          shiny::tags$div(class = "thermos-file-label", label),
          shiny::textInput(text_id, label = NULL, value = value)
        ),
        shiny::actionButton(button_id, button_label, class = "thermos-change-btn")
      )
    )
  }

  ps_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  }

  normalize_start_dir <- function(path) {
    if (!is.null(path) && nzchar(path)) {
      if (dir.exists(path)) {
        return(normalizePath(path, winslash = "/", mustWork = TRUE))
      }
      parent <- dirname(path)
      if (dir.exists(parent)) {
        return(normalizePath(parent, winslash = "/", mustWork = TRUE))
      }
    }
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }

  windows_choose_file_path <- function(caption, filter, default_path = getwd()) {
    initial_dir <- normalize_start_dir(default_path)
    script <- paste(
      "Add-Type -AssemblyName System.Windows.Forms",
      "$form = New-Object System.Windows.Forms.Form",
      "$form.TopMost = $true",
      "$form.StartPosition = 'CenterScreen'",
      "$form.WindowState = 'Minimized'",
      "$form.ShowInTaskbar = $false",
      "$dialog = New-Object System.Windows.Forms.OpenFileDialog",
      paste0("$dialog.Title = ", ps_quote(caption)),
      paste0("$dialog.InitialDirectory = ", ps_quote(normalizePath(initial_dir, winslash = "\\", mustWork = FALSE))),
      paste0("$dialog.Filter = ", ps_quote(filter)),
      "$dialog.Multiselect = $false",
      "$result = $dialog.ShowDialog($form)",
      "if ($result -eq [System.Windows.Forms.DialogResult]::OK) {",
      "  [Console]::OutputEncoding = [System.Text.Encoding]::UTF8",
      "  Write-Output $dialog.FileName",
      "}",
      sep = "; "
    )

    selected <- tryCatch(
      system2("powershell", c("-NoProfile", "-Command", script), stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )

    if (length(selected) == 0 || !nzchar(selected[1])) {
      return(NULL)
    }

    normalizePath(selected[1], winslash = "/", mustWork = FALSE)
  }

  choose_file_path <- function(caption, filter = "All files (*.*)|*.*", default_path = getwd()) {
    if (.Platform$OS.type == "windows") {
      return(windows_choose_file_path(caption, filter, default_path))
    }

    selected <- tcltk::tk_choose.files(
      caption = caption,
      multi = FALSE,
      default = file.path(normalize_start_dir(default_path), "")
    )
    if (length(selected) == 0 || !nzchar(selected[1])) {
      return(NULL)
    }

    normalizePath(selected[1], winslash = "/", mustWork = FALSE)
  }

  windows_choose_directory_path <- function(caption, default_path) {
    script <- paste(
      "Add-Type -AssemblyName System.Windows.Forms",
      "$form = New-Object System.Windows.Forms.Form",
      "$form.TopMost = $true",
      "$form.StartPosition = 'CenterScreen'",
      "$form.WindowState = 'Minimized'",
      "$form.ShowInTaskbar = $false",
      "$dialog = New-Object System.Windows.Forms.OpenFileDialog",
      paste0("$dialog.Title = ", ps_quote(caption)),
      paste0("$dialog.InitialDirectory = ", ps_quote(normalizePath(default_path, winslash = "\\", mustWork = FALSE))),
      "$dialog.Filter = 'Folders|*.none'",
      "$dialog.CheckFileExists = $false",
      "$dialog.CheckPathExists = $true",
      "$dialog.ValidateNames = $false",
      "$dialog.FileName = 'Select Folder'",
      "$result = $dialog.ShowDialog($form)",
      "if ($result -eq [System.Windows.Forms.DialogResult]::OK) {",
      "  [Console]::OutputEncoding = [System.Text.Encoding]::UTF8",
      "  Write-Output (Split-Path $dialog.FileName -Parent)",
      "}",
      sep = "; "
    )

    selected <- tryCatch(
      system2("powershell", c("-NoProfile", "-Command", script), stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )

    if (length(selected) == 0 || !nzchar(selected[1])) {
      return(NULL)
    }

    normalizePath(selected[1], winslash = "/", mustWork = FALSE)
  }

  choose_directory_path <- function(caption, default_path = getwd()) {
    if (is.null(default_path) || !nzchar(default_path)) {
      default_path <- getwd()
    }

    if (.Platform$OS.type == "windows") {
      return(windows_choose_directory_path(caption, default_path))
    }

    if (!requireNamespace("tcltk", quietly = TRUE)) {
      stop("Directory selection requires 'tcltk' on non-Windows platforms.", call. = FALSE)
    }

    selected <- tcltk::tk_choose.dir(
      default = normalizePath(default_path, winslash = "/", mustWork = FALSE),
      caption = caption
    )
    if (is.na(selected) || !nzchar(selected)) {
      return(NULL)
    }
    normalizePath(selected, winslash = "/", mustWork = FALSE)
  }

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::tags$head(
        shiny::tags$title("Thermos"),
        shiny::tags$style(
          shiny::HTML(
            "
            :root {
              --thermos-bg: #f5f8fb;
              --thermos-panel: #ffffff;
              --thermos-border: #dbe4ee;
              --thermos-text: #132238;
              --thermos-muted: #64748b;
              --thermos-blue: #0877b9;
              --thermos-blue-dark: #075985;
              --thermos-teal: #14b8a6;
              --thermos-green: #15975b;
              --thermos-amber: #f59e0b;
              --thermos-shadow: 0 10px 28px rgba(15, 23, 42, 0.08);
            }

            html, body {
              background: var(--thermos-bg);
              color: var(--thermos-text);
              font-family: 'Segoe UI', 'Aptos', sans-serif;
              min-width: 1180px;
            }

            .container-fluid {
              width: 100%;
              padding: 0;
            }

            .thermos-app {
              min-height: 100vh;
              display: grid;
              grid-template-columns: 72px minmax(330px, 420px) minmax(720px, 1fr);
              background:
                radial-gradient(circle at 18% 8%, rgba(20, 184, 166, 0.10), transparent 26%),
                linear-gradient(180deg, #f8fbff 0%, #eef4f9 100%);
            }

            .thermos-rail {
              background: #ffffff;
              border-right: 1px solid var(--thermos-border);
              display: flex;
              flex-direction: column;
              align-items: center;
              gap: 22px;
              padding: 18px 10px;
              position: sticky;
              top: 0;
              height: 100vh;
            }

            .thermos-logo {
              width: 42px;
              height: 42px;
              border-radius: 14px;
              display: grid;
              place-items: center;
              color: #ffffff;
              background: linear-gradient(135deg, var(--thermos-blue), var(--thermos-teal));
              box-shadow: 0 12px 24px rgba(8, 119, 185, 0.22);
              font-size: 19px;
            }

            .thermos-rail-item {
              width: 48px;
              height: 48px;
              border-radius: 12px;
              display: grid;
              place-items: center;
              color: var(--thermos-muted);
              font-size: 18px;
            }

            .thermos-rail-item.active {
              color: var(--thermos-blue);
              background: #eaf6ff;
              box-shadow: inset 3px 0 0 var(--thermos-blue);
            }

            .thermos-sidebar {
              background: rgba(255, 255, 255, 0.94);
              border-right: 1px solid var(--thermos-border);
              padding: 18px;
              height: 100vh;
              overflow-y: auto;
            }

            .thermos-main {
              min-width: 0;
              padding: 0 24px 24px 24px;
              overflow-x: hidden;
            }

            .thermos-header {
              min-height: 76px;
              display: flex;
              align-items: center;
              justify-content: space-between;
              gap: 18px;
              background: rgba(255, 255, 255, 0.90);
              border-bottom: 1px solid var(--thermos-border);
              margin: 0 -24px 18px -24px;
              padding: 14px 24px;
              backdrop-filter: blur(12px);
              position: sticky;
              top: 0;
              z-index: 8;
            }

            .thermos-title h1 {
              margin: 0;
              font-size: 24px;
              font-weight: 750;
              letter-spacing: 0;
            }

            .thermos-title p {
              margin: 3px 0 0 0;
              color: var(--thermos-muted);
              font-size: 13px;
            }

            .thermos-header-center {
              display: flex;
              align-items: center;
              gap: 14px;
              color: var(--thermos-muted);
              font-size: 13px;
            }

            .thermos-pill {
              display: inline-flex;
              align-items: center;
              gap: 7px;
              padding: 8px 12px;
              border-radius: 999px;
              border: 1px solid #bfe9d3;
              color: #087443;
              background: #ecfdf5;
              font-weight: 650;
            }

            .thermos-header-actions {
              display: flex;
              align-items: center;
              gap: 10px;
            }

            .thermos-primary-btn,
            .thermos-header-actions .btn-primary {
              background: linear-gradient(135deg, var(--thermos-blue), var(--thermos-blue-dark));
              border: none;
              color: #ffffff;
              box-shadow: 0 12px 22px rgba(8, 119, 185, 0.20);
              font-weight: 700;
            }

            .thermos-section {
              margin-bottom: 16px;
              border: 1px solid var(--thermos-border);
              border-radius: 10px;
              background: var(--thermos-panel);
              box-shadow: 0 2px 8px rgba(15, 23, 42, 0.04);
              overflow: hidden;
            }

            .thermos-section-head {
              display: flex;
              align-items: center;
              justify-content: space-between;
              padding: 12px 14px;
              border-bottom: 1px solid #eef2f7;
              font-weight: 750;
            }

            .thermos-section-number {
              display: inline-grid;
              place-items: center;
              width: 22px;
              height: 22px;
              border-radius: 999px;
              margin-right: 8px;
              color: #ffffff;
              background: var(--thermos-blue);
              font-size: 12px;
            }

            .thermos-section-body {
              padding: 12px;
            }

            .thermos-file-card {
              display: grid;
              grid-template-columns: 36px minmax(0, 1fr) auto;
              align-items: center;
              gap: 10px;
              padding: 9px;
              border: 1px solid var(--thermos-border);
              border-radius: 10px;
              background: #ffffff;
              margin-bottom: 10px;
            }

            .thermos-file-icon {
              width: 34px;
              height: 34px;
              display: grid;
              place-items: center;
              border-radius: 10px;
              color: var(--thermos-blue);
              background: #edf7ff;
            }

            .thermos-file-label {
              font-size: 12px;
              font-weight: 750;
              margin-bottom: 2px;
            }

            .thermos-file-body .form-group {
              margin-bottom: 0;
            }

            .thermos-file-body input.form-control {
              height: 25px;
              padding: 0;
              border: none;
              box-shadow: none;
              background: transparent;
              color: var(--thermos-muted);
              font-size: 12px;
              text-overflow: ellipsis;
            }

            .thermos-change-btn {
              border-color: var(--thermos-border);
              background: #f8fafc;
              color: var(--thermos-text);
              font-weight: 650;
            }

            .thermos-actions-grid {
              display: grid;
              grid-template-columns: 1fr 1fr;
              gap: 9px;
              margin-top: 12px;
            }

            .thermos-actions-grid .btn {
              width: 100%;
              min-height: 42px;
              font-weight: 700;
              text-align: left;
            }

            .thermos-tabs .nav-tabs {
              border-bottom: 1px solid var(--thermos-border);
              margin-bottom: 16px;
            }

            .thermos-tabs .nav-tabs > li > a {
              color: var(--thermos-muted);
              font-weight: 700;
              border: none;
              padding: 12px 18px;
            }

            .thermos-tabs .nav-tabs > li.active > a,
            .thermos-tabs .nav-tabs > li.active > a:focus,
            .thermos-tabs .nav-tabs > li.active > a:hover {
              color: var(--thermos-blue);
              border: none;
              border-bottom: 3px solid var(--thermos-blue);
              background: transparent;
            }

            .thermos-card-grid {
              display: grid;
              grid-template-columns: repeat(5, minmax(150px, 1fr));
              gap: 14px;
              margin-bottom: 14px;
            }

            .thermos-stat-card,
            .thermos-panel-card {
              border: 1px solid var(--thermos-border);
              border-radius: 10px;
              background: var(--thermos-panel);
              box-shadow: var(--thermos-shadow);
            }

            .thermos-stat-card {
              padding: 16px;
              min-height: 104px;
            }

            .thermos-stat-label {
              color: var(--thermos-muted);
              font-size: 12px;
              font-weight: 700;
            }

            .thermos-stat-value {
              font-size: 26px;
              font-weight: 800;
              margin-top: 7px;
            }

            .thermos-stat-sub {
              color: var(--thermos-muted);
              font-size: 12px;
              margin-top: 2px;
            }

            .thermos-panel-card {
              padding: 16px;
              margin-bottom: 14px;
            }

            .thermos-panel-title {
              margin: 0 0 12px 0;
              font-size: 15px;
              font-weight: 800;
            }

            .thermos-panel-title-row {
              display: flex;
              align-items: center;
              justify-content: space-between;
              gap: 12px;
              margin-bottom: 12px;
            }

            .thermos-panel-title-row .thermos-panel-title {
              margin-bottom: 0;
            }

            .thermos-export-btn.btn {
              border-color: var(--thermos-border);
              background: #ffffff;
              color: var(--thermos-blue);
              font-weight: 750;
            }

            .thermos-viz-grid {
              display: grid;
              grid-template-columns: minmax(0, 1fr) 280px;
              gap: 16px;
              align-items: start;
            }

            .thermos-viz-controls {
              display: grid;
              grid-template-columns: minmax(220px, 1fr) minmax(260px, 1.2fr);
              gap: 16px;
              margin-bottom: 12px;
            }

            .thermos-map-wrap {
              border: 1px solid var(--thermos-border);
              border-radius: 10px;
              background: #ffffff;
              padding: 12px;
              overflow: hidden;
            }

            .thermos-info-list {
              display: grid;
              gap: 10px;
              color: var(--thermos-muted);
              font-size: 13px;
            }

            .thermos-results-wrap {
              overflow-x: auto;
            }

            .thermos-results-wrap table {
              width: 100%;
              font-size: 13px;
            }

            .thermos-log-box pre,
            .thermos-status-box pre {
              border: 1px solid var(--thermos-border);
              background: #f8fafc;
              color: #0f172a;
              border-radius: 10px;
              min-height: 90px;
              white-space: pre-wrap;
            }

            .thermos-code-box {
              border: 1px solid var(--thermos-border);
              border-radius: 10px;
              background: #ffffff;
              margin-top: 10px;
              overflow: hidden;
            }

            .thermos-code-box > summary {
              cursor: pointer;
              padding: 10px 12px;
              font-weight: 750;
              color: var(--thermos-text);
              background: #f8fafc;
            }

            .thermos-code-box pre {
              margin: 0;
              border: none;
              border-top: 1px solid var(--thermos-border);
              border-radius: 0;
              background: #fbfdff;
            }

            .thermos-executed-scripts > summary {
              list-style: none;
              cursor: pointer;
              font-weight: 800;
              color: var(--thermos-text);
              display: flex;
              align-items: center;
              justify-content: space-between;
              padding: 2px 0 12px 0;
            }

            .thermos-executed-scripts > summary::-webkit-details-marker {
              display: none;
            }

            .thermos-executed-scripts > summary::after {
              content: '\\f077';
              font-family: FontAwesome;
              color: var(--thermos-blue-dark);
              font-weight: normal;
            }

            .thermos-executed-scripts:not([open]) > summary::after {
              content: '\\f078';
            }

            .thermos-task-panel-wrap {
              display: flex;
              justify-content: flex-end;
              margin-bottom: 0;
              pointer-events: none;
            }

            .thermos-task-panel {
              min-width: 360px;
              max-width: 620px;
              padding: 10px 14px 9px 14px;
              border-radius: 12px;
              background: rgba(255, 255, 255, 0.96);
              border: 1px solid #d8dee4;
              box-shadow: none;
              color: #1f2a37;
              pointer-events: auto;
            }

            .thermos-task-panel-top {
              display: flex;
              align-items: center;
              justify-content: space-between;
              gap: 12px;
              margin-bottom: 10px;
            }

            .thermos-task-panel-left {
              display: flex;
              align-items: center;
              gap: 10px;
              min-width: 0;
            }

            .thermos-task-panel-left .fa {
              font-size: 18px;
              color: #1f77b4;
            }

            .thermos-task-title {
              font-weight: 600;
              line-height: 1.2;
            }

            .thermos-task-subtitle {
              font-size: 12px;
              color: #667085;
              line-height: 1.2;
              margin-top: 2px;
            }

            .thermos-task-progress {
              position: fixed;
              width: 100%;
              height: 8px;
              position: relative;
              overflow: hidden;
              border-radius: 999px;
              background: #eaf0f6;
            }

            .thermos-task-progress-bar {
              position: absolute;
              left: -35%;
              top: 0;
              height: 100%;
              width: 35%;
              border-radius: 999px;
              background: linear-gradient(90deg, #1f77b4, #4db6ac);
              animation: thermos-progress-slide 1.25s ease-in-out infinite;
            }

            @keyframes thermos-progress-slide {
              0% {
                left: -35%;
              }
              100% {
                left: 100%;
              }
            }

            .thermos-top-actions {
              display: flex;
              justify-content: flex-end;
              margin-bottom: 12px;
            }

            .thermos-advanced-box {
              margin-top: 10px;
              margin-bottom: 14px;
              padding: 8px 10px 2px 10px;
              border: 1px solid #d8dee4;
              border-radius: 8px;
              background: #fafbfc;
            }

            .thermos-app {
              grid-template-columns: 470px minmax(0, 1fr);
              background: #ffffff;
              overflow-x: hidden;
            }

            .thermos-rail {
              display: none;
            }

            .thermos-rail {
              gap: 12px;
              padding: 18px 8px;
              background: #ffffff;
            }

            .thermos-nav-button.btn {
              width: 58px;
              height: 58px;
              padding: 6px 0;
              border: none;
              border-radius: 0;
              background: transparent;
              color: var(--thermos-muted);
              box-shadow: none;
              display: flex;
              flex-direction: column;
              align-items: center;
              justify-content: center;
              gap: 4px;
              font-size: 11px;
              font-weight: 700;
            }

            .thermos-nav-button.btn:hover,
            .thermos-nav-button.btn:focus {
              color: var(--thermos-blue);
              background: #f0f8ff;
              box-shadow: inset 3px 0 0 var(--thermos-blue);
            }

            .thermos-nav-button .fa {
              font-size: 18px;
            }

            .thermos-sidebar {
              background: #ffffff;
              width: 470px;
              min-width: 470px;
              max-width: 470px;
              scrollbar-gutter: stable;
              overflow-x: hidden;
            }

            .thermos-main {
              background: #ffffff;
            }

            .thermos-header {
              background: #ffffff;
              backdrop-filter: none;
              box-shadow: none;
            }

            .thermos-header-run {
              display: flex;
              align-items: center;
              gap: 12px;
              margin-left: auto;
            }

            .thermos-section {
              box-shadow: none;
              border-radius: 12px;
            }

            .thermos-section.thermos-collapsible > summary {
              list-style: none;
              cursor: pointer;
            }

            .thermos-section.thermos-collapsible > summary::-webkit-details-marker {
              display: none;
            }

            .thermos-section-head {
              background: #ffffff;
            }

            .thermos-param-grid {
              display: grid;
              grid-template-columns: 1fr;
              gap: 12px;
            }

            .thermos-param-grid .form-group {
              margin-bottom: 0;
            }

            .thermos-tools-box {
              margin-top: 14px;
              border: 1px dashed var(--thermos-border);
              border-radius: 10px;
              background: #fbfdff;
            }

            .thermos-tools-box > summary {
              padding: 10px 12px;
              cursor: pointer;
              color: var(--thermos-muted);
              font-weight: 750;
            }

            .thermos-tools-box .thermos-actions-grid {
              padding: 0 12px 12px 12px;
            }

            .thermos-tools-box .thermos-actions-grid .btn {
              white-space: normal;
            }

            .thermos-tabs .nav-tabs {
              display: none;
            }

            .thermos-card-grid {
              display: none;
            }

            .thermos-stat-card,
            .thermos-panel-card {
              box-shadow: 0 6px 18px rgba(15, 23, 42, 0.05);
            }
            "
          )
        )
        ,
        shiny::tags$script(
          shiny::HTML(
            "
            (function() {
              let thermosAlertInterval = null;

              function thermosSingleBeep() {
                try {
                  const AudioCtx = window.AudioContext || window.webkitAudioContext;
                  if (!AudioCtx) return;
                  const ctx = new AudioCtx();
                  const osc = ctx.createOscillator();
                  const gain = ctx.createGain();
                  osc.type = 'sine';
                  osc.frequency.value = 880;
                  gain.gain.value = 0.05;
                  osc.connect(gain);
                  gain.connect(ctx.destination);
                  osc.start();
                  setTimeout(function() {
                    osc.stop();
                    if (ctx.close) ctx.close();
                  }, 180);
                } catch (e) {
                  console.log('Thermos beep failed', e);
                }
              }

              Shiny.addCustomMessageHandler('thermosStartAlert', function(message) {
                if (thermosAlertInterval) {
                  clearInterval(thermosAlertInterval);
                }
                thermosSingleBeep();
                thermosAlertInterval = setInterval(thermosSingleBeep, 1400);
              });

              Shiny.addCustomMessageHandler('thermosStopAlert', function(message) {
                if (thermosAlertInterval) {
                  clearInterval(thermosAlertInterval);
                  thermosAlertInterval = null;
                }
              });

              Shiny.addCustomMessageHandler('thermosSetTaskRunning', function(message) {
                return;
              });
            })();
            "
          )
        )
      ),
      shiny::tags$div(
        class = "thermos-app",
        shiny::tags$aside(
          class = "thermos-sidebar",
          shiny::tags$details(
            class = "thermos-section thermos-collapsible",
            open = "open",
            shiny::tags$summary(
              class = "thermos-section-head",
              shiny::tags$span(shiny::tags$span(class = "thermos-section-number", "1"), "Input Data"),
              shiny::icon("chevron-up")
            ),
            shiny::tags$div(
              class = "thermos-section-body",
              path_input_ui("lc_path", "lc_path_browse", "Land cover (GeoPackage)"),
              path_input_ui("obs_path", "obs_path_browse", "Obstacles (GeoPackage)"),
              path_input_ui("dem_dir", "dem_dir_browse", "DEM"),
              path_input_ui("dsm_dir", "dsm_dir_browse", "DSM"),
              path_input_ui("met_xlsx", "met_xlsx_browse", "Meteorological Excel"),
              path_input_ui("output_root", "output_root_browse", "Parent output folder")
            )
          ),
          shiny::tags$details(
            class = "thermos-section thermos-collapsible",
            open = "open",
            shiny::tags$summary(
              class = "thermos-section-head",
              shiny::tags$span(shiny::tags$span(class = "thermos-section-number", "2"), "Parameters"),
              shiny::icon("chevron-up")
            ),
            shiny::tags$div(
              class = "thermos-section-body",
              shiny::selectInput(
                "plot_suffix",
                "Plot selection",
                choices = c(
                  "Auto-detect (recommended)" = "__auto__",
                  "All detected plots" = "__all__"
                ),
                selected = "__auto__"
              ),
              shiny::tags$div(
                class = "thermos-param-grid",
                shiny::numericInput("num_directions", "Number of directions", value = 72, min = 8),
                shiny::numericInput("max_distance", "Maximum search distance (m)", value = 30, min = 1),
                shiny::numericInput("observer_height", "Observer height for SVF (m)", value = 1.5, min = 0.1),
                shiny::numericInput("Met", "Metabolic rate (W/m2)", value = 80, min = 1),
                shiny::numericInput("Clo", "Clothing insulation (clo)", value = 0.9, min = 0),
                shiny::numericInput("ht", "Body height (m)", value = 1.75, min = 0.5),
                shiny::numericInput("mbody", "Body mass (kg)", value = 75, min = 1)
              )
            )
          ),
          shiny::tags$details(
            class = "thermos-section thermos-collapsible",
            shiny::tags$summary(
              class = "thermos-section-head",
              shiny::tags$span(shiny::tags$span(class = "thermos-section-number", "3"), "Step-by-step tools"),
              shiny::icon("chevron-up")
            ),
            shiny::tags$div(
              class = "thermos-section-body",
              shiny::tags$p(class = "help-block", "Use these only when you want to run one stage separately instead of the full analysis."),
              shiny::tags$div(
                class = "thermos-actions-grid",
                shiny::actionButton("check", "Check inputs", icon = shiny::icon("shield-alt")),
                shiny::actionButton("run_raster", "Run rasterization", icon = shiny::icon("table")),
                shiny::actionButton("run_svf", "Run SVF", icon = shiny::icon("sun")),
                shiny::actionButton("run_thermal", "Run thermal", icon = shiny::icon("thermometer-half"))
              )
            )
          )
        ),
        shiny::tags$main(
          class = "thermos-main",
          shiny::tags$header(
            class = "thermos-header",
            shiny::tags$div(
              class = "thermos-title",
              shiny::tags$h1("Thermos")
            ),
            shiny::tags$div(
              class = "thermos-header-run",
              shiny::uiOutput("task_panel_ui"),
              shiny::actionButton("run_all", "Run analysis", icon = shiny::icon("play"), class = "btn-primary thermos-primary-btn")
            )
          ),
          shiny::tags$div(
            class = "thermos-workspace",
            shiny::tags$div(
              class = "thermos-panel-card",
              shiny::tags$h3(class = "thermos-panel-title", "Raster Viewer"),
              shiny::tags$div(
                class = "thermos-viz-grid",
                shiny::tags$div(
                  shiny::tags$div(
                    class = "thermos-viz-controls",
                    shiny::selectInput("viz_category", "Output group", choices = c(
                      "Thermal results" = "results",
                      "SVF" = "svf",
                      "Rasterized layers" = "rasters_for_modeling"
                    )),
                    shiny::selectInput("viz_file", "Raster to preview", choices = character(0))
                  ),
                  shiny::textOutput("viz_info"),
                  shiny::tags$div(class = "thermos-map-wrap", shiny::plotOutput("viz_plot", height = "500px"))
                ),
                shiny::tags$div(
                  class = "thermos-panel-card",
                  shiny::tags$h3(class = "thermos-panel-title", "Raster info"),
                  shiny::uiOutput("raster_info")
                )
              )
            ),
            shiny::tags$div(
              class = "thermos-panel-card thermos-results-wrap",
              shiny::tags$div(
                class = "thermos-panel-title-row",
                shiny::tags$h3(class = "thermos-panel-title", "Results summary"),
                shiny::uiOutput("export_results_ui")
              ),
              shiny::tableOutput("results_overview")
            ),
            shiny::uiOutput("run_code_ui")
          )
        )
      )
    ),
    server = function(input, output, session) {
      cache_dir <- tools::R_user_dir("Thermos", "cache")
      browse_state_file <- file.path(cache_dir, "last_browse_dir.txt")
      input_paths_file <- file.path(cache_dir, "input_paths.rds")
      cached_input_ids <- c(
        "lc_path",
        "obs_path",
        "dem_dir",
        "dsm_dir",
        "met_xlsx",
        "output_root"
      )

      read_last_browse_dir <- function() {
        if (file.exists(browse_state_file)) {
          saved <- readLines(browse_state_file, warn = FALSE, n = 1)
          if (length(saved) > 0 && dir.exists(saved[1])) {
            return(normalizePath(saved[1], winslash = "/", mustWork = TRUE))
          }
        }
        getwd()
      }

      write_last_browse_dir <- function(path) {
        dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
        writeLines(path, browse_state_file, useBytes = TRUE)
      }

      read_cached_input_paths <- function() {
        if (!file.exists(input_paths_file)) {
          return(stats::setNames(as.list(rep("", length(cached_input_ids))), cached_input_ids))
        }

        saved <- tryCatch(readRDS(input_paths_file), error = function(e) NULL)
        if (!is.list(saved)) {
          saved <- list()
        }

        paths <- stats::setNames(as.list(rep("", length(cached_input_ids))), cached_input_ids)
        for (input_id in cached_input_ids) {
          path <- saved[[input_id]]
          if (!is.null(path) && length(path) == 1 && nzchar(path) &&
              (file.exists(path) || dir.exists(path))) {
            paths[[input_id]] <- normalizePath(path, winslash = "/", mustWork = TRUE)
          }
        }
        paths
      }

      write_cached_input_paths <- function(paths) {
        dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
        tryCatch(
          saveRDS(paths[cached_input_ids], input_paths_file),
          error = function(e) NULL
        )
        invisible(NULL)
      }

      status <- shiny::reactiveVal("Ready.")
      results <- shiny::reactiveVal(NULL)
      session_has_outputs <- shiny::reactiveVal(FALSE)
      current_task <- shiny::reactiveVal(NULL)
      last_browse_dir <- shiny::reactiveVal(read_last_browse_dir())
      cached_input_paths <- shiny::reactiveVal(read_cached_input_paths())
      last_run_scripts <- shiny::reactiveVal(NULL)

      session$onFlushed(function() {
        saved <- shiny::isolate(cached_input_paths())
        for (input_id in cached_input_ids) {
          if (nzchar(saved[[input_id]])) {
            shiny::updateTextInput(session, input_id, value = saved[[input_id]])
          }
        }
      }, once = TRUE)

      browse_default <- function(current_path = NULL) {
        if (!is.null(current_path) && nzchar(current_path)) {
          return(current_path)
        }
        last_browse_dir()
      }

      remember_browse_path <- function(path) {
        if (is.null(path) || !nzchar(path)) {
          return(invisible(NULL))
        }
        if (dir.exists(path)) {
          remembered <- normalizePath(path, winslash = "/", mustWork = TRUE)
          last_browse_dir(remembered)
          write_last_browse_dir(remembered)
        } else {
          parent <- dirname(path)
          if (dir.exists(parent)) {
            remembered <- normalizePath(parent, winslash = "/", mustWork = TRUE)
            last_browse_dir(remembered)
            write_last_browse_dir(remembered)
          }
        }
        invisible(NULL)
      }

      remember_input_path <- function(input_id, path) {
        if (!(input_id %in% cached_input_ids) || is.null(path) || !nzchar(path)) {
          return(invisible(NULL))
        }

        saved <- cached_input_paths()
        saved[[input_id]] <- normalizePath(path, winslash = "/", mustWork = FALSE)
        cached_input_paths(saved)
        write_cached_input_paths(saved)
        invisible(NULL)
      }

      r_literal <- function(x) {
        if (is.null(x) || length(x) == 0 || is.na(x)) {
          return("NULL")
        }
        if (is.character(x)) {
          return(encodeString(x, quote = "\""))
        }
        as.character(x)
      }

      deparse_function_source <- function(function_names) {
        ns <- asNamespace("Thermos")
        parts <- lapply(function_names, function(function_name) {
          if (!exists(function_name, envir = ns, inherits = FALSE)) {
            return(paste0("# Function not found in namespace: ", function_name))
          }
          paste(
            paste0(function_name, " <- "),
            paste(deparse(get(function_name, envir = ns)), collapse = "\n"),
            sep = ""
          )
        })
        paste(unlist(parts), collapse = "\n\n")
      }

      make_assignment_text <- function(args) {
        paste0(
          names(args),
          " <- ",
          vapply(args, r_literal, character(1)),
          collapse = "\n"
        )
      }

      make_call_text <- function(fun_name, args) {
        arg_lines <- paste0(
          "  ",
          names(args),
          " = ",
          vapply(args, r_literal, character(1)),
          collapse = ",\n"
        )
        paste0(fun_name, "(\n", arg_lines, "\n)")
      }

      make_full_script_text <- function(title, function_names, call_name, args) {
        paste(
          paste0("# ", title),
          "# Input values selected in the Thermos GUI",
          make_assignment_text(args),
          "",
          "# Full package code used for this stage",
          deparse_function_source(function_names),
          "",
          "# Stage command",
          make_call_text(call_name, args),
          sep = "\n"
        )
      }

      make_run_scripts <- function(args, include = c("raster", "svf", "thermal")) {
        scripts <- list()
        if ("raster" %in% include) {
          raster_args <- list(
            lc_path = args$lc_path,
            obs_path = args$obs_path,
            dem_dir = args$dem_dir,
            out_dir = args$lc_dir
          )
          scripts$Rasterization <- make_full_script_text(
            "Rasterization",
            c(
              "thermos_extract_suffix",
              "thermos_list_suffixes",
              "thermos_rasterize_attr",
              "thermos_make_const_rast",
              "thermos_fill_zero_in_mask",
              "thermos_first_match",
              "thermos_dir_must_exist",
              "thermos_rasterize_landcover"
            ),
            "thermos_rasterize_landcover",
            raster_args
          )
        }
        if ("svf" %in% include) {
          svf_args <- list(
            dem_dir = args$dem_dir,
            dsm_dir = args$dsm_dir,
            svf_dir = args$svf_dir,
            num_directions = args$num_directions,
            max_distance = args$max_distance,
            observer_height = args$observer_height
          )
          scripts$SVF <- make_full_script_text(
            "SVF calculation",
            c(
              "thermos_extract_suffix",
              "thermos_list_suffixes",
              "thermos_compute_svf_matrix",
              "thermos_dir_must_exist",
              "thermos_calculate_svf"
            ),
            "thermos_calculate_svf",
            svf_args
          )
        }
        if ("thermal" %in% include) {
          thermal_args <- list(
            dem_dir = args$dem_dir,
            dsm_dir = args$dsm_dir,
            svf_dir = args$svf_dir,
            lc_dir = args$lc_dir,
            out_dir = args$out_dir,
            plot_suffix = args$plot_suffix,
            met_xlsx = args$met_xlsx,
            Met = args$Met,
            Clo = args$Clo,
            ht = args$ht,
            mbody = args$mbody
          )
          scripts$`Thermal comfort` <- make_full_script_text(
            "Thermal comfort",
            c(
              "thermos_extract_suffix",
              "thermos_list_suffixes",
              "thermos_detect_common_suffixes",
              "thermos_detect_available_suffixes",
              "thermos_normalize_plot_suffix_input",
              "thermos_resolve_plot_suffixes",
              "thermos_resolve_plot_suffix",
              "thermos_rasterize_attr",
              "thermos_fill_zero_in_mask",
              "thermos_fill_default_in_mask",
              "thermos_make_const_rast",
              "thermos_first_match",
              "thermos_dir_must_exist",
              "thermos_compute_shadow",
              "thermos_compute_I0",
              "thermos_save_rast",
              "thermos_calc_pet",
              "thermos_calc_set",
              "thermos_lc_load",
              "thermos_lc_load_safe",
              "thermos_thermal_comfort_one_plot",
              "thermos_thermal_comfort"
            ),
            "thermos_thermal_comfort",
            thermal_args
          )
        }
        scripts
      }

      stop_completion_alert <- function() {
        session$sendCustomMessage("thermosStopAlert", list())
        shiny::removeModal()
      }

      show_completion_alert <- function(title, message) {
        session$sendCustomMessage("thermosStartAlert", list())
        shiny::showModal(
          shiny::modalDialog(
            title = title,
            shiny::tags$p(message),
            shiny::tags$p("Click OK to stop the alert."),
            footer = shiny::tagList(
              shiny::actionButton("dismiss_completion_alert", "OK", icon = shiny::icon("check"))
            ),
            easyClose = FALSE
          )
        )
      }

      shiny::observeEvent(input$dismiss_completion_alert, {
        stop_completion_alert()
      })

      set_task_running <- function(is_running) {
        session$sendCustomMessage(
          "thermosSetTaskRunning",
          list(running = isTRUE(is_running))
        )
      }

      ensure_output_dirs_exist <- function(fun_args) {
        candidate_dirs <- c(
          fun_args$output_root,
          fun_args$lc_dir,
          fun_args$svf_dir,
          fun_args$out_dir
        )
        candidate_dirs <- unique(candidate_dirs[nzchar(candidate_dirs)])

        for (dir_path in candidate_dirs) {
          dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
        }
      }

      start_background_task <- function(task_type,
                                        label,
                                        success_title,
                                        success_message,
                                        fun_name,
                                        fun_args) {
        if (!is.null(current_task())) {
          status("Another Thermos task is already running. Stop it first or wait until it finishes.")
          return(invisible(FALSE))
        }

        stop_completion_alert()
        ensure_output_dirs_exist(fun_args)
        task <- list(
          job = callr::r_bg(
            func = function(task_payload) {
              .libPaths(task_payload$libpaths)
              if (!is.null(task_payload$wd) && nzchar(task_payload$wd)) {
                setwd(task_payload$wd)
              }
              candidate_dirs <- c(
                task_payload$fun_args$output_root,
                task_payload$fun_args$lc_dir,
                task_payload$fun_args$svf_dir,
                task_payload$fun_args$out_dir
              )
              candidate_dirs <- unique(candidate_dirs[nzchar(candidate_dirs)])
              for (dir_path in candidate_dirs) {
                dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
              }
              suppressPackageStartupMessages(library(Thermos))
              do.call(
                get(task_payload$fun_name, envir = asNamespace("Thermos")),
                task_payload$fun_args
              )
            },
            args = list(
              task_payload = list(
                libpaths = .libPaths(),
                wd = getwd(),
                fun_name = fun_name,
                fun_args = fun_args
              )
            ),
            stdout = "|",
            stderr = "|",
            supervise = TRUE,
            wd = getwd()
          ),
          task_type = task_type,
          label = label,
          success_title = success_title,
          success_message = success_message
        )

        current_task(task)
        set_task_running(TRUE)
        status(paste(label, "running... Use 'Stop current run' to cancel."))
        invisible(TRUE)
      }

      finish_background_task <- function(task, result_obj) {
        if (identical(task$task_type, "pipeline")) {
          results(result_obj$thermal)
        } else {
          results(result_obj)
        }

        session_has_outputs(TRUE)
        refresh_visualization_choices()
        refresh_plot_suffix_choices(input$plot_suffix)
        status(paste(task$label, "complete."))
        show_completion_alert(task$success_title, task$success_message)
      }

      safe_browse <- function(expr) {
        tryCatch(
          expr,
          error = function(e) {
            status(paste("Browse error:", conditionMessage(e)))
            invisible(NULL)
          }
        )
      }

      build_output_dirs <- function(root_dir) {
        if (is.null(root_dir) || !nzchar(root_dir)) {
          root_dir <- getwd()
        }
        project_root <- file.path(root_dir, "Thermos_outputs")
        list(
          parent = root_dir,
          root = project_root,
          lc_dir = file.path(project_root, "rasters_for_modeling"),
          svf_dir = file.path(project_root, "svf"),
          out_dir = file.path(project_root, "results")
        )
      }

      list_raster_choices <- function(output_root, category) {
        if (!isTRUE(session_has_outputs())) {
          return(setNames(character(0), character(0)))
        }

        dirs <- build_output_dirs(output_root)
        folder <- switch(
          category,
          rasters_for_modeling = dirs$lc_dir,
          svf = dirs$svf_dir,
          results = dirs$out_dir,
          dirs$out_dir
        )

        if (!dir.exists(folder)) {
          return(setNames(character(0), character(0)))
        }

        files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
        if (length(files) == 0) {
          return(setNames(character(0), character(0)))
        }

        labels <- basename(files)
        stats <- file.info(files)
        ord <- order(stats$mtime, labels, decreasing = TRUE)
        files <- files[ord]
        labels <- labels[ord]
        setNames(files, labels)
      }

      reset_visualization_state <- function() {
        session_has_outputs(FALSE)
        shiny::updateSelectInput(
          session,
          "viz_file",
          choices = character(0),
          selected = character(0)
        )
      }

      shiny::observe({
        task <- current_task()
        if (is.null(task)) {
          return()
        }

        shiny::invalidateLater(1000, session)

        if (task$job$is_alive()) {
          return()
        }

        current_task(NULL)
        set_task_running(FALSE)

        exit_status <- task$job$get_exit_status()
        stderr_text <- paste(task$job$read_all_error_lines(), collapse = "\n")
        stdout_text <- paste(task$job$read_all_output_lines(), collapse = "\n")

        if (!identical(exit_status, 0L)) {
          err_text <- trimws(stderr_text)
          if (!nzchar(err_text)) {
            err_text <- trimws(stdout_text)
          }
          if (!nzchar(err_text)) {
            err_text <- "The background Thermos process ended unexpectedly."
          }
          status(paste(task$label, "error:", err_text))
          return()
        }

        result_obj <- tryCatch(
          task$job$get_result(),
          error = function(e) e
        )

        if (inherits(result_obj, "error")) {
          status(paste(task$label, "error:", conditionMessage(result_obj)))
          return()
        }

        finish_background_task(task, result_obj)
      })

      shiny::observeEvent(input$stop_task, {
        task <- current_task()
        if (is.null(task)) {
          status("No Thermos task is currently running.")
          return()
        }

        if (task$job$is_alive()) {
          task$job$kill()
        }

        current_task(NULL)
        set_task_running(FALSE)
        status("Current Thermos run was stopped by the user. Partial output files may remain in the output folder.")
      })

      session$onSessionEnded(function() {
        task <- shiny::isolate(current_task())
        if (!is.null(task) && task$job$is_alive()) {
          task$job$kill()
        }
      })

      refresh_visualization_choices <- function(preferred = NULL) {
        choices <- list_raster_choices(input$output_root, input$viz_category)
        selected <- preferred
        if (is.null(selected) || !nzchar(selected) || !(selected %in% unname(choices))) {
          selected <- if (length(choices) > 0) unname(choices)[1] else character(0)
        }
        shiny::updateSelectInput(
          session,
          "viz_file",
          choices = choices,
          selected = selected
        )
      }

      refresh_plot_suffix_choices <- function(preferred = NULL) {
        output_dirs <- build_output_dirs(input$output_root)
        detected <- thermos_detect_available_suffixes(
          dem_dir = input$dem_dir,
          dsm_dir = input$dsm_dir,
          svf_dir = output_dirs$svf_dir,
          lc_dir = output_dirs$lc_dir
        )

        choices <- c(
          "Auto-detect (recommended)" = "__auto__",
          "All detected plots" = "__all__"
        )

        if (length(detected) > 0) {
          detected_choices <- stats::setNames(detected, paste("Plot:", detected))
          choices <- c(choices, detected_choices)
        }

        selected <- preferred
        if (is.null(selected) || !nzchar(selected) || !(selected %in% unname(choices))) {
          selected <- "__auto__"
        }

        shiny::updateSelectInput(
          session,
          "plot_suffix",
          choices = choices,
          selected = selected
        )
      }

      shiny::observeEvent(input$lc_path_browse, {
        safe_browse({
          selected <- choose_file_path(
            "Select Land cover GeoPackage",
            "GeoPackage (*.gpkg)|*.gpkg|All files (*.*)|*.*",
            browse_default(input$lc_path)
          )
          if (!is.null(selected)) {
            remember_browse_path(selected)
            remember_input_path("lc_path", selected)
            shiny::updateTextInput(session, "lc_path", value = selected)
            status("Selected land-cover GeoPackage.")
          }
        })
      })

      shiny::observeEvent(input$obs_path_browse, {
        safe_browse({
          selected <- choose_file_path(
            "Select Obstacles GeoPackage",
            "GeoPackage (*.gpkg)|*.gpkg|All files (*.*)|*.*",
            browse_default(input$obs_path)
          )
          if (!is.null(selected)) {
            remember_browse_path(selected)
            remember_input_path("obs_path", selected)
            shiny::updateTextInput(session, "obs_path", value = selected)
            status("Selected obstacles GeoPackage.")
          }
        })
      })

      shiny::observeEvent(input$met_xlsx_browse, {
        safe_browse({
          selected <- choose_file_path(
            "Select Meteorological Excel",
            "Excel files (*.xlsx;*.xls)|*.xlsx;*.xls|All files (*.*)|*.*",
            browse_default(input$met_xlsx)
          )
          if (!is.null(selected)) {
            remember_browse_path(selected)
            remember_input_path("met_xlsx", selected)
            shiny::updateTextInput(session, "met_xlsx", value = selected)
            status("Selected meteorological Excel file.")
          }
        })
      })

      shiny::observeEvent(input$dem_dir_browse, {
        safe_browse({
          selected <- choose_directory_path("Select DEM folder", browse_default(input$dem_dir))
          if (!is.null(selected)) {
            remember_browse_path(selected)
            remember_input_path("dem_dir", selected)
            shiny::updateTextInput(session, "dem_dir", value = selected)
            status("Selected DEM folder.")
          }
        })
      })

      shiny::observeEvent(input$dsm_dir_browse, {
        safe_browse({
          selected <- choose_directory_path("Select DSM folder", browse_default(input$dsm_dir))
          if (!is.null(selected)) {
            remember_browse_path(selected)
            remember_input_path("dsm_dir", selected)
            shiny::updateTextInput(session, "dsm_dir", value = selected)
            status("Selected DSM folder.")
          }
        })
      })

      shiny::observeEvent(input$output_root_browse, {
        safe_browse({
          selected <- choose_directory_path("Select parent output folder", browse_default(input$output_root))
          if (!is.null(selected)) {
            remember_browse_path(selected)
            remember_input_path("output_root", selected)
            shiny::updateTextInput(session, "output_root", value = selected)
            results(NULL)
            last_run_scripts(NULL)
            reset_visualization_state()
            status("Selected parent output folder. Thermos will create a Thermos_outputs subfolder automatically.")
            refresh_plot_suffix_choices(input$plot_suffix)
          }
        })
      })

      shiny::observeEvent(input$viz_category, {
        refresh_visualization_choices()
      }, ignoreInit = FALSE)

      shiny::observeEvent(input$output_root, {
        results(NULL)
        last_run_scripts(NULL)
        reset_visualization_state()
        refresh_plot_suffix_choices(input$plot_suffix)
      }, ignoreInit = FALSE)

      shiny::observeEvent(input$dem_dir, {
        refresh_plot_suffix_choices(input$plot_suffix)
      }, ignoreInit = FALSE)

      shiny::observeEvent(input$dsm_dir, {
        refresh_plot_suffix_choices(input$plot_suffix)
      }, ignoreInit = FALSE)

      get_args <- function() {
        output_dirs <- build_output_dirs(input$output_root)
        list(
          lc_path = input$lc_path,
          obs_path = input$obs_path,
          dem_dir = input$dem_dir,
          dsm_dir = input$dsm_dir,
          lc_dir = output_dirs$lc_dir,
          svf_dir = output_dirs$svf_dir,
          out_dir = output_dirs$out_dir,
          parent_output_root = output_dirs$parent,
          output_root = output_dirs$root,
          plot_suffix = input$plot_suffix,
          met_xlsx = input$met_xlsx,
          num_directions = input$num_directions,
          max_distance = input$max_distance,
          observer_height = input$observer_height,
          Met = input$Met,
          Clo = input$Clo,
          ht = input$ht,
          mbody = input$mbody
        )
      }

      shiny::observeEvent(input$check, {
        args <- get_args()
        res <- thermos_check_inputs(
          lc_path = args$lc_path,
          obs_path = args$obs_path,
          dem_dir = args$dem_dir,
          dsm_dir = args$dsm_dir,
          met_xlsx = args$met_xlsx
        )
        detected_plots <- thermos_detect_available_suffixes(
          dem_dir = args$dem_dir,
          dsm_dir = args$dsm_dir,
          svf_dir = args$svf_dir,
          lc_dir = args$lc_dir
        )
        status(
          paste(
            paste(res$messages, collapse = "\n"),
            "",
            paste(
              "Detected plots:",
              if (length(detected_plots) > 0) paste(detected_plots, collapse = ", ") else "none yet"
            ),
            paste("Selected parent output folder:", args$parent_output_root),
            paste("Thermos project output folder:", args$output_root),
            paste("Rasterized outputs:", args$lc_dir),
            paste("SVF outputs:", args$svf_dir),
            paste("Thermal outputs:", args$out_dir),
            sep = "\n"
          )
        )
        results(as.data.frame(list(ok = res$ok)))
      })

      shiny::observeEvent(input$run_raster, {
        args <- get_args()
        last_run_scripts(make_run_scripts(args, include = "raster"))
        start_background_task(
          task_type = "raster",
          label = "Rasterization",
          success_title = "Rasterization complete",
          success_message = "Rasterized land-cover layers have been written to the output folder.",
          fun_name = "thermos_rasterize_landcover",
          fun_args = list(
            lc_path = args$lc_path,
            obs_path = args$obs_path,
            dem_dir = args$dem_dir,
            out_dir = args$lc_dir
          )
        )
      })

      shiny::observeEvent(input$run_svf, {
        args <- get_args()
        last_run_scripts(make_run_scripts(args, include = "svf"))
        start_background_task(
          task_type = "svf",
          label = "SVF calculation",
          success_title = "SVF calculation complete",
          success_message = "The SVF raster and summary file are ready.",
          fun_name = "thermos_calculate_svf",
          fun_args = list(
            dem_dir = args$dem_dir,
            dsm_dir = args$dsm_dir,
            svf_dir = args$svf_dir,
            num_directions = args$num_directions,
            max_distance = args$max_distance,
            observer_height = args$observer_height
          )
        )
      })

      shiny::observeEvent(input$run_thermal, {
        args <- get_args()
        last_run_scripts(make_run_scripts(args, include = "thermal"))
        start_background_task(
          task_type = "thermal",
          label = "Thermal comfort",
          success_title = "Thermal comfort complete",
          success_message = "Thermal rasters and summary outputs are ready.",
          fun_name = "thermos_thermal_comfort",
          fun_args = list(
            dem_dir = args$dem_dir,
            dsm_dir = args$dsm_dir,
            svf_dir = args$svf_dir,
            lc_dir = args$lc_dir,
            out_dir = args$out_dir,
            plot_suffix = args$plot_suffix,
            met_xlsx = args$met_xlsx,
            Met = args$Met,
            Clo = args$Clo,
            ht = args$ht,
            mbody = args$mbody
          )
        )
      })

      shiny::observeEvent(input$run_all, {
        args <- get_args()
        last_run_scripts(make_run_scripts(args, include = c("raster", "svf", "thermal")))
        start_background_task(
          task_type = "pipeline",
          label = "Full pipeline",
          success_title = "Full pipeline complete",
          success_message = "All outputs have been generated.",
          fun_name = "thermos_run_pipeline",
          fun_args = list(
            lc_path = args$lc_path,
            obs_path = args$obs_path,
            dem_dir = args$dem_dir,
            dsm_dir = args$dsm_dir,
            lc_dir = args$lc_dir,
            svf_dir = args$svf_dir,
            out_dir = args$out_dir,
            plot_suffix = args$plot_suffix,
            met_xlsx = args$met_xlsx,
            num_directions = args$num_directions,
            max_distance = args$max_distance,
            observer_height = args$observer_height,
            Met = args$Met,
            Clo = args$Clo,
            ht = args$ht,
            mbody = args$mbody
          )
        )
      })

      output$status <- shiny::renderText(status())
      output$output_paths <- shiny::renderText({
        dirs <- build_output_dirs(input$output_root)
        paste(
          paste("Parent output folder:", dirs$parent),
          paste("Thermos output folder:", dirs$root),
          paste("Rasterized layers:", dirs$lc_dir),
          paste("SVF:", dirs$svf_dir),
          paste("Thermal results:", dirs$out_dir),
          sep = "\n"
        )
      })
      output$task_panel_ui <- shiny::renderUI({
        task <- current_task()
        if (is.null(task)) {
          return(NULL)
        }

        shiny::tags$div(
          class = "thermos-task-panel-wrap",
          shiny::tags$div(
            class = "thermos-task-panel",
            shiny::tags$div(
              class = "thermos-task-panel-top",
              shiny::tags$div(
                class = "thermos-task-panel-left",
                shiny::icon("gear", class = "fa-spin"),
                shiny::tags$div(
                  shiny::tags$div(class = "thermos-task-title", paste(task$label, "running"))
                )
              ),
              shiny::actionButton("stop_task", "Stop current run", class = "btn-danger btn-sm")
            ),
            shiny::tags$div(
              class = "thermos-task-progress",
              shiny::tags$div(class = "thermos-task-progress-bar")
            )
          )
        )
      })
      output$results <- shiny::renderTable(results())
      output$results_overview <- shiny::renderTable(results(), striped = TRUE, hover = TRUE, spacing = "s")
      output$results_table <- shiny::renderTable(results(), striped = TRUE, hover = TRUE, spacing = "s")
      output$export_results_ui <- shiny::renderUI({
        if (!isTRUE(session_has_outputs()) || is.null(results())) {
          return(NULL)
        }
        shiny::downloadButton("export_results_csv", "Export CSV", class = "thermos-export-btn")
      })
      output$export_results_csv <- shiny::downloadHandler(
        filename = function() {
          paste0("thermos_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
          res <- results()
          if (is.null(res)) {
            res <- data.frame(message = "No results available")
          }
          utils::write.table(
            res,
            file,
            sep = ";",
            dec = ".",
            row.names = FALSE,
            quote = TRUE,
            fileEncoding = "UTF-8"
          )
        }
      )
      output$run_code_ui <- shiny::renderUI({
        scripts <- last_run_scripts()
        if (!isTRUE(session_has_outputs()) || is.null(scripts) || length(scripts) == 0) {
          return(NULL)
        }

        code_boxes <- lapply(names(scripts), function(title) {
          shiny::tags$details(
            class = "thermos-code-box",
            shiny::tags$summary(title),
            shiny::tags$pre(scripts[[title]])
          )
        })

        shiny::tags$details(
          class = "thermos-panel-card thermos-log-box thermos-executed-scripts",
          shiny::tags$summary("Executed scripts"),
          shiny::tags$div(
            class = "thermos-panel-title-row",
            shiny::tags$h3(class = "thermos-panel-title", "Full executed R scripts"),
            shiny::downloadButton("export_run_code_txt", "Export TXT", class = "thermos-export-btn")
          ),
          code_boxes
        )
      })
      output$export_run_code_txt <- shiny::downloadHandler(
        filename = function() {
          paste0("thermos_run_code_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
        },
        content = function(file) {
          scripts <- last_run_scripts()
          if (is.null(scripts) || length(scripts) == 0) {
            scripts <- list(`No run code available` = "No completed analysis is available yet.")
          }
          lines <- unlist(lapply(names(scripts), function(title) {
            c(
              paste0("## ", title),
              scripts[[title]],
              ""
            )
          }), use.names = FALSE)
          writeLines(lines, file, useBytes = TRUE)
        }
      )
      output$viz_info <- shiny::renderText({
        viz_file <- input$viz_file
        if (is.null(viz_file) || !nzchar(viz_file)) {
          return("No raster available yet for the selected output group.")
        }
        paste("Previewing:", basename(viz_file))
      })
      output$viz_plot <- shiny::renderPlot({
        viz_file <- input$viz_file
        shiny::req(!is.null(viz_file), nzchar(viz_file))
        shiny::req(file.exists(viz_file))
        r <- terra::rast(viz_file)
        nm <- basename(viz_file)

        if (grepl("^UTCI_class_", nm)) {
          terra::plot(r, type = "classes", main = nm)
        } else if (grepl("^landcover_", nm)) {
          terra::plot(r, type = "classes", main = nm)
        } else if (grepl("^svf_", nm)) {
          terra::plot(r, col = hcl.colors(24, "YlGnBu"), main = nm)
        } else if (grepl("^(PET|mPET|PMV|SET|UTCI|Tmrt)_", nm)) {
          terra::plot(r, col = hcl.colors(32, "Inferno"), main = nm)
        } else {
          terra::plot(r, col = hcl.colors(24, "Viridis"), main = nm)
        }
      })
      output$raster_info <- shiny::renderUI({
        viz_file <- input$viz_file
        if (is.null(viz_file) || !nzchar(viz_file) || !file.exists(viz_file)) {
          return(shiny::tags$div(class = "thermos-info-list", shiny::tags$span("No raster selected.")))
        }
        r <- terra::rast(viz_file)
        vals <- terra::global(r, c("min", "mean", "max"), na.rm = TRUE)
        ext_r <- terra::ext(r)
        crs_info <- tryCatch(terra::crs(r, describe = TRUE), error = function(e) NULL)
        crs_code <- "unknown"
        if (!is.null(crs_info) && "code" %in% names(crs_info) && length(crs_info$code) > 0 && nzchar(crs_info$code)) {
          crs_code <- crs_info$code
        }
        shiny::tags$div(
          class = "thermos-info-list",
          shiny::tags$span(paste("File:", basename(viz_file))),
          shiny::tags$span(paste("Resolution:", paste(round(terra::res(r), 3), collapse = " x "))),
          shiny::tags$span(paste("CRS:", crs_code)),
          shiny::tags$span(paste("Extent X:", paste(round(c(terra::xmin(ext_r), terra::xmax(ext_r)), 2), collapse = " to "))),
          shiny::tags$span(paste("Extent Y:", paste(round(c(terra::ymin(ext_r), terra::ymax(ext_r)), 2), collapse = " to "))),
          shiny::tags$span(paste("Min / Mean / Max:", paste(round(as.numeric(vals[1, ]), 2), collapse = " / ")))
        )
      })
      output$viz_file_full_ui <- shiny::renderUI({
        choices <- list_raster_choices(input$output_root, input$viz_category_full)
        shiny::selectInput("viz_file_full", "Raster to preview", choices = choices)
      })
      output$viz_plot_full <- shiny::renderPlot({
        viz_file <- input$viz_file_full
        shiny::req(!is.null(viz_file), nzchar(viz_file))
        shiny::req(file.exists(viz_file))
        r <- terra::rast(viz_file)
        nm <- basename(viz_file)

        if (grepl("^UTCI_class_", nm)) {
          terra::plot(r, type = "classes", main = nm)
        } else if (grepl("^landcover_", nm)) {
          terra::plot(r, type = "classes", main = nm)
        } else if (grepl("^svf_", nm)) {
          terra::plot(r, col = hcl.colors(24, "YlGnBu"), main = nm)
        } else if (grepl("^(PET|mPET|PMV|SET|UTCI|Tmrt)_", nm)) {
          terra::plot(r, col = hcl.colors(32, "Inferno"), main = nm)
        } else {
          terra::plot(r, col = hcl.colors(24, "Viridis"), main = nm)
        }
      })
    }
  )

  shiny::runApp(
    app,
    launch.browser = function(url) {
      utils::browseURL(sub("127.0.0.1", "localhost", url, fixed = TRUE))
    }
  )
  invisible(app)
}
