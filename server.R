library(ggplot2)
library(flowCore)
library(shinyFiles)
library(DT)
library(AutoSpectral)
library(shiny)
library(shinydashboard)
library(openxlsx)
library(jpeg)
library(grid)

# =========================================================
# SERVER
# =========================================================

function(input, output, session) {

  # ===================== REACTIVE VALUES =====================
  tab_rv <- reactiveVal(NULL)
  asp_rv <- reactiveVal(NULL)
  ff_rv  <- reactiveVal(NULL)

  control.def.file_rv <- reactiveVal(NULL)
  raw.def.file_rv     <- reactiveVal(NULL)

  flow.control_rv <- reactiveVal(NULL)
  spectra_rv      <- reactiveVal(NULL)

  unmixed_files_rv <- reactiveVal(character(0))
  raw_files_rv     <- reactiveVal(character(0))
  prepared_tab_rv  <- reactiveVal(NULL)   # control table spectra were computed from
  ff_raw_rv        <- reactiveVal(NULL)
  analysis_status  <- reactiveVal("")
  warning_rv       <- reactiveVal("")     # control check / preparation (Control preparation tab)
  error_rv         <- reactiveVal("")
  unmix_error_rv   <- reactiveVal("")     # unmixing (Analysis tab)
  fluor_status_rv  <- reactiveVal(NULL)
  check_rv         <- reactiveVal(NULL)     # last control check: cells to change, general problems, warnings
  manual_gates_rv   <- reactiveVal(list())   # gate name -> list(x, y, i) drawn by the user
  gate_points_rv    <- reactiveVal(NULL)     # corners clicked for the gate being drawn
  prepared_gates_rv <- reactiveVal(NULL)     # manual gates spectra were computed with
  prep_run_rv       <- reactiveVal(0)        # bumped after each preparation: figures on disk changed
  fallback_rv       <- reactiveVal(character())  # controls the last preparation moved to their own gate group
  has_tab_rv        <- reactiveVal(FALSE)    # a control table exists (see TABLE OUTPUT)
  table_cols_rv     <- reactiveVal(NULL)     # columns of the control table shown in Parameters

  # ===================== PATHS & DIRECTORIES =================
  session_id  <- session$token
  session_dir <- file.path(tempdir(), session_id)
  dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)

  unmixed_dir     <- file.path(session_dir, "unmixed")
  control_tmp_dir <- file.path(session_dir, "autospectral_controls")
  raw_tmp_dir     <- file.path(session_dir, "autospectral_raw")
  control_csv     <- file.path(session_dir, "fcs_control_file.csv")

  figure_dirs <- c(
    "figure_scatter",
    "figure_similarity_heatmap",
    "figure_spectra",
    "figure_gate",
    "figure_spectral_ribbon",
    "figure_clean_controls",
    "table_spectra"
  )

  # AutoSpectral writes its figures/tables in the working directory;
  # the working dir is only switched around the calls that need it.
  with_session_wd <- function(expr) {
    oldwd <- setwd(session_dir)
    on.exit(setwd(oldwd), add = TRUE)
    force(expr)
  }

  session$onSessionEnded(function() {
    unlink(session_dir, recursive = TRUE, force = TRUE)
  })

  # Unmixed files are only valid for the spectra they were unmixed with.
  clear_unmixed <- function() {
    unlink(list.files(unmixed_dir, pattern = "\\.fcs$", ignore.case = TRUE, full.names = TRUE))
    unmixed_files_rv(character(0))
    ff_rv(NULL)
    analysis_status("")
  }

  # Figures and tables of the previous preparation (their timestamped names would pile up).
  # The automatic gate definitions are kept by default: the manual gate plot overlays them.
  clear_prep_figures <- function(keep_gate_definitions = TRUE) {
    files <- list.files(file.path(session_dir, figure_dirs), full.names = TRUE, recursive = TRUE)
    if (keep_gate_definitions) files <- files[basename(dirname(files)) != "gate_definitions"]
    unlink(files)
  }

  # ===================== MACHINE SELECTION ====================
  observeEvent(input$machine, {
    asp_rv(
      get.autospectral.param(
        cytometer = input$machine,
        figures   = TRUE
      )
    )
  })

  # Reference control table: columns shown in the Parameters tab and the
  # fluorophore -> marker correspondence used to prefill the markers.
  marker_ref <- tryCatch(
    read.csv("marker_reference.csv", stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  # large.gate, gate.name and gate.define stay in the table sent to AutoSpectral but are not shown
  # (the gate groups have their own step and a read-only column)
  hidden_cols <- c("", "X", "large.gate", "gate.name", "gate.define")
  table_cols <- if (!is.null(marker_ref)) setdiff(names(marker_ref), hidden_cols) else NULL
  shown_cols <- function(tab) {
    intersect(table_cols %||% setdiff(names(tab), hidden_cols), names(tab))
  }
  norm_key <- function(x) gsub("[^a-z0-9]", "", tolower(x))

  fill_markers <- function(tab) {
    if (is.null(marker_ref) || !all(c("fluorophore", "marker") %in% names(tab))) return(tab)
    ref <- marker_ref[!is.na(marker_ref$marker) & nzchar(marker_ref$marker), ]
    m <- ref$marker[match(norm_key(tab$fluorophore), norm_key(ref$fluorophore))]
    empty <- is.na(tab$marker) | !nzchar(trimws(as.character(tab$marker)))
    tab$marker[empty & !is.na(m)] <- m[empty & !is.na(m)]
    tab
  }

  # a value create.control.file could not find ("No match") or left blank
  blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x))) | x %in% "No match"

  # A control create.control.file could not name takes the name of its file, without the
  # " (Beads).fcs" / " (Cells).fcs" ending (or ".fcs"), before its marker is looked up in
  # the reference. The channel is left blank: the control check asks for it.
  name_from_files <- function(tab) {
    if (!all(c("filename", "fluorophore") %in% names(tab))) return(tab)
    fl <- as.character(tab$fluorophore)
    unnamed <- blank(fl)
    fl[unnamed] <- trimws(sub("\\s*(\\((beads|cells)\\))?\\s*\\.fcs$", "", tab$filename[unnamed], ignore.case = TRUE))
    tab$fluorophore <- fl
    tab
  }

  # What is still blank in the fluorophore gets a placeholder from its row: fluo<row> (a file
  # named only "(Beads).fcs"). A marker the reference does not know stays empty.
  fill_names <- function(tab) {
    rows <- seq_len(nrow(tab))
    if ("fluorophore" %in% names(tab)) {
      fl <- as.character(tab$fluorophore)
      fl[blank(fl)] <- paste0("fluo", rows[blank(fl)])
      tab$fluorophore <- fl
    }
    if ("marker" %in% names(tab)) {
      mk <- as.character(tab$marker)
      mk[blank(mk)] <- ""
      tab$marker <- mk
    }
    tab
  }

  # Beads without a universal negative get the default beads negative
  # (only when that file was uploaded); filled values are never overwritten.
  default_beads_negative <- "billes (Beads).fcs"

  fill_negatives <- function(tab) {
    if (!all(c("filename", "control.type", "universal.negative") %in% names(tab)) ||
        !default_beads_negative %in% tab$filename) return(tab)
    un <- as.character(tab$universal.negative)
    empty <- tab$control.type %in% "beads" & (is.na(un) | !nzchar(trimws(un)))
    un[empty] <- default_beads_negative
    tab$universal.negative <- un
    tab
  }

  # Fluorophore names of the files as uploaded: a file that stops being a universal negative
  # takes its name back. neg_names holds the name given to the current negatives (Negative
  # for the beads, AF for the cells), to restore only a name the user did not edit since.
  orig_names <- reactiveVal(character())
  neg_names  <- reactiveVal(character())
  remember_names <- function(tab) {
    if (all(c("filename", "fluorophore") %in% names(tab)))
      orig_names(setNames(as.character(tab$fluorophore), tab$filename))
    neg_names(character())
    tab
  }

  # label: "Negative" or "AF"; file: the chosen universal negative ("None" or NULL for none).
  # The negative of the other kind keeps its name; the previous one of this kind is restored.
  set_negative_name <- function(tab, kind, file) {
    if (!all(c("filename", "fluorophore") %in% names(tab))) return(tab)
    label <- c(beads = "Negative", cells = "AF")[[kind]]
    nn <- neg_names()
    prev <- names(nn)[nn == label]
    for (f in prev) {
      i <- tab$filename == f
      if (any(i) && f %in% names(orig_names()) && all(tab$fluorophore[i] %in% label))
        tab$fluorophore[i] <- orig_names()[[f]]
    }
    nn <- nn[!names(nn) %in% prev]
    if (!is.null(file) && !file %in% "None" && file %in% tab$filename) {
      tab$fluorophore[tab$filename == file] <- label
      nn[file] <- label
    }
    neg_names(nn)
    tab
  }

  # TRUE/FALSE columns, typed in the table or read from a spreadsheet as text
  flag_cols <- c("large.gate", "is.viability")
  parse_flag <- function(x) {
    v <- toupper(trimws(as.character(x)))
    ifelse(v %in% c("TRUE", "T", "1", "YES", "Y", "OUI", "VRAI"), TRUE,
           ifelse(is.na(v) | v %in% c("FALSE", "F", "0", "NO", "N", "NON", "FAUX", "", "NA"), FALSE, NA))
  }

  # flag columns as logicals, empty = FALSE as AutoSpectral reads them (an edited cell
  # would otherwise turn the whole column into text)
  normalize_flags <- function(tab) {
    for (col in intersect(flag_cols, names(tab))) {
      v <- parse_flag(tab[[col]])
      v[is.na(v)] <- FALSE
      tab[[col]] <- v
    }
    tab
  }

  # Copy uploaded files to a folder, restoring their original names
  # (shiny stores uploads under generic names such as 0.fcs).
  copy_uploads <- function(upload, dest) {
    unlink(dest, recursive = TRUE, force = TRUE)
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    file.copy(
      from      = upload$datapath,
      to        = file.path(dest, upload$name),
      overwrite = TRUE
    )
    dest
  }

  # Marker names live in $PnS (desc) of the unmixed files; fall back to the channel.
  marker_label <- function(ff, ch) {
    pd <- flowCore::pData(flowCore::parameters(ff))
    d  <- as.character(pd$desc[match(ch, pd$name)])
    ifelse(is.na(d) | !nzchar(d) | d == ch, ch, d)
  }

  # Axis choices shown as "marker (channel)"; values stay channel names.
  axis_choices <- function(ff) {
    cn <- colnames(ff@exprs)
    lab <- marker_label(ff, cn)
    setNames(cn, ifelse(lab == cn, cn, paste0(lab, " (", cn, ")")))
  }

  # all visualisations show at most this many randomly drawn cells
  PLOT_N <- 10000

  # random downsample of a flowFrame (everything is kept when it has fewer events)
  subsample <- function(ff, n) {
    if (nrow(ff) > n) ff <- ff[sort(sample.int(nrow(ff), n)), ]
    ff
  }

  # Biplot with marker labels, drawn once and never saved to disk.
  draw_biplot <- function(ff, x, y, label_ff = ff, xlab = NULL, ylab = NULL) {
    p <- create.biplot(
      ff@exprs,
      x.dim = x, y.dim = y,
      asp_rv(),
      x.lab = xlab %||% marker_label(label_ff, x),
      y.lab = ylab %||% marker_label(label_ff, y),
      save = FALSE
    )
    if (inherits(p, "ggplot")) print(p)
    invisible(p)
  }

  # ===========================================================
  # 1. UPLOAD TAB
  # ===========================================================

  observeEvent(input$control, {

    req(input$control, asp_rv())

    copy_uploads(input$control, control_tmp_dir)
    # new controls: nothing prepared, checked, gated or unmixed from the previous ones is kept
    prepared_tab_rv(NULL)
    prepared_gates_rv(NULL)
    spectra_rv(NULL)
    flow.control_rv(NULL)
    fluor_status_rv(NULL)
    check_rv(NULL)
    warning_rv("")
    error_rv("")
    manual_gates_rv(list())
    gate_points_rv(NULL)
    fallback_rv(character())
    clear_unmixed()
    clear_prep_figures(keep_gate_definitions = FALSE)
    prep_run_rv(prep_run_rv() + 1)
    if (file.exists(control_csv)) file.remove(control_csv)

    tryCatch({

      create.control.file(
        control_tmp_dir,
        asp_rv(),
        output.dir = session_dir
      )

      control.def.file_rv(control_tmp_dir)

      tab <- read.csv(control_csv, stringsAsFactors = FALSE, check.names = FALSE)

      # drop the row-index column created by write.csv
      if (ncol(tab) > 0 && names(tab)[1] %in% c("X", "", "row.names")) {
        tab <- tab[, -1, drop = FALSE]
      }

      cols_to_fix <- setdiff(names(tab), c("fluorophore", "marker", "universal.negative"))
      tab[cols_to_fix] <- lapply(tab[cols_to_fix], function(x) {
        x[is.na(x)] <- FALSE
        x
      })

      tab_rv(fill_names(fill_negatives(fill_markers(remember_names(fill_names(name_from_files(normalize_flags(tab))))))))

    }, error = function(e) {
      showNotification(
        paste("Control file creation failed:", conditionMessage(e)),
        type = "error", duration = NULL
      )
    })
  })

  # upload raw
  observeEvent(input$raw, {
    raw.def.file_rv(copy_uploads(input$raw, raw_tmp_dir))
    files <- list.files(raw_tmp_dir, pattern = "\\.fcs$", ignore.case = TRUE)
    raw_files_rv(files)
  })

  output$raw_list <- renderUI({
    req(length(raw_files_rv()) > 0)
    HTML(paste0("<b>", length(raw_files_rv()), " raw file(s) loaded</b> (all use the same controls):<br>",
                paste(raw_files_rv(), collapse = "<br>")))
  })

  # ===========================================================
  # 2. PARAMETERS TAB
  # ===========================================================
  observeEvent(input$csv, {

    req(input$csv)

    ext <- tolower(tools::file_ext(input$csv$name))

    if (ext %in% c("xls", "xlsx")) {
      table <- tryCatch({
        if (ext == "xlsx") {
          openxlsx::read.xlsx(input$csv$datapath, check.names = FALSE)
        } else if (requireNamespace("readxl", quietly = TRUE)) {
          as.data.frame(readxl::read_excel(input$csv$datapath), stringsAsFactors = FALSE)
        } else stop("legacy .xls needs the 'readxl' package; save the table as .xlsx or .csv")
      }, error = function(e) {
        showNotification(paste("Table import failed:", conditionMessage(e)),
                         type = "error", duration = NULL)
        NULL
      })
      req(table)
    } else {
      first_line <- readLines(
        input$csv$datapath,
        n = 1,
        warn = FALSE
      )

      sep <- if (
        length(strsplit(first_line, ";")[[1]]) >
        length(strsplit(first_line, ",")[[1]])
      ) ";" else ","

      table <- read.csv(
        input$csv$datapath,
        sep = sep,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    # drop the index column created by write.csv
    if (
      ncol(table) > 0 &&
      names(table)[1] %in% c("X", "", "row.names")
    ) {
      table <- table[, -1, drop = FALSE]
    }

    message("Table imported / dim: ", paste(dim(table), collapse = " x "))

    # spreadsheets may hold the TRUE/FALSE columns as text
    tab_rv(fill_negatives(fill_markers(remember_names(normalize_flags(table)))))
  })

  observeEvent(input$HELP_TABLE, {
    showModal(modalDialog(
      title = "Control table columns",
      HTML(paste0(
        "<p><b>filename</b>: column with controls filenames, must be differents names.</p>",
        "<p><b>fluorophore</b>: column with fluorophores names. A control not recognised from its ",
        "filename is named after its file, without <i>(Beads).fcs</i> or <i>(Cells).fcs</i>.</p>",
        "<p><b>marker</b>: column with each marker corresponding to one fluorophore ",
        "(except for beads without colors and unstained cells, leave it blank). ",
        "A marker not recognised is left empty. ",
        "<p><b>channel</b>: CRITICAL : column with the corresponding channel.",
        "(except for beads without colors and unstained cells, leave it blank). ",
        "You can look on FluoroFinder, Cytek Cloud or BD Research Cloud.</p>",
        "<p><b>control.type</b>: whether the control is a bead or a cell.</p>",
        "<p><b>universal.negative</b>: name of the file responsible for the autofluorescence of this control.</p>",
        "<p><b>is.viability</b>: set <i>true</i> if it is a viability file (<i>false</i> by default).</p>",
        "<p><b>gate group</b>: control group for the automatic gating.</p>"
      )),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  observeEvent(input$HELP_METHOD, {
    showModal(modalDialog(
      title = "Unmixing methods",
      HTML(paste0(
        "<p><b>OLS</b>: ordinary least squares, fast, treats all detectors as equally noisy.</p>",
        "<p><b>WLS</b>: weighted least squares, down-weights the noisiest detectors.</p>",
        "<p><b>AutoSpectral</b>: per-cell unmixing that also fits autofluorescence spectra and spectral variants from an unstained sample.</p>",
        "<p><b>FastPoisson</b>: fast iterative maximum-likelihood unmixing assuming Poisson photon noise.</p>"
      )),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  observeEvent(input$HELP_GATE, {
    showModal(modalDialog(
      title = "Manual control gate",
      HTML(paste0(
        "<p>Click on the plot to place the corners of the new gate, then save it.</p>",
        "<p><b>Dashed black</b>: automatic gate of the last preparation. <b>Red</b>: saved manual gate.</p>",
        "<p>Apply the manual gates to gate all the controls of these gates again ",
        "and update the control plots and spectra above.</p>"
      )),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  observeEvent(input$HELP_GROUPS, {
    showModal(modalDialog(
      title = "Gate groups",
      HTML(paste0(
        "<p>The controls of a group share one automatic FSC/SSC gate, and their negative is gated with it.</p>",
        "<p>A control that falls back during the gating is moved to a group of its own.</p>",
        "<p>When the gate of a group keeps none of the events of its negative (controls that lie elsewhere ",
        "in FSC/SSC than the negative), its controls use the negative as gated in another group.</p>",
        "<p>The groups are applied at the next preparation (<i>Apply manual gates</i> or <i>Prepare controls</i>).</p>"
      )),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  # most frequent non-empty universal negative for a control type
  default_negative <- function(tab, type) {
    v <- tab$universal.negative[tab$control.type == type]
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v) == 0) "None" else names(sort(table(v), decreasing = TRUE))[1]
  }

  observeEvent(tab_rv(), {

    tab <- tab_rv()
    req(tab)

    choices <- c("None", tab$filename)

    keep_or_default <- function(current, type) {
      if (!is.null(current) && current %in% choices && nzchar(current)) current
      else if (type == "beads" && "billes (Beads).fcs" %in% choices) "billes (Beads).fcs"
      else default_negative(tab, type)
    }

    updateSelectInput(session, "negative_beads",
                      choices = choices,
                      selected = keep_or_default(isolate(input$negative_beads), "beads"))

    updateSelectInput(session, "negative_cells",
                      choices = choices,
                      selected = keep_or_default(isolate(input$negative_cells), "cells"))
  })

  observeEvent(input$negative_beads, {
    req(tab_rv(), input$negative_beads)

    tab <- tab_rv()

    if (input$negative_beads == "None") {
      tab$universal.negative[tab$control.type == "beads"] <- NA
    } else {
      tab$universal.negative[tab$control.type == "beads"] <- input$negative_beads
    }
    tab <- set_negative_name(tab, "beads", input$negative_beads)

    tab_rv(tab)
  }, ignoreInit = TRUE)

  observeEvent(input$negative_cells, {
    req(tab_rv(), input$negative_cells)

    tab <- tab_rv()

    if (input$negative_cells == "None") {
      tab$universal.negative[tab$control.type == "cells"] <- NA
    } else {
      tab$universal.negative[tab$control.type == "cells"] <- input$negative_cells
    }
    tab <- set_negative_name(tab, "cells", input$negative_cells)

    tab_rv(tab)
  }, ignoreInit = TRUE)

  # ===========================================================
  # CONTROL TABLE AND CHECK (Parameters > STEP 2)
  # ===========================================================

  # A reactiveVal only invalidates on a real change: the steps built on it are not rebuilt
  # (and the table not reset) at every cell edit.
  observe(has_tab_rv(!is.null(tab_rv())))

  # shown columns, the read-only gate group of each control (groups are edited in
  # Control preparation > STEP 2) and the result of the last control check: Pass/Fail per
  # control, plus one hidden ".problem.<column>" column per column with the problem of each
  # cell to change (always there, so the set of columns does not change with the check)
  table_data <- reactive({
    tab <- tab_rv()
    req(tab)
    d <- tab[, shown_cols(tab), drop = FALSE]
    d$gate.group <- row_groups(tab, control_gates())
    res <- check_rv()
    current <- !is.null(res) && length(res$status) == nrow(tab)
    d$check <- if (current) res$status else ""
    for (col in setdiff(names(d), "check")) {
      p <- character(nrow(d))
      if (current) {
        hit <- res$cells[res$cells$column == col, , drop = FALSE]
        p[hit$row] <- hit$message
      }
      d[[paste0(".problem.", col)]] <- p
    }
    d
  })
  observe(table_cols_rv(names(table_data())))

  # Built once per set of columns; edits and group changes only replace the data, which
  # keeps the page, the scroll position and the cell being edited. The hidden problem
  # columns come last, so a shown column has the same index in the data and among the cells
  # of a row (the row names are index 0).
  output$table <- renderDT({
    req(table_cols_rv())
    d <- isolate(table_data())
    problem <- grep("^[.]problem[.]", names(d))
    cols <- match(sub("^[.]problem[.]", "", names(d)[problem]), names(d))
    datatable(
      d,
      editable = list(target = "cell",
                      disable = list(columns = c(match(c("gate.group", "check"), names(d)), problem))),
      options = list(
        scrollX = TRUE, pageLength = 100,
        columnDefs = list(list(visible = FALSE, targets = problem)),
        # a cell to change is red, its problem shown on hover; Pass green, Fail red
        rowCallback = JS(sprintf(
          "function(row, data) {
             var cols = [%s], probs = [%s], check = %d;
             for (var k = 0; k < cols.length; k++) {
               var td = $('td', row).eq(cols[k]), p = data[probs[k]];
               if (p) td.css({'background-color': '#f8d7da', 'color': '#721c24'}).attr('title', $('<div>').html(p).text());
               else td.css({'background-color': '', 'color': ''}).removeAttr('title');
             }
             var c = data[check];
             $('td', row).eq(check).css({'font-weight': 'bold',
               'color': c === 'Fail' ? 'red' : (c === 'Pass' ? 'green' : '')});
           }",
          paste(cols, collapse = ","), paste(problem, collapse = ","), match("check", names(d))))
      )
    )
  })
  observe(replaceData(dataTableProxy("table"), table_data(), resetPaging = FALSE))

  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    tab  <- tab_rv()
    col  <- shown_cols(tab)[info$col]
    req(!is.na(col))
    value <- if (col %in% flag_cols || is.logical(tab[[col]])) parse_flag(info$value)
             else DT::coerceValue(info$value, tab[[col]][info$row])
    new <- tab
    if (!is.na(value)) new[info$row, col] <- value
    if (is.na(value))
      showNotification(paste0(col, " takes TRUE or FALSE."), type = "warning")
    # a refused or unchanged value: show the table's value again in the cell
    if (identical(new, tab))
      replaceData(dataTableProxy("table"), table_data(), resetPaging = FALSE)
    else tab_rv(new)
  })

  output$downloadData <- downloadHandler(
    filename = "Autospectral_table.csv",
    content  = function(file) write.csv(tab_rv(), file, row.names = FALSE)
  )

  output$table_ui <- renderUI({
    if (!has_tab_rv()) {
      return(NULL)
    }
    box(
      width = 12,
      HTML(paste0("<h4><span style='background-color:#fdfd96;'>STEP 2 :</span> Control table and check ",
                  as.character(actionButton("HELP_TABLE", "", icon("question-circle"), class = "btn-xs")), "</h4>")),

      fluidRow(
        column(6, HTML("<h4><b>Download uncompleted table</b></h4>"),
               downloadButton("downloadData", label = "Download", class = NULL, icon = shiny::icon("download"))),
        column(6, HTML("<h4><b>Upload completed table (.csv, .xls, .xlsx)</b></h4>"),
               fileInput("csv", label = NULL, buttonLabel = "upload table",
                         accept = c(".csv", ".xls", ".xlsx")))
      ),
      DTOutput("table", width = "100%"),
      div(style = "margin-top:10px;",
          actionButton("check_controls", "Check controls"),
          uiOutput("check_result_ui"))
    )
  })

  # ---------------- control check ----------------
  # Everything the preparation stops on before the gating, located in the table:
  # check.control.file (define.flow.control runs it in strict mode: any error stops it), the
  # gate groups (assign.gates), the manual gates (check.gates) and the channels of the machine.
  # check.control.file gives some errors no row, blames the wrong row for others and crashes
  # on a universal negative missing from the table: those are located here instead.
  # Errors are cells to change (red); warnings do not stop the preparation and stay as text.

  # voltage/parameter mismatches are not reported to the user
  ignored_rules <- c("voltage_mismatch", "parameter_mismatch", "voltage_error", "missing_marker")
  # check.control.file rules located by run_control_check itself
  located_rules <- c("universal_negative_missing", "channel_parameter_mismatch", "missing_channel",
                     "duplicate_fluorophore", "af_wrong_type", "very_low_event_count", "low_event_count")
  # cell of the rules check.control.file reports without a column, and clearer problems
  rule_column  <- c(duplicate_af = "fluorophore", fcs_header_read_failed = "filename")
  rule_problem <- c(fcs_header_read_failed = "not among the uploaded control files",
                    duplicate_af           = "only one AF (unstained cells) control is allowed",
                    missing_control_type   = "control.type missing: beads or cells",
                    negative_type_mismatch = "the universal negative is not of the same control.type")

  # Writes the table to `file` (the control file of the preparation by default), checks it
  # and updates the check result, the Pass/Fail per fluorophore and the warning/error texts.
  # Returns the result: cells (row, column, value, message), general problems not tied to a
  # cell, warnings and the Pass/Fail of every row. Errors propagate to the caller.
  run_control_check <- function(add_log, file = control_csv) {

    asp <- asp_rv()
    tab <- tab_rv()
    dir <- control.def.file_rv()

    # A file used as universal negative without a marker is a negative control: its fluorophore
    # is named Negative (else check.control.file warns "missing_marker" for it).
    if (all(c("filename", "fluorophore", "marker", "universal.negative") %in% names(tab))) {
      un_files <- unique(na.omit(trimws(as.character(tab$universal.negative))))
      neg <- tab$filename %in% un_files & (is.na(tab$marker) | !nzchar(trimws(as.character(tab$marker)))) &
        !grepl(negative_pattern, tab$fluorophore, ignore.case = TRUE)
      if (any(neg)) {
        tab$fluorophore[neg] <- ifelse(tab$control.type[neg] %in% "cells", "AF", "Negative")
        tab_rv(tab)
      }
    }

    # ---------------- write control file ----------------
    add_log("Writing control file...")

    sent <- tab
    chr <- vapply(sent, is.character, logical(1))
    sent[chr] <- lapply(sent[chr], function(x) { x[is.na(x)] <- ""; x })
    write.csv(sent, file, row.names = FALSE, quote = TRUE)

    # ---------------- check control file ----------------
    add_log("Checking control file...")

    # the table as AutoSpectral reads it back (same rows as tab)
    ct <- read.csv(file, stringsAsFactors = FALSE, strip.white = TRUE)
    ct[] <- lapply(ct, function(x) {
      if (is.character(x)) { x <- trimws(x); x[x == ""] <- NA }
      x
    })
    col_of <- function(col) if (col %in% names(ct)) ct[[col]] else rep(NA, nrow(ct))

    cells <- list()
    general <- character()
    warnings <- character()
    flag <- function(rows, column, message) {
      rows <- rows[!is.na(rows)]
      if (length(rows) > 0)
        cells[[length(cells) + 1]] <<- data.frame(row = rows, column = column, message = message,
                                                  stringsAsFactors = FALSE)
    }

    # headers of the control files: parameters of the first readable one, events of each
    files <- unique(na.omit(col_of("filename")))
    headers <- lapply(setNames(files, files), function(f)
      tryCatch(suppressWarnings(readFCSheader(file.path(dir, f))[[1]]), error = function(e) NULL))
    readable <- Filter(Negate(is.null), headers)
    params <- if (length(readable) > 0) {
      h <- readable[[1]]
      vapply(seq_len(as.integer(h["$PAR"])), function(i) h[[paste0("$P", i, "N")]], character(1))
    }
    events <- vapply(readable, function(h) as.integer(h["$TOT"]), integer(1))
    # define.flow.control looks for the time, scatter and spectral channels in the first file
    chans <- if (length(readable) > 0) tryCatch(suppressWarnings(list(
      fixed    = c(asp$default.time.parameter, AutoSpectral:::read.scatter.parameter(asp)),
      spectral = AutoSpectral:::.derive.spectral.channels(file.path(dir, names(readable)[1]), asp)
    )), error = function(e) NULL)

    un <- col_of("universal.negative")
    absent <- !is.na(un) & !un %in% col_of("filename")
    flag(which(absent), "universal.negative", "not a file of the table: choose the negative among the controls")
    # The channel of a stained control is its peak channel: the cleaning (universal negative)
    # and the spectra select its positive events there. check.control.file only warns about a
    # blank one (left so for a file create.control.file cannot match), which stops the cleaning
    # with "subscript out of bounds"; a non-spectral one finds too few positive events.
    fl <- col_of("fluorophore")
    ch <- col_of("channel")
    stained <- !is.na(fl) & !grepl(negative_pattern, fl, ignore.case = TRUE)
    flag(which(stained & is.na(ch)), "channel",
         "peak channel missing: choose the detector where this fluorophore is brightest")
    if (length(params) > 0)
      flag(which(!is.na(ch) & !ch %in% params), "channel", "not a channel of the FCS files")
    if (length(chans$spectral) > 0)
      flag(which(stained & ch %in% setdiff(params, chans$spectral)), "channel",
           "not a spectral detector: choose the detector where this fluorophore is brightest")
    flag(which(!is.na(fl) & (duplicated(fl) | duplicated(fl, fromLast = TRUE))), "fluorophore",
         "used by more than one control")
    af <- which(fl %in% "AF")
    flag(af[!col_of("control.type")[af] %in% "cells"], "control.type",
         "the AF (unstained cells) control must be of type cells")
    flag(which(col_of("filename") %in% names(events)[which(events < 1000)]), "filename",
         "fewer than 1000 events in this file")
    low <- names(events)[which(events >= 1000 & events < 5000)]
    if (length(low) > 0) warnings <- c(warnings, paste0("low_event_count: ", low))

    # check.control.file, on a copy without the negatives it would crash on
    checked <- file
    if (any(absent)) {
      checked <- tempfile(fileext = ".csv")
      on.exit(unlink(checked), add = TRUE)
      sent$universal.negative[absent] <- ""
      write.csv(sent, checked, row.names = FALSE, quote = TRUE)
    }
    control.file.errors <- tryCatch(
      withCallingHandlers(
        with_session_wd(
          check.control.file(
            control.dir      = dir,
            control.def.file = checked,
            asp              = asp
          )
        ),
        warning = function(w) {
          if (grepl("voltage|mismatch|missing_marker|marker missing", conditionMessage(w), ignore.case = TRUE))
            invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        general <<- c(general, paste("AutoSpectral's control file check failed:", strip_ansi(conditionMessage(e))))
        TRUE
      }
    )

    # check.control.file returns TRUE when clean, else an issues table
    # (rule, severity, filename, column, message) or a list depending on the AutoSpectral version.
    if (is.data.frame(control.file.errors) && nrow(control.file.errors) > 0) {
      issues <- control.file.errors[!control.file.errors$rule %in% c(ignored_rules, located_rules), , drop = FALSE]
      for (k in seq_len(nrow(issues))) {
        is <- issues[k, ]
        if (is$severity != "error") {
          warnings <- c(warnings, paste0(is$rule, if (!is.na(is$filename)) paste0(": ", is$filename)))
          next
        }
        rows <- which(col_of("filename") == is$filename)
        problem <- if (is$rule %in% names(rule_problem)) rule_problem[[is$rule]] else is$message
        if (is$rule == "invalid_negative_fluorophore") {
          # a stained control used as a negative, or a negative not named so
          flag(rows, "fluorophore", "used as a universal negative: name its fluorophore Negative (or AF)")
          flag(which(un == is$filename), "universal.negative", sprintf("'%s' is not a negative control", is$filename))
          next
        }
        column <- if (is$rule %in% names(rule_column)) rule_column[[is$rule]] else is$column
        if (length(rows) > 0 && !is.na(column) && column %in% names(tab)) flag(rows, column, problem)
        else general <- c(general, paste0(problem, if (!is.na(is$column)) paste0(" (", is$column, ")")))
      }
    } else if (is.list(control.file.errors)) {
      general  <- c(general, control.file.errors$Errors)
      warnings <- c(warnings, control.file.errors$Warnings)
    }
    warnings <- warnings[!grepl("voltage|mismatch|missing_marker|marker missing", warnings, ignore.case = TRUE)]

    # ---------------- gate groups and manual gates ----------------
    gt <- gate_table(tab)
    if (is.null(gt)) {
      general <- c(general, "The gate groups could not be assigned: check control.type and is.viability.")
    } else {
      gates <- active_gates()
      for (g in names(gates)) {
        msg <- tryCatch({ suppressWarnings(check.gates(gates[g], gt, asp)); NULL },
                        error = function(e) strip_ansi(conditionMessage(e)))
        if (is.null(msg)) next
        rows <- which(tab$filename %in% gt$filename[gt$gate.name %in% g])
        if (length(rows) > 0) flag(rows, "gate.group", paste("manual gate:", msg))
        else general <- c(general, msg)
      }
    }

    # ---------------- channels of the machine ----------------
    if (length(readable) > 0) {
      lacking <- if (!is.null(chans)) setdiff(chans$fixed, params)
      if (is.null(chans) || length(chans$spectral) == 0 || length(lacking) > 0) {
        general <- c(general, sprintf("The FCS files lack the channels of the %s%s: check the machine (Upload FCS tab).",
                                      asp$cytometer,
                                      if (length(lacking) > 0) paste0(" (", paste(lacking, collapse = ", "), ")") else ""))
      } else if (anyDuplicated(c(chans$fixed, chans$spectral)) != 0) {
        general <- c(general, "Names for channels overlap in the FCS files.")
      }
    }

    # ---------------- result ----------------
    # one line per cell, its problems joined
    cells <- if (length(cells) > 0) do.call(rbind, cells)
             else data.frame(row = integer(), column = character(), message = character(), stringsAsFactors = FALSE)
    key <- paste(cells$row, cells$column, sep = "\r")
    msg <- tapply(cells$message, key, function(m) paste(unique(m), collapse = "; "))
    cells <- cells[!duplicated(key), c("row", "column"), drop = FALSE]
    cells$message <- unname(as.character(msg[paste(cells$row, cells$column, sep = "\r")]))
    groups <- row_groups(tab, gt)
    cells$value <- vapply(seq_len(nrow(cells)), function(k) {
      v <- if (cells$column[k] == "gate.group") groups[cells$row[k]] else tab[[cells$column[k]]][cells$row[k]]
      if (is.na(v)) "" else as.character(v)
    }, character(1))
    cells <- cells[order(cells$row), , drop = FALSE]
    rownames(cells) <- NULL

    res <- list(cells = cells, general = unique(general), warnings = unique(warnings),
                status = ifelse(seq_len(nrow(tab)) %in% cells$row, "Fail", "Pass"))
    check_rv(res)

    # Pass / Fail per control file (fluorophore)
    fl <- !is.na(tab$fluorophore) & nzchar(tab$fluorophore) & tolower(tab$fluorophore) != "negative"
    fluor_status_rv(data.frame(
      fluorophore = tab$fluorophore[fl],
      status = res$status[fl],
      stringsAsFactors = FALSE
    ))

    warning_rv(paste(res$warnings, collapse = "\n"))
    error_rv(paste0(
      paste0("<span style='color:red;'>", htmltools::htmlEscape(res$general), "</span><br>", collapse = ""),
      if (nrow(cells) > 0)
        sprintf("<span style='color:red;'>%d cell(s) to change in %d control(s): red in the table.</span><br>",
                nrow(cells), length(unique(cells$row))),
      "<span style='background-color:#fdfd96;'>Check completed</span>"
    ))

    invisible(res)
  }

  # Prepare stops here, before the gating, when the check found something to change.
  stop_on_problems <- function(res) {
    cells <- res$cells
    if (nrow(cells) == 0 && length(res$general) == 0) return(invisible())
    lines <- sprintf("row %d %s / %s '%s': %s", cells$row, tab_rv()$filename[cells$row],
                     cells$column, cells$value, cells$message)
    stop(paste(c(
      sprintf("The control table has %d problem(s) to fix, red cells in Parameters > STEP 2 :",
              nrow(cells) + length(res$general)),
      res$general, head(lines, 10),
      if (length(lines) > 10) sprintf("... and %d more", length(lines) - 10)
    ), collapse = "\n"), call. = FALSE)
  }

  output$check_result_ui <- renderUI({
    res <- check_rv()
    st  <- fluor_status_rv()
    req(res, st)
    clean <- nrow(res$cells) == 0 && length(res$general) == 0
    tagList(
      HTML(sprintf("<h4><b>%d / %d fluorophores passed</b></h4>", sum(st$status == "Pass"), nrow(st))),
      HTML(error_rv()),
      if (nrow(res$cells) > 0) DTOutput("check_problems"),
      if (length(res$warnings) > 0)
        HTML(paste0("<p><b>Warnings</b> (the preparation still runs):<br>",
                    paste(htmltools::htmlEscape(res$warnings), collapse = "<br>"), "</p>")),
      if (clean) HTML("<p style='color:green;'><b>No problem found")
    )
  })

  output$check_problems <- renderDT({
    res <- check_rv()
    req(res, nrow(res$cells) > 0)
    cells <- res$cells
    datatable(
      data.frame(row = cells$row, filename = tab_rv()$filename[cells$row], column = cells$column,
                 value = cells$value, problem = cells$message, stringsAsFactors = FALSE),
      rownames = FALSE,
      options = list(dom = "t", pageLength = 100, scrollX = TRUE)
    )
  })

  observeEvent(input$check_controls, {
    req(tab_rv(), asp_rv(), control.def.file_rv())
    run_logged("Control check", run_control_check)
  })

  # once checked, the table is checked again at each change (on a copy of the control file),
  # so a fixed cell turns white
  observeEvent(list(tab_rv(), active_gates()), {
    req(check_rv(), tab_rv(), asp_rv(), control.def.file_rv())
    f <- tempfile(fileext = ".csv")
    on.exit(unlink(f), add = TRUE)
    tryCatch(run_control_check(function(msg) NULL, f), error = function(e) NULL)
  }, ignoreInit = TRUE)

  # ===========================================================
  # GATE GROUPS (Control preparation > STEP 2, below the gate plot)
  # ===========================================================
  # The controls of a group share one automatic FSC/SSC gate, and their universal negative
  # is gated with it. assign.gates groups the controls by negative, type, viability and
  # large.gate, but keeps a gate.name already in the table: that is how a group is created
  # here, and how a control that fell back during gating gets its own group.

  # negatives are recognised from the fluorophore name, as in assign.gates
  negative_pattern <- "(^AF$)|(\\bAF\\b)|(\\bnegative\\b)|(\\bunstained\\b)"
  # controls that make a gate group: the stained ones and the AF (the negatives only follow them)
  group_member <- function(fl) !grepl(negative_pattern, fl, ignore.case = TRUE) | grepl("^AF$", fl, ignore.case = TRUE)
  # names of the groups assign.gates makes itself
  auto_group_pattern <- "^(smallGate|largeGate|viabilityGate)_[0-9]+$|density_orphan"

  # Gate group of every control, assigned as define.flow.control will do it: the table
  # goes through the same csv round trip (written as in run_control_check, read back
  # and trimmed as in define.flow.control), and assign.gates rebuilds the sample names.
  gate_table <- function(tab) {
    chr <- vapply(tab, is.character, logical(1))
    tab[chr] <- lapply(tab[chr], function(x) { x[is.na(x)] <- ""; x })
    f <- tempfile(fileext = ".csv")
    on.exit(unlink(f), add = TRUE)
    write.csv(tab, f, row.names = FALSE, quote = TRUE)
    ct <- read.csv(f, stringsAsFactors = FALSE, strip.white = TRUE)
    ct[] <- lapply(ct, function(x) {
      if (is.character(x)) { x <- trimws(x); x[x == ""] <- NA }
      x
    })
    ct$sample <- NULL
    tryCatch(assign.gates(ct, "density", TRUE, verbose = FALSE), error = function(e) NULL)
  }

  control_gates <- reactive({
    req(tab_rv())
    gate_table(tab_rv())
  })

  # gates that can be replaced: those of the stained controls and the AF (negatives follow them)
  stained_gate_names <- function(ct) {
    unique(ct$gate.name[group_member(ct$fluorophore)])
  }

  # gate group(s) of each row of the control table; a negative lists the groups it is copied into
  row_groups <- function(tab, ct) {
    if (is.null(ct)) return(rep("", nrow(tab)))
    vapply(tab$filename, function(f) paste(unique(na.omit(ct$gate.name[ct$filename %in% f])), collapse = ", "),
           character(1), USE.NAMES = FALSE)
  }

  # groups fixed in the table: created here, or "<sample>_fallback" after a fallback
  fixed_groups <- function(tab) {
    g <- if ("gate.name" %in% names(tab)) as.character(tab$gate.name) else character()
    unique(g[!is.na(g) & nzchar(g)])
  }

  # a gate name ends up in file names
  clean_group_name <- function(x) gsub("[/\\\\:*?\"<>|]+", "_", trimws(x))

  # bottom part of the manual control gate step, below the gate plot
  groups_panel <- function() {
    tagList(
      HTML(paste0("<h4><b>Gate groups</b> ",
                  as.character(actionButton("HELP_GROUPS", "", icon("question-circle"), class = "btn-xs")), "</h4>")),
      DTOutput("groups_table"),
      fluidRow(
        column(6, uiOutput("group_controls_ui")),
        column(3, textInput("group_name", "New group name", width = "100%")),
        column(3, div(style = "margin-top:25px;",
                      actionButton("group_create", "Create group"),
                      actionButton("group_reset", "Back to automatic group")))
      )
    )
  }

  output$groups_table <- renderDT({
    tab <- tab_rv()
    ct  <- control_gates()
    req(tab, ct)
    stained <- group_member(ct$fluorophore)
    gs <- stained_gate_names(ct)
    members <- function(g, what) paste(unique(na.omit(ct[[what]][stained & ct$gate.name %in% g])), collapse = ", ")
    fixed <- fixed_groups(tab)
    datatable(
      data.frame(
        group    = gs,
        origin   = ifelse(!gs %in% fixed, "automatic", ifelse(endsWith(gs, "_fallback"), "fallback", "created")),
        controls = vapply(gs, members, character(1), what = "sample"),
        gate     = ifelse(gs %in% names(active_gates()), "manual", "automatic"),
        stringsAsFactors = FALSE
      ),
      rownames = FALSE,
      options = list(dom = "t", pageLength = 100, scrollX = TRUE)
    )
  })

  # stained controls, labelled like the control plots, with their current group
  output$group_controls_ui <- renderUI({
    tab <- tab_rv()
    ct  <- control_gates()
    req(tab, ct)
    stained <- which(group_member(ct$fluorophore[seq_len(nrow(tab))]))
    selectInput("group_controls", "Controls", multiple = TRUE, width = "100%",
                choices = setNames(tab$filename[stained],
                                   sprintf("%s (%s)", ct$sample[stained], ct$gate.name[stained])))
  })

  observeEvent(input$group_create, {
    tab <- tab_rv()
    req(tab)
    files <- input$group_controls
    name  <- clean_group_name(input$group_name %||% "")
    refuse <- function(msg) showNotification(msg, type = "warning")
    if (length(files) == 0) return(refuse("Choose the controls of the new group."))
    if (!nzchar(name)) return(refuse("Give the new group a name."))
    if (grepl(auto_group_pattern, name))
      return(refuse("This name is kept for the automatic groups: choose another one."))
    if (!"gate.name" %in% names(tab)) tab$gate.name <- NA_character_
    tab$gate.name <- as.character(tab$gate.name)
    rows <- union(which(tab$filename %in% files), which(tab$gate.name %in% name))
    if (length(unique(tab$control.type[rows])) > 1)
      return(refuse("A gate group cannot mix beads and cells."))
    tab$gate.name[tab$filename %in% files] <- name
    tab_rv(tab)
    updateTextInput(session, "group_name", value = "")
  })

  observeEvent(input$group_reset, {
    tab <- tab_rv()
    req(tab)
    if (length(input$group_controls) == 0)
      return(showNotification("Choose the controls to put back in their automatic group.", type = "warning"))
    if ("gate.name" %in% names(tab)) {
      tab$gate.name[tab$filename %in% input$group_controls] <- NA
      tab_rv(tab)
    }
  })

  # ===========================================================
  # 3. CONTROL PREPARATION TAB
  # ===========================================================
  output$warning <- renderText(warning_rv())
  output$error   <- renderUI(HTML(error_rv()))

  # ---------------- manual control gate ----------------
  # A gate drawn here replaces the automatic FSC/SSC gate of every control sharing
  # its gate name; define.flow.control gets it through gate.list at the next preparation
  # (Apply manual gates). The gate groups are in the GATE GROUPS section.

  # manual gates that still match a gate group of a control table
  gates_for <- function(gates, ct) {
    keep <- intersect(names(gates), stained_gate_names(ct))
    if (length(keep) > 0) gates[keep] else list()
  }
  active_gates <- function() gates_for(manual_gates_rv(), control_gates())

  # newest automatic gate AutoSpectral saved for a gate group (NULL before the first run):
  # its density gate, or the landmark gate a fallback used
  auto_gate <- function(g) {
    g <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", g)
    f <- list.files(file.path(session_dir, "figure_gate", "gate_definitions"),
                    pattern = paste0("^(density|landmark)_gate_definition_", g, "_[0-9]{8}_[0-9]{6}\\.rds$"),
                    full.names = TRUE)
    stamp <- sub(".*_([0-9]{8}_[0-9]{6})\\.rds$", "\\1", f)
    if (length(f) > 0) readRDS(f[order(stamp, decreasing = TRUE)][1])
  }

  output$gate_ui <- renderUI({
    ct <- if (!is.null(tab_rv())) control_gates()
    if (is.null(ct))
      return(HTML("<p>Upload the controls first (Upload FCS tab).</p>"))
    if (is.null(spectra_rv()))
      return(HTML("<p>Prepare the controls first (<i>Prepare controls</i> above).</p>"))

    gates <- stained_gate_names(ct)
    tagList(fluidRow(
      column(4,
             selectInput("gate_name", "Gate group",
                         choices = gates,
                         selected = isolate(input$gate_name)),
             uiOutput("gate_file_ui"),
             div(style = "margin-bottom:10px;",
                 actionButton("HELP_GATE", "", icon("question-circle"), class = "btn-xs")),
             actionButton("gate_undo", "Undo last point"),
             actionButton("gate_clear", "Clear points"),
             div(style = "margin-top:10px;",
                 actionButton("gate_save", "Save manual gate"),
                 actionButton("gate_reset", "Back to automatic gate")),
             div(style = "margin-top:10px;", actionButton("gate_apply", "Apply manual gates")),
             div(style = "margin-top:10px;", uiOutput("gate_status"))),
      column(8, plotOutput("gate_plot", click = "gate_click", width = "600px", height = "600px"))
    ),
    groups_panel())
  })

  output$gate_file_ui <- renderUI({
    ct <- control_gates()
    req(ct, input$gate_name)
    # the negatives copied into the group are not listed
    sel   <- ct$gate.name %in% input$gate_name &
      group_member(ct$fluorophore)
    files <- ct$filename[sel]
    keep  <- !duplicated(files)
    # labelled with the sample name of the control plots above (figure_gate), values stay file names
    lab <- ct$sample[sel] %||% files
    lab <- ifelse(is.na(lab) | !nzchar(lab), files, lab)
    selectInput("gate_file", "Controls shown",
                choices = setNames(files[keep], lab[keep]))
  })

  # FSC/SSC events of the controls shown (PLOT_N in all), read once per choice
  gate_scatter <- reactive({
    ct <- control_gates()
    req(ct, input$gate_name, asp_rv())
    files <- unique(ct$filename[ct$gate.name %in% input$gate_name &
                                  group_member(ct$fluorophore)])
    # one control at a time: the one chosen, the first of the group until a choice is made
    files <- if (isTRUE(input$gate_file %in% files)) input$gate_file else files[1]
    sc <- asp_rv()$default.scatter.parameter
    set.seed(1)
    do.call(rbind, lapply(files, function(f) {
      ff <- read.FCS(file.path(control_tmp_dir, f), truncate_max_range = FALSE)
      subsample(ff, ceiling(PLOT_N / length(files)))@exprs[, sc, drop = FALSE]
    }))
  })

  output$gate_plot <- renderPlot({
    prep_run_rv()   # the automatic gate is read from disk: re-read it after each preparation
    d  <- as.data.frame(gate_scatter())
    sc <- colnames(d)
    p <- ggplot(d, aes(x = .data[[sc[1]]], y = .data[[sc[2]]])) +
      geom_point(size = 0.3, alpha = 0.3, colour = "grey40") +
      coord_cartesian(xlim = range(d[[1]]), ylim = range(d[[2]])) +
      labs(x = sc[1], y = sc[2], title = input$gate_name) +
      theme_classic()
    polygon <- function(g, ...)
      geom_polygon(data = data.frame(x = g$x, y = g$y), aes(x = x, y = y),
                   inherit.aes = FALSE, fill = NA, ...)
    auto <- auto_gate(input$gate_name)
    if (!is.null(auto)) p <- p + polygon(auto, colour = "black", linetype = "dashed")
    manual <- manual_gates_rv()[[input$gate_name]]
    if (!is.null(manual)) p <- p + polygon(manual, colour = "red", linewidth = 1)
    pts <- gate_points_rv()
    if (!is.null(pts))
      p <- p + geom_path(data = pts, aes(x = x, y = y), inherit.aes = FALSE, colour = "blue") +
        geom_point(data = pts, aes(x = x, y = y), inherit.aes = FALSE, colour = "blue", size = 2)
    p
  })

  observeEvent(input$gate_click, {
    gate_points_rv(rbind(gate_points_rv(), data.frame(x = input$gate_click$x, y = input$gate_click$y)))
  })

  observeEvent(input$gate_undo, {
    pts <- gate_points_rv()
    gate_points_rv(if (!is.null(pts) && nrow(pts) > 1) pts[-nrow(pts), , drop = FALSE] else NULL)
  })

  observeEvent(input$gate_clear, gate_points_rv(NULL))
  observeEvent(input$gate_name, gate_points_rv(NULL))

  observeEvent(input$gate_save, {
    req(input$gate_name, asp_rv())
    pts <- gate_points_rv()
    if (is.null(pts) || nrow(pts) < 3) {
      showNotification("Click at least 3 points on the plot to draw the gate.", type = "warning")
      return()
    }
    asp <- asp_rv()
    gates <- manual_gates_rv()
    gates[[input$gate_name]] <- list(
      x = pmin(pmax(pts$x, asp$scatter.data.min.x), asp$scatter.data.max.x),
      y = pmin(pmax(pts$y, asp$scatter.data.min.y), asp$scatter.data.max.y),
      i = seq_len(nrow(pts))
    )
    # check.gates also warns about the gates left automatic
    ok <- tryCatch(suppressWarnings(check.gates(gates[input$gate_name], control_gates(), asp)),
                   error = function(e) {
                     showNotification(paste("Gate not saved:", strip_ansi(conditionMessage(e))),
                                      type = "error", duration = NULL)
                     FALSE
                   })
    if (!isTRUE(ok)) return()
    manual_gates_rv(gates)
    gate_points_rv(NULL)
  })

  observeEvent(input$gate_reset, {
    req(input$gate_name)
    gates <- manual_gates_rv()
    gates[[input$gate_name]] <- NULL
    manual_gates_rv(gates)
    gate_points_rv(NULL)
  })

  output$gate_status <- renderUI({
    gates <- active_gates()
    msg <- if (length(gates) == 0) "No manual gate: every control uses its automatic gate."
           else paste0("<b>Saved manual gate(s):</b> ", paste(names(gates), collapse = ", "))
    g <- gates[[input$gate_name %||% ""]]
    if (!is.null(g)) {
      d <- gate_scatter()
      kept <- tryCatch(nrow(apply.gate(d, g, scatter.param = colnames(d), min.fraction = 0)),
                       error = function(e) 0)
      msg <- c(msg, sprintf("%s keeps %.1f%% of the events shown.", input$gate_name, 100 * kept / nrow(d)))
    }
    if (!is.null(spectra_rv()) && !identical(prepared_gates_rv(), gates))
      msg <- c(msg, "<span style='background-color:#fdfd96;'>Click Apply manual gates to update the control plots and spectra.</span>")
    HTML(paste(msg, collapse = "<br>"))
  })

  # ---------------- parallel processing ----------------
  # Gating, control cleaning and unmixing run on asp$worker.process.n cores
  # (AutoSpectral default: available cores - 1, cgroup quotas and options(mc.cores) respected).
  n_threads <- function(asp) max(1L, as.integer(asp$worker.process.n %||% 1L))

  # Free memory in bytes: MemAvailable, bounded by the cgroup limit; NA when unknown (not Linux).
  free_memory <- function() {
    read_num <- function(f) suppressWarnings(as.numeric(readLines(f, n = 1, warn = FALSE)))
    mi <- tryCatch(readLines("/proc/meminfo"), error = function(e) character())
    free <- 1024 * as.numeric(sub("\\D+(\\d+).*", "\\1", grep("^MemAvailable:", mi, value = TRUE)))
    cgroups <- list(c("/sys/fs/cgroup/memory.max", "/sys/fs/cgroup/memory.current"),
                    c("/sys/fs/cgroup/memory/memory.limit_in_bytes",
                      "/sys/fs/cgroup/memory/memory.usage_in_bytes"))
    for (cg in cgroups) if (all(file.exists(cg))) free <- c(free, read_num(cg[1]) - read_num(cg[2]))
    free <- free[is.finite(free)]
    if (length(free) == 0) NA_real_ else max(0, min(free))
  }

  # Workers for a step that loads one FCS file per worker: at most `threads`, and only as
  # many as fit in half of the free memory (a worker needs about 10x the largest file size).
  fit_workers <- function(files, threads, file_bytes = max(c(0, file.size(files)), na.rm = TRUE)) {
    free <- free_memory()
    if (is.na(free) || file_bytes <= 0) return(as.integer(threads))
    as.integer(max(1, min(threads, floor(0.5 * free / (10 * file_bytes)))))
  }

  # fun(parallel) runs once with parallel = TRUE. A failed or killed worker is only a
  # warning from mclapply (and clean.controls swallows the resulting error), so that
  # warning stops the parallel run and the step is re-run with parallel = FALSE.
  worker_failure <- "resulted in an error|did not deliver|encountered errors? in user code"

  run_parallel <- function(step, add_log, fun) {
    tryCatch(
      withCallingHandlers(fun(TRUE), warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl(worker_failure, msg))
          signalCondition(structure(class = c("parallel_failure", "condition"),
                                    list(message = msg, call = NULL)))
        if (grepl("Parallelization has not been fully optimized", msg))
          invokeRestart("muffleWarning")
      }),
      parallel_failure = function(e) {
        add_log(paste0(step, ": parallel run failed (", conditionMessage(e),
                       "), running sequentially."))
        fun(FALSE)
      }
    )
  }

  strip_ansi <- function(x) gsub("\033\\[[0-9;]*m", "", x)

  # Runs fun(add_log) behind a blocking modal, then shows its full log in a closable one.
  # on_error(msg) records the error message; returns TRUE when fun succeeded.
  run_logged <- function(title, fun, on_error = function(msg) NULL) {

    log_msgs <- character()
    add_log <- function(msg) {
      log_msgs <<- c(log_msgs, msg)
      message(msg)
    }
    log_html <- function() {
      HTML(paste("<pre>", paste(log_msgs, collapse = "\n"), "</pre>"))
    }

    showModal(modalDialog(
      title = paste(title, "in progress"),
      HTML(paste0("<pre>Starting ", tolower(title), "...</pre>")),
      footer = NULL,
      easyClose = FALSE
    ))

    ok <- tryCatch({
      fun(add_log)
      TRUE
    }, error = function(e) {
      msg <- strip_ansi(conditionMessage(e))
      add_log(paste("ERROR:", msg))
      on_error(msg)
      FALSE
    })

    showModal(modalDialog(
      title = paste(title, if (ok) "completed" else "failed"),
      log_html(),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))

    invisible(ok)
  }

  # "none" before the first preparation, "current" while the control table and the manual
  # gates are those the spectra were computed from, "stale" once either changed.
  prep_state <- function() {
    if (is.null(spectra_rv())) return("none")
    if (identical(prepared_tab_rv(), tab_rv()) && identical(prepared_gates_rv(), active_gates()))
      "current" else "stale"
  }

  # define.flow.control checks gate.list against the gate.name column of the control
  # file, so with manual gates every stained control gets its group written there
  # (negatives stay blank: assign.gates copies them into the groups of their controls).
  write_gate_names <- function(tab, gates) {
    if (length(gates) == 0) return(invisible())
    ct  <- gate_table(tab)
    csv <- read.csv(control_csv, stringsAsFactors = FALSE, check.names = FALSE)
    stained <- !grepl(negative_pattern, ct$fluorophore, ignore.case = TRUE)
    group <- ifelse(stained, ct$gate.name, "")[seq_len(nrow(csv))]
    if (!"gate.name" %in% names(csv)) csv$gate.name <- NA_character_
    csv$gate.name <- as.character(csv$gate.name)
    empty <- is.na(csv$gate.name) | !nzchar(csv$gate.name)
    csv$gate.name[empty] <- group[empty]
    write.csv(csv, control_csv, row.names = FALSE, quote = TRUE, na = "")
  }

  # AutoSpectral names the sample it falls back for: a gate of its own (landmark gate)
  # when its group gate keeps too few events, all its events when that gate keeps none
  # (a negative gets no gate of its own: it goes straight to all its events)
  fallback_pattern <- "^Sample '(.+?)' (has fewer than [0-9]+ events after gating|\\(.*\\) returned 0 events after gating)"

  # define.flow.control, plus the stained controls it fell back for and the negative copies
  # it used ungated. The gates are defined from subsamples seeded with asp$bird.seed, drawn
  # with R's default generator as before parallel gating: under L'Ecuyer-CMRG every
  # automatic gate changes, and some controls fell back.
  # The cleaning that follows goes back to L'Ecuyer-CMRG (see run_preparation).
  gate_controls <- function(gates, threads, add_log) {
    on.exit(RNGkind("L'Ecuyer-CMRG"), add = TRUE)
    hits <- character()
    note <- function(cond) {
      msg <- strip_ansi(conditionMessage(cond))
      hit <- regmatches(msg, regexec(fallback_pattern, msg, perl = TRUE))[[1]]
      if (length(hit) > 1) hits <<- c(hits, hit[2])
    }
    fc <- withCallingHandlers(
      with_session_wd(
        run_parallel("Gating", add_log, function(parallel) {
          RNGkind("Mersenne-Twister")   # again for a sequential re-run: AutoSpectral's workers switch it
          define.flow.control(
            control.def.file_rv(),
            control_csv,
            asp_rv(),
            gate = TRUE,
            gating.system = "density",
            gate.list = if (length(gates) > 0) gates,
            parallel = parallel,
            threads = threads
          )
        })
      ),
      message = note, warning = note
    )
    negative <- fc$sample[grepl(negative_pattern, fc$fluorophore, ignore.case = TRUE)]
    hits <- unique(hits)
    list(fc = fc, fell_back = setdiff(hits, negative), ungated = intersect(hits, negative))
  }

  # A universal negative is copied into the gate group of each of its stained controls and
  # gated there with the gate of that group. In a group whose controls lie elsewhere in FSC/SSC
  # than the negative (a "<sample>_fallback" group, for one), the copy keeps no events and
  # AutoSpectral uses it ungated. The controls of that group take instead the copy of the same
  # negative that kept the most events in its gate: the negative they had in a shared group.
  use_gated_negatives <- function(fc, ungated, add_log) {
    events <- table(factor(fc$event.sample, levels = fc$sample))
    is_negative <- grepl(negative_pattern, fc$fluorophore, ignore.case = TRUE)
    for (copy in ungated) {
      users <- setdiff(names(fc$universal.negative)[fc$universal.negative == copy], copy)
      gated <- setdiff(fc$sample[is_negative & fc$filename == fc$filename[[copy]]], ungated)
      if (length(users) == 0 || length(gated) == 0) next
      home <- gated[which.max(events[gated])]
      fc$universal.negative[users] <- home
      add_log(sprintf("%s (%s) kept no events in the gate of its group: %s cleaned against %s (%d gated events) instead.",
                      copy, fc$filename[[copy]], paste(users, collapse = ", "), home, events[[home]]))
    }
    fc
  }

  # the plots of the negative copies in a "<sample>_fallback" group are not kept
  drop_negative_fallback_figures <- function(fc) {
    neg <- fc$sample[grepl(negative_pattern, fc$fluorophore, ignore.case = TRUE) & endsWith(fc$sample, "_fallback")]
    unlink(file.path(session_dir, "figure_gate", paste0(neg, ".jpg")))
  }

  # rows of the stained controls that fell back and are not yet in a group of their own
  fallback_rows <- function(tab, samples) {
    ct <- gate_table(tab)
    if (is.null(ct) || length(samples) == 0) return(integer())
    rows <- match(samples, ct$sample)
    rows <- rows[!is.na(rows) & rows <= nrow(tab)]
    rows <- rows[!grepl(negative_pattern, ct$fluorophore[rows], ignore.case = TRUE)]
    current <- if ("gate.name" %in% names(tab)) as.character(tab$gate.name[rows]) else rep(NA, length(rows))
    rows[is.na(current) | current != clean_group_name(paste0(ct$sample[rows], "_fallback"))]
  }

  # Control checks + gating + spectra extraction, skipped when nothing changed since the
  # last preparation. Returns TRUE on success; errors propagate to the caller's tryCatch.
  prepare_data <- function(add_log) {

    if (prep_state() == "current") {
      add_log("Controls unchanged: reusing spectra.")
      return(TRUE)
    }

    asp <- asp_rv()
    tab <- tab_rv()   # kept as is: run_control_check sends a copy with its NAs filled
    gates <- active_gates()

    # checked before anything is cleared: a table with cells to change stops here, before
    # the gating, and the previous preparation is kept
    stop_on_problems(run_control_check(add_log))

    # a failed run leaves the controls "not prepared" rather than half updated
    spectra_rv(NULL)
    flow.control_rv(NULL)
    prepared_tab_rv(NULL)
    prepared_gates_rv(NULL)
    # also drops the automatic definition plot of a gate replaced by a manual one
    clear_prep_figures()
    if (length(unmixed_files_rv()) > 0) {
      add_log("Previous unmixed files removed: they used the previous spectra.")
      clear_unmixed()
    }

    if (length(gates) > 0) add_log(paste("Manual gate(s):", paste(names(gates), collapse = ", ")))
    write_gate_names(tab, gates)

    # ---------------- flow control ----------------
    # one control file per worker for the gating, one sample per worker for the
    # autofluorescence removal of clean.controls (which uses half of the threads)
    threads <- fit_workers(list.files(control.def.file_rv(), full.names = TRUE), n_threads(asp))
    add_log(sprintf("Defining flow control (parallel gating on %d cores)...", threads))
    gating <- gate_controls(gates, threads, add_log)

    # A control left with too few events in its group gate fell back to a gate of its own
    # (or to all its events): it moves to its own group, "<sample>_fallback", and the
    # controls are gated again, so that every group shown has its automatic gate.
    fallback_rv(character())
    moved <- fallback_rows(tab, gating$fell_back)
    if (length(moved) > 0) {
      samples <- gate_table(tab)$sample[moved]
      if (!"gate.name" %in% names(tab)) tab$gate.name <- NA_character_
      tab$gate.name <- as.character(tab$gate.name)
      tab$gate.name[moved] <- clean_group_name(paste0(samples, "_fallback"))
      tab_rv(tab)
      fallback_rv(samples)
      add_log(paste0("Gating fell back for ", paste(samples, collapse = ", "),
                     ": moved to a gate group of its own. Gating again..."))
      # a manual gate whose group lost all its controls is dropped
      gates <- gates_for(gates, gate_table(tab))
      clear_prep_figures()
      run_control_check(add_log)
      write_gate_names(tab, gates)
      gating <- gate_controls(gates, threads, add_log)
      if (length(gating$fell_back) > 0)
        add_log(paste0("Still too few events in its own group for ", paste(gating$fell_back, collapse = ", "),
                       ": AutoSpectral's fallback gate is used."))
    }
    gating$fc <- use_gated_negatives(gating$fc, gating$ungated, add_log)
    drop_negative_fallback_figures(gating$fc)

    flow.control <- with_session_wd(
      run_parallel("Control cleaning", add_log, function(parallel)
        clean.controls(gating$fc, asp, parallel = parallel, threads = threads)
      )
    )
    flow.control_rv(flow.control)
    add_log("Flow control cleaned.")

    # ---------------- spectra ----------------
    add_log("Extracting fluorophore spectra...")

    spectra <- with_session_wd(
      get.fluorophore.spectra(
        flow.control,
        asp,
        use.clean.expr = TRUE
      )
    )
    spectra_rv(spectra)
    prepared_tab_rv(tab)
    prepared_gates_rv(gates)
    prep_run_rv(prep_run_rv() + 1)
    add_log("Spectra extracted.")

    TRUE
  }

  # AutoSpectral's forked workers switch R to the L'Ecuyer-CMRG generator before seeding
  # with asp$bird.seed; using it for the cleaning and the unmixing keeps the seeded subsamples
  # the same whether a step ran in parallel or sequentially. The gates are defined before
  # AutoSpectral's workers start, with R's default generator (gate_controls).
  run_preparation <- function() {
    if (is.null(tab_rv()) || is.null(control.def.file_rv())) {
      error_rv("<span style='background-color:#fdfd96;'>Upload the controls first (Upload FCS tab).</span>")
      return(invisible(FALSE))
    }
    RNGkind("L'Ecuyer-CMRG")
    run_logged("Control preparation", function(add_log) {
      withProgress(message = "Preparing controls", value = 0.3, prepare_data(add_log))
    }, on_error = function(msg) {
      error_rv(paste0("<span style='color:red; font-weight:bold;'>ERROR:</span><br>",
                      gsub("\n", "<br>", htmltools::htmlEscape(msg))))
      warning_rv("Pipeline stopped due to error")
    })
  }

  observeEvent(input$prepare, run_preparation())
  observeEvent(input$gate_apply, run_preparation())

  output$prep_summary_ui <- renderUI({
    tab <- tab_rv()
    if (is.null(tab))
      return(HTML("Upload the controls first (Upload FCS tab)."))
    state <- prep_state()
    if (state == "none")
      return(HTML(sprintf("<b>%d controls</b>, not prepared yet.",
                          nrow(tab))))
    gates <- prepared_gates_rv()
    msg <- c(sprintf("<b>%d controls prepared</b>", nrow(prepared_tab_rv())),
             sprintf("%d fluorophore spectra", nrow(spectra_rv())),
             paste("Gates:", if (length(gates) == 0) "automatic"
                             else paste("manual for", paste(names(gates), collapse = ", "))),
             if (length(fallback_rv()) > 0)
               paste("Gating fell back, own gate group for:", paste(fallback_rv(), collapse = ", ")))
    if (state == "stale")
      msg <- c(msg, "<span style='background-color:#fdfd96;'>The control table or the manual gates changed: prepare the controls again.</span>")
    HTML(paste(msg, collapse = "<br>"))
  })

  output$visual_ui <- renderUI({
    box(width = 12,
        HTML("<h4><span style='background-color:#fdfd96;'>STEP 1 :</span> Control plots</h4>"),
        if (is.null(spectra_rv()))
          HTML("<p>Click <i>Prepare controls</i> above </p>")
        else uiOutput("heatmap_ui"))
  })

  # ===========================================================
  # 4. ANALYSIS TAB
  # ===========================================================
  output$unmix_error <- renderUI(HTML(unmix_error_rv()))

  observeEvent(input$analysis, {

    # the unmixing uses the prepared spectra: say what to do first instead of running
    todo <- function(msg) unmix_error_rv(paste0("<span style='background-color:#fdfd96;'>", msg, "</span>"))
    if (length(raw_files_rv()) == 0)
      return(todo("Upload the raw files first (Upload FCS tab)."))
    state <- prep_state()
    if (state == "none")
      return(todo("Prepare the controls first (Control preparation tab)."))
    if (state == "stale")
      return(todo(paste("The control table or the manual gates changed since the last preparation:",
                        "prepare the controls again (Control preparation tab).")))
    unmix_error_rv("")
    analysis_status("")

    RNGkind("L'Ecuyer-CMRG")   # as for the preparation (run_preparation)

    run_logged("Analysis", function(add_log) {

      withProgress(message = "Unmixing", value = 0.1, {

        asp <- asp_rv()
        threads <- n_threads(asp)

        incProgress(0.5, detail = "Unmixing")
        add_log(paste("Starting unmixing process on:", paste(raw_files_rv(), collapse = ", ")))

        af_spectra <- NULL
        spectra_variants <- NULL
        speed <- "slow"
        if (identical(input$method, "AutoSpectral")) {
          speed <- if (is.null(input$af_speed)) "medium" else input$af_speed
          # the autofluorescence comes from the unstained cells used as the cells negative
          unst <- input$negative_cells
          if (is.null(unst) || unst == "None")
            stop("AutoSpectral needs unstained cells: choose 'no fluorescent cells' in 'Autofluorescence parameters'.")
          unst_path <- file.path(control_tmp_dir, unst)
          add_log(paste("Computing autofluorescence spectra from:", unst))
          af_spectra <- with_session_wd(
            get.af.spectra(
              unstained.sample = unst_path,
              asp = asp,
              spectra = spectra_rv(),
              figures = FALSE,
              save = FALSE,
              parallel = TRUE,
              threads = threads
            )
          )
          if (!is.matrix(af_spectra) || nrow(af_spectra) < 2)
            stop("Could not derive at least 2 autofluorescence spectra from '", unst, "'.")
          add_log(paste("Autofluorescence spectra:", nrow(af_spectra)))

          add_log("Computing spectral variants...")
          spectra_variants <- tryCatch(
            with_session_wd(
              get.spectral.variants(
                control.dir = control_tmp_dir,
                control.def.file = control_csv,
                asp = asp,
                spectra = spectra_rv(),
                figures = FALSE,
                parallel = TRUE,
                threads = threads
              )
            ),
            error = function(e) {
              add_log(paste("Spectral variants skipped:", strip_ansi(conditionMessage(e))))
              NULL
            }
          )
        }

        # OLS/WLS: one raw file per worker; AutoSpectral/FastPoisson: files in turn,
        # each one unmixed on all cores (AutoSpectralRcpp threads)
        per_file <- input$method %in% c("OLS", "WLS")
        raw_paths <- list.files(raw_tmp_dir, pattern = "\\.fcs$", ignore.case = TRUE, full.names = TRUE)
        unmix_threads <- if (per_file) fit_workers(raw_paths, threads) else threads
        add_log(if (per_file)
          sprintf("Unmixing %d file(s), %d at a time...", length(raw_paths),
                  min(length(raw_paths), unmix_threads))
          else sprintf("Unmixing on %d cores...", unmix_threads))

        run_parallel("Unmixing", add_log, function(parallel)
          with_session_wd(
            unmix.folder(
              fcs.dir          = raw_tmp_dir,
              spectra          = spectra_rv(),
              flow.control     = flow.control_rv(),
              asp              = asp,
              method           = input$method,
              af.spectra       = af_spectra,
              spectra.variants = spectra_variants,
              speed            = speed,
              output.dir       = unmixed_dir,
              parallel         = parallel,
              threads          = unmix_threads
            )
          )
        )

        incProgress(0.4)
      })

      add_log("Unmixing completed.")

      analysis_status("Unmixing completed")

      unmixed_files_rv(
        list.files(unmixed_dir, pattern = "\\.fcs$", ignore.case = TRUE)
      )
      load_ff(input$file)

    }, on_error = function(msg) {
      analysis_status("Unmixing failed")
      unmix_error_rv(paste0("<span style='color:red; font-weight:bold;'>ERROR:</span><br>", msg))
    })
  })

  output$analysis_status <- renderText({
    analysis_status()
  })

  output$gating_ui <- renderUI({
    if (is.null(spectra_rv()))
      return(box(width = 12,
                 HTML("<h4><span style='background-color:#fdfd96;'>STEP 1 :</span> Prepare the controls first (Control preparation tab)</h4>")))
    if (length(unmixed_files_rv()) == 0)
      return(box(width = 12,
                 HTML("<h4><span style='background-color:#fdfd96;'>STEP 1 :</span> Run the unmixing first (Start above)</h4>")))
    box(width = 12,
        HTML("<h4><span style='background-color:#fdfd96;'>STEP 1 :</span> Bidimensional gating</h4>"),
        uiOutput("select_file"))
  })

  output$progressBoxes <- renderUI({
    NULL
  })

  # ===========================================================
  # PLOTTING
  # ===========================================================
  output$select_file <- renderUI({
    files <- unmixed_files_rv()
    req(length(files) > 0)

    fluidRow(
      column(4,
             selectInput("file", "Unmixed file (working file)", choices = files,
                         selected = isolate(input$file)),
             uiOutput("marker1_ui"),
             uiOutput("marker2_ui")),
      column(8, plotOutput("plot", width = "650px", height = "650px"))
    )
  })

  # (re)reads the working file: on a new choice, and after each unmixing (which may rewrite it)
  load_ff <- function(f) {
    if (isTRUE(f %in% unmixed_files_rv()))
      ff_rv(read.FCS(
        file.path(unmixed_dir, f),
        truncate_max_range = FALSE
      ))
  }

  observeEvent(input$file, {
    req(input$file)
    load_ff(input$file)
  })

  output$marker1_ui <- renderUI({
    req(ff_rv())
    ch <- axis_choices(ff_rv())
    selectInput("marker1", "X axis", choices = ch, selected = ch[1])
  })

  output$marker2_ui <- renderUI({
    req(ff_rv())
    ch <- axis_choices(ff_rv())
    selectInput("marker2", "Y axis", choices = ch, selected = ch[min(2, length(ch))])
  })

  output$plot <- renderPlot({
    req(ff_rv(), input$marker1, input$marker2, asp_rv())
    set.seed(1)
    draw_biplot(subsample(ff_rv(), PLOT_N), input$marker1, input$marker2, label_ff = ff_rv())
  })

  # ===========================================================
  # DOWNLOAD ZIP OF UNMIXED FILES
  # ===========================================================
  output$download_unmixed_ui <- renderUI({
    req(length(unmixed_files_rv()) > 0)
    downloadButton("download_unmixed", label = "Download unmixed files",
                   class = NULL, icon = shiny::icon("download"))
  })

  output$download_unmixed <- downloadHandler(
    filename = function() paste0("autospectral_unmixed_", Sys.Date(), ".zip"),
    content  = function(file) zip::zipr(file, file.path(unmixed_dir, unmixed_files_rv()))
  )

  # ===========================================================
  # DOWNLOAD FIGURES
  # ===========================================================

  # Landscape pages with a grid of figures (per_page = nrow * ncol).
  # `exclude` is a regex on the file path; `on_page` is called once per page.
  create_image_pdf <- function(dir,
                               outfile = file.path(dir, "Autospectral_Report.pdf"),
                               exclude = NULL,
                               ncol = 2, nrow = 2,
                               on_page = function(done, total) NULL) {

    imgs <- list.files(
      dir,
      pattern = "\\.jpe?g$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (!is.null(exclude))
      imgs <- imgs[!grepl(exclude, imgs, ignore.case = TRUE)]

    # tall figures are represented by their tiles in the report
    tiles <- list.files(file.path(dir, "pdf_tiles"), pattern = "\\.jpe?g$")
    if (length(tiles) > 0) {
      tiled <- unique(sub(" part[0-9]+\\.jpg$", "", tiles, ignore.case = TRUE))
      imgs <- imgs[!(basename(imgs) %in% paste0(tiled, ".jpg") &
                       basename(dirname(imgs)) == "figure_spectra")]
    }

    if (length(imgs) == 0)
      return(NULL)

    per_page <- ncol * nrow
    pages <- split(imgs, ceiling(seq_along(imgs) / per_page))

    pdf(outfile, width = 14, height = 8.5)
    on.exit(dev.off(), add = TRUE)

    for (i in seq_along(pages)) {
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow, ncol)))
      for (k in seq_along(pages[[i]])) {
        img <- pages[[i]][k]
        pushViewport(viewport(
          layout.pos.row = (k - 1) %/% ncol + 1,
          layout.pos.col = (k - 1) %% ncol + 1
        ))
        grid.raster(readJPEG(img), y = unit(0.47, "npc"), height = unit(0.9, "npc"))
        grid.text(
          basename(img),
          y = unit(0.97, "npc"),
          gp = gpar(fontsize = 10, fontface = "bold")
        )
        popViewport()
      }
      popViewport()
      on_page(i, length(pages))
    }

    outfile
  }

  # Zip with a per-file progress bar (zip::zipr_append adds one file at a time).
  zip_with_progress <- function(zipfile, rel_files) {
    with_session_wd({
      for (i in seq_along(rel_files)) {
        if (i == 1) zip::zipr(zipfile, rel_files[i]) else zip::zipr_append(zipfile, rel_files[i])
        incProgress(0.3 / length(rel_files), detail = paste("Zipping", basename(rel_files[i])))
      }
    })
  }

  # All figures: the PDF report plus every figure JPEG, no FCS files.
  build_figures_zip <- function(file) {

    withProgress(message = "Preparing download", value = 0, {

      pdf_report <- create_image_pdf(
        session_dir,
        on_page = function(done, total) {
          incProgress(0.6 / total, detail = paste("Report page", done, "/", total))
        }
      )
      if (is.null(pdf_report)) incProgress(0.6)

      figs <- unlist(lapply(figure_dirs, function(d) {
        list.files(file.path(session_dir, d), full.names = TRUE, recursive = TRUE)
      }))
      files_to_zip <- c(figs, if (!is.null(pdf_report) && file.exists(pdf_report)) pdf_report)

      validate(need(length(files_to_zip) > 0, "No figures found"))

      incProgress(0.1, detail = "Zipping")
      rel_files <- sub(
        paste0("^", normalizePath(session_dir), .Platform$file.sep),
        "",
        normalizePath(files_to_zip)
      )
      zip_with_progress(file, rel_files)
    })
  }

  output$download_figures_ui <- renderUI({
    req(spectra_rv())
    downloadButton("download_figures", label = "Download all figures",
                   class = NULL, icon = shiny::icon("download"))
  })

  output$download_figures <- downloadHandler(
    filename = function() paste0("autospectral_figures_", Sys.Date(), ".zip"),
    content  = function(file) build_figures_zip(file)
  )

  # ===========================================================
  # UNMIXING / COMPENSATION MATRIX
  # ===========================================================
  output$download_matrix_ui <- renderUI({
    req(spectra_rv())
    downloadButton("download_matrix", label = "Download all unmixing matrices",
                   class = NULL, icon = shiny::icon("download"))
  })

  output$download_matrix <- downloadHandler(
    filename = function() paste0("autospectral_unmixing_matrix_", Sys.Date(), ".zip"),
    content  = function(file) {
      sp <- spectra_rv()
      req(sp)
      S  <- t(sp)                                   # detectors x fluorophores
      U  <- solve(crossprod(S)) %*% t(S)            # OLS unmixing matrix
      dimnames(U) <- list(rownames(sp), colnames(sp))

      out <- file.path(session_dir, "matrix_export")
      unlink(out, recursive = TRUE, force = TRUE)
      dir.create(out, recursive = TRUE)
      write.csv(U, file.path(out, "unmixing_matrix_OLS.csv"))
      write.csv(sp, file.path(out, "spillover_signature_matrix.csv"))
      writeLines(c(
        "unmixing_matrix_OLS.csv: fluorophores x detectors, (S'S)^-1 S' (OLS).",
        "spillover_signature_matrix.csv: fluorophores x detectors spectra (signatures).",
        "The per-cell weights of the WLS/FastPoisson/AutoSpectral unmixing are not included."
      ), file.path(out, "README.txt"))

      tp <- list.files(file.path(session_dir, "table_spectra"),
                       pattern = "unmixing_matrix.*\\.csv$", full.names = TRUE)
      if (length(tp)) file.copy(tp, out)

      with_session_wd(zip::zipr(file, file.path("matrix_export", list.files(out))))
    }
  )

  # ===========================================================
  # IMAGE MANAGEMENT
  # ===========================================================
  # figures AutoSpectral wrote in a figure folder during the preparation
  folder_images <- function(folder) {
    list.files(
      file.path(session_dir, folder),
      pattern = "\\.jpe?g$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
  }

  # same fixed values as the "Show n entries" menu of the Parameters table
  img_n_choices <- c(10, 25, 50, 100)
  # smallest choice that still shows n figures (the largest one beyond that)
  img_n_default <- function(n) {
    img_n_choices[min(sum(img_n_choices < n) + 1, length(img_n_choices))]
  }

  # Re-rendered after each preparation (the figures on disk changed); the folder and the
  # display choices are kept. The grid shows every figure_gate plot: one per control.
  output$heatmap_ui <- renderUI({
    req(spectra_rv())
    prep_run_rv()
    viewer_dirs <- setdiff(figure_dirs, c("table_spectra", "figure_spectral_ribbon", "figure_spectra"))  # still in the download
    existing <- viewer_dirs[dir.exists(file.path(session_dir, viewer_dirs))]
    req(length(existing) > 0)

    folder <- isolate(input$img_folder)
    if (!isTRUE(folder %in% existing))
      folder <- if ("figure_gate" %in% existing) "figure_gate" else existing[1]
    imgs <- folder_images(folder)

    fluidRow(
      column(3, selectInput("img_folder", "Figure folder", choices = existing, selected = folder)),
      column(3, radioButtons("img_display", "Display", choices = c("Grid", "Single"),
                             selected = isolate(input$img_display) %||% "Grid", inline = TRUE)),
      column(3, selectInput("img_n", "Number of figures to plot", choices = img_n_choices,
                            selected = isolate(input$img_n) %||%
                              img_n_default(length(folder_images("figure_gate"))))),
      column(3, selectInput("img_select", "Figure", choices = setNames(imgs, basename(imgs)),
                            selected = isolate(input$img_select))),
      column(12, uiOutput("img_viewer"))
    )
  })

  observeEvent(input$img_folder, {

    req(input$img_folder)

    imgs <- folder_images(input$img_folder)

    updateSelectInput(
      session,
      "img_select",
      choices = if (length(imgs) > 0) setNames(imgs, basename(imgs)) else c("No images found" = "")
    )
  })

  img_tag <- function(path, style) {
    tags$img(
      src = base64enc::dataURI(file = path, mime = "image/jpeg"),
      title = basename(path),
      style = style
    )
  }

  output$img_viewer <- renderUI({

    req(input$img_folder)
    prep_run_rv()
    imgs <- folder_images(input$img_folder)

    if (identical(input$img_display, "Grid") && length(imgs) > 1) {
      return(tags$div(
        style = paste("display:grid;",
                      "grid-template-columns:repeat(auto-fill, minmax(380px, 1fr));",
                      "gap:10px;"),
        lapply(head(imgs, as.integer(input$img_n %||% img_n_choices[1])), function(p) {
          tags$div(
            style = "text-align:center;",
            tags$div(style = "font-weight:bold;", basename(p)),
            img_tag(p, paste("max-width:100%; height:auto;",
                             "border-radius:10px; border:1px solid #ccc;"))
          )
        })
      ))
    }

    req(input$img_select)

    validate(
      need(file.exists(input$img_select), "Image not found")
    )

    tags$div(
      style = "text-align:center;",
      img_tag(input$img_select, paste(
        "max-width:1200px;", "max-height:800px;", "width:auto;", "height:auto;",
        "border-radius:10px;", "border:1px solid #ccc;"
      ))
    )
  })

}
