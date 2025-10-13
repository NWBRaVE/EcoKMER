# File: home_rm.R - Source mode selection, authentication, and dataset loading

# OUTPUT data_source_panel: render online creds or offline file picker
output$data_source_panel <- renderUI({
   req(input$data_mode)
   if (input$data_mode == "online") {
      tagList(
         div(
            class = "mode-card",
            strong("Credentials"),
            textInput("dfuser", NULL, placeholder = "DataFed Username"),
            passwordInput("dfpwd", NULL, placeholder = "Password"),
            div(
               style = "display:flex;gap:6px;flex-wrap:wrap;align-items:center;",
               actionButton("authenticate_datafed", "Authenticate", class = "btn btn-sm btn-secondary", title = "Login to DataFed using provided credentials"),
               actionButton(
                  "fetch_records",
                  "Fetch Records",
                  class = "btn btn-sm btn-info",
                  title = "Fetch DataFed records (Globus endpoint UUID is defined in www/DataFedSettings.cfg)"
               ),
               actionButton("logout_datafed", "Logout", class = "btn btn-sm btn-outline-danger", title = "End authenticated session (credentials cleared in-memory only)")
            )
         )
      )
   } else {
      tagList(
         div(
            class = "mode-card",
            strong("Offline Dataset"),
            fileInput("offline_file", NULL, placeholder = NULL, accept = c(".csv"), buttonLabel = "Browse CSV", multiple = FALSE),
            div(class = "help-note", "If no file selected uses default Data/harmonized.csv")
         )
      )
   }
})

# AUTH: authenticate only (no record fetch)
observeEvent(input$authenticate_datafed, {
   user <- trimws(input$dfuser %||% "")
   pass <- input$dfpwd %||% ""
   if (!nzchar(user) || !nzchar(pass)) {
      showNotification("Enter DataFed username and password.", type = "error")
      return()
   }
   prior_user <- getOption("ecokmer.datafed_user", default = "")
   if (nzchar(prior_user) && !identical(prior_user, user)) {
      Sys.unsetenv(c("DF_USER", "DF_PASS"))
      try(py_run_string("import os; os.environ.pop('DF_USER', None); os.environ.pop('DF_PASS', None)"), silent = TRUE)
   }
   Sys.setenv(DF_USER = user, DF_PASS = pass)
   py_run_string(sprintf(
      "import os; os.environ['DF_USER']=%s; os.environ['DF_PASS']=%s",
      jsonlite::toJSON(user, auto_unbox = TRUE),
      jsonlite::toJSON(pass, auto_unbox = TRUE)
   ))
   options(ecokmer.datafed_logged_in = TRUE, ecokmer.datafed_user = user)
   showNotification(paste0("Authenticated as ", user, "."), type = "message")
})

# FETCH: run record query script (no re-auth; assumes authenticated)
observeEvent(input$fetch_records, {
   if (!isTRUE(getOption("ecokmer.datafed_logged_in"))) {
      showNotification("Authenticate first.", type = "warning")
      return()
   }
   # simple progress wrapper
   withProgress(message = "Fetching DataFed records", value = 0, {
      incProgress(0.05, detail = "Preparing")
      py_code <- "\nimport sys, os, logging\nroot = logging.getLogger()\nif len(root.handlers) > 1:\n    for h in root.handlers[1:]:\n        try: root.removeHandler(h)\n        except Exception: pass\nprint('Fetching records for user:', os.environ.get('DF_USER'))\nsys.argv = ['Scripts/record_query.py', '--query', 'www/ecokmer_sample_query.json', '--merge-dependencies']\nexec(open('Scripts/record_query.py').read())\n"
      tryCatch(
         {
            incProgress(0.30, detail = "Running Python query")
            py_run_string(py_code)
            incProgress(0.90, detail = "Writing CSVs")
            showNotification("Record fetch completed.", type = "message")
         },
         error = function(e) {
            err <- tryCatch(reticulate::py_last_error(), error = function(.) NULL)
            if (!is.null(err) && identical(err$type, "SystemExit")) {
               val <- as.character(err$value)
               if (grepl("\\b0\\b", val)) {
                  message("[datafed] Suppressing benign SystemExit(0) from record_query.py")
                  showNotification("Record fetch completed (SystemExit 0).", type = "message")
                  return(invisible(NULL))
               }
            }
            showNotification(paste0("Record fetch failed: ", e$message), type = "error", duration = NULL)
         }
      )
      incProgress(1)
   })
})

# LOGOUT: clear env and state
observeEvent(input$logout_datafed, {
   Sys.unsetenv(c("DF_USER", "DF_PASS"))
   try(py_run_string("import os; os.environ.pop('DF_USER', None); os.environ.pop('DF_PASS', None)"), silent = TRUE)
   options(ecokmer.datafed_logged_in = FALSE, ecokmer.datafed_user = NULL)
   showNotification("Logged out of DataFed.", type = "message")
})

connection_status <- reactiveVal(list(state = "waiting", msg = "Waiting")) # REACTIVE load/auth status

output$connection_status_badge <- renderUI({ # OUTPUT colored status badge
   st <- connection_status()
   cls <- switch(st$state,
      waiting = "status-badge status-waiting",
      loading = "status-badge status-loading",
      online = "status-badge status-online",
      offline = "status-badge status-offline",
      error = "status-badge status-error",
      "status-badge status-waiting"
   )
   span(class = cls, st$msg)
})

harmonized_data <- reactiveVal(NULL) # REACTIVE current harmonized dataset
last_loaded_files <- reactiveVal(character()) # REACTIVE vector of last file paths loaded
active_data_mode <- reactiveVal(NULL) # REACTIVE loaded data mode ("online"/"offline")

output$data_mode_badge <- renderUI({ # OUTPUT global data mode badge
   mode <- active_data_mode()
   # Guard: mode can be NULL or length != 1 during initial render or error states
   if (is.null(mode) || length(mode) != 1 || is.na(mode)) {
      return(span(class = "status-badge status-waiting", "Not Loaded"))
   }
   label <- switch(mode,
      online = "Online (DataFed)",
      offline = "Offline (Local)",
      `Missing files` = "Missing Files", # future-safe placeholder if state name misused
      NULL
   )
   if (is.null(label)) {
      span(class = "status-badge status-waiting", "Not Loaded")
   } else if (identical(mode, "online")) {
      span(class = "status-badge status-online", label)
   } else if (identical(mode, "offline")) {
      span(class = "status-badge status-offline", label)
   } else {
      span(class = "status-badge status-error", label)
   }
})

## (Reverted) Removed dynamic button disabling logic.

output$dataset_summary <- renderUI({ # OUTPUT structured summary (multi-line)
   dat <- harmonized_data()
   if (is.null(dat)) {
      return(NULL)
   }
   n_rows <- nrow(dat)
   cols <- ncol(dat)
   key_cols <- intersect(c("Parent.ID", "Owen.s.Notation.for.sequencing", "Date"), names(dat))
   files <- last_loaded_files()
   div(
      class = "help-note",
      div(strong("Loaded:"), sprintf(" %s rows · %s columns", n_rows, cols)),
      div(strong("Keys:"), if (length(key_cols)) paste(key_cols, collapse = ", ") else "(none)"),
      if (length(files)) {
         tagList(
            div(strong("Files:")),
            lapply(files, function(f) div(code(normalizePath(f, winslash = "/", mustWork = FALSE))))
         )
      } else {
         NULL
      }
   )
})

observeEvent(input$pull_records, { # OBSERVE load button: fetch online or offline data
   req(input$data_mode)
   connection_status(list(state = "loading", msg = "Loading"))
   loaded_paths <- character()
   if (input$data_mode == "online") {
      # Auth guard (must authenticate first)
      if (!isTRUE(getOption("ecokmer.datafed_logged_in"))) {
         connection_status(list(state = "error", msg = "Auth required"))
         showNotification("Please authenticate with DataFed before loading online data.", type = "warning")
         return()
      }
      # Use centralized constants (see DATAFED_RECORD_PATHS in global.R)
      p_ext <- DATAFED_RECORD_PATHS$ext
      p_seq <- DATAFED_RECORD_PATHS$seq
      p_par <- DATAFED_RECORD_PATHS$par
      expected <- unname(unlist(DATAFED_RECORD_PATHS))
      missing <- expected[!file.exists(expected)]
      if (length(missing)) {
         connection_status(list(state = "error", msg = "Missing files"))
         # Map missing full paths back to role keys for clearer guidance
         role_map <- vapply(names(DATAFED_RECORD_PATHS), function(k) DATAFED_RECORD_PATHS[[k]], character(1))
         miss_roles <- names(role_map)[match(missing, role_map)]
         miss_roles[is.na(miss_roles)] <- "(unknown-role)"
         pretty_list <- paste(sprintf("%s -> %s", miss_roles, basename(missing)), collapse = "; ")
         showNotification(paste0(
            "Required DataFed file(s) not found. Authenticate then Fetch Records. Missing: ",
            pretty_list
         ), type = "error", duration = NULL)
         return()
      }
      loaded_paths <- expected
      ext <- read.csv(p_ext) %>%
         dplyr::rename(
            record_id_ext = record_id,
            title_ext = title,
            alias_ext = alias,
            tags_ext = tags,
            dependencies_ext = dependencies
         )
      seq <- read.csv(p_seq) %>%
         dplyr::rename(
            record_id_seq = record_id,
            title_seq = title,
            alias_seq = alias,
            tags_seq = tags,
            dependencies_seq = dependencies
         )
      par <- read.csv(p_par) %>%
         dplyr::rename(
            record_id_par = record_id,
            title_par = title,
            alias_par = alias,
            tags_par = tags,
            dependencies_par = dependencies
         ) %>%
         dplyr::filter(!is.na(Parent.ID) & Parent.ID != "")
      # Assert join key integrity before and after to detect silent row loss.
      pre_n_ext <- nrow(ext)
      pre_n_seq <- nrow(seq)
      pre_n_par <- nrow(par)
      # REQUIRED join keys presence
      required_keys <- c("Owen.s.Notation.for.sequencing", "SampleName", "Parent.ID")
      missing_in_ext <- setdiff("Owen.s.Notation.for.sequencing", names(ext))
      missing_in_seq <- setdiff("SampleName", names(seq))
      missing_in_par <- setdiff("Parent.ID", names(par))
      if (length(missing_in_ext) || length(missing_in_seq) || length(missing_in_par)) {
         connection_status(list(state = "error", msg = "Join key missing"))
         showNotification(paste0(
            "Join key(s) missing. ext missing: ", paste(missing_in_ext, collapse = ","),
            " | seq missing: ", paste(missing_in_seq, collapse = ","),
            " | par missing: ", paste(missing_in_par, collapse = ",")
         ), type = "error", duration = NULL)
         return()
      }

      dat_df <- ext %>%
         dplyr::left_join(seq, by = c("Owen.s.Notation.for.sequencing" = "SampleName"))

      post_join1 <- nrow(dat_df)
      if (post_join1 < pre_n_ext) {
         dropped <- pre_n_ext - post_join1
         showNotification(paste0("Warning: First join dropped ", dropped, " row(s) from ext (", post_join1, "/", pre_n_ext, ")."), type = "warning")
      }

      dat_df <- dat_df %>% dplyr::left_join(par, by = "Parent.ID")
      post_join2 <- nrow(dat_df)
      # Second join shouldn't reduce rows; if nrow decreased, signal.
      if (post_join2 < post_join1) {
         dropped2 <- post_join1 - post_join2
         showNotification(paste0("Warning: Second join dropped ", dropped2, " row(s) after Parent.ID merge (", post_join2, "/", post_join1, ")."), type = "warning")
      }

      # Optional integrity diagnostics (debug mode): uniqueness of key columns
      if (isTRUE(getOption("ecokmer.debug", FALSE))) {
         if ("Owen.s.Notation.for.sequencing" %in% names(ext)) {
            dup_ext <- sum(duplicated(ext$Owen.s.Notation.for.sequencing))
            if (dup_ext) message("[join-diag] ext duplicate Owen.s.Notation.for.sequencing: ", dup_ext)
         }
         if ("SampleName" %in% names(seq)) {
            dup_seq <- sum(duplicated(seq$SampleName))
            if (dup_seq) message("[join-diag] seq duplicate SampleName: ", dup_seq)
         }
         if ("Parent.ID" %in% names(par)) {
            dup_par <- sum(duplicated(par$Parent.ID))
            if (dup_par) message("[join-diag] par duplicate Parent.ID: ", dup_par)
         }
      }

      # Clean sentinel strings to NA
      dat_df <- clean_string_sentinels(dat_df)

      # Normalize deg symbol
      dat_df$Site.Coordinates <- stringr::str_replace_all(dat_df$Site.Coordinates, "deg", "°")

      numeric_columns <- c(
         "diversity", "kappa", "max_cyano.amoeba", "max_cyanophage.cyano",
         "max_pro.amoeba", "max_proc_phage.cyano", "max_syn.amoeba",
         "max_syn_phage.cyano", "modelR"
      )
      dat_df[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(dat_df[[x]]))
      connection_status(list(state = "online", msg = "Online (DataFed)"))
      active_data_mode("online")
   } else if (input$data_mode == "offline") {
      # Use uploaded file if present else fallback
      if (!is.null(input$offline_file) && !is.null(input$offline_file$datapath) && file.exists(input$offline_file$datapath)) {
         data_analysis_file_path <- input$offline_file$datapath
      } else {
         data_analysis_file_path <- "./Data/harmonized.csv"
      }
      if (!file.exists(data_analysis_file_path)) { # GOTCHA: file may be missing
         connection_status(list(state = "error", msg = "File missing"))
         showNotification(paste0("Offline file not found: ", data_analysis_file_path), type = "error")
         return()
      }
      dat_df <- tryCatch(read.csv(data_analysis_file_path, comment.char = "#", stringsAsFactors = FALSE), # TODO: consider fread for speed
         error = function(e) {
            connection_status(list(state = "error", msg = "Read failed"))
            showNotification(paste0("Failed to read file: ", e$message), type = "error")
            return(NULL)
         }
      )
      if (is.null(dat_df)) {
         return()
      }
      loaded_paths <- data_analysis_file_path
      connection_status(list(state = "offline", msg = "Offline (Local)"))
      active_data_mode("offline")
   }
   harmonized_data(dat_df)
   last_loaded_files(loaded_paths)
   if (length(loaded_paths)) {
      showNotification(paste0("Loaded ", length(loaded_paths), " file(s)."), type = "message")
   }
})

# (legacy shinyjs button toggle removed)
