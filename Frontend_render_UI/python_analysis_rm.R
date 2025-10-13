# File: python_analysis_rm.R - DataFed auth and Python record query execution via reticulate
# NOTE: When executing full Python scripts via exec(open(...).read()), their main() may call sys.exit(code),
# which reticulate surfaces as a SystemExit error. We explicitly downgrade SystemExit(0) to success to avoid
# confusing benign completion messages. Long-term: replace exec with importing a callable module.

`%||%` <- function(a, b) if (is.null(a)) b else a # HELPER null-coalescing

datafed_user <- reactiveVal("") # REACTIVE current DataFed user
datafed_query_running <- reactiveVal(FALSE) # REACTIVE flag: Python query in progress

output$datafed_status <- renderText({ # OUTPUT auth status line
   u <- datafed_user()
   if (!nzchar(u)) {
      "DataFed: Not signed in"
   } else {
      paste0("DataFed: signed in as ", u)
   }
})

output$datafed_query_activity <- renderUI({ # OUTPUT small activity indicator while query runs
   if (isTRUE(datafed_query_running())) {
      div(class = "help-note", style = "color:#555;font-size:11px;", "Query running...")
   } else {
      NULL
   }
})

# NOTE: Legacy combined authenticate+fetch observer removed (logic moved to home_rm.R with separate buttons)
