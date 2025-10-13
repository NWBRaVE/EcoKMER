# File: harmonized_analysis_tab.R - Static layout for harmonized dataset filtering & visualization
# TODO: Many hard-coded vh/px heights; consider a flexbox layout for improved responsiveness + externalize large CSS block.

harmonized_analysis_tab <- function() { # FUNCTION: build Harmonized Analysis tab UI
   div(
      tags$style(HTML(paste0(
         ".ui-resizable-e {background:#ddd;width:3px !important;right:-2px;cursor:col-resize;} .ui-resizable-e:hover {background:#007bff;} .resizable-sidebar {border-right:1px solid #ddd;background:#fafafa;}",
         "#select_columns + .selectize-control.multi .selectize-input {min-height:4.2em !important; height:auto !important; max-height:11em; overflow-y:auto; padding-top:4px; padding-bottom:4px; line-height:1.15; display:flex; flex-wrap:wrap;}",
         "#select_columns + .selectize-control.multi .selectize-input > .item {margin:2px 4px;padding:2px 6px;}",
         "#select_columns + .selectize-control.multi .selectize-input::-webkit-scrollbar {width:8px;}",
         "#select_columns + .selectize-control.multi .selectize-input::-webkit-scrollbar-track {background:#f5f5f5;}",
         "#select_columns + .selectize-control.multi .selectize-input::-webkit-scrollbar-thumb {background:#bbb;border-radius:4px;}",
         "#select_columns + .selectize-control.multi .selectize-input::-webkit-scrollbar-thumb:hover {background:#999;}",
         ".resizable-sidebar .form-group:has(.irs) {padding-left:12px;padding-right:12px;}",
         ".resizable-sidebar .form-group:not(:has(.irs)) {max-width:340px;}",
         "@media (max-width: 1200px){ .resizable-sidebar .form-group:not(:has(.irs)) {max-width:100%;} }",
         ".resizable-sidebar .irs, .resizable-sidebar .irs-with-grid {width:100% !important;}",
         ".resizable-sidebar .slider-wrapper {width:100%;}",
         "[id$='select_columns'] + .selectize-control.multi .selectize-input {min-height:4.4em !important; height:auto !important; max-height:11em; overflow-y:auto; display:flex; flex-wrap:wrap; padding-top:6px; padding-bottom:6px;}"
      ))),
      shiny::fluidRow(
         shiny::tabPanel(
            "Data Analysis",
            # Use splitLayout for interactive resizing instead of sidebarLayout
            shiny::splitLayout(
               cellWidths = c("35%", "65%"), # Initial proportions
               cellArgs = list(style = "padding: 0px;"), # Remove default padding
               # Left panel (resizable)
               jqui_resizable(
                  shiny::div(
                     id = "harmonized_sidebar",
                     class = "resizable-sidebar",
                     style = "height: 90vh; overflow-y: auto; padding: 15px;",
                     shiny::div(
                        style = "display:flex;flex-direction:column;align-items:flex-start;gap:4px;",
                        shiny::h3("Harmonized Dataset", style = "margin-bottom:2px;"),
                        shiny::div(shiny::uiOutput("data_mode_badge"))
                     ),
                     shiny::h4("Select Columns of Interest"),
                     shiny::uiOutput("filter_cols_ui"),
                     shiny::uiOutput("filter_inputs_ui"),
                     shiny::h5("Note: Some records are large (~1GB). Download only what's needed."),
                     # Conditional UI based on data mode
                     shiny::uiOutput("download_buttons_ui")
                  ),
                  options = list(
                     handles = "e", # Only allow resizing from the right edge
                     minWidth = 280,
                     maxWidth = 800,
                     grid = c(10, 10), # Snap to 10px grid for smoother resizing
                     alsoResize = FALSE
                  )
               ),
               # Right panel (main content) - single tabset including map
               shiny::div(
                  style = "height: 90vh; padding: 10px; overflow: hidden;",
                  shiny::tabsetPanel(
                     id = "harmonized_tables_tabs",
                     shiny::tabPanel(
                        "Full Dataset",
                        div(
                           style = "height: 80vh; overflow-y:auto;",
                           DT::dataTableOutput("harmonized_gt")
                        )
                     ),
                     shiny::tabPanel(
                        "Filtered Dataset",
                        div(
                           style = "height: 80vh; overflow-y:auto;",
                           DT::dataTableOutput("filtered_harmonized_gt")
                        )
                     ),
                     shiny::tabPanel(
                        "Line Plot",
                        div(
                           style = "height: 80vh; overflow-y:auto;",
                           shiny::uiOutput("plot_selector_ui"),
                           plotly::plotlyOutput("filtered_plot", height = "520px")
                        )
                     ),
                     shiny::tabPanel(
                        "Box Plot",
                        div(
                           style = "height: 80vh; overflow-y:auto;",
                           shiny::uiOutput("plot_selector_boxplot_ui"),
                           plotly::plotlyOutput("boxplot_filt", height = "520px")
                        )
                     ),
                     shiny::tabPanel(
                        "Map",
                        div(
                           style = "height: 80vh; overflow:hidden; padding:5px;",
                           div(
                              style = "height:100%; border:1px solid #ddd; border-radius:4px; padding:5px;",
                              shiny::h4("Interactive Map", style = "margin-top:0; margin-bottom:10px;"),
                              div(
                                 style = "height: calc(100% - 85px);",
                                 leaflet::leafletOutput("map2", height = "100%")
                              ),
                              shiny::div(
                                 style = "height:55px; margin-top:5px;",
                                 shiny::fluidRow(
                                    shiny::column(4, shiny::actionButton("clear_map_selection", "Clear Map Selection", class = "btn btn-sm btn-warning")),
                                    shiny::column(4, shiny::downloadButton("download_map_selection", "Download Map Selection", class = "btn btn-sm btn-primary")),
                                    shiny::column(4, shiny::textOutput("map_selection_status"))
                                 )
                              )
                           )
                        )
                     )
                  )
               )
            )
         )
      ),
      # JS to force sliders (ionRangeSlider) to recalc width after sidebar resize
      tags$script(HTML("$(function(){ var sidebar = $('#harmonized_sidebar'); var obs; sidebar.resizable({ resize: function(){ if(window.ecokmerResizeTO) clearTimeout(window.ecokmerResizeTO); window.ecokmerResizeTO = setTimeout(function(){ $(window).trigger('resize'); },120); }}); });"))
   )
}
