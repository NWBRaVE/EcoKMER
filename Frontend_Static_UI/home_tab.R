# File: home_tab.R - Static home tab layout (mode selection + load controls)
# TODO: Consider extracting inline CSS to shared stylesheet for reuse / minification.

home_tab <- function() { # FUNCTION: build Home tab UI
   shiny::tagList(
      shiny::tags$style(shiny::HTML(
         ".mode-card {border:1px solid #ddd;border-radius:6px;padding:10px;margin-bottom:10px;background:#fafafa;}
             .status-badge {display:inline-block;padding:2px 8px;border-radius:12px;font-size:11px;font-weight:600;letter-spacing:.5px;}
         .status-waiting {background:#e0e0e0;color:#444;}
             .status-loading {background:#fff3cd;color:#7a6222;}
             .status-online {background:#c8f3c8;color:#256029;}
             .status-offline {background:#ffe0b2;color:#8a4b00;}
             .status-error {background:#ffcdd2;color:#b71c1c;}
             .help-note {font-size:11px;color:#666;}"
      )),
      shiny::tabPanel(
         "Home",
         shiny::h3("EcoKMER"),
         shiny::p(class = "help-note", "A lightweight interface for loading, harmonizing, and exploring environmental sequencing metadata."),
         shinyWidgets::radioGroupButtons(
            inputId = "data_mode",
            label = NULL,
            choices = c("Online (DataFed)" = "online", "Offline" = "offline"),
            justified = TRUE
         ),
         shiny::fluidRow(
            shiny::column(
               6,
               shiny::div(
                  class = "mode-card",
                  shiny::strong("Online"),
                  shiny::div(class = "help-note", "Fetch latest records (requires credentials)")
               )
            ),
            shiny::column(
               6,
               shiny::div(
                  class = "mode-card",
                  shiny::strong("Offline"),
                  shiny::div(class = "help-note", "Use existing harmonized CSV")
               )
            )
         ),
         shiny::uiOutput("data_source_panel"),
         shiny::fluidRow(
            shiny::column(
               8,
               shiny::actionButton("pull_records", "Load Harmonized Dataset", class = "btn btn-primary btn-sm", title = "Build in-app dataset from downloaded (online) or selected (offline) source"),
               shiny::span(" "),
               shiny::uiOutput("connection_status_badge", inline = TRUE)
            ),
            shiny::column(4, align = "right", tagList(
               shiny::uiOutput("dataset_summary")
            ))
         )
      )
   )
}
