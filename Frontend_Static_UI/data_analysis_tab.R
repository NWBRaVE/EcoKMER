# File: data_analysis_tab.R - Static layout for JSON metrics ingestion & plotting
# TODO: Consolidate repeated fileInput help text; consider validation UI for JSON schema before enabling run buttons.

data_analysis_tab <- function() { # FUNCTION: build Data Analysis tab UI
   div(
      fluidRow(
         tabPanel(
            "Data Analysis",
            sidebarLayout(
               sidebarPanel(
                  shiny::div(
                     style = "display:flex;align-items:center;gap:10px;",
                     shiny::h3("Create or Upload a JSON File for Analyses"),
                     shiny::uiOutput("data_mode_badge")
                  ),
                  fluidRow(
                     shiny::column(
                        12,
                        shiny::h4("If processing NPO files, upload NPO JSON file here:"),
                        shiny::fileInput("upload_npo_json_file_data_analysis", "Upload NPO JSON file", accept = ".json"),
                        shiny::verbatimTextOutput("json_npo_input_file_data_analysis"),
                        shiny::actionButton("run_npo_processing", "Process NPO Data"),
                        shiny::downloadButton("downloadNPO_data", "Download NPO Dataset"),
                        shiny::h4("Upload a JSON Configuration File"),
                        shiny::fileInput("upload_json_file_data_analysis", "Upload a JSON file", accept = ".json"),
                        shiny::verbatimTextOutput("json_input_file_data_analysis"),
                        shiny::actionButton("run_diversity", "Run Process")
                     )
                  )
               ),
               shiny::mainPanel(
                  plotly::plotlyOutput("boxplot_diversity"),
                  plotly::plotlyOutput("lineplot_diversity")
               )
            )
         )
      )
   )
}
