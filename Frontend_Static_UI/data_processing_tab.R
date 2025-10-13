# File: data_processing_tab.R - Static layout for data processing (map + raw table)
# TODO: Inline cluster marker CSS could be centralized; map height currently fixed (600px) – consider responsive scaling.

data_processing_tab <- function() { # FUNCTION: build Data Processing tab UI
   div(
      fluidRow(
         tabPanel(
            title = "Data Processing",
            sidebarLayout(
               sidebarPanel(
                  h3("Select Samples/Sequences of Interest"),
                  fluidRow(
                     column(
                        12,
                        h4("Spatial Map Selections"),
                        downloadButton("downloadData", "Download Selected Data"),
                        actionButton("clearSelection", "Clear Selection", class = "btn btn-warning"),
                        br(), br(),
                        div(
                           style = "max-height: 600px; overflow-y: auto;",
                           tableOutput("sampleTable")
                        ),
                        uiOutput("download_options_UI")
                     )
                  )
               ),
               mainPanel(
                  fluidRow(
                     leafletOutput("map", height = "600px"),
                     tags$style(HTML("
                              .leaflet-marker-icon {
                               background-color: transparent !important;
                             }
                             .myCluster {
                               background-color: rgba(0, 123, 255, 0.5);
                               border: 2px solid rgba(0, 123, 255, 0.8);
                               border-radius: 50%;
                               color: black;
                             }
                             .myCluster div {
                               position: absolute;
                               top: 50%;
                               left: 50%;
                               transform: translate(-50%, -50%);
                               text-align: center;
                               font-weight: bold;
                               color: black;
                             }
                           "))
                  )
               )
            )
         )
      )
   )
}
