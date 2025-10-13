library(shiny) ## Purpose: server entrypoint. Gotcha: sources dynamic render UI scripts each refresh; consider modularization.

server <- function(input, output) {
   file_loads <- c(
      list.files("./Frontend_render_UI", recursive = T, full.names = T)
   )

   for (f in grep(".R$", file_loads, value = T)) source(f, local = TRUE)

   output$developer_buttons <- renderUI({ # Quick debug hook (hide in prod)
      div(
         style = "position:fixed;z-index:9999;bottom:10px;right:10px;",
         actionButton("Browser", "Debug", style = "background:deepskyblue")
      )
   })

   harmonized_data <- reactiveVal() # placeholder: set in future data load module
   datafed_data <- reactiveVal() # placeholder for online DataFed content

   # Touch once so linters don't flag unused (will be used in future modules)
   observe({
      invisible(harmonized_data())
      invisible(datafed_data())
   })
}
