# File: python_analysis_tab.R - Static layout for Python-based DataFed record querying UI
# TODO: pickerInput choices depend on harmonized_df existing in global scope; add guard / reactive source.

python_analysis_tab <- function() { # FUNCTION: build Python Analysis tab UI
   div(
      fluidRow(
         tabPanel(
            "Python Analysis",
            div(
               style = "margin-bottom:8px;",
               strong(textOutput("datafed_status")),
               uiOutput("datafed_query_activity")
            ),
            pickerInput(
               "seq_names", "Select Sequences",
               choices = character(0),
               options = list(`actions-box` = TRUE, title = "(Load data first)"), multiple = TRUE
            ),
            # results
            textOutput("python_results")
         )
      )
   )
}
