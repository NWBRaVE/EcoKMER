# File: data_analysis_rm.R - Ingest and process NPO & diversity JSON configs to produce plots and downloadable CSVs

# NPO: reactive JSON load
json_data_npo <- reactive({ # REACTIVE returns parsed NPO JSON
   req(input$upload_npo_json_file_data_analysis)
   # load json file
   json <- fromJSON(input$upload_npo_json_file_data_analysis$datapath)
   return(json)
})

output$json_npo_input_file_data_analysis <- renderPrint({ # OUTPUT echo NPO JSON
   req(json_data_npo())
   json_data_npo()
})

process_npo <- eventReactive(input$run_npo_processing, { # EVENT process NPO JSON -> tibble
   req(json_data_npo())
   process_npo <- npo_processing(json_data_npo())
   return(process_npo)
})

output$downloadNPO_data <- downloadHandler( # DOWNLOAD processed NPO CSV
   filename = function() {
      paste("processed_npo_", Sys.Date(), ".csv", sep = "")
   },
   content = function(file) {
      # Select only the Experiment_Index column before saving.
      write.csv(process_npo(), file, row.names = FALSE)
   }
)


# Diversity: reactive JSON load
json_data_diversity <- reactive({ # REACTIVE returns parsed diversity JSON
   req(input$upload_json_file_data_analysis)
   # load json file
   json <- fromJSON(input$upload_json_file_data_analysis$datapath)
   return(json)
})

output$json_input_file_data_analysis <- renderPrint({ # OUTPUT echo diversity JSON
   req(json_data_diversity())
   # print json file
   json_data_diversity()
})

process_diversity <- eventReactive(input$run_diversity, { # EVENT process diversity JSON
   req(json_data_diversity())
   # process the data
   process_diversity <- process_data(json_data_diversity())
   return(process_diversity)
})

output$boxplot_diversity <- renderPlotly({ # OUTPUT diversity boxplot (uses boxplot_data)
   req(process_diversity())

   boxplot_diversity <- boxplot_data(process_diversity())
   boxplot_diversity$boxplot_plot[[1]] + theme(text = element_text(size = 20))
})

output$lineplot_diversity <- renderPlotly({ # OUTPUT diversity line plot (uses lineplot_data)
   req(process_diversity())

   lineplot_diversity <- lineplot_data(process_diversity())
   lineplot_diversity$lineplot_plot[[1]] + theme(text = element_text(size = 20))
}) # TODO: Consider JSON schema validation & error messaging for malformed configs
