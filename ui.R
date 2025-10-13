## Purpose: top-level UI composition (static tabs). Gotcha: avoid heavy data loads here—do in server/reactive.
ui <- fluidPage(
   tags$head(
      includeCSS("www/app.css")
   ),
   titlePanel("EcoKMER"),
   navbarPage(
      title = "EcoKMER",
      id = "top_page",
      tabPanel("Home", home_tab()),
      navbarMenu(
         "Data Analysis",
         tabPanel("Harmonized Analysis", harmonized_analysis_tab())
      )
   )
)
