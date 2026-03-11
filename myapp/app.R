# STATISTIK FUER INFORMATIKSTUDIEN
# Tool zur Visualisierung von statistischen Konzepten
# Autor: Julius Baumhakel
# Zum Starten der App: 
#          runApp("Pfad/zum/Ordner/der/App")
#   oder: "Run App" Button in RStudio (oben rechts im Script-Editor) 


#liblist <- c("shiny", "ggplot2", "bslib", "PearsonDS", "munsell", "markdown")
#lapply(liblist, function(pkg) {
#  if (!require(pkg, character.only = TRUE)) {
#    install.packages(pkg)
#    library(pkg, character.only = TRUE)
#  }
#})

suppressPackageStartupMessages({
library("shiny")
library("ggplot2")
library("bslib")
#library("PearsonDS")
library("munsell")
library("markdown")
})

source("app_guides.R", local = TRUE)
source("app_pageui.R", local = TRUE)
source("app_logic.R", local = TRUE)

# --- UI for the main content, dynamically rendered based on current page ---
server <- function(input, output, session) {
  
  # --- Track current page ---
  current_page <- reactiveVal("home")
  
  # --- Navigation mapping ---
  nav_buttons <- list(
    go_hist = "histogram", go_lln = "lln", go_ecdf = "ecdf_conv", 
    go_mle = "mle", go_location = "location", #go_pearson = "pearson", 
    go_spread = "spread", go_quant = "quantiles", go_ci = "ci",
    go_boxplot = "boxplot", go_back = "home"
  )
  
  lapply(names(nav_buttons), function(btn) {
    observeEvent(input[[btn]], { current_page(nav_buttons[[btn]]) })
  })
  
  # --- UI Renderer ---
  output$main_content <- renderUI({
    page <- current_page()
    
    # Fallback falls Seite nicht definiert
    if (!page %in% names(page_ui)) return(NULL)
    
    # Render the UI for the current page
    page_ui[[page]]()
  })
  
  # --- Call all logic modules ---
  lapply(list(
    lln_logic, hist_logic, ecdf_logic, mle_logic, # pearson_logic,
    spread_logic, location_logic, quantile_logic, ci_logic, boxplot_logic
  ), function(f) f(input, output, session))
  
}

# --- Base UI with placeholder for dynamic content ---
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Statistical Objects with Sliders"),
  
  # Dynamic UI Container
  uiOutput("main_content")
)

# Run the application
shinyApp(ui = ui, server = server)
