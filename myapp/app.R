# STATISTIK FUER INFORMATIKSTUDIEN
# Tool zur Visualisierung von statistischen Konzepten
# Autor: Julius Baumhakel
# Zum Starten der App: 
#          runApp("Pfad/zum/Ordner/der/App")
#   oder: "Run App" Button in RStudio (oben rechts im Script-Editor) 


# to run locally, make sure all packages are installed
suppressPackageStartupMessages({
library("shiny")
library("ggplot2")
library("bslib")
library("munsell")
library("markdown")
})

# to run locally, make sure these are in the same directory as app.R
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
    go_location = "location", go_kurt = "kurt_ui", 
    go_skew = "skew_ui", go_spread = "spread", go_quant = "quantiles", 
    go_ci = "ci", go_boxplot = "boxplot", go_back = "home",
    go_norm1 = "mle_norm1", go_norm2 = "mle_norm2", go_bern = "mle_bern"
  )
  
  call_functions <- list(
    histogram = hist_logic, lln = lln_logic, ecdf_conv = ecdf_logic, location = location_logic,
    kurt_ui = kurt_logic, skew_ui = skew_logic, spread = spread_logic, quantiles = quantile_logic, ci = ci_logic, boxplot = boxplot_logic,
    mle_norm1 = mle_norm1_logic, mle_norm2 = mle_norm2_logic, mle_bern = mle_bern_logic
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
  
  
  # # --- Call all logic modules ---
  lapply(list(
    lln_logic, hist_logic, ecdf_logic, kurt_logic, skew_logic,
    spread_logic, location_logic, quantile_logic, ci_logic, boxplot_logic,
    mle_norm1_logic, mle_norm2_logic, mle_bern_logic
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
