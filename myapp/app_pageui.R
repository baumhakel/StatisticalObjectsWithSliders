# STATISTIK FUER INFORMATIKSTUDIEN
# Tool zur Visualisierung von statistischen Konzepten
# Autor: Julius Baumhakel
# Zum Starten der App: 
#          runApp("Pfad/zum/Ordner/der/App")
#   oder: "Run App" Button in RStudio (oben rechts im Script-Editor) 


# This file generates a lookup table for page UI functions

# Helper function to create a consistent guide accordion for each page
guide_accordion <- function(text_key) {
  accordion(
    open = FALSE,
    accordion_panel(
      "Show Guide",
      icon = icon("info-circle"),
      markdown(app_guides[[text_key]])
    )
  )
}

# Page UI lookup table: Each entry is a function that returns the UI for that page.
page_ui <- list(
  home = function() {
    fluidPage(
      h2("Overview", class = "text-center my-4"),
      
      # Section 1
      h4("1. Descriptive Statistics & Distributions", class = "mt-4 mb-3 border-bottom"),
      layout_column_wrap(
        width = 1/5,
        card(card_header("Quantiles"), p("Explore empirical quantiles..."), actionButton("go_quant", "Quantiles [D1]", class="btn-primary w-100")),
        card(card_header("Location & Spread"), p("Compare Mean, Median, and Trimmed Mean..."),
             layout_column_wrap(width = 1/2, actionButton("go_location", "Location [D2]", class="btn-outline-primary"),
                                actionButton("go_spread", "Spread [D3]", class="btn-outline-primary"))),
        card(card_header("Histograms"), p("Explore binning effects"), actionButton("go_hist", "Histogram & Binning [D6]", class="btn-primary w-100")),
        card(card_header("Boxplots"), p("Explore IQR, whiskers, outliers"), actionButton("go_boxplot", "Boxplot [D7]", class="btn-primary w-100")),
        card(card_header("Distributional Shapes"), p("Manipulate Skew and Kurtosis"), 
             layout_column_wrap(width = 1/2, actionButton("go_skew", "Skew [D8]", class="btn-outline-primary"),
                                actionButton("go_kurt", "Kurtosis [D8]", class="btn-outline-primary")))
      ),
      
      # Section 2
      h4("2. Limits & Asymptotics", class = "mt-5 mb-3 border-bottom"),
      layout_column_wrap(
        width = 1/2,
        card(card_header("Law of Large Numbers"), p("Convergence of sample mean"), actionButton("go_lln", "Mean Convergence [D4]", class="btn-info w-100")),
        card(card_header("Glivenko-Cantelli Theorem"), p("ECDF convergence"), actionButton("go_ecdf", "ECDF Convergence [D5]", class="btn-info w-100"))
      ),
      
      # Section 3
      h4("3. Point Estimators", class = "mt-5 mb-3 border-bottom"),
      layout_column_wrap(
        width = 1/3,
        card(card_header("MLEs for Normal Distribution"), p("Explore the concept of likelihood"), 
             layout_column_wrap(width = 1/2, actionButton("go_norm1", "MLE for μ [D9]", class="btn-outline-success"),
                                actionButton("go_norm2", "MLE for μ and σ [D11]", class="btn-outline-success"))),
        card(card_header("MLEs for Bernoulli Distribution"), p("Explore the concept of likelihood"),
             actionButton("go_bern", "MLE for p [D10]", class="btn-success w-100")),
        card(card_header("Confidence Intervals"), p("Explore how coverage works and how parameters influence the size of confidence intervals."), 
             actionButton("go_ci", "Confidence Intervals [D12]", class="btn-success w-100"))
      )
    )
  },
  histogram = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        hr(),
        renderText("Standard Normal Distribution"),
        sliderInput("n_samp", "Sample Size (n):", min = 1, max = 1000, value = 50),
        sliderInput("bins", "Number of Bins:", min = 1, max = 50, value = 20),
        sliderInput("range", "X-Axis Limits:", min = -10, max = 10, value = c(-4, 4)),
        checkboxInput("scaling", "Scale to density", value = FALSE)
      ),
      guide_accordion("histogram"),
      card(
        card_header(textOutput("hist_title")),
        plotOutput("histPlot", height = "600px") # Height adjusted for 16:9 feel
      )
    )
  },
  lln = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        hr(),
        renderText("Standard Normal Dist."),
        sliderInput("n_obs", "Observations to show (N):", min = 1, max = 500, value = 100),
        sliderInput("n_paths", "Number of Realizations (m):", min = 1, max = 20, value = 5),
        actionButton("recompute", "Generate New Data", class = "btn-warning w-100")
      ),
      guide_accordion("lln"),
      card(
        card_header(textOutput("lln_title")),
        plotOutput("llnPlot", height = "600px")
      )
    )
  },
  ecdf_conv = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        hr(),
        renderText("Standard Normal Dist."),
        sliderInput("n_show", "Sample Size (n):", min = 1, max = 1000, value = 100, step = 10),
        actionButton("recompute_ecdf", "New Random Sample", class = "btn-warning w-100")
      ),
      guide_accordion("ecdf_conv"),
      # Side-by-side plots
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("ECDF vs. Theoretical CDF"),
          plotOutput("ecdfPlot", height = "550px")
        ),
        card(
          card_header("Histogram vs. Theoretical Density"),
          plotOutput("densPlot", height = "550px")
        )
      )
    )
  },
  mle_norm1 = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        hr(),
        sliderInput("mu_guess_n1", "Proposed Mean (μ):", min = -3, max = 3, value = 0, step = 0.01),
        actionButton("jump_to_mle_n1", "Jump to MLE", class = "btn-info w-100 mt-2"),
        actionButton("resample_mle_n1", "New Random Data", class = "btn-warning w-100")
      ),
      guide_accordion("mle_norm1"),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Visual Likelihood: Data & Contributions"),
          plotOutput("mleDataPlot_n1", height = "350px")
        ),
        layout_column_wrap(
          width = 1/2,
          card(card_header("Likelihood"), plotOutput("likPlotMu_n1", height = "250px")),
          card(card_header("Log-Likelihood"), plotOutput("logLikPlotMu_n1", height = "250px"))
        )
      )
    )
  },
  mle_norm2 = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        hr(),
        sliderInput("mu_guess", "Proposed Mean (μ):", min = -3, max = 3, value = 0, step = 0.01),
        sliderInput("sd_guess", "Proposed SD (σ):", min = 0.1, max = 4, value = 1, step = 0.01),
        actionButton("jump_to_mle", "Jump to MLE", class = "btn-info w-100 mt-2"),
        actionButton("resample_mle", "New Random Data", class = "btn-warning w-100")
      ),
      guide_accordion("mle_norm2"),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Visual Likelihood: Data & Contributions"),
          plotOutput("mleDataPlot", height = "350px")
        ),
        # Grid of 4 plots: 2 for Mu, 2 for Sigma
        layout_column_wrap(
          width = 1/2,
          card(card_header("Likelihood wrt μ"), plotOutput("likPlotMu", height = "250px")),
          card(card_header("Log-Likelihood wrt μ"), plotOutput("logLikPlotMu", height = "250px")),
          card(card_header("Likelihood wrt σ"), plotOutput("likPlotSigma", height = "250px")),
          card(card_header("Log-Likelihood wrt σ"), plotOutput("logLikPlotSigma", height = "250px"))
        )
      )
    )
  },
  mle_bern = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        hr(),
        sliderInput("p_guess", "Proposed p:", min = 0.01, max = 0.99, value = 0.6, step = 0.01),
        actionButton("jump_to_mle_bern", "Jump to MLE", class = "btn-info w-100 mt-2"),
        actionButton("resample_bern", "New Random Data (p=0.3)", class = "btn-warning w-100")
      ),
      guide_accordion("mle_bern"),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Visual Likelihood: Outcomes & PMF"),
          plotOutput("mleDataPlot_bern", height = "350px")
        ),
        layout_column_wrap(
          width = 1/2,
          card(card_header("Likelihood"), plotOutput("likPlotP_bern", height = "250px")),
          card(card_header("Log-Likelihood"), plotOutput("logLikPlotP_bern", height = "250px"))
        )
      )
    )
  },
  skew_ui = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back", class="btn-secondary mb-3"),
        sliderInput("s_skew", "Skewness (γ):", 0.1, 2, 0.5, step = 0.1),
        hr(),
        sliderInput("s_n", "Sample Size:", 20, 1000, 200),
        actionButton("resample_s", "New Sample", class="btn-warning w-100")
      ),
      guide_accordion("skew"),
      layout_column_wrap(
        width = 1/3,
        card(card_header("Density (Shifted Gamma)"), plotOutput("s_dens")),
        card(card_header("ECDF"), plotOutput("s_ecdf")),
        card(card_header("Summary Stats"), tableOutput("s_sum")),
        card(card_header("Histogram"), plotOutput("s_hist")),
        card(card_header("Boxplot"), plotOutput("s_box")),
        card(card_header("Normal QQ-Plot"), plotOutput("s_qq"))
      )
    )
  },
  kurt_ui = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back", class="btn-secondary mb-3"),
        sliderInput("k_kurt", "Kurtosis (κ):", 3.1, 10, 3.5, step = 0.1),
        hr(),
        sliderInput("k_n", "Sample Size:", 20, 1000, 200),
        actionButton("resample_k", "New Sample", class="btn-warning w-100")
      ),
      guide_accordion("kurt"),
      layout_column_wrap(
        width = 1/3,
        card(card_header("Density (Scaled t)"), plotOutput("k_dens")),
        card(card_header("ECDF"), plotOutput("k_ecdf")),
        card(card_header("Summary Stats"), tableOutput("k_sum")),
        card(card_header("Histogram"), plotOutput("k_hist")),
        card(card_header("Boxplot"), plotOutput("k_box")),
        card(card_header("Normal QQ-Plot"), plotOutput("k_qq"))
      )
    )
  },
  location = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        hr(),
        sliderInput("loc_outlier", "Manipulate Outlier:", min = -15, max = 15, value = 5, step = 0.5),
        sliderInput("loc_alpha", "Alpha (Trim proportion):", min = 0, max = 0.4, value = 0.2, step = 0.05),
        actionButton("resample_loc", "New Sample (N=10)", class="btn-warning w-100")
        
      ),
      guide_accordion("location"),
      card(
        card_header("Location Measures & Outlier Sensitivity"),
        plotOutput("plot_location", height = "600px")
      )
    )
  },
  spread = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back", class="btn-secondary mb-3"),
        sliderInput("spread_step", "Step-by-Step Calculation:", min = 1, max = 4, value = 1, step = 1),
        sliderInput("outlier_val", "Manipulate Outlier (Point 1):", min = -10, max = 10, value = 1, step = 0.5),
        actionButton("resample_spread", "New Sample (N=8)", class="btn-warning w-100"),
      ),
      guide_accordion("spread"),
      layout_column_wrap(
        width = 1/3,
        card(card_header("Empirical Standard Deviation (S)"), plotOutput("plot_s", height = "600px")),
        card(card_header("Median Abs. Deviation (MAD) Standard Deviation"), plotOutput("plot_mad", height = "600px")),
        card(card_header("Interquartile Range (IQR) Standard Deviation"), plotOutput("plot_iqr", height = "600px"))
      )
    )
  },
  quantiles = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        actionButton("resample_q", "Create New Dataset", class="btn-warning w-100"),
        hr(),
        sliderInput("q_n_show", "Sample Size to use:", min = 5, max = 500, value = 50),
        sliderInput("q_prob", "Quantile (p):", min = 0, max = 1, value = 0.5, step = 0.01)
      ),
      guide_accordion("quantiles"),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Observation View (Sorted & Jittered)"),
          plotOutput("q_jitter", height = "300px")
        ),
        layout_column_wrap(
          width = 1/2,
          card(card_header("CDF View"), plotOutput("q_cdf", height = "350px")),
          card(card_header("Density View"), plotOutput("q_dens", height = "350px"))
        ),
        layout_column_wrap(
          width = 1/2,
          card(card_header("ECDF View"), plotOutput("q_ecdf", height = "350px")),
          card(card_header("Histogram View"), plotOutput("q_hist", height = "350px"))
        )
      )
    )
  },
  ci = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        actionButton("resample_ci", "Generate New Intervals", class="btn-warning w-100"),
        hr(),
        sliderInput("ci_sd", "True SD (σ):", min = 0.5, max = 5, value = 1, step = 0.1),
        sliderInput("ci_n_obs", "Sample Size (n per CI):", min = 5, max = 100, value = 30),
        sliderInput("ci_conf_level", "Confidence Level (1-α):", min = 0.5, max = 0.99, value = 0.95, step = 0.01),
        sliderInput("ci_show_num", "Number of Intervals to Display:", min = 1, max = 100, value = 50)
      ),
      guide_accordion("ci"),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Confidence Interval Comparison (Long-run Process)"),
          plotOutput("ci_plot", height = "450px")
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Detail: Construction of Interval #1"),
            plotOutput("ci_detail", height = "300px")
          ),
          card(
            card_header("Theoretical Critical Values (N-Dist)"),
            plotOutput("ci_theory", height = "300px")
          )
        )
      )
    )
  },
  boxplot = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        actionButton("resample_box", "Generate New Sample", class="btn-warning w-100"),
        hr(),
        sliderInput("box_obs_val", "Value of Point #20:", min = -7, max = 7, value = 0, step = 0.1)
      ),
      guide_accordion("boxplot"),
      card(
        card_header("Boxplot vs. Individual Observations"),
        plotOutput("box_main_plot", height = "750px") # Increased height for 16:9 feel
      )
    )
  }
)
