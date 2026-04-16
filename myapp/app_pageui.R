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

# Helper function for uniform button styling
concept_btn <- function(id, label, code, color_class = "secondary") {
  actionButton(
    id,
    label = HTML(paste0("<b>", label, "</b> <br><small>[", code, "]</small>")),
    class = paste0("btn-", color_class, " m-1 py-3"),
    style = "width: 220px; text-align: center; border-radius: 8px;"
  )
}

# Page UI lookup table: Each entry is a function that returns the UI for that page.
page_ui <- list(
  home = function() {
    fluidPage(
      # Custom CSS for the monochromatic section accents
      tags$style(HTML("
      .section-header { 
        border-left: 6px solid #495057; 
        padding-left: 15px; 
        margin-top: 40px; 
        margin-bottom: 20px; 
        font-weight: 700;
        color: #343a40;
      }
      /* Specific colors for section borders to match the buttons */
      .border-desc { border-color: ##007bff !important; } /* Blue */
      .border-limit { border-color: #0d6efd !important; } /* Cyan/Teal */
      .border-estim { border-color: #198754 !important; } /* Green */
      .border-hypo { border-color: #ffc107 !important; }  /* Yellow/Amber */
      .border-reg { border-color: #dc3545 !important; }   /* Red */
    ")),
      
      h1("Statistical Objects with Sliders", class = "text-center my-5 fw-bold"),
      
      # Section 1
      h4("1. Descriptive Statistics & Distributions", class = "section-header border-desc"),
      layout_column_wrap(
        width = "220px", # Fixed width for each button
        fixed_width = TRUE,
        concept_btn("go_quant", "Quantiles", "D1", "primary"),
        concept_btn("go_location", "Measures of Location", "D2", "primary"),
        concept_btn("go_spread", "Measures of Spread", "D3", "primary"),
        concept_btn("go_hist", "Histogram", "D6", "primary"),
        concept_btn("go_boxplot", "Boxplot", "D7", "primary"),
        concept_btn("go_skew", "Detecting Skewness", "D8", "primary"),
        concept_btn("go_kurt", "Detecting Kurtosis", "D8", "primary")
      ),
      
      # Section 2
      h4("2. Limits & Asymptotics", class = "section-header border-limit"),
      layout_column_wrap(
        width = "220px", fixed_width = TRUE,
        concept_btn("go_lln", "Mean Convergence", "D4", "info"),
        concept_btn("go_ecdf", "ECDF Convergence", "D5", "info")
      ),
      
      # Section 3
      h4("3. Point Estimators", class = "section-header border-estim"),
      layout_column_wrap(
        width = "220px", fixed_width = TRUE,
        concept_btn("go_norm1", "MLE for μ", "D9", "success"),
        concept_btn("go_norm2", "MLE for μ & σ", "D11", "success"),
        concept_btn("go_bern", "MLE for p", "D10", "success"),
        concept_btn("go_ci", "Confidence Intervals", "D12", "success")
      ),
      
      # Section 4
      h4("4. Hypothesis Testing", class = "section-header border-hypo"),
      layout_column_wrap(
        width = "220px", fixed_width = TRUE,
        concept_btn("go_ztest", "Z-test", "D13", "warning"),
        concept_btn("go_ttest", "t-test", "D15", "warning"),
        concept_btn("go_pval", "p-values", "D16", "warning"),
        concept_btn("go_test_ci", "CI vs. Z-test", "D14", "warning"),
        concept_btn("go_twosample", "Two-sample tests", "D17", "warning")
      ),
      
      # Section 5
      h4("5. Simple Linear Regression", class = "section-header border-reg"),
      layout_column_wrap(
        width = "220px", fixed_width = TRUE,
        concept_btn("go_slr_est", "SLR Estimation", "D18", "danger"),
        concept_btn("go_slr_dist", "Coefficient Estimator Distribution", "D19", "danger"),
        concept_btn("go_slr_bands", "Confidence and prediction bands", "D20", "danger"),
        concept_btn("go_slr_coverage", "Coverage of conf./pred. intervals", "D21", "danger"),
        concept_btn("go_slr_r2", "R²", "D22", "danger"),
        concept_btn("go_slr_violation", "(Violation of) Assumptions", "D23", "danger")
      ),
      
      div(style = "margin-bottom: 100px;")
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
  },
  
  ztest = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        actionButton("resample_z", "Generate New Samples", class="btn-warning w-100"),
        hr(),
        sliderInput("z_true_mu", "True Mean (μ):", min = -2, max = 2, value = 0, step = 0.1),
        sliderInput("z_n_obs", "Sample Size (n):", min = 5, max = 100, value = 30),
        selectInput("z_alt", "Alternative Hypothesis:",
                    choices = list("Two-Sided (≠)" = "two.sided", 
                                   "Greater (>)" = "greater", 
                                   "Less (<)" = "less")),
        sliderInput("z_alpha", "Significance Level (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
        sliderInput("z_show_num", "Number of Repetitions:", min = 1, max = 100, value = 50)
      ),
      guide_accordion("ztest"),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Hypothesis Testing Outcomes (Rejection Regions)"),
          plotOutput("z_plot", height = "450px")
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Latest Sample Distribution"),
            plotOutput("z_detail", height = "300px")
          ),
          card(
            card_header("Theoretical Critical Values (N-Dist)"),
            plotOutput("z_theory", height = "300px")
          )
        )
      )
    )
  },
  
  ttest = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        actionButton("resample_t", "Generate New Samples", class="btn-warning w-100"),
        hr(),
        sliderInput("t_true_mu", "True Mean (μ):", min = -2, max = 2, value = 0, step = 0.1),
        sliderInput("t_n_obs", "Sample Size (n):", min = 2, max = 100, value = 30), # n=2 is min for t-test
        selectInput("t_alt", "Alternative Hypothesis:",
                    choices = list("Two-Sided (≠)" = "two.sided", 
                                   "Greater (>)" = "greater", 
                                   "Less (<)" = "less")),
        sliderInput("t_alpha", "Significance Level (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
        sliderInput("t_show_num", "Number of Repetitions:", min = 1, max = 100, value = 50)
      ),
      guide_accordion("ttest"),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Hypothesis Testing Outcomes (t-Distribution Rejection)"),
          plotOutput("t_plot", height = "450px")
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Latest Sample Distribution"),
            plotOutput("t_detail", height = "300px")
          ),
          card(
            card_header("Theoretical Critical Values (t-Dist)"),
            plotOutput("t_theory", height = "300px")
          )
        )
      )
    )
  },
  
  pval = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        actionButton("resample_p", "Generate New Sample", class="btn-warning w-100"),
        hr(),
        sliderInput("p_true_mu", "True Mean (μ):", min = -0.5, max = 0.5, value = 0, step = 0.1),
        sliderInput("p_n_obs", "Sample Size (n):", min = 5, max = 25, value = 15),
        selectInput("p_alt", "Alternative Hypothesis:",
                    choices = list("Greater (>)" = "greater", 
                                   "Less (<)" = "less",
                                   "Two-Sided (≠)" = "two.sided")),
        hr(),
        # High-resolution slider for Alpha/p-value comparison
        sliderInput("p_alpha", "Significance Level (α):", 
                    min = 0, max = 1, value = 0.05, step = 0.00001),
        actionButton("jump_p", "Jump to p-value", class="btn-info w-100")
      ),
      guide_accordion("pval"),
      layout_column_wrap(
        width = 1,
        card(
          card_header("The p-value Visualization"),
          # Large plot showing the observed mean relative to the Null Distribution
          plotOutput("p_main_plot", height = "500px")
        )
      )
    )
  },
  
  testci = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        actionButton("resample_dual", "Generate New Sample", class="btn-warning w-100"),
        hr(),
        sliderInput("dual_true_mu", "True Mean (μ):", min = -2, max = 2, value = 0.3, step = 0.1),
        sliderInput("dual_n_obs", "Sample Size (n):", min = 5, max = 100, value = 30),
        selectInput("dual_alt", "Alternative Hypothesis (Test Type):",
                    choices = list("Two-Sided (≠)" = "two.sided", 
                                   "Greater (>)" = "greater", 
                                   "Less (<)" = "less")),
        hr(),
        # Linking Alpha to Confidence Level visually: (1 - Alpha)
        sliderInput("dual_alpha", "Significance Level (α):", 
                    min = 0.01, max = 0.20, value = 0.05, step = 0.01)
      ),
      guide_accordion("testci"),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Duality: Confidence Intervals & Hypothesis Tests"),
          # Single high-impact plot showing the CI and the Null Distribution Rejection Regions
          plotOutput("dual_plot", height = "550px")
        )
      )
    )
  },
  
  twosample = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        actionButton("resample_2z", "Generate New Samples", class="btn-warning w-100"),
        hr(),
        # Group 1 Controls
        span("Group 1", style="font-weight: bold; color: #007bff;"),
        sliderInput("z2_mu1", "True Mean (μ1):", min = -2, max = 2, value = 0.5, step = 0.1),
        sliderInput("z2_n1", "Sample Size (n1):", min = 5, max = 100, value = 30),
        hr(),
        # Group 2 Controls
        span("Group 2", style="font-weight: bold; color: #28a745;"),
        sliderInput("z2_mu2", "True Mean (μ2):", min = -2, max = 2, value = 0, step = 0.1),
        sliderInput("z2_n2", "Sample Size (n2):", min = 5, max = 100, value = 30),
        hr(),
        selectInput("z2_alt", "Alternative Hypothesis:",
                    choices = list("Difference (μ1 ≠ μ2)" = "two.sided", 
                                   "Group 1 > Group 2" = "greater", 
                                   "Group 1 < Group 2" = "less")),
        sliderInput("z2_alpha", "Significance Level (α):", 
                    min = 0.01, max = 0.20, value = 0.05, step = 0.01)
      ),
      guide_accordion("twosample"),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Two-Sample Comparison: Mean Differences"),
          # This plot will show the raw points for both groups and the difference scale
          plotOutput("z2_plot", height = "600px")
        )
      )
    )
},

  slr_est = function() {
  layout_sidebar(
    sidebar = sidebar(
      actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
      hr(),
      sliderInput("beta0_guess", "Proposed Intercept (β₀):", 
                  min = -5, max = 5, value = 1, step = 0.05),
      sliderInput("beta1_guess", "Proposed Slope (β₁):", 
                  min = -5, max = 5, value = -1, step = 0.05),
      actionButton("jump_to_min_sse", "Jump to Optimizer", class = "btn-info w-100 mt-2"),
      actionButton("resample_slr", "New Random Data", class = "btn-warning w-100")
    ),
    guide_accordion("slr_est"),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Data Space: Residuals & Regression Line"),
        plotOutput("slr_data_plot", height = "350px")
      ),
      layout_column_wrap(
        width = 1/3,
        card(card_header("SSE Surface"), plotOutput("sse_surface", height = "300px")),
        card(card_header("SSE Projection: β₀"), plotOutput("sse_beta0", height = "300px")),
        card(card_header("SSE Projection: β₁"), plotOutput("sse_beta1", height = "300px"))
      )
    )
  )
  },

  slr_dist = function() {
  layout_sidebar(
    sidebar = sidebar(
      actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
      hr(),
      sliderInput("n_show", "Number of Samples to Show (N):", 
                  min = 2, max = 500, value = 50, step = 1),
      actionButton("resample_dist", "Generate New Simulations", class = "btn-warning w-100")
    ),
    guide_accordion("slr_dist"),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Sampling Variability: The 'Shadow' Lines"),
        plotOutput("slr_shadow_plot", height = "400px")
      ),
      layout_column_wrap(
        width = 1/2,
        card(card_header("Distribution of Intercept Estimator (β̂₀)"), 
             plotOutput("dist_beta0", height = "300px")),
        card(card_header("Distribution of Slope Estimator (β̂₁)"), 
             plotOutput("dist_beta1", height = "300px"))
      )
    )
  )
},

  slr_r2 = function() {
  layout_sidebar(
    sidebar = sidebar(
      actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
      hr(),
      sliderInput("true_slope_r2", "True Slope (β₁):", 
                  min = -4, max = 4, value = 2, step = 0.1),
      sliderInput("true_sd_r2", "Error SD (σ):", 
                  min = 0.1, max = 5, value = 2, step = 0.1),
      actionButton("resample_r2", "Generate New Noise", class = "btn-warning w-100")
    ),
    guide_accordion("slr_r2"),
    layout_column_wrap(
      width = 1,
      layout_column_wrap(
        width = 1/2, # Splits the top row into two columns
        card(
          card_header("R² Visualization: Variation Explained"),
          plotOutput("r2_main_plot", height = "350px")
        ),
        card(
          card_header("The R² Energy Bar (SSR / SST)"),
          plotOutput("r2_energy_bar", height = "350px")
        )
      ),
      card(
        card_header("Sum of Squares Decomposition: SST = SSR + SSE"),
        plotOutput("r2_decomp_plot", height = "300px")
      )
    )
  )
},

  slr_violation = function() {
  layout_sidebar(
    sidebar = sidebar(
      actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
      hr(),
      radioButtons("ui_mode", "Interface Mode:", 
                   choices = c("Simple (Scenarios)" = "simple", "Complex (Sandbox)" = "complex")),
      conditionalPanel(
        condition = "input.ui_mode == 'simple'",
        selectInput("scenario", "Select Violation:",
                    choices = c("Everything is Fine" = "none",
                                "Non-linear Mean" = "nonlinear",
                                "Heteroscedasticity" = "hetero",
                                "Heavy Tails" = "tails",
                                "Influential Outliers" = "outliers"))
      ),
      conditionalPanel(
        condition = "input.ui_mode == 'complex'",
        sliderInput("nl_strength", "Non-linearity (Quadratic):", min = 0, max = 2, value = 0, step = 0.1),
        sliderInput("het_strength", "Heteroscedasticity:", min = 0, max = 3, value = 0, step = 0.1),
        sliderInput("tail_df", "Tail Heaviness (t-dist df):", min = 1, max = 30, value = 30, step = 1),
        sliderInput("outlier_count", "Number of Outliers:", min = 0, max = 5, value = 0, step = 1),
        sliderInput("outlier_dist", "Outlier Severity:", min = 0, max = 20, value = 10, step = 1)
      ),
      actionButton("resample_assumptions", "New Random Seed", class = "btn-warning w-100 mt-3")
    ),
    guide_accordion("slr_violation"),
    layout_column_wrap(
      width = 1,
      card(card_header("Data Space & Fitted Line"), plotOutput("assumption_main_plot", height = "350px")),
      card(
        card_header("Standard Diagnostic Plots (Base R)"),
        layout_column_wrap(width = 1/2,
                           plotOutput("diag_1", height = "250px"), plotOutput("diag_2", height = "250px"),
                           plotOutput("diag_3", height = "250px"), plotOutput("diag_4", height = "250px")
        )
      )
    )
  )
},

  slr_bands = function() {
    layout_sidebar(
      sidebar = sidebar(
        actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
        hr(),
        sliderInput("slr_n", "Sample Size (n):", min = 5, max = 100, value = 30, step = 1),
        sliderInput("slr_alpha", "Confidence Level (1-α):", min = 0.50, max = 0.99, value = 0.90, step = 0.01),
        sliderInput("slr_x_spread", "X-Value Spread:", min = 0.5, max = 5, value = 1, step = 0.1),
        sliderInput("slr_y_spread", "Error Noise (σ):", min = 0.1, max = 5, value = 1, step = 0.1),
        actionButton("resample_slr", "New Random Data", class = "btn-warning w-100 mt-2")
      ),
      guide_accordion("slr_bands"),
      card(
        card_header("Simple Linear Regression: Confidence vs. Prediction Bands"),
        plotOutput("slrPlot", height = "600px")
      )
    )
},

  slr_coverage = function() {
  layout_sidebar(
    sidebar = sidebar(
      actionButton("go_back", "← Back to Home", class="btn-secondary mb-3"),
      hr(),
      sliderInput("cov_n", "Sample Size (n):", min = 5, max = 100, value = 30),
      sliderInput("cov_alpha", "Confidence Level (1-alpha):", min = 0.5, max = 0.99, value = 0.90, step = 0.01),
      sliderInput("cov_N_sims", "Number of Repetitions:", min = 10, max = 100, value = 25),
      sliderInput("cov_xval", "X-Value for Interval Estimation:", min = -3, max = 3, value = 1, step = 0.1),
      hr(),
      actionButton("resample_cov", "Generate New Samples", class = "btn-warning w-100"),
      helpText("Generates new true coefficients and a new set of random error realizations.")
    ),
    guide_accordion("slr_coverage"),
    layout_column_wrap(
      width = 1,
      # Row 1: Confidence Interval (The Mean)
      layout_column_wrap(
        width = 1/2,
        card(card_header("CI: Current Simulation (Live)"), plotOutput("ci_live_plot", height = "350px")),
        card(card_header("CI: Coverage History"), plotOutput("ci_hist_plot", height = "350px"))
      ),
      # Row 2: Prediction Interval (The Individual)
      layout_column_wrap(
        width = 1/2,
        card(card_header("PI: Current Simulation (Live)"), plotOutput("pi_live_plot", height = "350px")),
        card(card_header("PI: Coverage History"), plotOutput("pi_hist_plot", height = "350px"))
      )
    )
  )
}
)
