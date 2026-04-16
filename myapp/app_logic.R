# STATISTIK FUER INFORMATIKSTUDIEN
# Tool zur Visualisierung von statistischen Konzepten
# Autor: Julius Baumhakel
# Zum Starten der App: 
#          runApp("Pfad/zum/Ordner/der/App")
#   oder: "Run App" Button in RStudio (oben rechts im Script-Editor) 



# Server logic for each page of the app. Each function corresponds to a 
# different tab and contains the reactive expressions and render functions 
# for that tab's content.

lln_logic <- function(input, output, session) {
  
  # --- Data generation ---
  
  # store precomputed cumulative means
  master_data <- reactiveVal()
  
  # generate 500 observations for 20 realizations
  observeEvent(input$recompute, ignoreNULL = FALSE, {
    
    raw_samples <- matrix(rnorm(500 * 20), nrow = 500, ncol = 20)
    
    # cumulative mean for each realization
    cum_means <- apply(raw_samples, 2, function(x) cumsum(x) / seq_along(x))
    
    master_data(cum_means)
  })
  
  # --- Title ---
  
  output$lln_title <- renderText({
    paste("LLN Convergence for: Standard Normal Dist.")
  })
  
  # --- Plot ---
  
  output$llnPlot <- renderPlot({
    req(master_data())
    
    # subset based on slider inputs
    plot_mat <- master_data()[1:input$n_obs, 1:input$n_paths, drop = FALSE]
    
    # reshape for ggplot
    df <- data.frame(
      obs = rep(1:input$n_obs, times = input$n_paths),
      val = as.vector(plot_mat),
      path = as.factor(rep(1:input$n_paths, each = input$n_obs))
    )
    
    ggplot(df, aes(x = obs, y = val, group = path, color = path)) +
      geom_line(alpha = 0.7, linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
      labs(
        x = "Sample Size (n)", 
        y = "Sample Mean",
        subtitle = paste("Showing", input$n_paths, "realizations of the mean process")
      ) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none")
  })
}

hist_logic <- function(input, output, session) {
  
  # --- Data storage ---
  
  # store a fixed random sample
  stored_data <- reactiveVal(rnorm(1000))
  
  # --- Title ---
  
  output$hist_title <- renderText({
    "Standard Normal Distribution"
  })
  
  # --- Histogram ---
  
  output$histPlot <- renderPlot({
    
    storedsample <- stored_data()
    
    # subset sample size from slider
    storedsample <- storedsample[1:input$n_samp]
    
    df <- data.frame(x = storedsample)
    
    # compute bin edges
    binv <- seq(input$range[1], input$range[2], length.out = input$bins + 1)
    
    hist(
      df$x,
      breaks = binv,
      freq = !input$scaling,
      xlim = input$range,
      main = "",
      xlab = "Value",
      ylab = ifelse(input$scaling, "Density", "Frequency")
    )
    
    # overlay normal density
    if (input$scaling) {
      curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lwd = 2)
    }
    
  })
}

ecdf_logic <- function(input, output, session) {
  
  # --- Data generation ---
  
  # store a fixed random sample
  master_vec <- reactiveVal()
  
  observeEvent(input$recompute_ecdf, ignoreNULL = FALSE, {
    master_vec(rnorm(1000))
  })
  
  # --- ECDF plot ---
  
  output$ecdfPlot <- renderPlot({
    req(master_vec())
    
    # subset sample size
    current_data <- master_vec()[1:input$n_show]
    
    df <- data.frame(x = current_data)
    
    ggplot(df, aes(x)) +
      # theoretical CDF
      stat_function(fun = pnorm, color = "red", linewidth = 1.2, alpha = 0.6) +
      # empirical CDF
      stat_ecdf(geom = "step", color = "#2c3e50", linewidth = 1) +
      labs(subtitle = paste("n =", input$n_show), y = "F(x)") +
      theme_minimal(base_size = 16)
  })
  
  # --- Density / histogram plot ---
  
  output$densPlot <- renderPlot({
    req(master_vec())
    
    current_data <- master_vec()[1:input$n_show]
    
    df <- data.frame(x = current_data)
    
    ggplot(df, aes(x)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 30,
        fill = "steelblue",
        color = "white",
        alpha = 0.4
      ) +
      # theoretical density
      stat_function(fun = dnorm, color = "red", linewidth = 1.2) +
      # empirical density estimate
      geom_density(color = "#2c3e50", linewidth = 1, linetype = "dashed") +
      labs(subtitle = paste("n =", input$n_show), y = "f(x)") +
      theme_minimal(base_size = 16)
  })
  
}

skew_logic <- function(input, output, session) {
  
  # Reactive parameters for a standardized Gamma
  params <- reactive({
    skew <- max(0.01, input$s_skew) # Avoid division by zero
    shape <- 4 / (skew^2)
    scale <- 0.5 * skew # derived from Var = shape * scale^2 = 1
    list(shape = shape, scale = scale, shift = -(shape * scale))
  })
  
  skew_data <- reactive({
    input$resample_s
    p <- params()
    rgamma(input$s_n, shape = p$shape, scale = p$scale) + p$shift
  })
  
  output$s_dens <- renderPlot({
    p <- params()
    x_seq <- seq(-4, 6, length.out = 200)
    # Density of Gamma(x - shift)
    y_vals <- dgamma(x_seq - p$shift, shape = p$shape, scale = p$scale)
    
    plot(x_seq, y_vals, type="l", lwd=3, main="Skewed Distribution (Type III)", 
         xlab="x", ylab="Density", col="darkorange")
    abline(v=0, col="red", lty=2) # Mean is at 0
  })
  
  output$s_hist <- renderPlot({
    
    x <- req(skew_data())
    
    par(mar = c(4, 4, 2, 1))
    
    hist(
      x,
      breaks = seq(-10, 10, by = 0.5),
      col = "gray80",
      border = "white",
      xlim = c(-7, 7),
      ylim = c(0, input$s_n / 2),
      main = "Sample Histogram",
      xlab = "Value"
    )
  })
  
  # --- Boxplot ---
  
  output$s_box <- renderPlot({
    
    x <- req(skew_data())
    
    par(mar = c(4, 4, 2, 1))
    
    boxplot(
      x,
      horizontal = TRUE,
      col = "steelblue",
      ylim = c(-7, 7),
      main = "Sample Boxplot",
      frame = FALSE
    )
  })
  
  # --- ECDF ---
  
  output$s_ecdf <- renderPlot({
    
    x <- req(skew_data())
    
    moments <- c(
      mean = input$p_mu,
      variance = input$p_var,
      skewness = input$p_skew,
      kurtosis = input$p_kurt
    )
    
    par(mar = c(4, 4, 2, 1))
    
    plot(
      ecdf(x),
      main = "ECDF vs Theoretical CDF",
      xlim = c(-7, 7),
      ylim = c(0, 1),
      col = "steelblue",
      lwd = 2
    )
    
    
    p <- params()
    pgammashift <- function(q) pgamma(q - p$shift, shape = p$shape, scale = p$scale)
    
    curve(
      pgammashift(x),
      add = TRUE,
      col = "red",
      lwd = 2
    )
    
    
    legend(
      "bottomright",
      legend = c("Sample ECDF", "Theoretical CDF"),
      col = c("steelblue", "red"),
      lty = 1,
      lwd = 2,
      bty = "n"
    )
  })
  
  # --- Normal QQ plot ---
  
  output$s_qq <- renderPlot({
    
    x <- req(skew_data())
    
    par(mar = c(4, 4, 2, 1))
    
    qqnorm(
      x,
      xlim = c(-3, 3),
      ylim = c(-7, 7),
      main = "Normal Q-Q Plot",
      pch = 19,
      col = rgb(0, 0, 0, 0.3)
    )
    
    qqline(x, col = "red", lwd = 2)
  })
  
  # --- Summary statistics ---
  
  output$s_sum <- renderTable({
    
    x <- req(skew_data())
    
    s_mean <- mean(x)
    s_var  <- var(x)
    
    # standardized moments
    z <- (x - s_mean) / sqrt(s_var)
    
    s_skew <- mean(z^3)
    s_kurt <- mean(z^4)
    
    data.frame(
      Statistic = c(
        "Sample Mean",
        "Sample Variance",
        "Sample Skewness",
        "Sample Kurtosis",
        "Sample Median"
      ),
      Value = as.character(
        round(c(s_mean, s_var, s_skew, s_kurt, median(x)), 3)
      )
    )
    
  }, striped = TRUE, width = "100%")
}

kurt_logic <- function(input, output, session) {
  
  # Reactive parameters for a standardized t-distribution
  params <- reactive({
    kurt <- max(3.01, input$k_kurt)
    df <- 6 / (kurt - 3) + 4
    scale_factor <- sqrt((df - 2) / df)
    list(df = df, scale = scale_factor)
  })
  
  kurt_data <- reactive({
    input$resample_k
    p <- params()
    rt(input$k_n, df = p$df) * p$scale
  })
  
  output$k_dens <- renderPlot({
    p <- params()
    x_seq <- seq(-5, 5, length.out = 200)
    # Scaled t-density: f(x) = (1/scale) * dt(x/scale, df)
    y_vals <- (1/p$scale) * dt(x_seq / p$scale, df = p$df)
    
    plot(x_seq, y_vals, type="l", lwd=3, main="Fat-Tailed Distribution (Type VII)", 
         xlab="x", ylab="Density", col="purple")
    # Compare with Normal
    curve(dnorm(x), add=TRUE, col="gray", lty=2)
  })
  
  output$k_hist <- renderPlot({
    
    x <- req(kurt_data())
    
    par(mar = c(4, 4, 2, 1))
    
    hist(
      x,
      breaks = seq(-10, 10, by = 0.5),
      col = "gray80",
      border = "white",
      xlim = c(-7, 7),
      ylim = c(0, input$k_n / 2),
      main = "Sample Histogram",
      xlab = "Value"
    )
  })
  
  # --- Boxplot ---
  
  output$k_box <- renderPlot({
    
    x <- req(kurt_data())
    
    par(mar = c(4, 4, 2, 1))
    
    boxplot(
      x,
      horizontal = TRUE,
      col = "steelblue",
      ylim = c(-7, 7),
      main = "Sample Boxplot",
      frame = FALSE
    )
  })
  
  # --- ECDF ---
  
  output$k_ecdf <- renderPlot({
    
    x <- req(kurt_data())
    
    moments <- c(
      mean = input$p_mu,
      variance = input$p_var,
      skewness = input$p_skew,
      kurtosis = input$p_kurt
    )
    
    par(mar = c(4, 4, 2, 1))
    
    plot(
      ecdf(x),
      main = "ECDF vs Theoretical CDF",
      xlim = c(-7, 7),
      ylim = c(0, 1),
      col = "steelblue",
      lwd = 2
    )
    
    
    p <- params()
    ptrescl <- function(q) pt(q / p$scale, df = p$df)
    
    curve(
      ptrescl(x),
      add = TRUE,
      col = "red",
      lwd = 2
    )
    
    legend(
      "bottomright",
      legend = c("Sample ECDF", "Theoretical CDF"),
      col = c("steelblue", "red"),
      lty = 1,
      lwd = 2,
      bty = "n"
    )
  })
  
  # --- Normal QQ plot ---
  
  output$k_qq <- renderPlot({
    
    x <- req(kurt_data())
    
    par(mar = c(4, 4, 2, 1))
    
    qqnorm(
      x,
      xlim = c(-3, 3),
      ylim = c(-7, 7),
      main = "Normal Q-Q Plot",
      pch = 19,
      col = rgb(0, 0, 0, 0.3)
    )
    
    qqline(x, col = "red", lwd = 2)
  })
  
  # --- Summary statistics ---
  
  output$k_sum <- renderTable({
    
    x <- req(kurt_data())
    
    s_mean <- mean(x)
    s_var  <- var(x)
    
    # standardized moments
    z <- (x - s_mean) / sqrt(s_var)
    
    s_skew <- mean(z^3)
    s_kurt <- mean(z^4)
    
    data.frame(
      Statistic = c(
        "Sample Mean",
        "Sample Variance",
        "Sample Skewness",
        "Sample Kurtosis",
        "Sample Median"
      ),
      Value = as.character(
        round(c(s_mean, s_var, s_skew, s_kurt, median(x)), 3)
      )
    )
    
  }, striped = TRUE, width = "100%")
}

spread_logic <- function(input, output, session) {
  
  # --- Data generation ---
  
  spread_data_rand <- reactiveVal(rnorm(7, mean = 0, sd = 1))
  
  observeEvent(input$resample_spread, {
    spread_data_rand(rnorm(7, mean = 0, sd = 2))
  })
  
  spread_data <- reactive({
    input$resample_spread
    # sample + manipulated outlier
    c(input$outlier_val, spread_data_rand())
  })
  
  # helper: draw observations on 1D axis
  draw_base_points <- function(x, center, center_label) {
    plot(
      x, rep(0, length(x)),
      pch = c(23, rep(21, length(x) - 1)),
      bg = "steelblue",
      cex = 2,
      xlim = c(-10, 10),
      ylim = c(-2, 10),
      yaxt = "n",
      ylab = "",
      xlab = "Value"
    )
    
    abline(h = 0, col = "gray80")
    abline(v = center, col = "red", lwd = 2, lty = 2)
    
    text(center, 9, center_label, col = "red", pos = 4)
  }
  
  # --- Standard deviation ---
  
  output$plot_s <- renderPlot({
    
    x <- req(spread_data())
    
    m <- mean(x)
    s <- sd(x)
    
    step <- input$spread_step
    
    par(mar = c(4, 1, 2, 1))
    
    draw_base_points(x, m, "Mean")
    
    if (step >= 2) {
      # distances to mean
      for (i in 1:length(x)) {
        lines(c(x[i], m), c(i, i), col = "steelblue", lwd = 2)
      }
    }
    
    if (step >= 3) {
      # squared deviations
      for (i in 1:length(x)) {
        rect(m, i - 0.4, m + (x[i] - m)^2, i + 0.4, col = rgb(1, 0, 0, 0.3))
      }
      
      text(m, 9.5, "Squared Deviations", cex = 0.8)
    }
    
    if (step == 4) {
      arrows(
        m - s, -1,
        m + s, -1,
        code = 3,
        angle = 90,
        length = 0.1,
        lwd = 4,
        col = "darkgreen"
      )
      
      text(m, -1.5, paste("S =", round(s, 2)), col = "darkgreen", font = 2)
    }
    
  })
  
  # --- MAD ---
  
  output$plot_mad <- renderPlot({
    
    x <- req(spread_data())
    
    med <- median(x)
    
    smad <- mad(x, constant = 1)
    
    step <- input$spread_step
    
    par(mar = c(4, 1, 2, 1))
    
    draw_base_points(x, med, "Median")
    
    if (step >= 2) {
      # absolute deviations
      for (i in 1:length(x)) {
        lines(c(x[i], med), c(i, i), col = "purple", lwd = 2)
      }
    }
    
    if (step == 3) {
      arrows(
        med - smad, -1,
        med + smad, -1,
        code = 3,
        angle = 90,
        length = 0.1,
        lwd = 4,
        col = "purple"
      )
      
      text(med, -1.5, paste("MAD =", round(smad, 2)), col = "purple", font = 2)
    }
    
    if (step == 4) {
      # scaled MAD (normal reference)
      arrows(
        med - smad / 0.674, -1,
        med + smad / 0.674, -1,
        code = 3,
        angle = 90,
        length = 0.1,
        lwd = 4,
        col = "purple"
      )
      
      text(
        med,
        -1.5,
        paste("Scaled MAD =", round(smad / 0.674, 2)),
        col = "purple",
        font = 2
      )
    }
    
  })
  
  # --- IQR ---
  
  output$plot_iqr <- renderPlot({
    
    x <- req(spread_data())
    
    qs <- quantile(x, c(0.25, 0.5, 0.75))
    
    iqr_val <- diff(qs[c(1, 3)])
    
    step <- input$spread_step
    
    par(mar = c(4, 1, 2, 1))
    
    draw_base_points(x, qs[2], "Median")
    
    if (step >= 2) {
      # highlight central 50%
      ordered_x <- sort(x)
      
      points(ordered_x[3:6], rep(0, 4), pch = 21, bg = "orange", cex = 2.5)
      
      abline(v = qs[1], col = "orange", lwd = 2)
      abline(v = qs[3], col = "orange", lwd = 2)
      
      rect(qs[1], 1, qs[3], 5, col = rgb(1, 0.6, 0, 0.2), border = NA)
    }
    
    if (step == 3) {
      arrows(
        qs[1], -1,
        qs[3], -1,
        code = 3,
        angle = 90,
        length = 0.1,
        lwd = 4,
        col = "orange"
      )
      
      text(qs[2], -1.5, paste("IQR =", round(iqr_val, 2)), col = "orange", font = 2)
    }
    
    if (step == 4) {
      # scaled IQR (normal reference)
      arrows(
        qs[2] - iqr_val / 1.349, -1,
        qs[2] + iqr_val / 1.349, -1,
        code = 3,
        angle = 90,
        length = 0.1,
        lwd = 4,
        col = "orange"
      )
      
      text(
        qs[2],
        -1.5,
        paste("Scaled IQR =", round(iqr_val / 1.349, 2)),
        col = "orange",
        font = 2
      )
    }
    
  })
  
}

location_logic <- function(input, output, session) {
  
  # --- Data generation ---
  
  loc_data_rand <- reactiveVal(rnorm(9, mean = 0, sd = 2))
  
  observeEvent(input$resample_loc, {
    loc_data_rand(rnorm(9, mean = 0, sd = 2))
  })
  
  loc_data <- reactive({
    input$resample_loc
    # sample + manipulated outlier
    c(input$loc_outlier, loc_data_rand())
  })
  
  # --- Location measures plot ---
  
  output$plot_location <- renderPlot({
    
    x <- req(loc_data())
    
    n <- length(x)
    
    alpha <- input$loc_alpha
    
    # location measures
    val_mean   <- mean(x)
    val_median <- median(x)
    val_trim   <- mean(x, trim = alpha)
    
    # number of observations trimmed per side
    k <- floor(alpha * n)
    
    kept_indices <- (k + 1):(n - k)
    
    # mark trimmed vs kept observations
    pt_colors <- rep("gray80", n)
    pt_colors[kept_indices] <- "steelblue"
    
    # reorder colors to match original data order
    sorted_indices <- order(x)
    pt_colors <- pt_colors[order(sorted_indices)]
    
    par(mar = c(5, 1, 4, 1))
    
    plot(
      x, rep(0, n),
      pch = c(23, rep(21, n - 1)),
      bg = pt_colors,
      cex = 2.5,
      xlim = c(-15, 15),
      ylim = c(-1, 5),
      yaxt = "n",
      ylab = "",
      xlab = "Value",
      main = "Sensitivity to Outliers"
    )
    
    abline(h = 0, col = "gray90")
    
    # location markers
    abline(v = val_mean,   col = "firebrick", lwd = 3)
    abline(v = val_median, col = "darkgreen", lwd = 3, lty = 2)
    abline(v = val_trim,   col = "orange",    lwd = 3, lty = 3)
    
    legend(
      "topleft",
      legend = c(
        paste("Mean:", round(val_mean, 2)),
        paste("Median:", round(val_median, 2)),
        paste("Trimmed Mean:", round(val_trim, 2))
      ),
      col = c("firebrick", "darkgreen", "orange"),
      lty = c(1, 2, 3),
      lwd = 3,
      bty = "n",
      cex = 1.2
    )
    
    text(-15, 3.5, paste("Trimmed points per side (k):", k), pos = 4, font = 3)
    
  })
  
}

quantile_logic <- function(input, output, session) {
  
  # --- Data generation ---
  
  q_master_data <- reactiveVal(rnorm(500))
  q_master_jitter <- reactiveVal(jitter(rep(1, 500), amount = 0.2))
  
  observeEvent(input$resample_q, ignoreNULL = FALSE, {
    nval <- 500
    q_master_data(rnorm(nval))
    q_master_jitter(jitter(rep(1, nval), amount = 0.2))
  })
  
  # --- Current sample ---
  
  q_current_data <- reactive({
    req(q_master_data())
    q_master_data()[1:min(input$q_n_show, length(q_master_data()))]
  })
  
  q_current_jitter <- reactive({
    req(q_master_jitter())
    q_master_jitter()[1:min(input$q_n_show, length(q_master_jitter()))]
  })
  
  # --- Jitter plot with highlighted quantile observation ---
  
  output$q_jitter <- renderPlot({
    
    x <- q_current_data()
    thejitter <- q_current_jitter()
    
    n <- length(x)
    p <- input$q_prob
    
    # empirical quantile location
    idx <- min(n, floor(p * n + 1))
    
    # sort observations
    sorted_indices <- order(x)
    x <- x[sorted_indices]
    thejitter <- thejitter[sorted_indices]
    
    q_val <- x[idx]
    
    cols <- rep(rgb(0,0,0,0.2), n)
    cols[idx] <- "red"
    
    par(mar = c(4, 1, 4, 1))
    
    plot(
      x, thejitter,
      pch = 19,
      col = cols,
      cex = 2,
      xlim = c(-4, 4),
      ylim = c(0.5, 2.5),
      yaxt = "n",
      ylab = "",
      xlab = "Observation Value"
    )
    
    abline(v = q_val, col = "red", lty = 2)
    
    # theoretical quantile
    q_val_theor <- qnorm(input$q_prob)
    
    mtext(
      paste0(
        "Empirical Quantile (p = ", p, "): ",
        round(q_val, 3),
        "    (theoretical quantile: ",
        round(q_val_theor, 3), ")"
      ),
      side = 3, line = 2, font = 2
    )
    
    mtext(
      paste(
        "Index:", idx, "|",
        "Below/At:", round(idx / n * 100, 1), "% |",
        "Above:", round((n - idx) / n * 100, 1), "%"
      ),
      side = 3,
      line = 0.5
    )
    
  })
  
  # --- ECDF view ---
  
  output$q_ecdf <- renderPlot({
    
    x <- q_current_data()
    q_val <- quantile(x, probs = input$q_prob)
    
    par(mar = c(4, 4, 2, 1))
    
    plot(
      ecdf(x),
      main = "ECDF and p-threshold",
      xlim = c(-4, 4),
      col = "steelblue",
      lwd = 2
    )
    
    abline(h = input$q_prob, col = "red", lty = 3)
    abline(v = q_val, col = "red", lwd = 2)
    
    points(q_val, input$q_prob, pch = 19, col = "red", cex = 1.5)
    
  })
  
  # --- Histogram view ---
  
  output$q_hist <- renderPlot({
    
    x <- q_current_data()
    q_val <- quantile(x, probs = input$q_prob)
    
    par(mar = c(4, 4, 2, 1))
    
    hist(
      x,
      breaks = 20,
      col = "gray90",
      border = "white",
      xlim = c(-4, 4),
      main = "",
      xlab = "Value"
    )
    
    abline(v = q_val, col = "red", lwd = 3)
    
  })
  
  # --- CDF representation ---
  
  output$q_cdf <- renderPlot({
    
    xv <- seq(-4, 4, length.out = 1000)
    
    cdfval <- pnorm(xv)
    q_val <- qnorm(input$q_prob)
    
    par(mar = c(4, 4, 2, 1))
    
    plot(
      xv, cdfval,
      main = "CDF and quantile",
      xlim = c(-4, 4),
      col = "steelblue",
      lwd = 2,
      type = "l",
      xlab = "Value",
      ylab = "F(x)"
    )
    
    abline(h = input$q_prob, col = "red", lty = 3)
    abline(v = q_val, col = "red", lwd = 2)
    
    points(q_val, input$q_prob, pch = 19, col = "red", cex = 1.5)
    
    text(-3.5, input$q_prob + 0.05, paste0("p = ", input$q_prob), col = "red", font = 2)
    text(q_val + 0.25, 0, paste0("xp = ", round(q_val, 3)), col = "red", font = 2)
    
  })
  
  # --- Density representation ---
  
  output$q_dens <- renderPlot({
    
    xv <- seq(-4, 4, length.out = 1000)
    
    densval <- dnorm(xv)
    q_val <- qnorm(input$q_prob)
    
    par(mar = c(4, 4, 2, 1))
    
    plot(
      xv, densval,
      main = "Density and quantile",
      xlim = c(-4, 4),
      col = "steelblue",
      lwd = 2,
      type = "l"
    )
    
    abline(v = q_val, col = "red", lwd = 2)
    
    # shade probability mass left of quantile
    x_shade <- seq(-4, q_val, length.out = 1000)
    y_shade <- dnorm(x_shade)
    
    polygon(
      c(x_shade, rev(x_shade)),
      c(y_shade, rep(0, length(y_shade))),
      col = rgb(1, 0, 0, 0.3),
      border = NA
    )
    
    text(q_val, dnorm(q_val) / 2, paste("p =", input$q_prob), col = "red", font = 2)
    
  })
  
}

boxplot_logic <- function(input, output, session) {
  
  # --- Master data: 100 samples of size 100 from N(0,1) ---
  
  box_base_data <- reactiveVal(rnorm(19))
  box_jitter <- reactiveVal(jitter(rep(1, 20), amount = 0.1))
  
  observeEvent(input$resample_box, ignoreNULL = FALSE, {
    box_base_data(rnorm(19))
    box_jitter(jitter(rep(1, 20), amount = 0.1))
  })
  
  box_full_data <- reactive({
    c(box_base_data(), input$box_obs_val)
  })
  
  output$box_main_plot <- renderPlot({
    x <- box_full_data()
    jit <- box_jitter()
    
    # Calculate stats
    stats_list <- boxplot.stats(x)
    q1 <- quantile(x, 0.25)
    q3 <- quantile(x, 0.75)
    iqr <- q3 - q1
    lower_fence <- q1 - 1.5 * iqr
    upper_fence <- q3 + 1.5 * iqr
    
    # Color logic
    p_cols <- ifelse(x < lower_fence | x > upper_fence, "#e41a1c", 
                     ifelse(x >= q1 & x <= q3, "steelblue", "gray60"))
    
    # Adjusting margins for better label spacing
    par(mar = c(5, 5, 4, 10), xpd = TRUE) 
    
    # Initialize plot with wide Y limits
    plot(NULL, NULL, xlim = c(0.5, 1.5), ylim = c(-4, 4), 
         xaxt = "n", xlab = "", ylab = "Value", cex.lab = 1.2,
         main = "Interactive Boxplot Construction")
    
    axis(1, at = c(0.75, 1.25), labels = c("Summary (Boxplot)", "Raw Observations"), cex.axis = 1.1)
    
    # 1. Draw Fences first (so they are in the background)
    abline(h = c(lower_fence, upper_fence), col = "red", lty = 3, lwd = 1.5)
    
    # 2. Draw the Boxplot
    boxplot(x, add = TRUE, at = 0.75, col = "gray97", 
            outline = TRUE)
    
    # 3. Draw Pointcloud
    # Map points to the 1.25 position
    points(jit + 0.25, x, pch = 19, col = p_cols, cex = 1.8)
    
    # 4. Highlight Point #20
    points(jit[20] + 0.25, input$box_obs_val, pch = 1, cex = 3, lwd = 2.5, col = "black")
    
    # 5. Descriptive Text for Fences
    text(1.55, upper_fence+0.2, "Upper Fence\n(Q3 + 1.5*IQR)", col = "red", pos = 4, cex = 0.9)
    text(1.55, lower_fence+0.2, "Lower Fence\n(Q1 - 1.5*IQR)", col = "red", pos = 4, cex = 0.9)
    
    # 6. Improved Legend placement (outside plot to the right)
    legend(1.55, 5, 
           legend = c("Outlier", "Inside IQR", "Whisker Range", "Target Point (#20)"),
           col = c("#e41a1c", "steelblue", "gray60", "black"), 
           pch = c(19, 19, 19, 1), 
           pt.cex = c(1.8, 1.8, 1.8, 2.5),
           bty = "n", cex = 1.1, title = "Point Classification", title.font = 2)
  })
}

mle_norm1_logic <- function(input, output, session) {
  
  # --- Data generation ---
  
  # small sample used for likelihood calculations
  mle_data <- reactiveVal(rnorm(8, mean = 0.0, sd = 1))
  
  observeEvent(input$resample_mle_n1, {
    mle_data(rnorm(8, mean = 0, sd = 1))
  })
  
  observeEvent(input$jump_to_mle_n1, {
    dat <- mle_data()
    # The MLE for the Mean (mu) of a Normal distribution is the sample mean
    mle_mu <- mean(dat)
    
    # Update the slider to the calculated MLE
    updateSliderInput(session, "mu_guess_n1", value = mle_mu)
  })
  
  # --- Intuition plot ---
  
  output$mleDataPlot_n1 <- renderPlot({
    
    dat <- data.frame(x = mle_data())
    
    mu <- input$mu_guess_n1
    sigma <- 1
    
    # density values at observations
    dat$y_dens <- dnorm(dat$x, mean = mu, sd = sigma)
    
    ggplot(dat, aes(x = x)) +
      stat_function(
        fun = dnorm,
        args = list(mean = mu, sd = sigma),
        color = "firebrick",
        linewidth = 1.5
      ) +
      # likelihood contributions
      geom_segment(
        aes(xend = x, y = 0, yend = y_dens),
        linetype = "dashed",
        color = "steelblue",
        linewidth = 1
      ) +
      geom_point(aes(y = 0), size = 4, color = "black") +
      geom_point(aes(y = y_dens), size = 3, color = "steelblue") +
      xlim(-5, 5) +
      ylim(0, 0.8) +
      labs(
        y = "Density",
        x = "Observation Value",
        subtitle = "The likelihood is the product of the blue heights."
      ) +
      theme_minimal(base_size = 14)
  })
  
  # --- Likelihood wrt mu ---
  
  output$likPlotMu_n1 <- renderPlot({
    
    mu_range <- seq(-3, 3, length.out = 300)
    
    # likelihood = product of densities
    lik_vals <- sapply(mu_range, function(m)
      prod(dnorm(mle_data(), m, 1)))
    
    df_lik <- data.frame(mu = mu_range, L = lik_vals)
    
    current_L <- prod(dnorm(mle_data(), input$mu_guess_n1, 1))
    
    ggplot(df_lik, aes(mu, L)) +
      geom_line(color = "darkgreen", linewidth = 1) +
      geom_point(aes(x = input$mu_guess_n1, y = current_L), color = "red", size = 4) +
      labs(x = "μ", y = "L(μ)") +
      theme_minimal()
  })
  
  # --- Log-likelihood wrt mu ---
  
  output$logLikPlotMu_n1 <- renderPlot({
    
    mu_range <- seq(-3, 3, length.out = 300)
    
    # log-likelihood = sum of log-densities
    loglik_vals <- sapply(mu_range, function(m)
      sum(dnorm(mle_data(), m, 1, log = TRUE)))
    
    df_loglik <- data.frame(mu = mu_range, logL = loglik_vals)
    
    current_logL <- sum(dnorm(mle_data(), input$mu_guess_n1, 1, log = TRUE))
    
    ggplot(df_loglik, aes(mu, logL)) +
      geom_line(color = "purple", linewidth = 1) +
      geom_point(aes(x = input$mu_guess_n1, y = current_logL), color = "red", size = 4) +
      labs(x = "μ", y = "ℓ(μ)") +
      theme_minimal()
  })
  
}

mle_norm2_logic <- function(input, output, session) {
  
  # --- Data generation ---
  
  # small sample used for likelihood calculations
  mle_data <- reactiveVal(rnorm(8, mean = 0.0, sd = 1))
  
  observeEvent(input$resample_mle, {
    mle_data(rnorm(8, mean = 0, sd = 1))
  })
  
  observeEvent(input$jump_to_mle, {
    dat <- mle_data()
    # The MLE for the Mean (mu) of a Normal distribution is the sample mean
    mle_mu <- mean(dat)
    mle_sigma <- sqrt(1/length(dat) * sum((dat - mle_mu)^2))
    
    # Update the slider to the calculated MLE
    updateSliderInput(session, "mu_guess", value = mle_mu)
    updateSliderInput(session, "sd_guess", value = mle_sigma)
  })
  
  # --- Intuition plot ---
  
  output$mleDataPlot <- renderPlot({
    
    dat <- data.frame(x = mle_data())
    
    mu <- input$mu_guess
    sigma <- input$sd_guess
    
    # density values at observations
    dat$y_dens <- dnorm(dat$x, mean = mu, sd = sigma)
    
    ggplot(dat, aes(x = x)) +
      stat_function(
        fun = dnorm,
        args = list(mean = mu, sd = sigma),
        color = "firebrick",
        linewidth = 1.5
      ) +
      # likelihood contributions
      geom_segment(
        aes(xend = x, y = 0, yend = y_dens),
        linetype = "dashed",
        color = "steelblue",
        linewidth = 1
      ) +
      geom_point(aes(y = 0), size = 4, color = "black") +
      geom_point(aes(y = y_dens), size = 3, color = "steelblue") +
      xlim(-5, 5) +
      ylim(0, 0.8) +
      labs(
        y = "Density",
        x = "Observation Value",
        subtitle = "The likelihood is the product of the blue heights."
      ) +
      theme_minimal(base_size = 14)
  })
  
  # --- Likelihood wrt mu ---
  
  output$likPlotMu <- renderPlot({
    
    mu_range <- seq(-3, 3, length.out = 300)
    
    # likelihood = product of densities
    lik_vals <- sapply(mu_range, function(m)
      prod(dnorm(mle_data(), m, input$sd_guess)))
    
    df_lik <- data.frame(mu = mu_range, L = lik_vals)
    
    current_L <- prod(dnorm(mle_data(), input$mu_guess, input$sd_guess))
    
    ggplot(df_lik, aes(mu, L)) +
      geom_line(color = "darkgreen", linewidth = 1) +
      geom_point(aes(x = input$mu_guess, y = current_L), color = "red", size = 4) +
      labs(x = "μ", y = "L(μ)") +
      theme_minimal()
  })
  
  # --- Log-likelihood wrt mu ---
  
  output$logLikPlotMu <- renderPlot({
    
    mu_range <- seq(-3, 3, length.out = 300)
    
    # log-likelihood = sum of log-densities
    loglik_vals <- sapply(mu_range, function(m)
      sum(dnorm(mle_data(), m, input$sd_guess, log = TRUE)))
    
    df_loglik <- data.frame(mu = mu_range, logL = loglik_vals)
    
    current_logL <- sum(dnorm(mle_data(), input$mu_guess, input$sd_guess, log = TRUE))
    
    ggplot(df_loglik, aes(mu, logL)) +
      geom_line(color = "purple", linewidth = 1) +
      geom_point(aes(x = input$mu_guess, y = current_logL), color = "red", size = 4) +
      labs(x = "μ", y = "ℓ(μ)") +
      theme_minimal()
  })
  
  # --- Likelihood wrt sigma ---
  
  output$likPlotSigma <- renderPlot({
    
    sig_range <- seq(0.1, 4, length.out = 300)
    
    lik_vals <- sapply(sig_range, function(s)
      prod(dnorm(mle_data(), input$mu_guess, s)))
    
    df_lik <- data.frame(sigma = sig_range, L = lik_vals)
    
    current_L <- prod(dnorm(mle_data(), input$mu_guess, input$sd_guess))
    
    ggplot(df_lik, aes(sigma, L)) +
      geom_line(color = "darkorange", linewidth = 1) +
      geom_point(aes(x = input$sd_guess, y = current_L), color = "red", size = 4) +
      labs(x = "σ", y = "L(σ)") +
      theme_minimal()
  })
  
  # --- Log-likelihood wrt sigma ---
  
  output$logLikPlotSigma <- renderPlot({
    
    sig_range <- seq(0.1, 4, length.out = 300)
    
    loglik_vals <- sapply(sig_range, function(s)
      sum(dnorm(mle_data(), input$mu_guess, s, log = TRUE)))
    
    df_loglik <- data.frame(sigma = sig_range, logL = loglik_vals)
    
    current_logL <- sum(dnorm(mle_data(), input$mu_guess, input$sd_guess, log = TRUE))
    
    ggplot(df_loglik, aes(sigma, logL)) +
      geom_line(color = "brown", linewidth = 1) +
      geom_point(aes(x = input$sd_guess, y = current_logL), color = "red", size = 4) +
      labs(x = "σ", y = "ℓ(σ)") +
      theme_minimal()
  })
  
}

mle_bern_logic <- function(input, output, session) {
  
  # --- Data generation ---
  # Initialize with p=0.3 as requested
  mle_data_b <- reactiveVal(rbinom(15, 1, 0.3))
  
  observeEvent(input$resample_bern, {
    mle_data_b(rbinom(20, 1, 0.3))
  })
  
  observeEvent(input$jump_to_mle_bern, {
    dat <- mle_data_b()
    # The MLE for Bernoulli is the sample mean (proportion of 1s)
    mle_val <- mean(dat)
    
    # Ensure we don't hit exactly 0 or 1 if the slider min/max is 0.01/0.99
    mle_val <- max(0.01, min(0.99, mle_val))
    
    updateSliderInput(session, "p_guess", value = mle_val)
  })
  
  # --- Intuition plot ---
  output$mleDataPlot_bern <- renderPlot({
    raw_data <- mle_data_b()
    n <- length(raw_data)
    p_curr <- input$p_guess
    
    # Create a data frame for plotting points (stacked at x=0 and x=1)
    df_points <- data.frame(obs = raw_data)
    df_points$y_stack <- 0 
    # spread points vertically based on their value (0 or 1), 0 values between -0.4 and 0.4, 1 values between 0.6 and 1.4
    df_points$x[df_points$obs == 0] <- seq(-0.3, 0.3, length.out = sum(df_points$obs == 0))
    df_points$x[df_points$obs == 1] <- seq(0.7, 1.3, length.out = sum(df_points$obs == 1))
    
    # PMF step function data
    df_step <- data.frame(
      x = c(-0.5, 0.5, 0.5, 1.5),
      y = c(1-p_curr, 1-p_curr, p_curr, p_curr)
    )
    
    # Likelihood contributions (heights)
    # At x=0, height is 1-p. At x=1, height is p.
    df_points$height <- ifelse(df_points$obs == 0, 1 - p_curr, p_curr)
    
    ggplot() +
      # The "Step" PMF
      geom_step(data = df_step, aes(x, y), color = "firebrick", linewidth = 1.5) +
      # Vertical contribution lines
      geom_segment(data = df_points, 
                   aes(x = x, xend = x, y = y_stack, yend = height),
                   linetype = "dashed", color = "steelblue", alpha = 0.5) +
      # Observed points (stacked)
      geom_point(data = df_points, aes(x = x, y = y_stack), 
                 size = 3, color = "black") +
      # Points at the top of the likelihood heights
      geom_point(data = df_points, aes(x = x, y = height), 
                 size = 2, color = "steelblue") +
      scale_x_continuous(breaks = c(0, 1), limits = c(-0.5, 1.5)) +
      ylim(0, 1.1) +
      labs(
        y = "Probability P(X=x)",
        x = "Outcome",
        subtitle = "Likelihood = (1-p)^(count of 0s) * p^(count of 1s)"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # --- Likelihood Plot ---
  output$likPlotP_bern <- renderPlot({
    p_range <- seq(0.01, 0.99, length.out = 100)
    dat <- mle_data_b()
    
    lik_vals <- sapply(p_range, function(p) prod(dbinom(dat, 1, p)))
    current_L <- prod(dbinom(dat, 1, input$p_guess))
    
    ggplot(data.frame(p = p_range, L = lik_vals), aes(p, L)) +
      geom_line(color = "darkgreen", linewidth = 1) +
      geom_point(aes(x = input$p_guess, y = current_L), color = "red", size = 4) +
      labs(x = "p", y = "L(p)") +
      theme_minimal()
  })
  
  # --- Log-Likelihood Plot ---
  output$logLikPlotP_bern <- renderPlot({
    p_range <- seq(0.01, 0.99, length.out = 100)
    dat <- mle_data_b()
    
    loglik_vals <- sapply(p_range, function(p) sum(dbinom(dat, 1, p, log = TRUE)))
    current_logL <- sum(dbinom(dat, 1, input$p_guess, log = TRUE))
    
    ggplot(data.frame(p = p_range, logL = loglik_vals), aes(p, logL)) +
      geom_line(color = "purple", linewidth = 1) +
      geom_point(aes(x = input$p_guess, y = current_logL), color = "red", size = 4) +
      labs(x = "p", y = "ℓ(p)") +
      theme_minimal()
  })
}

ci_logic <- function(input, output, session) {
  
  # --- Master data: 100 samples of size 100 from N(0,1) ---
  
  ci_master_data <- reactiveVal(matrix(rnorm(100 * 100), nrow = 100))
  ci_master_jitter <- reactiveVal(jitter(rep(0, 100), amount = 0.3))
  
  observeEvent(input$resample_ci, ignoreNULL = FALSE, {
    ci_master_data(matrix(rnorm(100 * 100), nrow = 100))
    ci_master_jitter(jitter(rep(0, 100), amount = 0.3))
  })
  
  # --- Compute confidence intervals ---
  
  intervals_df <- reactive({
    
    raw_data <- ci_master_data() * input$ci_sd
    
    n <- input$ci_n_obs
    conf <- input$ci_conf_level
    alpha <- 1 - conf
    
    num_to_show <- input$ci_show_num
    subset_data <- raw_data[1:num_to_show, 1:n, drop = FALSE]
    
    means <- rowMeans(subset_data)
    
    # Z-interval: mean ± z * (sigma / sqrt(n))
    se <- input$ci_sd / sqrt(n)
    z_crit <- qnorm(1 - alpha/2)
    
    lower <- means - (z_crit * se)
    upper <- means + (z_crit * se)
    
    data.frame(
      id = 1:num_to_show,
      mean = means,
      lower = lower,
      upper = upper,
      hit = (lower <= 0 & upper >= 0)  # interval contains true mean
    )
    
  })
  
  # --- Coverage plot ---
  
  output$ci_plot <- renderPlot({
    
    df <- intervals_df()
    
    n_total <- nrow(df)
    n_hits <- sum(df$hit)
    
    pct <- round((n_hits / n_total) * 100, 1)
    
    par(mar = c(4, 4, 3, 1))
    
    plot(
      0, 0, type = "n",
      xlim = c(-3, 3),
      ylim = c(0.5, n_total + 0.5),
      xlab = "Value",
      ylab = "Interval ID",
      main = paste0(
        "Coverage: ", n_hits, "/", n_total,
        " (", pct, "%) | Conf: ",
        input$ci_conf_level * 100, "%"
      )
    )
    
    abline(v = 0, col = "black", lwd = 2)
    
    colors <- ifelse(df$hit, "steelblue", "#e41a1c")
    
    segments(df$lower, df$id, df$upper, df$id, col = colors, lwd = 2)
    points(df$mean, df$id, pch = 19, col = colors, cex = 0.8)
    
  })
  
  # --- Detail: construction of first CI ---
  
  output$ci_detail <- renderPlot({
    
    first_sample <- ci_master_data()[1, 1:input$ci_n_obs] * input$ci_sd
    jit_vals <- ci_master_jitter()[1:input$ci_n_obs]
    
    df_first <- intervals_df()[1, ]
    
    par(mar = c(4, 4, 2, 1))
    
    plot(
      first_sample, jit_vals,
      pch = 21,
      bg = "gray40",
      col = "white",
      ylim = c(-1, 1),
      xlim = c(-3, 3),
      yaxt = "n",
      ylab = "",
      xlab = "Value",
      main = "Data Points & Resulting CI"
    )
    
    color_hit <- ifelse(df_first$hit, "steelblue", "#e41a1c")
    
    segments(df_first$lower, -0.6, df_first$upper, -0.6,
             col = color_hit, lwd = 5)
    
    points(df_first$mean, -0.6, pch = 18, col = "black", cex = 2)
    
    abline(v = 0, lty = 2)
    
    legend("topright",
           legend = c("Sample Mean", "Data Point"),
           pch = c(18, 21),
           bty = "n")
    
  })
  
  # --- Theoretical z critical values ---
  
  output$ci_theory <- renderPlot({
    
    xv <- seq(-4, 4, length.out = 500)
    yv <- dnorm(xv)
    
    conf <- input$ci_conf_level
    z_crit <- qnorm(1 - (1 - conf)/2)
    
    par(mar = c(4, 4, 2, 1))
    
    plot(
      xv, yv,
      type = "l",
      lwd = 2,
      xlab = "z-score",
      ylab = "Density",
      main = "Standard Normal Critical Values"
    )
    
    x_shade <- seq(-z_crit, z_crit, length.out = 100)
    
    polygon(
      c(x_shade, rev(x_shade)),
      c(dnorm(x_shade), rep(0, 100)),
      col = rgb(70/255, 130/255, 180/255, 0.3),
      border = NA
    )
    
    abline(v = c(-z_crit, z_crit), col = "red", lty = 2)
    
    text(0, 0.15, paste0(conf * 100, "%"),
         font = 2, cex = 1.5, col = "steelblue")
    
    mtext(
      side = 1,
      at = c(-z_crit, z_crit),
      text = round(c(-z_crit, z_crit), 2),
      col = "red",
      line = 0.5
    )
    
  })
  
}

ztest_logic <- function(input, output, session) {
  
  # --- Master data generation based on True Mu ---
  z_master_data <- reactiveVal(matrix(rnorm(100 * 100), nrow = 100))
  z_master_jitter <- reactiveVal(jitter(rep(0, 100), amount = 0.3))
  
  observeEvent(input$resample_z, ignoreNULL = FALSE, {
    # Generate standard normal and shift by true mu later for reactivity
    z_master_data(matrix(rnorm(100 * 100), nrow = 100))
    z_master_jitter(jitter(rep(0, 100), amount = 0.3))
  })
  
  # --- Compute Test Statistics and Rejection ---
  test_results_df <- reactive({
    # Shift standard normal data by the user-defined true mean
    raw_data <- z_master_data() + input$z_true_mu
    n <- input$z_n_obs
    alpha <- input$z_alpha
    num_to_show <- input$z_show_num
    
    subset_data <- raw_data[1:num_to_show, 1:n, drop = FALSE]
    means <- rowMeans(subset_data)
    se <- 1 / sqrt(n) # Sigma is known as 1
    
    # Calculate Critical Values in terms of X-bar (the "Raw" scale)
    if (input$z_alt == "two.sided") {
      crit_val <- qnorm(1 - alpha/2) * se
      lower_crit <- -crit_val
      upper_crit <- crit_val
      rejected <- means < lower_crit | means > upper_crit
    } else if (input$z_alt == "greater") {
      lower_crit <- -Inf
      upper_crit <- qnorm(1 - alpha) * se
      rejected <- means > upper_crit
    } else {
      lower_crit <- qnorm(alpha) * se
      upper_crit = Inf
      rejected <- means < lower_crit
    }
    
    data.frame(
      id = 1:num_to_show,
      mean = means,
      rejected = rejected,
      lower_crit = lower_crit,
      upper_crit = upper_crit
    )
  })
  
  # --- Main Rejection Plot ---
  output$z_plot <- renderPlot({
    df <- test_results_df()
    n_total <- nrow(df)
    n_rej <- sum(df$rejected)
    
    par(mar = c(4, 4, 3, 1))
    
    plot(0, 0, type = "n", xlim = c(-3, 3), ylim = c(0.5, n_total + 0.5),
         xlab = expression(paste("Sample Mean (", bar(x), ")")), ylab = "Trial ID",
         main = paste0("Rejection Rate: ", n_rej, "/", n_total, 
                       " (", round(n_rej/n_total*100), "%) | H0: mu=0"))
    
    # Draw the Critical Regions (Red Rectangles)
    se <- 1 / sqrt(input$z_n_obs)
    alpha <- input$z_alpha
    
    ylims <- par("usr")[3:4]
    xlims <- par("usr")[1:2]
    
    if (input$z_alt == "two.sided" || input$z_alt == "less") {
      l_bound <- if(input$z_alt == "two.sided") qnorm(alpha/2)*se else qnorm(alpha)*se
      rect(-4, 0.5, l_bound, n_total + 0.5, col = rgb(1, 0, 0, 0.1), border = NA)
      abline(v = l_bound, col = "red", lty = 3)
      
      text(mean(c(l_bound, xlims[1])), 1/4 * ylims[1] + 3/4 * ylims[2], "critical region", col = "red", font = 2)
    }
    if (input$z_alt == "two.sided" || input$z_alt == "greater") {
      u_bound <- if(input$z_alt == "two.sided") qnorm(1-alpha/2)*se else qnorm(1-alpha)*se
      rect(u_bound, 0.5, 4, n_total + 0.5, col = rgb(1, 0, 0, 0.1), border = NA)
      abline(v = u_bound, col = "red", lty = 3)
      
      text(mean(c(u_bound, xlims[2])), 1/4 * ylims[1] + 3/4 * ylims[2], "critical region", col = "red", font = 2)
    }
    
    abline(v = 0, lwd = 2, lty = 1) # Null Hypothesis
    
    colors <- ifelse(df$rejected, "#e41a1c", "darkgrey")
    points(df$mean, df$id, pch = 19, col = colors, cex = 0.8)
  })
  
  # --- Detail Plot: Points for sample #1 ---
  output$z_detail <- renderPlot({
    first_sample <- z_master_data()[1, 1:input$z_n_obs] + input$z_true_mu
    res <- test_results_df()[1, ]
    
    par(mar = c(4, 4, 2, 1))
    ylims <- range(z_master_jitter()) + c(-1.5,1.5) * diff(range(z_master_jitter()))
    jit <- z_master_jitter()[1:input$z_n_obs]
    plot(first_sample, jit,
         xlim=c(-4, 4), ylim=ylims, xlab = "Value", ylab="", main = "Individual Observations (Trial #1)")

    xlims <- par("usr")[1:2]

    points(res$mean, 3/4 * ylims[1] + 1/4 * ylims[2], pch = 18, col = ifelse(res$rejected, "red", "blue"), cex = 2.5)
    abline(v = 0, lty = 2)
    

    # also draw critical region
    se <- 1 / sqrt(input$z_n_obs)
    alpha <- input$z_alpha
    if (input$z_alt == "two.sided" || input$z_alt == "less") {
      l_bound <- if(input$z_alt == "two.sided") qnorm(alpha/2)*se else qnorm(alpha)*se
      rect(xlims[1], ylims[1], l_bound, ylims[2], col = rgb(1, 0, 0, 0.1), border = NA)
      abline(v = l_bound, col = "red", lty = 3)
      
      text(mean(c(l_bound, xlims[1])), 1/4 * ylims[1] + 3/4 * ylims[2], "critical\nregion", col = "red", font = 2)
    }
    if (input$z_alt == "two.sided" || input$z_alt == "greater") {
      u_bound <- if(input$z_alt == "two.sided") qnorm(1-alpha/2)*se else qnorm(1-alpha)*se
      rect(u_bound, ylims[1], xlims[2], ylims[2], col = rgb(1, 0, 0, 0.1), border = NA)
      abline(v = u_bound, col = "red", lty = 3)
      
      text(mean(c(u_bound, xlims[2])), 1/4 * ylims[1] + 3/4 * ylims[2], "critical\nregion", col = "red", font = 2)
    }
    
    legend("topright", legend = c("Null Value (0)", "Sample Mean"), lty = c(2, NA), 
           pch = c(NA, 18), col = c("black", "blue"), bty = "n")
  })
  
  # --- Sampling Dist Plot ---
  output$z_theory <- renderPlot({
    se <- 1
    xv <- seq(-3, 3, length.out = 500)
    # Density of the mean under H0
    yv_h0 <- dnorm(xv, mean = 0, sd = se)
    
    par(mar = c(4, 4, 2, 1))
    plot(xv, yv_h0, type = "l", lwd = 2, xlab = expression(bar(x)), ylab = "Density",
         main = "Standard normal critical values")
    
    # Shade Rejection Region
    alpha <- input$z_alpha
    if (input$z_alt %in% c("two.sided", "less")) {
      crit <- if(input$z_alt == "two.sided") qnorm(alpha/2)*se else qnorm(alpha)*se
      x_s <- seq(-4, crit, length.out = 100)
      polygon(c(x_s, crit), c(dnorm(x_s, 0, se), 0), col = rgb(1, 0, 0, 0.3), border = NA)
      
      # label critical value
      mtext(
        side = 1,
        at = crit,
        text = round(crit, 2),
        col = "red",
        line = 0.5
      )
      abline(v = crit, col = "red", lty = 2)
      
      # text above critical region with arrow to it
      # "critical region", x%
      perc <- if(input$z_alt == "two.sided") alpha/2*100 else alpha*100
      text((crit-3)/2, 0.15, paste0("Critical Region\n", round(perc,2), "%"), 
           col = "red", font = 2, cex = 1.2)
      arrows((crit-3)/2, 0.12, (crit-3)/2, 0.02, col = "red", length = 0.1)
    }
    if (input$z_alt %in% c("two.sided", "greater")) {
      crit <- if(input$z_alt == "two.sided") qnorm(1-alpha/2)*se else qnorm(1-alpha)*se
      x_s <- seq(crit, 4, length.out = 100)
      polygon(c(crit, x_s), c(0, dnorm(x_s, 0, se)), col = rgb(1, 0, 0, 0.3), border = NA)
    
      mtext(
        side = 1,
        at = crit,
        text = round(crit, 2),
        col = "red",
        line = 0.5
      )
      abline(v = crit, col = "red", lty = 2)
      
      perc <- if(input$z_alt == "two.sided") alpha/2*100 else alpha*100
      text((crit+3)/2, 0.15, paste0("Critical Region\n", round(perc,2), "%"), 
           col = "red", font = 2, cex = 1.2)
      arrows((crit+3)/2, 0.12, (crit+3)/2, 0.02, col = "red", length = 0.1)
    }
    
    # label inner part with percentage
    text(0, 0.2, paste0(round((1-alpha)*100,2), "%"),
      col = "steelblue", font = 2, cex=2)
    
    # line at y=0
    abline(h = 0)
  })
}

ttest_logic <- function(input, output, session) {
  
  # --- Master data generation based on True Mu ---
  t_master_data <- reactiveVal(matrix(rnorm(100 * 100), nrow = 100))
  t_master_jitter <- reactiveVal(jitter(rep(0, 100), amount = 0.3))
  
  observeEvent(input$resample_t, ignoreNULL = FALSE, {
    t_master_data(matrix(rnorm(100 * 100), nrow = 100))
    t_master_jitter(jitter(rep(0, 100), amount = 0.3))
  })
  
  # --- Compute Test Statistics and Rejection ---
  test_results_df <- reactive({
    raw_data <- t_master_data() + input$t_true_mu
    n <- input$t_n_obs
    alpha <- input$t_alpha
    num_to_show <- input$t_show_num
    df_val <- n - 1
    
    subset_data <- raw_data[1:num_to_show, 1:n, drop = FALSE]
    means <- rowMeans(subset_data)
    
    # Calculate sample standard deviation for each row
    # Using apply for clarity; rowSds from matrixStats would be faster for large scales
    sds <- apply(subset_data, 1, sd)
    se <- sds / sqrt(n)
    
    tstat <- means / se  # t-statistic for each sample
    
    # Calculate rejection based on T-distribution
    # Note: Critical values in 'raw scale' vary per sample because 'se' varies
    if (input$t_alt == "two.sided") {
      t_crit <- qt(1 - alpha/2, df = df_val)
      lower_crit <- -t_crit * se
      upper_crit <- t_crit * se
      rejected <- means < lower_crit | means > upper_crit
    } else if (input$t_alt == "greater") {
      t_crit <- qt(1 - alpha, df = df_val)
      lower_crit <- -Inf
      upper_crit <- t_crit * se
      rejected <- means > upper_crit
    } else {
      t_crit <- qt(alpha, df = df_val)
      lower_crit <- t_crit * se
      upper_crit <- Inf
      rejected <- means < lower_crit
    }
    
    data.frame(
      id = 1:num_to_show,
      mean = means,
      tstat = tstat,
      rejected = rejected,
      lower_crit = lower_crit,
      upper_crit = upper_crit
    )
  })
  
  # --- Main Rejection Plot ---
  output$t_plot <- renderPlot({
    df <- test_results_df()
    n_total <- nrow(df)
    n_rej <- sum(df$rejected)
    
    par(mar = c(4, 4, 3, 1))
    plot(0, 0, type = "n", xlim = c(-8, 8), ylim = c(0.5, n_total + 0.5),
         xlab = expression(paste("t statistic (", sqrt(n), bar(x), "/s)")), ylab = "Trial ID",
         main = paste0("Rejection Rate: ", n_rej, "/", n_total, 
                       " (", round(n_rej/n_total*100), "%) | H0: mu=0"))
    
    # Since SE varies per trial in T-tests, we use the average SE to visualize the region
    df_val <- input$t_n_obs - 1
    ylims <- par("usr")[3:4]
    xlims <- par("usr")[1:2]
    
    if (input$t_alt == "two.sided" || input$t_alt == "less") {
      l_bound <- if(input$t_alt == "two.sided") qt(input$t_alpha/2, df_val) else qt(input$t_alpha, df_val)
      rect(-8, 0.5, l_bound, n_total + 0.5, col = rgb(1, 0, 0, 0.05), border = NA)
      text(mean(c(l_bound, xlims[1])), 0.9 * ylims[2], "critical\nregion", col = "red", font = 2, cex=0.8)
    }
    if (input$t_alt == "two.sided" || input$t_alt == "greater") {
      u_bound <- if(input$t_alt == "two.sided") qt(1-input$t_alpha/2, df_val) else qt(1-input$t_alpha, df_val)
      rect(u_bound, 0.5, 8, n_total + 0.5, col = rgb(1, 0, 0, 0.05), border = NA)
      text(mean(c(u_bound, xlims[2])), 0.9 * ylims[2], "critical\nregion", col = "red", font = 2, cex=0.8)
    }
    
    abline(v = 0, lwd = 2, lty = 1)
    colors <- ifelse(df$rejected, "#e41a1c", "darkgrey")
    points(df$tstat, df$id, pch = 19, col = colors, cex = 0.8)
  })
  
  # --- Detail Plot ---
  output$t_detail <- renderPlot({
    first_sample <- t_master_data()[1, 1:input$t_n_obs] + input$t_true_mu
    res <- test_results_df()[1, ]
    
    par(mar = c(4, 4, 2, 1))
    ylims <- range(t_master_jitter()) + c(-1.5,1.5) * diff(range(t_master_jitter()))
    jit <- t_master_jitter()[1:input$t_n_obs]
    
    plot(first_sample, jit, xlim=c(-4, 4), ylim=ylims, xlab = "Value", ylab="", main = "Individual Observations (Trial #1)")
    xlims <- par("usr")[1:2]
    
    points(res$mean, 3/4 * ylims[1] + 1/4 * ylims[2], pch = 18, col = ifelse(res$rejected, "red", "blue"), cex = 2.5)
    abline(v = 0, lty = 2)
    
    # Draw specific critical region for Sample #1
    if (input$t_alt == "two.sided" || input$t_alt == "less") {
      rect(xlims[1], ylims[1], res$lower_crit, ylims[2], col = rgb(1, 0, 0, 0.1), border = NA)
      abline(v = res$lower_crit, col = "red", lty = 3)
      
      # label critical region
      text(mean(c(res$lower_crit, xlims[1])), 1/4 * ylims[1] + 3/4 * ylims[2], "critical\nregion", col = "red", font = 2)
    }
    if (input$t_alt == "two.sided" || input$t_alt == "greater") {
      rect(res$upper_crit, ylims[1], xlims[2], ylims[2], col = rgb(1, 0, 0, 0.1), border = NA)
      abline(v = res$upper_crit, col = "red", lty = 3)
      
      text(mean(c(res$upper_crit, xlims[2])), 1/4 * ylims[1] + 3/4 * ylims[2], "critical\nregion", col = "red", font = 2)
    }
    
    legend("topright", legend = c("Null Value (0)", "Sample Mean"), lty = c(2, NA), 
           pch = c(NA, 18), col = c("black", "blue"), bty = "n")
  })
  
  # --- Sampling Dist Plot (t-Distribution) ---
  output$t_theory <- renderPlot({
    df_val <- input$t_n_obs - 1
    xv <- seq(-4, 4, length.out = 500)
    yv_h0 <- dt(xv, df = df_val) # T-density
    
    par(mar = c(4, 4, 2, 1))
    plot(xv, yv_h0, type = "l", lwd = 2, xlab = "t-statistic", ylab = "Density",
         main = paste0("t-distribution (df = ", df_val, ")"))
    
    alpha <- input$t_alpha
    if (input$t_alt %in% c("two.sided", "less")) {
      crit <- if(input$t_alt == "two.sided") qt(alpha/2, df_val) else qt(alpha, df_val)
      x_s <- seq(-5, crit, length.out = 100)
      polygon(c(x_s, crit), c(dt(x_s, df_val), 0), col = rgb(1, 0, 0, 0.3), border = NA)
      abline(v = crit, col = "red", lty = 2)
      mtext(side = 1, at = crit, text = round(crit, 2), col = "red", line = 0.5)
      
      perc <- if(input$t_alt == "two.sided") alpha/2*100 else alpha*100
      text((crit-4)/2, 0.15, paste0("Critical Region\n", round(perc,2), "%"), 
           col = "red", font = 2, cex = 1.2)
      arrows((crit-4)/2, 0.12, (crit-4)/2, 0.02, col = "red", length = 0.1)
    }
    if (input$t_alt %in% c("two.sided", "greater")) {
      crit <- if(input$t_alt == "two.sided") qt(1-alpha/2, df_val) else qt(1-alpha, df_val)
      x_s <- seq(crit, 5, length.out = 100)
      polygon(c(crit, x_s), c(0, dt(x_s, df_val)), col = rgb(1, 0, 0, 0.3), border = NA)
      abline(v = crit, col = "red", lty = 2)
      mtext(side = 1, at = crit, text = round(crit, 2), col = "red", line = 0.5)
      
      perc <- if(input$t_alt == "two.sided") alpha/2*100 else alpha*100
      text((crit+4)/2, 0.15, paste0("Critical Region\n", round(perc,2), "%"), 
           col = "red", font = 2, cex = 1.2)
      arrows((crit+4)/2, 0.12, (crit+4)/2, 0.02, col = "red", length = 0.1)
    }
    
    text(0, max(yv_h0)/2, paste0(round((1-alpha)*100,2), "%"), col = "steelblue", font = 2, cex=2)
    abline(h = 0)
  })
}

pval_logic <- function(input, output, session) {
  # --- Master data generation (Single Sample) ---
  # Generate 100 points once; we subset based on input$p_n_obs
  p_master_data <- reactiveVal(rnorm(100))
  p_master_jitter <- reactiveVal(jitter(rep(0, 100), amount = 0.2))
  
  observeEvent(input$resample_p, ignoreNULL = FALSE, {
    p_master_data(rnorm(100))
    p_master_jitter(jitter(rep(0, 100), amount = 0.2))
  })
  
  # --- Observed Statistics ---
  obs_stats <- reactive({
    n <- input$p_n_obs
    # Shift raw data by true mu
    sample_data <- p_master_data()[1:n] + input$p_true_mu
    x_bar <- mean(sample_data)
    se <- 1 / sqrt(n)
    z_obs <- x_bar / se
    
    # Calculate p-value based on alternative
    p_val <- switch(input$p_alt,
                    "two.sided" = 2 * pnorm(-abs(z_obs)),
                    "greater"   = pnorm(z_obs, lower.tail = FALSE),
                    "less"      = pnorm(z_obs, lower.tail = TRUE)
    )
    
    list(x_bar = x_bar, z_obs = z_obs, p_val = p_val, se = se)
  })
  
  # --- Jump to p-value Logic ---
  observeEvent(input$jump_p, {
    updateSliderInput(session, "p_alpha", value = obs_stats()$p_val)
  })
  
  # --- The All-in-One Plot ---
  output$p_main_plot <- renderPlot({
    s <- obs_stats()
    n <- input$p_n_obs
    alpha <- input$p_alpha
    se <- s$se
    
    # Setup plot coordinates
    # We plot on the "Mean" scale to make the data points intuitive
    xv <- seq(-3, 3, length.out = 500)
    yv <- dnorm(xv, mean = 0, sd = se)
    
    densmax <- max(yv)
    
    par(mar = c(5, 4, 4, 2))
    plot(xv, yv, type = "l", lwd = 2, col = "black",
         xlab = expression(paste("Scale of Sample Mean (", bar(x), ")")), 
         ylab = "Density",
         main = paste0("p-value: ", round(s$p_val, 4), " | alpha: ", round(alpha, 3)),
         ylim = c(-0.4, 2*densmax))
    abline(h = 0)
    
    # 3. Plot Individual Data Points (at bottom of plot)
    points(p_master_data()[1:n] + input$p_true_mu, 
           1.5*densmax + p_master_jitter()[1:n] * 0.1*densmax, 
           pch = 21, bg = "black", col = "white", cex=1.5)
    
    # 1. Visualize Critical Region (Alpha) based on H0
    if (input$p_alt == "two.sided" || input$p_alt == "less") {
      curr_alpha <- if(input$p_alt == "two.sided") alpha/2 else alpha
      crit <- qnorm(curr_alpha, sd = se)
      rect(-4, 1.1*densmax, crit, 2*densmax, col = rgb(1, 0, 0, 0.05), border = NA)
      abline(v = crit, col = "red", lty = 3)
      
      # label the critical region
      text(mean(c(crit, -4)), 1.1*densmax + 0.5*densmax, paste0("critical region\nα=", round(alpha, 4)), col = "red", font = 2)
    }
    if (input$p_alt == "two.sided" || input$p_alt == "greater") {
      curr_alpha <- if(input$p_alt == "two.sided") alpha/2 else alpha
      crit <- qnorm(1 - curr_alpha, sd = se)
      rect(crit, 1.1*densmax, 4, 2*densmax, col = rgb(1, 0, 0, 0.05), border = NA)
      abline(v = crit, col = "red", lty = 3)
      
      text(mean(c(crit, 4)), 1.1*densmax + 0.5*densmax, paste0("critical region\nα=", round(alpha, 4)), col = "red", font = 2)
    }
    
    # 2. Visualize p-value Area (The "Observed" Extremity)
    if (input$p_alt == "two.sided") {
      ext <- abs(s$x_bar)
      x_left <- seq(-4, -ext, length.out = 100)
      x_right <- seq(ext, 4, length.out = 100)
      polygon(c(x_left, -ext), c(dnorm(x_left, sd = se), 0), col = rgb(0, 0, 1, 0.3), border = NA)
      polygon(c(ext, x_right), c(0, dnorm(x_right, sd = se)), col = rgb(0, 0, 1, 0.3), border = NA)
      
      text(0, 0.5*densmax, paste0("p-value area\n", round(s$p_val, 4)), col = "blue", font = 2)
    } else if (input$p_alt == "greater") {
      x_s <- seq(s$x_bar, 4, length.out = 100)
      polygon(c(s$x_bar, x_s), c(0, dnorm(x_s, sd = se)), col = rgb(0, 0, 1, 0.3), border = NA)
      
      text(s$x_bar + 1, 0.5*densmax, paste0("p-value area\n", round(s$p_val, 4)), col = "blue", font = 2)
    } else {
      x_s <- seq(-4, s$x_bar, length.out = 100)
      polygon(c(x_s, s$x_bar), c(dnorm(x_s, sd = se), 0), col = rgb(0, 0, 1, 0.3), border = NA)
      
      text(s$x_bar - 1, 0.5*densmax, paste0("p-value area\n", round(s$p_val, 4)), col = "blue", font = 2)
    }
    
    
    
    # 4. Highlight the Sample Mean
    abline(v = s$x_bar, col = "blue", lwd = 3)
    abline(v = 0, lty = 2) # Null
    
    # Legend and labels
    legend("topright", 
           legend = c("Null Distribution", "Critical Region (alpha)", "p-value area", "Observed Mean"),
           fill = c(NA, rgb(1, 0, 0, 0.1), rgb(0, 0, 1, 0.3), NA),
           border = c("black", NA, NA, NA),
           lty = c(1, NA, NA, 1),
           col = c("black", NA, NA, "blue"),
           lwd = c(1, NA, NA, 3),
           bty = "n")
  })
}

testci_logic <- function(input, output, session) {
    # --- Master data generation ---
    dual_master_data <- reactiveVal(rnorm(100))
    dual_master_jitter <- reactiveVal(jitter(rep(0, 100), amount = 0.2))
    
    observeEvent(input$resample_dual, ignoreNULL = FALSE, {
      dual_master_data(rnorm(100))
      dual_master_jitter(jitter(rep(0, 100), amount = 0.2))
    })
    
    # --- Calculations ---
    dual_stats <- reactive({
      n <- input$dual_n_obs
      alpha <- input$dual_alpha
      sample_data <- dual_master_data()[1:n] + input$dual_true_mu
      x_bar <- mean(sample_data)
      se <- 1 / sqrt(n)
      
      # Determine critical multiplier based on alternative
      if (input$dual_alt == "two.sided") {
        z_crit <- qnorm(1 - alpha/2)
      } else {
        z_crit <- qnorm(1 - alpha)
      }
      
      # CI boundaries
      ci_lower <- x_bar - (z_crit * se)
      ci_upper <- x_bar + (z_crit * se)
      
      list(x_bar = x_bar, se = se, ci_l = ci_lower, ci_u = ci_upper, z_crit = z_crit)
    })
    
    output$dual_plot <- renderPlot({
      s <- dual_stats()
      n <- input$dual_n_obs
      alpha <- input$dual_alpha
      se <- s$se
      
      # We define an arbitrary height for our zones since density is gone
      # Zone 3: Data points (Top)
      # Zone 2: Critical Region (Middle)
      # Zone 1: Confidence Interval (Bottom)
      
      par(mar = c(5, 4, 4, 2))
      plot(NULL, xlim = c(-3, 3), ylim = c(-1, 3), 
           xlab = expression(paste("Scale of Sample Mean (", bar(x), ")")), 
           ylab = "", yaxt = "n",
           main = paste0("Duality: Hypothesis Test vs. Confidence Interval (alpha = ", alpha, ")"))
      
      abline(v = 0, lty = 2, lwd = 2) # Null value marker
      text(0, 2.8, "Null Value (0)", pos = 3, font = 3, cex = 0.8)
      
      # --- 1. TOP ZONE: Data Points (y ~ 2.5) ---
      points(dual_master_data()[1:n] + input$dual_true_mu, 
             2.5 + dual_master_jitter()[1:n] * 0.3, 
             pch = 21, bg = "black", col = "white", cex = 1.3)
      text(-1.5, 2.5, "Sample\nData", font = 2, pos = 4)
      
      # --- 2. MIDDLE ZONE: Hypothesis Test Critical Regions (y from 1 to 2) ---
      # Draw a "Track" for the test
      rect(-4, 1, 4, 2, border = "gray80", lty = 1)
      
      if (input$dual_alt == "two.sided" || input$dual_alt == "less") {
        curr_alpha <- if(input$dual_alt == "two.sided") alpha/2 else alpha
        crit_val <- qnorm(curr_alpha, sd = se)
        rect(-4, 1, crit_val, 2, col = rgb(1, 0, 0, 0.15), border = "red", lty = 3)
        text(crit_val, 1.5, paste0("Crit. Value\n", round(crit_val, 3)), 
             col = "red", pos = 2, cex = 0.8, font = 2)
      }
      if (input$dual_alt == "two.sided" || input$dual_alt == "greater") {
        curr_alpha <- if(input$dual_alt == "two.sided") alpha/2 else alpha
        crit_val <- qnorm(1 - curr_alpha, sd = se)
        rect(crit_val, 1, 4, 2, col = rgb(1, 0, 0, 0.15), border = "red", lty = 3)
        text(crit_val, 1.5, paste0("Crit. Value\n", round(crit_val, 3)), 
             col = "red", pos = 4, cex = 0.8, font = 2)
      }
      
      text(-2.8, 1.5, "Rejection\nRegions", font = 2, pos = 4)
      
      # --- 3. BOTTOM ZONE: Confidence Interval (y ~ 0) ---
      ci_y <- 0
      # Logic for rendering one-sided intervals as rays
      l_bound <- if(input$dual_alt == "greater") -4 else s$ci_l
      u_bound <- if(input$dual_alt == "less") 4 else s$ci_u
      
      # Draw the CI line
      segments(x0 = l_bound, y0 = ci_y, x1 = u_bound, y1 = ci_y, lwd = 2, col = "steelblue")
      points(s$x_bar, ci_y, pch = 18, col = "steelblue", cex = 3) # Sample mean
      
      # Label CI
      text(s$x_bar, ci_y+0.2, paste0(round((1-alpha)*100), "% CI"), font = 2, pos = 4, col = "steelblue")
      
      # --- 4. THE CONNECTION ---
      # Draw a line showing the sample mean across all zones
      abline(v = s$x_bar, col = "darkblue", lwd = 2, lty = 4)
      text(s$x_bar+0.05, 2.8, expression(bar(x)), col = "darkblue", font = 2)
      
      # --- 5. Duality Decision Summary ---
      contained <- (l_bound <= 0 && u_bound >= 0)
      decision_color <- if(contained) "black" else "#e41a1c"
      
      # Summary box at the very bottom
      rect(-3, -0.9, 3, -0.4, col = "gray95", border = decision_color, lwd = 2)
      summary_msg <- if(contained) {
        "H₀ Not Rejected: 0 is inside the Confidence Interval."
      } else {
        "H₀ Rejected: 0 is outside the Confidence Interval."
      }
      text(0, -0.65, summary_msg, col = decision_color, font = 2, cex = 1.1)
      
    })
  }
  
twosample_logic <- function(input, output, session) {
  
  # --- Master data generation (Two Independent Streams) ---
  z2_master1 <- reactiveVal(rnorm(100))
  z2_master2 <- reactiveVal(rnorm(100))
  z2_jitter1 <- reactiveVal(jitter(rep(0, 100), amount = 0.15))
  z2_jitter2 <- reactiveVal(jitter(rep(0, 100), amount = 0.15))
  
  observeEvent(input$resample_2z, ignoreNULL = FALSE, {
    z2_master1(rnorm(100))
    z2_master2(rnorm(100))
    z2_jitter1(jitter(rep(0, 100), amount = 0.15))
    z2_jitter2(jitter(rep(0, 100), amount = 0.15))
  })
  
  # --- Calculations ---
  z2_stats <- reactive({
    n1 <- input$z2_n1
    n2 <- input$z2_n2
    
    # Extract and shift data
    data1 <- z2_master1()[1:n1] + input$z2_mu1
    data2 <- z2_master2()[1:n2] + input$z2_mu2
    
    x1_bar <- mean(data1)
    x2_bar <- mean(data2)
    diff_obs <- x1_bar - x2_bar
    
    # Standard Error of the difference (Sigma=1 known for both)
    se_diff <- sqrt((1^2/n1) + (1^2/n2))
    
    alpha <- input$z2_alpha
    if (input$z2_alt == "two.sided") {
      z_crit <- qnorm(1 - alpha/2)
    } else {
      z_crit <- qnorm(1 - alpha)
    }
    
    list(
      x1_bar = x1_bar, x2_bar = x2_bar, 
      diff = diff_obs, se = se_diff, 
      z_crit = z_crit, n1 = n1, n2 = n2
    )
  })
  
  output$z2_plot <- renderPlot({
    s <- z2_stats()
    alpha <- input$z2_alpha
    
    # Layout Setup: 
    # y=3: Group 1 Points | y=2: Group 2 Points | y=0.5: Difference Test
    par(mar = c(5, 4, 4, 2))
    plot(NULL, xlim = c(-3, 3), ylim = c(-1, 4), 
         xlab = "Value / Difference Scale", ylab = "", yaxt = "n",
         main = "Two-Sample Z-Test (Known Variance = 1)")
    
    # --- 1. TOP ZONE: Raw Data ---
    # Group 1 (Blue)
    points(z2_master1()[1:s$n1] + input$z2_mu1, 3.2 + z2_jitter1()[1:s$n1], 
           pch = 21, bg = "#007bff", col = "white", cex = 1.2)
    lines(c(s$x1_bar, s$x1_bar), c(1.8, 3.6), col = "#007bff", lwd = 2, lty = 2)
    
    # Group 2 (Green)
    points(z2_master2()[1:s$n2] + input$z2_mu2, 2.2 + z2_jitter2()[1:s$n2], 
           pch = 21, bg = "#28a745", col = "white", cex = 1.2)
    lines(c(s$x2_bar, s$x2_bar), c(1.8, 3.6), col = "#28a745", lwd = 2, lty = 2)
    
    text(-2.8, 3.2, "Group 1", col = "#007bff", font = 2, pos = 4)
    text(-2.8, 2.2, "Group 2", col = "#28a745", font = 2, pos = 4)
    
    # --- 2. MIDDLE ZONE: The Difference Scale (centered at 0) ---
    # Highlight the 0 line (Null Hypothesis: mu1 - mu2 = 0)
    abline(v = 0, lwd = 2, lty = 1)
    rect(-4, 0.2, 4, 1.2, border = "gray90")
    
    # Rejection Regions on the difference scale
    if (input$z2_alt == "two.sided" || input$z2_alt == "less") {
      curr_alpha <- if(input$z2_alt == "two.sided") alpha/2 else alpha
      crit <- qnorm(curr_alpha, sd = s$se)
      rect(-4, 0.2, crit, 1.2, col = rgb(1, 0, 0, 0.15), border = "red", lty = 3)
    }
    if (input$z2_alt == "two.sided" || input$z2_alt == "greater") {
      curr_alpha <- if(input$z2_alt == "two.sided") alpha/2 else alpha
      crit <- qnorm(1 - curr_alpha, sd = s$se)
      rect(crit, 0.2, 4, 1.2, col = rgb(1, 0, 0, 0.15), border = "red", lty = 3)
    }
    
    # Plot the observed difference
    points(s$diff, 0.7, pch = 18, col = "darkred", cex = 3)
    segments(x0 = s$x2_bar, y0 = 1.8, x1 = s$x1_bar, y1 = 1.8, lwd = 3, col = "darkred")
    text(s$diff, 1.4, paste("Observed Difference:", round(s$diff, 3)), col = "darkred", font = 2)
    # add arrow from text to point and center of segment
    
    arrows((s$x1_bar + s$x2_bar)/2, 1.7, s$diff, 0.8, col = "darkred", length = 0.1)
    
    text(-2.8, 0.7, "Difference\nScale (H0: 0)", font = 2, pos = 4)
    
    # --- 3. BOTTOM ZONE: Summary ---
    # Decision Logic
    is_significant <- if(input$z2_alt == "two.sided") {
      abs(s$diff) > (s$z_crit * s$se)
    } else if(input$z2_alt == "greater") {
      s$diff > (s$z_crit * s$se)
    } else {
      s$diff < (-s$z_crit * s$se)
    }
    
    box_col <- if(is_significant) "#e41a1c" else "black"
    rect(-2.5, -0.8, 2.5, -0.2, border = box_col, lwd = 2, col = "gray98")
    res_text <- if(is_significant) "Decision: Reject H₀ (Significant Difference)" else "Decision: Fail to Reject H₀"
    text(0, -0.5, res_text, col = box_col, font = 2, cex = 1.2)
    
    # Legend
    legend("topright", legend = c("Sample Mean 1", "Sample Mean 2", "Difference (1-2)"),
           col = c("#007bff", "#28a745", "darkred"), lty = c(2, 2, 1), lwd = 2, bty = "n")
  })
}

slr_est_logic <- function(input, output, session) {
    
    # --- Data generation ---
    # Generates 20 points where true coefficients are at least |0.5| away from zero
    generate_slr_data <- function() {
      x <- seq(-2, 2, length.out = 20)
      
      # Ensure a visible slope and intercept offset
      b0_true <- runif(1, 0.8, 2) * sample(c(-1, 1), 1)
      b1_true <- runif(1, 0.8, 2) * sample(c(-1, 1), 1)
      
      y <- b0_true + b1_true * x + rnorm(20, sd = 0.5)
      data.frame(x = x, y = y)
    }
    
    slr_data <- reactiveVal(generate_slr_data())
    
    observeEvent(input$resample_slr, {
      slr_data(generate_slr_data())
    })
    
    observeEvent(input$jump_to_min_sse, {
      dat <- slr_data()
      fit <- lm(y ~ x, data = dat)
      
      updateSliderInput(session, "beta0_guess", value = as.numeric(coef(fit)[1]))
      updateSliderInput(session, "beta1_guess", value = as.numeric(coef(fit)[2]))
    })
    
    # Helper for SSE calculation
    calc_sse <- function(dat, b0, b1) {
      sum((dat$y - (b0 + b1 * dat$x))^2)
    }
    
    # --- 1. Intuition Plot (Data & Residuals) ---
    
    output$slr_data_plot <- renderPlot({
      req(slr_data(), input$beta0_guess, input$beta1_guess) # Ensure values exist
      dat <- slr_data()
      b0 <- input$beta0_guess
      b1 <- input$beta1_guess

      dat$y_hat <- b0 + b1 * dat$x
      current_sse <- calc_sse(dat, b0, b1)
      plot(c(0,1), c(0,1))

      ggplot(dat, aes(x = x, y = y)) +
        geom_segment(
          aes(xend = x, yend = y_hat),
          color = "steelblue", linetype = "dashed", linewidth = 0.8
        ) +
        geom_point(size = 3) +
        geom_abline(intercept = b0, slope = b1, color = "firebrick", linewidth = 1.5) +
        # Use coord_cartesian but ensure it's wide enough for your random data
        coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-8, 8)) +
        annotate("label", x = -1.5, y = 7,
                 label = paste("SSE =", round(current_sse, 2)), fill = "white") +
        theme_minimal(base_size = 14)
  
    })
    
    # --- 2. SSE Surface ---
    
    output$sse_surface <- renderPlot({
      req(slr_data(), input$beta0_guess, input$beta1_guess)
      dat <- slr_data()
      
      grid_res <- 40 # Lower resolution slightly for faster rendering
      b0_seq <- seq(-5, 5, length.out = grid_res)
      b1_seq <- seq(-5, 5, length.out = grid_res)
      
      reg_coefs <- lm(y ~ x, data = dat)$coef
      
      df_grid <- expand.grid(b0 = b0_seq, b1 = b1_seq)
      
      # Ensure calculation is clean
      df_grid$sse <- mapply(function(b0_val, b1_val) {
        sum((dat$y - (b0_val + b1_val * dat$x))^2)
      }, df_grid$b0, df_grid$b1)
      
      ggplot(df_grid, aes(x = b1, y = b0)) +
        geom_raster(aes(fill = sse), interpolate = TRUE) +
        # add optimizer as point
        annotate("point", x = reg_coefs[2], y = reg_coefs[1], color = "blue", size = 5, shape = 18) +
        scale_fill_viridis_c(option = "magma", direction = -1) +
        # Provide explicit data to annotate to avoid length mismatches
        annotate("point", x = input$beta1_guess, y = input$beta0_guess, 
                 color = "red", size = 5, shape = 18) +
        # Add expand=c(0,0) to fill the card
        scale_x_continuous(expand = c(0, 0)) + 
        scale_y_continuous(expand = c(0, 0)) +
        labs(x = expression(beta[1]~(Slope)), y = expression(beta[0]~(Intercept))) +
        theme_minimal() +
        theme(legend.position = "none") # Heatmaps look cleaner without the legend
    })
    
    # --- 3. SSE Projection: Beta 0 ---
    
    output$sse_beta0 <- renderPlot({
      dat <- slr_data()
      b1_fixed <- input$beta1_guess
      b0_range <- seq(-5, 5, length.out = 200)
      
      df_line <- data.frame(
        b0 = b0_range, 
        sse = sapply(b0_range, function(b0) calc_sse(dat, b0, b1_fixed))
      )
      
      reg_coefs <- lm(y ~ x, data = dat)$coef
      reg_sse <- calc_sse(dat, reg_coefs[1], b1_fixed)
      
      current_sse <- calc_sse(dat, input$beta0_guess, b1_fixed)
      
      ggplot(df_line, aes(x = b0, y = sse)) +
        geom_line(color = "darkgreen", linewidth = 1) +
        # blue point at optimizer
        geom_point(data = data.frame(x = reg_coefs[1], y = reg_sse), 
                   aes(x = x, y = y), color = "blue", size = 4) +
        # Use a 1-row data frame to avoid the "length 1 vs 200" warning
        geom_point(data = data.frame(x = input$beta0_guess, y = current_sse), 
                   aes(x = x, y = y), color = "red", size = 4) +
        labs(x = expression(beta[0]), y = "SSE", subtitle = "Slice at current Slope") +
        theme_minimal()
    })
    
    # --- 4. SSE Projection: Beta 1 ---
    
    output$sse_beta1 <- renderPlot({
      dat <- slr_data()
      b0_fixed <- input$beta0_guess
      b1_range <- seq(-5, 5, length.out = 200)
      
      df_line <- data.frame(
        b1 = b1_range, 
        sse = sapply(b1_range, function(b1) calc_sse(dat, b0_fixed, b1))
      )
      
      reg_coefs <- lm(y ~ x, data = dat)$coef
      reg_sse <- calc_sse(dat, b0_fixed, reg_coefs[2])
      
      current_sse <- calc_sse(dat, b0_fixed, input$beta1_guess)
      
      ggplot(df_line, aes(x = b1, y = sse)) +
        geom_line(color = "purple", linewidth = 1) +
        # blue point at optimizer
        geom_point(data = data.frame(x = reg_coefs[2], y = reg_sse), 
                   aes(x = x, y = y), color = "blue", size = 4) +
        # Use a 1-row data frame to avoid the "length 1 vs 200" warning
        geom_point(data = data.frame(x = input$beta1_guess, y = current_sse), 
                   aes(x = x, y = y), color = "red", size = 4) +
        labs(x = expression(beta[1]), y = "SSE", subtitle = "Slice at current Intercept") +
        theme_minimal()
    })
  }

slr_dist_logic <- function(input, output, session) {
  
  # --- Simulation Engine ---
  # Generates 500 regressions based on a fixed "True" model
  sim_results <- reactiveVal({
    # Initial seed data
    data.frame(intercept = numeric(0), slope = numeric(0))
  })
  
  true_params <- reactiveVal(list(b0 = 1.5, b1 = -0.8))
  
  observeEvent(input$resample_dist, {
    # 1. Randomize the "True" underlying model
    b0_t <- runif(1, 0.8, 2) * sample(c(-1, 1), 1)
    b1_t <- runif(1, 0.8, 2) * sample(c(-1, 1), 1)
    true_params(list(b0 = b0_t, b1 = b1_t))
    
    # 2. Run 500 simulations
    x_fixed <- seq(-2, 2, length.out = 20)
    
    results <- replicate(500, {
      y <- b0_t + b1_t * x_fixed + rnorm(20, sd = 3)
      coef(lm(y ~ x_fixed))
    })
    
    sim_results(data.frame(
      intercept = results[1, ],
      slope = results[2, ]
    ))
  }, ignoreNULL = FALSE) # Runs once at startup
  
  # --- 1. Shadow Plot ---
  output$slr_shadow_plot <- renderPlot({
    req(sim_results(), true_params())
    
    # Filter based on slider
    show_df <- head(sim_results(), input$n_show)
    tp <- true_params()
    
    # compute sensible y-value for text
    ytext <- tp$b0 + 1.5 * tp$b1 # y-value at x=0 for the true line
    
    ggplot() +
      # The "Shadow" estimated lines
      geom_abline(data = show_df, aes(intercept = intercept, slope = slope), 
                  color = "steelblue", alpha = 0.2, linewidth = 0.5) +
      # The TRUE line (Fixed ground truth)
      geom_abline(intercept = tp$b0, slope = tp$b1, 
                  color = "firebrick", linewidth = 2) +
      # add vertical line at 0 with label "mean of predictors xbar"
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      annotate("text", x = 0, y = ytext, label = "Mean of Predictors (x̄)", 
               color = "gray50", fontface = "italic", vjust = -1.5) +
      coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-8, 8)) +
      labs(subtitle = paste("Red line = True Model | Blue lines =", input$n_show, "Estimated Models"),
           x = "x", y = "y") +
      theme_minimal(base_size = 14)
  })
  
  # --- 2. Intercept Histogram ---
  output$dist_beta0 <- renderPlot({
    req(sim_results())
    df <- head(sim_results(), input$n_show)
    tp <- true_params()
    
    ggplot(df, aes(x = intercept)) +
      geom_histogram(aes(y = after_stat(density)), bins = 20, 
                     fill = "purple", alpha = 0.4, color = "white") +
      stat_function(fun = dnorm, 
                    args = list(mean = mean(df$intercept), sd = sd(df$intercept)),
                    color = "purple", linewidth = 1) +
      geom_vline(xintercept = tp$b0, color = "firebrick", linewidth = 1, linetype = "dashed") +
      labs(x = expression(hat(beta)[0]), y = "Density") +
      theme_minimal()
  })
  
  # --- 3. Slope Histogram ---
  output$dist_beta1 <- renderPlot({
    req(sim_results())
    df <- head(sim_results(), input$n_show)
    tp <- true_params()
    
    ggplot(df, aes(x = slope)) +
      geom_histogram(aes(y = after_stat(density)), bins = 20, 
                     fill = "darkgreen", alpha = 0.4, color = "white") +
      stat_function(fun = dnorm, 
                    args = list(mean = mean(df$slope), sd = sd(df$slope)),
                    color = "darkgreen", linewidth = 1) +
      geom_vline(xintercept = tp$b1, color = "firebrick", linewidth = 1, linetype = "dashed") +
      labs(x = expression(hat(beta)[1]), y = "Density") +
      theme_minimal()
  })
}

slr_bands_logic <- function(input, output, session) {
  
  # --- Data generation ---
  # We store the base data AND the pre-calculated plot limits here
  data_store <- reactiveVal({
    set.seed(123)
    x_base <- runif(100, -2, 2)
    e_base <- rnorm(100, 0, 1)
    
    # random coefficients, between -2 and -0.5 or 0.5 and 2
    b0 <- runif(1, 0.5, 2) * sample(c(-1, 1), 1)
    b1 <- runif(1, 0.3, 0.7) * sample(c(-1, 1), 1)
    
    # Calculate limits assuming maximum slider values (x_spread=5, y_spread=5)
    # plus a little padding for the bands
    list(
      df = data.frame(x_base = x_base, e_base = e_base),
      lims = list(
        x = c(-2, 2) * 5.5, # max x_spread + padding
        y = c(-8, 10)       # Heuristic based on intercept + max slope*x + max noise
      ),
      b0 = b0, b1 = b1
    )
  })
  
  observeEvent(input$resample_slr, {
    x_base <- runif(100, -2, 2)
    e_base <- rnorm(100, 0, 1)
    
    # recalculate true coefs
    b0 <- runif(1, 0.5, 2) * sample(c(-1, 1), 1)
    b1 <- runif(1, 0.5, 2) * sample(c(-1, 1), 1)
    
    # Re-calculate limits based on the new random draw
    # We use the max slider values to ensure the view stays fixed while sliding
    max_x <- max(abs(x_base)) * 5 * 1.1
    max_y <- (b1 * max_x) + (max(abs(e_base)) * 5) + 2 
    
    data_store(list(
      df = data.frame(x_base = x_base, e_base = e_base),
      lims = list(x = c(-max_x, max_x), y = c(b0 - max_y, b0 + max_y)),
      b0 = b0, b1 = b1
    ))
  })
  
  # Processed data based on sliders
  processed_data <- reactive({
    ds <- data_store()
    df <- ds$df[1:input$slr_n, ]
    df$x <- df$x_base * input$slr_x_spread
    df$y <- ds$b0 + (ds$b1 * df$x) + (df$e_base * input$slr_y_spread)
    return(df)
  })
  
  output$slrPlot <- renderPlot({
    df <- processed_data()
    alpha <- 1 - input$slr_alpha
    lims <- data_store()$lims
    
    b0 <- data_store()$b0
    b1 <- data_store()$b1
    
    model <- lm(y ~ x, data = df)
    
    # Grid for bands covers the whole visible X range
    grid <- data.frame(x = seq(lims$x[1], lims$x[2], length.out = 250))
    
    conf_band <- predict(model, newdata = grid, interval = "confidence", level = 1 - alpha)
    pred_band <- predict(model, newdata = grid, interval = "prediction", level = 1 - alpha)
    
    df_bands <- cbind(grid, as.data.frame(conf_band))
    names(df_bands) <- c("x", "fit", "c_lwr", "c_upr")
    df_bands$p_lwr <- pred_band[, "lwr"]
    df_bands$p_upr <- pred_band[, "upr"]
    
    ggplot(df, aes(x = x, y = y)) +
      # Prediction Band (PI)
      geom_ribbon(data = df_bands, aes(y = fit, ymin = p_lwr, ymax = p_upr), 
                  fill = "steelblue", alpha = 0.15) +
      geom_line(data = df_bands, aes(x = x, y = p_lwr), color = "steelblue", linetype = "dotted") +
      geom_line(data = df_bands, aes(x = x, y = p_upr), color = "steelblue", linetype = "dotted") +
      # Confidence Band (CI)
      geom_ribbon(data = df_bands, aes(y = fit, ymin = c_lwr, ymax = c_upr), 
                  fill = "firebrick", alpha = 0.3) +
      geom_line(data = df_bands, aes(x = x, y = c_lwr), color = "firebrick", linetype = "dashed") +
      geom_line(data = df_bands, aes(x = x, y = c_upr), color = "firebrick", linetype = "dashed") +
      # Ground Truth
      geom_abline(intercept = b0, slope = b1, color = "black", linewidth = 1, alpha = 0.5) +
      # Estimated Line
      geom_line(data = df_bands, aes(x = x, y = fit), color = "firebrick", linewidth = 1.2) +
      # Data Points
      geom_point(shape = 21, fill = "white", size = 2.5, stroke = 1) +
      # --- FIXED LIMITS HERE ---
      coord_cartesian(xlim = lims$x, ylim = lims$y) +
      labs(
        title = "Inference in Linear Regression",
        subtitle = "Red = Confidence Band (Mean); Blue = Prediction Band (Individual observations)",
        caption = "Solid red line: Estimated regression line | Faded black line: Ground truth",
        x = "Predictor (X)",
        y = "Response (Y)"
      ) +
      theme_minimal(base_size = 16)
  })
}

slr_coverage_logic <- function(input, output, session) {
  
  # --- Central Data Store (The Universe) ---
  universe <- reactiveVal({
    set.seed(123)
    
    # Initial setup
    b0 <- runif(1, 0.5, 2) * sample(c(-1, 1), 1)
    b1 <- runif(1, 0.5, 2) * sample(c(-1, 1), 1)
    
    # Pre-generate a large buffer of noise (100 pts x 200 sims)
    x_buffer <- lapply(1:200, function(i) runif(100, -3, 3))
    e_buffer <- lapply(1:200, function(i) rnorm(100, 0, 1.5))
    new_obs_e <- rnorm(200, 0, 1.5) # Error for the 'future' point
    
    # Static limits based on the fixed spread
    # b0 + b1*x + 3*sigma
    list(b0 = b0, b1 = b1, xb = x_buffer, eb = e_buffer, ne = new_obs_e,
         lims = list(x = c(-3.5, 3.5), y = c(b0 - 8, b0 + 8)))
  })
  
  observeEvent(input$resample_cov, {
    b0 <- runif(1, 0.5, 2) * sample(c(-1, 1), 1)
    b1 <- runif(1, 0.5, 2) * sample(c(-1, 1), 1)
    universe(list(
      b0 = b0, b1 = b1, 
      xb = lapply(1:200, function(i) runif(100, -3, 3)),
      eb = lapply(1:200, function(i) rnorm(100, 0, 1.5)),
      ne = rnorm(200, 0, 1.5),
      lims = list(x = c(-3.5, 3.5), y = c(b0 - 8, b0 + 8))
    ))
  })
  
  # --- Compute All Interval Data ---
  sim_data <- reactive({
    uni <- universe()
    n <- input$cov_n
    level <- input$cov_alpha
    
    results <- lapply(1:input$cov_N_sims, function(i) {
      x <- uni$xb[[i]][1:n]
      y <- uni$b0 + uni$b1 * x + uni$eb[[i]][1:n]
      model <- lm(y ~ x)
      
      xval <- input$cov_xval
      
      # Targeted evaluation at X = 0
      target <- data.frame(x = xval)
      true_mean <- uni$b0 + uni$b1 * xval
      true_obs  <- uni$b0 + uni$b1 * xval + uni$ne[i]
      
      ci <- predict(model, target, interval = "confidence", level = level)
      pi <- predict(model, target, interval = "prediction", level = level)
      
      data.frame(
        id = i, true_mean = true_mean, true_obs = true_obs,
        ci_lwr = ci[1,2], ci_upr = ci[1,3],
        pi_lwr = pi[1,2], pi_upr = pi[1,3],
        ci_covers = (true_mean >= ci[1,2] & true_mean <= ci[1,3]),
        pi_covers = (true_obs >= pi[1,2] & true_obs <= pi[1,3])
      )
    })
    do.call(rbind, results)
  })
  
  
  # --- Rendering Plots ---
  
  # 1. CI LIVE
  output$ci_live_plot <- renderPlot({
    
    uni <- universe()
    res <- sim_data()[1, ]
    n <- input$cov_n
    
    # 1. Prepare data for the current simulation points
    df <- data.frame(
      x = uni$xb[[1]][1:n],
      y = uni$b0 + uni$b1 * uni$xb[[1]][1:n] + uni$eb[[1]][1:n]
    )
    
    # 2. Fit model and generate Prediction Bands for the entire X range
    mod <- lm(y ~ x, data = df)
    grid_x <- seq(uni$lims$x[1], uni$lims$x[2], length.out = 100)
    p_bands <- predict(mod, newdata = data.frame(x = grid_x), interval = "confidence", level = input$cov_alpha)
    
    df_p <- data.frame(
      x = grid_x,
      fit = p_bands[, "fit"],
      lwr = p_bands[, "lwr"],
      upr = p_bands[, "upr"]
    )
    
    
    ggplot(df, aes(x, y)) +
      geom_point(alpha = 0.4) +
      geom_ribbon(data = df_p, aes(x = x, ymin = lwr, ymax = upr), 
                  fill = "firebrick", alpha = 0.1, inherit.aes = FALSE) +
      geom_line(data = df_p, aes(x = x, y = fit), 
                color = "firebrick", linewidth = 1, inherit.aes = FALSE) +
      geom_point(alpha = 0.4) +
      geom_abline(intercept = uni$b0, slope = uni$b1, linetype = "dotted", linewidth = 1) +
      annotate("errorbar", x = input$cov_xval, ymin = res$ci_lwr, ymax = res$ci_upr, 
               color = "black", linewidth = 1.5, width = 0.2) +
      # Labeling the components
      annotate("text", x = uni$lims$x[2] + 0.2, y = uni$b0 + uni$b1 * uni$lims$x[2], 
               label = "True Mean Line", hjust = 1.1, vjust = -1, fontface = "italic") +
      annotate("label", x = input$cov_xval, y = res$ci_upr, 
               label = "Confidence Interval", vjust = -0.5, fill = "firebrick", color = "white", fontface = "bold") +
      coord_cartesian(xlim = uni$lims$x, ylim = uni$lims$y) +
      labs(subtitle = paste0("Estimating the Mean Line (Evaluating at X = ", input$cov_xval, ")"), 
           x = "X", y = "Y") + 
      theme_minimal()
  })
  
  # 2. CI HISTORY
  output$ci_hist_plot <- renderPlot({
    res <- sim_data(); uni <- universe()
    ggplot(res, aes(x = id, y = true_mean)) +
      geom_errorbar(aes(ymin = ci_lwr, ymax = ci_upr, color = ci_covers)) +
      geom_hline(yintercept = uni$b0 + uni$b1 * input$cov_xval, linetype = "dashed", alpha = 0.5, color="black", linewidth=1.5) +
      scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
      annotate("label", x = Inf, y = Inf, label = paste0("Coverage: ", round(mean(res$ci_covers)*100, 1), "%"), hjust = 1.1, vjust = 1.5) +
      labs(x = "Simulations", y = "Interval Range", color = "Covers?") + theme_minimal() + theme(legend.position = "none")
  })
  
  # 3. PI LIVE (Fixed geom_ribbon and aesthetics)
  output$pi_live_plot <- renderPlot({
    uni <- universe()
    res <- sim_data()[1, ]
    n <- input$cov_n
    
    # 1. Prepare data for the current simulation points
    df <- data.frame(
      x = uni$xb[[1]][1:n],
      y = uni$b0 + uni$b1 * uni$xb[[1]][1:n] + uni$eb[[1]][1:n]
    )
    
    # 2. Fit model and generate Prediction Bands for the entire X range
    mod <- lm(y ~ x, data = df)
    grid_x <- seq(uni$lims$x[1], uni$lims$x[2], length.out = 100)
    p_bands <- predict(mod, newdata = data.frame(x = grid_x), interval = "prediction", level = input$cov_alpha)
    
    df_p <- data.frame(
      x = grid_x,
      fit = p_bands[, "fit"],
      lwr = p_bands[, "lwr"],
      upr = p_bands[, "upr"]
    )
    
    ggplot(df, aes(x = x, y = y)) +
      geom_ribbon(data = df_p, aes(x = x, ymin = lwr, ymax = upr), 
                  fill = "steelblue", alpha = 0.1, inherit.aes = FALSE) +
      geom_line(data = df_p, aes(x = x, y = fit), 
                color = "steelblue", linewidth = 1, inherit.aes = FALSE) +
      geom_point(alpha = 0.4) +
      geom_abline(intercept = uni$b0, slope = uni$b1, linetype = "dotted", linewidth = 1) +
      annotate("errorbar", x = input$cov_xval, ymin = res$pi_lwr, ymax = res$pi_upr, 
               color = "steelblue", linewidth = 1.5, width = 0.4) +
      annotate("point", x = input$cov_xval, y = res$true_obs, color = "purple", size = 4) +
      # Labeling the components
      annotate("label", x = input$cov_xval, y = res$pi_upr, 
               label = "Prediction Interval", vjust = -0.5, fill = "steelblue", color = "white", fontface = "bold") +
      annotate("text", x = input$cov_xval, y = res$true_obs, 
               label = "New Observation", hjust = -0.2, color = "purple", fontface = "bold") +
      coord_cartesian(xlim = uni$lims$x, ylim = uni$lims$y) +
      labs(
        subtitle = "Predicting a New Individual (Purple Point)",
        x = "Predictor (X)", 
        y = "Response (Y)"
      ) + 
      theme_minimal()
  })
  
  # 4. PI HISTORY
  output$pi_hist_plot <- renderPlot({
    res <- sim_data()
    ggplot(res, aes(x = id, y = true_obs)) +
      geom_errorbar(aes(ymin = pi_lwr, ymax = pi_upr, color = pi_covers)) +
      geom_point(color = "purple", size = 4, alpha = 0.6) +
      scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
      annotate("label", x = Inf, y = Inf, label = paste0("Coverage: ", round(mean(res$pi_covers)*100, 1), "%"), hjust = 1.1, vjust = 1.5) +
      labs(x = "Simulations", y = "Interval Range", color = "Covers?") + theme_minimal() + theme(legend.position = "none")
  })
}

slr_r2_logic <- function(input, output, session) {
  # --- Data generation ---
  # Reduced n to 15 and randomized X for better scannability
  r2_base_data <- reactiveVal({
    set.seed(123)
    x <- sort(runif(15, -3, 3)) # Randomly spaced X
    z <- rnorm(15)              # Standard normal noise
    list(x = x, z = z)
  })
  
  observeEvent(input$resample_r2, {
    base <- r2_base_data()
    r2_base_data(list(x = base$x, z = rnorm(15)))
  })
  
  # Reactive data frame that responds to sliders
  r2_data <- reactive({
    base <- r2_base_data()
    b1 <- input$true_slope_r2
    sigma <- input$true_sd_r2
    
    y <- b1 * base$x + (sigma * base$z)
    df <- data.frame(x = base$x, y = y)
    
    # Calculate components
    fit <- lm(y ~ x, data = df)
    df$y_hat <- predict(fit)
    df$y_bar <- mean(df$y)
    df
  })
  
  # --- 1. Main Plot: Data & R-Squared ---
  output$r2_main_plot <- renderPlot({
    df <- r2_data()
    mod <- lm(y ~ x, data = df)
    r2_val <- summary(mod)$r.squared
    
    grid_x <- seq(min(df$x), max(df$x), length.out = 10)
    pred <- predict(mod, newdata = data.frame(x = grid_x), level = input$cov_alpha)
    
    df_p <- data.frame(
      x = grid_x,
      fit = pred
    )
    
    ggplot(df, aes(x, y)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_line(data = df_p, aes(x = x, y = fit), 
                color = "firebrick", linewidth = 1, inherit.aes = FALSE) +
      annotate("label", x = -2.5, y = 12, size = 6,
               label = paste("R-squared =", round(r2_val, 3)), fill = "white") +
      coord_cartesian(xlim = c(-3.5, 3.5), ylim = c(-15, 15)) +
      theme_minimal(base_size = 14)
  })
  
  # --- 2. The Energy Bar ---
  output$r2_energy_bar <- renderPlot({
    df <- r2_data()
    ssr <- sum((df$y_hat - df$y_bar)^2)
    sse <- sum((df$y - df$y_hat)^2)
    
    bar_df <- data.frame(
      Type = factor(c("Explained (SSR)", "Unexplained (SSE)"), 
                    levels = c("Unexplained (SSE)", "Explained (SSR)")),
      Value = c(ssr, sse)
    )
    
    ggplot(bar_df, aes(x = "Total Variation (SST)", y = Value, fill = Type)) +
      geom_col(width = 0.5, color = "white") +
      scale_fill_manual(values = c("Unexplained (SSE)" = "firebrick", 
                                   "Explained (SSR)" = "darkgreen")) +
      theme_minimal(base_size = 14) +
      labs(x = NULL, y = "Sum of Squares", fill = NULL) +
      theme(panel.grid.major.x = element_blank())
  })
  
  # --- 3. Improved Decomposition Plot ---
  output$r2_decomp_plot <- renderPlot({
    df <- r2_data()
    
    # Transform for faceting
    sst_df <- transform(df, type = "SST (Total)",      y_start = y_bar, y_end = y)
    ssr_df <- transform(df, type = "SSR (Regression)", y_start = y_bar, y_end = y_hat)
    sse_df <- transform(df, type = "SSE (Error)",      y_start = y_hat, y_end = y)
    
    df_long <- rbind(sst_df, ssr_df, sse_df)
    df_long$type <- factor(df_long$type, 
                           levels = c("SST (Total)", "SSR (Regression)", "SSE (Error)"))
    
    ggplot(df_long, aes(x = x, y = y)) +
      # Faded regression and mean lines for context in all panels
      geom_hline(aes(yintercept = y_bar), linetype = "dashed", alpha = 0.3) +
      geom_line(aes(y = y_hat), color = "black", alpha = 0.15) +
      
      # Deviation segments: Thicker and colored by type
      geom_segment(aes(xend = x, y = y_start, yend = y_end, color = type), 
                   linewidth = 1.2) +
      
      # Points are slightly larger but transparent to highlight the segments
      geom_point(size = 2, alpha = 0.5) +
      
      facet_wrap(~type) +
      scale_color_manual(values = c("SST (Total)" = "black", 
                                    "SSR (Regression)" = "darkgreen", 
                                    "SSE (Error)" = "firebrick")) +
      coord_cartesian(ylim = c(-15, 15)) +
      labs(subtitle = "Vertical distances represent the individual deviations that are squared and summed.") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(2, "lines") # Add space between facets
      )
  })
}

slr_violation_logic <- function(input, output, session) {
  # --- Base Data Generation ---
  base_noise <- reactiveVal(rnorm(50))
  base_x <- seq(-3, 3, length.out = 50)
  
  observeEvent(input$resample_assumptions, { base_noise(rnorm(50)) })
  
  # --- Data Synthesis Logic ---
  processed_data <- reactive({
    x <- base_x
    z <- base_noise()
    
    # Defaults
    nl <- 0; het <- 0; df_t <- 30; n_out <- 0; out_sev <- 0
    
    if (input$ui_mode == "simple") {
      if (input$scenario == "nonlinear") nl <- 1.5
      if (input$scenario == "hetero") het <- 2
      if (input$scenario == "tails") df_t <- 2
      if (input$scenario == "outliers") { n_out <- 2; out_sev <- 12 }
    } else {
      nl <- input$nl_strength; het <- input$het_strength
      df_t <- input$tail_df; n_out <- input$outlier_count; out_sev <- input$outlier_dist
    }
    
    # 1. Non-normal noise (t-distribution mapping)
    # We use the probability integral transform to keep the 'jitter' consistent
    noise <- qt(pnorm(z), df = df_t)
    
    # 2. Heteroscedasticity (noise scales with x)
    noise <- noise * (1 + het * (x + 3)^(3/2)/4)
    
    # 3. Non-linear Mean (adding quadratic term)
    y <- 1 + 1.5 * x + nl * (x^2) + noise
    
    # 4. Outliers (Modify specific points at the end of the vector)
    if (n_out > 0) {
      idx <- sample(1:50, n_out)
      y[idx] <- y[idx] + sample(c(-1, 1), n_out, replace = TRUE) * out_sev
    }
    
    data.frame(x = x, y = y)
  })
  
  # --- Main Plot ---
  output$assumption_main_plot <- renderPlot({
    df <- processed_data()
    
    mod <- lm(y ~ x, data = df)
    
    grid_x <- seq(min(df$x), max(df$x), length.out = 10)
    pred <- predict(mod, newdata = data.frame(x = grid_x), level = input$cov_alpha)
    
    df_p <- data.frame(
      x = grid_x,
      fit = pred
    )

    ggplot(df, aes(x, y)) +
      geom_point(alpha = 0.7) +
      geom_line(data = df_p, aes(x = x, y = fit), 
                color = "firebrick", linewidth = 1, inherit.aes = FALSE) +
      theme_minimal(base_size = 14) +
      labs(subtitle = "Linear model (OLS) fitted to the current data scenario.")
  })
  
  # --- Diagnostic Plots ---
  # Helper to render base R plots in individual ggplot-like cards
  render_diag <- function(which) {
    renderPlot({
      dat <- processed_data()
      model <- lm(y ~ x, data = dat)
      par(mar = c(4, 4, 2, 1))
      plot(model, which = which)
    })
  }
  
  output$diag_1 <- render_diag(1) # Residuals vs Fitted
  output$diag_2 <- render_diag(2) # Normal Q-Q
  output$diag_3 <- render_diag(3) # Scale-Location
  output$diag_4 <- render_diag(5) # residuals vs leverage
}