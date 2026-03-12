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

