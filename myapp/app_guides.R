# STATISTIK FUER INFORMATIKSTUDIEN
# Tool zur Visualisierung von statistischen Konzepten
# Autor: Julius Baumhakel
# Zum Starten der App: 
#          runApp("Pfad/zum/Ordner/der/App")
#   oder: "Run App" Button in RStudio (oben rechts im Script-Editor) 



# Mardown strings for the guides of each app page.

app_guides <- list(
  quantiles = "
**Guide:**
Data is generated from a normal distribution.
* **Sliders:** Adjust **p** to change computed quantile and **n** to see how sample size affects estimation accuracy.
* **First row of plots:**  Empirical quantile marked in red.
* **Second row of plots:** CDF with horizontal p-line used to determine theoretical quantile x_p and density with shaded area up to x_p.
* **Third row of plots:** ECDF with horizontal p-line used to determine empirical quantile and histogram showing data distribution.
* **Observation:** As the sample size increases, the empirical quantiles better approximate the theoretical ones.
",
  location = "
Data consists of a fixed set of observations from a normal distribution plus one adjustable outlier.
* **Sliders:** Move **Outlier** to change its value and **Alpha** to set the percentage of data removed for the trimmed mean.
* **Plot:** Dataset and comparisons of the Mean, Median, and Trimmed Mean. The diamond represents the outlier, and gray dots represent points excluded by the current alpha.
* **Observation:** Notice how the Mean is pulled strongly by the outlier, while the Median and Trimmed Mean remain stable (robust).
",
  spread = "
Data consists of a fixed set of observations plus one adjustable outlier.
* **Sliders:** Use **Step-by-Step** to build the measures and **Outlier** to change its extreme value.
* **Steps:**
    * **Step 1:** Start with raw observations and identify the center (Mean for S, Median for MAD/IQR).
    * **Step 2:** Calculate distances from the center
    * **Step 3:** Square distances for S, take absolute values for MAD, and compute quartiles for IQR.
    * **Step 4:** Finalize the spread measure.
* **Observation:** The Standard Deviation (S) is highly sensitive to outliers because it squares the distances, whereas MAD and IQR are much more robust.
",
  lln = "
Sample N observations from a standard normal distribution and compute the rolling mean (that is, the mean only of the first n observations for n = 1, 2, ..., N) to obtain paths.
* **Sliders:** Adjust **N** for the number of observations and **m** for the number of independent random paths.
* **Plot:** Shows the running average (cumulative mean) as more data points are collected.
* **Observation:** At small n, the paths are highly volatile. As n increases, all paths converge toward the true population mean of 0.
",
  ecdf_conv = "
Data is sampled from a Standard Normal Distribution.
* **Sliders:** Increase **n** to see the effect of a larger sample.
* **Left Plot:** Comparison of the step-like Empirical CDF (blue) against the smooth Theoretical CDF (red).
* **Right Plot:** Comparison of the Sample Histogram against the Theoretical Density curve.
* **Observations:** 
    * As n grows, the 'staircase' line from the ECDF in black aligns almost perfectly with the smooth red line (CDF), illustrating the Glivenko-Cantelli theorem.
    * The histogram bars also start to match the shape of the normal density curve as n increases.
",
  histogram = "
Data is generated from a normal distribution.
* **Sliders:** 
    * Adjust **n** for sample size and 
    * **Bins** to change the width of the intervals. 
    * Change the **X-Axis Limits** to slightly shift the bins or zoom in/out.
    * Toggle **Scale to density** to switch between frequency and density on the y-axis and also draw the normal density for comparison.
* **Plot:** An interactive histogram with an optional density overlay.
* **Observations:** 
    * With too few bins, you lose the shape of the distribution (oversmoothing). 
    * With too many bins, the histogram becomes 'noisy' and jagged (undersmoothing).
    * Adjusting the x-axis can lead to different bin alignments, which can affect the visual interpretation of the data distribution.
    * The density overlay helps to see how well the histogram approximates the underlying normal distribution, especially as n increases.
",
  boxplot = "
Data consists of 19 fixed points and 1 interactive point (#20).
* **Sliders:** Drag **Point #20** to move it vertically across the distribution.
* **Left side:** A standard boxplot showing the Median, IQR, and Whiskers.
* **Right side:** The raw data points. Point #20 is highlighted with a circle. Points within the IQR are blue, outliers are red.
* **Observation:** Watch how the whiskers act: They extend to the last point within 1.5*IQR from the box. As you move Point #20, it can switch from being an inlier (blue) to an outlier (red), and the whiskers will adjust accordingly.

Watch out: The boxplot uses a specific definition of quantiles that can differ from the one used to color the points, which may result in outliers not showing up correctly.
",
  skew = "
This module uses a **Standardized Gamma Distribution (Pearson Type III)** to isolate the effect of asymmetry while keeping the center and spread constant.

* **The Logic:**
    * The slider controls the **Skewness (gamma)**. 
    * To ensure you only see the effect of skew, the distribution is automatically shifted and scaled so the **Mean is always 0** and the **Variance is always 1**.
* **Visualizing Asymmetry:**
    * **Positive Skew (gamma > 0):** The 'tail' pulls to the right. Notice how the **Mode** (the peak) stays to the left of the **Mean** (red line).
    * **Normal Limit:** As Skewness approaches 0, the Gamma distribution begins to resemble the Normal distribution.
* **Observations:**
    * **Boxplot:** Watch the 'whiskers.' In a skewed distribution, one whisker becomes longer, and outliers cluster on one side.
    * **Q-Q Plot:** A 'curved' Q-Q plot is the classic signature of skewness. The data points will depart from the straight line at both ends in the same direction (forming a U-shape or inverted U-shape).
",
  kurt = "
This module uses a **Scaled Student’s t Distribution (Pearson Type VII)** to demonstrate tail-heaviness (Kurtosis) independently of skewness.

* **The Logic:**
    * The slider controls **Kurtosis (kappa)**. 
    * We use the Student's t because it is naturally symmetric (Skewness = 0) but allows us to move from a 'Normal' tail to 'Fat' tails by changing the degrees of freedom.
    * The data is scaled so the **Variance remains 1**, allowing you to see that Kurtosis is about the *distribution* of variance, not the amount of it.
* **Visualizing 'Fat Tails':**
    * **High Kurtosis (kappa > 3):** Compare to the normal distribution (grey lines): Observe how the peak becomes pointier and the 'shoulders' of the distribution deplete to push more probability mass into the extreme tails.
* **Observations:**
    * **Boxplot:** High kurtosis is the 'outlier generator.' You will see many more extreme values than you would in a Normal distribution.
    * **Q-Q Plot:** An 'S-shaped' deviation from the straight line indicates that the tails of your sample are heavier (or thinner) than the Normal distribution.
",
  mle_norm1 = "
Visualizes the Maximum Likelihood Estimation for a Normal Distribution by displaying the density corresponding to current parameters and the Log-Likelihood curve.
* **Sliders:** Manually adjust **mu** (mean) to fit the data. 
* **Top Plot:** The density curve overlaying the data. Vertical blue lines represent the likelihood of each individual point, which get combined by multiplication to form the value of the likelihood function.
* **Bottom Row:** The Likelihood and Log-Likelihood curves depending on **mu**. The red point indicates the current value of mu.
* **Observations:** 
    * The goal is to maximize the Likelihood by shifting the density curve in the top plot to better fit the data by adjusting mu and sigma (the blue lines should be maximally long).
    * Maximization of the Likelihood occurs occurs when the peak of the curves in the bottom plots are reached. 
",
  mle_norm2 = "
Visualizes the Maximum Likelihood Estimation for a Normal Distribution by displaying the density corresponding to current parameters and the Log-Likelihood curve.
* **Sliders:** Manually adjust **mu** (mean) and **sigma** (std. deviation) to fit the data. 
* **Top Plot:** The density curve overlaying the data. Vertical blue lines represent the likelihood of each individual point, which get combined by multiplication to form the value of the likelihood function.
* **Middle Row:** The Likelihood and Log-Likelihood curves depending on **mu**. The red point indicates the current value of mu.
* **Bottom Row:** The Likelihood and Log-Likelihood curves depending on **sigma**. The red point indicates the current value of sigma.
* **Observations:** 
    * The goal is to maximize the Likelihood by shifting the density curve in the top plot to better fit the data by adjusting mu and sigma (the blue lines should be maximally long).
    * Maximization of the Likelihood occurs occurs when the peak of the curves in the bottom plots are reached. 
    * Changing **mu** or **sigma** will change the Likelihood and Log-Likelihood curves for the other parameter, but the maximum will always be at the same point because maximization is independent. 
",
  mle_bern = "
Visualizes the Maximum Likelihood Estimation for a Bernoulli Distribution (Success/Failure) by displaying how the choice of 'p' affects the probability of the observed sample.
* **Sliders:** Adjust **Proposed p** to change your estimate of the probability of success (x=1).
* **Top Plot:** 
    * The black dots represent your sample (around 0 for failure, around 1 for success) 
    * The red step function represents the probability mass: the height at 0 is 1-p and the height at 1 is p.
    * Blue dashed lines represent the 'contribution' of each point to the total likelihood.The Likelihood is the product of line lengths, which should be as large as possible.
* **Bottom Row:** The Likelihood and Log-Likelihood curves. The red dot moves as you adjust the slider.
* **Observations:** The Maximum Likelihood Estimate (MLE) for p is simply the proportion of 1s in your sample.
    * When the red step function matches the 'ratio' of the stacked points, the blue lines are collectively as long as possible, and the likelihood is maximized.
",
  ci = "
Simulation of N independent confidence intervals for the mean (true mean = 0) based on normally distributed data.
* **Sliders:** 
    * Change the **Standard Deviation** of the underlying normal distribution
    * Change the number of points generated from the distribution with **Sample Size**.
    * Change the **Confidence Level** that the intervals fulfill.
    * Change how many samples to generate (each leads to one interval) with **Number of Intervals**.
* **Plots:**
    * Top Plot: Each horizontal line is one interval. Blue lines cover the true mean (0), while red lines do not. In the title, the percentage of intervals that successfully captured the true mean is displayed.
    * Bottom Row:
        * Left: The dataset corresponding to the first interval, with the sample mean and the interval limits marked.
        * Right: The density function of a standard normal distriution with the area corresponding to the confidence level shaded in gray and the critical values marked with vertical lines.
* **Observations:** 
    * Even with a 95% confidence level, roughly 5 out of 100 intervals will fail to capture the true mean by random chance - by design!
    * As the standard deviation increases, the intervals become wider. We are more unsure about our estimation
    * As the sample size increases, the intervals become narrower. We are more certain about our estimation.
    * Increasing the confidence level also widens the intervals, as we require more certainty that they will capture the true mean.
    * As we increase the number of intervals generated, the observed percentage of intervals that capture the true mean should get closer to the theoretical confidence level (e.g., 95%).
",
  ztest = "
Simulation of N independent samples from a normal distribution with a true mean that can be adjusted and sigma=1 known. For each sample, a z-test is performed to test the null hypothesis that the true mean is 0.
  * **Sliders:**
    * **True Mean:** Sets the center of the data. At 0, the null hypothesis is true.
    * **Sample Size:** Larger samples provide more evidence and higher precision.
    * **Alternative Hypothesis:** Choose between two-sided or directional (one-sided) tests.
    * **Significance Level (alpha):** Sets the threshold for rejecting the null hypothesis.
* **Plots:**
    * **Outcomes:** Dots in red regions indicate \"statistically significant\" results (rejections).
    * **Sample Distribution:** Shows raw data points and the mean and result for a single trial.
    * **Theoretical Values:** Displays the bell curve and the cut-offs for rejection.
* **Observations:**
    * **False Alarms:** At a true mean of 0, alpha percent of repetitions will still be red by pure chance.
    * **Power:** Increasing sample size or moving the true mean from 0 makes rejection more likely.
    * **Precision:** Larger samples shrink the critical regions, detecting smaller differences.",
  ttest = "
Simulation of N independent samples from a normal distribution with a true mean that can be adjusted and sigma unknown. For each sample, a t-test is performed to test the null hypothesis that the true mean is 0.  
* **Sliders:**
    * **True Mean:** Sets the center of the data. At 0, the null hypothesis is true.
    * **Sample Size:** Larger samples provide more evidence and higher precision.
    * **Alternative Hypothesis:** Choose between two-sided or directional (one-sided) tests.
    * **Significance Level (alpha):** Sets the threshold for rejecting the null hypothesis.
* **Plots:**
    * **Outcomes:** Dots in red regions indicate \"statistically significant\" results (rejections).
    * **Sample Distribution:** Shows raw data points and the mean and result for a single trial.
    * **Theoretical Values:** Displays the bell curve and the cut-offs for rejection.
* **Observations:**
    * **False Alarms:** At a true mean of 0, alpha percent of repetitions will still be red by pure chance.
    * **Power:** Increasing sample size or moving the true mean from 0 makes rejection more likely.
    * **Precision:** Larger samples shrink the critical regions, detecting smaller differences.",
  pval = "
Illustrates the concept of p-values as that significance level, at which the observed result would be just significant. 
* **Sliders:**
    * **True Mean:** Shifts the sample. If set to 0, the data follows the null hypothesis.
    * **Sample Size:** Adjusts how many points are in the sample, changing the curve's width.
    * **Significance Level (alpha):** Sets the red \"rejection\" threshold.
    * **Jump to p-value:** Snaps the alpha slider to match the current p-value.
* **Plot:**
    * **Bell Curve:** Represents the \"Null\" world. The **blue shaded area** is the p-value.
    * **Top Bars:** The red shaded regions represent the critical zones for a given alpha.
    * **Points:** Individual data observations are shown at the very top.
    * **Blue Line:** Marks the calculated sample mean.
* **Observations:**
    * **Definition:** The p-value is the blue area. It shows how likely it is to see a result this extreme if the true mean were 0.
    * **Significant Results:** If the blue line enters the red region, the p-value is smaller than alpha, and we reject the null hypothesis.
    * **The \"Jump\":** Using the jump button shows that the p-value is exactly the point where the result becomes \"significant.\"
    * **Sample Size:** Increasing n makes the test more exact if the true mean is not 0 (smaller deviation of the mean from 0 is required to reject)",
  testci = "
Illustrates the duality between hypothesis testing and confidence intervals for the mean. Landing in the critical region of the test is mathematically equivalent to finding that the null value (0) is outside your confidence interval.
* **Sliders:**
    * **True Mean:** Shifts the sample. At 0, the null hypothesis is true.
    * **Sample Size:** Controls evidence strength. Larger samples make rejection regions and intervals narrower.
    * **Alternative Hypothesis:** Sets the test type (two-sided or one-sided).
    * **Significance Level (alpha):** Sets the error budget for the test and the coverage for the interval.
* **Plot:**
    * **Sample Data (Top):** Shows individual points and the calculated sample mean.
    * **Rejection Regions (Middle):** The red zones based on the null value of 0. If the sample mean enters these zones, we reject the null.
    * **Confidence Interval (Bottom):** A blue bar representing the range of likely values for the mean.
* **Observations:**
    * **The Duality:** If the blue confidence interval does not touch the dashed line at 0, the sample mean will always be in the red rejection region. Rejecting the null hypothesis is mathematically identical to finding that the null value (0) is outside your confidence interval.
    * **Alpha vs. Confidence:** A 5% significance level (alpha = 0.05) corresponds exactly to a 95% (=1-alpha) confidence interval.
    * **One-Sided Tests:** Notice how choosing \"Greater\" or \"Less\" turns the confidence interval into a ray that extends infinitely in one direction.",
  twosample = "
Illustrates the concept of two-sample testing by simulating data for two groups and showing how the difference between their means relates to the rejection regions of a test.  
* **Sliders:**
    * **Group 1 & 2 Means:** Set the true center for each group. We are interested in their difference.
    * **Sample Sizes (n1, n2):** Control how many points are in each group. Larger samples increase the test's precision.
    * **Alternative Hypothesis:** Choose whether to test for any difference or a specific direction (e.g., Group 1 has larger mean).
    * **Significance Level (alpha):** Sets the threshold for declaring the difference \"statistically significant.\"
* **Plots:**
    * **Raw Data (Top):** Displays individual points for Group 1 (blue) and Group 2 (green). Dashed lines mark the average of each sample.
    * **Middle:** Point corresponds to difference between sample means (Group1-Group2). The red area is the rejection region. If the dark red point lands here, the groups are considered significantly different.
* **Observations:**
    * **The Gap:** Watch the red horizontal line between the groups. As that gap grows, the point on the difference scale moves further from zero.
    * **Precision:** If you decrease the sample size of even one group, the red rejection regions will expand. This shows how a small sample makes it harder to prove a difference exists.
    * **Directionality:** If you choose a one-sided test, the entire red region moves to one side. A difference in the opposite direction will never be \"significant\" in this mode.
    * **Decision Box:** The summary at the bottom confirms if the evidence is strong enough to reject the idea that the two groups are identical.",
  slr_est = "
Visualizes the estimation of regression coefficients by minimizing the Sum of Squared Errors (SSE) for a simple linear model.
* **Sliders:** Manually adjust **beta_0** (intercept) and **beta_1** (slope) to fit the regression line to the data points.
* **Top Plot:** Displays the 20 data points and the proposed regression line. Vertical dashed lines represent the residuals (errors) for each point.
* **Bottom Row:** A heatmap and two projection plots showing the SSE surface. The red point indicates your current manual guess, while the blue point marks the analytical OLS optimizer (the global minimum).
* **Observations:** 
    * The goal is to minimize the SSE by adjusting the sliders until the regression line passes through the center of the data cloud.
    * In the heatmap, this corresponds to moving the red point into the brightest 'valley' of the loss surface.
    * When the red point overlaps with the blue point, you have found the most efficient statistical fit for the data.
",
  slr_dist = "
Explores the sampling distribution of regression estimators by simulating multiple datasets from the same underlying 'True' model.
* **Slider:** Adjust **N** to control how many simulated regression lines are displayed simultaneously.
* **Top Plot:** The bold red line is the ground truth. The blue 'shadow' lines represent individual estimated regression lines from different random samples. This visualizes the uncertainty and variability of the regression line.
* **Bottom Row:** Histograms showing the distribution of the estimated intercepts (β₀) and slopes (β₁). The dashed red line marks the true parameter value. The colored curve is the (normal) density of the estimator.
* **Observations:** 
    * Notice how the estimated lines form a 'bow-tie' shape, being most stable near the center of the data and more variable at the edges.
    * As you increase N, the histograms of the estimators begin to follow a Normal distribution, centered around the true values.
",
slr_r2 = "
Explores R-Squared (the Coefficient of Determination) by visualizing how much of the total variation in the data is captured by the regression model.
* **Sliders:** Adjust the **True Slope** to change the strength of the dependence and **Error SD** to change the error level.
* **Top Row:** 
    * **Left:** Shows the regression fit.
    * **Right:** A 'stacked' visualization of the total variation (SST) in the data, decomposed into the portion explained by the regression (SSR) and the unexplained portion (SSE). The proportion of SSR corresponding to the total is the R-squared value.
* **Bottom Row:** A side-by-side decomposition of the variation:
    * **SST (Total):** The total distance of points from the sample mean.
    * **SSR (Regression):** The portion of the distance 'accounted for' by the slope of the line.
    * **SSE (Error):** The remaining 'leftover' distance between the points and the line.
* **Observations:** 
    * R-squared is the ratio of SSR to SST. It represents the percentage of total variation explained by the model.
    * When the Error SD is zero, SSR equals SST, and R-squared is exactly 1.
    * When there is no dependence (slope=0), SSR is zero, and R-squared is 0.
",
slr_violation = "
Investigates how violations of linear regression assumptions manifest in data and diagnostic plots.
* **Interface Mode:** Switch between **Simple** (curated scenarios) and **Complex** (manual control over violation intensity).
* **Violations Included:**
    * **Non-linear Mean:** Breaks the assumption that the relationship is a straight line.
    * **Heteroscedasticity:** Breaks the assumption of constant error variance (look for the 'fan' shape).
    * **Heavy Tails:** Introduces non-normal noise (look for deviations at the ends of the Q-Q plot).
    * **Outliers:** Introduces points with high leverage or large residuals (check residuals vs. leverage).
* **Diagnostic Plots:**
    * **Residuals vs Fitted:** Should show no pattern. Curves suggest non-linearity; fans suggest heteroscedasticity.
    * **Normal Q-Q:** Points should lie on the dashed line. Curvature here suggests non-normal residuals.
    * **Residuals vs. Leverage:** Identifies specific observations that disproportionately influence the model coefficients.
",
slr_bands = "
Visualizes the influence of the data and sample size on Confidence and Prediction Intervals.
* **Sliders:** 
    * **Sample Size (n):** Notice how the Confidence Band (red) shrinks (in parts) almost to nothing as n increases, while the Prediction Band (blue) remains wider as it accounts for irreducible noise.
    * **Spreads:** Increasing the X-spread improves the precision of the slope estimate (narrowing the 'waist' of the band). Increasing Error Noise (sigma) expands both bands.
* **The Lines:**
    * **Solid Red:** The estimated regression line: Y_hat = beta0_hat + beta1_hat * X.
    * **Faded Black:** The 'Ground Truth' (True Model) from which the data was simulated.
* **The Bands:**
    * **Red Ribbon (Confidence):** Contains the true regression line with (1-alpha)% probability across repeated samples. It reflects uncertainty in our parameter estimates (the mean).
    * **Blue Ribbon (Prediction):** Contains a new individual observation with (1-alpha)% probability. It is always wider because it includes both parameter uncertainty and the residual variance (sigma-squared).
",
slr_coverage = "
This module visualizes the Frequentist definition of 'Confidence'. A 95% interval does not mean there is a 95% probability that the truth is in *this* specific interval. It means that 95% of such intervals generated by this procedure will contain the truth in the long run.
* **Top Row: Confidence Interval (CI)**
    * We are trying to capture the **True Mean** (the point on the dotted line) at the chosen X-value.
    * In the history plot, a red bar indicates a 'failed' interval that does not overlap the true mean.
* **Bottom Row: Prediction Interval (PI)**
    * We are trying to capture a **New Individual Observation** (the purple point) at the chosen X-value.
    * The history plot shows whether each simulation's interval successfully 'trapped' the unique random realization of that simulation's new data point.
* **The Frequentist Takeaway:**
    * **Coverage Probability:** Notice the 'Coverage' percentage shown in the history plots. As you increase the Number of Repetitions, this percentage will converge toward your chosen Confidence Level (1-alpha).
    * **Randomness of the Interval:** In this framework, the 'Truth' (the mean or the point) is fixed or realized, and it is the *interval itself* that is the random variable. It hits the target 95% of the time.
    * **Alpha as Error Rate:** If you set alpha to 0.05, you are explicitly accepting that, on average, 1 out of every 20 experiments you conduct will produce an interval that does not contain the truth.
"
)
