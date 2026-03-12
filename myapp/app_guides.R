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
Simulation of 100 independent confidence intervals for the mean (true mean = 0) based on normally distributed data.
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
"
)
