# Training script: t-test and visualizing effect size

# In this training script we will use a function that implements a t-test. A t-test is used to compare the observed difference in means to the uncertainty expected from sampling error. The training script involves comparing the means for two groups: in this case, the difference in means for the amount mercury per gram in two species of tuna. The two species are yellowtail and bigeye (Thunnus albacares and Thunnus obesus, respectively).

# get the data and store it in an object
tuna <- read.csv("tuna_mercury.csv")

#look at the structure
str(tuna)

# Note that the data comprise 5 variables: the first vector is a sample id (a label researchers use to identify each sample). The second vector is an accession number to Genbank that references a DNA sequence used to positively ID the species. The third vector is the species name. The fourth vector is the sushi name that was used in the restaurant or supermarket where they purchased the fish for analysis. The last vector is the amount of mercury (µg/g or ppm).

# The t-test function is simple: you define x (predictor) and y (response) vectors and run the test. In this case we are asking the question: is there a difference in the amount of mercury between two different species of tuna? So species is our x (predictor) variable and mercury amount is our y (response) variable. The null hypothesis is that there is not a difference (mean difference = 0) but of course we know that we are likely to get a difference in means due to sampling error. The t-test allows us to examine whether the difference in means between the two groups is greater than expected from sampling error. 

# Let's define our predictor (x) and response (y) variables
x <- tuna$species
y <- tuna$mercury_ppm

# Run the t-test and store the values in an object
tuna_t_results <- t.test(y ~ x)

# We can see the results by executing the results object by itself
tuna_t_results

# We can also look at the structure of the results object to see all of the vectors it contains (each subset of the results is in its own vector)
str(tuna_t_results)

# Now we want to visualize the estimated effect and the associated uncertainty. The estimated effect is the difference in means. The means for the two groups are part of the t-test results, in a vector called $estimate. We can print that particular subset of the t-test results like this: 
tuna_t_results$estimate

# If we just want the value for albacares, we can use indexing to select the first value in the estimate vector.
alba_mean <- tuna_t_results$estimate[1]

# Similarly, if we want the second value (for obesus)...
obes_mean <- tuna_t_results$estimate[2]

# The parameter we are trying to estimate is the difference in means:
dif_means_alb_obe <- alba_mean - obes_mean

# So our estimated difference in means is...
dif_means_alb_obe

# Note the value is negative because we are subtracting obesus (larger) from albacares (smaller)...this is arbitrary and we are effectively saying to ourselves "what's the mercury level in albacares in relation to obesus?" In this case, albacares has less mercury than obesus.

# Now we want to visualize the effect of species on mercury content. The effect is the difference in means. When we make the plot, we need to give ourselves space to draw the error bars (uncertainty, or confidence interval) around the estimated effect. First let's look at the estimated uncertainty.
tuna_t_results$conf.int

# Note that the values go from -0.6 to -0.24. We need to make the area in the graph large enough to show both ends of the confidence interval.

# When we draw our visualization, we want it to include the expectation of the null (difference in means = 0). The xlim should be large enough to include the lowest and highest confidence limits, as a horizontal line going through a single point (the calculated difference in means). We first make an empty graphing space.
plot(NA, NA, xlim=c(-0.7, 0.7), ylim=c(0,1), 
     xlab="Difference in means (ppm mercury)", 
     ylab=NA, yaxt="n", 
     main="Effect of species on ppm mercury", 
     cex.main = 0.8)

# As you can see you get a rectangle. Let's draw in the expectation of the null.
abline(v = 0, lty = 3, lwd = 2)

# Now lets draw on the graph the confidence limits. We will use the segments function. Segments draws a line from starting and ending coordinates. It will be a horizontal line so the y value is the same for the end points. Our range of y values in our graph space goes from 0 to 1 so we will use 0.5. We will use the lower and upper limits from the confidence interval for our x values.
x0 <- tuna_t_results$conf.int[1]
y0 <- 0.5
x1 <- tuna_t_results$conf.int[2]
y1 <- 0.5
segments(x0, y0, x1, y1)

# We can put the estimated effect (difference in means) on the graph using the points function.
points(dif_means_alb_obe, 0.5, pch=19)

# Finally, to make things clear for anyone looking at our graph, we want to label the two spaces on either side of the null hypothesis line. One is albacares > obesus and the other is obesus > albacares. We can use the cex argument to scale the font size; 0.7 makes it a bit smaller than the default (default is cex = 1).
text(-0.4,0.75,"obesus > albacares", cex=0.7) 
text(0.4,0.75,"albacares > obesus", cex=0.7) 

# Now that you know how to use a t-test and visualize the results, your job is to repeat the analysis and visualization process for a different data set. You need to assess whether elephant seals that forage in different geographic locations ("Northern" vs. "Southern") have different amounts of mercury in their blood (BloodHg_ug_g_ww). The data are stored in the file called Elephant_seals_data.csv.

# What to submit: visualization showing the difference in mean blood mercury content for seals that forage in the two different locations, and all of your original code used to produce the graph, with original annotations.

