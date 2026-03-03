# Evaluation of the differences in means when there are more than two categorical predictor variables: ANOVA

# Last week you learned how to use a t-test to analyze the difference in means between two groups. Analysis of variance (ANOVA) is a method of analysis for more than two groups. If you have not done so already please complete the reading about ANOVA (linked in the Canvas assignment) before continuing with this training script.

# We will be using a data set from a study that was interested in whether the population genetic diversity of eelgrass individuals causes a difference in productivity (number of grass shoots produced by each individual). Eelgrass forms meadows in shallow, warm, and protected marine environments, providing important habitat for many marine species. For this experiment, groups of eelgrass individuals were planted that contained 1, 3 or 6 different genotypes. After a period of time, the number of shoots per individual was measured. All confounding variables were controlled via experimental design.

# import the data
plant_div <- read.csv("EelgrassGenotypes.csv")

# Look at the top 6 rows, and the structure
head(plant_div)
str(plant_div)

# There are two variables: a treatment variable with three different categorical values (one, three and six - referring to genotype diversity) and a response variable: the number of shoots per plant.
x <- plant_div$treatmentGenotypes
y <- plant_div$shoots

# The function for an ANOVA is aov(). You need to specify the x and y variables, just like when you run a t-test.
aov_model_results <- aov(y ~ x)

# The summary of the results tells you the probability of significant differences in means between any of the groups (the p value - symbolized as Pr>F), as well as the F value (reminder: F>>1 indicates differences between groups).  
summary(aov_model_results)

# Results printed in the console tell you that there are differences in means between groups (very low probability of the differences being due to chance; p = 0.01%).

# Now we want to know what the differences in means are between each pair of groups. We can use the function TukeyHSD() to do this. (There are some details under the hood that you should learn if you stay in science and plan to use Tukey tests... we're not going there right now).
Tukey_res <- TukeyHSD(aov_model_results)
print(Tukey_res)

# The results in the console show the calculated differences between each pair (diff), the 95% confidence interval (lwr and upr), and the probability of the differences being due to sampling error / chance (p adj... a p-value).

# See if you can use the confidence intervals and p-values to make a claim about the effect of genotype diversity on productivity. Is there an effect of six genotypes compared to one? What about three compared to one? Three compared to six?

# The data are best summarized using a barplot. To generate the barplot we need to calculate the mean number of shoots for each group. We can use the tapply() function to do this.
eelgrass_means <- tapply(plant_div$shoots, # specify the column for which we are calculating the means
                         plant_div$treatmentGenotypes, # specify the column that identifies the groups
                         mean) # specify the function to apply to the data... we want to calculate the mean

# Look at the means you just calculated...
eelgrass_means

# Modify so they are in order of least to most genotypes: 
eelgrass_means_ordered <- eelgrass_means[c(1,3,2)]

# Make an eelgrass-colored barplot of the ordered means. 
barplot(eelgrass_means_ordered, names = c("One","Three","Six"), xlab="Number of genotypes", ylab="Mean number of shoots", las=1, col="darkolivegreen2", ylim=c(0,80)) 

# Now we need to visualize uncertainty on the barplot. We will do this using confidence intervals (mean +/- 2*SE). We need to do some calculations, in order to be able to calculate SE (standard error).  

# First calculate sd, again using tapply()
sds <- tapply(plant_div$shoots, # specify the column for which we are calculating the means
                                plant_div$treatmentGenotypes, # specify the column that identifies the groups
                                sd) # specify the function to apply to the data... we want to calculate the standard deviation

# Make a vector of sample sizes, n (= the number of samples per treatment)
ns <- tapply(plant_div$shoots, # specify the column for which we are calculating the means
                    plant_div$treatmentGenotypes, # specify the column that identifies the groups
                    length) # specify the function to apply to the data... we want to calculate the lengths (=n)

# Now we can calculate standard error for each group (SE = sd/sqrt(n))
ses <- sds/sqrt(ns)

# View the object you just made...
ses

# Change the order, so it matches the order of the bars in the graph
ses_ordered <- ses[c(1,3,2)]

# View the object to make sure your indexing was correct...
ses_ordered

# Next calculate the lower and upper 95% confidence intervals. Reminder: CI = mean +/- 2*SE.
lower_ci <- eelgrass_means_ordered - 2*ses_ordered
upper_ci <- eelgrass_means_ordered + 2*ses_ordered

# We will use segments() to draw the confidence intervals on the bars of the barplot. segments() needs starting and ending x and y coordinates for each segment. We know the y values (these are the upper and lower CI values). To place the lines in the correct x position, we need to know the x position of each bar on the plot (currently labeled with words... not x values). To get those x values, we can store the barplot() in an object. I'm calling it barx, because it holds the x values of each bar.
barx <- barplot(eelgrass_means_ordered, names = c("One","Three","Six"), xlab="Number of genotypes", 
                ylab="Mean number of shoots", las=1, col="darkolivegreen2", 
                ylim=c(0,80)) 

# print barx in the console, so you can see what you generated (the x locations of each bar).
print(barx)

# Now we can add the confidence intervals on the graph using segments()
segments(barx, lower_ci, barx, upper_ci)

# The last step is to label the groups so the viewer knows which, if any, are statistically different from each other. Look at the results of the Tukey's test again:
print(Tukey_res)

# The p values from the Tukey's test show which groups are statistically different from each other. A commonly used cutoff for statistical significance is p = 0.05; when p < 0.05, there is less than a 5% chance of the observed result being due to sampling error (random chance). 

# The Tukeys results point to two statistically distinguishable groups: One genotype is different from Six genotypes (p = 0.008), so we will label them as A and B on the graph. The eelgrass treatment with three genotypes was not statistically distinguishable from the other two treatments, so we will label this bar as AB. This shows that this group is not statistically different from group A (p = 0.3), and not statistically different from group B (p = 0.2).

# Add letters to the barplot to show statistical groupings.
text(barx, 75, c("A","AB","B"))

# What to submit: an image of your graph, a claim about the effect of genetic diversity on eelgrass productivity below the graph (use the evidence from the ANOVA and Tukeys results and the information in your graph to support your claim), and your originally annotated code below your claim. I recommend deleting all of the annotations above, then going through each line and describing the process in your own words. This will help you learn the code, and it will ensure that your annotations are original.

