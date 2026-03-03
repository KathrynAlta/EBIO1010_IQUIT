# import the data
snakes <- read.csv("Snake_resistance.csv")

# Look at the top 6 rows, and the structure
head(snakes)
str(snakes)

# assign x and y (resistance is predicted by locality)
x <- snakes$locality
y <- snakes$resistance

# AOV
aov_model_results <- aov(y ~ x)

# summary  
summary(aov_model_results)

# Tukey's
Tukey_res <- TukeyHSD(aov_model_results)
print(Tukey_res)

# tapply() to get the means
snake_means <- tapply(snakes$resistance, # specify the column for which we are calculating the means
                         snakes$locality, # specify the column that identifies the groups
                         mean) # specify the function to apply to the data... we want to calculate the mean

# Look at the means you just calculated...
snake_means

# barplot it
barplot(snake_means, names = c("Benton", "Warrenton", "Willow Creek"), xlab="Locality", ylab="Mean resistance", las=1, col="#FD6467", ylim=c(0,.8)) 

# Now we need to visualize uncertainty on the barplot. We will do this using confidence intervals (mean +/- 2*SE). We need to do some calculations, in order to be able to calculate SE (standard error).  

# First calculate sd, again using tapply()
sds <- tapply(snakes$resistance, # specify the column for which we are calculating the means
                      snakes$locality, # specify the column that identifies the groups
                      sd) # specify the function to apply to the data... we want to calculate the standard deviation

# Make a vector of sample sizes, n (= the number of samples per treatment)
ns <- tapply(snakes$resistance, # specify the column for which we are calculating the means
                      snakes$locality, # specify the column that identifies the groups
                      length) # specify the function to apply to the data... we want to calculate the lengths (=n)

# Now we can calculate standard error for each group (SE = sd/sqrt(n))
ses <- sds/sqrt(ns)

# View the object you just made...
ses

# Next calculate the lower and upper 95% confidence intervals. Reminder: CI = mean +/- 2*SE.
lower_ci <- snake_means - 2*ses
upper_ci <- snake_means + 2*ses

# We will use segments() to draw the confidence intervals on the bars of the barplot. segments() needs starting and ending x and y coordinates for each segment. We know the y values (these are the upper and lower CI values). To place the lines in the correct x position, we need to know the x position of each bar on the plot (currently labeled with words... not x values). To get those x values, we can store the barplot() in an object. I'm calling it barx, because it holds the x values of each bar.
barx <- barplot(snake_means, names = c("Benton", "Warrenton", "Willow Creek"), xlab="Locality", ylab="Mean resistance", las=1, col="#FD6467", ylim=c(0,1)) 

# print barx in the console, so you can see what you generated (the x locations of each bar).
print(barx)

# Now we can add the confidence intervals on the graph using segments()
segments(barx, lower_ci, barx, upper_ci)

# The last step is to label the groups so the viewer knows which, if any, are statistically different from each other. Look at the results of the Tukey's test again:
print(Tukey_res)

# add letters to show statistical groups
text(barx, .95, c("A","B","B"))

