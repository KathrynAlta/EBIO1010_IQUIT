# This training script is a set of prompts- please fill in the code as you go, and be sure to make the annotations original prior to submitting your final work. The focus of this challenge is on 1) visualizing the residuals for two alternative hypotheses (the null and a biological hypothesis) and 2) using the residuals to calculate measures of the fit of the data to the predictions of the hypotheses using the sum of the squares. 

# First get the data (AgeMutations.csv includes information about age of fathers and the number of DNA mutations measured in offspring). 
mutations <- read.csv("data/AgeMutations.csv")
head(mutations)

# Make two objects (x and y) corresponding to the predictor and response variables, respectively.
# assign predictor and response variables 
x <- mutations$AgeOfFather
y <- mutations$numberOfNewMutations
  
# Plot the data and label the axes. 
# las= 1 to rotate the y axis numbers, pch = point type
plot(x, y, xlab="Father's age (years)", ylab="Number of mutations in offspring", las =1, pch = 19, col = rainbow(12))

# Now we want to put the predictions of the null hypothesis on the graph. We will use abline to do this. Consider: what is the slope of the null line? What is the y-intercept? Fill in the code below to draw a line for the null hypothesis on your graph. The argument h = places a horizontal line on a graph.
abline(h = mean(y))

# The residuals for the null are the observed values of y minus the predictions of the null for each value of x. To visualize the residuals, we will draw a vertical line from the predictions of the null to each point on the graph. Use the segments(x0,y0,x1,y1) function to do this. Replace x0, y0, x1, y1 below with the appropriate information. 
segments(x, mean(y), x, y)
  # draw a segment starting at (x = age of father, y = mean) and going to (x = age of the father, y = )
  # for segments function you need starting x and y and then ending x and y 

# Calculate the sum of squares: square all the residuals and add them together 
# To evaluate the "fit" of the data to the predictions of the null, we need to calculate the sum of the squared residual values. Put differently, we have to calculate the difference between each observed value of y and the predicted value of y, square this value, and sum all the squared values (sum of squares!). 

# First let's calculate the difference between each observed value of y and the predicted value of y for the null hypothesis and store this information in null_y_diff. This should be a vector that has the same number of values as y. Fill in the code below. 
# make an object for all residuals 
null_y_diff <- y-mean(y) # take all of our y vaules and substract the mean 
# explicitly talk about vectorization early in the class? 
  
# square all of the residuals 
# Now square the values in null_y_diff and store the values in sq_null_y_diff. 
sq_null_y_diff <- null_y_diff^2    # the ^2 means square the values

# Now calculate the sum of these values and store it in ss_null (short for sum of squares null). 
ss_null <- sum(sq_null_y_diff)
  
# add the ss value to the graph 
# Now let's write the ss_null in the open space of the graph. We are going to use the text() function. You need to fill in x and y. We will write SS null = and then write the SS value by using the paste() function (effectively sticks things together). We will also make the font size a little smaller than the default of 1.
text(20, 80, paste("SS null = ",round(ss_null)), cex=0.6)

# Congratulations! You just visualized and analyzed the fit of the data to the null hypothesis. 


# Now let's do it for the biological hypothesis. First we need to construct a linear model. Use the function lm(), and store the results in a new object called lm_model.
lm_model <- lm(y~x) # ~ "tilda" means "is predited by"
summary(lm_model)# p value for the slope is the same as the p-value for the overall model 
  
# First we need to plot the data again. 
plot(x, y, xlab="Father's age (years)", ylab="Number of mutations in offspring", las =1, pch = 19, col = rainbow(12))


# Use abline() with the model results object to draw the model line on the graph.
abline(lm_model, lwd = 2, col = "grey") # you can draw a line using the results of the model analysis 

# lm_model contains two vectors that are useful for us (you can use str(lm_model) to see all of the vectors in the model object). lm_model$fitted.values contains the predicted values of y (based on the model) given each value of x. We can use segments() to draw lines from the fitted.values to each of the y values for all values of x. Replace x0, y0, x1, y1 with the appropriate information. 
str(lm_model)
segments(x, y, x, lm_model$fitted.values) # start at each datapoint, then x stays the same moving vertically, 
# fittted values are all the predicted y values 
# draw line segments from (x, y) (the observed point) to ( x = fathers age, y = modeled number of mutations aka fitted values ) 

# Now, like we did for the null model, we need to generate a vector in which each residual (the lines from the predicted value to each point) is squared. The residuals are in the vector lm_model$residuals. Store these values in sq_bio_y_diff
sq_bio_y_diff <- lm_model$residuals^2 # we have all of the residuals stored in the lm_model
  
# Now calculate the sum of these values and store it in ss_bio
ss_bio <- sum(sq_bio_y_diff) # add the squared residuals together 
  
# Now use the text function to write the value of the SS like we did for the null 
# add the ss bio value to the graph as text 
text(20, 80, paste("SS bio = ",round(ss_bio)), cex=0.6)

# Congratulations, you just evaluated the fit of data to the predictions of the biological / alternative hypothesis! 

# The final step is to plot your two graphs (null and biological hypotheses) in the same space. First format the graphing space using the par() function (par refers to the parameters of graphing). The argument mfrow makes rows and columns for graphing. So if mfrow=c(2,1), the graphs will land in two rows and one column: one graph above the other (2 refers to rows and 1 to columns). We are going to use mfrow=c(1,2) so we have 1 row and 2 columns... so graphs are side-by-side.
par(mfrow=c(1,2)) # par is graphing parameters, mfrow is the number of rows and then the number of columns 
# here we are changing graph space to have one 

# all object names need to be orriginal 

# Now run the code again to visualize your two graphs next to each other. If you need more space, you can open a separate graph window using quartz() on a mac or windows() on a PC. An empty graphing window will pop open, and your graphs will be plotted there.

# See the rubric on Canvas so you know what to submit. 

# Compare ss for bio and null 
ss_bio
ss_null
# Calculate R2 and compare to the model summary output 
r2 <- 1 - (ss_bio / ss_null)
summary(lm_model)

# histograms for lecture slides
null <- c(-13.8, 22.2, -5.8, 6.2, -15.8, 6.2, -.8, -2.8, 2.2, 2.2)
bio <- c(-1.6, 8.1, -5.4, 2.9, 8.5, 1.5, -6.9, -7.1, 4.7, -4.6)

hist(bio, ylim = c(0,5), xlim = c(-30,30), xlab = "Residual value", main = "Biological hypothesis", breaks = 2, col = "purple", las = 1)
hist(null, ylim = c(0,5), xlim = c(-30,30), xlab = "Residual value", main = "Null hypothesis", breaks = 5, col = "purple", las = 1)



