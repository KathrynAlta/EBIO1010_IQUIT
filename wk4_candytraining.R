# For this training script, we are going to explore three ways of visualizing the data for a single numerical variable.

# Typically, if you want to summarize data for a single numeric variable, there are 3 main ways of doing that: a "strip" plot, a box plot, and a histogram. There is an R function that makes strip plots, but it does not allow for much manipulation of the graphing space. So, we will use the plot() function to make a strip plot.

# Let's generate some numbers to practice with:
x <- rnorm(35,20,3)  # this generates 35 random numbers sampled from a bell-shaped ("normal") distribution of values, centered at 20, and with a spread (standard deviation) of 3

# To add our x values to a graph, we need corresponding y values for every x (every point requires both x and y coordinates). In this case, our graph will have points spread out in a line (a strip, hence "strip plot"): the x values vary, but the y axis values will be the same number so all points are arranged in a horizontal line.
y <- rep(1,35)  # the rep() repeats 1, 35 times
print(y)  # notice you have 35 numbers and each one is 1

# We are also going to wiggle the y values around a little to help us see each point on the graph. We can do this with the jitter() function.
y <- jitter(y)

# Now we will plot the data. We need to specify x and y, as well as the y axis limits. If we leave the y axis at the default setting, our graph looks like this (not a strip of points...):
plot(x, y)

# Let's make the y axis range from 0.8 to 1.2. We will also remove the y axis labels and tick marks because they are meaningless. ylab = NA removes the y axis label and yaxt = "n" removes the tick marks and numbers.
plot(x, y, ylim=c(0.8,1.2), ylab=NA, yaxt="n", main="Strip plot")

# Next we are going to make a boxplot. This one is very straightforward and we don't need to do much coding. The downside of a boxplot is that we can't see all the data points, only summaries of the data.
boxplot(x, horizontal = T, main="Box plot", xlab="x")

# Finally, we will make a histogram.
hist(x, main="Histogram")

# Now we will graph all three graphs in the same space so we can visually compare them. This requires changing the graphing parameters using par() to make three separate spaces in our graphing space, one for each graph (strip plot, boxplot, and histogram). The function par(mfrow=c(3,1)) makes the graphing space have 3 rows and 1 column, allowing us to visualize all three graphs in a vertical stack.
par(mfrow=c(3,1))

# Execute the code for the three graphs again:
# Strip plot
plot(x,y, ylim=c(0.8,1.2), ylab=NA, yaxt="n", main="Strip plot")

# Boxplot
boxplot(x, horizontal = T, main="Box plot", xlab="x")

# Histogram
hist(x, main="Histogram")

# Now change the graphing space back to how it was before, so that there is only one space for one graph (instead of three stacked spaces).
par(mfrow=c(1,1))

# Now it's your turn: visualize the estimated candy bag means collected in the first class session this week, using all three types of graph (strip plot, boxplot, histogram). The data will be available to download as a .csv file on Canvas. Once it's available on Canvas, download the CSV file and move it to your EBIO 1010 project folder. 

# Import the data using the read.csv() function. Refer to your notes from earlier in the semester if you need a reminder about how to do this, or reach out to one of the teaching team members for support. Refer to the training code above for reminders about how to make each type of graph.

# What to submit on Canvas: image of the three candy mean graphs in a single graph space, with original and thoroughly annotated code pasted below. Check the rubric for information about points associated with this assignment.


