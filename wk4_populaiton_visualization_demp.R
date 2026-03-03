# Populaiton visualization demp 

# use the rnorm() function to generate a population of individuals trait values

inds <- rnorm(1000, 5, 3) # randomly pull 1000 values from a normal distribution with a mean of 5 and a sd of 3 

# store the calues in an object called x 
x <- inds


# make an object y that holds all of the y coordinates 
y <- jitter(rep(1, 1000))

# plot the data 
plot(x, y, ylim = c(.97, 1.3), cex = .3, pch = 19, col = rgb(.5, .7, .2), ylab = NA, yaxt = "n", xlab = "Individuals trait values") # cex is the size of the points, pch makes them filled in 



# add a vertical line to show the mean 
abline(v = mean(x))

# add std devation info to the graph 
mean_x <- mean(x)
sd_x <- sd(x)

#Line and brackets for one set sd off of the mean 
x0 <- mean_x - sd_x
y0 <- 1.07
x1 <- mean_x + sd_x
y1 <- 1.07
arrows(x0, y0, x1, y1, code = 3, angle = 90, length = 0.1) # code = 3 has the arrows on both sides, angle 90 to make flat 

x2 <- mean_x - sd_x*2 
y2 <- 1.07
x3 <- mean_x + sd_x*2 
y3 <- 1.07
arrows(x2, y2, x3, y3, code = 3, angle = 90, length = 0.1) # code = 3 has the arrows on both sides, angle 90 to make flat 

# Random samples 
rs1 <- sample(x, 10) # use the sample function to randomly sample from x and do it 10 times 
rsy1 <- rep(1.20, 10) # give it y points for where you are going to plot things 
points(rs1, rsy1, pch= 19, cex = 0.5) # use the points funciton to add them to your graph. The points function will only add points to your existing graph 
points(mean(rs1), 1.20, pch = "*", cex = 2, col = "blue") # plot the mean 

# Random samples again
rs2 <- sample(x, 10) # use the sample function to randomly sample from x and do it 10 times 
rsy2 <- rep(1.25, 10) # give it y points for where you are going to plot things 
points(rs2, rsy2, pch= 19, cex = 0.5) # use the points funciton to add them to your graph. The points function will only add points to your existing graph 
points(mean(rs2), 1.25, pch = "*", cex = 2, col = "green") # plot the mean 

# input the tuna data, calculate mean and sd then use those numbers in the same 
