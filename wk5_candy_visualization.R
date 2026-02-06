#################### 
# WK 5 THHURS 20260205 
# Candy Visualizations 

####################
# Abby In Class demp 

# Goal make three graphs: a strip plot, a boxplot, and a histrogram, using an estimated
# mean candy data from tuesday 

# import the data 
candy_means <- read.csv("candy_bag_means_TTH002.csv")

# use the head and strt functions to look at the data 
head(candy_means)
str(candy_means)

# save the mean weights as 
x <- candy_means$mean_weight_g
x

# make a strip plot 
nrow(candy_means)
y <- rep(1, 35)
y <- jitter(y)
plot(x,y, ylim=c(0.8,1.2), ylab=NA, yaxt="n", main="Strip plot")

# box plot 
boxplot(x, horizontal = T)

# histogram 
hist(x, main = NA, xlab = "mean candy weight", ylim = c(0, 10), xlim= c(0, 60))

# change paratmeters to 
# mar(mar = c(4, 4, 4, 3))
par(mfrow=c(3,1)) # rows, columns 
plot(x,y, ylim=c(0.8,1.2), ylab=NA, yaxt="n", main="Strip plot")
boxplot(x, horizontal = T)
hist(x, main = NA, xlab = "mean candy weight")
par(mfrow=c(1,1)) # reset graphing parameter space 

#############################
# Generating data 

# download the andy pop 
candy_pop <- read.csv("candypop_SP26.csv")
head(candy_pop)
nrow(candy_pop)

# randomly sample 5 peices from the population 
ran_samp_1 <- sample(candy_pop$weight, 5, replace = FALSE)
# ran_samp_1
mean(ran_samp_1)



#############################
# In class challenge 

# Calculate true mean 
mean(candy_pop$weight)

# plot human means and computer means 
human_means <- read.csv("candy_bag_means_TTH002.csv")
human_x <- human_means$mean_weight_g
human_y <- jitter(rep(1, 35))
human_mean <- mean(human_means$mean_weight_g)

computer_means <- read.csv("classdat_randmeans_TTH002.csv")
head(computer_means)
nrow(computer_means)
computer_x <- computer_means$rand_mean
computer_y <- jitter( rep(2, 33) )
computer_mean <- mean(computer_means$rand_mean)

plot(human_x, human_y, ylim= c(0, 3), xlim= c(0, 60), ylab=NA, yaxt="n", main="Strip plot")

plot((computer_x), computer_y, ylim= c(0, 3), xlim= c(0, 60), ylab=NA, yaxt="n", main="Strip plot", xlab = "candy weight")
points(human_x, human_y)
points(human_mean, 1, cex = 3, col = "darkgreen", pch = "*")
points(computer_mean, 2, cex = 3, col = "darkgreen", pch = "*")
axis(2, at = c(2, 1), labels = c("humans", "computers"), las = 1, cex.axis = 0.8)
# put on the left y axis, labels that you want, orient them vertically, set the size 

# use par(mar) to change the margin sizes 
# the default sizes are roughtly 4, 4, 4, 3 (start at the bottom and move counter clockwise )
?points()
