# Using the chi-square test

# In class we generated the expected number of times an individual visited one of five friends by choosing their path from their home to their friends' homes using a random coin flip. The same process can be used to investigate the sex composition for a family with four children. In this scenario, we are using identity of chromosomes to define biological sex. Instead of heads or tails as a defining property that determines whether an individual visits a particular friend, we will make a coin with two sides: one with a Y and one with an X, corresponding to the Y and X chromosomes, respectively. 

# First we need to generate a coin. If the coin flip results in a Y, the couple's offspring is labeled male. If the coin flip results in an X, the couple's offspring is labeled female. 
coin <- c("Y","X")

# We can flip the coin once by using the sample function.
flip <- sample(coin, 1)

#we have to flip the coin four times to figure out how many offspring are labeled female.
four_flips <- c(sample(coin, 1), sample(coin, 1), sample(coin, 1), sample(coin, 1))

# alternative method:
four_flips <- sample(coin, 4, replace = T)

#For the four_flips, we can determine the number of females by summing the number of times X appears in the vector four_flips.
sum_heads <- sum(four_flips=="X")

#To replicate what we did in class with the friend visits (i.e., generate a distribution by having all students flip coins and record their data), we need to repeat this process n_students times and then generate a vector of the number of Xs obtained for each of the simulated students. We have 39 students in our section.
n_students <- 39

#make a vector to store the results of the coin flips (i.e. the number of Xs)
sum_X_all <- rep(NA, n_students)

#repeat the coin flipping four times for each student, sum the number of Xs, and store this information
for (i in 1:n_students){
  four_flips <- c(sample(coin, 1), sample(coin, 1), sample(coin, 1), sample(coin, 1))
  sum_X_all[i] <- sum(four_flips=="X")
}

#Let's look at a histogram of the data.
hist_summary <- hist(sum_X_all, breaks=seq(-0.5,4.5,1), xlab = "Number of Xs out of 4 flips")

#now let's figure out the expected number of outcomes we should see assuming the probability of passing on an X or Y chromosome to the offspring is equal. 
exp_females <- c(0.0625*n_students, 0.25*n_students, 0.375*n_students, 0.25*n_students, 0.0625*n_students)

#make a barplot of the simulated and expected data by first making a matrix with the two vectors using rbind()
obs_exp_combined <- rbind(hist_summary$counts, exp_females)

#then making a barplot. The "\n" notation in the names argument adds a line break, so "females" is below the number.
barplot(obs_exp_combined, beside=T, names=c("0\nfemales","1\nfemale", "2\nfemales","3\nfemales", "4\nfemales"), xlab="# of females in a family with four offspring", ylab="Number of families", las=1, col=c("black","light grey"))


#put a legend on the graph
legend("topright", fill=c("black","light grey"), legend=c("observed","expected"))

#use the function chisq.test to evaluate whether the observed data differs from the expectations. To use this function, we need to put the observed and expected data into a table with observation in one column, and expected values in another column. We use cbind() and as.table() to do this.
obs_vs_exp <- as.table(cbind(hist_summary$counts, exp_females))
print(obs_vs_exp)

#add names to the table
dimnames(obs_vs_exp) <- list(ratio = c("All male","3 male, 1 female","2 males and 2 females","3 female, 1 male","All female"), status = c("Observed","Expected"))
print(obs_vs_exp)

#now to the chisq.test
chisq.test(obs_vs_exp)

# Use the evidence in your graph, and the results of the chi-square test to make a claim about differences between the observed and expected values. What to submit: image of your graph, claim, and code with thorough and original annotations (this means you need to go back through the code and write your own annotations, to demonstrate that you understand what is happening at each step). 
