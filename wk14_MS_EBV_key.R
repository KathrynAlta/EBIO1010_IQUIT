#HW: Use what you learned in the training script as a guide to evaluate whether the probability of being diagnosed 
# with multiple sclerosis (MS) is conditional on infection by the Epstein-Barr virus (EBv) or not.


# test for a conditional relationship between EBV and MS null: the variables are indpenedent (MS is not influenced by EBV )
#Import the data
MSdat <- read.csv("data/EBV_MS.csv")
head(MSdat)

#Summarize the data in a table
# cover the character data into numeric data. We can use the table() function to make a table that shows the counts of different words in the columns 
MS_summary <- table(MSdat)
print(MS_summary)

#Make a barplot of the data in the table using the funciton barplot
# remember you can only use the first position to enter data, either concatinate within the function or add things into an object 
# default wil give you stacked barplot, you need the beside = T to make it paired (next to eachother)
# add legend to tell what we are looking at 
barplot(MS_summary, beside = T, legend = T)

#Flip it: 
# flip the data table so that the barplot has the predictor variable on the x axis 
# use the t or transpose funciton to do this 
flipped_data <- t(MS_summary)
print(flipped_data)

# graph it, also store coords in barx
# make a bar plot with the flipped data
barx <- barplot(flipped_data, beside = T, ylab="Number of Individuals Diagnosed with MS", 
        ylim=c(0,60), main = "Epstein-Barr virus and MS", las= 1, col = c("cornflowerblue", "goldenrod"), 
        xlab = "Epstein-Barr infection", legend = T, legend.text = c("No MS", "Yes MS"))

#Add a legend to the top right of the graph. Make sure the colors correspond accurately to the colors you put in your graph.
# legend("topright", legend = c("No MS","Yes MS"), pch=15, col = c("black", "lightgrey"))

# Chi Square Analysis 
#Now we use a chi squared test to see if the difference in proportions we observe can be explained by sampling error (chance). summary() does the same test, slightly different math...
chisq.test(flipped_data) # either flipped or original data table can go in and you will get same result 
summary(flipped_data) # summary function will also automatically give you a chi squared test 

# slightly different because chisq.test() uses continuity correction (-0.5 to account for small sample sizes... 
# esp. in 2x2 tables)


#The barx object shows the x values for each bar (where each bar is located along the x axis). 
# You can use this information to designate where to place your p value text.
print(barx)

#Let's put the text at x = 3.5 and y = 50:
# now we want to print the numbers on the graph 
# first give the x value and the y value for where you want them 
# \n will be on a new line 
text(3.5, 45, "Chisq = 16.352 \n p-value = 5.261e-05")

# claim: MS diagnosis seems to be influenced by Epstein-Barr virus infection - absence of infection greatly 
# decreases probability of MS diagnosis.

# probabiliy of these data being due to change is 
# chi square is your effect size and p is measure of uncertainty 