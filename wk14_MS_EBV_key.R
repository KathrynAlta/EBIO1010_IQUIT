#HW: Use what you learned in the training script as a guide to evaluate whether the probability of being diagnosed 
# with multiple sclerosis (MS) is conditional on infection by the Epstein-Barr virus (EBv) or not.

#Import the data
MSdat <- read.csv("data/EBV_MS.csv")
head(MSdat)

#Summarize the data in a table
MS_summary <- table(MSdat)
print(MS_summary)

#Make a barplot of the data
barplot(MS_summary, beside = T)

#Flip it
flipped_data <- t(MS_summary)
print(flipped_data)

# graph it, also store coords in barx
barx <- barplot(flipped_data, beside = T, ylab="Number of Individuals Diagnosed with MS", 
        ylim=c(0,60), main = "Epstein-Barr virus and MS", las= 1, col = c("black", "lightgrey"), 
        xlab = "Epstein-Barr infection")

#Add a legend to the top right of the graph. Make sure the colors correspond accurately to the colors you put in your graph.
legend("topright", legend = c("No MS","Yes MS"), pch=15, col = c("black", "lightgrey"))

#Now we use a chi squared test to see if the difference in proportions we observe can be explained by sampling error (chance). summary() does the same test, slightly different math...
chisq.test(flipped_data)
summary(flipped_data)

# slightly different because chisq.test() uses continuity correction (-0.5 to account for small sample sizes... 
# esp. in 2x2 tables)


#The barx object shows the x values for each bar (where each bar is located along the x axis). 
# You can use this information to designate where to place your p value text.
print(barx)
#Let's put the text at x = 3.5 and y = 50:
text(3.5, 50, "Chisq = 16.352 \n p-value = 5.261e-05")

# claim: MS diagnosis seems to be influenced by Epstein-Barr virus infection - absence of infection greatly 
# decreases probability of MS diagnosis.
