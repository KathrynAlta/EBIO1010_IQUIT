# To learn how to use the test, we will be comparing the proportion of students in the class who voted in this year's election to the national estimate of eligible voters aged 18-29 who voted.

# According to TuftsNow, "almost 50 million young people, defined here as ages 18 to 29, were eligible to vote in the 2024 election; according to the exit poll data, about 42% of them did" (https://now.tufts.edu/2024/11/12/young-voters-shifted-toward-trump-still-favored-harris-overall).

# The syntax of the prop.test() is as follows: prop.test(successes, n, prop, "greater"). "successes" is the count of successes (in this case, the number of EBIO 1010 students who DID vote this year... success!), n is the total number of individuals (in this case, the number of students who voted + those who did not), prop is the proportion we are testing against (in this case, the national estimate = 0.42), and the "greater" is specifying that we are asking whether the class proportion is statistically greater than the national proportion (if successes/n is less than prop, we would use "less" as the final argument). See below for an example:
# proptest_results <- prop.test(successes, n, prop, "greater")

# Your job is to import the class data, generate the objects needed to run the prop.test(), make a graph that shows the parameter of interest, then make a claim. In your claim, state what the null hypothesis is and whether we can reject it, citing specific evidence from your analysis.

# the data
clasdat <- read.csv("1010_SP25_survey.csv")
str(clasdat)

# store n
n <- length(clasdat$vote2024)

# store national estimate
natprop <- 0.47

# store voters
yesVoted <- sum(clasdat$vote2024=="Yes")

# prop.test
proptestresults <- prop.test(yesVoted, n, natprop, "greater")
print(proptestresults)
summary(proptestresults)

############## visualize with a barplot ###################

# store the proportion of yes voters
clasprop <- proptestresults$estimate

# barplot it  
barx <- barplot(c(clasprop, natprop), col = "#ABDDDE", ylab = "Proportion voted in 2024", names = c("EBIO 1010", "National \n (ages 18-29)"), ylim = c(0,1), las = 1)

text(1.9, .8, "X^2 = 18.648 \n p = 7.861e-06")

################ a claim ############
# There is a miniscule probability of the EBIO 1010 voter proportion being greater than the national proportion due to sampling error (chance); we can reject the null hypothesis of no difference between the two proportions.