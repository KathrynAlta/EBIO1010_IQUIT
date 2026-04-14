# Exploration of the visualization and analysis of conditional probability data: car crashes and brain parasites. Review the reading (linked in the Canvas assignment) as needed for background information on the topic of toxoplasma infection and car crashes.

# get the data
toxo <- read.csv("Toxoplasma_data.csv")
head(toxo)

# summarize the data
toxo_summary <- table(toxo)
print(toxo_summary)


# make a barplot with two sets of bars
barplot(toxo_summary, beside = TRUE, legend = TRUE)

# the barplot you just made visualizes number of infected and uninfected individuals, grouped by car crash status.

# let's flip the matrix to group by infection status. The t() function transposes the data,
toxo_flipped <- t(toxo_summary)

# visualize the rearranged data:
barplot(toxo_flipped, beside = TRUE, legend = TRUE, legend.text = c("No car crash", "Car crash"), cex.names = 1, ylab = "Number of individuals", ylim = c(0, 220), xlab = "Toxo infection")

# we can use a chi-square test to test the hypothesis that getting in a car crash is conditional on being infected with toxoplasmosis. In other words, infection increases the probability of a car crash. The null hypothesis is that the two variables are independent (infection has no effect on car crash occurrence).

# do the chi-squared test, using chisq.test()
chisq.test(toxo_flipped)

# or you can use the summary() function, which automatically runs a chi-square test (because of the format of the data). This option is nice because it reminds you in the output what the chi-square test is doing (see output in the console).
summary(toxo_flipped)

# add results to your barplot 
text(2, 160, "Chisq = 33.21 \n p-value = 8.273e-09")

