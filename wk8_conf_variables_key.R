###################### Confounding variables ######################

# Directions: add code below each of the annotations below. Use past scripts for reference, as needed.

# Import the data file (fecal_study.csv)
fecal <- read.csv("fecal_study.csv")

# Print the object you just made, so you can see all of the information in the data file
print(fecal)

##################### Part I: assess the study design ##################

# As you can see, there were 47 individuals assigned to either receive the therapy (fecal transplant) or the placebo. We also have four pieces of prior health history information for each study subject: BMI (body mass index), whether they have taken vancomycin (an antibiotic; 0=no, 1=yes), number of days they took the antibiotic prior to the study, and stool frequency (poops per day). These are all possibly confounding variables: they might influence the outcome of the study. To eliminate the effects of confounding variables, study subjects MUST be randomly assigned to either the therapy or placebo group. Your job is to assess whether this has been done, based on the data - were individuals randomly assigned? Or is there a significant difference in means between the two groups, for any of the confounding variables? Insert code below to test for this, and write your results in Table 1 on the worksheet.
t.test(fecal$BMI ~ fecal$treatment)
t.test(fecal$vancomycin ~ fecal$treatment)
t.test(fecal$days_antibiotic ~ fecal$treatment)
t.test(fecal$stool_freq ~ fecal$treatment)


########################## Part II: fix the study design ############################

# We can use the sample() function to randomly assign individuals to either the therapy or placebo group. We will add their new assignments to a new column in the data, called randomized. Fill out the code below to sample the treatment column, and store the results in fecal$randomized. 
fecal$randomized <- sample(fecal$treatment)

# Print the object containing all of the data, so you can see the new column you just made.
print(fecal)

# Analyze the data again to assess whether there is a significant difference in means between the newly generated therapy and placebo groups, for any of the confounding variables. Write your results in Table 2 on the worksheet.
t.test(fecal$BMI ~ fecal$randomized)
t.test(fecal$vancomycin ~ fecal$randomized)
t.test(fecal$days_antibiotic ~ fecal$randomized)
t.test(fecal$stool_freq ~ fecal$randomized)
