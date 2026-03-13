# In this training script you will learn how to analyze the relationship between two continuous numeric variables by constructing a linear model. The linear model predicts the response value for any predictive variable values using the equation y = b0 + b1*x.

# Import the data. These data are the number of new mutations (called de novo mutations) in a couple's offspring predicted by the age of the parent who provided the sperm (labeled "father" from here on).
mutation <- read.csv("data/AgeMutations.csv")

# look at the structure of the data
str(mutation)

# assign x and y variables to the predictor and response variables, respectively 
Predictor <- mutation$AgeOfFather
Response <- mutation$numberOfNewMutations

# visualize the data as a scatterplot
plot(Predictor, Response, xlim=c(10,40), ylim=c(20,90), 
     xlab="Age of father (years)", 
     ylab="Number of mutations in offspring", 
     las=1)

# use the lm() function to perform a linear model analysis
model <- lm(Response ~ Predictor)
model_data <- summary(model)

# look at the results
print(model_data)

# Next we want to draw a line showing the predicted value of Y. First make vectors with the minimum and maximum values in the Predictor object.
min_pred <- min(Predictor)
max_pred <- max(Predictor)

# Next we will make a new vector that has all the integers (ages) from the minimum to the maximum.
new_predictor <- seq(min_pred, max_pred)

# Now harvest the two coefficients from the model. The coefficients are the B values in the equation y = B0 + B1*x. 
B0 <- summary(model)$coeff[1,1] # y intercept
B1 <- summary(model)$coeff[2,1] # slope of the line

# Calculate the predicted values of Y using the model equation.
pred_Y <- B0 + B1*new_predictor

# Draw a line showing the predicted values of Y 
lines(new_predictor, pred_Y, lwd=2)

# Use the function predict() to generate the confidence interval around the predicted values of Y. These predicted y values can also be thought of as the mean values of y for all values of x.

# To use predict() you need to use the new predictor vector that has all the values of x between the min and the max. You then replace the Predictor object from above with the NEW predictor object (we called it new_predictor) and use the confidence argument for interval. The argument newdata=data.frame() creates a new model using the values in the object new_predictor.
predicted <- predict(model, newdata=data.frame(Predictor= new_predictor), interval="confidence")

# Look at the object you just made. It has three columns" fit (the predicted y vals), lwr (lower 95% CI), and upper (upper 95% CI).
print(predicted)

# Now plot the upper and lower confidence intervals around the predicted values of Y, for each value of X. Use the lines() function to add the lines to the graph, and use indexing (square brackets) to point R to the correct columns in the predicted object.
lines(new_predictor,predicted[ ,2],lty=3)
lines(new_predictor,predicted[ ,3],lty=3)

# The confidence interval estimates the uncertainty of the expected value of y (number of mutations in the offspring) for each value of father's age ranging from 15 to 37.

# What to do next: go back through the training script and rewrite the annotations in your own words and make original object names. 

# What to submit: the image of your graph, an evidence-based claim that references the graph as well as the results of the linear model analysis, and your originally annotated code with original object names. Review the rubric on Canvas prior to submission.
