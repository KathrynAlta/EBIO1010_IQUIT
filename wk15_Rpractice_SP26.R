################## R practice problems ###############
# Make a graph, analyze the data, and make a claim for each of the four scenerios below.

# Modified excerpt from Peterson et al. (2015)Links to an external site. abstract: Northern elephant seals (Mirounga angustirostris) biannually travel thousands of kilometres to forage within coastal and open-ocean regions of the northeast Pacific Ocean. We coupled satellite telemetry, diving behaviour and stable isotopes (carbon and nitrogen) for 77 adult females, that varied by foraging location and diving depth, to assess mercury concentrations in blood and muscle. Our results indicate that foraging behaviour influences mercury exposure and mesopelagic predators foraging in the northeast Pacific Ocean may be at high risk for mercury bioaccumulation.

# 1. Does the length of foraging trips (short vs. long; ~73 days vs. ~223 days respectively) influence blood mercury content? Data columns = Trip and BloodHg_ug_g_ww

# 2. There are three different foraging clusters: northerly, shallower offshore, and deeper offshore. Is there a difference in blood mercury content between any of the three clusters? Data columns = Cluster_name and BloodHg_ug_g_ww

# 3. What is the effect of time spent foraging in the California Current Ecosystem (data are percentages) on blood mercury content? Data columns = Percent_Time_In_California_Current_Ecoregion and BloodHg_ug_g_ww
  
# 4. And last, unrelated to elephant seals... does the probability of algal blooms occurring in lakes on the Front Range depend on whether they are adjacent to agricultural operations? The data are here Download data are here.

#################### solutions #################
  
  
# import all of the data
seals <- read.csv("data/Elephant_seals_data.csv")
algae <- read.csv("data/algal_blooms.csv")

##################### 1 - t test ####################
# Does the length of foraging trips (short vs. long; ~73 days vs. ~223 days respectively) influence blood
#  mercury content? Data columns = Trip and BloodHg_ug_g_ww
x <- seals$Trip
y <- seals$BloodHg_ug_g_ww

ttestres <- t.test(y~x)
ttestres

plot(c(0.4286957 - 0.3455484), 1, pch = 19, xlab = "Difference in mean blood Hg", ylab = NA, yaxt = "n", xlim = c(-.01, .14))
    # make a plot and show the difference in means (you could also calculate the diff in means outside )
abline(v = 0, lty = 2) # add a vertical line at zero to show the null hypothesis 
segments(0.12849230, 1, 0.03780223, 1) # add a line segment, give the starting coordinates and the ending coordinates 
text(.08, 1.1, "Short trips > Long trips") # add text to the plot showing that on this side of zero the Hg in short trips is greater than the Hg in long trips group 
text(.12, 1.3, "p-value = 0.0004814 \n t = -3.6541") # add p-value and t statistic to the graph 

###################### 2 - ANOVA ####################
# There are three different foraging clusters: northerly, shallower offshore, and deeper offshore. Is there a difference in blood mercury content between any of the three clusters? Data columns = Cluster_name and BloodHg_ug_g_ww
x <- seals$Cluster_name
y <- seals$BloodHg_ug_g_ww

aov_model_results <- aov(y ~ x)
summary(aov_model_results)

Tukey_res <- TukeyHSD(aov_model_results)
print(Tukey_res)

mean_DeepOff <- mean(seals$BloodHg_ug_g_ww[seals$Cluster_name=="Deeper offshore"])
mean_ShallowOff <- mean(seals$BloodHg_ug_g_ww[seals$Cluster_name=="Shallower offshore"])
mean_Northerly <- mean(seals$BloodHg_ug_g_ww[seals$Cluster_name=="Northerly"])

seal_cluster_means <- c(mean_DeepOff, mean_ShallowOff, mean_Northerly)

barplot(seal_cluster_means, names = c("Deeper Offshore","Shallower offshore","Northerly"), xlab="Elephant seal foraging locations", ylab="Mean blood mercury", las=1, col="#3B9AB2", ylim=c(0,.9)) 

sd_DeepOff <- sd(seals$BloodHg_ug_g_ww[seals$Cluster_name=="Deeper offshore"])
sd_ShallowOff <- sd(seals$BloodHg_ug_g_ww[seals$Cluster_name=="Shallower offshore"])
sd_Northerly <- sd(seals$BloodHg_ug_g_ww[seals$Cluster_name=="Northerly"])

n_DeepOff <- length(seals$BloodHg_ug_g_ww[seals$Cluster_name=="Deeper offshore"])
n_ShallowOff <- length(seals$BloodHg_ug_g_ww[seals$Cluster_name=="Shallower offshore"])
n_Northerly <- length(seals$BloodHg_ug_g_ww[seals$Cluster_name=="Northerly"])

SE_DeepOff <- sd_DeepOff/sqrt(n_DeepOff)
SE_ShallowOff <- sd_ShallowOff/sqrt(n_ShallowOff)
SE_Northerly <- sd_Northerly/sqrt(n_Northerly)

SEs <- c(SE_DeepOff, SE_ShallowOff, SE_Northerly)

lower_ci <- seal_cluster_means - 2*SEs
upper_ci <- seal_cluster_means + 2*SEs

barx <- barplot(seal_cluster_means, names = c("Deeper Offshore","Shallower offshore","Northerly"), xlab="Elephant seal foraging locations", ylab="Mean blood mercury", las=1, col="#D3DDDC", ylim=c(0,1))

segments(barx, lower_ci, barx, upper_ci)

print(Tukey_res)
text(barx, .7, c("A","B","C"))
text(3, .9, "AOV results: \np = 1.2e-07 \n F = 19.91")


#################### 3 - linear model ################
# What is the effect of time spent foraging in the California Current Ecosystem (data are percentages) on blood mercury content? Data columns = Percent_Time_In_California_Current_Ecoregion and BloodHg_ug_g_ww
x <- 100*seals$Percent_Time_In_California_Current_Ecoregion  # convert to percent (currently as proportion)
y <- seals$BloodHg_ug_g_ww

plot(x, y, xlab="Percent of time foraging in CA current ecosystem", ylab="Seal blood mercury", las=1)

model <- lm(y ~ x)
model_data <- summary(model)
print(model_data)

min_pred <- min(x)
max_pred <- max(x)
new_predictor <- seq(min_pred, max_pred)

B0 <- summary(model)$coeff[1,1] # y intercept
B1 <- summary(model)$coeff[2,1] # slope of the line

pred_Y <- B0 + B1*new_predictor

lines(new_predictor, pred_Y, lwd=2)

predicted <- predict(model, newdata=data.frame(x= new_predictor), interval="confidence")
print(predicted)

lines(new_predictor,predicted[ ,2],lty=3)
lines(new_predictor,predicted[ ,3],lty=3)


text(85, .6, "p = 0.1142 \n R-squared = 0.03293")

##################### 4 - chi-square #####################
# Fertilizer applied to agricultural fields can result in nutrient runoff into adjacent water bodies, which may result in increased growth of primary producers like algae. Does the probability of algal blooms occurring in lakes on the Front Range depend on whether they are adjacent to agricultural operations?
algae_summary <- table(algae)
print(algae_summary)

barplot(algae_summary, beside = TRUE, legend = TRUE)
algae_flipped <- t(algae_summary)

barplot(algae_flipped, beside = TRUE, legend = TRUE, legend.text = c("No algal bloom", "Yes algal bloom"), cex.names = 1, ylab = "Number of lakes", ylim = c(0, 40), xlab = "Adjacent to agriculture")

chisq.test(algae_flipped)

# add results to your barplot 
text(2, 35, "Chisq = 13.542 \n p-value = 0.0002333")
