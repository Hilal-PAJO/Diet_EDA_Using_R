#📖INTRODUCTION
#This study aims to understand the effects of individuals' stress level, 
#sleep quality, age, gender and other factors on weight change. The aim of this 
#analysis is to create a model that can predict weight change.
#For this purpose, answers were sought to questions such as:
#What are the relationship among provided factors and visualization of results?
#What are the variables that best explain weight change?
#Is there a regression model that can predict weight change?

#🏥This analysis can be beneficial for health professionals, nutritionists, and 
#individuals seeking to understand the key factors influencing weight change 
#and develop more personalized health and wellness strategies.

#💻This project was conducted to showcase my data analysis skills, and while the 
#insights are valuable, the findings could be further strengthened with a 
#larger sample size. Additionally, the analysis could be expanded with 
#alternative perspectives and methods, reflecting the dynamic nature of data analysis.


#🔗DATA SOURCE AND METHODOLOGY
#Source of this dataset is:https://www.kaggle.com/datasets/abdullah0a/comprehensive-weight-change-prediction/data
#The dataset includes variables such as stress, sleep, age, gender, calorie 
#consumption and basal metabolic rate of individuals(100 raws and 14 columns). 
#In this study, data cleaning, exploratory analysis, regression analysis and 
#hypothesis testing were performed.

#ABOUT THIS FILE
#📌Participant ID: Unique identifier for each participant in the study.
#📌Age: The age of the participant (in years), which can influence metabolism and weight change.
#📌Gender: Gender of the participant (M/F), as physiological differences may affect weight management.
#📌Current Weight (lbs): The participant's weight at the beginning of the study, serving as a baseline for weight change.
#📌BMR (Calories): Basal Metabolic Rate, calculated using the Mifflin-St Jeor equation, representing the number of calories burned at rest.
#📌Daily Calories Consumed: Total caloric intake per day, including variability to reflect real-world eating habits.
#📌Daily Caloric Surplus/Deficit: The difference between calories consumed and BMR, indicating whether the participant is in a caloric surplus or deficit.
#📌Weight Change (lbs): The estimated change in weight over a specified duration, based on caloric surplus/deficit.
#📌Duration (weeks): The time period over which weight change is measured, ranging from 1 to 12 weeks.
#📌Physical Activity Level: Self-reported level of physical activity, categorized as Sedentary, Lightly Active, Moderately Active, or Very Active.
#📌Sleep Quality: Self-reported quality of sleep, categorized as Poor, Fair, Good, or Excellent, which can affect weight management.
#📌Stress Level: A numerical score (1-10) indicating the participant's perceived stress level, as stress can influence eating behaviors and weight.

# improting data
diet_csv <- read.csv("C:/Users/90544/Documents/project portfolio/Diet analysis, predict the weight/weight_change_dataset.csv") 
head(diet_csv)

#data cleaning
sum(is.na(diet_csv))
colSums(is.na(diet_csv))
#Data validation step complete: No missing values detected, ensuring data 
# quality. Next steps include exploring potential outliers and inconsistencies.

#summary statistics
summary(diet_csv)
prop.table(table(diet_csv$Gender))
prop.table(table(diet_csv$Physical.Activity.Level))
prop.table(table(diet_csv$Sleep.Quality))

#examining categorical variables
table(diet_csv$Gender)
table(diet_csv$Physical.Activity.Level)
table(diet_csv$Sleep.Quality)
str(diet_csv)

#Calculate the correlation matrix to understand relationships between numeric features.
numeric_columns <- sapply(diet_csv, is.numeric)
correlation_matrix <- cor(diet_csv[, numeric_columns])
print(round (correlation_matrix, 2))
#The correlation matrix shows the linear relationships between weight change and 
#other variables. The correlation coefficients between variables take values 
#between -1 and +1. Values. close to 0 indicate no relationship, 0.3-0.7 indicate 
#a moderate relationship, and 0.7 and above indicate a strong relationship. 
#In this analysis, a positive relationship of 0.65 was observed between weight 
#change and stress level. This shows that as stress level increases, weight change tends to increase.

#correlation heatmap
install.packages("corrplot")
library(corrplot)

png("correlation_heatmap.png" , width = 800, height = 600)
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)
dev.off()

library(ggcorrplot)
#Plotting the correlation matrix as a heat map
png("correlation_matrix_heatmap.png", width = 800, height = 600)
ggcorrplot(correlation_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red", "white", "blue"), 
           title = "Correlation Matrix Heat Map")
dev.off()
#The heatmap visually presents the relationship between variables. Here, it is 
#clearly seen that there is a positive relationship of 0.65 between 'Weight 
#Change (lbs)' and 'Stress Level'. No significant relationship was found between 
#sleep quality and weight change. Also, a strong relationship of 0.80 is seen 
#between 'Daily Calories Consumed' and 'BMR', indicating that there may be multicollinearity.

#*Current Weight (lbs) and Final Weight (lbs): Current weight and final weight naturally has a relationship
#current and final weight relationship
library(ggplot2)
png("current_vs_final_weight.png", width = 800, height = 600)
ggplot(diet_csv, aes(x = Current.Weight..lbs., y = Final.Weight..lbs.)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title = "Current Weight vs Final Weight", x = "Current Weight (lbs)", y = "Final Weight (lbs)")
dev.off()

#*Stress Level and Weight Change (lbs): Stress has a moderate negative effect on weight lost.
#stress level and weight change relationship
png("stress_level_vs_weight_change.png", width = 800, height = 600)
ggplot(diet_csv, aes(x = Stress.Level, y = Final.Weight..lbs.)) + 
  geom_point(alpha = 0.6) + 
  geom_smooth( method = "lm", col = "red") +
  theme_minimal() +
  labs(title = "Stress Level vs Weight Change", x = "Stress Level", y = "Weight Change (lbs)")
dev.off()

#*Daily Caloric Surplus/Deficit and Weight Change (lbs): A moderately positive correlation exists, indicating that a greater 
#caloric deficit or surplus is associated with changes in weight. This reflects a fundamental principle of energy balance in weight management.
#Daily calorie intake and weight change relationship
png("daily_calorie_intake_vs_weight_change.png", width = 800, height = 600)
ggplot(diet_csv, aes(x = Daily.Calories.Consumed, y = Weight.Change..lbs.)) +
  geom_point(alpha = 0.6, color = "green" ) +
  geom_smooth(method = "lm", col = "darkblue") +
  theme_minimal () +
  labs(title = "Daily Calories Consumed vs Weight Change", x = "Daily Calories Consumed", y = "Weihgt Change (lbs)")
dev.off()

#Explore the relationship between BMR and Current Weight to assess metabolic rate differences.
png("BMR_vs_current_weight.png", width = 800, height = 600)
ggplot(diet_csv, aes(x = BMR..Calories., y = Current.Weight..lbs.)) +
  geom_point(alpha = 0.6, color = "green" ) +
  geom_smooth(method = "lm", col = "darkblue") +
  theme_minimal () +
  labs(title = "BMR Calories vs Current Weight (lbs)", x = "BMR Calories", y = "Current Weight (lbs)")
dev.off()

#visualization to see the effects of categorical variables
#sleep quality
png("sleep_quality_distribution.png", width = 800, height = 600)
ggplot(diet_csv, aes(x = Sleep.Quality)) + 
  geom_bar(fill = "coral") + 
  labs(title = "Sleep Quality Distribution")
dev.off()

#physical activity level
png("physical_activity_distribution.png", width = 800, height = 600)
ggplot(diet_csv, aes(x = Physical.Activity.Level)) + 
  geom_bar(fill = "darkgreen") + 
  labs(title = "Physical Activity Level Distribution")
dev.off()

#age
png("age_distribution.png", width = 800, height = 600)
ggplot(diet_csv, aes(x = Age)) + 
  geom_bar(fill = "purple") + 
  labs(title = "Age Distribution")
dev.off()

#which parameters are much related to weight change via regression analysis
model <- lm(Weight.Change..lbs. ~ Age + Stress.Level + Daily.Calories.Consumed + Current.Weight..lbs., data = diet_csv)
summary(model)
png("weight_change_regression_diagnostics.png", width = 800, height = 600)
plot(model, which = 1)
dev.off()
#Stress level has significant effect on weight change

#Check if weight change significantly differs between genders using ANOVA
result_gender <- aov(Weight.Change..lbs. ~ Gender , data = diet_csv)
summary(result_gender)
#Gender does not significantly affect weight change (p = 0.505).

#Check if weight change significantly differs in physical activity level using ANOVA
result_physical <- aov(Weight.Change..lbs. ~ Physical.Activity.Level , data = diet_csv)
summary(result_physical)

png("weight_change_by_physical_activity.png", width = 800, height = 600)
boxplot(Weight.Change..lbs. ~ Physical.Activity.Level , data = diet_csv, 
        main = "Weight Change by Physical Activity Level " , 
        xlab = "Physical Activity Level" , ylab = "Weight Change (lbs)",
        col = "lightblue", border = "darkblue", frame = FALSE)
dev.off()
#"No significant relationship found between physical activity and weight change 
#(potentially due to small sample size)."

#Evaluate whether weight change differs significantly by sleep quality using ANOVA.
result_sleep <- aov(Weight.Change..lbs. ~ Sleep.Quality , data = diet_csv)
summary(result_sleep)

png("weight_change_by_sleep_quality.png", width = 800, height = 600)
boxplot(Weight.Change..lbs. ~ Sleep.Quality , data = diet_csv, 
        main = "Weight Change by Sleep Quality " , 
        xlab = "Sleep Quality" , ylab = "Weight Change (lbs)",
        col = "green" , border = "black" , frame = FALSE)
dev.off()
#there is a meaningful relationship between weight change and sleep quality.

#Check if sleep quality significantly differs for daily calories consumed using ANOVA
result_sleepvscalorieconsumed <- aov(Daily.Calories.Consumed ~ Sleep.Quality , data = diet_csv)
summary(result_sleepvscalorieconsumed)
#this data does not show a significant relationship between sleep quality and daily calories consumed

#Check if sleep quality significantly differs for final weight using ANOVA
result_sleepvscfinalw <- aov(Final.Weight..lbs. ~ Sleep.Quality , data = diet_csv)
summary(result_sleepvscfinalw)
#We cannot talk about a meaningful relationship between sleep quality and final weight

#Evaluate whether sleep quality differs significantly by stress level using ANOVA.
result_sleepvsstress <- aov(Stress.Level ~ Sleep.Quality , data = diet_csv)
summary(result_sleepvsstress)

png("stress_level_by_sleep_quality.png", width = 800, height = 600)
boxplot(Stress.Level ~ Sleep.Quality , data = diet_csv, 
        main = "Stress Level by Sleep Quality " , 
        xlab = "Sleep Quality" , ylab = "Stress Level" ,
        col = "lightblue" , border = "darkblue" , frame = FALSE)
dev.off()
#sleep quality is also cannot be explained by stress level as well according to our sample group.

#histogram analysis
#Plot distribution of weight change to check for skewness and normality.
png("weight_change_histogram.png", width = 800, height = 600)
hist(diet_csv$Weight.Change..lbs., breaks = 25, col = "lightblue", main = "Weight Change Distribution" ,
     xlab = "Weight Change (lbs)" )
dev.off()

#Plot distribution of age to check for skewness and normality.
png("age_histogram.png", width = 800, height = 600)
hist(diet_csv$Age , breaks = 15 ,col = "lightblue" , main = "Age Distribution" , xlab = "Age")
dev.off()

#Plot distribution of current weight to check for skewness and normality.
png("current_weight_histogram.png", width = 800, height = 600)
hist(diet_csv$Current.Weight..lbs. , breaks = 15 , col = "lightblue" , main = "Current Weight Distribution (lbs)" ,
     xlab = "Current Weight (lbs) ")
dev.off()

#Plot distribution of BMR Calories to check for skewness and normality.
png("BMR_histogram.png", width = 800, height = 600)
hist(diet_csv$BMR..Calories., breaks = 15 , col = "lightblue", main = "BMR Calories Distribution",
     xlab = "BMR Calories")
dev.off()

#Plot distribution of daily calories consumed to check for skewness and normality.
png("daily_calories_consumed_histogram.png", width = 800, height = 600)
hist(diet_csv$Daily.Calories.Consumed , breaks = 10 ,col = "lightblue" , 
     main = "Daily Calorie Consumption Distribution", xlab = "Daily Calories Consumed")
dev.off()

#Plot distribution of daily caloric surplus/deficit to check for skewness and normality.
png("daily_caloric_surplus_deficit_histogram.png", width = 800, height = 600)
hist(diet_csv$Daily.Caloric.Surplus.Deficit, col = "lightblue", 
     main = "Daily Caloric Surplus Deficit" , xlab = "Daily Caloric Surplus Deficit")
dev.off()

#Plot distribution of final weight to check for skewness and normality.
png("final_weight_histogram.png", width = 800, height = 600)
hist(diet_csv$Final.Weight..lbs., col = "lightblue", main = "Final Weight Distribution",
     xlab = "Final Weight", breaks = 15)
dev.off()

#Examining parameters in relationship
model_calorie <- lm(Daily.Caloric.Surplus.Deficit ~ BMR..Calories. + 
                      Daily.Calories.Consumed, data = diet_csv)
summary(model_calorie)
png("daily_caloric_surplus_deficit_regression_diagnostics.png", width = 800, height = 600)
plot(model_calorie, which = 1)
dev.off()
#Daily Caloric Surplus/Deficit=Daily Calories Consumed−BMR Calories
#The nearly perfect fit (R-squared = 1) confirms that this formula fully 
#explains the variance in the dependent variable, leaving almost no room 
#for residual variation. 

#Can Weight change be explained by age and sleep quality
model_wc <- lm(Weight.Change..lbs. ~ Age + Sleep.Quality, data = diet_csv)
summary(model_wc)
png("weight_change_regression_diagnostics.png", width = 800, height = 600)
plot(model_wc, which = 1)
dev.off()
#This suggests that individuals with poor sleep quality experience a 
#significantly greater weight change (about 10 lbs less) compared to those 
#with excellent sleep quality. And age does not matter in this data set.

#focusing on poor sleep quality
diet_csv$PoorSleep <- ifelse(diet_csv$Sleep.Quality == "Poor", 1,0)

model_poor_sleep <- lm(Weight.Change..lbs. ~ Stress.Level * PoorSleep, data = diet_csv)
summary(model_poor_sleep)
png("poor_sleep_regression_diagnostics.png", width = 800, height = 600)
plot(model_poor_sleep,which = 1)
dev.off()
#Stress Level has a significant impact on weight change, with higher stress levels leading to greater weight loss.
#Poor Sleep Quality alone does not have a statistically significant effect, but it amplifies the negative impact 
#of stress levels on weight change (as shown by the significant interaction term).
#This suggests that the combination of high stress and poor sleep quality is particularly detrimental to weight maintenance or gain.

#Visualize the interaction effect of stress level and poor sleep on weight change.
install.packages("interactions")
library(interactions)
png("stress_poor_sleep_interaction_plot.png", width = 800, height = 600)
interact_plot (model_poor_sleep,pred = Stress.Level , modx = PoorSleep,
              interval = TRUE, plot.points = TRUE)
dev.off()

#CONCLUSION
#The analysis conducted on this dataset provides valuable insights into the 
#relationships between various factors and weight change. Here are the key findings:

#Stress Level and Weight Change: A positive correlation was observed between 
#stress levels and weight change, suggesting that higher stress levels tend 
#to contribute to greater changes in weight. Additionally, stress appears to 
#amplify the negative effects of poor sleep quality on weight change.

#Sleep Quality and Weight Change: Poor sleep quality was associated with 
#significantly greater weight changes, particularly when combined with high 
#stress levels. However, sleep quality alone was not found to be a strong 
#predictor of weight change in this sample.

#Daily Caloric Surplus/Deficit and Weight Change: As expected, a caloric 
#surplus or deficit was moderately correlated with weight change, confirming 
#the fundamental principle of energy balance in weight management.

#Age and Gender: Neither age nor gender was found to have a significant 
#impact on weight change within this dataset.

#Physical Activity: Despite existing research showing the positive impact of 
#physical activity on weight management, no significant relationship was 
#observed in this sample. This discrepancy may be due to the small sample size.

#Sample Size Limitations: The dataset used in this study included only 
#100 individuals, which limits the generalizability of the findings. 
#Small sample sizes are more prone to statistical variability, and results 
#should be interpreted with caution.







































