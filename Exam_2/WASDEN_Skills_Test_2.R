library(tidyverse)
library(ggplot2)
library(MASS)
library(modelr)

#TASK I

#1.
  read.csv("salaries.csv")
  df = read.csv("salaries.csv")

#2. 
  gather(df, key = "FacultyRank", value = "Salary", 5:7)
  df = gather(df, key = "FacultyRank", value = "Salary", 5:7)
  
#3.
  stat(df)
  levels(df$FacultyRank)
  df$FacultyRank

  
    p1 <- ggplot(df, aes(x= Tier, y= Salary, fill= factor("FacultyRank")) +
    geom_boxplot()
    
    p1 + ggtitle("Faculty Salaries - 1995") + labs(fill = "Faculty Rank")
    
    p2 = p1 + ggtitle("Faculty Salaries - 1995") + labs(fill = "Faculty Rank")

 #4. Plot has been successfuly exported to Repository DATA_COURSE_WASDEN Subdirectory Exam_2.   
 
    
    
#TASK II

#1.
  read.csv("atmosphere.csv")
  df = read.csv("atmosphere.csv")
  
#2.  Create two different linear models with Diversity as the dependent variable. The second model should have the same terms as the first, but an additional one or two terms as well. (10 points)
  df$Month = factor(df$Month)
  
  ggplot(df, aes(x= Aerosol_Density, y= Diversity, color = Month)) +
    geom_point() + 
    facet_wrap(~Quarter)
  
  ggplot(df, aes(x= Precip, y= Diversity)) + 
    geom_point() +
    geom_smooth(method = "lm")
  
  ggplot(df, aes(x= Precip, y= Diversity, color = Month)) +
    geom_point() +
    facet_wrap(~Quarter)
  
  ggplot(df, aes(x= Month, y= Diversity)) +
    geom_point()
  
  ggplot(df, aes(x=  CO2_Concentration, y= Diversity)) +
    geom_point() +
    geom_smooth(method = "lm")
  
  mod1 = lm(Diversity ~ Precip, data = df)
  
  mod2 = lm(Diversity ~ Precip*CO2_Concentration, data = df)
  
#3.  Compare the residuals of the two models and document which has better explanatory power for the data (10 points)
  summary(mod1)
  summary(mod2)
  
  residuals(mod1)
  residuals(mod2)
  
  mean(residuals(mod1))
  mean(residuals(mod2))
  
  # The mean of the residuals for both methods show that mod2 has a lower mean, which amounts to lower values for the residuals.
  
  mean(residuals(mod1) - residuals(mod2))
  
  anova(mod1)
  anova(mod2)
  anova(mod1, mod2)
 
#4.  Use these both models to predict Diversity values in the data set (10 points)
  
  ?add_predictions()
  df = add_predictions(df, mod1, var = "pred1")
  df = add_predictions(df, mod2, var = "pred2")
  
  ggplot(df, aes(x= Precip, y= Diversity)) + 
    geom_point() +
    geom_point(aes(y = pred1, color = "Blue", alpha = 0.5)) +
    geom_point(aes(y = pred2, color = "Red", alpha = 0.5)) +
    geom_smooth(method = "lm")

  ggplot(df, aes(x= Precip*CO2_Concentration, y= Diversity)) +
    geom_point() +
    geom_smooth(method = "lm", aes(y= pred2))
  
#5.  Make a plot showing actual Diversity values, along with the two models' predicted Diversity values. Use color or some other aesthetic to differentiate the actual values and both predictions (10 points)

  ggplot(df, aes(x= Precip, y= Diversity)) +
    geom_point() +
    geom_smooth(method = "lm", aes(y= pred1, color = "Red"), se = TRUE) +
    geom_smooth(method = "lm", aes(y= pred2, color = "Blue"), se = TRUE) 
    
  ggplot(df, aes(x= Precip*CO2_Concentration, y= Diversity)) +
    geom_point() +
    geom_smooth(method = "lm", aes(y= pred1, color = "Red"), se = TRUE) +
    geom_smooth(method = "lm", aes(y= pred2, color = "Blue"), se = TRUE) 
  
#6.  Write code to show the predicted values of Diversity for each model using the hypothetical data found in hyp_data.csv (10 points)
    read.csv("hyp_data.csv")
    df2 <- read.csv("hyp_data.csv")
    
    df2 = add_predictions(df2, mod1, var = "pred1")
    df2 = add_predictions(df2, mod2, var = "pred2") 
    
    ggplot(df2, aes(x= Precip, y= pred1)) + 
      geom_point() + 
      geom_smooth(aes(y= mod1))
    
#7.  Export a text file that contains the summary output from *both* your models to "model_summaries.txt" (10 points)
  s <- summary(mod1)
  s2 <- summary(mod2)
  capture.output(s, file = "model_summary1.txt")
  capture.output(s2, file = "model_summary2.txt")
  
#*Bonus*
#8.  Add these predicted values (from hypothetical data - Part II, Step 6) to a plot of actual data 
and differentiate them by color. (10 bonus points possible for a pretty graph)

ggplot(df, aes(x= Precip, y= Diversity)) + 
  geom_point() +
  geom_point(aes(y = df2$pred2)) + 
  geom_smooth(method = "lm")


#extra Bird Project

#read data

bird = read.csv("Desktop/Data_Course/Data/Bird_Measurements.csv")
names(bird)

#find columns with mass (except egg mass)

masscols = c(5, 7, 9)

#find columns to keep in every subset

impt.cols = c(1:4)
?grep

#subset to mass only

bird.mass = bird[,c(impt.cols, masscols)]

#turn to long format

mass.long = gather(bird.mass, Sex, Mass, 5:7)

#clean up sex values

mass.long$Sex = str_remove(mass.long$Sex, "_mass")

unique(mass.long$Sex)

mass.long$Mass = str_remove(mass.long$Mass, "NA")

?na.omit

mass.long = na.omit(mass.long)

mass.long.alpha <- mass.long[order(mass.long$Species_name),]

Push the following to your github web page in your new Exam_2 directory:
  1.  Your complete R script for ALL the above tasks, saved as LASTNAME_Skills_Test_2.R
2.  Your plot from Part I
3.  Any plots generated from Part II
4.  model_summaries.txt
5.  A separate R script for importing and cleaning up the bird data (from Tuesday) 
*20 BONUS if your script works and is well-documented*
  
  
  