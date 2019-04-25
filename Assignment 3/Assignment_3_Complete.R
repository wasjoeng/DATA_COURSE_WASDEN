
############ YOUR HOMEWORK ASSIGNMENT ##############

# 1.  Make a scatterplot of headwidth vs mass. See if you can get the points to be colored by "Colony"
library(ggplot2)

setwd("~/Desktop/Data_Course/data/")

dat = read.csv("thatch_ant.csv")

dat$Colony = as.factor(dat$Colony)

p <- ggplot(dat, aes(x=Headwidth, y=Mass, colour = Colony)) +
  geom_point() +
  coord_flip() +
  labs(title = "Thatch Ant Headwidth vs Mass")

# 2.  Write the code to save it (with meaningful labels) as a jpeg file
jpeg('Thatch_Graph.jpeg')
p
dev.off()

# 3.  Subset the thatch ant data set to only include ants from colony 1 and colony 2
library(dplyr)
Colony1_2dat = dat %>%
  filter(Colony == 1 & 2)

# 4.  Write code to save this new subset as a .csv file

write.csv(Colony1_2dat, file = "ThatchColon1_2.csv")

# 5.  Upload this R script (with all answers filled in and tasks completed) to canvas
      # I should be able to run your R script and get all the plots created and saved, etc.
