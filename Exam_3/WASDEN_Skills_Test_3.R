library(tidyverse)
library(gapminder)
library(ggplot2)

#Custom colour palette 

pal = c("#6b5456","#ec8d1b","#6abf2a","#8b53b7","#70acbe",
        "#01c95b","#c00014","#31332f","#f7d000","#abba00")

#Data for plot 1

plot1.data = gapminder

#Code for copy of plot 1 is here

ggplot(plot1.data, aes(x = year, y = lifeExp, color = continent)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method  = loess, se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Life Expectancy Over Time", subtitle = "Colored by Continent") +
  theme_minimal() +
  ylab("Life Expectancy") +
  xlab("Year")
  
png(filename = "Wasden_plot1.png", width = 5, height = 5, units = "in", res = 600)
ggplot(plot1.data, aes(x = year, y = lifeExp, color = continent)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method  = loess, se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Life Expectancy Over Time", subtitle = "Colored by Continent") +
  theme_minimal() +
  ylab("Life Expectancy") +
  xlab("Year")
dev.off()
?png

#Data for plot 2

set.seed(123)
a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
plot2.data <- rbind(a,b,c)


#Code for plot 2 is here

ggplot(plot2.data, aes(x = x, y = y)) +
  geom_bin2d() +
  theme_void()

plot(plot2.data)

png(filename = "Wasden_plot2.png", width = 5, height = 5, units = "in", res = 600)
ggplot(plot2.data, aes(x = x, y = y)) +
  geom_bin2d() +
  theme_void()
dev.off()
