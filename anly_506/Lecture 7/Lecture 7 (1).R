##########-------------------Chapter 15-----------------------###########

#Clear workspace
rm(list=ls())
#Clear plots
dev.off()

#Install packages. This will only install if you don't already have the package downloaded.
if(!require(ggplot2)){install.packages("ggplot2")}

#Slide 4
with(airquality, { 
        plot(Temp, Ozone)
        lines(loess.smooth(Temp, Ozone))
})

#Slide 5
library(ggplot2)
ggplot(airquality, aes(Temp, Ozone)) + 
        geom_point() + 
        geom_smooth(method = "loess", se = FALSE)

#Slide 9
str(mpg)

#Slide 10
qplot(displ, hwy, data = mpg)

#Slide 11
qplot(displ, hwy, data = mpg, color = drv)

#Slide 12
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))

#Slide 13
qplot(hwy, data = mpg, fill = drv, binwidth = 2)

#Slide 14
qplot(drv, hwy, data = mpg, geom = "boxplot")

#Slide 15
qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2)

#Slide 16
qplot(displ, hwy, data = mpg, facets = . ~ drv)

#Slide 17
qplot(displ, hwy, data = mpg, facets = . ~ drv) + geom_smooth()

#Slide 18
#Set working directory
setwd("./source/class_assignments/anly_506/Week 7")
#Read in data
maacs <- read.csv("maacs.csv")
str(maacs)

#Slide 20
qplot(log(eno), data = maacs)

#Slide 21
qplot(log(eno), data = maacs, fill = mopos)

#Slide 22
qplot(log(eno), data = maacs, geom = "density")

#Slide 23
qplot(log(eno), data = maacs, geom = "density", color = mopos)

#Slide 24
qplot(log(pm25), log(eno), data = maacs, geom = c("point", "smooth"))

#Slide 25
qplot(log(pm25), log(eno), data = maacs, shape = mopos)

#Slide 26
qplot(log(pm25), log(eno), data = maacs, color = mopos)

#Slide 27
qplot(log(pm25), log(eno), data = maacs, color = mopos, 
      geom = c("point", "smooth"), method = "lm")

#Slide 28
qplot(log(pm25), log(eno), data = maacs, geom = c("point", "smooth"), 
      method = "lm", facets = . ~ mopos)

##########-------------------Chapter 16-----------------------###########
#Slide 34
#Set working directory
# setwd("C:\\Users\\Owner\\Documents\\Tommy\\Lectures\\Lecture 7")
#Read in data
maacs2 <- read.csv("maacs2.csv")
str(maacs2)

#Slide 35
ggplot(maacs2, aes(logpm25, NocturnalSympt)) + geom_point()

#Slide 36
#Start with blank grid
g <- ggplot(maacs2, aes(logpm25, NocturnalSympt))
g

#Slide 37
#Add geom_point() to create the graph
g + geom_point()

#Slide 38
#Add line
g + geom_point() + geom_smooth()

#Slide 39
g + geom_point() + geom_smooth(method = "lm")

#Slide 40
g + geom_point() + 
        geom_smooth(method = "lm") +
        facet_grid(. ~ bmicat) 

#Slide 41
g + geom_point(color = "steelblue", size = 4, alpha = 1/2)

#Slide 42
g + geom_point(aes(color = bmicat), size = 4, alpha = 1/2)

#Slide 43
g + geom_point(aes(color = bmicat)) + 
        labs(title = "MAACS Cohort") + 
        labs(x = expression("log " * PM[2.5]), y = "Nocturnal Symptoms")

#Slide 44
g + geom_point(aes(color = bmicat), size = 2, alpha = 1/2) + 
  geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE)

#Slide 45
g + geom_point(aes(color = bmicat)) + theme_bw(base_family = "Times")

#Slides 46 and 47
cutpoints <- quantile(maacs2$logno2_new, seq(0, 1, length = 4), na.rm = TRUE)
maacs2$no2tert <- cut(maacs2$logno2_new, cutpoints)
#See the levels of the newly created factor variable
levels(maacs2$no2tert)
#Setup ggplot with data frame
g <- ggplot(maacs2, aes(logpm25, NocturnalSympt))
#Add layers
g + geom_point(alpha = 1/3) + 
  facet_wrap(bmicat ~ no2tert, nrow = 2, ncol = 4) + 
  geom_smooth(method="lm", se=FALSE, col="steelblue") + 
  theme_bw(base_family = "Avenir", base_size = 10) + 
  labs(x = expression("log " * PM[2.5])) + 
  labs(y = "Nocturnal Symptoms") + 
  labs(title = "MAACS Cohort")

#Slide 48
testdat <- data.frame(x = 1:100, y = rnorm(100))
testdat[50,2] <- 100  ## Outlier!
plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3))

#Slide 49
g <- ggplot(testdat, aes(x = x, y = y))
g + geom_line()

#Slide 50
g + geom_line() + ylim(-3, 3)

#Slide 51
g + geom_line() + coord_cartesian(ylim = c(-3, 3))