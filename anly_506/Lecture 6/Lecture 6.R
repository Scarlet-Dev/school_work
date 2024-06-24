##########-------------------Chapter 8-----------------------###########

#Clear workspace
rm(list=ls())
#Clear plots
dev.off()

#Install packages. This will only install if you don't already have the package downloaded.
if(!require(dplyr)){install.packages("dplyr")}
if(!require(lattice)){install.packages("lattice")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggplot2)){install.packages("RColorBrewer")}

#Set working directory
setwd("C:\\Users\\Owner\\Documents\\Tommy\\Lectures\\Lecture 6")

##Slide 4
#Open list of all built in datasets in R
data()
#Load "cars" built in dataset
data(cars)
## Create the plot / draw canvas
with(cars, plot(speed, dist))
#Another way to create the same plot
plot(cars$speed,cars$dist)
## Add annotation
title("Speed vs. Stopping distance")

##Slide 5
#Load "airquality" built in dataset
data(airquality)
#Create the plot/draw canvas
with(airquality, {
        plot(Temp, Ozone)
        lines(loess.smooth(Temp, Ozone))
})
#Add annotation
title("Effect of Ozone on Temperature")

##Slide 8
library(lattice)
#Load built in dataset state.x77 and define new custom dataset based on state.x77
state <- data.frame(state.x77, region = state.region)
#Create xyplot
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

##Slide 10
library(ggplot2)
#Load built in dataset mpg
data(mpg)
#Create qplot
qplot(displ, hwy, data = mpg)

##########-------------------Chapter 9-----------------------###########

##Slide 16
#Load built in "faithful" dataset
data(faithful)
## Make plot appear on screen device
with(faithful, plot(eruptions, waiting))
## Annotate with a title
title(main = "Old Faithful Geyser data")

##Slide 18
#Set working directory
setwd("C:\\Users\\Owner\\Documents\\Tommy\\Lectures\\Lecture 6")
## Open PDF device; create 'myplot.pdf' in my working directory
pdf(file = "myplot.pdf")
## Create plot and send to a file (no plot appears on screen)
with(faithful, plot(eruptions, waiting))
## Annotate plot; still nothing on screen
title(main = "Old Faithful Geyser data")
## Close the PDF file device
dev.off()
## Now you can view the file 'myplot.pdf' on your computer (in working directory)

##Slide 23
## Create plot on screen device
with(faithful, plot(eruptions, waiting))
## Add a main title
title(main = "Old Faithful Geyser data")
## Copy my plot to a PNG file
dev.copy(png, file = "geyserplot.png")
## Don't forget to close the PNG device!
dev.off()

##########-------------------Chapter 10-----------------------###########

##Slide 30
## Draw a new plot on the screen device
hist(airquality$Ozone)

##Slide 31
with(airquality, plot(Wind, Ozone))

##Slide 32
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

##Slide 35
par("lty")
par("col")
par("pch")
par("bg")
par("mar")
par("mfrow")

##Slide 37
## Make the initial plot
with(airquality, plot(Wind, Ozone))
## Add a title
title(main = "Ozone and Wind in New York City")  

##Slide 38
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

##Slide 39
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))

##Slide 40
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20))
## Fit a simple linear regression model
model <- lm(Ozone ~ Wind, airquality)
## Draw regression line on plot
abline(model, lwd = 2)

#Slide 41
par(mfrow = c(1, 2))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})

##Slide 42
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
  plot(Temp, Ozone, main = "Ozone and Temperature")
  mtext("Ozone and Weather in New York City", outer = TRUE)
})

##########-------------------Chapter 11-----------------------###########

##Slide 47
#Load built in "CO2" dataset
data("CO2")
#Subset to first 21 observations
CO2=CO2[(1:21),]
#Set R to return to displaying one plot on the screen
par(mfrow=c(1,1))
with(CO2,plot(conc,uptake))
with(CO2,plot(conc,uptake,col=Plant))

##Slide 49
#Display all available built in colors
colors()
#Add color to the histogram we made earlier
hist(airquality$Ozone, col = "red")
hist(airquality$Ozone, col = "maroon")
hist(airquality$Ozone, col = "steelblue")

##Slide 50
#Red
with(CO2,plot(conc,uptake,col=rgb(1,0,0)))
#Green
with(CO2,plot(conc,uptake,col=rgb(0,1,0)))
#Blue
with(CO2,plot(conc,uptake,col=rgb(0,0,1)))
#Custom
with(CO2,plot(conc,uptake,col=rgb(0.4,0,.3)))

##Slide 52
library(RColorBrewer)
display.brewer.all()

##Slide 53
cols <- brewer.pal(3, "BuGn")
cols
image(volcano,col=cols)
pal <- colorRampPalette(cols)
pal(20)
image(volcano, col = pal(20))

##Slide 54
set.seed(1)
x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x, y)

##Slide 55
#Create plot without transaparency
set.seed(2)
x <- rnorm(2000)
y <- rnorm(2000)
plot(x, y, pch = 19)

##Slide 56
#Add transaparency
plot(x, y, pch = 19, col = rgb(0, 0, 0, 0.15))
