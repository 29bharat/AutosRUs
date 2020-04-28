# Import tidyverse
library(tidyverse)

## Challenge part 1

# Import mechcar_mpg csv dataset - MechaCar _ MPG
MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv',stringsAsFactors = F)

# Generate multiple linear regression model
lm <- lm(mpg ~ vehicle.length + vehicle.weight +	spoiler.angle	+ ground.clearance + AWD,data=MechaCar_mpg) 
summary(lm)


## Challenge part 2 - Suspension Coil
# Import data from the csv file
Suspension_Coil <- read.csv(file='Suspension_Coil.csv',stringsAsFactors = F)

# Summary to include mean, median, var and standard deviation
summary(Suspension_Coil$PSI)
var(Suspension_Coil$PSI)
sd(Suspension_Coil$PSI)

summary_by_lot <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean_lot=mean(PSI), variance_lot = var(PSI), SD_lot = sd(PSI))
summary_by_lot

## Challenge part 3 - suspension coil t-test
t.test(Suspension_Coil$PSI, mu = 1500)

## For Design your own study
cor(mtcars$mpg, mtcars$hp)
cor(mtcars$mpg, mtcars$cyl)
