library(readr)

cars2018 <- read_csv("data/cars2018.csv")

#getting all the vehicles with manual transmissions

library(dplyr) # make dplyr functions available 
manual <- filter(cars2018, Transmission == "Manual") 

## getting all cars with 4 or more cylinders

cyl_4 <- filter(cars2018, Cylinders >= 4)


### getting cars with manual transmission and 4 or more cylinders

manual_cyl4 <- filter(cars2018, Transmission == "Manual", Cylinders >= 4)


auto_l6 <- filter(cars2018, Transmission == "Automatic", Gears < 6)


### only keep Model, MPG, Transmission and Cylinders

cars_narrow <- select(cars2018, Model, MPG, Transmission, Cylinders)


## remove ID, Intake and Exhaust 

cars_alt <- select(cars2018, -ID, -Intake, -Exhaust)



### sort by MPG 

cars_sorted <- arrange(cars2018, MPG)

cars_desc <- arrange(cars2018, desc(MPG))

## Sort by Transmission type and then by MPG
cars_transmpg <- arrange(cars2018, Transmission, desc(MPG))

### Question 
## start with the original data set (all employees)
## restrict to CVT transmission vehicles
## remove the FuelInjection, Aspiration and LTC columns
## order the data in descending order of MPG
## if cars have the same MPG order them by Model alphabetically

cvt <- filter(cars2018, Transmission == "CVT")
cvt <- select(cvt, -FuelInjection, -Aspiration, -LTC)
cvt <- arrange(cvt, desc(MPG), Model)


### using mutate to create a new variable if MPG >= 30 

cars_30 <- mutate(cars2018, above30 = if_else(MPG >= 30, TRUE, FALSE))


## How many cars are there in total and what is the average MPG?
report <- summarise(cars2018, NumCars = n(),
                    AvgMPG = mean(MPG))


## How many manual cars and what is the median MPG?

manual_report <- summarise(manual, NumCars = n(),
                           MedMPG = median(MPG))

### getting summary stats for each transmission type 

grouped_cars <- group_by(cars2018, Transmission)

transmission_rpt <- summarise(grouped_cars, NumCars = n(), 
                              AvgMPG = mean(MPG))


## fill in the blanks and reorder to 
## 1. counts the number of cars of each transmission type with MPG >= 30
## 2. Rank the transmission types by the count (descending order)

df <- filter(cars2018, MPG >= 30)
df <- group_by(df, Transmission)
df <- summarise(df, Count = n())
answer <- arrange(df, desc(Count))




## Using pipelines to connect functions

## Shows the number of cars of each type and Avg MPG sorted by Avg MPG

transmission_rpt <- group_by(cars2018, Transmission) %>% 
  summarise(NumCars = n(), AvgMPG = mean(MPG)) %>% 
  arrange(desc(AvgMPG))

### Alternative way to do the above

trans_rpt2 <- cars2018 %>% group_by(Transmission) %>%
  summarise(NumCars = n(), AvgMPG = mean(MPG)) %>% 
  arrange(desc(AvgMPG))













