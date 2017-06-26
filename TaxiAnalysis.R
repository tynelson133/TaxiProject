##################### Capital One Data Challenge ############################
########### Question 1:
## A.  Programmatically download the "Green" Taxis dataset

# Using the function read.csv(), we need to put the URL from which the data
# comes from. Therefore, if you right click on the .csv file online and 
# and copy the Link address you can paste this in the function.
# I have named my dataset "tripdat"

tripdat <- read.csv('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv')

## B. Reporting the number of rows and columns. 

# The function dim() will give us the rows (first) and columns (second)
dim(tripdat)

########### Question 2:
## A. Plotting a histogram of the number of the "Trip Distance". 

# Before I plot the data I wanted to give an easy command to show you
# how to get the names of all the columns. From this we can see that 
# the name of the column we need is "Trip_distance"

colnames(tripdat)

# Now, I could just use the simple function hist() to construct a histogram,
# but for visual preference I will use the package 'ggplot2' and the function
# ggplot() to construct all plots.

install.packages('ggplot2')
library(ggplot2)

# At first glance when looking at the histogram we see that the data is severly
# skewed right and due to outliers the plot is not aesthetically pleasing to the eye

ggplot(data = tripdat , aes(x = Trip_distance)) + 
  geom_histogram()+ggtitle("Uncorrected Histogram of Trip Distance")
  
# Further investigation shows that there are just a few values that are extremely
# large compared to the rest of the data. Below we are using the tail() function, which
# takes the last n values of the vector specified, and the sort() function, which puts
# the vector in order from smallest to largest. Below is the 20 largest Trip distance
# values. 

tail(sort(tripdat$Trip_distance),20)

# Therefore, we will consider a plot with the middle 99% to try and get a better idea 
# of the shape of the data.

middlePercent <- 0.99

# Below is two values that specify the lower and upper cutoff values so that 
# we can remove them from the dataset. quantile() is a function used to 
# find the quantile value of interest from a vector. 

# Lower and Upper cutoff values based on middle percent

lowerCutoff <- quantile(tripdat$Trip_distance, (1 - middlePercent)/2)
upperCutoff <- quantile(tripdat$Trip_distance, (1 + middlePercent)/2)

# The which() function below sifts through the vectors supplied 
# to find the exact location of the argument provided. Here we 
# find all the trip distance values that are equal to or below/above the 
# cutoff values expressed above. Note "|" is the OR command.

# Labeling which values should be removed from data

valsNeedRemoved <- which(tripdat$Trip_distance <= lowerCutoff |
                       tripdat$Trip_distance >= upperCutoff)

# Now we remove the entries that meet the criteria above by
# using the "-" function.

# Reduced data with middlePercent values left

NewDat <- tripdat[-valsNeedRemoved,]

# New plot with the extreme values removed 

ggplot(data = NewDat, aes(x = Trip_distance)) + 
  geom_histogram() +ggtitle("Corrected Histogram of Trip Distance")

# Notes:
# 1. The data apears to be heavily skewed to the right.
# 2. There does appear to be some very large values (603.1 is the largest value followed
#     by 246.28) that make it hard to plot a histogram with the proper number of bins
#     to see anything worth while.
# 3. If all you are after is the overall shape of the data you can remove a small piece 
#    of the outliers on each end, as we did above, to get a good understanding how
#    "most" of the data is acting.
# 4. It is important for one to note that some extreme values had been removed when 
#    presenting or writing up the results if the second plot is used.


########### Question 3: 
## A. Reporting of Mean and median trip distance by hour of day.
# Because the data is very "clean" we are able to use a simplistic function to extract
# hours of the day in which a pickup time happened. The steps are as follows:
# 1. We must find the variable that is dealing with pickup time, lpep_pickup_datetime.
# 2. Now we must look at the structure of the data in this vector

str(tripdat$lpep_pickup_datetime)

# 3. We need to find a pattern that each datapoint has. For this particular data we
#    see that there is a space after the date, therefore we can use this to our
#    advantage when trying to get a value for hour of the day they were picked up.
# 4. Now using the package "stringr" and the function str_extract() we can create a 
#    new column name, "Hour", and add it to the datafram tripdat.

install.packages("stringr")
library(stringr)

# This allows us to find the pattern that we have in each string

Pattern <- "[ ]([0-9][0-9])"

# We first have to convert the 'lpep_pickup_datetime' variable to a character
# Then once the str_extract() function works we convert all the back to factors

tripdat$Hour <- as.factor(str_extract(as.character(tripdat$lpep_pickup_datetime), Pattern))

# 5. Now we will get the mean and median at each hour of day.
# Using the sapply() function we are able to get the mean and distance traveled
# at each hour of the day. We do this by once again using the which() function
# to subset the trip distance at each hour.

MeanDistance <- sapply(levels(tripdat$Hour), function(x){
                mean(tripdat$Trip_distance[which(tripdat$Hour==x)])
                })

MedianDistance <- sapply(levels(tripdat$Hour), function(x){
                  median(tripdat$Trip_distance[which(tripdat$Hour==x)])
                  })

install.packages("reshape2")
library(reshape2)

# Combining the two sets into a data.frame and then melting them together to
# make it easier to use in ggplot. We use the function melt() from the reshape2 package
# to complete this task

Results <- data.frame("Hours" = 0:23,MeanDistance, MedianDistance)
StackDat <- melt(Results, id.vars = "Hours")

# Plotting each of the vectors on the same plot in R

ggplot(StackDat, aes(x = Hours, y = value, group = variable,
                     color = variable)) + geom_line()+
                ggtitle("Plot of Mean and Median Trip Distance across Time")
                
## B. Count of how many transactions fit starting or finishing at an NYC airport.
# To get a "rough" sense of the number of people going to one airport in NYC
# using a "green" taxi I have located a variable, 'RateCodeID', that can be 5 different
# numbers. The meaning of these numbers can be explained in more detail at
# http://www.nyc.gov/html/tlc/html/passenger/taxicab_rate.shtml#AirportTrips, but 
# when the ID is equal to 2 that means someone was picked up To/From JFK airport.
# Thus, I have subsetted this data below. 

JFKdat <- tripdat[which(tripdat$RateCodeID == 2),]

# 1. The number of of transactions are

dim(JFKdat)[1]

# 2. The average fair is

mean(JFKdat$Fare_amount)
mean(tripdat$Fare_amount)

########### Question 4: 

## A. Build a derived variable for tip as a percentage of fair.
# Take the tip amount divided by the fare amount

tripdat$tip_percent <- tripdat$Tip_amount/tripdat$Fare_amount

###### Stipulations
# Removing any extreme outliers from the tip percent variable, NA values, and negative 
# values. The oultier detection is just the basic tool of adding and 
# Subtracting 1.5*IQR from Q1 and Q3.

LowerBound <- quantile(tripdat$tip_percent, 0.25, na.rm = TRUE) - 
              1.5*IQR(tripdat$tip_percent, na.rm = TRUE)
UpperBound <- quantile(tripdat$tip_percent, 0.75, na.rm = TRUE) + 
              1.5*IQR(tripdat$tip_percent, na.rm = TRUE)

OutlierRemoved <- which(tripdat$tip_percent > UpperBound | tripdat$tip_percent < 0 |
                          is.na(tripdat$tip_percent))

# Removing longitude values specified as zero
longRemoved <- which(tripdat$Pickup_longitude == 0 |
                              tripdat$Dropoff_longitude == 0)

# Removing latitude values specified as zero
latRemoved <- which(tripdat$Pickup_latitude == 0 |
                              tripdat$Dropoff_latitude == 0)
# Removing payment methods other than cash or credit so we can use this as
# a predictor in the model.
payRemoved <- which(tripdat$Payment_type %in% 3:6)

# If tip is negative we want to remove it
NoTipRemoved <- which(tripdat$Tip_amount < 0)

# If you have either 0, 7, 8, 9 people in your car we remove you.
PeopleRemoved <- which(tripdat$Passenger_count %in% c(0,7,8,9))

# Removing the trip distances larger than 100 miles
DistRemoved <- which(tripdat$Trip_distance == 0 | tripdat$Trip_distance >= 100)

# Making a vector of the observations that need to be removed based on the criteria above
RemovedObs <- unique(c(longRemoved, latRemoved, payRemoved, NoTipRemoved,
                       OutlierRemoved, DistRemoved))

# Variables to be considered to be implemented in the model

varsConsidered <- c("tip_percent", "Pickup_longitude", "Pickup_latitude", 
                    "Dropoff_longitude","Dropoff_latitude", 
                    "Trip_distance", "Hour", "Payment_type", "Fare_amount")

# A simple way to test if two variables are highly correlated leading to the
# issue of multicollinearity we take a look at the simple correlation of all
# the variables. 

test_dat      <- na.omit(tripdat[-RemovedObs, varsConsidered])
test_dat$Hour <- as.numeric(as.character(test_dat$Hour))
Correlation   <- cor(test_dat)

# As suspected, the two different latitude and longitude values are highly correlated
# with one another therefore, I have selected the one that is most highly correlated
# with tip_percent. Also, Fare_amount anf Trip_distance are highly correlated (no surprise)
# therefore we once again keep the one that is most highly correlated with tip_percent
# which happens to be trip_distance. 

# Thus we have the left over variables as follows

varsUsed <- c("tip_percent", "Pickup_longitude",
              "Dropoff_latitude", "Trip_distance", "Payment_type", "Hour")

# Our new dataset 
trip_subset <- na.omit(tripdat[-RemovedObs, varsUsed])
trip_subset$Payment_type <- as.factor(trip_subset$Payment_type)
trip_subset$Hour <- as.numeric(as.character(trip_subset$Hour))

# Fitting tip percent with all the variables in linear regression
fit <- lm(tip_percent ~ . , data = trip_subset)

# A summary of the linear model
summary(fit)

install.packages("MASS")
library(MASS)

# Step-wise variable selection
# This is not a great tool for large datasets, but is a quick way to see
# if a variable should be removed. When this is done, the model selected is
# the one with all the variables (not a surprise)
step <- stepAIC(fit, direction="both")

step$anova # display results

install.packages("mfp")
library(mfp)
# Using multivariable fractional polynomial to select a model.

f <- mfp(tip_percent ~ fp(Trip_distance) + fp(Pickup_longitude) + fp(Dropoff_latitude) +
           Payment_type + Hour, data = trip_subset,   family = gaussian, alpha=0.05, 
         verbose = TRUE, na.action = na.exclude)
# Estimates from the model
summary(f)

# I would select the linear regression model just due to the simplicity
# and understanding of the model. 

###################### Question 5.
locFunc <- function(dat, lon, lat, miles, time = "Not Specified"){
  
  # Making a circumference around the specified longitude and latitude values
  # The 69 and 54.6 came from the internet on the amount of miles between one
  # degree of longitude and latitude values, respectively. The radius is the 
  # specified number of miles that you want to consider. 
  lonCutoff <- miles/69
  lowerLon  <- lon - lonCutoff
  upperLon  <- lon + lonCutoff
  latCutoff <- miles/54.6
  lowerLat  <- lat - latCutoff
  upperLat  <- lat + latCutoff
  
  # If you do not specify a longitude and latitude value
  # within the range of the data an error message will be
  # returned
  if(lon < min(dat$Pickup_longitude) |
       lon > max(dat$Pickup_longitude) |
       lat < min(dat$Pickup_latitude) |
       lat > max(dat$Pickup_latitude)){
    stop("The specified Longitude and Latitude do not fall in the range of 
         the data")
  }
  
  if(time == "Not Specified"){
  # Now we only count the number of people within the radius specified
  # using the function which()
  NumberInRange <- length(which(dat$Pickup_longitude >= lowerLon & 
                                dat$Pickup_longitude <= upperLon &
                                dat$Pickup_latitude >= lowerLat & 
                                dat$Pickup_latitude <= upperLat))
  }
  
  else{
    if(time < 0 | time > 23)stop("A Proper Hour for time was not specified")
  NumberInRange <- length(which(dat$Pickup_longitude >= lowerLon & 
                                    dat$Pickup_longitude <= upperLon &
                                    dat$Pickup_latitude >= lowerLat & 
                                    dat$Pickup_latitude <= upperLat &
                                    as.numeric(dat$Hour) == time))
    }
  
  return(NumberInRange)
  
}
## A. Using the function without the time aspect.
# 1. dat = data you want to consider (for simplicity I used the same data that I
#          created in Question 4 (test_dat) because it already had the missing values removed.)
# 2. lon = longitude value you want to specifty
# 3. lat = latitude value you want to specify
# 4. miles = the radius from the longitude and latitude you want to consider
locFunc(dat = test_dat, lon = -73.9, lat = 40.7, miles = 1)
## B. Bringing in time aspect to consider
# 1. time = the hour of the day you want to consider. This must be numeric
#           from 0 to 23.
# 2. This does not need to be specified for the function to work.
# 3. One more complexity could bring in minutes to the problem, but I only used
#    the variable I constructed for Question 3.

locFunc(dat = test_dat, lon = -73.9, lat = 40.7, miles = 1, time = 7)
