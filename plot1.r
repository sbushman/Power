#MOST OF THIS DOCUMENT IMPORTS AND PREPARES THE ORIGINAL DATA.  GENERATING THE PLOT ITSELF OCCURS ONLY IN THE FINAL LINES OF THE DOCUMENT

Power <- read.csv('exdata_data_household_power_consumption/household_power_consumption.txt')

library('tidyr')
library('lubridate')
library('dplyr')
library('ggplot2')
library('lattice')

#These are what I want to name my variables.
Powernames <- c('Date',
                'Time',
                'Global_active_power', 
                'Global_reactive_power', 
                'Voltage', 
                'Global_intensity', 
                'Sub_metering_1', 
                'Sub_metering_2', 
                'Sub_metering_3')

Power1 <- separate(Power, col = 1, into = Powernames, sep = ";")
Power1 <- Power1 %>% mutate(DateTime = paste(Date, Time))

#The power of coercion is strong:
Power1$Date <- as.Date(Power1$Date, "%d/%m/%Y")
Power1$Time <- hms(Power1$Time)
Power1$Global_active_power <- as.numeric(Power1$Global_active_power)
Power1$Global_reactive_power <- as.numeric(Power1$Global_reactive_power)
Power1$Voltage <- as.numeric(Power1$Voltage)
Power1$Global_intensity <- as.numeric(Power1$Global_intensity)
Power1$Sub_metering_1 <- as.numeric(Power1$Sub_metering_1)
Power1$Sub_metering_2 <- as.numeric(Power1$Sub_metering_2)
Power1$Sub_metering_3 <- as.numeric(Power1$Sub_metering_3)
Power1$DateTime <- dmy_hms(Power1$DateTime)

#The time frame in question:
ThePower <- Power1 %>%
  filter(Date >= '2007-02-01' & Date <= '2007-02-02')

#I'M USING BASE PLOTTING BECAUSE I NEED THE PRACTICE IN THAT SCHEME

#PLOT 1
#Using base r

hist(ThePower$Global_active_power, col = 'red', main = 'Global Active Power', xlab = 'Global Active Power (kilowatts)', ylab = 'Frequency')
dev.copy(png, file = 'plot1.png')
dev.off()