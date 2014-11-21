# This file is specifically for the Second course project in the 
# Exploratory Data Analysis Coursera Course.

# Written by: William Kashdan

setwd("C:\Users\will\Programming\R\ExploreDataProj")
set1 <- readRDS("summarySCC_PM25.rds")
set2 <- readRDS("Source_Classification_Code.rds")

# Question 1: Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, and 2008.

means <- tapply(set1$Emissions, set1$year, sum)
dfmeans <- as.data.frame(means)
png(file = "plot1.png")
plot(rownames(means), dfmeans$means, type="l", xlab="Years", ylab="Total Emissions")
abline(lm(dfmeans$means ~ as.numeric(rownames(means))), col="red")
title(main = "Total Emissions from 1999 - 2008 in the US")
dev.off()

# Question 2: Have total emissions from PM2.5 decreased in the 
# Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

baltData <- set1[(set1$fips == "24510"),]
baltMeans <- tapply(baltData$Emissions, baltData$year, sum)
dfBaltMeans <- as.data.frame(baltMeans)
png(file = "plot2.png")
plot(rownames(dfBaltMeans), dfBaltMeans$baltMeans, type="l", xlab="Years", ylab="Total Emissions in Baltimore City, Maryland")
abline(lm(dfBaltMeans$baltMeans ~ as.numeric(rownames(dfBaltMeans))), col="red")
title(main = "Total Emissions from 1999 - 2008 in the Baltimore City")
dev.off()

# Question 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.

# X-axis is sum of emissions, y-axis is years, four graphs on type
library(reshape2)
library(ggplot2)

baltTypeData <- data.frame(baltData$Emissions, baltData$type, baltData$year)
colnames(baltTypeData) <- c("emissions", "type", "year")
dataMelt <- melt(baltTypeData, id=c("year", "type"))
emissionsByYearAndType <- dcast(dataMelt, year+type~variable, sum)
png(file = "plot3.png")
qplot(year, emissions, data = emissionsByYearAndType, facets = .~type, geom = "line") + geom_smooth(method='lm')
dev.off()

# Question 4: Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

set3 <- merge(set1, set2) #very intensive operation, might be easier to only merge necessary columns
coalData <- set3[grepl("coal", as.character(set3$EI.Sector)),]
coalMeans <- tapply(coalData$Emissions, coalData$year, sum)
dfCoalMeans <- as.data.frame(coalMeans)
png(file = "plot4.png")
plot(rownames(dfCoalMeans), dfCoalMeans$coalMeans, type="l", xlab="Years", ylab="Total Coal Combustion Emissions")
abline(lm(dfCoalMeans$coalMeans ~ as.numeric(rownames(dfCoalMeans))), col="red")
title(main = "Coal- Combustion Related Emissions in the US")
dev.off()

# Question 5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

baltMerge <- merge(baltData, set2)
vehicleBaltData <- baltMerge[grepl("Vehicle", as.character(baltMerge$Short.Name)),]
vehicleBaltMeans <- tapply(vehicleBaltData$Emissions, vehicleBaltData$year, sum)
dfVehicleBaltMeans <- as.data.frame(vehicleBaltMeans)
x <- as.numeric(rownames(dfVehicleBaltMeans))
y <- dfVehicleBaltMeans$vehicleBaltMeans
plot(x, y, type="l", xlab="Years", ylab="Total Vehicle Emissions in Baltimore City")
abline(lm(y ~ x), col="red")


# Question 6: Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

baltAndLA <- subset(set1, xor(set1$fips == "06037", set1$fips == "24510"))
mergedBaltAndLA <- merge(baltAndLA, set2)
condensed <- data.frame(mergedBaltAndLA$fips, mergedBaltAndLA$year, mergedBaltAndLA$Emissions)
colnames(condensed) <- c("fips", "year", "emissions")
baltLAMelt <- melt(condensed, id=c("year", "fips"))
baltLAEmissionsByYear <- dcast(BaltLAMelt, year+fips~variable, sum)
qplot(year, emissions, data = baltLAEmissionsByYear, geom = "line") + geom_point() + facet_grid(. ~ fips ) + geom_smooth(method='lm')