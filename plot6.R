#----------Q6----------
#Compare emissions from motor vehicle sources in Baltimore City with emissions
#from motor vehicle sources in Los Angeles County, California (fips == "06037").
#Which city has seen greater changes over time in motor vehicle emissions?
#Fetch the Raw Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
years <- c(1999,2002,2005,2008)

#Filter the data to retrieve only the data corresponding to Mobile/Vehicle 
#I choose to use "Mobile" as my keyword. 
#Others would have used "Vehicle" as their keyword. 
#Just a matter of preference/taste I guess. ^^;
baltimoreData = subset(NEI, fips == "24510")
losAngelesData <- subset(NEI, fips == "06037")

motorSCC <- SCC[grep("Mobile", SCC$SCC.Level.One, ignore.case=T),]

baltimoreMotorEmissionData <- subset(baltimoreData, SCC %in% motorSCC$SCC)
losAngelesMotorEmissionData <- subset(losAngelesData, SCC %in% motorSCC$SCC)

baltimoreMotorEmissionData$fipsName <- "Baltimore"
losAngelesMotorEmissionData$fipsName <- "Los Angeles"

emissionData <- rbind(baltimoreMotorEmissionData, losAngelesMotorEmissionData)

#compute the summed and mean values
require(plyr)
summaryCompareSum <- ddply(emissionData, c("year", "fipsName"),
                           summarise, Sum = sum(Emissions))
  
summaryCompareMean <- ddply(emissionData, c("year", "fipsName"),
                            summarise, Mean = mean(Emissions))
  
summaryBaltimoreByTypeSum <- ddply(baltimoreMotorEmissionData, c("type","year"),
                                   summarise, typeSum = sum(Emissions))
  
summaryLosAngelesByTypeSum <- ddply(losAngelesMotorEmissionData, c("type", "year"),
                                    summarise, typeSum = sum(Emissions))

summaryBaltimoreByTypeMean <- ddply(baltimoreMotorEmissionData, c("type", "year"),
                                    summarise, typeMean = mean(Emissions))
  
summaryLosAngelesByTypeMean <- ddply(losAngelesMotorEmissionData, c("type", "year"),
                                     summarise, typeMean = mean(Emissions))
  
#prepare the plots by encapsulating them via a variable call
require(ggplot2)
require(gridExtra) #this library helps in rendering multiple gglot plots in one image
sumEmissionPlot <- ggplot(summaryCompareSum, aes(x = year, y = Sum, colour = fipsName)) +
                          geom_line() + geom_point(size = 4) +
                          scale_x_continuous(breaks = years) +
                          labs(x = "Years", y = "Emissions", colour = "Locations") +
                          ggtitle("Summed by Year")
                          

meanEmissionPlot <- ggplot(summaryCompareMean, aes(x = year , y = Mean, colour = fipsName)) + 
                          geom_line() + geom_point(size = 4) +
                          scale_x_continuous(breaks = years) +
                          labs(x = "Years", y = "Emissions") +
                          ggtitle("Averaged(mean) by Year") +
                          theme(legend.position = "none")
  
sumBaltimoreByTypePlot <- ggplot(summaryBaltimoreByTypeSum, 
                                 aes(x = year, y = typeSum, colour = type)) +
                                 geom_line() + geom_point(size = 4) +
                                 scale_x_continuous(breaks = years) +
                                 labs(x = "Years", y = "Emissions", colour = "Source Types") +
                                 ggtitle("Summed by Type and Year (Baltimore)")
  
sumLosAngelesByTypePlot <- ggplot(summaryLosAngelesByTypeSum,
                                  aes(x = year, y = typeSum, colour = type)) +
                                  geom_line() + geom_point(size = 4) +
                                  scale_x_continuous(breaks = years) +
                                  labs(x = "Years", y = "Emissions") +
                                  ggtitle("Summed by Type and Year (Los Angeles)") + 
                                  theme(legend.position = "none")

meanBaltimoreByTypePlot <- ggplot(summaryBaltimoreByTypeMean,
                                  aes(x = year, y = typeMean, colour = type)) +
                                  geom_line(position = position_dodge(0.2)) +
                                  geom_point(position = position_dodge(0.2), size = 4) +
                                  scale_x_continuous(breaks = years) +
                                  labs(x = "Years", y = "Emissions", colour = "Source Types") +
                                  ggtitle("Averaged(mean) by Type and Year (Baltimore)")

meanLosAngelesByTypePlot <- ggplot(summaryLosAngelesByTypeMean,
                                  aes(x = year, y = typeMean, colour = type)) +
                                  geom_line(position = position_dodge(0.2)) +
                                  geom_point(position = position_dodge(0.2), size = 4) +
                                  scale_x_continuous(breaks = years) + 
                                  labs(x = "Year", y = "Emissions") +
                                  ggtitle("Averaged(mean) by Type and Year (Los Angeles)") +
                                  theme(legend.position = "none")
  
#The sweetest Part, LETS RENDER the IMAGE!!!!
png("plot6.png", width=750, height=1000)
grid.arrange(sumEmissionPlot, meanEmissionPlot, 
             sumBaltimoreByTypePlot, sumLosAngelesByTypePlot,
             meanBaltimoreByTypePlot, meanLosAngelesByTypePlot,
             ncol = 2, nrow = 3,
             main = "Comparison of Motor/Vehicle Emissions between Baltimore and Los Angeles")
dev.off()