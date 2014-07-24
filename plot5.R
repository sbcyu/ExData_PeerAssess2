#----------Q5----------
#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
#Definition of VEHICLE came from http://www.merriam-webster.com/dictionary/vehicle
#The definition is the one which guided me in infering what to filter out in the dataset.

#Fetch the Raw Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
years <- c(1999,2002,2005,2008)

#Filter the data to retrieve only the data corresponding to Mobile/Vehicle 
#I choose to use "Mobile" as my keyword. 
#Others would have used "Vehicle" as their keyword. 
#Just a matter of preference/taste I guess. ^^;
baltimoreData = subset(NEI, fips == "24510")
motorSCC <- SCC[grep("Mobile", SCC$SCC.Level.One, ignore.case=T),]
motorEmissionData <- subset(baltimoreData, SCC %in% motorSCC$SCC)

#compute the summed and mean values
require(plyr)
summaryMotorSum <- ddply(motorEmissionData, c("year"),
                         summarise, Sum = sum(Emissions))

summaryMotorByTypeSum <- ddply(motorEmissionData, c("type", "year"),
                              summarise, typeSum = sum(Emissions))

summaryMotorMean <- ddply(motorEmissionData, c("year"),
                          summarise, Mean = mean(Emissions))

summaryMotorByTypeMean <- ddply(motorEmissionData, c("type", "year"),
                                summarise, typeMean = mean(Emissions))

#prepare the plots by encapsulating them via a variable call
require(ggplot2)
require(gridExtra) #this library helps in rendering multiple gglot plots in one image
sumMotorPlot <- ggplot(summaryMotorSum, aes(x = year, y = Sum)) +
                       geom_line(colour = "red") + geom_point(size = 4, shape = 21) +
                       scale_x_continuous(breaks = years) +
                       xlab("Years") + ylab("Emissions") + 
                       ggtitle("Summed by Year")

meanMotorPlot <- ggplot(summaryMotorMean, aes(x = year, y = Mean)) +
                        geom_line(colour = "red") + geom_point(size = 4, shape = 21) +
                        scale_x_continuous(breaks = years) +
                        xlab("Years") + ylab("Emissions") +
                        ggtitle("Averaged(mean) by Year")

sumMotorByTypePlot <- ggplot(summaryMotorByTypeSum,
                             aes(x = year, y = typeSum, colour = type)) +
                             geom_line() + geom_point(size = 4) +
                             scale_x_continuous(breaks = years) +
                             xlab("Years") + ylab("Emissions") +
                             labs(colour = "Source Type\n(s)") +
                             ggtitle("Summed by Source and Year")

meanMotorByTypePlot <- ggplot(summaryMotorByTypeMean,
                              aes(x = year, y = typeMean, colour = type)) +
                              #position dodge will help in overlapping points and lines 
                              geom_line(position= position_dodge(0.2)) + 
                              geom_point(position= position_dodge(0.2), size = 4) +
                              scale_x_continuous(breaks = years) + 
                              labs(x = "Years", y = "Emissions") + 
                              ggtitle("Averaged(mean) by Source and Year") +
                              theme(legend.position = "none")

#The Sweetest Part, LETS RENDER the IMAGE!!!!
png("plot5.png", width=614, height=380)
grid.arrange(sumMotorPlot, meanMotorPlot, 
             sumMotorByTypePlot, meanMotorByTypePlot,
             ncol = 2, nrow = 2,
             main = "Total Emission per Year for Motor/Vehicle related Sources for Baltimore City")
dev.off()