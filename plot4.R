#----------Q4----------
#Across the United States, how have emissions from coal combustion-related sources
#changed from 1999–2008?
#Fetch the Raw Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
years <- c(1999,2002,2005,2008)

#Filter the data with sources that are coal Combustion related
coalSCC <- SCC[grep("Coal", SCC$EI.Sector, ignore.case=T),]
coalEmissionData <- subset(NEI, SCC %in% coalSCC$SCC)
rm(coalSCC)

#compute the summed and mean values
require(plyr)
summaryCoalSum <- ddply(coalEmissionData, c("year"),
                          summarise, typeSum = sum(Emissions))

summaryCoalByTypeSum <- ddply(coalEmissionData, c("type", "year"),
                        summarise, typeSum = sum(Emissions))

summaryCoalMean <- ddply(coalEmissionData, c("year"),
                           summarise, typeMean = mean(Emissions))

summaryCoalByTypeMean <- ddply(coalEmissionData, c("type", "year"),
                         summarise, typeMean = mean(Emissions))

#prepare the plots by encapsulating them via a variable call
require(ggplot2)
require(gridExtra) #this library helps in rendering multiple gglot plots in one image
sumCoalPlot <- ggplot(summaryCoalSum, aes(x= year, y = typeSum)) +
                  geom_line(colour = "red") + geom_point(size = 4, shape = 21) +
                  scale_x_continuous(breaks=years) +
                  xlab("Years") + ylab("Emissions") + 
                  ggtitle("Summed by Year")
  
meanCoalPlot <- ggplot(summaryCoalMean, aes(x= year, y = typeMean)) +
  geom_line(colour = "red") + geom_point(size = 4, shape = 21) +
  scale_x_continuous(breaks=years) +
  xlab("Years") + ylab("Emissions") + 
  ggtitle("Averaged(mean) by Year")

sumCoalTypePlot <- ggplot(summaryCoalByTypeSum , 
                  aes(x=year, y=typeSum, colour=type)) + 
  geom_line() + geom_point(size = 4) +
  scale_x_continuous(breaks=years) + 
  xlab("Years") + ylab("Emissions") + 
  labs(colour = "Source Type\n(s)") +
  ggtitle("Summed by Source and Year")

meanCoalTypePlot <- ggplot(summaryCoalByTypeMean , 
                   aes(x=year, y=typeMean, colour=type)) + 
  geom_line() + geom_point(size = 4) +
  scale_x_continuous(breaks=years) +
  labs(x = "Years" , y = "Emissions") +
  ggtitle("Averaged(mean) by Source and Year") + 
  theme(legend.position="none")

#The Sweetest Part, LETS RENDER the IMAGE!!!!
png("plot4.png", width=614, height=380)
grid.arrange(sumCoalPlot, meanCoalPlot, sumCoalTypePlot, meanCoalTypePlot,
             ncol=2, nrow= 2,
             main = "Total Emission per Year for Coal Combustion related Source Types")
dev.off()