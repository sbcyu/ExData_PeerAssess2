#----------Q3----------
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
#Which have seen increases in emissions from 1999–2008?
#Use the ggplot2 plotting system to make a plot answer this question.
require(ggplot2)
#fetch the data
NEI <- readRDS("summarySCC_PM25.rds")
years <- c(1999,2002,2005,2008)
baltimoreData = subset(NEI, fips == "24510")

#transform the data
require(plyr)
summaryByTypeSum <- ddply(baltimoreData, c("type", "year"),
                          summarise, typeSum = sum(Emissions))

summaryByTypeMean <- ddply(baltimoreData, c("type", "year"),
                           summarise, typeMean = mean(Emissions))

#prepare the plots by encapsulating them via a variable call
require(gridExtra) #this library helps in rendering multiple gglot plots in one image
sumPlot <- ggplot(summaryByTypeSum , 
                  aes(x=year, y=typeSum, colour=type)) + 
                  geom_line() + geom_point(size = 4) +
                  scale_x_continuous(breaks=years) + 
                  xlab("Years") + ylab("Emissions") + 
                  labs(colour = "Source Type\n(s)") +
                  ggtitle("Summed by Year")
  
meanPlot <- ggplot(summaryByTypeMean , 
                   aes(x=year, y=typeMean, colour=type)) + 
                  geom_line() + geom_point(size = 4) +
                  scale_x_continuous(breaks=years) +
                  labs(x = "Years" , y = "Emissions") +
                  ggtitle("Averaged(mean) by Year") + 
                  theme(legend.position="none")

#The Sweetest Part, LETS RENDER the IMAGE!!!!
png("plot3.png", width=614, height=380)
grid.arrange(sumPlot, meanPlot, ncol=2,
             main = "Total Emission per Year for Baltimore City by Source Type")
dev.off()