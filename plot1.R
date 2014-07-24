#----------Q1----------
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
#Using the base plotting system, make a plot showing the total PM2.5 emission
#from all sources for each of the years 1999, 2002, 2005, and 2008.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
years <- c(1999,2002,2005,2008)

#The Sweetest Part, LETS RENDER the IMAGE!!!!
png(file="plot1.png", width=614, height=380)
par(mfcol=c(1,2), 
    oma=c(0,0,1,0))

with(NEI, {
  totalEmissionsPerYearSummed <- tapply(NEI$Emissions, NEI$year, sum)
  plot(years, totalEmissionsPerYearSummed , xlab="Year", xaxt="n",
       ylab="USA Total emissions of PM2.5 per year",
       main="Summed by Year",
       col="blue")
  axis(side = 1, at=years)
  fit <- lm(as.numeric(totalEmissionsPerYearSummed) ~ years)
  abline(fit,col="red")
  
  totalEmissionsPerYearMean <- tapply(NEI$Emissions, NEI$year, mean)
  plot(years, totalEmissionsPerYearMean , xlab="Year", xaxt="n",
       ylab="USA Total emissions of PM2.5 per year",
       main="Averaged(mean) by Year",
       col="blue")
  axis(side = 1, at=years)
  fit <- lm(as.numeric(totalEmissionsPerYearMean) ~ years)
  abline(fit,col="red")
})

mtext("Total Emission per Year",
      NORTH<-3, line=0, adj=0.5, cex=1.2, col="red", outer=TRUE)

dev.off()