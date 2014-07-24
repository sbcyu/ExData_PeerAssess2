#----------Q2----------
#Have total emissions from PM2.5 decreased in the Baltimore City,
#Maryland (fips == "24510") from 1999 to 2008?
#Use the base plotting system to make a plot answering this question.

#Fetch the data
NEI <- readRDS("summarySCC_PM25.rds")
years <- c(1999,2002,2005,2008)

#Filter the data to get just Baltimore City data
baltimoreData = subset(NEI, fips == "24510")

#The Sweetest Part, LETS RENDER the IMAGE!!!!
png("plot2.png", width=614, height=380)
par(mfcol=c(1,2), 
    oma=c(0,0,1,0))

with(baltimoreData, {
  totalBaltimoreEmissionsPerYearSummed <- 
    tapply(baltimoreData$Emissions, baltimoreData$year, sum)
  
  plot(years,totalBaltimoreEmissionsPerYearSummed, 
       main="Data Summed per Year",
       xlab="Year", xaxt="n",
       ylab = "Emissions (in tons)",
       col="blue")
  axis(side = 1, at=years)
  fit <- lm(as.numeric(totalBaltimoreEmissionsPerYearSummed) ~ years)
  abline(fit,col="red")
  
  totalBaltimoreEmissionsPerYearMean <- 
    tapply(baltimoreData$Emissions, baltimoreData$year, mean)
  
  plot(years,totalBaltimoreEmissionsPerYearMean, 
       main="Data Averaged(mean) perYear",
       xlab="Year", xaxt="n",
       ylab = "Emissions (in tons)",
       col="blue")
  axis(side = 1, at=years)
  fit <- lm(as.numeric(totalBaltimoreEmissionsPerYearMean) ~ years)
  abline(fit,col="red")
})

mtext("Total Emission per Year for Baltimore City, Maryland",
      NORTH<-3, line=0, adj=0.5, cex=1.2, col="red", outer=TRUE)

dev.off()

