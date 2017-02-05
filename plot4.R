#' Produce png file containing 2x2 matrix of plots of power consumption data.
#' 
#' @param dataPath  Path to the data source file household_power_consumption.txt.
#' @return  No return value;  plot written to png file in code directory.
#' @examples
#' plot4("../Data")
plot4 <- function(dataPath) {
    outpath <- getwd()
    
    f <- paste(dataPath,
               "household_power_consumption.txt",
               sep = "/")
    
    temp <- read.table(f, sep = ";", nrows = 1, header = TRUE)
    columnHeadings <- names(temp)
    
    a <- grep('1/2/2007', readLines(f))[1]
    b <- grep('3/2/2007', readLines(f))[1]
    
    myData <- read.table(f,
                         sep = ";",
                         skip = a-2,
                         nrows = b-a,
                         header = TRUE)
    
    names(myData) <- columnHeadings
    
    ts <- strptime( paste(myData$Date, myData$Time),
                    format = "%e/%m/%Y %H:%M:%S")
    datetime <- as.POSIXct(ts)
    rm(ts)
    
    
    par( mfcol = c(2, 2) )
    
    ## top-left plot
    plot( datetime,
          myData$Global_active_power,
          type = "l",
          xlab = "",
          ylab = "Global Active Power (kilowatts)" )
    
    ## bottom-left plot
    plot( datetime,
          myData$Sub_metering_1,
          type = "l",
          col = "black",
          xlab = "",
          ylab = "Energy sub metering" )
    
    lines( datetime,
           myData$Sub_metering_2,
           col = "red" )
    
    lines( datetime,
           myData$Sub_metering_3,
           col = "blue" )
    
    legend( "topright",
            lty =  c(1, 1, 1),
            bty = "n",
            col = c("black", "red", "blue"),
            legend = c( "Sub_metering_1",
                        "Sub_metering_2",
                        "Sub_metering_3" ),
            cex = 0.5 )
    
    ## top-right plot
    plot( datetime,
          myData$Voltage,
          type = "l",
          ylab = "Voltage" )
    
    
    ## bottom-right plot
    plot( datetime,
          myData$Global_reactive_power,
          type = "l",
          ylab = "Global_reactive_power" )
    
    
    
    f <- paste(outpath,
               "plot4.png",
               sep = "/")
    dev.copy(device = png,
             filename = f)
    dev.off()
    
    par( mfcol = c(1, 1) )
    
}