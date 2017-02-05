#' Produce png file containing time series line plots of the 3 sub-meterings.
#' 
#' @param dataPath  Path to the data source file household_power_consumption.txt.
#' @return  No return value;  plot written to png file in code directory.
#' @examples
#' plot3("../Data")
plot3 <- function(dataPath) {
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
    
    
    par( mfcol = c(1, 1) )
    
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
            col = c("black", "red", "blue"),
            legend = c( "Sub_metering_1",
                        "Sub_metering_2",
                        "Sub_metering_3" ) )
    
    
    f <- paste(outpath,
               "plot3.png",
               sep = "/")
    dev.copy(device = png,
             filename = f)
    dev.off()
    
}