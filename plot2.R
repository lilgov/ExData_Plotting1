#' Produce png file containing time series line plot of global active power.
#' 
#' @param dataPath  Path to the data source file household_power_consumption.txt.
#' @return  No return value;  plot written to png file in code directory.
#' @examples
#' plot2("../Data")
plot2 <- function(dataPath) {
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
          myData$Global_active_power,
          type = "l",
          xlab = "",
          ylab = "Global Active Power (kilowatts)" )
    
    
    f <- paste(outpath,
               "plot2.png",
               sep = "/")
    dev.copy(device = png,
             filename = f)
    dev.off()
    
}