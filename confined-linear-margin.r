calculate_and_render <- function(lowerBound, upperBound, lowerMarginLimit, upperMarginLimit, sampleWindowWidth) {
  calc <- function(lowerBound, upperBound, lowerMarginLimit, upperMarginLimit, sampleWindowWidth) {
    activeWidth <- upperBound - lowerBound
    activeMargin <- upperMarginLimit - lowerMarginLimit
    rateOfChange <- activeMargin / activeWidth
    sectionSize <- upperBound - sampleWindowWidth
    marginComplement <- sectionSize * rateOfChange
    pixelConversion <- 16L
    calculatedMarginInPixels <- (upperMarginLimit * pixelConversion) - (sectionSize * rateOfChange * pixelConversion)
    calculatedMarginInRem <- calculatedMarginInPixels / pixelConversion
    
    return(
      c(lowerBound = lowerBound,
        upperBound = upperBound,
        lowerMarginLimit = lowerMarginLimit,
        upperMarginLimit = upperMarginLimit,
        sampleWindowWidth = sampleWindowWidth,
        activeWidth = activeWidth,
        activeMargin = activeMargin,
        rateOfChange = rateOfChange,
        sectionSize = sectionSize,
        marginComplement = marginComplement,
        calculatedMarginInPixels = calculatedMarginInPixels,
        calculatedMarginInRem = calculatedMarginInRem)
    )
  }
  
  result <- calc(
    lowerBound = lowerBound,
    upperBound = upperBound,
    lowerMarginLimit = lowerMarginLimit,
    upperMarginLimit = upperMarginLimit, 
    sampleWindowWidth = sampleWindowWidth
  )
  
  format_result <- function(result) {
    formatted_result <- sapply(result, function(x) format(x, scientific = FALSE))
    return(formatted_result)
  }
  
  formatted_result <- format_result(result)
  print(formatted_result)
  
  lim <- c(-25, 25)
  plot(NA, xlim = lim, ylim = lim, xlab = "Active Width", ylab = "Change in Margin", main = "Confined Linear Margin")
  grid()
  points(0, 0, pch = 19)
  
  slope <- result["rateOfChange"]
  x <- c(-50, 50)
  y <- c(-(50 * slope), 50 * slope)
  lines(x, y)
}

calculate_and_render(
  lowerBound = 380,
  upperBound = 1920,
  lowerMarginLimit = 3,
  upperMarginLimit = 24, 
  sampleWindowWidth = 1000
)