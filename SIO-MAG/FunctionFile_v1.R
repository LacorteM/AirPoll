Month2Season <- function(month) {
  ## month is an integer (1-12)
  ## a factor with levels {"DJF", "MAM", "JJA", "SON"} is returned
  seasons <- c("DJF", "MAM", "JJA", "SON")
  index <- findInterval(month %% 12, seq(0, 12, 3))
  factor(seasons[index], seasons)
}

ReadTSeries <- function(filename, windFilename, timecolumn="datetime", timeformat="%d.%m.%Y %H:%M") {
  ## read the table, strip units in column names, rename time column
  ##   and change data type of time column from a string of characters to
  ##   a numeric type so that we can perform operations on it
  data <- read.table(filename, skip=5, header=TRUE, sep=";", check.names=FALSE)
  names(data) <- sub("[ ].*$","",names(data)) # strip units for simplification
  names(data) <- sub("Date/time", timecolumn, names(data), fixed=TRUE)
  data[,timecolumn] <- as.chron(data[,timecolumn], timeformat) - 1/24 # end time -> start time
  ## extract additional variables from the time column
  data[,"year"] <- years(data[,timecolumn])
  data[,"month"] <- months(data[,timecolumn])
  data[,"day"] <- days(data[,timecolumn])
  data[,"hour"] <- hours(data[,timecolumn])
  data[,"dayofwk"] <- weekdays(data[,timecolumn])
  data[,"daytype"] <- ifelse(data[,"dayofwk"] %in% c("Sat","Sun"), "Weekend", "Weekday")
  data[,"season"] <- Month2Season(unclass(data[,"month"]))
  ## return value
  data
}

Lag <- function(pair, k) {
  out <- data.frame(lag=k, head(pair[,1],-k), tail(pair[,2],-k))
  names(out)[2:3] <- colnames(pair)
  out
}
MeanWind <- function(data){
  wind <- array(0,dim=c((nrow(data)/6),2))
  count <- 1
  speed <- 0
  direction <- 0
  for(j in 1:(nrow(data)/6)){
    while(count != 6){
      speed <- (speed + data[(6*(j-1)+count),"windSpeed"])
      direction <- (direction + data[(6*(j-1)+count),"windDirection"])
      count <- (count + 1)
    }
    wind[j,1] <- (speed/count)
    wind[j,2] <- (direction/count)
    count <- 0
    speed <- 0
    direction <- 0
  }
  wind <- as.data.frame(wind)
}

mean.angle <- function(theta, r=1, ...) {
  ## Function for averaging angles
  ## Polar coordinates -> Cartesian coordinates -> polar coordinates
  ##   'theta' is in degrees
  ##   'r=1' for unit circle
  ##   returns value is mean theta in degrees
  theta.rad <- theta * pi/180
  x <- mean(r * cos(theta.rad), ...)
  y <- mean(r * sin(theta.rad), ...)
  theta.deg <- atan2(y, x) * 180/pi
  ifelse(sign(theta.deg) < 0, (theta.deg + 360) %% 360, theta.deg) # -179--180 to 0--359
}

ControlMinute <- function(minutes) {
  count0 <-0
  count1 <-0
  count2 <-0
  count3 <-0
  count4 <-0
  count5 <-0
  for(i in 1:length(minutes)){
    if(minutes[i]==0){
      count0=count0+1
    }
    if(minutes[i]==10){
      count1=count1+1
    }
    if(minutes[i]==20){
      count2=count2+1
    }
    if(minutes[i]==30){
      count3=count3+1
    }
    if(minutes[i]==40){
      count4=count4+1
    }
    if(minutes[i]==50){
      count5=count5+1
    }
  }
  
  print(count0)
  print(count1)
  print(count2)
  print(count3)
  print(count4)
  print(count5)
}

LaggedCorrelation <- function(pair, ...) {
  out <- ccf(pair[,1], pair[,2], ..., na.action=na.pass, plot=FALSE)
  data.frame(lag=out[["lag"]], value=out[["acf"]])
}