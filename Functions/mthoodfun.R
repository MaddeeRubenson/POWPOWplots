### Mt Hood Snow Plots

SnowDepthPlot <- function(startdate = (Sys.Date() - 2), enddate = Sys.Date()) {
  #this function creates a plot of the snow depth at Meadows, Skibowl, and Timberline
  #inputs are start date and end date (%y-%m-%d), default set to present date and 15 days prior
  
  #https://www.nwac.us/data-portal/location/mt-hood/q?field_name=snow_depth&year=2017&custom_startdate=2017-12-17&custom_enddate=2017-12-19
  require(ggplot2)
  require(data.table)
  require(curl)
  
  url <- paste0('https://www.nwac.us/data-portal/csv/location/mt-hood/sensortype/snow_depth/start-date/', startdate, '/', 'end-date/', enddate, '/')  
  mthood <- fread(url)
  
  #mthood <- fread('https://www.nwac.us/data-portal/csv/location/mt-hood/sensortype/snow_depth/start-date/2017-12-17/end-date/2017-12-20/')
  
  names(mthood) <- c('DateTimePST', 'SkiBowl', 'MtHoodMeadows', 'TimberlineLodge')
  mthood$DateTimePST <- as.POSIXct(mthood$DateTimePST)
  mthood <- na.omit(mthood)
  
  g <- ggplot(data = mthood, aes()) +
    geom_line(aes(x = DateTimePST, y = SkiBowl, color = 'Ski Bowl Summit (5010 feet)')) +
    geom_line(aes(x = DateTimePST, y = MtHoodMeadows, color = 'Meadows Base(5380 feet)')) +
    geom_line(aes(x = DateTimePST, y = TimberlineLodge, color = 'Timberline Lodge (5880 feet)')) +
    ylab('Snow Depth (inches)') +
    xlab('Date/Time (PST)') + 
    theme_bw()+
    theme(legend.position = "top", legend.direction = "horizontal") +
    labs(colour = "") 
  
  return(g)
  
}

SnowDepthPlot()

snowfall <- function(startdate = (Sys.Date() - 2), enddate = Sys.Date()) {
  url <- paste0('https://www.nwac.us/data-portal/csv/location/mt-hood/sensortype/snowfall_24_hour/start-date/', startdate, '/', 'end-date/', enddate, '/')  
  mthood <- fread(url)
  
  names(mthood) <- c('DateTimePST', 'MeadowsBase', 'TimberlineLodge')
  mthood$DateTimePST <- as.POSIXct(mthood$DateTimePST)
  #mthood <- na.omit(mthood)
  
  mthood <- melt(mthood, id.vars = c('DateTimePST'))
  names(mthood) <- c('DateTimePST', 'Location', '24HourSnowFall_inches')
  y.lim <- c(0, max(mthood$`24HourSnowFall_inches`))
  
  g <- ggplot(mthood) +
    geom_point(aes(DateTimePST, `24HourSnowFall_inches`, color = Location)) +
    ylim(y.lim)
  g
  
}

snowfall()

Precip <- function(startdate = (Sys.Date() - 3), enddate = Sys.Date()) {
  url <- paste0('https://www.nwac.us/data-portal/csv/location/mt-hood/sensortype/precipitation/start-date/', startdate, '/', 'end-date/', enddate, '/')  
  mthood <- fread(url)
  
  names(mthood) <- c('DateTimePST','SkiBowl', 'MeadowsBase', 'TimberlineLodge')
  mthood$DateTimePST <- as.POSIXct(mthood$DateTimePST)
  #mthood <- na.omit(mthood)
  
  mthood <- melt(mthood, id.vars = c('DateTimePST'))
  names(mthood) <- c('DateTimePST', 'Location', 'Precip')
  y.lim <- c(0, max(mthood$Precip))
  
  g <- ggplot(mthood) +
    geom_line(aes(DateTimePST, Precip, color = Location)) +
    ylim(y.lim)
  g
  
}

Precip()
