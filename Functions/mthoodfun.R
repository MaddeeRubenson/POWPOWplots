#https://www.nwac.us/data-portal/location/mt-hood/q?field_name=snow_depth&year=2017&custom_startdate=2017-12-17&custom_enddate=2017-12-19
library(ggplot2)

mthood <- fread('https://www.nwac.us/data-portal/csv/location/mt-hood/sensortype/snow_depth/start-date/2017-12-17/end-date/2017-12-20/')
names(mthood) <- c('DateTimePST', 'SkiBowl', 'MtHoodMeadows', 'TimberlineLodge')
mthood$DateTimePST <- as.POSIXct(mthood$DateTimePST)
mthood <- na.omit(mthood)

g <- ggplot(data = mthood, aes()) +
  geom_point(aes(x = DateTimePST, y = SkiBowl, color = 'Ski Bowl Summit (5010 feet)')) +
  geom_point(aes(x = DateTimePST, y = MtHoodMeadows, color = 'Meadows Base(5380 feet)')) +
  geom_point(aes(x = DateTimePST, y = TimberlineLodge, color = 'Timberline Lodge (5880 feet)')) +
  ylab('Snow Depth (inches)') +
  xlab('Date/Time (PST)') + 
  theme_bw()+
  theme(legend.position = "top", legend.direction = "horizontal") +
  labs(colour = "") 
g
  
