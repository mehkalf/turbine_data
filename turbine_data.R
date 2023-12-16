# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

#import data
fulldata <- read.csv(paste(getwd(), 'SCRIPTS_R/turbine_data/Turbine_Data.csv', sep = "/"))

# check data
str(fulldata)

# 'data.frame':	118224 obs. of  22 variables:
#   $ X                           : chr  "2017-12-31 00:00:00+00:00" "2017-12-31 00:10:00+00:00" "2017-12-31 00:20:00+00:00" "2017-12-31 00:30:00+00:00" ...
# $ ActivePower                 : num  NA NA NA NA NA NA NA NA NA NA ...
# $ AmbientTemperatue           : num  NA NA NA NA NA NA NA NA NA NA ...
# $ BearingShaftTemperature     : num  NA NA NA NA NA NA NA NA NA NA ...
# $ Blade1PitchAngle            : num  NA NA NA NA NA NA NA NA NA NA ...
# $ Blade2PitchAngle            : num  NA NA NA NA NA NA NA NA NA NA ...
# $ Blade3PitchAngle            : num  NA NA NA NA NA NA NA NA NA NA ...
# $ ControlBoxTemperature       : num  NA NA NA NA NA NA NA NA NA NA ...
# $ GearboxBearingTemperature   : num  NA NA NA NA NA NA NA NA NA NA ...
# $ GearboxOilTemperature       : num  NA NA NA NA NA NA NA NA NA NA ...
# $ GeneratorRPM                : num  NA NA NA NA NA NA NA NA NA NA ...
# $ GeneratorWinding1Temperature: num  NA NA NA NA NA NA NA NA NA NA ...
# $ GeneratorWinding2Temperature: num  NA NA NA NA NA NA NA NA NA NA ...
# $ HubTemperature              : num  NA NA NA NA NA NA NA NA NA NA ...
# $ MainBoxTemperature          : num  NA NA NA NA NA NA NA NA NA NA ...
# $ NacellePosition             : num  NA NA NA NA NA NA NA NA NA NA ...
# $ ReactivePower               : num  NA NA NA NA NA NA NA NA NA NA ...
# $ RotorRPM                    : num  NA NA NA NA NA NA NA NA NA NA ...
# $ TurbineStatus               : num  NA NA NA NA NA NA NA NA NA NA ...
# $ WTG                         : chr  "G01" "G01" "G01" "G01" ...
# $ WindDirection               : num  NA NA NA NA NA NA NA NA NA NA ...
# $ WindSpeed                   : num  NA NA NA NA NA NA NA NA NA NA ...

# change date format
fulldata$X <- strptime(fulldata$X, format = "%Y-%m-%d %H:%M:%S")
fulldata$X <- as.POSIXct(fulldata$X)


#calculate monthly windspeed
monthly_mean_windspeed <- fulldata %>%
  mutate(Date = format(X, "%Y-%m")) %>%
  group_by(Date) %>%
  summarise(Avg_WindSpeed = mean(WindSpeed, na.rm = TRUE),
            Data_count = sum(!is.na(WindSpeed)))

#plot monthly mean windspeed
g1 <- ggplot(monthly_mean_windspeed, aes(x = Date, y = Avg_WindSpeed)) +
  geom_bar(stat = "identity", fill = "navy") +
  geom_text(aes(label = Data_count), vjust = -0.5, color = "black") +
  labs(title = "Monthly Average Wind Speed and Data Count",
       x = "Months",
       y = "Avg. Wind Speed") +
  theme_grey() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

show(g1)
ggsave("RuzgarHiziGrafik.jpeg", plot = g1, width = 8, height = 6, dpi = 300)

#rounding WindDirection to integer and grouping
fulldata$WindDirection <- round(fulldata$WindDirection)
WindDirection  <- as.data.frame(na.omit(fulldata$WindDirection))
WindDirection <- WindDirection %>% 
  rename("Wind_Dir" = "na.omit(fulldata$WindDirection)")
WindDir_grouped <- WindDirection %>% group_by(Wind_Dir) %>% 
  summarise(Count = n())

# ggplot ile çizim
g2 <- ggplot(WindDir_grouped, aes(x = factor(Wind_Dir), y = Count)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_polar(start = 0) +  # Polar plot için
  labs(title = "Wind Direction Distribution",
       x = "Wind Direction (Degree)",
       y = "Data Count") +
  scale_x_discrete(labels = seq(0, 350, by = 10), breaks = seq(0, 350, by = 10))
  theme_grey()

show(g2)
ggsave("RuzgarYonuGrafik.jpeg", plot = g2, width = 8, height = 6, dpi = 300)

