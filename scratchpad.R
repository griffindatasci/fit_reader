library(data.table)
library(zoo)
library(FITfileR)
library(dplyr)
library(ggplot2)




normalized_power <- function(x){
  mean(x^4)^(1/4)
}



# readFitFile("data/zrl_r2_s5/Zwift_Race_3R_Three_Sisters_Hilly_Race_47km_29_2mi_879m_B_on_Three_Sisters_in_Watopia.fit") %>%
#   records() %>%
#   data.table ->
#   activity
# 
# activity %>%
#   fwrite("data/dt_activity.txt")

activity <- fread("data/dt_activity.txt")

# Add power zone ---------------------------------------------------------------
ftp <- 307
power_zones <- c(0.55,0.75,0.90,1.05,1.2,1.5)
activity[, power_zone := sapply(power, function(i){min(which(c(ftp*power_zones, Inf)>i))})]


# Add heart zone ---------------------------------------------------------------
max_hr <- 188
heart_zones <- c(0.59,0.78,0.87,0.97,Inf)
activity[, heart_zone := sapply(heart_rate, function(i){min(which(c(max_hr*heart_zones, Inf)>i))})]



# Analysis =====================================================================
# Map --------------------------------------------------------------------------
activity[, ggplot(.SD, aes(x=position_long, y=position_lat))+
             geom_path()]


# Elevation Profile ------------------------------------------------------------
activity[, ggplot(.SD, aes(x=distance, y=altitude)) +
           geom_area()]


# Summary Stats ----------------------------------------------------------------
# - Distance
activity_kms <- activity[, max(distance)/1000]

# - Time
activity_seconds <- activity[, as.numeric(difftime(max(timestamp), min(timestamp), units="secs"))]
activity[, sprintf("%02.f:%02.f:%02.f",
                   activity_seconds%/%60%/%60,
                   activity_seconds%/%60%%60,
                   activity_seconds%%60%%60)]

# - Average Speed
activity_kmh <- activity_kms/activity_seconds*3600

# - Max Speed
activity[, kmh:=activity_kmh/mean(speed)*speed]
activity[, max(kmh)]

# - Elevation Gain
activity[, sum(ifelse(altitude-shift(altitude)>0, altitude-shift(altitude), 0), na.rm=TRUE)]

# - Power averages, normalised, peaks
activity[, mean(power)]
activity[, median(power)]
activity[, normalized_power(power)]
activity[, max(rollapply(power, 60*20, mean))]
activity[, max(rollapply(power, 60*5, mean))]
activity[, max(rollapply(power, 60, mean))]
activity[, max(rollapply(power, 15, mean))]

# - kCal
#   - A joule is the energy required to generate one watt for one second, so joules (at the pedals) = mean(power) * duration
#   - Roughly, humans use 5 joules to generate 1 watt at the pedals, so the total joules used = mean(power) * duration * 5
#   - Happily, a joules is about 0.24 calories, cancelling out the *5, making kCal burnt = mean(power) * duration
activity[, mean(power)] * activity_seconds / 1000

# - Intensity
activity[, median(power)] / ftp * 100


# Time in Each Heart Zone ------------------------------------------------------
activity[, sprintf("%02.f:%02.f", .N%/%60, .N%%60), keyby=heart_zone]


# Time in Each Power Zone ------------------------------------------------------
activity[, sprintf("%02.f:%02.f", .N%/%60, .N%%60), keyby=power_zone]


# Power Histogram --------------------------------------------------------------
activity[, ggplot(.SD, aes(x=power)) +
             geom_bar() +
             scale_x_binned(breaks=seq(0, ceiling(max(power)/25)*25, 25))]


# Power Curve ------------------------------------------------------------------
power_curve_data <- activity[ , .("secs"=1:1200,
                                  "power"=sapply(1:1200, function(secs){ max(rollapply(power, secs, mean)) }))]

power_curve_data[, ggplot(.SD, aes(x=secs, y=power)) +
                     scale_x_log10() +
                     geom_area()]



# Analysis Plot ----------------------------------------------------------------
activity[, power_30s := c(rep(NA, 29), rollapply(power, 30, mean))]
activity[, ggplot(.SD, aes(x=distance)) +
             geom_area(aes(y=altitude)) +
             geom_path(aes(y=heart_rate), color="red") +
             geom_path(aes(y=power_30s), color="orange") +
             geom_path(aes(y=cadence), color="green") +
             geom_path(aes(y=kmh), color="blue")]











activity[, ggplot(.SD, aes(x=distance, y=heart_rate)) +
             theme_light() +
             geom_ribbon(aes(ymin=min(heart_rate)-10, 
                             ymax=diff(range(heart_rate))/max(altitude)*altitude+min(heart_rate)), fill="#10506020") +
             geom_path(aes(y=heart_rate)) +
             geom_hline(yintercept=mean(heart_rate), lty=2)]



metric_plot <- function(data, met, label, smoothing=1){
  ggplot(data, aes(x=distance, y=met)) +
    geom_ribbon(aes(ymin=min(met), 
                    ymax=diff(range(met))/max(altitude)*altitude+min(met)), fill="#10506040") +
    geom_path(aes(y=c(rep(NA,smoothing-1), rollapply(met,smoothing,mean)))) +
    geom_hline(yintercept=mean(met), lty=2) +
    labs(x="Distance (m)", y=label)
  }

activity[, metric_plot(.SD, heart_rate, "Heart Rate (BPM)", smoothing=60*5)]
activity[, metric_plot(.SD, power, "Power (W)", smoothing=60*5)]
