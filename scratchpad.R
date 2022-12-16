library(data.table)
library(zoo)
library(FITfileR)
library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)


normalize <- function(x){
  mean(x^4)^(1/4)
}


# readFitFile("data/Zwift_Race_Zwift_Racing_League_WTRL_EMEAE_North_Open_B2_on_Cobbled_Climbs_in_Richmond.fit") %>%
#   records() %>%
#   data.table ->
#   activity
# 
# activity %>%
#   fwrite("data/dt_activity.txt")

activity <- fread("data/dt_activity.txt")

input <- list()




# Add power zone ---------------------------------------------------------------
input$ftp <- 307
power_zones <- c(0.55,0.75,0.90,1.05,1.2,1.5)
activity[, power_zone := sapply(power, function(i){min(which(c(input$ftp*power_zones, Inf)>i))})]


# Add heart zone ---------------------------------------------------------------
input$max_hr <- 188
heart_zones <- c(0.59,0.78,0.87,0.97,Inf)
activity[, heart_zone := sapply(heart_rate, function(i){min(which(c(input$max_hr*heart_zones, Inf)>i))})]

# Add distance in km -----------------------------------------------------------
activity[, km:=distance/1000]

# Add in gradient --------------------------------------------------------------
activity[, grade:=c(rep(NA, 19), rollapply(altitude-shift(altitude, 1), 20, sum)/
                      rollapply(distance-shift(distance, 1), 20, sum) * 100)]



# Analysis =====================================================================
values <- list()
figures <- list()
tables <- list()
plotly_figures <- list()


# Summary Stats ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Distance
values$tot_kms <- activity[, max(km)]

# - Time
values$tot_sec <- activity[, as.numeric(difftime(max(timestamp), min(timestamp), units="secs"))]
values$tot_tim <- activity[, sprintf("%02.f:%02.f:%02.f",
                                     values$tot_sec%/%60%/%60,
                                     values$tot_sec%/%60%%60,
                                     values$tot_sec%%60%%60)]

# - Speed
values$avg_kmh <- values$tot_kms/values$tot_sec*3600
activity[, kmh:=values$avg_kmh/mean(speed)*speed]
values$max_kmh <- activity[, max(kmh)]

# - Cadence (removing zeros)
values$avg_cad <- activity[cadence>0, mean(cadence)]
values$max_cad <- activity[cadence>0, max(cadence)]

# - Heart Rate
values$avg_hrt <- activity[, mean(heart_rate)]
values$max_hrt <- activity[, max(heart_rate)]

# - Elevation Gain
values$tot_clm <- activity[, sum(ifelse(altitude-shift(altitude)>0, altitude-shift(altitude), 0), na.rm=TRUE)]

# - Power averages, normalised, peaks
values$avg_pwr <- activity[, mean(power)]
values$max_pwr <- activity[, max(power)]
values$med_pwr <- activity[, median(power)]
values$nrm_pwr <- activity[, normalize(power)]
values$m20_pwr <- activity[, max(rollapply(power, 60*20, mean))]
values$m05_pwr <- activity[, max(rollapply(power, 60*5, mean))]
values$m01_pwr <- activity[, max(rollapply(power, 60, mean))]
values$s15_pwr <- activity[, max(rollapply(power, 15, mean))]

# - kCal
#   - A joule is the energy required to generate one watt for one second, so joules (at the pedals) = mean(power) * duration
#   - Roughly, humans use 5 joules to generate 1 watt at the pedals, so the total joules used = mean(power) * duration * 5
#   - Happily, a joules is about 0.24 calories, cancelling out the *5, making kCal burnt = mean(power) * duration
values$tot_cal <- activity[, mean(power)] * values$tot_sec / 1000

# - Intensity
values$tot_int <- activity[, median(power)] / input$ftp * 100



# Tables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Time in Each Heart Zone ------------------------------------------------------
tables$tim_hrt <- activity[, sprintf("%02.f:%02.f", .N%/%60, .N%%60), keyby=heart_zone]


# Time in Each Power Zone ------------------------------------------------------
tables$tim_pwr <- activity[, sprintf("%02.f:%02.f", .N%/%60, .N%%60), keyby=power_zone]




# Figures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Map --------------------------------------------------------------------------
figures$map <- 
  ggplot(data=activity, aes(x=position_long, y=position_lat)) +
    geom_path(color="#41706c", size=1.6) +
    geom_path(color=NA, aes(text=sprintf("%.1f km (%.fm)<br>%.0fW<br>%0.f bpm", km, altitude, power, heart_rate))) +
    theme(panel.background=element_blank(),
          panel.grid=element_blank(),
          # aspect.ratio = 1,
          plot.margin=margin(0,0,0,0,"cm"),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()) +
  scale_x_continuous(limits=activity[, mean(range(position_long))+c(-1,1) * max(diff(range(position_long)), diff(range(position_lat)))/2]) +
  scale_y_continuous(limits=activity[, mean(range(position_lat))+c(-1,1) * max(diff(range(position_long)), diff(range(position_lat)))/2])

plotly_figures$map <- figures$map %>% 
  ggplotly(tooltip="text", height=500, width=500)


# Elevation Profile ------------------------------------------------------------
figures$alt <- 
  ggplot(data=activity, aes(x=km, y=altitude)) +
    theme(panel.background=element_blank(),
          panel.grid.major.y=element_line(color="grey92"),
          axis.line=element_line(colour="black"),
          #aspect.ratio = 0.33,
          plot.margin=margin(0,0,0,0,"cm")) +
    geom_ribbon(aes(ymin=min(activity$altitude), ymax=activity$altitude), fill="#41706c60") +
    geom_path(color=NA, aes(text=sprintf("%.0f km<br>%.0f m", km, altitude))) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
  labs(x="Distance (km)", y="Elevation (m)")

plotly_figures$alt <- figures$alt %>% 
  ggplotly(tooltip="text", height=250, width=500)


# Power Histogram --------------------------------------------------------------
x_breaks <- seq(0, max(activity$power)%/%25*25+25, 25)
x_labels <- ifelse(x_breaks%%200==0|x_breaks==0, x_breaks, "")

figures$pwr_hst <-
  ggplot(activity[, .N, by=power%/%25][,
                    .(N, "pwr_from"=25*power, "pwr_to"=25*power+24)],
         aes(x=pwr_from+12.5, y=N))  +
  labs(x="Power", y="") +
  theme(panel.background=element_blank(),
        panel.grid.major.y=element_line(color="grey92"),
        axis.line=element_line(colour="black"),
        #aspect.ratio = 0.33,
        plot.margin=margin(0,0,0,0,"cm")) +
  scale_y_continuous(breaks=seq(0, 10000, 120), 
                     labels=sprintf("%02.f:%02.f", seq(0, 10000, 120)%/%60, seq(0, 10000, 120)%%60),
                     expand=c(0,0)) +
  scale_x_binned(limits=c(0, max(x_breaks)), 
                 breaks=x_breaks, 
                 labels=x_labels, expand=c(0,0)) +
  geom_col(fill="#41706c60", aes(text=(sprintf("%4.f-%4.f<br>%02.f:%02.f", pwr_from, pwr_to, (N/60)%/%1, N%%60))))

plotly_figures$pwr_hist <- figures$pwr_hst %>% 
  ggplotly(tooltip="text", height=250, width=500)




# Power Curve ------------------------------------------------------------------
crv_int <- c(seq(1,59,1), seq(60,299,5), seq(300,599,10), seq(600,1800,20))
pwr_crv <- activity[ , .("secs"=crv_int,
                         "power"=sapply(crv_int, function(secs){ max(rollapply(power, secs, mean)) }))]

x_breaks <- c(5,30,120,600)

figures$pwr_crv <- 
  ggplot(data=pwr_crv, aes(x=secs, y=power)) +
    geom_ribbon(color=NA, fill="#41706c60", aes(ymin=0, ymax=power)) +
    geom_path(color="#41706c", aes(text=paste(sprintf("%02.f:%02.f", secs%/%60, secs%%60),
                             "@", round(power,0),"W"))) +
    labs(x="", y="Power") +
    theme(panel.background=element_blank(),
          panel.grid.major.y=element_line(color="grey92"),
          axis.line=element_line(colour="black"),
          #aspect.ratio = 0.33,
          plot.margin=margin(0,0,0,0,"cm")) +
    scale_x_log10(breaks=x_breaks,
                  labels=sprintf("%02.f:%02.f", x_breaks%/%60, x_breaks%%60),
                  expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,max(activity$power)*1.05))

plotly_figures$pwr_crv <- figures$pwr_crv %>% 
  ggplotly(tooltip="text", height=250, width=500)


# Heart Zones ------------------------------------------------------------------
hr_key <- data.table(heart_zone=1:5, 
                     zone_name=c("Endurance", "Moderate", "Tempo", "Threshold", "Anaerobic"))

figures$hr_zones <-
  activity[, .N, keyby=heart_zone][hr_key, on="heart_zone"][, .(heart_zone, zone_name, "N"=ifelse(is.na(N), 0, N))] %>% 
  ggplot(aes(x=zone_name, y=N)) +
  geom_col(fill="#41706c60", aes(text=sprintf("%02.f:%02.f", N%/%60, N%%60))) +
  coord_flip() +
  labs(y="", x="") +
  theme(panel.background=element_blank(),
        panel.grid.major.y=element_line(color="grey92"),
        axis.line=element_line(colour="black"),
        #aspect.ratio = 0.33,
        plot.margin=margin(0,0,0,0,"cm")) +
  scale_x_discrete(expand=c(0,0), limits=c("Endurance", "Moderate", "Tempo", "Threshold", "Anaerobic")) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0,100000,600), labels=sprintf("%02.f:%02.f", seq(0,100000,600)%/%60, seq(0,100000,600)%%60))

plotly_figures$hr_zones <- figures$hr_zones %>% 
  ggplotly(tooltip="text", height=250, width=500)


# Power Zones ------------------------------------------------------------------
pwr_key <- data.table(power_zone=1:7, 
                     zone_name=c("Recovery", "Endurance", "Tempo", "Threshold", "VO2 Max", "Anaerobic", "Neuromuscular"))

figures$pwr_zones <-
  activity[, .N, keyby=power_zone][pwr_key, on="power_zone"][, .(power_zone, zone_name, "N"=ifelse(is.na(N), 0, N))] %>% 
  ggplot(aes(x=zone_name, y=N)) +
  geom_col(fill="#41706c60", aes(text=sprintf("%02.f:%02.f", N%/%60, N%%60))) +
  coord_flip() +
  labs(y="", x="") +
  theme(panel.background=element_blank(),
        panel.grid.major.y=element_blank(),
        axis.line=element_line(colour="black"),
        #aspect.ratio = 0.33,
        plot.margin=margin(0,0,0,0,"cm")) +
  scale_x_discrete(expand=c(0,0), limits=c("Recovery", "Endurance", "Tempo", "Threshold", "VO2 Max", "Anaerobic", "Neuromuscular")) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0,100000,600), labels=sprintf("%02.f:%02.f", seq(0,100000,600)%/%60, seq(0,100000,600)%%60))

plotly_figures$pwr_zones <- figures$pwr_zones %>% 
  ggplotly(tooltip="text", height=250, width=500)


# 
# 
# 
# # Analysis Plot ----------------------------------------------------------------
# activity[, power_30s := c(rep(NA, 29), rollapply(power, 30, mean))]
# 
# figures$mix <-
#   ggplot(data=activity, aes(x=distance)) +
#     geom_area(aes(y=altitude)) +
#     geom_path(aes(y=c(rep(NA, 59), rollapply(heart_rate, 60, mean))), color="red") +
#     geom_path(aes(y=c(rep(NA, 59), rollapply(power, 60, mean))), color="orange") +
#     geom_path(aes(y=c(rep(NA, 59), rollapply(cadence, 60, mean))), color="green") +
#     geom_path(aes(y=c(rep(NA, 59), rollapply(kmh, 60, mean))), color="blue")
# 
# 
# 
# data.table(names(values), values)
# 
# 
# 
# 
# 
# 
# # TODO - experiment with smoothing
# metric_plot <- function(data, met, label, smoothing=1){
#   
#   ggplot(data, aes(x=distance, y=met)) +
#     geom_ribbon(aes(ymin=min(met), 
#                     ymax=diff(range(met))/max(altitude)*altitude+min(met)), fill="#10506040") +
#     geom_path(aes(y=c(rep(NA,smoothing-1), rollapply(met,smoothing,mean)))) +
#     geom_hline(yintercept=mean(met), lty=2) +
#     labs(x="Distance (m)", y=label)
#   }
# 
# activity[, metric_plot(.SD, heart_rate, "Heart Rate (BPM)", smoothing=300)]
# activity[, metric_plot(.SD, power, "Power (W)", smoothing=300)]
# 
# 
# 
# 
# activity[, m05_pwr:=c(rep(NA,299), rollapply(power, 300, mean))]
# ggplot(activity[!is.na(m05_pwr)], aes(x=distance, y=m05_pwr)) +
#   geom_ribbon(aes(ymin=min(m05_pwr)-abs(min(altitude)),
#                   ymax=(min(m05_pwr)*(altitude+altitude/max(altitude))))) +
#   geom_path(color="red")
# 
# 
# 
# 
