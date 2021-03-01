library('dplyr')
library('tidyr')
library('ggplot2')
source(file="C:/Users/Cob/index/educational/usask/research/masters/repos/upper-clearing-met/r/windrose.r")

uc_in = "C:/Users/Cob/index/educational/usask/research/masters/data/met/2016_19_QC_data_merge/Upper_Clearing_15min_2016_19_slim_merge.txt"
uc_raw = read.csv(uc_in, header=TRUE, na.strings=c(-9999, 'NA'), sep=",", skip=1)

ut_in = "C:/Users/Cob/index/educational/usask/research/masters/data/met/2016_19_QC_data_merge/Upper_Clearing_Tower_15min_2016_19_slim_merge.txt"
ut_raw = read.csv(ut_in, header=TRUE, na.strings=c(-9999, 'NA'), sep=",", skip=1)

uf_in = "C:/Users/Cob/index/educational/usask/research/masters/data/met/2016_19_QC_data_merge/Upper_Forest_15min_2016_19_slim_merge.txt"
uf_raw = read.csv(uf_in, header=TRUE, na.strings=c(-9999, 'NA'), sep=",", skip=1)

ls_in = "C:/Users/Cob/index/educational/usask/research/masters/data/met/hanging tree/lysimeters_merged.csv"
ls_raw = read.csv(ls_in, header=TRUE, na.strings=c(-9999, 'NA'), sep=",")

# qc of each dataset:
data_qc <- function(df) {
  # remove rows with NA date values
  df = df[!is.na(df$TIMESTAMP),]
  # conver datetime to posix
  df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, format="%Y-%m-%d %H:%M:%OS", tz="MST")
  # keep within start and end date
  start_date <- as.POSIXct("2019-02-01 12:55:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")
  end_date <- as.POSIXct("2019-02-25 11:40:00", format="%Y-%m-%d %H:%M:%OS", tz="MST")
  # start_date <- as.POSIXct("2019-02-19 12:40:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")
  # end_date <- as.POSIXct("2019-02-21 11:40:00", format="%Y-%m-%d %H:%M:%OS", tz="MST")
  # start_date <- as.POSIXct("2018-10-01 00:00:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")
  # end_date <- as.POSIXct("2019-10-01 00:00:00", format="%Y-%m-%d %H:%M:%OS", tz="MST")
  df = df[(df$TIMESTAMP >= start_date) & (df$TIMESTAMP <= end_date),]
  for(ii in 2:ncol(df)){
    df[, ii] = as.numeric(as.character(df[, ii]))
  }
  
  result <- df
}



uc = data_qc(uc_raw)
ut = data_qc(ut_raw)
uf = data_qc(uf_raw)
ls = data_qc(ls_raw)

s_045 = as.POSIXct("2019-02-14 12:55:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")
s_050 = as.POSIXct("2019-02-19 12:41:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")
s_052 = as.POSIXct("2019-02-21 11:40:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")

n_043 = as.POSIXct("2019-02-12 12:45:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")
n_045 = as.POSIXct("2019-02-14 12:45:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")
n_050 = as.POSIXct("2019-02-19 12:45:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")
n_052 = as.POSIXct("2019-02-21 11:45:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")
n_107 = as.POSIXct("2019-04-17 11:15:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")
n_123 = as.POSIXct("2019-05-03 11:15:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")
n_149 = as.POSIXct("2019-05-29 12:00:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")

date_nn = c(n_045, n_050, n_052)

z_pad = uc$Incremental.All.Precipitation
z_pad[is.na(z_pad)] = 0
uc$Cumulative.All.Precipitation = cumsum(z_pad)

# calculate event snow accumulation (SWE and depth)
uc_ts = (uc$TIMESTAMP == n_045) | (uc$TIMESTAMP == n_050) | (uc$TIMESTAMP == n_052) | (uc$TIMESTAMP == n_107) | (uc$TIMESTAMP == n_123) | (uc$TIMESTAMP == n_149)
uf_ts = (uf$TIMESTAMP == n_045) | (uf$TIMESTAMP == n_050) | (uf$TIMESTAMP == n_052) | (uf$TIMESTAMP == n_107) | (uf$TIMESTAMP == n_123) | (uf$TIMESTAMP == n_149)

ts_cumprecip = uc$Cumulative.All.Precipitation[uc_ts]
ts_uchs = uc$Snow.Depth[uc_ts]
ts_ufhs = uf$Snow.Depth[uf_ts]

d_precip = diff(ts_cumprecip)
d_uchs = diff(ts_uchs)
d_ufhs = diff(ts_ufhs)


# all precip
ggplot() +
  geom_line(data=uc, aes(x=TIMESTAMP, y=Cumulative.All.Precipitation - 4.342135, color='cPre (mm)')) +
  geom_line(data=uc, aes(x=TIMESTAMP, y=(Snow.Depth - .566) * 100, color='uc dHS (cm)')) + 
  geom_line(data=uf, aes(x=TIMESTAMP, y=(Snow.Depth - 0.328) * 100, color='uf dHS (cm)')) + 
  geom_line(data=ls, aes(x=TIMESTAMP, y=(m1 - 28.990), color='dTree (kg)')) +
  geom_vline(xintercept=s_045, linetype="dashed", size=1) +
  geom_vline(xintercept=s_050, linetype="dashed", size=1) +
  geom_vline(xintercept=s_052, linetype="dashed", size=1) +
  labs(title="Met station snow observations", x="Date", y="")

# all wind
ggplot() +
  # geom_line(data=uc, aes(x=TIMESTAMP, y=Cumulative.All.Precipitation - 4.342135, color='cPre (mm)')) +
  # geom_line(data=uc, aes(x=TIMESTAMP, y=(Snow.Depth - .566) * 100, color='dHS (cm)')) + 
  # geom_line(data=ls, aes(x=TIMESTAMP, y=(m1 - 28.990), color='dTree (kg)')) +
  geom_line(data=ut, aes(x=TIMESTAMP, y=Wind.Speed, color='tower')) +
  geom_line(data=uc, aes(x=TIMESTAMP, y=Wind.Speed, color='upper clearing')) +
  geom_line(data=uf, aes(x=TIMESTAMP, y=Wind.Speed, color='upper forest')) +
  # geom_line(data=uf, aes(x=TIMESTAMP, y=Air.Temperature, color='air_temp')) +
  # geom_line(data=uf, aes(x=TIMESTAMP, y=Relative.Humidity/10, color='RH')) +
  geom_vline(xintercept=s_045, linetype="dashed", size=1) +
  geom_vline(xintercept=s_050, linetype="dashed", size=1) +
  geom_vline(xintercept=s_052, linetype="dashed", size=1) +
  labs(title="Met station wind speeds", x="Date", y="Wind speed (m/s)")

ggplot() +
  # geom_line(data=uc, aes(x=TIMESTAMP, y=Cumulative.All.Precipitation - 4.342135, color='cPre (mm)')) +
  # geom_line(data=uc, aes(x=TIMESTAMP, y=(Snow.Depth - .566) * 100, color='dHS (cm)')) + 
  # geom_line(data=ls, aes(x=TIMESTAMP, y=(m1 - 28.990), color='dTree (kg)')) +
  geom_line(data=ut, aes(x=TIMESTAMP, y=Wind.Direction, color='tower')) +
  geom_line(data=uc, aes(x=TIMESTAMP, y=Wind.Direction, color='upper clearing')) +
  geom_line(data=uf, aes(x=TIMESTAMP, y=Wind.Direction, color='upper forest')) +
  # geom_line(data=uf, aes(x=TIMESTAMP, y=Air.Temperature, color='air_temp')) +
  # geom_line(data=uf, aes(x=TIMESTAMP, y=Relative.Humidity/10, color='RH')) +
  geom_vline(xintercept=s_045, linetype="dashed", size=1) +
  geom_vline(xintercept=s_050, linetype="dashed", size=1) +
  geom_vline(xintercept=s_052, linetype="dashed", size=1) +
  labs(title="Met station direction", x="Date", y="Wind direction (deg)")

# temperatures
ggplot() +
  # geom_line(data=uc, aes(x=TIMESTAMP, y=Cumulative.All.Precipitation - 4.342135, color='cPre (mm)')) +
  # geom_line(data=uc, aes(x=TIMESTAMP, y=(Snow.Depth - .566) * 100, color='dHS (cm)')) + 
  # geom_line(data=ls, aes(x=TIMESTAMP, y=(m1 - 28.990), color='dTree (kg)')) +
  # geom_line(data=ut, aes(x=TIMESTAMP, y=Wind.Speed, color='tower')) +
  # geom_line(data=uc, aes(x=TIMESTAMP, y=Wind.Speed, color='upper clearing')) +
  # geom_line(data=uf, aes(x=TIMESTAMP, y=Wind.Speed, color='upper forest')) +
  geom_line(data=ut, aes(x=TIMESTAMP, y=Air.Temperature, color='tower')) +
  geom_line(data=uc, aes(x=TIMESTAMP, y=Air.Temperature, color='clearing')) +
  geom_line(data=uf, aes(x=TIMESTAMP, y=Air.Temperature, color='forest')) +
  # geom_line(data=uf, aes(x=TIMESTAMP, y=Relative.Humidity/10, color='RH')) +
  geom_vline(xintercept=s_045, linetype="dashed", size=1) +
  geom_vline(xintercept=s_050, linetype="dashed", size=1) +
  geom_vline(xintercept=s_052, linetype="dashed", size=1) +
  labs(title="Met station temperatures", x="Date", y="Temp (degC)")

# RH
ggplot() +
  # geom_line(data=uc, aes(x=TIMESTAMP, y=Cumulative.All.Precipitation - 4.342135, color='cPre (mm)')) +
  # geom_line(data=uc, aes(x=TIMESTAMP, y=(Snow.Depth - .566) * 100, color='dHS (cm)')) + 
  # geom_line(data=ls, aes(x=TIMESTAMP, y=(m1 - 28.990), color='dTree (kg)')) +
  # geom_line(data=ut, aes(x=TIMESTAMP, y=Wind.Speed, color='tower')) +
  # geom_line(data=uc, aes(x=TIMESTAMP, y=Wind.Speed, color='upper clearing')) +
  # geom_line(data=uf, aes(x=TIMESTAMP, y=Wind.Speed, color='upper forest')) +
  geom_line(data=ut, aes(x=TIMESTAMP, y=Relative.Humidity, color='tower')) +
  geom_line(data=uc, aes(x=TIMESTAMP, y=Relative.Humidity, color='clearing')) +
  geom_line(data=uf, aes(x=TIMESTAMP, y=Relative.Humidity, color='forest')) +
  # geom_line(data=uf, aes(x=TIMESTAMP, y=Relative.Humidity/10, color='RH')) +
  geom_vline(xintercept=s_045, linetype="dashed", size=1) +
  geom_vline(xintercept=s_050, linetype="dashed", size=1) +
  geom_vline(xintercept=s_052, linetype="dashed", size=1) +
  labs(title="Met station relative humidity", x="Date", y="RH (%)") +
  ylim(0, 100)


# wind rose
uct = full_join(ut, uc, by="TIMESTAMP", suffix=c('_ut', '_uc'))

ggplot(uct, aes(x=Wind.Direction_ut, y=Wind.Speed_ut, color=Incremental.All.Precipitation)) + 
  coord_polar() +
  geom_point() +
  labs(title="Clearing tower wind speed and direction", color="clearing incremental precip", x="", y="Wind speed (m/s)")


ggplot(uct, aes(x=Wind.Direction_ut, y=Wind.Speed_ut)) +
  geom_bin2d(binwidth=c(30, .20)) +
  coord_polar() +
  labs(title="Wind rose for Upper Clearing Tower, Feb. 19-21", x="Wind direction (deg)", y="Wind speed (m/s)")


ggplot(uct, aes(x=Wind.Direction_ut, y=Wind.Speed_ut * Incremental.All.Precipitation)) +
  geom_bin2d(binwidth=c(30, .04)) +
  coord_polar() +
  labs(title="Weighted wind rose for Upper Clearing Tower, Feb 14-19", x="Wind direction (deg)", y="Interval precip * Wind speed (mm * m/s)")

t0 = n_050
t1 = n_052
data = uc

new_snow_dens = function(t0, t1, data){
  # subset data to range
  valid = (data$TIMESTAMP >= t0) & (data$TIMESTAMP <= t1) 
  vata = data[valid,]
  
  # find snow depth accumulation interval
  hs_min = min(vata$Snow.Depth, na.rm=TRUE)
  hs_max = max(vata$Snow.Depth, na.rm=TRUE)
  ac_0 = max(vata$TIMESTAMP[vata$Snow.Depth == hs_min], na.rm=TRUE)
  ac_1 = min(vata$TIMESTAMP[vata$Snow.Depth == hs_max], na.rm=TRUE)
  
  
  # # mean air temp weighted by snow depth increase
  # # lag difference in snow depths
  # dhs = diff(vata$Snow.Depth)
  # dhs[is.na(dhs)] = 0
  # dhs = c(dhs, 0)
  # Ta = mean(vata$Air.Temperature[dhs > 0] * dhs[dhs > 0]) / mean(dhs[dhs > 0])
  
  # mean air temp over accumulation interval
  acc = (vata$TIMESTAMP >= ac_0) & (vata$TIMESTAMP >= ac_1)
  Ta = mean(vata$Air.Temperature[acc], na.rm=TRUE)
  
  # model fresh snow density from Hedstrom and Pomeroy 1998
  hpd = 67.92 + 51.25 * exp(Ta/2.59)
  
  # estimate SWE
  swe = (hs_max - hs_min) * hpd
  
  # calculate density at end of interval
  nsd = swe / (tail(vata$Snow.Depth, n=1) - hs_min)
  
  c(nsd, swe, hpd, Ta)
}

new_snow_dens(n_043, n_045, uc)
new_snow_dens(n_045, n_050, uc)
new_snow_dens(n_050, n_052, uc)

new_snow_dens(n_043, n_045, uf)
new_snow_dens(n_045, n_050, uf)
new_snow_dens(n_050, n_052, uf)

# 
# ggplot() +
#   geom_line(data=uc, aes(x=TIMESTAMP, y=Cumulative.All.Precipitation, color='c_Precip')) +
#   geom_line(data=uc, aes(x=TIMESTAMP, y=(Snow.Depth - 0.574) * 100, color='d_HS')) + 
# 
#   geom_line(data=ut, aes(x=TIMESTAMP, y=Wind.Direction / 10, color='3'))
# 
# wind = ggplot(uc, aes(x=TIMESTAMP, y=Wind.Speed)) +
#   geom_line()
# 
# 
# prec + wind
# 
# ggplot(uf, aes(x = ))
# 
# cxc <- ggplot(mtcars, aes(x = factor(cyl))) +
#   geom_bar(width = 1, colour = "black")
# cxc + coord_polar()
# 
# uc %>% select(c('TIMESTAMP', 'Incremental.All.Precipitation', 'Snow.Depth')) %>%
#   gather(key, value, 2:3) %>%
#   ggplot(aes(x=TIMESTAMP, y=value, color=key)) +
#   geom_line()
# 
# 
# uc$Wind.Speed * uc$Incremental.All.Precipitation
# 
# p0 <- plot.windrose(spd = ut$Wind.Speed * uc$Incremental.All.Precipitation * 10,
#                     dir = ut$Wind.Direction)
# 
# 
# 
# # wind analysis
# angular_mean <- function(theta, weights=1, degree=FALSE, na.rm=FALSE){
#   if (degree){
#     theta = theta * pi / 180
#   }
#   
#   x = mean(sin(theta) * weights, na.rm=na.rm)
#   y = mean(cos(theta) * weights, na.rm=na.rm)
#   
#   theta_m = atan2(x, y)
#   r_m = sqrt(x^2 + y^2)
#   
#   if (degree){
#     theta_m = theta_m * 180 / pi
#   }
#   
#   output = c(theta_m, r_m)
#   
#   #return
#   output
# }
# 
# angular_mean(uct$Wind.Direction_ut, degree=TRUE)
# angular_mean(uct$Wind.Direction_ut, weights=uct$Wind.Speed_ut, degree=TRUE)
# angular_mean(uct$Wind.Direction_ut, weights=uct$Wind.Speed_ut * uct$Cumulative.All.Precipitation, degree=TRUE)
# 



# data = uct
# spd = "Wind.Speed_ut"
# dir = "Wind.Direction_ut"
# n_spd = 10
# windplot <- function(data, spd, dir, n_spd){
#   
#   spdmax=max(data[[spd]])
#   spdres <- spdmax/n_spd
#   spdseq <- seq(0,spdmax,spdres)
#   spdmid <- spdseq[1:n_spd] + (spdseq[2:(n_spd + 1)] - spdseq[1:n_spd])/2
#   spd.labels <- paste(c(spdseq[1:n_spd]), '-', c(spdseq[2:(n_spd + 1)]))
#   spdseq[1] = -1
#   data$spd.binned <- cut(x = data[[spd]],
#                          breaks = spdseq,
#                          labels = spdmid,
#                          ordered_result = TRUE)
#   
#   
#   n_dir = 12
#   dirres=360/n_dir
#   
#   
#   dir.breaks <- c(-dirres/2,
#                   seq(dirres/2, 360-dirres/2, by = dirres),
#                   360+dirres/2)
#   dirmid <- dir.breaks[1:(n_dir+1)] + (dir.breaks[2:(n_dir + 2)] - dir.breaks[1:(n_dir + 1)])/2
#   dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
#                   paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
#                         "-",
#                         seq(3*dirres/2, 360-dirres/2, by = dirres)),
#                   paste(360-dirres/2,"-",dirres/2))
#   # assign each wind direction to a bin
#   data$dir.binned <- cut(data[[dir]],
#                     breaks = dir.breaks,
#                     labels = dirmid,
#                     ordered_result = TRUE)
# }
# 
# binned <- data %>%
#   group_by(spd.binned, dir.binned) %>%
#   summarise(cum_precip = sum(Incremental.All.Precipitation))
# 
# ggplot(binned, aes(x=dir.binned, y=spd.binned, fill=cum_precip)) +
#   geom_raster() + 
#   coord_polar()



# (T
#   %>% mutate(phi = (180/pi)*atan2(y, x))
#   %>% mutate(r = sqrt(x*x + y*y))
#   %>% ggplot(aes(phi, r))
#   + geom_bin2d()
#   + coord_polar()
#   + scale_fill_viridis(trans = 'log10')
# )
# 
# # snow density analysis
# (uc$Cumulative.All.Precipitation[uc$TIMESTAMP == n_050] - uc$Cumulative.All.Precipitation[uc$TIMESTAMP == n_045]) / (uc$Snow.Depth[uc$TIMESTAMP == n_050] - uc$Snow.Depth[uc$TIMESTAMP == n_045])
