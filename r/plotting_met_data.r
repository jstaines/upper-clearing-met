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

# qc of each dataset:
data_qc <- function(df) {
  # remove rows with NA date values
  df = df[!is.na(df$TIMESTAMP),]
  # conver datetime to posix
  df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, format="%Y-%m-%d %H:%M:%OS", tz="MST")
  # keep within start and end date
  start_date <- as.POSIXct("2019-02-14 00:00:00", format="%Y-%m-%d %H:%M:%OS",tz="MST")
  end_date <- as.POSIXct("2019-02-22 00:00:00", format="%Y-%m-%d %H:%M:%OS", tz="MST")
  df = df[(df$TIMESTAMP >= start_date) & (df$TIMESTAMP <= end_date),]
  for(ii in 2:ncol(df)){
    df[, ii] = as.numeric(as.character(df[, ii]))
  }
  
  result <- df
}

uc = data_qc(uc_raw)
ut = data_qc(ut_raw)
uf = data_qc(uf_raw)

ggplot(uf, aes(x=TIMESTAMP, y=Wind.Speed)) +
  geom_line()


ggplot(uf, aes(x = ))

cxc <- ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(width = 1, colour = "black")
cxc + coord_polar()

uc %>% select(c('TIMESTAMP', 'Incremental.All.Precipitation', 'Snow.Depth')) %>%
  gather(key, value, 2:3) %>%
  ggplot(aes(x=TIMESTAMP, y=value, color=key)) +
  geom_line()


uc$Wind.Speed * uc$Incremental.All.Precipitation

p0 <- plot.windrose(spd = ut$Wind.Speed * uc$Incremental.All.Precipitation * 10,
                    dir = ut$Wind.Direction)

x = 3
x<-3
