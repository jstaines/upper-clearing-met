library('dplyr')
library('tidyr')
library('ggplot2')

uc_in = "C:/Users/Cob/index/educational/usask/research/masters/data/met/2016_19_QC_data_merge/Upper_Clearing_15min_2016_19_slim_merge.txt"
uc_raw = read.csv(uc_in, header=TRUE, na.strings=c(-9999, 'NA'), sep=",", skip=1)

uct_in = "C:/Users/Cob/index/educational/usask/research/masters/data/met/2016_19_QC_data_merge/Upper_Clearing_Tower_15min_2016_19_slim_merge.txt"
uct_raw = read.csv(uct_in, header=TRUE, na.strings=c(-9999, 'NA'), sep=",", skip=1)

uf_in = "C:/Users/Cob/index/educational/usask/research/masters/data/met/2016_19_QC_data_merge/Upper_Forest_15min_2016_19_slim_merge.txt"
uf_raw = read.csv(uf_in, header=TRUE, na.strings=c(-9999, 'NA'), sep=",", skip=1)

# qc of each dataset:
data_qc <- function(df) {
  # remove rows with NA date values
  df = df[!is.na(df$TIMESTAMP),]
  # conver datetime to posix
  df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, format="%Y-%m-%d %H:%M:%OS")
  # keep within start and end date
  start_date <- as.POSIXct("2019-02-19 00:00:00", format="%Y-%m-%d %H:%M:%OS")
  end_date <- as.POSIXct("2019-02-22 00:00:00", format="%Y-%m-%d %H:%M:%OS")
  df = df[(df$TIMESTAMP >= start_date) & (df$TIMESTAMP <= end_date),]
  for(ii in 2:ncol(df)){
    df[, ii] = as.numeric(as.character(df[, ii]))
  }
  # remove rows with NA date values (again...)
  df = df[!is.na(df$TIMESTAMP),]
  result <- df
}

uc = data_qc(uc_raw)
uct = data_qc(uct_raw)
uf = data_qc(uf_raw)

ggplot(uct, aes(x=TIMESTAMP, y=Wind.Speed)) +
  geom_line()


uc %>% select(c('TIMESTAMP', 'Incremental.All.Precipitation', 'Snow.Depth')) %>%
  gather(key, value, 2:3) %>%
ggplot(aes(x=TIMESTAMP, y=value, color=key)) +
  geom_line()


