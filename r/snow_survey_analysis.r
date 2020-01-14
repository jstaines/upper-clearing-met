library('dplyr')
library('ggplot2')


# import snow survey data
surveydir = "C:/Users/Cob/index/educational/usask/research/masters/data/snow_surveys/"
infile = "marmot_snow_surveys_raw_19_045.csv"
outfile = "19_045_depth_swe_coefficients.csv"
survey <- read.csv(paste0(surveydir,infile), skip=1, header=TRUE, na.strings=c("NA",""))

# subset density samples
dens <- survey %>%
  filter(!is.na(swe_raw_cm))
# calculate swe
dens$swe_mm <- 10*(dens$swe_raw_cm - dens$swe_tare_cm)
dens$density_kgpm3 <- 100*dens$swe_mm/dens$snow_depth_cm
# throw out data with quality flags
dens <- dens %>%
  filter(!swe_quality_flag)

ggplot(dens, aes(snow_depth_cm, density_kgpm3)) + 
  geom_point(aes(color=swe_quality_flag))

ggplot(dens, aes(snow_depth_cm, density_kgpm3)) + 
  geom_point(aes(color=standardized_survey_notes))

ggplot(dens, aes(snow_depth_cm, swe_mm)) + 
  geom_point(aes(color=standardized_survey_notes))

# density analysis
density_mean = group_by(dens, standardized_survey_notes) %>%
  summarise(avg = mean(density_kgpm3))
swe_mean = group_by(dens, standardized_survey_notes) %>%
  summarise(avg = 0.01*(mean(density_kgpm3)*mean(snow_depth_cm) + cov(snow_depth_cm, density_kgpm3)))

# linear model
linearMod <- lm(snow_depth_cm ~ swe_mm, data=dens)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

# forest
forest <- filter(dens, standardized_survey_notes == 'forest')
forest$flag <- 'forest_data'
linearMod <- lm(density_kgpm3 ~ snow_depth_cm, data=forest)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

alpha = linearMod$coefficients[[2]]
beta = linearMod$coefficients[[1]]

alpha_forest = alpha/100
beta_forest = beta/100

snow_depth_cm <- seq(from = 0, to = 80, by = 1)
swe_mm <- (alpha*snow_depth_cm + beta)*snow_depth_cm/100

model <- data.frame(snow_depth_cm,swe_mm)
model$flag <- 'forest_model'
mess <-  merge(model,forest,by=c("snow_depth_cm",'swe_mm','flag'), all=TRUE)

# clearing
clearing <- filter(dens, standardized_survey_notes == 'clearing')
clearing$flag <- 'clearing_data'
linearMod <- lm(density_kgpm3 ~ snow_depth_cm, data=clearing)  # build linear regression model on full data

alpha = linearMod$coefficients[[2]]
beta = linearMod$coefficients[[1]]

alpha_clearing = alpha/100
beta_clearing = beta/100

snow_depth_cm <- seq(from = 0, to = 80, by = 1)
swe_mm <- (alpha*snow_depth_cm + beta)*snow_depth_cm/100

model <- data.frame(snow_depth_cm,swe_mm)
model$flag <- 'clearing_model'
mess <-  merge(mess,clearing,by=c("snow_depth_cm",'swe_mm','flag'), all=TRUE)
mess <-  merge(mess,model,by=c("snow_depth_cm",'swe_mm','flag'), all=TRUE)

ggplot(mess, aes(snow_depth_cm, swe_mm)) + 
  geom_point(aes(color=flag))


# output coefficients
output <- data.frame(alpha_forest, beta_forest, alpha_clearing, beta_clearing)
write.csv(output, file = paste0(surveydir,outfile))
