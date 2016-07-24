rm(list = ls())
#### Library List ####
library(data.table)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(caTools) # In case subset is needed
library(RColorBrewer)

# Load UDFs
sapply(paste0('UDF/', list.files('UDF')), source)

#### Read & Convert Data ####
# You need to download this data by yourself. See link in Github's README
collision <- fread('NYPD_Motor_Vehicle_Collisions.csv', stringsAsFactors = F)

# Transform Date & Time (rounded to hour)
work_dt <- collision %>%  
  mutate(FULLDATE = as.POSIXct(paste(DATE, TIME), 
                               format = '%m/%d/%Y %H:%M',
                               tz = 'EST')
  ) %>%
  mutate(YEAR = year(FULLDATE), 
         MONTH = month(FULLDATE),
         DAY = mday(FULLDATE),
         HOUR = hour(round(FULLDATE, 'hour'))
  ) %>%
  as.data.table

# Data group for yearly comparison (Each period July - June)
work_dt[MONTH <  7, PERIOD := paste(YEAR - 1, YEAR, sep = '-')]
work_dt[MONTH >= 7, PERIOD := paste(YEAR, YEAR + 1, sep = '-')]
# Remove 2016.7, since it falls into period 2016-2017
work_dt <- work_dt[PERIOD != '2016-2017']

# Build subsets for each topic
right_dt  <- work_dt %>%
  select(-starts_with('CONTRIBUTING FACTOR'), 
         -starts_with('VEHICLE TYPE CODE'), 
         -matches('KILLED|INJURED'),
         ID = `UNIQUE KEY`)

# factor_dt  <- melt_count(work_dt, 'matches', 'FACTOR', 'cat') %>% 
#   merge(right_dt, by = 'ID')
# 
# vehicle_dt <- melt_count(work_dt, 'starts', 'VEHICLE', 'cat') %>% 
#   merge(right_dt, by = 'ID')

injur_dt <- melt_count(work_dt, 'ends', 'INJURED', 'stat') %>% 
  merge(right_dt, by = 'ID')

kill_dt <- melt_count(work_dt, 'matches', 'KILLED', 'stat') %>% 
  merge(right_dt, by = 'ID')


#### 1. Check Time Range ####
# Original time range
g <- ggplot(data = sample_set(work_dt),
            aes(x = factor(month(FULLDATE)), 
                y = YEAR))
g +
  geom_tile(aes(fill = factor(YEAR)), color = 'white') +
  # ggtitle('Time Range of NYC Motor Vehicle Collision Data') +
  labs(x = 'Month')+
  theme(legend.position = 'none', 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")
  )

# Group by 12-month periods for analysis
g +
  geom_tile(aes(fill = PERIOD), color = 'white') +
  labs(x = 'Month') +
  # ggtitle('12 Month Periods For Yearly Comparison') +
  theme(legend.position = 'top',
        legend.title = element_text(face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

#### 2. Count of accidents per year ####
accident_n_dt <- work_dt %>%
  group_by(PERIOD) %>%
  summarise(N = n())

ggplot(data = accident_n_dt) +
  geom_bar(aes(x = PERIOD, y = N, fill = PERIOD), stat = 'identity') +
  labs(y = NULL, 
       title = 'Number of Motor Vehicle Collisions for Each 12-Month Period') +
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(x = PERIOD, y = N - 6000, label = N), size = 5)

# Seems not changing that much, what about percentage?
accident_p_vec <- c(0, tail(accident_n_dt$N, nrow(accident_n_dt) - 1) / 
                head(accident_n_dt$N, nrow(accident_n_dt) - 1) - 1)

# NUmber of registered vehicles in NYC
# https://dmv.ny.gov/about-dmv/statistical-summaries
reg_nyc <- read.csv('nyc_car_reg.csv')
reg_p_vec <- c(0, tail(reg_nyc$Reg_num, nrow(reg_nyc)-1) /
                 head(reg_nyc$Reg_num, nrow(reg_nyc)-1) - 1)

temp <- data.frame(PERIOD = rep(sort(unique(work_dt$PERIOD)), 2),
                   TYPE   = rep(c('Motor Vehicle Collision',
                                  'Vehicle Registration in NYC'), each = 4),
                   VALUE  = c(accident_p_vec, reg_p_vec))
ggplot(data = temp,
       aes(x = PERIOD, y = VALUE, color = TYPE, group = TYPE)) + 
  geom_point(size = 3) +
  geom_line(size = 2, alpha = .8) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'none') +
  labs(title = 'Yearly Growth Rate', y = '% Growth Since Last Year') +
  facet_wrap( ~ TYPE, nrow = 2)

#### 3. Injury Per Period ####
injur_n_dt <- injur_dt               %>%
  group_by(PERIOD, INJURED)          %>%
  summarise(Injury = sum(INJURED_V))  %>%
  filter(!grepl('PERSONS', INJURED)) %>% # Num. of persons is the total of 
  # other three stats
  arrange(desc(Injury)) %>%
  mutate(INJURED = INJURED %>%
           gsub('NUMBER OF ', '', .) %>%
           gsub(' INJURED', '', .) %>% 
           factor(levels = c('CYCLIST', 
                             'PEDESTRIANS', 
                             'MOTORIST')))


injur_n_label_dt <- injur_n_dt %>%
  group_by(PERIOD) %>%
  summarise(N = sum(Injury))

g <- ggplot(data = injur_n_dt) +
  geom_bar(aes(x = PERIOD, y = Injury, fill = reorder(INJURED, Injury)), 
           stat = 'identity') +
  scale_fill_manual(values = c("#468966", "#77C4D3", "#FFB03B")) +
  guides(fill = guide_legend(ncol = 3, title = 'Injury Type')) +
  theme(legend.position = 'top',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

g + geom_text(data = injur_n_label_dt,
              aes(x = PERIOD, y = N + 2000, label = N), size = 5)

# Break down
g + facet_wrap(~ INJURED)

temp <- injur_n_label_dt %>% 
  mutate(injur_rate = N / accident_n_dt$N)

ggplot(data = temp, aes(x = PERIOD, y = injur_rate)) +
  geom_line(group = 1, size = 2, alpha = .5, color = 'red') +
  geom_label(aes(label = round(injur_rate, 3))) +
  labs(x = NULL, y = 'Injury Per Accident') +
  theme(legend.position = 'none', 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  coord_cartesian(ylim = c(0, .5))

#### 4. Deaths Per Period ####
death_n_dt <- kill_dt               %>%
  group_by(PERIOD, KILLED)          %>%
  summarise(Death = sum(KILLED_V))  %>%
  filter(!grepl('PERSONS', KILLED)) %>% # Num. of persons is the total of 
                                        # other three stats
  arrange(desc(Death))

death_n_dt$KILLED <- death_n_dt$KILLED %>% 
  gsub('NUMBER OF ', '', .) %>%
  gsub(' KILLED', '', .)

death_n_label_dt <- death_n_dt %>%
  group_by(PERIOD) %>%
  summarise(N = sum(Death))

g <- ggplot(data = death_n_dt) +
  geom_bar(aes(x = PERIOD, y = Death, fill = reorder(KILLED, Death)), 
           stat = 'identity') +
  scale_fill_manual(values = c("#468966", "#77C4D3", "#FFB03B")) +
  guides(fill = guide_legend(ncol = 3, title = 'Killed Type')) +
  theme(legend.position = 'top',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

g + geom_text(data = death_n_label_dt,
              aes(x = PERIOD, y = N + 20, label = N), size = 5)
# Seems death number actually droped! Let's check the change by each death type
g + facet_wrap(~ KILLED)
# Two major death types has dropped obviously, which is a big good news;
# Meanwhile, cyclist deaths still increases constantly, this might be caused
# by increasing amount of cyclists on the streets (Citibike, for example)



# What about severe accidents?
# Data for violin density plot
death_id_dt <- kill_dt %>% 
  filter(grepl('PERSONS', KILLED))
# Data for density plot labeling
death_density_dt <- death_id_dt %>%
  group_by(PERIOD, KILLED_V) %>%
  summarise(N = n())

ggplot(data = death_id_dt) +
  geom_violin(aes(x = PERIOD, y = KILLED_V, fill = PERIOD)) +
  labs(y = 'Death(s) Per Accident') +
  theme(legend.position = 'none')
# Well, of course it's heavily right-skewed, since severve 
# accidents are way less likely to occur.
# Let's only focus on accidents that causes more than 1 death
ggplot(data = death_id_dt %>% filter(KILLED_V > 1),
       aes(x = PERIOD, y = KILLED_V, fill = PERIOD)) +
  geom_violin(scale = "width", fill = 'gray', alpha = .5, color = 'gray') +
  labs(y = 'Death(s) Per Accident') +
  theme(legend.position = 'none') +
  geom_dotplot(binaxis = "y", stackdir = "center") + 
  geom_label(data = death_density_dt %>% filter(KILLED_V > 1), 
            aes(x = PERIOD, y = KILLED_V + .2, label = N), 
            fontface = "bold")
# Seems severe accidents have also became less deadly and less frequently.

# One last exam on death - deaths per 1000 accidents
temp <- data.frame(PERIOD = death_n_label_dt$PERIOD,
                   d_rate = death_n_label_dt$N / accident_n_dt$N * 1000)
ggplot(data = temp, aes(x = PERIOD, y = d_rate)) +
  geom_point(shape = 16, size = 3, color = 'red', alpha = .5) +
  geom_line(aes(group = 1), size = 2, color = 'red', alpha = .5) +
  labs(x = NULL, y = 'Deaths Per 1000 Accidents') +
  theme(legend.position = 'none') +
  geom_label(aes(y = d_rate + .03, label = round(d_rate, 2)))

#### 5. Find Deadlist Spots ####
# Accident count by location
mvc_count <- work_dt %>%
  filter(!(is.na(LATITUDE) | is.na(LONGITUDE))) %>%
  mutate(LAT = round(LATITUDE, 2),
         LON = round(LONGITUDE, 2)) %>%
  group_by(LAT, LON) %>%
  summarise(N = n())

# Top injuries by location
mvc_inj <- injur_dt %>%
  filter(!(is.na(LATITUDE) | is.na(LONGITUDE))) %>%
  filter(grepl('PERSONS', INJURED)) %>%
  mutate(LAT = round(LATITUDE, 2),
         LON = round(LONGITUDE, 2)) %>%
  select(INJURED_V, LAT, LON) %>%
  group_by(LAT, LON) %>%
  summarise(INJURED_V = sum(INJURED_V)) %>%
  arrange(desc(INJURED_V)) %>%
  # table(mvc_sum$KILLED_V)
  #   1   2   3   4   5 {}  6   7   8  10 
  # 231 121  50  17  13 {}  8   4   2   1
  # ^ Break here
  filter(INJURED_V > 850)

# Top deaths by location
mvc_dth <- kill_dt %>%
  filter(!(is.na(LATITUDE) | is.na(LONGITUDE))) %>%
  filter(grepl('PERSONS', KILLED)) %>%
  mutate(LAT = round(LATITUDE, 2),
         LON = round(LONGITUDE, 2)) %>%
  select(KILLED_V, LAT, LON) %>%
  group_by(LAT, LON) %>%
  summarise(KILLED_V = sum(KILLED_V)) %>%
  arrange(desc(KILLED_V)) %>%
  # table(mvc_sum$KILLED_V)
  #   1   2   3   4   5 {}  6   7   8  10 
  # 231 121  50  17  13 {}  8   4   2   1
                      # ^ Break here
  filter(KILLED_V >5)
  
# Load NYC map
# Only need to run once, then you can load map locally
nyc_map <- get_map(location = find_map_cent(mvc_count$LON,
                                            mvc_count$LAT),
                   zoom = 11)
save(nyc_map, file = 'nyc_map.RData') 
# load('nyc_map.RData') # Load saved nyc map to save time

# Mapping accident counts
ggmap(nyc_map, extent = 'device') +
  geom_point(data = mvc_count, alpha = .5,
             aes(x = LON, y = LAT, color = N, size = N)) +
  scale_color_gradient(low = 'white', high = 'red') +
  theme(legend.justification=c(1,1), legend.position=c(.95,.95),
        legend.background = element_rect(color = 'black',
                                         fill = alpha('white', 0.8)),
        legend.title = element_blank()) +
  guides(size = FALSE) +
  ggtitle('Accident Frequencies by Location')

# Top Locations with most deaths
ggmap(nyc_map, extent = 'device') +
  geom_point(data = mvc_dth,
             aes(x = LON, y = LAT,
                 size = KILLED_V,
                 fill = KILLED_V),
             shape = 21, alpha = .5) +
  scale_size_area(max_size = 10) +
  scale_fill_continuous(low = 'yellow', high = 'red') +
  geom_text(data = mvc_dth, 
            aes(x = LON, y = LAT, label = KILLED_V),
            size = 4, check_overlap = T, fontface = "bold") +
  theme(legend.position = 'none') +
  ggtitle('Deadlist Locations July 2012 - June 2016')

