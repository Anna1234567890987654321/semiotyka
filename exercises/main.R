library(modelr)
library(kableExtra)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(knitr)
library(nycflights13)
library(forcats)
library(tidyverse)

# Excercise 1
smallpox <- read.csv("GlobalSmallpoxCases.csv", header = TRUE)

# Excercise 2
head(smallpox)

# Excercise 3
names(smallpox)[4] <- "cases"
# 4 is the number of column we want to change name of.

# Excercise 4
smallpox <- smallpox[, -2]
# We want to remove second column, so we use -2.

# Excercise 5
mean(smallpox$cases)
median(smallpox$cases)

# Excercise 6
mean(smallpox$cases[smallpox$Year >= 1920 & smallpox$Year <= 1930])

# Excercise 7
mean(smallpox$cases[smallpox$Year >= 1930 & smallpox$Year <= 1950])
mean(smallpox$cases[smallpox$Year >= 1950 & smallpox$Year <= 1970])

# Excercise 8
drunkDriving <- read.csv("DrunkAndDrivingPoland.csv", sep = ";")

# Excercise 9
head(drunkDriving)

# Excercise 10
names(drunkDriving)[1] <- "yearOfCasesOfDrunkAndDriving"

# Excercise 11
drunkDriving <- drunkDriving[, -4]
# We want to remove fourth column, so we use -4.

# Excercise 12
mean(drunkDriving$knownCases)
median(drunkDriving$knownCases)

# Excercise 13
mean(drunkDriving$knownCases[drunkDriving$yearOfCasesOfDrunkAndDriving >= 2001 & drunkDriving$yearOfCasesOfDrunkAndDriving <= 2006])
median(drunkDriving$knownCases[drunkDriving$yearOfCasesOfDrunkAndDriving >= 2001 & drunkDriving$yearOfCasesOfDrunkAndDriving <= 2006])

# Excercise 14
mean(drunkDriving$knownCases[drunkDriving$yearOfCasesOfDrunkAndDriving >= 2001 & drunkDriving$yearOfCasesOfDrunkAndDriving <= 2010])
median(drunkDriving$knownCases[drunkDriving$yearOfCasesOfDrunkAndDriving >= 2001 & drunkDriving$yearOfCasesOfDrunkAndDriving <= 2010])
# According to this data, drunk driving is decreasing problem.

# Excercise 15
ggplot(smallpox, aes(x = Year, y = cases)) +
  geom_line(color = "skyblue") +
  theme_tufte()

# Excercise 16
ggplot(heights, aes(x = education, y = income)) +
  geom_point() +
  geom_hline(yintercept = mean(heights$income), color = "red") +
  theme_tufte()

# Excercise 17
# Plot income vs education, colored by sex with mean lines
ggplot(heights, aes(x = education, y = income, color = sex)) +
  geom_point(size = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = sex), size = 2) +
  labs(x = "Education (years)", y = "Income (USD)") +
  theme_classic()

# Add median lines to the plot
ggplot(heights, aes(x = education, y = income, color = sex)) +
  geom_point(size = 2) +
  stat_summary(fun = median, geom = "line", aes(group = sex), size = 2) +
  labs(x = "Education (years)", y = "Income (USD)") +
  theme_classic()

# Split the plots by sex
ggplot(heights, aes(x = education, y = income)) +
  geom_point(aes(color = sex), size = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = sex), size = 2) +
  facet_wrap(~sex) +
  labs(x = "Education (years)", y = "Income (USD)") +
  theme_classic()

# Excercise 18
ggplot(heights, aes(x = education, y = income, color = sex)) +
  geom_point(size = 2) +
  stat_summary(fun = median, geom = "line", aes(group = sex), size = 1) +
  facet_grid(sex ~ marital) +
  labs(x = "Education (years)", y = "Income (USD)") +
  theme_classic()

# Excercise 19
ggplot(heights, aes(x = education, y = income, color = sex)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Education (years)", y = "Income (USD)") +
  theme_classic()

# Excercise 20
ggplot(faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  labs(x = "Waiting time (minutes)", y = "Eruption time (minutes)") +
  theme_classic()
# The scatterplot of eruption times vs. waiting times for the Old Faithful dataset shows a bimodal distribution for both
# variables, suggesting that the geyser alternates between two modes of behavior with different waiting times and
# eruption times. Additionally, there is a positive correlation between waiting time and eruption time, with longer
# waiting times corresponding to longer eruption times.

# Excercise 21
ggplot(drunkDriving, aes(x = yearOfCasesOfDrunkAndDriving, y = knownCases)) +
  geom_line(color = "skyblue") +
  theme_tufte()

# Excercise 22
nypd <- read.csv("NYPD_ShootingMOD.csv", sep = ",")

nypd$OCCUR_DATE <- as.Date(nypd$OCCUR_DATE)
nypd$MONTH <- as.integer(format(nypd$OCCUR_DATE, "%m", trim = TRUE))
nypd_monthly_counts <- nypd %>%
  mutate(MONTH = format(OCCUR_DATE, "%Y-%m")) %>%
  group_by(MONTH) %>%
  summarise(OCCURRENCES = n()) %>%
  ungroup()

ggplot(nypd_monthly_counts, aes(x = MONTH, y = OCCURRENCES)) +
  geom_point() +
  labs(x = "Month of the year", y = "Number of occurrences") +
  theme_classic()

# Excercise 23
nypd_monthly_counts_boro <- nypd %>%
  mutate(MONTH = format(OCCUR_DATE, "%Y-%m")) %>%
  group_by(MONTH, BORO) %>%
  summarise(OCCURRENCES = n()) %>%
  ungroup()

ggplot(nypd_monthly_counts_boro, aes(x = MONTH, y = OCCURRENCES, color = BORO)) +
  geom_point() +
  labs(x = "Month of the year", y = "Number of occurrences") +
  scale_color_discrete(name = "Borough") +
  theme_classic()

# The resulting plot shows the number of occurrences of shootings in each month of the year, with each point colored by
# borough. We can see that there are some differences in the occurrence rates across boroughs, with some boroughs
# having higher occurrence rates than others. For example, Brooklyn appears to have a higher occurrence rate than the
# other boroughs.

# Excercise 24
nypd_monthly_counts_age <- nypd %>%
  mutate(MONTH = format(OCCUR_DATE, "%Y-%m")) %>%
  group_by(MONTH, VIC_AGE_GROUP) %>%
  summarise(OCCURRENCES = n()) %>%
  ungroup()

ggplot(nypd_monthly_counts_age, aes(x = MONTH, y = OCCURRENCES, color = VIC_AGE_GROUP)) +
  geom_point() +
  labs(x = "Month of the year", y = "Number of occurrences") +
  scale_color_discrete(name = "Age group") +
  theme_classic()

# The resulting plot shows the number of occurrences of shootings in each month of the year, with each point colored by
# the age group of the victim. We can see that there are some differences in the occurrence rates across age groups,
# with some age groups having higher occurrence rates than others. For example, the "18-24" and "25-44" age groups
# appear to have higher occurrence rates than the other age groups. Additionally, we can see some patterns that are not
# apparent in the previous plot colored by borough, such as a higher occurrence rate for the "UNKNOWN" age group in the
# early months of the year.

# Excercise 25
nypd_monthly_counts_boro <- nypd %>%
  mutate(MONTH = format(OCCUR_DATE, "%Y-%m")) %>%
  group_by(MONTH, BORO) %>%
  summarise(OCCURRENCES = n()) %>%
  ungroup()

ggplot(nypd_monthly_counts_boro, aes(x = MONTH, y = OCCURRENCES, color = BORO)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Month of the year", y = "Number of occurrences") +
  scale_color_discrete(name = "Borough") +
  theme_classic()

# The resulting plot shows the number of occurrences of shootings in each month of the year, with each point colored by
# borough and smoothed with a line of best fit. We can see that there are some differences in the occurrence rates
# across boroughs, with some boroughs having higher occurrence rates than others. For example, Brooklyn appears to have
# a higher occurrence rate than the other boroughs, and the Bronx has a relatively high occurrence rate as well.
# Additionally, we can see some trends in the data over the year, with some boroughs experiencing higher occurrence
# rates in certain months. For example, Manhattan has a higher occurrence rate in the summer months, while Queens has
# a higher occurrence rate in the fall and winter months.

# Excercise 26
old_faithful <- faithful

ggplot(old_faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  labs(x = "Waiting time (minutes)", y = "Eruption time (minutes)") +
  theme_classic()

# The resulting plot shows a clear pattern in the data, with longer waiting times generally corresponding to longer
# eruption times. This pattern is expected, as Old Faithful is a geyser with a somewhat predictable eruption cycle.

# Excercise 27
ggplot(heights, aes(x = sex, y = income)) +
  geom_boxplot() +
  labs(x = "Sex", y = "Income (USD)") +
  theme_classic()

# Excercise 28
nypd_monthly_counts_boro <- nypd %>%
  mutate(MONTH = format(OCCUR_DATE, "%Y-%m")) %>%
  group_by(MONTH, BORO) %>%
  summarise(OCCURRENCES = n()) %>%
  ungroup()

ggplot(nypd_monthly_counts_boro, aes(x = BORO, y = OCCURRENCES)) +
  geom_boxplot() +
  labs(x = "Borough", y = "Number of incidents") +
  theme_classic()

# Excercise 29
twoLast <- filter(flights, month == 11 | month == 12)
head(twoLast)

# Excercise 30
twoLast_ordered <- twoLast[order(twoLast$dep_delay),]

flightsMutated <- select(flights, year:day, ends_with("delay"), distance, air_time) %>%
  mutate(gain = arr_delay - dep_delay,
         hours = air_time / 60,
         gain_per_hour = gain / hours)

head(flightsMutated)

# Excercise 31
daily_delay <- flights %>%
  group_by(month, day) %>%
  summarize(mean_delay = mean(dep_delay, na.rm = TRUE))

ggplot(data = daily_delay, aes(x = 30 * (month - 1) + day, y = mean_delay)) +
  geom_line(alpha = 0.7, color = "blue") +
  labs(x = "Day of the Year", y = "Mean Daily Delay") +
  theme_bw()

# Excercise 32
ggplot(data = flights, aes(x = distance, y = dep_delay)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_smooth(color = "skyblue", se = FALSE) +
  labs(x = "Distance (miles)", y = "Delay (minutes)") +
  theme_bw()

# Excercise 33
twoLast <- filter(nypd, OCCUR_DATE >= "2021-11-01" & OCCUR_DATE <= "2021-12-31")
head(twoLast)

# Excercise 34
twoLast_ordered <- twoLast[order(twoLast$BORO),]

# Excercise 35
nypd$MONTH <- format(nypd$OCCUR_DATE, "%m")
nypd$DAY <- format(nypd$OCCUR_DATE, "%d")

monthly_incidents <- nypd %>%
  group_by(MONTH, DAY) %>%
  summarise(INCIDENT_COUNT = n()) %>%
  group_by(MONTH) %>%
  summarise(MEAN_INCIDENTS = mean(INCIDENT_COUNT))

ggplot(monthly_incidents, aes(x = MONTH, y = MEAN_INCIDENTS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Month of the year", y = "Mean number of incidents") +
  theme_classic()

# Excercise 36
monthly_borough <- nypd %>%
  mutate(month = lubridate::month(OCCUR_DATE)) %>%
  group_by(BORO, month) %>%
  summarize(MEAN_INCIDENTS = n()/n_distinct(year(OCCUR_DATE)*100+month)) %>%
  ungroup()

# Create the plot
ggplot(monthly_borough, aes(x = month, y = MEAN_INCIDENTS, color = BORO)) +
  geom_point() +
  stat_summary(fun = mean, geom = "line", aes(group = BORO), col = "red", size = 1) +
  stat_summary(fun = median, geom = "line", aes(group = BORO), col = "skyblue", size = 1) +
  scale_x_continuous(name = "Month", breaks = 1:12) +
  scale_color_discrete(name = "Borough") +
  labs(title = "Average Number of Incidents per Month by Borough",
       subtitle = "Data from NYPD Complaint Database") +
  theme_minimal()

# Excercise 37
ggplot(diamonds, aes(x = cut, fill = cut)) +
  geom_bar() +
  scale_fill_discrete(name = "Diamond Cut") +
  labs(title = "Diamond Cuts by Count",
       x = "Diamond Cut",
       y = "Count") +
  theme_minimal()

# Excercise 38
relig <- gss_cat %>%
  group_by(relig) %>%
  summarize(mean_age = mean(age, na.rm = TRUE),
            mean_tvhours = mean(tvhours, na.rm = TRUE),
            count = n())

# Excercise 39
ggplot(relig, aes(x = relig, y = mean_tvhours)) +
  geom_point() +
  labs(title = "Religion vs. TV Hours",
       x = "Religion",
       y = "Mean TV Hours") +
  theme_minimal()

# Excercise 40
ggplot(gss_cat, aes(x = rincome, y = tvhours)) +
  geom_point() +
  labs(title = "Reported Income Level vs. TV Hours",
       x = "Reported Income Level",
       y = "TV Hours") +
  theme_minimal()

# Excercise 41
nypd_by_race <- nypd %>%
  group_by(VIC_RACE) %>%
  summarise(total_incidents = n())

ggplot(nypd_by_race, aes(x = VIC_RACE, y = total_incidents, fill = VIC_RACE)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Shooting Incidents by Victim's Race",
       x = "Victim's Race",
       y = "Total Incidents")

# Excercise 42
nypd_borough_race <- nypd %>%
  group_by(BORO, VIC_RACE) %>%
  summarize(total_incidents = n())

ggplot(nypd_borough_race, aes(x = BORO, y = total_incidents, fill = VIC_RACE)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of Shooting Incidents by Borough and Victim's Race",
       x = "Borough",
       y = "Total Incidents",
       fill = "Victim's Race") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Excercise 43
nypd_borough_race_perc <- nypd %>%
  group_by(BORO, VIC_RACE) %>%
  summarize(total_incidents = n()) %>%
  group_by(BORO) %>%
  mutate(percent = (total_incidents/sum(total_incidents)) * 100)

head(nypd_borough_race_perc)

ggplot(nypd_borough_race_perc, aes(x = BORO, y = percent, fill = VIC_RACE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Shooting Incidents by Borough and Victim's Race",
       x = "Borough",
       y = "Percent of Total Incidents",
       fill = "Victim's Race") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Excercise 44
monthly_boro_incidents <- nypd %>%
  group_by(BORO, month = lubridate::month(OCCUR_DATE)) %>%
  summarize(total_incidents = n()) %>%
  ungroup()

ggplot(monthly_boro_incidents, aes(x = month, y = total_incidents, fill = BORO)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of Shooting Incidents by Month and Borough",
       x = "Month",
       y = "Total Incidents",
       fill = "Borough") +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal()

# Excercise 45
nypd$hour <- as.numeric(substring(nypd$OCCUR_TIME, 1, 2))

ggplot(nypd, aes(x = hour)) +
  geom_histogram(color = "black", fill = "darkblue", bins = 24) +
  labs(title = "Number of Shooting Incidents by Hour of the Day",
       x = "Hour of the Day",
       y = "Number of Incidents") +
  scale_x_continuous(breaks = 0:24) +
  theme_minimal()

safest_hour <- nypd %>%
  group_by(hour) %>%
  summarize(total_incidents = n()) %>%
  arrange(total_incidents) %>%
  slice(1)