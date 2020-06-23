# Load some libraries
library(readr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(pastecs)
library(dplyr)
library(vcd)
library(stringr)

# Read current data
oe_current <- read_tsv('https://download.bls.gov/pub/time.series/oe/oe.data.0.Current')

# Read area file
oe_area <- read_tsv('https://download.bls.gov/pub/time.series/oe/oe.area')

# Read Occupation file
oe_occupation <- read_tsv('https://download.bls.gov/pub/time.series/oe/oe.occupation')

# Current data has the series as an ID, want to split it to access some attributes
oe_current_split <- oe_current %>% tidyr::separate(series_id, c("survey_abbr","seasonal_code","areatype_code","area_code",
                              "industry_code","occupation_code","datatype_code"),
                         c(2,3,4,11,17,23))

# Merge current data with area data to get area details
current_area <- merge(oe_current_split, oe_area, by = c("area_code","areatype_code"))

# merge previously merged set with occupation data to get occupation details
current_area_occupation <- merge(current_area, oe_occupation, by = "occupation_code")

# Get Ohio (state code 39) metropolitan areas (areatype code M) 
# annual mean wage and employment count (datatype code 04, 01) for computer & mathematical occupations
ohio_area_sal <- subset(current_area_occupation, state_code=="39" & areatype_code=="M" & 
                           datatype_code=="04" & display_level==3 & substr(occupation_code,1,2)==15)

ohio_area_emp <- subset(current_area_occupation, state_code=="39" & areatype_code=="M" & 
                          datatype_code=="01" & display_level==3 & substr(occupation_code,1,2)==15)


# merge the two data sets, select columns I want, and clean up names
merged_ohio <- merge(ohio_area_sal, ohio_area_emp[,c("occupation_code","area_code","value")], by = c("occupation_code","area_code"),)
merged_ohio <- subset(merged_ohio,select=c("occupation_code","area_code","area_name","occupation_name","year","value.x","value.y"))
merged_ohio <- rename(merged_ohio, c("value.x"="mean_wage","value.y"="emp_count"))

# Convert some values to numeric
merged_ohio$mean_wage <- as.numeric(as.character(merged_ohio$mean_wage))
merged_ohio$emp_count <- as.numeric(as.character(merged_ohio$emp_count))
merged_ohio$area_code <- as.numeric(as.character(merged_ohio$area_code))

# Create a numeric factor for occuption
merged_ohio$occupation.factor <- as.numeric(factor(merged_ohio$occupation_name))

# The above generates some NAs, remove them
merged_ohio <- na.omit(merged_ohio) 

# Display row count and head of data
nrow(merged_ohio)
head(merged_ohio)

# Plot histogram with density for wage and employment
ggplot(data=merged_ohio, aes(x=mean_wage)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

# Group data by occupation name
employment_grouped <- merged_ohio %>% 
  dplyr::group_by(occupation_name) %>% 
  dplyr::summarise(employment = sum(emp_count))

ggplot(data=employment_grouped, aes(x=employment)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

# Boxplot for major cities
# If we try to do all area codes the plot is pretty wide
# so lets break it down to Cincinnati, Cleveland, and Columbus
ggplot(data=subset(merged_ohio,area_code %in% c("17140","17460","18140")),aes(x=area_name, y=mean_wage)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4) +
  ylim(45000,120000)

# Display some basic summary statistics
cor(merged_ohio[,c("emp_count","mean_wage","area_code","occupation.factor")])
cov(merged_ohio[,c("emp_count","mean_wage","area_code","occupation.factor")])
sd(merged_ohio$emp_count)
sd(merged_ohio$mean_wage)

stat.desc(merged_ohio[,"emp_count"], basic = FALSE, norm = TRUE)
stat.desc(merged_ohio[,"mean_wage"], basic = FALSE, norm = TRUE)

merged_ohio[,c("occupation_name","area_code","emp_count")]

# Sum employee counts by occupation name
merged_ohio %>% 
  dplyr::group_by(occupation_name) %>% 
  dplyr::summarise(emp_cnt_by_occupation = sum(emp_count))

# Sum employee counts by occupation name and return bottom 5 
# These means fewer number of people employed, so assuming more "specialized"
merged_ohio %>% 
  dplyr::group_by(occupation_name) %>% 
  dplyr::summarise(emp_cnt_by_occupation = sum(emp_count)) %>%
  top_n(-5)

# Sum employee counts by area name and return top 5 
merged_ohio %>% 
  dplyr::group_by(area_name) %>% 
  dplyr::summarise(emp_cnt_by_area = sum(emp_count)) %>%
  top_n(5)

# Plot mean wage against employee count
ggplot(data=subset(merged_ohio,area_code %in% c("10420","17140","17460","18140","19380") &
                     occupation_name %in% 
                     c("Actuaries",
                       "Computer and Information Research Scientists",
                       "Data Scientists and Mathematical Science Occupations, All Other",
                       "Database Administrators and Architects",
                       "Statisticians")), 
       aes(x=emp_count, y=mean_wage, color=occupation_name)) +
  geom_point()

# Group data by occupation name
group_data <- merged_ohio %>% 
  dplyr::group_by(occupation_name) %>% 
  dplyr::summarise(occupation_mean_wage = mean(mean_wage), occupation_max_wage = max(mean_wage))

# Plot bar graph for mean wage by occupation
ggplot(data=group_data, 
       aes(x=occupation_name, y=occupation_mean_wage)) +
  geom_col() +
  scale_x_discrete(labels = function(occupation_name) str_wrap(occupation_name, width = 10))

# Plot bar graph for max wage by occupation
ggplot(data=group_data, 
       aes(x=occupation_name, y=occupation_max_wage)) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels = function(occupation_name) str_wrap(occupation_name, width = 10))

# write data to a csv
write_csv(merged_ohio,"merged_ohio.csv")
