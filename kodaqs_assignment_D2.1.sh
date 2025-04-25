#!/bin/bash

### Dataframes
#### Landsize per prefecture (landsize)
landsize <- read_excel("data/landsize.xls")
landsize.edited <- subset(landsize, select = -c(1, 4, 5, 6)) %>% ## remove unnecessary columns
  filter(!row_number() %in% c(1:13)) ## remove unnecessary rows

colnames(landsize.edited)[c(1, 2)] <- c("prefecture", "landsize") ## rename column names
landsize.edited <- landsize.edited[rowSums(is.na(landsize.edited)) != ncol(landsize.edited), ] ## remove the last rows with NA

#### Population size 1 (pop1)
pop1 <- read_excel("data/pop_2005_2010.xls")
pop1.edited <- subset(pop1, select = -c(1:3)) %>% ## remove unnecessary columns
  filter(!row_number() %in% c(1:13)) ## remove unnecessary rows
colnames(pop1.edited)[c(1:19)] <- c("prefecture", "2005_all", "2005_m", "2005_f", "2006_all", "2006_m", "2006_f", "2007_all", "2007_m", "2007_f", "2008_all", "2008_m", "2008_f", "2009_all", "2009_m", "2009_f", "2010_all", "2010_m", "2010_f") ## rename column names

pop1.edited$prefecture <- sub("-.*", "", pop1.edited$prefecture) ## remove everything after and including the hyphen

#### Population size 2 (pop2)
pop2 <- read_excel("data/pop_2010_2015.xls")

pop2.edited <- subset(pop2, select = -c(1:3)) %>% ## remove unnecessary columns
  filter(!row_number() %in% c(1:13)) ## remove unnecessary rows
colnames(pop2.edited)[c(1:19)] <- c("prefecture", "2010_all", "2010_m", "2010_f", "2011_all", "2011_m", "2011_f", "2012_all", "2012_m", "2012_f", "2013_all", "2013_m", "2013_f", "2014_all", "2014_m", "2014_f", "2015_all", "2015_m", "2015_f") ## rename column names

pop2.edited$prefecture <- sub("-.*", "", pop2.edited$prefecture) ## remove everything after and including the hyphen

#### population size 3 (pop3)
pop3 <- read_excel("data/pop_2015_2020.xlsx")

pop3.edited <- subset(pop3, select = -c(2, 3, 8, 9, 10, 11, 12)) %>% ## remove unnecessary columns
  filter(!row_number() %in% c(1, 2, 3, 4, 5, 246, 247)) ## remove unnecessary rows

colnames(pop3.edited)[c(1:5)] <- c("year", "prefecture_jp", "all", "male", "female") ## rename column names

prefecture <- c("all", "Hokkaido", "Aomori", "Iwate", "Miyagi", "Akita", "Yamagata", "Fukushima", "Ibaraki", "Tochigi", "Gumma", "Saitama", "Chiba", "Tokyo", "Kanagawa", "Niigata", "Toyama", "Ishikawa", "Fukui", "Yamanashi", "Nagano", "Gifu", "Shizuoka", "Aichi", "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo", "Nara", "Wakayama", "Tottori", "Shimane", "Okayama", "Hiroshima", "Yamaguchi", "Tokushima", "Kagawa", "Ehime", "Kochi", "Fukuoka", "Saga", "Nagasaki", "Kumamoto", "Oita", "Miyazaki", "Kagoshima", "Okinawa") ## create a list of prefecture names
repeat_prefecture <- rep(prefecture, times = 5)

pop3.edited$prefecture <- repeat_prefecture ## add prefecture at the end
pop3.edited2 <- pop3.edited[, c("year", "prefecture_jp", "prefecture", "all", "male", "female")] ## reorder columns

pop3.edited2_1 <- pop3.edited2[1:48, ] %>% ## split the dataframe into parts, 2016
  subset(select = -c(1:2)) %>% ## remove unnecessary columns
  mutate_at(vars(-prefecture), as.numeric)
colnames(pop3.edited2_1)[c(1:4)] <- c("prefecture", "2016_all", "2016_m", "2016_f")

pop3.edited2_2 <- pop3.edited2[49:96, ] %>% ## split the dataframe into parts, 2017
  subset(select = -c(1:2)) %>%  ## remove unnecessary columns
  mutate_at(vars(-prefecture), as.numeric)
colnames(pop3.edited2_2)[c(1:4)] <- c("prefecture", "2017_all", "2017_m", "2017_f")

pop3.edited2_3 <- pop3.edited2[97:144, ] %>% ## split the dataframe into parts, 2018
  subset(select = -c(1:2)) %>% ## remove unnecessary columns
  mutate_at(vars(-prefecture), as.numeric)
colnames(pop3.edited2_3)[c(1:4)] <- c("prefecture", "2018_all", "2018_m", "2018_f")

pop3.edited2_4 <- pop3.edited2[145:192, ] %>% ## split the dataframe into parts, 2019
  subset(select = -c(1:2)) %>% ## remove unnecessary columns
  mutate_at(vars(-prefecture), as.numeric)
colnames(pop3.edited2_4)[c(1:4)] <- c("prefecture", "2019_all", "2019_m", "2019_f")

pop3.edited2_5 <- pop3.edited2[193:240, ] %>% ## split the dataframe into parts, 2020
  subset(select = -c(1:2)) %>% ## remove unnecessary columns
  mutate_at(vars(-prefecture), as.numeric)
colnames(pop3.edited2_5)[c(1:4)] <- c("prefecture", "2020_all", "2020_m", "2020_f")

pop3.edited3 <- pop3.edited2_1 %>% ## merge the five parts into one
  left_join(pop3.edited2_2, by = "prefecture") %>%
  left_join(pop3.edited2_3, by = "prefecture") %>%
  left_join(pop3.edited2_4, by = "prefecture") %>%
  left_join(pop3.edited2_5, by = "prefecture")

pop3.edited3 <- pop3.edited3[-1, ] ## remove "all" (= the country level population)

### Merge landsize.edited, pop1.edited, pop2.edited, pop3.edited3
pop2.edited2 <- subset(pop2.edited, select = -c(2:4)) ## remove duplicated columns with pop1.edited

pop <- landsize.edited %>%
  left_join(pop1.edited, by = "prefecture") %>%
  left_join(pop2.edited2, by = "prefecture") %>%
  left_join(pop3.edited3, by = "prefecture")

pop <- pop %>% 
  mutate(across(-prefecture, as.numeric)) ## change all but prefecture variable to numeric

### Calculate population density
#### Population x1000
pop.real <- pop %>%
  mutate_at(vars(3:50), ~ . * 1000)

#### Population density
##### Divide year_all by landsize while keeping the prefecture name
density <- pop.real %>%
  select(prefecture, landsize, seq(3, ncol(pop.real), by = 3)) %>%
  mutate(across(seq(3, ncol(.), by = 1), ~ . / landsize)) %>%
  select(-landsize) ## remove landsize from the dataframe
print(density)

#### Assign numbers 1-47 to prefecture names in order to match JES data, as JES data codes prefecture names numerically
density.numeric <- density %>%
  mutate(prefecture = 1:n()) %>%
  rename("PREFECTURE" = prefecture)

print(density.numeric)