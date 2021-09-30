#just importing everything and getting workspace ready-----------------------

library(rio)
library(tidyverse)
library(dplyr)
# install.packages('stargazer')
library(stargazer)

#get and set wd
getwd()
setwd("C:/Users/limsh/Downloads/Winter Quarter 2021/POLI 5")

#import dataset
uni_data = import('mrc_table10.dta')
view(uni_data)
head(uni_data)


#make data into percents---------------------------------------------------------
##all the original percent share is in decimal form, convert to a percent

#asian
uni_data$asian_or_pacific_share_fall_2000 <- uni_data$asian_or_pacific_share_fall_2000*100
view(uni_data$asian_or_pacific_share_fall_2000)

#black
uni_data$black_share_fall_2000 <- uni_data$black_share_fall_2000*100
head(uni_data)

#hispanic
uni_data$hisp_share_fall_2000 <- uni_data$hisp_share_fall_2000*100
head(uni_data)

#alien
uni_data$alien_share_fall_2000 <- uni_data$alien_share_fall_2000*100
head(uni_data)


#binning data------------------------------------------------------------------
#put data into bins so we can rearrange into a stacked histogram 
#(rel_data is a new data frame to take just the data relevant to minority share)

#select only data dealing with minority share
rel_data <- uni_data %>% select('asian_or_pacific_share_fall_2000':'alien_share_fall_2000')
head(rel_data)

#set vector for breaks in data
breaks <- seq(0, 100, 5)
breaks

#set vector for labels
labels <- seq(5, 101, 5)
labels

# make new columns with the bins of each data

#asian bins
rel_data$asian_group <- as.numeric(cut(rel_data$asian_or_pacific_share_fall_2000,
                   breaks = breaks, labels = labels))

#black bins
rel_data$black_group <- as.numeric(cut(rel_data$black_share_fall_2000,
                                       breaks = breaks, labels = labels))

#hispanic bins
rel_data$hisp_group <- as.numeric(cut(rel_data$hisp_share_fall_2000,
                                      breaks = breaks, labels = labels))

#foreign bins
rel_data$alien_group <- as.numeric(cut(rel_data$alien_share_fall_2000,
                                       breaks = breaks, labels = labels))

#create new data frame that puts all the bins in one column, and minority group in another
rel_data2 <- rel_data %>% 
  pivot_longer(cols = c(asian_group, black_group, hisp_group, alien_group), 
               names_to = 'group_name', values_to = 'bins')

#mult the bins by 5 to make them percents
rel_data2$bins <- rel_data2$bins*5

#basic descriptive statistics------------------------------------------------------------

#make a table that has the basic descriptive stats of all the minority share variables
stargazer(uni_data[c('asian_or_pacific_share_fall_2000', 'black_share_fall_2000', 
                     'hisp_share_fall_2000', 'alien_share_fall_2000')], 
          type = 'html', 
          title = 'Table 1: Minority Share At Universities', 
          out = 'minority_table.htm')

#make a table with basic descriptive stats of tuition cost
stargazer(uni_data['sticker_price_2000'],
          type = 'html',
          title = 'Table 2: Tuition Cost Distribution',
          out = 'tuition_distrib.htm')
##some histograms-------------------------------------------------------------
#make histograms showing spread of each of the variables individually

##asian/ pacific islander
asia <- ggplot(data = uni_data, aes(x = asian_or_pacific_share_fall_2000))+
  geom_histogram(binwidth = 5,
                 fill = 'White',
                 color = 'Black') +
  labs(title = 'Asian/Pacific Islander Share at Universities', 
       x = 'Percent Share of Asian/Pacific Islander Students, 2000',
       y = 'Number of Universities')
asia


# black students
black <- ggplot(data = uni_data, aes(x = black_share_fall_2000))+
  geom_histogram(binwidth = 5,
                 fill = 'White',
                 color = 'Black') +
  labs(title = 'Black Share at Universities', 
       x = 'Percent Share of Black Students, 2000',
       y = 'Number of Universities')
black

#hispanic
hisp <- ggplot(data = uni_data, aes(x = hisp_share_fall_2000))+
  geom_histogram(binwidth = 5,
                 fill = 'White',
                 color = 'Black') +
  labs(title = 'Hispanic Share at Universities', 
       x = 'Percent Share of Hispanic Students, 2000',
       y = 'Number of Universities') 
hisp

#foreign
foreign <- ggplot(data = uni_data, aes(x = alien_share_fall_2000))+
  geom_histogram(binwidth = 5,
                 fill = 'White',
                 color = 'Black') +
  labs(title = 'Foreign Share at Universities', 
       x = 'Percent Share of Foreign Students, 2000',
       y = 'Number of Universities') 
foreign

#make histogram showing the spread of the tuition cost
#cost of tuition
enroll_cost <- ggplot(data = uni_data, aes(x = sticker_price_2000))+
  geom_histogram(binwidth = 750,
                 fill = 'White',
                 color = 'Black') +
  labs(title = 'Figure 2: Cost of Tuition Across Universities', 
       x = 'Cost of Tuition, 2000',
       y = 'Number of Universities') 
enroll_cost

#make a histogram of all the minorities stacked
stacked_hist <- ggplot(data = rel_data2, aes(x = bins, fill = group_name))+
  geom_histogram(bins = 10) +
  labs(title = 'Figure 1: Minority Share Across US Universities',
       x = 'Percent Share of Minority Students',
       y = 'Number of Universities',
       fill = 'Minority Groups') +
  scale_fill_discrete(labels = c('Foreign', 'Asian/Pacific Islander', 
                                 'Black', 'Hispanic')) +
  theme(legend.position= c(.8, .8))

stacked_hist


#bivariate scatterplots----------------------------------------------------
##make scatterplots with tuition as indep variable and each minor share as dep variable

#asian scatterplot vs cost of tuition
asian_tuition <- ggplot(data = uni_data, aes(y = asian_or_pacific_share_fall_2000,
                                             x = sticker_price_2000))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y ~x) +
  labs(title = 'Figure 3: Asian/Pacific Islander Attendance by Cost of Tuition',
       x = 'Cost of Tuition, 2000', 
       y = 'Percent Share of Asian/Pacific Islander Students, 2000')
asian_tuition

#black scatterplot vs cost of tuition
black_tuition <- ggplot(data = uni_data, aes(x = sticker_price_2000,
                                             y = black_share_fall_2000))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y~x) +
  labs(title = 'Figure 6: Black Attendance by Cost of Tuition',
       x = 'Cost of Tuition, 2000',
       y = 'Percent Share of Black Students, 2000')

black_tuition

#hispanic scatterplot vs cost of tuition
hisp_tuition <- ggplot(data = uni_data, aes(x = sticker_price_2000,
                                             y = hisp_share_fall_2000))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y~x) +
  labs(title = 'Figure 5: Hispanic Attendance by Cost of Tuition',
       x = 'Cost of Tuition, 2000',
       y = 'Percent Share of Hispanic Students, 2000')
hisp_tuition

#foreign scatterplot vs cost of tuition
foreign_tuition <- ggplot(data = uni_data, aes(x = sticker_price_2000,
                                             y = alien_share_fall_2000))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y~x) +
  labs(title = 'Figure 4: Foreign Attendance by Cost of Tuition',
       x = 'Cost of Tuition, 2000',
       y = 'Percent Share of Foreign Students, 2000')
foreign_tuition

#bivariate regressions -----------------------------------------------

#make tuition in thousands to make slope easier to interpret
uni_data$tuition <- uni_data$sticker_price_2000/1000

#check to make sure new variable is as expected
head(uni_data$tuition)


asian_reg <- lm(asian_or_pacific_share_fall_2000 ~ tuition,
                data = uni_data)
summary(asian_reg)

#run black regression
black_reg <- lm(black_share_fall_2000 ~ tuition,
                data = uni_data)
summary(black_reg)

#run hispanic regression
hisp_reg <- lm(hisp_share_fall_2000 ~ tuition,
                data = uni_data)
summary(hisp_reg)

#run foreign regression
foreign_reg <- lm(alien_share_fall_2000 ~ tuition,
                  data = uni_data)
summary(foreign_reg)


#put regressions together into one stargazer plot

stargazer(asian_reg, black_reg, 
                     hisp_reg, foreign_reg, 
          type = 'html', 
          dep.var.labels = c('Asian Share', 'Black Share', 'Hispanic Share', 'Foreign Share'),
          covariate.labels = c('Cost of Tuition (in thousands)'),
          title = 'Table 3: Bivariate Regressions', 
          out = 'bivariate_reg.htm')
