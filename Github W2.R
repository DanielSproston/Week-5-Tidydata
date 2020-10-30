setwd("~/University/Data Analysis Y4")
hdi1 <- read.csv("Human-development-index.csv") 
#skip = 2 %>%
 # janitor::clean_names()

library(tidyverse)
library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(stringr)
library(forcats)
library(janitor)
view(hdi1)

hdi1 %>% summary()

hdi1 %>% 
  drop_na() %>% 
  summary()

hdi <- gather(data = hdi1, key = "Year", value = "HDI", 3:32)
str(hdi)
#hdi_no_na <- is.na(hdi) 
#str(hdi_no_na)

hdi_no_na <- na.omit(hdi) 

#is.na_remove <- hdi$HDI.Rank..2018.[!is.na(hdi$HDI.Rank..2018.)]
#view(is.na_remove)

hdi_summary <- hdi_no_na %>% 
  group_by(Country) %>%  
  summarise(mean_index = mean(HDI))


hdi_summary <- hdi_no_na %>% 
  group_by(Country) %>% 
  summarise(mean_index = mean(HDI),
            n = length(HDI), 
            sd = sd(HDI),
            se = sd/sqrt(n))


#sqrt = square root
#S.E.=s.d.n???

hdi_summary_low <- hdi_summary %>% 
  filter(rank(mean_index) < 11)

hdi_summary_low

hdi_summary_low %>% 
  ggplot() +
  geom_point(aes(x = Country,
                 y = mean_index)) +
  geom_errorbar(aes(x = Country,
                    ymin = mean_index - se, 
                    ymax = mean_index + se)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()

#Task 2

