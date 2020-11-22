setwd("~/University/Data Analysis Y4")
hdi1 <- read.csv("Human-development-index.csv") 
#skip = 2 %>%
 # janitor::clean_names()
library("Hmisc")
library(Rmisc)
library(pgirmess)
library(tidyverse)
library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(stringr)
library(forcats)
library(janitor)
library(rlang)
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

file <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44025h2011.txt.gz&dir=data/historical/stdmet/"
readLines(file, n = 4)

buoy44025 <- read_table(file, 
                        col_names = FALSE,
                        skip = 2)
#git check
task2 <- scan(file, skip = 1, nlines = 1, what = character()) %>%
  str_remove("#") %>%
  str_replace("/", "_per_")

measure <- scan(file, 
                nlines = 1,
                what = character()) %>%
  str_remove("#")
# replacing the / with _per_ as / is a special character
units <- scan(file, 
              skip = 1,
              nlines = 1, 
              what = character()) %>% 
  str_remove("#") %>% 
  str_replace("/", "_per_")
# paste the variable name and its units together for the column names
names(buoy44025) <- paste(task2, units, sep = "_")


# Task 3
# define file name
filesol <- "../data-raw/Y101_Y102_Y201_Y202_Y101-5.csv"
sol <- read_csv(filesol, skip = 2) %>% 
  janitor::clean_names()

sol <- sol %>% 
  filter(str_detect(description, "OS=Homo sapiens")) %>% 
  filter(x1pep == "x")

# Extract the genename from the description and put it in a column.
sol <- sol %>%
  mutate(genename =  str_extract(description,"GN=[^\\s]+") %>% 
           str_replace("GN=", ""))

# trying it out on one value
accession <- sol$accession[2]
protid <- str_extract(accession, "1::[^;]+") %>% 
  str_replace("1::", "")

# adding a new column 
sol <- sol %>%
  mutate(protid =  str_extract(accession, "1::[^;]+") %>% 
           str_replace("1::", ""))

sol2 <- sol %>% pivot_longer(names_to = "lineage_rep",
                             values_to = "abundance",
                             cols = starts_with("y"))

sol2 <- sol2 %>%
  extract(lineage_rep,
          c("line", "rep"),
          "(y[0-9]{3,4})\\_([a-c])")

file <-  "../data-processed/sol2.txt"
write.table(sol2, 
            file, 
            quote = FALSE,
            row.names = FALSE)