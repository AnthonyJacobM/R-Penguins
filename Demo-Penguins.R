#install.packages("palmerpenguins") # install the library to run demo - data viz.
library("palmerpenguins") # load the library
data(package = 'palmerpenguins') # load the data

# Two datasets: penguins and penguins_raw - use ?penguins to better understand

View(penguins) # View the dataset in tabular form
head(penguins) # look at the first few records
summary(penguins) # get categorical and quantitative summaries by field


#install.packages("tidyverse") # install data analysis platform
library("tidyverse") # load the analysis platform
# count the number of penguins per species (categorical) type
penguins %>% 
  count(species) 

# group the data by species and average the numeric fields.
penguins %>%
  group_by(species) %>%
  summarise(across(where(is.numeric), mean, na.rm=TRUE))


# create a visualization to see the distributions by species (flipper_length)
library(ggplot2)
library(dplyr)
p <- penguins %>%
  ggplot( aes(x=flipper_length_mm,fill=species)) +
          geom_histogram(alpha = 0.6, position = 'identity') +
          theme_light() +
          labs(fill="")


p2 <- penguins %>%
  ggplot(aes(x=flipper_length_mm, y=body_mass_g))+
  geom_point(aes(color=species, alpha=0.6))+
  theme_light()+
  labs(fill="")
