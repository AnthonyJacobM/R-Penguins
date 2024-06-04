# Demo to look at the features of ggplot2
# S1: Load the necessary libraries
library(ggplot2)
library(tidyverse)
library(palmerpenguins)

# S2: Take a look at the data
# View the dataset of interest
data("penguins")
View(penguins)
str(penguins)

# S3: Clean and organize the data
# -- species -- #
df_species <- penguins %>% 
  group_by(species) %>% 
  drop_na() %>% # drop nulls before summarizing
  summarize(avg_bill_length_mm = mean(bill_length_mm), avg_bill_depth_mm = mean(bill_depth_mm), 
            avg_flipper_length_mm = mean(flipper_length_mm), avg_body_mass_g = mean(body_mass_g))

tail(df_species)

# -- island -- #
df_island <- penguins %>% 
  group_by(island) %>% 
  drop_na() %>% # drop nulls before summarizing
  summarize(avg_bill_length_mm = mean(bill_length_mm), avg_bill_depth_mm = mean(bill_depth_mm), 
            avg_flipper_length_mm = mean(flipper_length_mm), avg_body_mass_g = mean(body_mass_g))

tail(df_island)

# -- sex -- #
df_sex <- penguins %>% 
  group_by(sex) %>% 
  drop_na() %>% # drop nulls before summarizing
  summarize(avg_bill_length_mm = mean(bill_length_mm), avg_bill_depth_mm = mean(bill_depth_mm), 
            avg_flipper_length_mm = mean(flipper_length_mm), avg_body_mass_g = mean(body_mass_g))

tail(df_sex)

# -- Explore correlations between numeric variables of the original matrix -- #
library(corrplot)
df_num <- penguins %>%
  select(bill_depth_mm, bill_length_mm, body_mass_g, flipper_length_mm) %>% 
  drop_na()

corr_mat <- cor(as.matrix(df_num))
colnames(corr_mat) <- c("Bill Depth (mm)", "Bill Length (mm)", "Body Mass (g)", "Flipper Length (mm)")
rownames(corr_mat) <- colnames(corr_mat)
corrplot(corr_mat, tl.col = 'black')

corr_mat <- cor(as.matrix(df_num))
corrplot(corr_mat, tl.col = 'black')

# Alternatively drop all non-numeric cols
df2 <- penguins %>% select(where(is.numeric)) %>%  drop_na()
corr_mat <- cor(as.matrix(df2))
corrplot(corr_mat, tl.col = 'black')

# -- plotting -- #
# 1: flipper length vs body mass
ggplot(data = penguins, aes(flipper_length_mm, body_mass_g)) + # plot labels
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g)) + # geom - fill points
  geom_smooth(method = lm, se = FALSE) + # geom - linear smoothing and regression
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)", title = "Scatter Plot: Flipper Length vs Body Mass") +
  theme_linedraw()
ggsave("Scatter_Species_per_Island_Body_Mass_Flipper_Length.PNG")

# Scatterplot / Bubble Chart
# -- Demonstrate all modifications in a Bubble Chart -- #
ggplot(data=penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species, alpha = bill_length_mm)) +
  geom_point(data=penguins, aes(x = penguins$flipper_length_mm, y = penguins$body_mass_g, size = penguins$bill_depth_mm)) +
  #geom_smooth(aes(x = penguins$flipper_length_mm, y = penguins$body_mass_g)) +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)", title = "Scatter Plot: Species and Features") +
  theme_linedraw()
# -- #

# Facet Grid displays multiple windows #
ggplot(data=penguins, aes(x = flipper_length_mm, y = body_mass_g, color = sex, alpha = 0.5)) +
  geom_point(data=penguins, aes(x = penguins$flipper_length_mm, y = penguins$body_mass_g)) +
  #geom_smooth(aes(x = penguins$flipper_length_mm, y = penguins$body_mass_g)) +
  facet_grid(island ~ species) +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)", title = "Scatter Plot: Species per Island") +
  theme_linedraw()
# -- #

# 2: bill length vs body mass
ggplot(data = penguins, aes(bill_depth_mm, body_mass_g)) +
  geom_point(mapping = aes(x = bill_depth_mm, y = body_mass_g)) +
  geom_smooth(method = lm, se = FALSE) + facet_wrap(penguins$species) +
  labs(x = "Bill Depth (mm)", y = "Body Mass (g)", title = "Scatter Plot: Bill Depth vs Body Mass") +
  theme_linedraw()
# 3: bill depth vs bill length
ggplot(data = penguins, aes(bill_depth_mm, bill_length_mm)) +
  geom_point(mapping = aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_smooth(method = lm, se = FALSE) + facet_wrap(penguins$species) +
  labs(x = "Bill Depth (mm)", y = "Bill Length (mm)", title = "Scatter Plot: Bill Depth vs Bill Length") +
  theme_linedraw()
# 4: bill depth vs body mass
ggplot(data = penguins, aes(bill_depth_mm, body_mass_g, color = species)) +
  geom_point(mapping = aes(x = bill_depth_mm, y = body_mass_g)) +
  geom_smooth(method = lm, se = FALSE) + facet_wrap(penguins$species) + 
  labs(x = "Bill Depth (mm)", y = "Body Mass (g)", title = "Scatter Plot: Bill Depth vs Body Mass") +
  theme_linedraw()
# 5: bar chart of species
ggplot(data=penguins) +
  geom_bar(data=penguins, aes(penguins$species)) + 
  labs(x = "Species", y = "Frequency", title = "Bar Chart") +
  theme_linedraw()
# 6: Boxplot - species vs flipper length
ggplot(data=penguins, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot(data=penguins, aes(penguins$species, penguins$flipper_length_mm)) + 
  labs(x = "Species", y = "Flipper Length (mm)", title = "Box Plot: Species vs Flipper Length") +
  theme_linedraw()
# 7: Boxplot - species vs bill length
ggplot(data=penguins, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_boxplot(data=penguins, aes(penguins$species, penguins$bill_length_mm)) + 
  labs(x = "Species", y = "Bill Length (mm)", title = "Box Plot: Species vs Bill Length") +
  theme_linedraw()
# 8: Boxplot - species vs bill depth
ggplot(data=penguins, aes(x = species, y = bill_depth_mm, fill = species)) +
  geom_boxplot(data=penguins, aes(penguins$species, penguins$bill_depth_mm)) + 
  labs(x = "Species", y = "Bill Depth (mm)", title = "Box Plot: Species vs Bill Depth") +
  theme_linedraw()
# 9: Boxplot - species vs body mass
ggplot(data=penguins, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot(data=penguins, aes(penguins$species, penguins$body_mass_g)) + 
  labs(x = "Species", y = "Body Mass (g)", title = "Box Plot: Species vs Body Mass") +
  theme_linedraw()


# Example with smoothing
# ggplot(data=penguins, aes(x = flipper_length_mm, y = body_mass_g, alpha = bill_length_mm)) +
#   geom_point(data=penguins, aes(x = penguins$flipper_length_mm, y = penguins$body_mass_g)) +
#   #facet_wrap(penguins$species) +
#   geom_smooth(aes(x = penguins$flipper_length_mm, y = penguins$body_mass_g)) +
#   theme_linedraw()

# - Use loess smoothing for data with less than 1k points - default
ggplot(data=penguins, aes(x = flipper_length_mm, y = body_mass_g, alpha = bill_length_mm)) +
  geom_point(data=penguins, aes(x = penguins$flipper_length_mm, y = penguins$body_mass_g)) +
  #facet_wrap(penguins$species) +
  geom_smooth(method = 'loess', aes(x = penguins$flipper_length_mm, y = penguins$body_mass_g)) +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)", title = "Scatter Plot w/ Loess Smoothing") +
  theme_linedraw()

# - Use gam smoothing for data with large number of points - more time consuming
ggplot(data=penguins, aes(x = flipper_length_mm, y = body_mass_g, alpha = bill_length_mm)) +
  geom_point(data=penguins, aes(x = penguins$flipper_length_mm, y = penguins$body_mass_g)) +
  #facet_wrap(penguins$species) +
  geom_smooth(method = 'gam', aes(x = penguins$flipper_length_mm, y = penguins$body_mass_g)) +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)", title = "Scatter Plot w/ Gam Smoothing") +
  theme_linedraw()


# Example with Bar Chart - visualize the categorical data
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut)) + 
  labs(x = "Cut", y = "Frequency", title = "Bar Chart") + 
  theme_linedraw()
  

bar2 <- ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity, color = cut)) + 
  labs(x = "Cut", y = "Frequency", title = "Stacked Bar Chart") + 
  theme_linedraw()

# Annotate a section of the bar chart
bar2 + annotate("text", label="Most common", x = "Ideal", y = 23000) + 
       annotate("text", label="2nd Least Common", x = "Good", y = 10000, angle = 90)

# Explore the varying distribution of a color and cut of a diamond
> ggplot(data = diamonds) + geom_bar(mapping = aes(x = color, fill = cut)) + facet_wrap(~clarity) +
  labs(x = "Color", y = "Frequency", title = "Stacked Bar Chart: Color vs Cut") + 
  theme_linedraw()

