##################################################
##### ---- Into the tidyverse R tutorial---- #####
##################################################


#### ------ 0. We will be using the penguins native R dataset ---- ####

data(penguins)
?penguins

####------- 0.1. Install tidyverse & make dataframe --------####
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

penguin_data <- penguins

####------- 1. dplyr ------####
### data wrangling, makes it easy to chain operations
### How to work with data

# With these tools we can ask: Which penguin species have the greatest bill length
# and greatest weight on average?

## a. Piping

# piping allows you to continue one function to the next, 
# making code easier to read and write
head(penguin_data, 10)

penguin_data %>% head(10)


###- Column functions -##

## b. select() - shows us only these columns
penguin_data %>%
  select(species, island, bill_len, flipper_len, body_mass, sex, year) 
#or
penguin_data %>%
  select(-'bill_dep') #removes only this column

## c. mutate() - creates a new column that is based on the input you gave
penguins_with_kg <- penguin_data %>%
  select(species, island, bill_len, flipper_len, body_mass, sex, year) %>%
  mutate(body_mass_kg = body_mass / 1000) %>%
  rename(body_mass_g = body_mass, bill_length_mm = bill_len) %>%
  select(species, island, bill_length_mm, flipper_len, body_mass_kg, body_mass_g, sex, year)


### Row functions

## d. filter() - grabs rows that only have these exact qualifications
penguins_with_kg %>%
  select(species, island, bill_length_mm, flipper_len, body_mass_kg, sex, year) %>%
  filter(bill_length_mm >= 38, body_mass_kg >= 4.0, sex == 'female')

## e. distinct() - shows us only unique species with which the filter requirements occur
penguins_with_kg %>%
  select(species, island, bill_length_mm, flipper_len, body_mass_kg, sex, year) %>%
  filter(bill_length_mm >= 38, body_mass_kg >= 4.0, sex == 'female')%>%
  distinct(species)

## f. and g. group_by() and summarise() - 
penguins_with_kg %>%
  select(species, island, bill_length_mm, flipper_len, body_mass_kg, sex, year) %>%
  group_by(species) %>%
  summarise(avg_mass = mean(body_mass_kg, na.rm = TRUE))

## h. arrange() - put species with the greatest average mass in descending order
penguins_with_kg %>%
  select(species, island, bill_length_mm, flipper_len, body_mass_kg, sex, year) %>%
  group_by(species) %>%
  summarise(avg_mass = mean(body_mass_kg, na.rm = TRUE)) %>%
  arrange(desc(avg_mass), species)

## How to save your new dataset
write.csv(penguins_with_kg, "this_is_my_penguin_data.csv")



####------ 2. ggplot2 ---------####
### data analysis and visualization

# aes() - aesthetics - visual properties of the plot - designating your x and y axes
# geom_point() - adds a layer of points to the plot
# geom_smooth - adds trend line, in this case based on a linear model
# labs - Refine title, x, and y axes

# With these tools we can ask: What is the relationship between bill length and
# body mass across islands, according to species?

## Load data and axes
penguins_with_kg %>%
  ggplot(aes(x = bill_length_mm, y = body_mass_g, color = island))

# Load scatter plot points
penguins_with_kg %>%
  ggplot(aes(x = bill_length_mm, y = body_mass_g, color = island)) +
  geom_point()

# Load trend line in linear model
penguins_with_kg %>%
  ggplot(aes(x = bill_length_mm, y = body_mass_g, color = island)) +
  geom_point() +
  geom_smooth(method = "lm")

# With species represented by shapes
penguins_with_kg %>%
  ggplot(aes(x = bill_length_mm, y = body_mass_g, color = island)) +
  geom_point(aes(shape = species)) +
  geom_smooth(method = "lm")

# With title and axes refined
penguins_with_kg %>%
  ggplot(aes(x = bill_length_mm, y = body_mass_g, color = island)) +
  geom_point(aes(shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass (g) and Bill length (mm)",
    x = "Bill length (mm)", y = "Body mass (g)",
    color = "Island", shape = "Island"
  )

## Box whisker plot 
penguins_with_kg %>%
  ggplot(aes(x = island, y = body_mass_g, color = island)) +
  geom_boxplot()

## Violin plot
penguins_with_kg %>%
  ggplot(aes(x = island, y = body_mass_g, color = island)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun = mean,
               geom = "point", size = 2, color = "black")

## Bar chart
penguins_with_kg %>%
  ggplot(aes(x = island, fill = island)) +
  geom_bar()

## Histogram
penguins_with_kg %>%
  ggplot(aes(x = body_mass_g)) +
  geom_histogram(binwidth = 150)

## Density plot
penguins_with_kg %>%
  ggplot(aes(x = body_mass_g, color = island)) +
  geom_density(linewidth = 0.5)

## How to save your plot
ggsave(filename = "this_is_my_penguin_plot.png")

## Combine ggplot2 and dplyr??
penguins_with_kg %>%
  filter(body_mass_g >= 4000) %>%
  ggplot(aes(x = island, y = body_mass_g)) +
  geom_boxplot()


####------ 3. tidyr & tibble ---------####

## For this example we will make our own data table
## This table presents three different penguins whose bill length was
## measured at three separate times

df <- tribble(
  ~penguin,  ~bill_length_mm1, ~bill_length_mm2, ~bill_length_mm3,
  "A",  100,  105, 110,
  "B",  140,  142, 145,
  "C",  120,  125, 130
)

# To transform this data so that each column is a single variable, and each row
# is a value, we will use the pivot_longer function

# cols selects columns with these names
# names_to moves time of measurement of bill length 1-3 to a column named measurement
# values_to moves values of the bill length columns to a column named value
df_longer <- df

df_longer %>%
  pivot_longer(
    cols = bill_length_mm1:bill_length_mm3,
    names_to = "measurement",
    values_to = "bill_length_mm"
  )

# remove text from measurement values
df_longer %>%
  pivot_longer(
    cols = bill_length_mm1:bill_length_mm3,
    names_to = "measurement",
    values_to = "bill_length_mm") %>%
  mutate(
    measurement = parse_number(measurement))


## Next we will use pivot_wider
#First we make a dataframe
df_wider <- df

df_wider <- tribble(
  ~penguin, ~measurement, ~bill_length_mm,
  "A",        "bill_length_mm1",    100,
  "B",        "bill_length_mm1",    140,
  "B",        "bill_length_mm2",    142, 
  "A",        "bill_length_mm2",    145,
  "A",        "bill_length_mm3",    110
)

## pivot_wider()
# This function will do the opposite of pivot_longer
# We will start by again, making a dataset

df_wider %>% 
  pivot_wider(
    names_from = measurement,
    values_from = bill_length_mm
  )


####------ 4. readr ---------####

## reading in csv files with read.csv()

Penguins_nests <- read.csv("data/AllCounts_V_4_4.csv")

## Or can be done using path to wherever the data is located

####------ 5. forcats and stringr-------####

# Select only rows that are recorded as adult counts
Penguins_just_adults <- Penguins_nests %>%
  filter(str_detect(site_name, "Island")) %>%
  filter(count_type == 'adults')

# Plot these points, but they are super hard to read!
Penguins_just_adults %>%
  group_by(year, common_name) %>%
  summarise(mean_adults_count = mean(penguin_count)) %>%
  ggplot(aes(year, mean_adults_count, color = common_name)) +
  geom_point()

# Log transforming makes the distribution easier to see
Penguins_just_adults %>%
  group_by(year, common_name) %>%
  summarise(mean_adults_count = mean(penguin_count)) %>%
  ggplot(aes(year, mean_adults_count, color = common_name)) +
  geom_point() +
  scale_y_log10()

# Use fct_reorder() - reorders common_names by the average count of adults in the dataset
Penguins_just_adults %>%
  group_by(year, common_name) %>%
  summarise(mean_adults_count = mean(penguin_count)) %>%
  mutate(common_name = fct_reorder(common_name, mean_adults_count)) %>%
  ggplot(aes(year, mean_adults_count, color = common_name)) +
  geom_point() +
  scale_y_log10()
