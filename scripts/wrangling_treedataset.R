## Cecilia Martinez
## November 25, 2024
## This script is to wrangle the dataset that we will be using in our analysis 
## for our final project in STATS574E



# load packages -----------------------------------------------------------

library(readr) # for reading data
library(dplyr) # for wrangling data



# read in the data --------------------------------------------------------

data <- read_csv("data/final_proj.csv")


# subset based on pines, so anything in 100s species code of the dataset: 

data <- data %>% filter(SPCD %in% c(100, 101, 102, 135, 103, 104, 105, 140, 115, 116, 117, 
                                  118, 142, 133, 119, 120, 136, 121, 122, 123, 138, 124, 141, 
                                  125, 126, 127, 128, 114, 129, 130, 131, 139, 132, 137, 143, 
                                  144, 107, 108, 109, 134, 110, 106, 111, 112, 113))


# write final version of data frame with only pines -------------------------------------

write_csv(data, "data/pinesonly_data.csv")
data <- read_csv("data/pinesonly_data.csv")

#filter for most recent complete inventory cycle

filtered_data <- data %>% filter(CYCLE == 2)

#There are 348 unique plot composite ids
unique_plot_ids <- filtered_data %>% summarise(n_unique = n_distinct(PLOT_COMPOSITE_ID))
print(unique_plot_ids)

#Create new column called "TOTALTREE" that tallies number of trees for each unique PLOT_COMPOSITE_ID.
#Create new column called "TOTALDEAD" that tallies number of dead trees for each unique PLOT_COMPOITE_ID
#Create new column called "DEADPROP" that is the proportion of dead/total trees for each unique PLOT_COMPOSITE_ID

data <- filtered_data %>% 
  group_by(PLOT_COMPOSITE_ID) %>%
  mutate(TREESALL = n(), 
         TOTALDEAD = sum(STATUSCD == 2),
         DEADPROP = TOTALDEAD / TREESALL) %>%
  ungroup()

#write version of data with added columns and filtered for inventory cycle year 2

write_csv(data, "data/pines_filtereddata.csv")

# create dataframe that's just on the plot_level
pines_dat <- read_csv("data/pines_filtereddata.csv")


# Summarize tree-level variables by PLOT
grouped_dat <- pines_dat %>%
  dplyr::select(-DIA, -PREV_STATUS_CD, -AGENTCD, -matches("^TREE_|DIA|DAMAGE")) %>%
  group_by(PLOT) %>%
  summarize(across(everything(), first)) %>% 
  dplyr::select(-2, -4, -5) %>% 
  ungroup()

# write csv for plot level analaysis - no individual trees
  
write_csv(grouped_dat, "data/pines_plotsdat.csv")
