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
