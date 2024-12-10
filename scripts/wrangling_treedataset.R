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

# okay filter dataframe for CON_PROP_UNADJ >0.9

pines_dat <- read_csv("data/pines_plotsdat.csv")

pines_dat <- pines_dat %>% 
  filter(CONDPROP_UNADJ >= 0.9)

#okay now write this to csv 
write_csv(pines_dat, "data/pines_plotsdat.csv")


# okay now i will center and scale contunuous vars so they are comparable in terms of effect size --------

pines_dat <- read_csv("data/pines_plotsdat.csv")
pines_dat <- pines_dat %>%
  mutate(
    precip = as.vector(scale(precip)),
    temp = as.vector(scale(temp)),
    BALIVE = as.vector(scale(BALIVE))
  )



write_csv(pines_dat, "data/pines_plotsdat.csv")



### data wrangling continued 
pines_idaho_all <- read_csv("data/final_project.csv") %>% 
  filter(STATECD == 16) %>% 
  filter(SPCD == 101)


pines_idaho_filt <- pines_idaho_all %>% filter(CYCLE == 2)
#There are 348 unique plot composite ids
unique_plot_ids <- pines_idaho_filt %>% summarise(n_unique = n_distinct(PLOT_COMPOSITE_ID))
print(unique_plot_ids)

#Create new column called "TOTALTREE" that tallies number of trees for each unique PLOT_COMPOSITE_ID.
#Create new column called "TOTALDEAD" that tallies number of dead trees for each unique PLOT_COMPOITE_ID
#Create new column called "DEADPROP" that is the proportion of dead/total trees for each unique PLOT_COMPOSITE_ID

pines_idaho_filt <- pines_idaho_filt %>% 
  group_by(PLOT_COMPOSITE_ID) %>%
  mutate(TREESALL = n(), 
         TOTALDEAD = sum(STATUSCD == 2),
         DEADPROP = TOTALDEAD / TREESALL) %>%
  ungroup()

write_csv(pines_idaho_filt, 'data/wbp_allidaho_trees.csv')

# Summarize tree-level variables by PLOT
grouped_dat <- pines_idaho_filt %>%
  dplyr::select(-DIA, -PREV_STATUS_CD, -AGENTCD, -matches("^TREE_|DIA|DAMAGE")) %>%
  group_by(PLOT) %>%
  summarize(across(everything(), first)) %>% 
  dplyr::select(-2, -4, -5) %>% 
  ungroup()


grouped_dat$INSECT <- ifelse(grouped_dat$DSTRBCD1 >= 10 & grouped_dat$DSTRBCD1 <= 12, 1, 0)
grouped_dat$DISEASE <- ifelse(grouped_dat$DSTRBCD1 >= 20 & grouped_dat$DSTRBCD1 <= 22, 1, 0)
grouped_dat$FIRE <- ifelse(grouped_dat$DSTRBCD1 >= 30 & grouped_dat$DSTRBCD1 <= 32, 1, 0)


summary_insect <- summary(grouped_dat$DEADPROP[grouped_dat$INSECT == 1])
summary_disease <- summary(grouped_dat$DEADPROP[grouped_dat$DISEASE == 1])
summary_fire <- summary(grouped_dat$DEADPROP[grouped_dat$FIRE == 1])

grouped_dat$INSECT <- as.numeric(grouped_dat$INSECT)
grouped_dat$DISEASE <- as.numeric(grouped_dat$DISEASE)
grouped_dat$FIRE <- as.numeric(grouped_dat$FIRE)

grouped_dat$disturbance_type <- with(grouped_dat, ifelse(
  INSECT + DISEASE + FIRE == 0, "None",
  ifelse(INSECT == 1 & DISEASE == 0 & FIRE == 0, "INSECT",
         ifelse(DISEASE == 1 & INSECT == 0 & FIRE == 0, "DISEASE",
                ifelse(FIRE == 1 & INSECT == 0 & DISEASE == 0, "FIRE",
                       ifelse(INSECT == 1 & DISEASE == 1 & FIRE == 0, "INSECT, DISEASE",
                              ifelse(INSECT == 1 & FIRE == 1 & DISEASE == 0, "INSECT, FIRE",
                                     ifelse(DISEASE == 1 & FIRE == 1 & INSECT == 0, "DISEASE, FIRE",
                                            "INSECT, DISEASE, FIRE"
                                     )
                              )
                       )
                )
         )
  )
))

table(grouped_dat$disturbance_type)

write_csv(grouped_dat, "data/wbp_allidaho_plots.csv")
