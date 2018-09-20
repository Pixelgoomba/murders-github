#Let's explore how many murders per capita there are for each state in the United States 

library(tigris)
library(dplyr)
library(leaflet)

states <- states(cb=T)
states %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(popup=~NAME)

m2 <- rename(murders, 
             NAME=fstate_label)

mrds_state <- m2 %>%
  filter(Year >=2013 & Year <=2016) %>%
  group_by(NAME) %>%
  summarize(total=n())


states_merged_mrds <- geo_join(states, mrds_state, "NAME", "NAME")
pal <- colorNumeric("Reds", domain=states_merged_mrds$total)
states_merged_mrds <- subset(states_merged_mrds, !is.na(total))
popup_mrds <- paste0("Total: ", as.character(states_merged_mrds$total))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = states_merged_mrds, 
              fillColor = ~pal(states_merged_mrds$total), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_mrds) %>%
  addLegend(pal = pal, 
            values = states_merged_mrds$total, 
            position = "bottomright", 
            title = "Murders 2013 - 2016")

# Getting total population by state from the API

install.packages("censusapi")
library(censusapi)
library(tidycensus)

state_pop <- getCensus(name = "acs/acs5", 
                       vintage = 2016, 
                       vars = c("NAME", "B01003_001E"), 
                       region = "state:*")
head(state_pop)

colnames(state_pop) <- c("state_id", "NAME", "population")
state_pop$state_id <- as.numeric(state_pop$state_id)
state_off <- data.frame(state.abb, state.name)
head(state_off)

colnames(state_off) <- c("state", "NAME")
state_pop <- left_join(state_pop, state_off)

state_pop$state <- ifelse(state_pop$NAME=="District of Columbia", "DC", as.character(state_pop$state))
state_pop$state <- ifelse(state_pop$NAME=="Puerto Rico", "PR", as.character(state_pop$state))

mrds_state_pop <- left_join(mrds_state, state_pop)

mrds_state_pop$per_capita <- round(mrds_state_pop$total/mrds_state_pop$population*100000,2)
mrds_state_pop <- filter(mrds_state_pop, !is.na(per_capita))
head(mrds_state_pop)

states_merged_mrds_pc <- geo_join(states, mrds_state_pop, "STUSPS", "state")
pal_mrds <- colorNumeric("Reds", domain=states_merged_mrds_pc$per_capita)
states_merged_mrds_pc <- subset(states_merged_mrds_pc, !is.na(per_capita))

popup_mrds <- paste0("<strong>", states_merged_mrds_pc$NAME, 
                     "</strong><br />Total: ", states_merged_mrds_pc$total,
                     "<br />Per capita: ", 
                     as.character(states_merged_mrds_pc$per_capita))
head(popup_mrds)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = states_merged_mrds_pc , 
              fillColor = ~pal_mrds(states_merged_mrds_pc$per_capita), 
              fillOpacity = 0.9, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_mrds) %>%
  addLegend(pal = pal_mrds, 
            values = states_merged_mrds_pc$per_capita, 
            position = "bottomright", 
            title = "Murders<br />per 100,000<br/>residents")