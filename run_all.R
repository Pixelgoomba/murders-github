#Importing murders from database

packages <- c("devtools", "dplyr", "foreign")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(foreign)
library(dplyr)


temp <- tempfile()
unzip("data/SHR76_16.sav.zip", exdir="data", overwrite=T)
unlink(temp)


data_labels <- read.spss("data/SHR76_16.sav", to.data.frame=TRUE)
data_only <- read.spss("data/SHR76_16.sav", to.data.frame=TRUE, use.value.labels=F)


new_labels <- select(data_labels,
                     ID, CNTYFIPS, Ori, State, Agency, AGENCY_A,
                     Agentype_label=Agentype,
                     Source_label=Source,
                     Solved_label=Solved,
                     Year,
                     Month_label=Month,
                     Incident, ActionType,
                     Homicide_label=Homicide,
                     Situation_label=Situation,
                     VicAge,
                     VicSex_label=VicSex,
                     VicRace_label=VicRace,
                     VicEthnic, OffAge,
                     OffSex_label=OffSex,
                     OffRace_label=OffRace,
                     OffEthnic,
                     Weapon_label=Weapon,
                     Relationship_label=Relationship,
                     Circumstance_label=Circumstance,
                     Subcircum, VicCount, OffCount, FileDate,
                     fstate_label=fstate,
                     MSA_label=MSA)

## OK, we're dropping ID, CNTYFIPS, Ori, State, Agency, and AGENCY_A columns
## And we're going to rename the other ones so that we know they're specifically values


new_data_only <- select(data_only,
                        Agentype_value=Agentype,
                        Source_value=Source,
                        Solved_value=Solved,
                        Month_value=Month,
                        Homicide_value=Homicide,
                        Situation_value=Situation,
                        VicSex_value=VicSex,
                        VicRace_value=VicRace,
                        OffSex_value=OffSex,
                        OffRace_value=OffRace,
                        Weapon_value=Weapon,
                        Relationship_value=Relationship,
                        Circumstance_value=Circumstance,
                        fstate_value=fstate,
                        MSA_value=MSA)

# cbind() means column binding-- it only works if the number of rows are the same

murders <- cbind(new_labels, new_data_only)

# Now we're going to use the select() function to reorder the columns so labels and values are next to each other

murders <- select(murders,
                  ID, CNTYFIPS, Ori, State, Agency, AGENCY_A,
                  Agentype_label, Agentype_value,
                  Source_label, Source_value,
                  Solved_label, Solved_value,
                  Year,
                  Month_label, Month_value,
                  Incident, ActionType,
                  Homicide_label,Homicide_value,
                  Situation_label,Situation_value,
                  VicAge,
                  VicSex_label,VicSex_value,
                  VicRace_label,VicRace_value,
                  VicEthnic, OffAge,
                  OffSex_label,OffSex_value,
                  OffRace_label,OffRace_value,
                  OffEthnic,
                  Weapon_label,Weapon_value,
                  Relationship_label,Relationship_value,
                  Circumstance_label,Circumstance_value,
                  Subcircum, VicCount, OffCount, FileDate,
                  fstate_label,fstate_value,
                  MSA_label,MSA_value)

# remove the old data frames because they're huge and we want to free up memory
rm(data_labels)
rm(data_only)
rm(new_labels)
rm(new_data_only)
rm(packages)
rm(temp)
file.remove("data/SHR76_16.sav")

##Visualizing data

install.packages("learnr")
install.packages("rmarkdown")
install.packages("tidyverse")
install.packages("ggrepel")
install.packages("wesanderson")
install.packages("ggplot2")

library(ggplot2)
library(dplyr)

glimpse(murders)

#Filter of murders by state

murders_state <- murders %>% 
  group_by(State) %>% 
  summarize(total=n()) %>% 
  arrange(desc(total)) %>% 
  mutate(percent=round(total/sum(total)*100,2))

##Bar comparison of murders by year

ggplot(data=murders,
       aes(x=Year)) +
  geom_density(fill="red")+
  labs(x="Year", y="density",fill="Year",
       title = "Density of murders by Year",
       subtitle = "R for Journalists class",
       caption = "Murder Accountability Project.")

ggplot(murders,
       aes(x=Year)) +
  geom_bar()+
  labs(x="Year", y="murders",fill="Year",
       title = "Comparison of murders by Year",
       subtitle = "R for Journalists class",
       caption = "Murder Accountability Project.") +
  scale_fill_manual(values=c("aquamarine", "darkorchid", "deepskyblue2", "lemonchiffon2"))

##Bar comparison of murders by gender
ggplot(murders,
       aes(x=Year, fill=VicSex_label)) +
  geom_bar()+
  labs(x="Year", y="murders",fill="Gender",
       title = "Comparison of murders by gender",
       subtitle = "for R for Journalists class",
       caption = "Murder Accountability Project.") +
  theme_minimal()

#Let's explore how many murders per capita there are for each state in the United States in 2016

library(tigris)
library(dplyr)
library(leaflet)

m2 <- rename(murders, 
             NAME=fstate_label)

mrds_state2 <- m2 %>%
  filter(Year == 2016) %>%
  group_by(NAME) %>%
  summarize(total=n())

# Getting total population by state from the API

state_pop <- getCensus(name = "acs/acs5", 
                       vintage = 2016, 
                       vars = c("NAME", "B01003_001E"), 
                       region = "state:*")

colnames(state_pop) <- c("state_id", "NAME", "population")
state_pop$state_id <- as.numeric(state_pop$state_id)
state_off <- data.frame(state.abb, state.name)
head(state_off)

colnames(state_off) <- c("state", "NAME")
state_pop <- left_join(state_pop, state_off)

state_pop$state <- ifelse(state_pop$NAME=="District of Columbia", "DC", as.character(state_pop$state))
state_pop$state <- ifelse(state_pop$NAME=="Puerto Rico", "PR", as.character(state_pop$state))
state_pop$state <- ifelse(state_pop$NAME=="Alaska", "AK", as.character(state_pop$state))

mrds_state_pop2 <- left_join(mrds_state2, state_pop)

mrds_state_pop2$per_capita <- round(mrds_state_pop2$total/mrds_state_pop2$population*100000,2)
mrds_state_pop2 <- filter(mrds_state_pop2, !is.na(per_capita))
head(mrds_state_pop2)

states_merged_mrds_pc2 <- geo_join(states, mrds_state_pop2, "STUSPS", "state")
pal_mrds2 <- colorNumeric("Reds", domain=states_merged_mrds_pc2$per_capita)
states_merged_mrds_pc2 <- subset(states_merged_mrds_pc2, !is.na(per_capita))

popup_mrds2 <- paste0("<strong>", states_merged_mrds_pc2$NAME, 
                      "</strong><br />Total: ", states_merged_mrds_pc2$total,
                      "<br />Per capita: ", 
                      as.character(states_merged_mrds_pc2$per_capita))
head(popup_mrds2)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = states_merged_mrds_pc2 , 
              fillColor = ~pal_mrds2(states_merged_mrds_pc2$per_capita), 
              fillOpacity = 0.9, 
              weight = 0.2, 
              smoothFactor = 0.5, 
              popup = ~popup_mrds2) %>%
  addLegend(pal = pal_mrds2, 
            values = states_merged_mrds_pc2$per_capita, 
            position = "bottomright", 
            title = "Murders<br />per 100,000<br/>residents - 2016")

##I'll try to find to Richard Ramirez an American serial killer Dubbed the 'Night Stalker
#killed at least 14 people and tortured dozens more mostly during the spring and summer of 1985.
#first known murder on June 28, 1984 - the victim was 79-year-old Jennie Vincow, who was sexually assaulted, stabbed and killed
#pattern for the killer: The husband was shot first, then the wife was brutally assaulted and stabbed to death.
#LA - San Francisco
#Apprehended in August 1985

library(readr)

county.fips <- read_csv("data/fips_counties.csv")

# FIPS change over time. Data tends to do that when you've got decades of stuff
# We'll swap out some County Names (most are from Alaska) before we join the data sets

murders  <- murders %>%
  mutate(CNTYFIPS=as.numeric(as.character(CNTYFIPS))) %>% 
  mutate(CNTYFIPS=case_when(
    CNTYFIPS==51560 ~ 51005,
    CNTYFIPS==2232 ~ 2105,
    CNTYFIPS==2280 ~ 2195,
    CNTYFIPS==2201 ~ 2198,
    TRUE ~ CNTYFIPS
  )) %>% 
  left_join(county.fips, by=c("CNTYFIPS"="fips"))


n_stalker1 <- murders %>% 
  filter(State=="California" & name_of_county=="Los Angeles"|
           name_of_county=="San Francisco") %>%
  filter(Year==1985) %>%
  filter(Month_value>=3 & Month_value<=8) %>% 
  filter(Solved_label=="Yes") %>%
  filter(OffSex_label=="Male") %>% 
  filter(OffAge==25) %>% 
  filter(Weapon_label=="Handgun - pistol, revolver, etc"|
           Weapon_label=="Firearm, type not stated") %>% 
  filter(Homicide_label=="Murder and non-negligent manslaughter")

