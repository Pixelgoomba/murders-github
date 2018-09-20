library(dplyr)
glimpse(murders)

##I'll try to find to Richard Ramirez an American serial killer Dubbed the 'Night Stalker
#killed at least 14 people and tortured dozens more mostly during the spring and summer of 1985.
#first known murder on June 28, 1984 - the victim was 79-year-old Jennie Vincow, who was sexually assaulted, stabbed and killed
#pattern for the killer: The husband was shot first, then the wife was brutally assaulted and stabbed to death.
#LA - San Francisco
#Apprehended in August 1985


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

ggplot(murders,
       aes(x=Year)) +
  geom_bar()

##Bar
ggplot(murders,
       aes(x=Year, fill=VicSex_label)) +
  geom_bar()+
  labs(x="Year", y="murders",fill="Gender",
       title = "Comparison of murders by gender",
       subtitle = "for R for Journalists class",
       caption = "Murder Accountability Project.") +
  theme_minimal()

ggsave("Murders-by-gender.png", width=40, height=30, units="cm")
  

  


