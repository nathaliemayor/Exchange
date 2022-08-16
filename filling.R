# setup -------------------------------------------------------------------


rm(list=ls())


install.packages("clipr")

library(tidyverse)
library(clipr)

date_release <- read_excel("Book1.xlsx", sheet = 1) 
indices_monthly <- read_excel("Book1.xlsx", sheet = 2) 
truc_a_completer <- read_excel("Book1.xlsx", sheet = 3) 

date_release$month <- format(as.Date(date_release$date),'%Y-%m')
indices_monthly$month <- format(as.Date(indices_monthly$date),'%Y-%m')
all <- merge(date_release,indices_monthly, by='month')
new_all <- all[,!names(all) %in% c("month","date.y")]
colnames(new_all) <- c("date","indice3")

# cleaning   --------------------------------------------------------------

#Convert into an ISO formatting

new_all <-new_all %>% 
  mutate(date=as.Date(date),
         indice3 = as.numeric(indice3),
        # monthly= substring(as.character(date),1,nchar(date)+2), 
        # flemme de trouver mieux, mais ça marche haha (:
         new_val=1) %>%  as_tibble()

date_release_upgraded <- new_all %>%
  filter(new_val==1) %>%
  group_by(date) %>% 
  mutate(ID=cur_group_id()) %>%
  select(-new_val) %>% 
  as_tibble()

# newval=1 : "this line switches values"
# ID= change of value + order the changes

truc_a_completer <- truc_a_completer %>% 
  mutate(date=as.Date(date)) %>%
  select(date,indice3) %>% # for this exercice, only indice3 matters
  mutate(indice3=as.numeric(indice3)) %>% 
  arrange(date)%>%  as_tibble()

# -------------------------------------------------------------------------
# methode normale :

final<- truc_a_completer %>% 
  left_join(date_release_upgraded) %>% 
  group_by(ID)%>%
  arrange(date)%>% ungroup() %>% 
  fill(ID,.direction = "down") %>%
  group_by(ID) %>% fill(indice3)

final %>% view()

# ------------------------------------------------------------------------


# CALENDER METHOD : 

Calendar <- seq(as.Date("2021-12-03"),as.Date("2022-12-31"), by="days") %>%
  as.data.frame()

colnames(Calendar) <- c('date')


  Final2 <- date_release_upgraded %>%
    right_join(Calendar) %>%
    group_by(ID)%>%
    arrange(date)%>% ungroup() %>% 
    fill(ID,.direction = "down") %>%
    group_by(ID) %>% fill(indice3) #%>%View()
  
  
view(Final2)
  