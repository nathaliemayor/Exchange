library(dplyr)
library(tidyverse)

colname <- "a"
x <- data.frame("a" = seq(0,10),
           "b" = seq(20,30))

x %>% mutate({{colname}} := .[,colname] %>% as.character()) %>% str()
