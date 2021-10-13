list.of.packages <- c("tidyverse", "sf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))

df <- read_delim(file = "Data/base-ic-evol-struct-pop-2013.csv", delim = ",", col_names = TRUE, skip = 5, locale = locale(encoding = "UTF-8"))

iris <- df %>% 
  filter(DEP=="75") %>%
  select(IRIS, COM, TYP_IRIS, P13_POP, P13_POP0014:P13_POP75P) 

iris <- df %>% 
  filter(DEP=="75") %>%
  select(IRIS, COM, TYP_IRIS, P13_POP, P13_POP0014:P13_POP75P) %>%
  mutate(TYP_IRIS = as.factor(TYP_IRIS)) %>%
  mutate_at(vars(P13_POP0014:P13_POP75P), funs(pc=./P13_POP))

iris <- df %>% 
  filter(DEP=="75") %>%
  select(IRIS, COM, TYP_IRIS, P13_POP, P13_POP0014:P13_POP75P) %>%
  mutate(TYP_IRIS = as.factor(TYP_IRIS)) %>%
  mutate_at(vars(P13_POP0014:P13_POP75P), funs(pc=./P13_POP)) %>%
  mutate_if(is.numeric, funs(ifelse(is.nan(.), 0, .)))

iris <- df %>% 
  filter(DEP=="75") %>%
  select(IRIS, COM, TYP_IRIS, P13_POP, P13_POP0014:P13_POP75P) %>%
  mutate(TYP_IRIS = as.factor(TYP_IRIS)) %>%
  mutate_at(vars(P13_POP0014:P13_POP75P), funs(pc=./P13_POP)) %>%
  mutate_if(is.numeric, funs(ifelse(is.nan(.), 0, .))) %>%
  mutate(name_arrd = substr(COM, 4, 5)) %>%
  mutate(name_arrd = paste0("Paris ", name_arrd)) 

arrd <- iris %>% 
  select(COM, P13_POP, P13_POP0014:P13_POP75P) %>%
  group_by(COM) %>%
  summarise_all(funs(sum(.))) %>%
  ungroup %>%
  mutate_at(vars(P13_POP0014:P13_POP75P), funs(pc=./P13_POP)) %>%
  mutate_if(is.numeric, funs(ifelse(is.nan(.), 0, .)))

long <- arrd %>%
  gather(key = population_variable, value = value, -COM)

wide <- long %>%
  spread(key = population_variable, value = value)

iris <- df %>% 
  filter(DEP=="75") %>%
  select(IRIS, COM, TYP_IRIS, P13_POP, P13_POP0014:P13_POP75P) %>%
  mutate(TYP_IRIS = as.factor(TYP_IRIS)) %>%
  mutate_at(vars(P13_POP0014:P13_POP75P), funs(pc=./P13_POP)) %>%
  mutate_if(is.numeric, funs(ifelse(is.nan(.), 0, .))) %>%
  mutate(name_arrd = substr(COM, 4, 5)) %>%
  mutate(name_arrd = paste0("Paris ", name_arrd)) %>%
  write_csv("Output/iris.csv") %>%
  write_rds("Output/iris.rds") 

arrd <- iris %>% 
  select(COM, P13_POP, P13_POP0014:P13_POP75P) %>%
  group_by(COM) %>%
  summarise_all(funs(sum(.))) %>%
  ungroup %>%
  mutate_at(vars(P13_POP0014:P13_POP75P), funs(pc=./P13_POP)) %>%
  mutate_if(is.numeric, funs(ifelse(is.nan(.), 0, .))) %>%
  write_csv("Output/arrd.csv") %>%
  write_rds("Output/arrd.rds") 

irisshp <- read_sf(dsn = "Data/iris", layer = "CONTOURS-IRIS") %>%
  select(IRIS=CODE_IRIS) %>%
  right_join(iris, by="IRIS")

iristoplot <- irisshp %>%
  select(P13_POP75P_pc) 

plot(iristoplot)


arrdshp <- read_sf(dsn = "Data/arrondissements", layer = "arrondissements") %>%
  select(COM=c_arinsee) %>%
  mutate(COM=as.character(COM)) %>%
  left_join(arrd, by="COM") %>%
  select(P13_POP75P_pc)

plot(arrdshp)









