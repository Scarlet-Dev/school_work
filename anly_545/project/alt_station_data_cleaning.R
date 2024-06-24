library(tidyverse)
library(ggmosaic)
library(stringr)
library(corrplot)
library(kableExtra)
library(knitr)
library(vcd)

replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", 
                     gsub("\\ ", "", x)))
}


# Alternative Fuel Stations
raw_afs <- readr::read_csv('./raw_data/alt_fuel_stations (Apr 30 2022).csv')

# Get historic alt fuel stations per year
historic_afs <- readxl::read_xlsx('./raw_data/historical_afs_count_2020.xlsx', 1, col_names = FALSE)
colnames(historic_afs) <- historic_afs[2,]
historic_afs <- historic_afs[-c(1, 2), ]

# historic for tristate
tri.historic.ev <- historic_afs %>% filter(State == "New York" | 
                                             State == "New Jersey" | State == "Connecticut") %>% 
  select(State, ``Electrica (stations / charging outlets Level 1 / Level 2 / DC Fast)``) %>% 
  rename(Electric.Stations = `Electrica (stations / charging outlets Level 1 / Level 2 / DC Fast)`)

tri.historic.ev[c('Electric.Stations','Charging.Outlets', 'Charging.Lv1', 'Charging.Lv2', 'Dc.Fast')] <- 
  str_split_fixed(string = tri.historic.ev$Electric.Stations, pattern = ' / ', 5) %>% 
  trimws(which = "both")

tri.historic.ev[c(2:6)] <- apply(tri.historic.ev[c(2:6)], FUN = replaceCommas, MARGIN = 2)

knitr::kable(tri.historic.ev)


# Get EV registration
reg_ev_state <- readxl::read_xlsx('./raw_data/10962-ev-registration-counts-by-state_6-11-21.xlsx', 1)
reg_ev_state <- reg_ev_state[,c(1,2)]
colnames(reg_ev_state) <- reg_ev_state[1,]
reg_ev_state <- reg_ev_state[-1,]

tri.reg_ev_count <- reg_ev_state %>%  filter(State == "New York" | 
                                   State == "New Jersey" | State == "Connecticut") %>% 
  rename(EV.Registrations = `Registration Count`)
tri.reg_ev_count$EV.Registrations <- as.integer(tri.reg_ev_count$EV.Registrations)
# write.csv2(tri.reg_ev_count, file = "./processed/triState.Ev.Reg.Count.csv")


ggplot(tri.reg_ev_count, aes(x = State, y = EV.Registrations/10e4))+
  geom_col(fill = c("firebrick", "steelblue", "gold")) +
  labs(title = "Electrich Vehicle Registrations per State", ) +
  ylab("Electric Vehicle Registration per 10,000") +
  xlab("States") +
  theme_classic()


# Study is focused on NY, NJ and CT
tri.state <- c('NJ', 'NY', 'CT') 
eastCoast <- raw_afs %>% filter(State == tri.state)

# We identify columns that should be converted into a factor
factorCols <- c('State', 'Status Code', 'Access Code', 'Facility Type')

eastCoast[factorCols] <- lapply(eastCoast[factorCols], factor)
eastCoast$`Fuel Type Code` <- factor(eastCoast$`Fuel Type Code`, 
                                     labels = c("Biodisel", 
                                                "Compressed Natural Gas",
                                                "Ethanol E85", "Electric", 
                                                "Hydrogen", "Propane")
                                     )
# Base Tri-state levels
triState.base <- eastCoast %>% 
  select(`Fuel Type Code`, City, State, ZIP, `Status Code`, 
         `Access Code`, `EV Network`, `Facility Type`) %>% 
  filter(!is.na(`Facility Type`))
# write.csv2(triState.base, file = './processed/triStateBase.csv')

# Mosaic
afs_per_state <- xtabs(formula = ~ `Fuel Type Code` + State, data = triState.base)
afs_state.df <- as.data.frame(afs_per_state)
ggplot(data = afs_state.df) +
  geom_mosaic(aes(weight=Freq, x=product(Fuel.Type.Code, State), fill=Fuel.Type.Code, na.rm = TRUE)) +
  theme_classic() +
  labs(y = "Alternative Fuel Stations", y = "States", title = "Alternative Fuel Stations Per State (NJ, NY, CT)") +
  guides(fill = guide_legend(title="Alternative Fuel Types", reverse = TRUE))


# EV focus in New York
ny.ev.reg.per.county.raw <- read.csv('./raw_data/Electric_Vehicles_per_County_New_York.csv') %>% 
  select(County, Registration.Class) %>% 
  rename(Registration.Count = Registration.Class)
# write.csv2(ny.ev.reg.per.county.raw, file = './processed/new_york_ev_reg.clean.csv')
