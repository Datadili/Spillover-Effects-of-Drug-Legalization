library(pacman, quietly = TRUE)

#p_load load multiple packages at one, and also installs them if not in system. Including the "here" package for ease of directory change

p_load(haven, tidyverse, gt, modelsummary, janitor, fixest, urbnmapr)


#<><><><><><><><><><><><><><><><><><><><><><><><>
# Cleaning Arrests Data----
#<><><><><><><><><><><><><><><><><><><><><><><><>
crime_dat <- read_dta("Data/Raw/ucr/ucr_arrests_yearly_all_crimes_race_sex_1974_2020.dta")

#Dropping columns that contain race
race <- c('asian', 'black', "white", "non_hisp", "hispanic", "amer_ind")

#drop race data, and other outcome data not interested in
crime_dat_no_race <- crime_dat |> 
  select(-c(contains(race), 
            (matches("curfew|disorder|arson|
                     |all_other_tot|embezzlement|
                     |family_off|forgery|fraud|
                     |gamble|oth_assault|oth_sex|
                     |prostitution|rape|vagrancy|
                     |weapons|juv_referred|drunkenness|
                     |agg_assault|liquor|manslaught|
                     |runaways|murder|vandalism|
                     |suspicion|address"))))


#keep id, only total arrests, state and county stuff
crime_dat_no_race <- crime_dat_no_race |> 
  select(c(matches("ori|treat|agency|
                   |tot_arrests|state|
                   |months_reported|county|
                   |country|year"))) 


#<><><><><><><><><><><><><><><><><><><><><><><><>
#Loading state data to get state names
#<><><><><><><><><><><><><><><><><><><><><><><><>
state_name <- c(tolower(state.name)) #names
state_abb <- c(state.abb) #abbr

#####Adding "DC" to the term----
states <- append(state_name, "district of columbia", after = 8)
states_abb <- append(state_abb, "DC", after = 8)



##Cleaning states----
#more cleaning, both clean the names and AB
df_test <- crime_dat_no_race |>
  mutate(state = ifelse(
    #look at when states is "98"
    state == "98",
    #truth these values if the condition holds
    case_when(
      fips_state_code == "11" ~ "district of columbia",
      fips_state_code == "24" ~ "maryland",
      fips_state_code == "47" ~ "tennessee",
      TRUE ~ state #else just keep the name of state (with the state == "98")
    ),
    #return state if the conditions don't hold 
    state
  )) |> 
  #replace the state abb column
  mutate(state_abb = ifelse(
    #look at when state abb is "0"
    state_abb == "0",
    #truth these values if the condition holds
    case_when(
      fips_state_code == "11" ~ "DC",
      fips_state_code == "24" ~ "MD",
      fips_state_code == "47" ~ "TN",
      TRUE ~ state_abb #keep the abb state (with the state == "98")
    ),
    #return state if the conditions don't hold 
    state_abb
  ))


#remove non states, and others
df_c <- df_test|>
  #remove non states (non DC)
  filter(state %in% states) |>
  #remove if no fips
  filter(!(fips_state_county_code == "0")) |>
  #remove if undefined
  filter(!(fips_state_county_code == "21999")) 

####logic check
#replace value with NA if less than 0
df_c <- df_c |> 
    mutate_all(~ ifelse(. < 0, NA, .))

#delete all missing values (non were missing before)
df_c <- na.omit(df_c)

saveRDS(df_c, file = "Data/Cleaned/temp/clean_nocollapse.RData")


#<><><><><><><><><><><><><><><><><><><><><><><><>
df_c <- readRDS("Data/Cleaned/temp/clean_nocollapse.RData")

#all the variables that end with "tot_arrests"
col_arrests <- df_c |> 
  select(c(ends_with("tot_arrests"))) |> 
  colnames()

#<><><><><><><><><><><><><><><><><><><><><><><><>
# COLLAPSING BY SUM, COUNTY BY YEAR, FOR COUNTY
#<><><><><><><><><><><><><><><><><><><><><><><><>
for (i in col_arrests) {
  #create name rows, but I could just replace
  name <- paste("s", i, sep = "_")
  df_c <- df_c |> #has to be the same! if not it doesn't store the updates
    group_by(year, fips_state_code, fips_state_county_code) |> 
    #eg. sum_poss_arrests = sum(poss_arrests, na.rm = T)
    mutate(
      #eval(parse(text = i)) is returning s_''_tot_arrests
      !!name := sum(eval(parse(text = i)), na.rm = TRUE)) 
}


##### Collapsing county ----

#by year, state, county, pick distinct values of "tot_arrest" columns
df_d <- df_c |> 
  group_by(year, fips_state_code, fips_state_county_code) |> 
  distinct(pick(c(starts_with("s_"), 
                  contains("state"), 
                  contains("county"))))

saveRDS(df_d, file = "Data/Cleaned/temp/collapse_county.RData")

#<><><><><><><><><><><><><><><><><><><><><><><><>
######MAKING OUTCOME DATA (TO USE IN REGRESSION----

state_name <- c(tolower(state.name)) #names
state_abb <- c(state.abb) #abbr


df_d <- readRDS("Data/Cleaned/temp/collapse_county.RData")

#subset the cannabis 
can <- str_subset(colnames(df_d), "cannabis")

#make them sum .eg arrests + drugs
can_sum <- paste(can, collapse = "+")


#below makes the outcome "marijuana", "hard drug"
df_out <- df_d |> 
  mutate(weed_out = s_poss_cannabis_tot_arrests) |> 
  #mutate(weed_out = eval(parse(text = can_sum))) |>
  mutate(hardDrugs_out = s_poss_drug_total_tot_arrests  - weed_out) |> 
  #mutate(hardDrugs_out = s_total_drug_tot_arrests - weed_out) |> 
  
  #sanity check, sum should not be < 0
  filter(!(hardDrugs_out < 0 | weed_out < 0 )) |> 
  #no longer need sale, etc of marijuana or hard drugs
  select(-c(contains("cannabis"), 
            contains("heroin"), 
            contains("synth_narc"),
            contains("drug_tot_"),
            contains("drug_total_tot")))


df_out_other <-  df_out |> 
  #theft = total theft
  mutate(theft_out = s_theft_tot_arrests) |>
  #dui - driving under the influence
  mutate(dui_out = s_dui_tot_arrests) |> 
  #sanity check, sum should not be < 0
  filter(!(theft_out < 0 | dui_out < 0 )) |> 
  #remove if has tot_arrests, only keep the ones you want!
  select(-c(contains("tot_arrests")))
#<><><><><><><><><><><><><><><><><><><><><><><><> 

######Saving collapsed county data----
saveRDS(df_out_other, file = "Data/Cleaned/ucr/collapsed_ucr_county.RData")

#<><><><><><><><><><><><><><><><>
# COLLAPSING BY SUM, STATE BY YEAR,
#<><><><><><><><><><><><><><><><>
#####Collapsing by state----
df_count_collasp <- readRDS("Data/Cleaned/ucr/collapsed_ucr_county.RData")

#select all our outcome variables
outcomes <- str_subset(colnames(df_count_collasp), "out")


for (i in outcomes) {
  #Below has to be the same! if not it doesn't store the updates...
  df_count_collasp <- df_count_collasp |> 
    #group by just year and state
    group_by(year, fips_state_code) |>
    #eg. weed_out = sum(weed_out), with states and years
    mutate(!!i := sum(eval(parse(text = i)), na.rm = TRUE)) 
      }

#by year, state, pick distinct values of "outcome" columns
df_state <- df_count_collasp |> 
  group_by(year, fips_state_code) |> 
  distinct(pick(c(contains("out"), 
                  contains("state_abb"),
                  "state")))



######Saving collapsed state data----
saveRDS(df_state, file = "Data/Cleaned/ucr/collapsed_ucr_state.RData")


#<><><><><><><><><><><><><><><><>
#Cleaning MML Data----
#<><><><><><><><><><><><><><><><>

rmml <- read_csv("Data/Raw/mml/marijuana_laws.csv")

mrml <- rmml |> 
  clean_names() |> 
  mutate(
    rec = ifelse(recreational == "Yes", 1, 0),
    med = ifelse(medical == "Yes", 1, 0)
  ) |> 
  rename(
    "year_rec" = year_recreational,
    "year_med" = year_medical
  ) |> 
  #remove decrimalized
  select(-decriminalized) |> 
  #make it lower case
  mutate(states = tolower(states),
         recreational = tolower(recreational),
         medical = tolower(medical))

#bringing state name (plus DC)
state_name <- c(tolower(state.name)) #names
state_abb <- c(state.abb) #abbr

#####Adding "DC" to the term----
states <- append(state_name, "district of columbia", after = 8)
states_abb <- append(state_abb, "DC", after = 8)


#adding a "state abbrevaition" column to the dataset (see above)
mrml$state_abb <-  states_abb

##saving clean MML----
saveRDS(mrml, file = "Data/Cleaned/mml/clean_mml.RData")

#<><><><><><><><><><><><><><><><>
#Cleaning Border Data----
#<><><><><><><><><><><><><><><><>
df <- read_csv("Data/Raw/border/county_adjacency2010.csv")

#separate the column names
df_sep <- df |> 
  separate(countyname, c("countyname", "state"), sep = ",") |> 
  separate(neighborname, c("county_adj", "state_adj"), sep = ",") 


#keep only border counties
df_new <- df_sep |> 
  filter(!(state == state_adj)) #BORDER COUNTIES!


#function to clean string whiteshape
for (i in colnames(df_new)) {
  #Below has to be the same! if not it doesn't store the updates...
  df_new <- df_new |> 
    #replace each col-name and remove the whitespace
    mutate(!!i := str_trim(eval(parse(text = i)))) 
}

##saving clean MML----
saveRDS(df_new, file = "Data/Cleaned/border/clean_border.RData")

