#<><><<<><><><><><><<><><><><><><><>
#Author; Dili Maduabum
#Date: November 11th, 2023
#Updated:
#JOIN AND PREP DATA FOR ANALYSIS
#<><><><><><><><><><><><><><><><><><><><>



library(pacman, quietly = TRUE)

#p_load load multiple packages at one, and also installs them if not in system. Including the "here" package for ease of directory change

p_load(haven, patchwork, sf, viridis, tidyverse, gt, modelsummary, janitor, fixest, urbnmapr)

#<><><><><><><><><><><><><><><><>
#Loading cleaned datasets----
#<><><><><><><><><><><><><><><><>
df_bord <- readRDS("Data/Cleaned/border/clean_border.RData")
df_count <- readRDS("Data/Cleaned/ucr/collapsed_ucr_county.RData")
df_state <- readRDS("Data/Cleaned/ucr/collapsed_ucr_state.RData")
df_mml <- readRDS("Data/Cleaned/mml/clean_mml.RData")

#<><><><><><><><><><><><><><><><>
#Loading datasets for plotting----
#<><><><><><><><><><><><><><><><>

#get county geo
urban_map <- get_urbn_map(map = "counties", sf = TRUE)
#get state geo
state_urban_map <- get_urbn_map(map = "states", sf = TRUE)
#get state labels 
labels <- get_urbn_labels(map = "territories_states", sf = TRUE)

#<><><><><><><><><><><><><><><><>
#Join "border" and "mml"----
#<><><><><><><><><><><><><><><><>

#treated rec or med states (abbrev----
dfStateTreat <- df_mml |> 
  filter(rec == 1 | med == 1)

#get treated or rec states
treatState <- c(unique(dfStateTreat$state_abb))
treatState


#update the dataset
df_bord <- df_bord |>
  #if the adjacent state is a treated state, make it one
  mutate(treat_adj = case_when(
    state_adj %in% treatState == T ~ 1,
    #if adjacent state is not, make it zero (ignoring non-border counties)
    !(is.na(state_adj)) & (state_adj %in% treatState == F) ~ 0,
    is.na(state_adj) == TRUE ~ 0
  )) |> 
  #generate a border column (since the dataset is one of borders)
  mutate(borders = 1)

#make a list of border treated states
#treated_adjacents
df_tb <- df_bord |>
  filter(treat_adj == 1)

#counties beside treated (even though they could be beside untreated)
list_tb <- c(unique(df_tb$fipscounty))



#joining state_adj with states in mml----
bord_mml <- left_join(df_bord,
                      df_mml,
                      by = c("state_adj" = "state_abb"))


bord_mml_norm <- left_join(df_bord,
                         df_mml,
                         by = c("state" = "state_abb"))

##making the "min year" of recreation----
df_bml_min <- bord_mml_norm |> 
  mutate(year_min = ifelse(
    (!is.na(year_rec) >= !is.na(year_med)) | 
      (is.na(year_rec) & !is.na(year_med)), 
    year_med, 
    year_rec)) 

#make lowest state
bml_min <- df_bml_min  |>
  #near rec, but it's not a med state
  mutate(
    treated_none = ifelse(
      recreational == "no" & medical == "no" &
        borders == 1 &
        fipscounty %in% list_tb, 1, 0)) 


#joining t
temp <- left_join(urban_map |> 
                    mutate(state_name = tolower(state_name)),
                  bml_min,
                  by = c("county_fips" = "fipscounty",
                         "state_abbv" = "state"))


#Validity check
ggplot() +
  geom_sf(data = temp, 
          mapping = aes(fill = as.factor(treated_none))) +
  labs(fill = "treated_none") +
  scale_fill_manual(values = c("1" = "red")) +
  theme_bw() +
  labs(y = "", x = "") + 
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank()) + 
  coord_sf(datum = NA) 

















#GOTTEN BORDER STATES AND THE YEAR IT WAS TREATED----


temp2 <- 





#<><><><><><><><><><><><><><><><><><>
#Data FOR PLOTTING just borders----
#<><><><><><><><><><><><><><><><><><>
#has )
spatial_data <- left_join(urban_map,
                          bord_mml,
                          by = c("county_fips" = "fipscounty",
                                 "state_abbv" = "state"))



#get column of borders, 1 or 0
spatial_data <- spatial_data |> 
  mutate(borders = ifelse(!is.na(countyname), 1, 0)) 





#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#Join "border" and "mml" and "arrests" ----
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df_arbml <- left_join(df_count,
                      df_bml_min,
                    by = c("state_abb" = "state", 
                           "fips_state_county_code" = "fipscounty"))


df_arbml_treat <- 


























#<><><><><><><><><><><><><><><><><><>
#Data FOR PLOTTING just borders----
#<><><><><><><><><><><><><><><><><><>
spatial_data <- left_join(urban_map,
                          df_bord,
                          by = c("county_fips" = "fipscounty"))


order_county <- spatial_data |> 
  mutate(borders = ifelse(!is.na(countyname), 1, 0))


saveRDS(order_county, file = "Data/Cleaned/plot/border_plot.RData")

#<><><><><><><><><><><><><><><><><><>
#Data FOR PLOTTING just borders----
#<><><><><><><><><><><><><><><><><><>





