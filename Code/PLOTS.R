
library(pacman, quietly = TRUE)

#p_load load multiple packages at one, and also installs them if not in system. Including the "here" package for ease of directory change

p_load(haven,
       patchwork,
       sf,
       viridis,
       tidyverse,
       gt,
       modelsummary,
       janitor,
       fixest,
       urbnmapr)

#<><><><><><><><><><><><><><><><>
#Loading datasets for plotting----
#<><><><><><><><><><><><><><><><>

#get county geo
urban_map <- get_urbn_map(map = "counties", sf = TRUE)
#get state geo
state_urban_map <- get_urbn_map(map = "states", sf = TRUE)
#get state labels
labels <- get_urbn_labels(map = "territories_states", sf = TRUE)







#<><><><><><><><><><><><><><><><><><>
#Data FOR PLOTTING just borders----
#<><><><><><><><><><><><><><><><><><>
#has )

blah <- readRDS("Data/Final/final_outcome.RData")
#graphyyy (for treat and control)----
temp <- left_join(urban_map |> 
                    mutate(state_name = tolower(state_name)),
                  blah,
                  by = c("county_fips" = "fips_state_county_code",
                         "state_abbv" = "state_abb",
                         "state_name" = "state")) |> 
  mutate(treat_untreat = ifelse(
    !is.na(treat_untreat), treat_untreat, 2
  ))



##Validity check----
ggplot() +
  geom_sf(data = temp, 
          mapping = aes(fill = as.factor(treat_untreat))) +
  labs(fill = "") +
  scale_fill_manual(labels = c("1" = "Treated",
                               "0" = "Control",
                               "2" = "Legalized Counties"),
                    values = c("1" = "red",
                               "0" = "blue",
                               "2" = "white")) +
  theme_bw() +
  labs(y = "", x = "") + 
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank()) + 
  theme(legend.position = "bottom") +
  coord_sf(datum = NA) #+
  #theme(legend.text = element_text(size = 12)) 


#save data summary gra[h]----
ggsave(filename = "Paper/images/summary_treated_graph.png", width = 5, height = 3)
#ggsave(filename = "Paper/images/summary_treated_graph.png", width = 5, height = 3, dpi = 150, units = "in")

