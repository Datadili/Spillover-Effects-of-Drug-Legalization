#<><><<<><><><><><><<><><><><><><><>
#Author; Dili Maduabum
#Date: Nov. 20th, 2023
#Updated: 
#REGRESSION TABLES
#<><><><><><><><><><><><><><><><><><><><>



library(pacman, quietly = TRUE)

#p_load load multiple packages at one, and also installs them if not in system. Including the "here" package for ease of directory change

p_load(haven,
       patchwork,
       sf,
       viridis,
       tidyverse,
       gt,
       modelsummary,
       panelsummary,
       janitor,
       fixest,
       urbnmapr, 
       knitr,
       xtable,
       broom,
       glue)

#<><><><><><><><><><><><><><><><>
#Loading cleaned datasets----
#<><><><><><><><><><><><><><><><>

#county summary table----
df <- readRDS("Data/Final/final_outcome.RData") |> 
  select(-c(theft_out))

df1 <- df
df1 |>
  ungroup() |> 
  ## select variables to show
  select(c(contains("out"), treat)) |>
  pivot_longer(cols = c(weed_out, hardDrugs_out, dui_out),
names_to = "name", values_to = "value") |> 
  #pivot_longer(cols = everything()) |>
  group_by(treat, name) |>
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = sum(!is.na(value))
  ) |>
  ## group variables
  mutate(cat = recode(
    name,
    dui_out = "Outcome",
    hardDrugs_out = "Outcome",
    weed_out  = "Outcome")) |>
  ## rename variable descriptions
  mutate(name = recode_factor(
    name,
    dui_out = "Driving Under the Influence (Alcohol)",
    hardDrugs_out = "Posession of Hard Drugs",
    weed_out  = "Possession of Marijuana")) |>
  mutate(treat = recode_factor(
    treat,
    `1` = "Treatment",
    `0` = "Control")) |> 
  arrange(name) |>
  group_by(treat) |>
  select(-cat)|> 
  gt() |>
  ## rename column names
  cols_align("left", 1) |>
  cols_label(mean = "Mean", sd = "Std. Dev", name = "") |>
  ## round
  fmt_number(columns = c(mean, sd), decimals = 2) |>
  ## add commas to integers
  fmt_integer(columns = n) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |> 
  gtsave("Paper/summary_table.tex")






#county REGRESSION----
df2 <- df
weed_1 <- feols(weed_out ~ treat | year + fips_state_county_code,
                data = df, cluster = ~fips_state_county_code)
weed_2 <- feols(weed_out ~ treat + year*as.factor(fips_state_county_code) | year + fips_state_county_code,
                data = df, cluster = ~fips_state_county_code)
hardDrugs_1 <- feols(hardDrugs_out ~ treat | year + fips_state_county_code,
                     data = df, cluster = ~fips_state_county_code)
hardDrugs_2 <- feols(hardDrugs_out ~ treat + year*as.factor(fips_state_county_code) | year + fips_state_county_code,
                     data = df, cluster = ~fips_state_county_code)
dui_1 <- feols(dui_out ~ treat | year + fips_state_county_code,
               data = df, cluster = ~fips_state_county_code)
dui_2 <- feols(dui_out ~ treat + year*as.factor(fips_state_county_code) | year + fips_state_county_code,
               data = df, cluster = ~fips_state_county_code)

mod_vec <- list(weed_1, weed_2, 
                hardDrugs_1, hardDrugs_2, 
                dui_1, dui_2)

rows <- tribble(~term,  ~weed1,  ~weed1, ~weed1, ~weed1, ~weed1, ~weed1,
                'County Fixed Effects', 'Yes', 'Yes', 'Yes',  'Yes', 'Yes', 'Yes',
                'Year Fixed Effects', 'Yes', 'Yes', 'Yes',  'Yes', 'Yes', 'Yes',
                'County-Linear Time Trends', 'No', 'Yes', 'No',  'Yes', 'No', 'Yes')


attr(rows, 'position') <- c(3, 4, 5)

#rename the coefficient
cm <- c('treat' = 'Border County (AML)')
#rename the statistics
gm <- tribble(
  ~raw,        ~clean,      ~fmt,
  "r.squared", "R Squared", 3,
  "nobs", "Observations", 0)

#modelsummary(models, gof_map = gm)
modelsummary(mod_vec, 
             coef_omit = "year|fips_state_county_code",
             add_rows = rows, 
             fmt = 3,
             align = "lcccccc",
             coef_map = cm,
             gof_map = gm,
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
             output = "gt")  |> 
  # spanning labels
  tab_spanner(label = 'Marijuana', columns = 2:3)  |> 
  tab_spanner(label = 'Hard Drugs', columns = 4:5)  |> 
  tab_spanner(label = 'DUIs', columns = 6:7) |> 
  gtsave("Paper/ident.tex")



#Event Study plots----
  for (i in seq(1, 5, by = 1)) {
    #create name rows, but I could just replace
    lag <- paste("lag", i, sep = "_")
    lead <-  paste("lead", i, sep = "_")
    df2 <- df2 |>
      group_by(fips_state_county_code) |>
      #by year
      arrange(year) |>
      mutate(!!lag := lag(treat, i),
             !!lead := lead(treat, i))
  }


event <- str_subset(colnames(df2), "lag|lead")

event_plot <- paste(event, collapse = "+")


#not including war_lag1 since it's in {coef_names}
spec_w <- glue("weed_out ~ treat + {event_plot} + year*as.factor(fips_state_county_code) | year + fips_state_county_code")
spec_hd <- glue("hardDrugs_out ~ treat + {event_plot} + year*as.factor(fips_state_county_code) | year + fips_state_county_code")
spec_d <- glue("dui_out ~ treat + {event_plot} + year*as.factor(fips_state_county_code) | year + fips_state_county_code")


fit_w <- feols(as.formula(spec_w), data = df2, cluster = ~fips_state_county_code)
fit_hd <- feols(as.formula(spec_hd), data = df2, cluster = ~fips_state_county_code)
fit_d <- feols(as.formula(spec_d), data = df2, cluster = ~fips_state_county_code)

#weed event study ----
#function(x) {
fiit_w <- fit_w |> 
  tidy() |>
  #keep only rows that have war in them
  filter(term %in% c(str_subset(term, "treat|lag|lead"))) |>
  mutate(
    #this relabels the term and orders them
    term = recode_factor(
      term,
      lead_5 = "-5",
      lead_4 = "-4",
      lead_3 = "-3",
      lead_2 = "-2",
      lead_1 = "-1",
      treat = "0",
      lag_1 = "1",
      lag_2 = "2",
      lag_3 = "3",
      lag_4 = "4",
      lag_5 = "5"
    )
  )  

fiit_w$term <- fct_relevel(fiit_w$term, "-1")

fiit_w |> 
  ggplot(aes(x = term, y = estimate)) +
  geom_point(data = filter(fiit_w, term != "-1"), size = 3) +
  geom_point(data = fiit_w |> 
               filter(term == "-1")  |> 
               mutate(estimate = 0), 
             color = "black", size = 3) + 
  #no error bar for 1
  geom_errorbar(data = filter(fiit_w, term != "-1"),
                aes(ymin = estimate - 2 * std.error, #confidence interval
                    ymax = estimate + 2 * std.error),
                width = .3) + #thickness of the error bar at the top?
  geom_vline(xintercept = "-1", linetype = "dashed", color = "black") + 
  labs(y = "",
       x = ""#,#"Years since AML Adoption",
      #title = "Possesion of Marijuana"
       ) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(limits = c("-5", "-4", "-3", "-2", "-1", 
                              "0", "1", "2", "3", "4", "5")) 
#ggsave(filename = "Paper/images/event_weed.png", width = 10, height = 8, dpi = 150, units = "in")
ggsave(filename = "Paper/images/event_weed.png", width = 5, height = 3)


#plot for Hard drugs----
fiit_hd <- fit_hd |> 
  tidy() |>
  #keep only rows that have war in them
  filter(term %in% c(str_subset(term, "treat|lag|lead"))) |>
  mutate(
    #this relabels the term and orders them
    term = recode_factor(
      term,
      lead_5 = "-5",
      lead_4 = "-4",
      lead_3 = "-3",
      lead_2 = "-2",
      lead_1 = "-1",
      treat = "0",
      lag_1 = "1",
      lag_2 = "2",
      lag_3 = "3",
      lag_4 = "4",
      lag_5 = "5"
    )
  )  

fiit_hd$term <- fct_relevel(fiit_hd$term, "-1")

fiit_hd |> 
  ggplot(aes(x = term, y = estimate)) +
  geom_point(data = filter(fiit_hd, term != "-1"), size = 3) +
  geom_point(data = fiit_hd |> 
               filter(term == "-1")  |> 
               mutate(estimate = 0), 
             color = "black", size = 3) + 
  #no error bar for 1
  geom_errorbar(data = filter(fiit_hd, term != "-1"),
                aes(ymin = estimate - 2 * std.error, #confidence interval
                    ymax = estimate + 2 * std.error),
                width = .3) + #thickness of the error bar at the top?
  geom_vline(xintercept = "-1", linetype = "dashed", color = "black") + 
  labs(y = "",
       x = "" #,"Years since AML Adoption",
       #title = "Possesion of Hard Drugs"
       ) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(limits = c("-5", "-4", "-3", "-2", "-1", 
                              "0", "1", "2", "3", "4", "5")) 
#ggsave(filename = "Paper/images/event_hardDrug.png", width = 10, height = 8, dpi = 150, units = "in")
ggsave(filename = "Paper/images/event_hardDrug.png", width = 5, height = 3)


#plot for dui----
fiit_d <- fit_d |> 
  tidy() |>
  #keep only rows that have war in them
  filter(term %in% c(str_subset(term, "treat|lag|lead"))) |>
  mutate(
    #this relabels the term and orders them
    term = recode_factor(
      term,
      lead_5 = "-5",
      lead_4 = "-4",
      lead_3 = "-3",
      lead_2 = "-2",
      lead_1 = "-1",
      treat = "0",
      lag_1 = "1",
      lag_2 = "2",
      lag_3 = "3",
      lag_4 = "4",
      lag_5 = "5"
    )
  )  

fiit_d$term <- fct_relevel(fiit_d$term, "-1")

fiit_d |> 
  ggplot(aes(x = term, y = estimate)) +
  geom_point(data = filter(fiit_d, term != "-1"), size = 3) +
  geom_point(data = fiit_d |> 
               filter(term == "-1")  |> 
               mutate(estimate = 0), 
             color = "black") + 
  #no error bar for 1
  geom_errorbar(data = filter(fiit_d, term != "-1"),
                aes(ymin = estimate - 2 * std.error, #confidence interval
                    ymax = estimate + 2 * std.error),
                width = .3, size = 3) + #thickness of the error bar at the top?
  geom_vline(xintercept = "-1", linetype = "dashed", color = "black") + 
  labs(y = "",
       x = ""#,#"Years since AML Adoption",
       #title = "Driving Under the Influence (DUIs)"
       ) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_discrete(limits = c("-5", "-4", "-3", "-2", "-1", 
                              "0", "1", "2", "3", "4", "5")) #+
  #theme(
  #  axis.text.x = element_text(size = 15),
 #   axis.text.y = element_text(size = 15),
 #   plot.title = element_text(size = 18, hjust = 0.5)
 # )

#ggsave(filename = "Paper/images/event_dui.png", width = 10, height = 8, dpi = 150, units = "in")
ggsave(filename = "Paper/images/event_dui.png", width = 5, height = 3)







#for (o in outcomes) {
#  mod_vec[[o]] <- tidy(feols(as.formula(paste(o, fe_county_year, sep = " ~")), data = df))
#  mod_vec2[[o]] <- tidy(feols(as.formula(paste(o, fe_county, sep = " ~")), data = df))
#}

#, stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1)
#mod1 <- mod_vec |> list_rbind(names_to = "model") |> select(-c(term, statistic, p.value)) |> 
#  pivot_longer(names_to = "estimates",
#               cols = c("estimate", "std.error"))
#mod2 <- mod_vec2 |> list_rbind(names_to = "model") |> select(-c(term, statistic, p.value)) |> 
#  pivot_longer(names_to = "estimates",
#               cols = c("estimate", "std.error"))
#
#left_join(mod1, mod2, by = c("model", "estimates")) |> select(-c(estimates)) |> gt()
#

#make regressionsss----
outcomes <- c(str_subset(colnames(df), "out"))

mod_vec <- c()
mod_vec2 <- c()

#year, county, fixed effects
fe_county_year <-  "treat | year + fips_state_county_code"
#year, county, yearxcounty interaction (linear time trends) fixed effects
#fe_county_year_linear <- "treat + as.factor(year) + as.factor(fips_state_county_code) + year*as.factor(fips_state_county_code)"
fe_county_year_linear <- "treat | year + as.factor(fips_state_county_code) + year*as.factor(fips_state_county_code)"


for (o in outcomes) {
  mod_vec[[o]] <- feols(as.formula(paste(o, fe_county_year, sep = " ~")), 
                        data = df)
  # mod_vec2[[o]] <- feols(as.formula(paste(o, fe_county, sep = " ~")), data = df)
}




#outcomes <- c("log(weed_out)","log(hardDrugs_out)", "log(dui_out)" )

weed_1 <- feols(hardDrugs_out ~ treat | year + fips_state_county_code,
                data = df, cluster = ~fips_state_county_code)
weed_2 <- feols(weed_out ~ treat + year*as.factor(fips_state_county_code) | year + fips_state_county_code,
                data = df, cluster = ~fips_state_county_code)
hardDrugs_1 <- feols(hardDrugs_out ~ treat | year + fips_state_county_code,
                     data = df, cluster = ~fips_state_county_code)
hardDrugs_2 <- feols(hardDrugs_out ~ treat + year*as.factor(fips_state_county_code) | year + fips_state_county_code,
                     data = df, cluster = ~fips_state_county_code)
dui_1 <- feols(dui_out ~ treat | year + fips_state_county_code,
               data = df, cluster = ~fips_state_county_code)
dui_2 <- feols(dui_out ~ treat + year*as.factor(fips_state_county_code) | year + fips_state_county_code,
               data = df, cluster = ~fips_state_county_code)




weed_1 <- feols(weed_out ~ treat | year + fips_state_county_code,
                data = df, cluster = ~year)
weed_2 <- feols(weed_out ~ treat + year*as.factor(fips_state_county_code) | year + fips_state_county_code,
                data = df, cluster = ~year)
hardDrugs_1 <- feols(hardDrugs_out ~ treat | year + fips_state_county_code,
                     data = df, cluster = ~year)
hardDrugs_2 <- feols(hardDrugs_out ~ treat + year*as.factor(fips_state_county_code) | year + fips_state_county_code,
                     data = df, cluster = ~year)
dui_1 <- feols(dui_out ~ treat | year + fips_state_county_code,
               data = df, cluster = ~year)
dui_2 <- feols(dui_out ~ treat + year*as.factor(fips_state_county_code) | year + fips_state_county_code,
               data = df, cluster = ~year)



#Event Study plots----
out <- c(str_subset(colnames(df2), "out"))
for (k in out) {
  for (i in seq(1, 5, by = 1)) {
    #create name rows, but I could just replace
    lag <- paste(k, "lag", i, sep = "_")
    lead <-  paste(k, "lead", i, sep = "_")
    df2 <- df2 |>
      group_by(fips_state_county_code) |>
      #by year
      arrange(year) |>
      mutate(!!lag := lag(eval(parse(text = k)), i),
             !!lead := lead(eval(parse(text = k)), i))
  }
}



weed_event <- str_subset(colnames(df2), "weed_out")
hardDrugs_event <- str_subset(colnames(df2), "hardDrugs_out")
dui_event <- str_subset(colnames(df2), "dui_out")

w_event_plot <- paste(weed_event, collapse = "+")
hd_event_plot <- paste(hardDrugs_event, collapse = "+")
d_event_plot <- paste(dui_event, collapse = "+")


fit_w <- feols(as.formula(spec_w), data = df2, cluster = ~fips_state_county_code)
fit_hd <- feols(as.formula(spec_hd), data = df2, cluster = ~fips_state_county_code)
fit_d <- feols(as.formula(spec_d), data = df2, cluster = ~fips_state_county_code)



df1 |>
  ## select variables to show
  select(contains("out")) |>
  ## rescale so length is the same
  mutate(rgdppc = rgdppc/1000) |>
  ## change from wide to long so we can take means and SDs for all variables at once
  pivot_longer(cols = everything()) |>
  group_by(name) |>
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = sum(!is.na(value))
  ) |>
  ## group variables
  mutate(cat = recode(
    name,
    topitaxrate2 = "Outcome",
    unisuffrage = "Treatment",
    himobpopyear2p = "Treatment",
    .default = "Covariate")) |>
  ## rename variable descriptions
  mutate(name = recode_factor(
    name,
    topitaxrate2 = "Top Inheritance Tax Rate",
    unisuffrage = "Universal Male Suffrage (0 or 1)",
    leftexec2 = "Left Executive (0 or 1)",
    himobpopyear2p = "War Mobilization (0 or 1)",
    rgdppc = "Real GDP per capita (USD 1000s)")) |>
  arrange(name) |>
  group_by(cat) |>
  gt() |>
  ## rename column names
  cols_align("left", 1) |>
  cols_label(mean = "Mean", sd = "Std. Dev", name = "") |>
  ## round
  fmt_number(columns = c(mean, sd), decimals = 2) |>
  ## add commas to integers
  fmt_integer(columns = n) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

