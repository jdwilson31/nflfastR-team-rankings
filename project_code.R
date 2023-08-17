library(mosaic)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(gt)
library(forcats)
library(tidyr)


options(scipen = 9999)

pbp <- load_pbp(2022) %>% 
  filter(season_type == "REG")


##### Half Time Adjustements 

###### Half time scores with ratio
half_scores <- pbp %>% 
  filter(!is.na(posteam), game_half != "Overtime") %>% 
  group_by(posteam, game_half, game_id) %>% 
  summarize(score = last(posteam_score_post)) %>% 
  group_by(posteam, game_half) %>% 
  summarize(avg_half_points = mean(score), total_half_points = sum(score))

half1_scores <- half_scores[half_scores$game_half == "Half1",]
half2_scores <- half_scores[half_scores$game_half == "Half2",]

half_total_scores <- half1_scores %>% left_join(half2_scores, by = "posteam")

half_total_scores_tbl <- half_total_scores %>% 
  mutate(avg_half_1_score = avg_half_points.x,
         avg_half_2_score = avg_half_points.y - avg_half_1_score,
         ratio = avg_half_2_score / avg_half_1_score,
         total_points = total_half_points.y,
         adj_ratio = ratio * total_points
         ) %>% 
  select(-game_half.x, -game_half.y, -avg_half_points.x, -avg_half_points.y) %>% 
  arrange(-ratio) %>%
  ungroup() %>% 
  mutate(
    Rank = row_number(),
    first = round(avg_half_1_score, 2),
    second = round(avg_half_2_score, 2),
    ratio = round(ratio, 2)
  ) %>% 
  select(Rank, posteam, first, second, ratio, adj_ratio)
  
half_total_scores_tbl <- half_total_scores_tbl %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>% 
  select(Rank, team_wordmark, first, second, ratio)

tbl <- half_total_scores_tbl %>%
  gt() %>%
  cols_label(Rank = "Rank",
             team_wordmark = "",
             first = "Avg 1st Half Points",
             second = "Avg 2nd Half Points", 
             ratio = "Ratio") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_img_rows(team_wordmark) %>% 
  tab_source_note(source_note = "John Wilson | @jdwanalytics") %>%
  tab_source_note(source_note =  "data: @nflfastr")

library(webshot2)

tbl %>% 
  tab_options(
    table.margin.right = px(1000)
  ) %>%  
  cols_width(columns = everything() ~ 150) %>% 
  cols_width(Rank ~ 50) %>% 
  gtsave("pnt_ratio.png", vwidth = 1000)


##############################################################

half_time <- pbp %>% 
  filter(game_half != "Overtime", !is.na(play_type), !is.na(posteam)) %>% 
  mutate(
    half1_run_play = ifelse(game_half == "Half1" & play_type == "run", 1, 0),
    half2_run_play = ifelse(game_half == "Half2" & play_type == "run", 1, 0),
    half1_pass_play = ifelse(game_half == "Half1" & play_type == "pass", 1, 0),
    half2_pass_play = ifelse(game_half == "Half2" & play_type == "pass", 1, 0)
  ) %>% 
  group_by(posteam) %>% 
  summarize(half1_pass_plays = sum(half1_pass_play), 
            half1_pass_yards = sum(yards_gained[half1_pass_play == 1]),
            half2_pass_plays = sum(half2_pass_play), 
            half2_pass_yards = sum(yards_gained[half2_pass_play == 1]),
            half1_run_plays = sum(half1_run_play), 
            half1_run_yards = sum(yards_gained[half1_run_play == 1]),
            half2_run_plays = sum(half2_run_play), 
            half2_run_yards = sum(yards_gained[half2_run_play == 1]),
            half_1_score_dif = sum(score_differential[half1_run_play == 1 | half1_pass_play] == 1)
            )


half_time <- half_time %>% 
  mutate(
    avg_half1_pass_yards = half1_pass_yards / half1_pass_plays,
    avg_half2_pass_yards = half2_pass_yards / half2_pass_plays,
    avg_half1_run_yards = half1_run_yards / half1_run_plays,
    avg_half2_run_yards = half2_run_yards / half2_run_plays,
    pass_yards_dif = avg_half2_pass_yards - avg_half1_pass_yards,
    run_yards_dif = avg_half2_run_yards - avg_half1_run_yards
    ) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))



half_time %>% 
  ggplot(aes(x = run_yards_dif, y = pass_yards_dif, label = posteam)) + 
  geom_point(alpha = 0) + 
  geom_hline(yintercept = mean(half_time$pass_yards_dif), col = "red", linetype = "dashed", linewidth = 1) + 
  geom_vline(xintercept = mean(half_time$run_yards_dif), col = "red", linetype = "dashed", linewidth = 1) + 
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = .05, alpha = .7) + 
  labs(
    x = "Offensive Run Yards 2cd Half Difference",
    y = "Offensive Pass Yards 2cd Half Differece",
    title = "Offenisve Perfomance After Halftime",
    caption = "John Wilson | @jdwanalytics
                  data: @nflfastR") + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = .5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = .5, size = 15),
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15)
  )

  

half_time_def <- pbp %>% 
  filter(game_half != "Overtime", !is.na(play_type), !is.na(defteam)) %>% 
  mutate(
    half1_run_play = ifelse(game_half == "Half1" & play_type == "run", 1, 0),
    half2_run_play = ifelse(game_half == "Half2" & play_type == "run", 1, 0),
    half1_pass_play = ifelse(game_half == "Half1" & play_type == "pass", 1, 0),
    half2_pass_play = ifelse(game_half == "Half2" & play_type == "pass", 1, 0)
  ) %>% 
  group_by(defteam) %>% 
  summarize(half1_pass_plays = sum(half1_pass_play), 
            half1_pass_yards = sum(yards_gained[half1_pass_play == 1]),
            half2_pass_plays = sum(half2_pass_play), 
            half2_pass_yards = sum(yards_gained[half2_pass_play == 1]),
            half1_run_plays = sum(half1_run_play), 
            half1_run_yards = sum(yards_gained[half1_run_play == 1]),
            half2_run_plays = sum(half2_run_play), 
            half2_run_yards = sum(yards_gained[half2_run_play == 1])
  )

half_time_def <- half_time_def %>% 
  mutate(
    avg_half1_pass_yards = half1_pass_yards / half1_pass_plays,
    avg_half2_pass_yards = half2_pass_yards / half2_pass_plays,
    avg_half1_run_yards = half1_run_yards / half1_run_plays,
    avg_half2_run_yards = half2_run_yards / half2_run_plays,
    pass_yards_dif = avg_half2_pass_yards - avg_half1_pass_yards,
    run_yards_dif = avg_half2_run_yards - avg_half1_run_yards
  ) %>% 
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr"))


half_time_def %>% 
  ggplot(aes(x = run_yards_dif, y = pass_yards_dif, label = defteam)) + 
  geom_point(alpha = 0) + 
  geom_hline(yintercept = mean(half_time_def$pass_yards_dif), col = "red", linetype = "dashed", linewidth = 1) + 
  geom_vline(xintercept = mean(half_time_def$run_yards_dif), col = "red", linetype = "dashed", linewidth = 1) + 
  nflplotR::geom_nfl_logos(aes(team_abbr = defteam), width = .05, alpha = .7) +
  scale_x_reverse() + 
  scale_y_reverse() + 
  labs(
    x = "Defensive Run Yards 2cd Half Difference",
    y = "Defensive Pass Yards 2cd Half Differece",
    title = "Defensive Perfomance After Halftime",
    caption = "John Wilson | @jdwanalytics
                  data: @nflfastR") + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = .5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = .5, size = 15),
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15)
  )
  

##############################################################################
# EPA Visuals 

afc_teams = c("MIA", "BUF", "NE", "NYJ", 
              "BAL", "CIN", "CLE", "PIT", 
              "HOU", "IND", "JAX", "TEN",
              "DEN", "KC", "LV", "LAC")

south = c("HOU", "IND", "JAX", "TEN", "TB", "NO", "CAR", "ATL")

north = c("BAL", "CIN", "CLE", "PIT", "MIN", "DET", "GB", "CHI")

west = c("DEN", "KC", "LV", "LAC", "SF", "SEA", "LA", "ARI")

east = c("MIA", "BUF", "NE", "NYJ", "PHI", "DAL", "NYG", "WAS")


######  Off Epa cumulative graph
off_epa <- pbp %>% 
  filter(!is.na(epa), !is.na(posteam)) %>% 
  group_by(posteam) %>% 
  mutate(play = n()) %>% 
  ungroup() %>%
  select(epa, posteam) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))


off_epa$play_num <- ave(off_epa$epa, off_epa$posteam, FUN = seq_along)
off_epa$csum <- ave(off_epa$epa, off_epa$posteam, FUN = cumsum)

off_epa <- off_epa %>% 
  mutate(
    conf= ifelse(posteam %in% afc_teams, "AFC", "NFC"),
    div = case_when(
      posteam %in% south ~ "SOUTH",
      posteam %in% north ~ "NORTH",
      posteam %in% east ~ "EAST",
      posteam %in% west ~ "WEST"
    )
  )

off_epa %>%
  ggplot(aes(x = play_num, y = csum)) + 
  geom_smooth(aes(group = posteam, col = team_color), se = F, linewidth = 2) + 
  scale_color_identity(aesthetics = c("fill", "color")) + 
  facet_grid(conf ~ div) + 
  labs(
    x = "Number of Plays",
    y = "Cumulative EPA",
    title = "Cumulative Offensive EPA Throughout The Season",
    caption = "John Wilson | @jdwanalytics
                  data: @nflfastR") + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = .5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = .5, size = 15),
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15)
  )


###### Off NFC / AFC Table

off_epa_tbl <- pbp %>% 
  filter(!is.na(epa), !is.na(posteam)) %>% 
  group_by(posteam) %>% 
  mutate(play = n()) %>% 
  summarize(plays = n(), cum_epa = sum(epa))%>%
  arrange(-cum_epa)

off_epa_tbl <- off_epa_tbl %>% 
  mutate(
    conf= ifelse(posteam %in% afc_teams, "AFC", "NFC"),
    div = case_when(
      posteam %in% south ~ "SOUTH",
      posteam %in% north ~ "NORTH",
      posteam %in% east ~ "EAST",
      posteam %in% west ~ "WEST"
    )
  )

## AFC and NFC table  

off_epa_tbl_nfc <- off_epa_tbl %>% 
  filter(conf == "NFC") %>% 
  arrange(div, -cum_epa) %>% 
  mutate(
    cum_epa = round(cum_epa, 2),
    rank = ((row_number() - 1) %% 4) + 1
  ) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>% 
  select(team_wordmark, rank, cum_epa, div) 

off_epa_tbl_afc <- off_epa_tbl %>% 
  filter(conf == "AFC") %>% 
  arrange(div, -cum_epa) %>% 
  mutate(
    cum_epa = round(cum_epa, 2),
    rank = ((row_number() - 1) %% 4) + 1
  ) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>% 
  select(team_wordmark, rank, cum_epa, div)

off_epa_tbl_afc %>% 
  gt() %>% 
  cols_label(
    team_wordmark = "Team",
    cum_epa = "Cumulative EPA") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_img_rows(team_wordmark) %>% 
  tab_options(
    table.margin.right = px(1000),
  ) %>% 
  tab_header("AFC Expected Points Added at the End of the year") %>% 
  tab_source_note(source_note = "John Wilson | @jdwanalytics") %>%
  tab_source_note(source_note =  "data: @nflfastr") %>% 
  gtsave("off_afc_epa.png", vwidth = 1000)

off_epa_tbl_nfc %>% 
  gt() %>% 
  cols_label(
    team_wordmark = "Team",
    cum_epa = "Cumulative EPA") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_img_rows(team_wordmark) %>% 
  tab_options(
    table.margin.right = px(1000),
  ) %>% 
  tab_header("NFC Expected Points Added at the End of the year") %>% 
  tab_source_note(source_note = "John Wilson | @jdwanalytics") %>%
  tab_source_note(source_note =  "data: @nflfastr") %>% 
  gtsave("off_nfc_epa.png", vwidth = 1000)



## Off all tables

all_tbl <- off_epa_tbl %>%
  mutate(
    cum_epa = round(cum_epa, 2)
  ) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>% 
  arrange(-cum_epa) %>% 
  mutate(rank = row_number())%>% 
  select(team_wordmark, rank, cum_epa) 
  

all_tbl %>% 
  filter(rank <= 16) %>% 
  gt() %>% 
  cols_label(
             team_wordmark = "Team",
             cum_epa = "Cumulative EPA") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_img_rows(team_wordmark) %>% 
  tab_options(
    table.margin.right = px(1000),
  ) %>% 
  tab_header("Expected Points Added at the End of the year") %>% 
  tab_source_note(source_note = "John Wilson | @jdwanalytics") %>%
  tab_source_note(source_note =  "data: @nflfastr") %>% 
  gtsave("all_epa.png", vwidth = 1000)

all_tbl %>% 
  filter(rank > 16) %>% 
  gt() %>% 
  cols_label(
    team_wordmark = "Team",
    cum_epa = "Cumulative EPA") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_img_rows(team_wordmark) %>% 
  tab_options(
    table.margin.right = px(1000),
  ) %>% 
#  tab_header("Expected Points Added at the End of the year") %>% 
  tab_source_note(source_note = "John Wilson | @jdwanalytics") %>%
  tab_source_note(source_note =  "data: @nflfastr") %>% 
  gtsave("all_epa2.png", vwidth = 1000)


###################### Defensive EPA
##### Def Epa cumulative graph

def_epa <- pbp %>% 
  filter(!is.na(epa), !is.na(defteam)) %>% 
  group_by(defteam) %>% 
  mutate(play = n()) %>% 
  ungroup() %>%
  select(epa, defteam) %>%
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr"))
  
def_epa$play_num <- ave(def_epa$epa, def_epa$defteam, FUN = seq_along)
def_epa$csum <- ave(def_epa$epa, def_epa$defteam, FUN = cumsum)

def_epa <- def_epa %>% 
  mutate(
    conf= ifelse(defteam %in% afc_teams, "AFC", "NFC"),
    div = case_when(
      defteam %in% south ~ "SOUTH",
      defteam %in% north ~ "NORTH",
      defteam %in% east ~ "EAST",
      defteam %in% west ~ "WEST"
    )
  )

def_epa %>%
  ggplot(aes(x = play_num, y = csum)) + 
  geom_smooth(aes(group = defteam, col = team_color), se = F, linewidth = 2) + 
  scale_color_identity(aesthetics = c("fill", "color")) + 
  scale_y_reverse() +
  facet_grid(conf ~ div) + 
  labs(
    x = "Number of Plays",
    y = "Cumulative EPA",
    title = "Cumulative Defensive EPA Throughout The Season",
    subtitle = "Measures how good a defense is at taking away points",
    caption = "John Wilson | @jdwanalytics
                  data: @nflfastR") + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = .5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = .5, size = 15),
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15)
  )

##### Def AFC / NFC Tables

def_epa_tbl <- pbp %>% 
  filter(!is.na(epa), !is.na(defteam)) %>% 
  group_by(defteam) %>% 
  mutate(play = n()) %>% 
  summarize(plays = n(), cum_epa = sum(epa))%>%
  arrange(cum_epa)

def_epa_tbl <- def_epa_tbl %>% 
  mutate(
    conf= ifelse(defteam %in% afc_teams, "AFC", "NFC"),
    div = case_when(
      defteam %in% south ~ "SOUTH",
      defteam %in% north ~ "NORTH",
      defteam %in% east ~ "EAST",
      defteam %in% west ~ "WEST"
    )
  )

# AFC and NFC table Defensive Table

def_epa_tbl_nfc <- def_epa_tbl %>% 
  filter(conf == "NFC") %>% 
  arrange(div, cum_epa) %>% 
  mutate(
    cum_epa = round(cum_epa, 2),
    rank = ((row_number() - 1) %% 4) + 1
  ) %>% 
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr")) %>% 
  select(team_wordmark, rank, cum_epa, div) 

def_epa_tbl_afc <- def_epa_tbl %>% 
  filter(conf == "AFC") %>% 
  arrange(div, cum_epa) %>% 
  mutate(
    cum_epa = round(cum_epa, 2),
    rank = ((row_number() - 1) %% 4) + 1
  ) %>% 
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr")) %>% 
  select(team_wordmark, rank, cum_epa, div) 

def_epa_tbl_afc %>% 
  gt() %>% 
  cols_label(
    team_wordmark = "Team",
    cum_epa = "Cumulative EPA") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_img_rows(team_wordmark) %>% 
  tab_options(
    table.margin.right = px(1000),
  ) %>% 
  tab_header("AFC Expected Points Taken Away at the End of the year") %>% 
  tab_source_note(source_note = "John Wilson | @jdwanalytics") %>%
  tab_source_note(source_note =  "data: @nflfastr") %>% 
  gtsave("def_afc_epa.png", vwidth = 1000)


def_epa_tbl_nfc %>% 
  gt() %>% 
  cols_label(
    team_wordmark = "Team",
    cum_epa = "Cumulative EPA") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_img_rows(team_wordmark) %>% 
  tab_options(
    table.margin.right = px(1000),
  ) %>% 
  tab_header("NFC Expected Points Taken Away at the End of the year") %>% 
  tab_source_note(source_note = "John Wilson | @jdwanalytics") %>%
  tab_source_note(source_note =  "data: @nflfastr") %>% 
  gtsave("def_nfc_epa.png", vwidth = 1000)


################ ALL defensive tables

all_def_tbl <- def_epa_tbl %>%
  mutate(
    cum_epa = round(cum_epa, 2)
  ) %>% 
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr")) %>% 
  arrange(cum_epa) %>% 
  mutate(rank = row_number())%>% 
  select(team_wordmark, rank, cum_epa) 

all_def_tbl %>% 
  filter(rank > 16) %>% 
  gt() %>% 
  cols_label(
    team_wordmark = "Team",
    cum_epa = "Cumulative EPA") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_img_rows(team_wordmark) %>% 
  tab_options(
    table.margin.right = px(1000),
  ) %>% 
 # tab_header("Expected Points Added at the End of the year") %>% 
  tab_source_note(source_note = "John Wilson | @jdwanalytics") %>%
  tab_source_note(source_note =  "data: @nflfastr") %>% 
  gtsave("def_all2.png", vwidth = 1000)


all_def_tbl %>% 
  filter(rank <= 16) %>% 
  gt() %>% 
  cols_label(
    team_wordmark = "Team",
    cum_epa = "Cumulative EPA") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_img_rows(team_wordmark) %>% 
  tab_options(
    table.margin.right = px(1000),
  ) %>% 
  tab_header("Expected Points Taken Away at the End of the year") %>% 
  tab_source_note(source_note = "John Wilson | @jdwanalytics") %>%
  tab_source_note(source_note =  "data: @nflfastr") %>% 
  gtsave("def_all.png", vwidth = 1000)




###################################################################
# Yards Gained Table

yards_gained <- pbp %>% 
  filter(!is.na(posteam), !is.na(yards_gained)) %>% 
  group_by(posteam) %>% 
  summarize(total_yards_gained = sum(yards_gained))
  
yards_given <- pbp %>% 
  filter(!is.na(defteam), !is.na(yards_gained)) %>% 
  group_by(defteam) %>% 
  summarize(total_yards_given = sum(yards_gained))

yards_dif <- yards_gained %>% 
  left_join(yards_given, by = c("posteam" = "defteam")) %>% 
  mutate(tot_yards_dif = total_yards_gained - total_yards_given) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

yards_dif %>% 
  ggplot(aes(x = fct_reorder(posteam, tot_yards_dif), y = tot_yards_dif)) +
  geom_col(aes(color = team_color2, fill = team_color), width = .85, linewidth = .85) + 
  scale_color_identity(aesthetics = c("fill", "color")) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.0375, alpha = .9) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(
    title = "Net Yards Gained",
    subtitle = "Total Yards Gain - Total Yards Given Up",
    x = "Team",
    y = "Net Yards",
    caption = "John Wilson | @jdwanalytics
                  data: @nflfastR") + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = .5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = .5, size = 15),
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15)
  )


###################################################
# Momentum Plays
#Defensive big plays: Forced fumble, tackle for loss, qb hit, sack, interception, safety
#Offensive: passes over 20 yards and rushes over 10 yards, touchdown

def_mom <- pbp %>% 
  filter(!is.na(defteam)) %>% 
  mutate(
    def_mom_play = case_when(
      fumble_lost == 1 ~ 1,
      tackled_for_loss == 1 ~ 1,
      sack == 1 ~ 1,
      interception == 1 ~ 1,
      safety == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  group_by(defteam) %>% 
  summarize(def_mom_plays = sum(def_mom_play))

off_mom <- pbp %>% 
  filter(!is.na(posteam)) %>% 
  mutate(
    off_mom_play = case_when(
      rush == 1 & yards_gained > 10 ~ 1,
      pass == 1 & yards_gained > 20 ~ 1,
      touchdown == 1 ~ 1,
      fourth_down_converted == 1 ~ 1, 
      TRUE ~ 0
    )
  ) %>% 
  group_by(posteam) %>% 
  summarize(off_mom_plays = sum(off_mom_play))

momentum_play <- off_mom %>% 
  left_join(def_mom, by = c("posteam" = "defteam")) 

mom_long <- momentum_play %>% 
  pivot_longer(cols = -posteam, names_to = "momentum_type", values_to = "momentum_plays") %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>% 
  mutate(momentum_type = if_else(momentum_type == "def_mom_plays", "Def Momentum Plays", "Off Momentum Plays"))

mom_long %>% 
  ggplot(aes(x = fct_reorder(posteam, momentum_plays), y = momentum_plays)) + 
  geom_col(aes(color = team_color2, fill = team_color), width = .85, linewidth = .85) + 
  scale_color_identity(aesthetics = c("fill", "color")) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.0375, alpha = .9) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  facet_grid(momentum_type ~ .) + 
  labs(
    title = "Number of Offensive and Defensive Momentum Plays",
    subtitle = "Order by least total momentum plays to most",
    x = "",
    y = "Number of Momentum Play",
    caption = "John Wilson | @jdwanalytics
                  data: @nflfastR") + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = .5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = .5, size = 15),
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15)
  )

momentum_play %>% 
  ggplot(aes(x = fct_reorder(posteam, off_mom_plays), y = off_mom_plays)) + 
  geom_col()

momentum_play %>% 
  mutate(total = off_mom_plays + def_mom_plays) %>% 
  arrange(-total)




