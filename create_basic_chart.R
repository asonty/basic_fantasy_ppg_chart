"
author: Adam Sonty (@sonty_1)
desc: this script contains the code from my blog's post entitled 'Interactively
      Visualizing Fantasy Football Scoring'
"

# =============================================================================
# dependencies
# =============================================================================
# only run the following 2 commands if you haven't installed these packages
# install.packages('dplyr')
# install.packages('plotly')

# load packages
library(plotly)
library(dplyr)
library(readr)

# =============================================================================
# data import
# =============================================================================
nfl_stats_url <- 'https://raw.githubusercontent.com/asonty/pfr-scrapers/master/data/2019_all_offensive_data.csv'
nfl_stats <- readr::read_csv(nfl_stats_url)

# =============================================================================
# data cleaning
# =============================================================================
# view columns names in dataset
colnames(nfl_stats)

# select relevant columns
fantasy_data <- nfl_stats %>% 
  select(`_player`,`_pos`,
         `_tm`,`_opp`, `_week`,
         passing_att, passing_yds, passing_td, passing_int, passing_sk,
         rushing_att, rushing_yds, rushing_td,
         receiving_tgt, receiving_rec, receiving_yds, receiving_td,
         scoring_2pm, fumbles_fl)

# rename columns with leading underscores
colnames(fantasy_data) <- c('player', 'pos', 'team', 'opp', 'week', 
                            colnames(fantasy_data)[6:19])

# check new column names
colnames(fantasy_data)

# compute players' weekly fantasy output
# change these values to match up with your league's scoring format
pass_yd_pt <- (1 / 25)
pass_td_pt <- 4.00
pass_in_pt <- -2.00
pass_sk_pt <- -0.50 
rush_yd_pt <- (1 / 10)
rush_td_pt <- 6.00
ppr_rec_pt <- 1.00
recp_yd_pt <- (1 / 10)
recp_td_pt <- 6.00
two_cnv_pt <- 2.00
fumb_fl_pt <- -2.00

fantasy_data$pts <- (fantasy_data$passing_yds*pass_yd_pt + fantasy_data$passing_td*pass_td_pt + 
                       fantasy_data$passing_int*pass_in_pt  + fantasy_data$passing_sk*pass_sk_pt +
                       fantasy_data$rushing_yds*rush_yd_pt + fantasy_data$rushing_td*rush_td_pt + 
                       fantasy_data$receiving_rec*ppr_rec_pt + fantasy_data$receiving_yds*recp_yd_pt + 
                       fantasy_data$receiving_td*recp_td_pt +
                       fantasy_data$scoring_2pm*two_cnv_pt + fantasy_data$fumbles_fl*fumb_fl_pt)

# remove players who scored 0 fantasy points in a week
fantasy_data <- fantasy_data %>%
  select(player, pos, team, opp, week, pts) %>%
  filter(pts != 0)

# remove players who only played 1 game
fantasy_data <- fantasy_data %>%
  inner_join(fantasy_data %>% 
               group_by(player, pos, team) %>%
               tally(),
             by = c('player', 'pos', 'team')) %>%
  filter(n > 1)

# select players in position of interest
# change this from 'WR' to whichever offensive position you're interested in
fantasy_data <- fantasy_data %>%
  filter(pos == 'WR')

# check number of players at your selected position contained in dataset
fantasy_data %>%
  distinct(player) %>%
  nrow()

# create new column of player labels that includes team name
fantasy_data$tm_player <- paste('[', fantasy_data$team, '] ', 
                                fantasy_data$player, sep='')

# =============================================================================
# chart theme
# =============================================================================
# ---- margin ----
margin_format <- list(l = 50, r = 50, t = 50, b = 50, pad = 4)

# ---- title ----
title_text <- '2019-20 WR Points Per Game'
title_font <- list(size = 15,
                   color = '#111111')

# ---- axes ----
x_axis_title <- 'Week'
y_axis_title <- 'Points Per Game (PPG)'
axis_font <- list(size = 15,
                  color = '#333333')
tick_font <- list(size = 14,
                  color = '#333333')
x_axis_format <- list(title = x_axis_title,
                      titlefont = axis_font,
                      dtick = 1.0,
                      tick0 = 1.0,
                      tickmode = 'linear',
                      tickfont = tick_font)
y_axis_format <- list(title = y_axis_title,
                      titlefont = axis_font,
                      dtick = 5.0,
                      tick0 = 0.0,
                      tickmode = 'linear',
                      tickfont = tick_font)

# ---- legend ----
legend_font <- list(size = 12,
                    color = '#333333')
legend_format <- list(x = 1,
                      y = 1,
                      font = legend_font)

# ---- tooltip ----
hover_font <- list(size = 12,
                   color = '#444444')
hover_format <- list(align = 'left',
                     bgcolor = 'white',
                     bordercolor = 'transparent',
                     font = hover_font)

# ---- annotations ----
caption_font <- list(size = 12,
                     color = '#adadad')
caption_format <- list(x = 1,
                       y = 0,
                       text = 'Source: Pro Football Reference',
                       showarrow = FALSE,
                       xref = 'paper',
                       yref = 'paper',
                       xanchor = 'right',
                       yanchor = 'bottom',
                       xshift = 0,
                       yshift = 0,
                       font = caption_font)

# ---- format base chart ----
fantasy_chart_theme <- plot_ly() %>%
  layout(autosize = FALSE,
         title = title_text, font = title_font,
         xaxis = x_axis_format,
         yaxis = y_axis_format) %>%
  layout(margin = margin_format) %>%
  layout(legend = legend_format, showlegend = TRUE) %>%
  layout(annotations = caption_format)

# ** this is the important part **
# ---- drop-down team filter ----
# create list of teams for dropdown filter
team_list = vector(mode = 'list')
for(i in 1:32) {
  team_list <- append(team_list,
                      list(list(method = 'restyle',
                                args = list('transforms[0].value',
                                            unique(fantasy_data$team)[i]),
                                label = unique(fantasy_data$team[i])
                                )
                           )
                      )
}

# add filtering component to chart theme
fantasy_chart_theme <- fantasy_chart_theme %>%
  layout(
    updatemenus = list(
      list(type = 'dropdown',
           active = 0,
           buttons = team_list
      )
    )
  )

# =============================================================================
# create chart
# =============================================================================
# add 75th percentile trace to chart
# then add player traces
fantasy_chart <- fantasy_chart_theme %>%
  add_trace(data = fantasy_data %>%
              group_by(week) %>%
              summarise(q3 = quantile(pts, 0.75)),
            x = ~week,
            y = ~q3,
            type = 'scatter',
            mode = 'lines',
            name = '75th %ile',
            line = list(color = '#000000',
                        dash = 'dot',
                        width = 1),
            hoverlabel = hover_format,
            hovertemplate = ~paste('Points: %{y:0.2f}',
                                   'Week: %{x}',
                                   sep = '\n')
  ) %>%
  add_trace(data = fantasy_data %>% 
              arrange(tm_player, week),
            x = ~week, 
            y = ~pts,
            text = ~opp,
            name = ~tm_player,
            type = 'scatter', 
            mode = 'lines+markers',
            hoverlabel = hover_format,
            hovertemplate = ~paste('Points: %{y}',
                                   'Week: %{x}',
                                   'Versus: %{text}',
                                   sep = '\n'),
            transforms = list(list(type = 'filter',
                                   target = ~team,
                                   operation = '=',
                                   value = unique(fantasy_data$team)[1])
            )
  ) 

# view chart
fantasy_chart
