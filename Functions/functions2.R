#Data Cleaning and creating the winners data
cleaning = function(Scores){
  Scores %>% 
  mutate(home_team = case_when(
    home_team == "Packers" ~ "Green Bay Packers",
    home_team == "Football Team" ~ "Washington Football Team",
    home_team == "Bills" ~ "Buffalo Bills",
    home_team == "Falcons" ~ "Atlanta Falcons",
    home_team == "Ravens" ~ "Baltimore Ravens",
    home_team == "Chiefs" ~ "Kansas City Chiefs",
    home_team == "Titans" ~ "Tennessee Titans",
    home_team == "Colts" ~ "Indianapolis Colts",
    home_team == "Bengals" ~ "Cincinnati Bengals",
    home_team == "49ers" ~ "San Francisco 49ers",
    home_team == "Giants" ~ "New York Giants",
    home_team == "Lions" ~ "Detroit Lions",
    home_team == "Steelers" ~ "Pittsburgh Steelers",
    home_team == "Texans" ~ "Houston Texans",
    home_team == "Broncos" ~ "Denver Broncos",
    home_team == "Buccaneers" ~ "Tampa Bay Buccaneers",
    home_team == "Cardinals" ~ "Arizona Cardinals",
    home_team == "Bears" ~ "Chicago Bears",
    home_team == "Panthers" ~ "Carolina Panthers",
    home_team == "Eagles" ~ "Philadelphia Eagles",
    home_team == "Jets" ~ "New York Jets",
    home_team == "Vikings" ~ "Minnesota Vikings",
    home_team == "Dolphins" ~ "Miami Dolphins",
    home_team == "Jaguars" ~ "Jacksonville Jaguars",
    home_team == "Browns" ~ "Cleveland Browns",
    home_team == "Chargers" ~ "Los Angeles Chargers",
    home_team == "Seahawks" ~ "Seattle Seahawks",
    home_team == "Cowboys" ~ "Dallas Cowboys",
    home_team == "Patriots" ~ "New England Patriots",
    home_team == "Saints" ~ "New Orleans Saints",
    home_team == "Raiders" ~ "Las Vegas Raiders",
    home_team == "Rams" ~ "Los Angeles Rams")) %>%
  mutate(away_team = case_when(
    away_team == "Packers" ~ "Green Bay Packers",
    away_team == "Football Team" ~ "Washington Football Team",
    away_team == "Bills" ~ "Buffalo Bills",
    away_team == "Falcons" ~ "Atlanta Falcons",
    away_team == "Ravens" ~ "Baltimore Ravens",
    away_team == "Chiefs" ~ "Kansas City Chiefs",
    away_team == "Titans" ~ "Tennessee Titans",
    away_team == "Colts" ~ "Indianapolis Colts",
    away_team == "Bengals" ~ "Cincinnati Bengals",
    away_team == "49ers" ~ "San Francisco 49ers",
    away_team == "Giants" ~ "New York Giants",
    away_team == "Lions" ~ "Detroit Lions",
    away_team == "Steelers" ~ "Pittsburgh Steelers",
    away_team == "Texans" ~ "Houston Texans",
    away_team == "Broncos" ~ "Denver Broncos",
    away_team == "Buccaneers" ~ "Tampa Bay Buccaneers",
    away_team == "Cardinals" ~ "Arizona Cardinals",
    away_team == "Bears" ~ "Chicago Bears",
    away_team == "Panthers" ~ "Carolina Panthers",
    away_team == "Eagles" ~ "Philadelphia Eagles",
    away_team == "Jets" ~ "New York Jets",
    away_team == "Vikings" ~ "Minnesota Vikings",
    away_team == "Dolphins" ~ "Miami Dolphins",
    away_team == "Jaguars" ~ "Jacksonville Jaguars",
    away_team == "Browns" ~ "Cleveland Browns",
    away_team == "Chargers" ~ "Los Angeles Chargers",
    away_team == "Seahawks" ~ "Seattle Seahawks",
    away_team == "Cowboys" ~ "Dallas Cowboys",
    away_team == "Patriots" ~ "New England Patriots",
    away_team == "Saints" ~ "New Orleans Saints",
    away_team == "Raiders" ~ "Las Vegas Raiders",
    away_team == "Rams" ~ "Los Angeles Rams")) %>%
  mutate(winner = case_when(home_score > away_score ~ home_team,
                            home_score == away_score ~ "TIE",
                            home_score < away_score ~ away_team)) 
}

#Data Cleaning function for CBS Schedule page and creating the winners data
cleaning2 = function(Scores){
  Scores %>% 
    filter(Score1!="pm") %>% #accounting for postponed games.  
    mutate(TM1 = case_when(
      TM1 == "GB" ~ "Green Bay Packers",
      TM1 == "WAS" ~ "Washington Football Team",
      TM1 == "BUF" ~ "Buffalo Bills",
      TM1 == "ATL" ~ "Atlanta Falcons",
      TM1 == "BAL" ~ "Baltimore Ravens",
      TM1 == "KC" ~ "Kansas City Chiefs",
      TM1 == "TEN" ~ "Tennessee Titans",
      TM1 == "IND" ~ "Indianapolis Colts",
      TM1 == "CIN" ~ "Cincinnati Bengals",
      TM1 == "SF" ~ "San Francisco 49ers",
      TM1 == "NYG" ~ "New York Giants",
      TM1 == "DET" ~ "Detroit Lions",
      TM1 == "PIT" ~ "Pittsburgh Steelers",
      TM1 == "HOU" ~ "Houston Texans",
      TM1 == "DEN" ~ "Denver Broncos",
      TM1 == "TB" ~ "Tampa Bay Buccaneers",
      TM1 == "ARI" ~ "Arizona Cardinals",
      TM1 == "CHI" ~ "Chicago Bears",
      TM1 == "CAR" ~ "Carolina Panthers",
      TM1 == "PHI" ~ "Philadelphia Eagles",
      TM1 == "NYJ" ~ "New York Jets",
      TM1 == "MIN" ~ "Minnesota Vikings",
      TM1 == "MIA" ~ "Miami Dolphins",
      TM1 == "JAC" ~ "Jacksonville Jaguars",
      TM1 == "CLE" ~ "Cleveland Browns",
      TM1 == "LAC" ~ "Los Angeles Chargers",
      TM1 == "SEA" ~ "Seattle Seahawks",
      TM1 == "DAL" ~ "Dallas Cowboys",
      TM1 == "NE" ~ "New England Patriots",
      TM1 == "NO" ~ "New Orleans Saints",
      TM1 == "LV" ~ "Las Vegas Raiders",
      TM1 == "LAR" ~ "Los Angeles Rams")) %>%
    mutate(TM2 = case_when(
      TM2 == "GB" ~ "Green Bay Packers",
      TM2 == "WAS" ~ "Washington Football Team",
      TM2 == "BUF" ~ "Buffalo Bills",
      TM2 == "ATL" ~ "Atlanta Falcons",
      TM2 == "BAL" ~ "Baltimore Ravens",
      TM2 == "KC" ~ "Kansas City Chiefs",
      TM2 == "TEN" ~ "Tennessee Titans",
      TM2 == "IND" ~ "Indianapolis Colts",
      TM2 == "CIN" ~ "Cincinnati Bengals",
      TM2 == "SF" ~ "San Francisco 49ers",
      TM2 == "NYG" ~ "New York Giants",
      TM2 == "DET" ~ "Detroit Lions",
      TM2 == "PIT" ~ "Pittsburgh Steelers",
      TM2 == "HOU" ~ "Houston Texans",
      TM2 == "DEN" ~ "Denver Broncos",
      TM2 == "TB" ~ "Tampa Bay Buccaneers",
      TM2 == "ARI" ~ "Arizona Cardinals",
      TM2 == "CHI" ~ "Chicago Bears",
      TM2 == "CAR" ~ "Carolina Panthers",
      TM2 == "PHI" ~ "Philadelphia Eagles",
      TM2 == "NYJ" ~ "New York Jets",
      TM2 == "MIN" ~ "Minnesota Vikings",
      TM2 == "MIA" ~ "Miami Dolphins",
      TM2 == "JAC" ~ "Jacksonville Jaguars",
      TM2 == "CLE" ~ "Cleveland Browns",
      TM2 == "LAC" ~ "Los Angeles Chargers",
      TM2 == "SEA" ~ "Seattle Seahawks",
      TM2 == "DAL" ~ "Dallas Cowboys",
      TM2 == "NE" ~ "New England Patriots",
      TM2 == "NO" ~ "New Orleans Saints",
      TM2 == "LV" ~ "Las Vegas Raiders",
      TM2 == "LAR" ~ "Los Angeles Rams")) %>%
    mutate(winner = case_when(as.numeric(Score1) > as.numeric(Score2) ~ TM1,
                              as.numeric(Score1) == as.numeric(Score2) ~ "TIE",
                              as.numeric(Score1) < as.numeric(Score2) ~ TM2))
}
#home team cleaning
cleaning3 = function(Scores){
  Scores %>% 
    mutate(home_team = case_when(
      home_team == "Green Bay" ~ "Green Bay Packers",
      home_team == "Washington" ~ "Washington Football Team",
      home_team == "Buffalo" ~ "Buffalo Bills",
      home_team == "Atlanta" ~ "Atlanta Falcons",
      home_team == "Baltimore" ~ "Baltimore Ravens",
      home_team == "Kansas City" ~ "Kansas City Chiefs",
      home_team == "Tennessee" ~ "Tennessee Titans",
      home_team == "Indianapolis" ~ "Indianapolis Colts",
      home_team == "Cincinnati" ~ "Cincinnati Bengals",
      home_team == "San Francisco" ~ "San Francisco 49ers",
      home_team == "N.Y. Giants" ~ "New York Giants",
      home_team == "Detroit" ~ "Detroit Lions",
      home_team == "Pittsburgh" ~ "Pittsburgh Steelers",
      home_team == "Houston" ~ "Houston Texans",
      home_team == "Denver" ~ "Denver Broncos",
      home_team == "Tampa Bay" ~ "Tampa Bay Buccaneers",
      home_team == "Arizona" ~ "Arizona Cardinals",
      home_team == "Chicago" ~ "Chicago Bears",
      home_team == "Carolina" ~ "Carolina Panthers",
      home_team == "Philadelphia" ~ "Philadelphia Eagles",
      home_team == "N.Y. Jets" ~ "New York Jets",
      home_team == "Minnesota" ~ "Minnesota Vikings",
      home_team == "Miami" ~ "Miami Dolphins",
      home_team == "Jacksonville" ~ "Jacksonville Jaguars",
      home_team == "Cleveland" ~ "Cleveland Browns",
      home_team == "L.A. Chargers" ~ "Los Angeles Chargers",
      home_team == "Seattle" ~ "Seattle Seahawks",
      home_team == "Dallas" ~ "Dallas Cowboys",
      home_team == "New England" ~ "New England Patriots",
      home_team == "New Orleans" ~ "New Orleans Saints",
      home_team == "Las Vegas" ~ "Las Vegas Raiders",
      home_team == "L.A. Rams" ~ "Los Angeles Rams"))
}

#Creating a list of winners for each week

weekly_winners = function(weeks){
  Scores %>% select(week, winner) %>% 
    filter(week == weeks) %>%
    select(winner)
}

#Creating a list of cbs percentages for each week

cbs_percent = function(weeks){
  cbs %>% select(week, Percent) %>% 
    filter(week == weeks) %>%
    select(Percent)
}

#Creating a list of cbs season percentages for each week

cbs_season_percent = function(weeks){
  cbs_season %>% select(week, Percent) %>% 
    filter(week == weeks) %>%
    select(Percent)
}

#Creating a list of cbs experts beat for each week

experts_beat = function(cbs_weekly_percent, weekly_win_percentage){
  cbs_weekly_percent %>% mutate(beat = case_when(Percent<=weekly_win_percentage~1,
                                                 TRUE~0)) %>% 
    summarise(sum(beat))
}

#Creating a list of number of cbs experts predictions by week

experts_tot = function(cbs_weekly_percent){
  cbs_weekly_percent %>%
    mutate(row = row_number()) %>% 
    filter(row==max(row)) %>% 
    select(row) %>% pull()
}

#Creating a list of ESPN percentages for each week

espn_percent = function(weeks){
  espn %>% select(week, Percent) %>% 
    filter(week == weeks) %>%
    select(Percent)
}

#Creating a list of ESPN season percentages for each week

espn_season_percent = function(weeks){
  espn_season %>% select(week, Percent) %>% 
    filter(week == weeks) %>%
    select(Percent)
}

#Creating a list of how many games each week.

week_number_games = function(weeks){
  dim(winners[[weeks]])[1]
}

#needs work, getting cbs wins per week
CBS_weekly = function(winners, cbs){
  CBS_winners = if_else(winners[[weeks]] == CBS_tot[[weeks]], 1, 0)
}

#Creating a list of everyones picks for each week
games_fn = function(x) {janitor::clean_names(x) %>%
    select(starts_with("game"))}  #Creating the set of just games

#Sorting function to get the prediction for each game and number for and against during 1 week.   
sorting = function(x) {t_x <- sort(table(x), decreasing=TRUE) # get the frequencies and sort them in decreasing order
list(Prediction=names(t_x)[1], # name of the value with highest frequency
     votes_for=t_x[1], # highest frequency
     votes_against=t_x[2]) # Lowest Freq
}

#function to appy the sorting function to each week and create a list of predictions for each week.
pred_table_fn = function(y){data.frame(Game=colnames(y), # apply sorting to each column
                                       t(sapply(y,sorting))) %>% 
    # turning NA in votes against column into 0
    mutate(votes_against = ifelse(is.na(votes_against)==T, 0, as.integer(votes_against))) %>% 
    #choosing prediction or tie if no prediction
    mutate(Prediction = ifelse(as.numeric(votes_for) == as.numeric(votes_against), "Tie", as.character(Prediction))) 
}

#function to add who won to the predictions
adding_winners = function(x,y){
  x %>% add_column(y)
}

#Function to get weekly results for instructors
results_fn = function(x,y){
  x %>% select(Prediction, Winner = winner, votes_for, votes_against) %>% 
    mutate(Correct = ifelse(Prediction =="Tie", "--", ifelse(Prediction == Winner, "Yes", "No"))) %>%
    mutate(`Correct Votes` = ifelse(Prediction == Winner, votes_for, ifelse(Winner == "TIE", 0, votes_against))) %>% 
    mutate(`Correct Percent` = round(as.numeric(`Correct Votes`)/(dim(inst.picks[[length(inst.picks)]])[1]),4)) %>%
    add_column(Game = 1:y) %>% 
    select(Game, Prediction, Winner, Correct, `Correct Votes`, `Correct Percent`)
}  

#Function to get weekly results for cadets
c_results_fn = function(x,y){
  x %>% select(Prediction, Winner = winner, votes_for, votes_against) %>% 
    mutate(Correct = ifelse(Prediction =="Tie", "--", ifelse(Prediction == Winner, "Yes", "No"))) %>%
    mutate(`Correct Votes` = ifelse(Prediction == Winner, votes_for, ifelse(Winner == "TIE", 0, votes_against))) %>% 
    mutate(`Correct Percent` = round(as.numeric(`Correct Votes`)/(dim(cdt.picks[[length(cdt.picks)]])[1]),4)) %>%
    add_column(Game = 1:y) %>% 
    select(Game, Prediction, Winner, Correct, `Correct Votes`, `Correct Percent`)
}  

#Function to get weekly results for combined picks
comb_results_fn = function(x,y){
  x %>% select(Prediction, Winner = winner, votes_for, votes_against) %>% 
    mutate(Correct = ifelse(Prediction =="Tie", "--", ifelse(Prediction == Winner, "Yes", "No"))) %>%
    mutate(`Correct Votes` = ifelse(Prediction == Winner, votes_for, ifelse(Winner == "TIE", 0, votes_against))) %>% 
    mutate(`Correct Percent` = round(as.numeric(`Correct Votes`)/(dim(inst.picks[[length(inst.picks)]])[1]+dim(cdt.picks[[length(cdt.picks)]])[1]),4)) %>%
    add_column(Game = 1:y) %>% 
    select(Game, Prediction, Winner, Correct, `Correct Votes`, `Correct Percent`)
}  

#function to pick how many games correct, incorrect, and not picked
weekly_group_correct_fn = function(x){
  count(x,Correct)
  
}

#function to calculate the number of games picked each week
weekly_games_picked_fn = function(x, y){
  (y-ifelse(identical(x$n[which(x$Correct == "--")], integer(0))==TRUE, 0,
            x$n[which(x$Correct == "--")]))
}

#Function to get how many correct picks each week
weekly_group_correct_picks_fn = function(x){
  x$n[which(x$Correct == "Yes")]
}

#Function to calculate the win percentage each week
weekly_win_percentage_fn = function(x,y){
  round(x/y,4)
}

# Creating a function to calculate weekly correct picks 
indiv_weekly_pred = function(x,y,z){
  indiv = x %>%
    select(Name, starts_with("Game")) 
  
  games_wk = indiv %>%
    select(-Name)
  
  indiv_correct_wk = NULL
  help = NULL
  for (i in 1:length(indiv$Name)){
    for(j in 1:length(games_wk)){
      help[j] = if_else(games_wk[i,j]==y[j,1],1,0)
      indiv_correct_wk[i] = sum(help)
    }
  }
  

  week_num = glue("Week {z}") # use this to have "Week x" as the column headers
  season_wk = indiv %>%
    select(Name) %>%
    add_column(!!week_num := indiv_correct_wk)
  
  return(season_wk)
}

#function to get individual percentages for each week.
indiv_percent = function(x,y){
  x %>% mutate(x[,2]/y)
}

#function to combine cadet and inst picks
comb_picks_fn = function(x,y){
  bind_rows(x,y)
}

#function for matching cadet and instructor picks.
matched_fn = function(x,y){
  x %>% add_column(C_Pred = y$Prediction) %>% 
    mutate(Prediction = ifelse(Prediction == C_Pred, Prediction, "--"),
           Correct = ifelse(Prediction == C_Pred, Correct, "--")) %>% 
    select(Game, Prediction, Correct)
}

#function to get the matched percent correct each week.  
matched_percent_fn = function(x){
  Yes = x %>% 
    count(Correct) %>% 
    filter(Correct =="Yes") %>% 
    select(n) %>% 
    pull()
  
  No = x %>% 
    count(Correct) %>% 
    filter(Correct =="No") %>% 
    select(n) %>% 
    pull()
  
  return(round(Yes/(Yes+No),4))
}
