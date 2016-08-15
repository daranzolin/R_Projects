library(rvest)
library(dplyr)
library(magrittr)
library(stringr)

#Function to scrape data from basketball-reference

get_nba_season_data <- function(year) {
  
  options(warn = -1) #turns off later left_join() warning about coercion to factor
  
    url <- paste0("http://www.basketball-reference.com/leagues/NBA_", year, ".html")
    
    #Get season data
    season_data <- url %>%
      read_html() %>% 
      html_nodes("#team") %>%
      html_table(header = FALSE, fill = FALSE) %>%
      data.frame()

    names(season_data) <- season_data %>% 
      slice(1) %>%
      str_replace_all("%", "_pct") %>%
      str_replace_all("/", "_per_") %>%
      make.names(unique = TRUE, allow = TRUE) %>% 
      str_replace_all("[.]", "") %>%
      str_replace_all("1", "_pct") %>% 
      str_replace_all("X", "") %>% 
      tolower()
    
    season_data %<>%
      slice(-c(1, nrow(.))) %>% 
      mutate(year = rep(year, nrow(.)),
             playoffs = ifelse(grepl("[*]", team), "playoffs", "lottery")) %>% 
      mutate(team = str_replace_all(team, "[*]", "")) %>% 
      select(-rk, -g)
    
    #Now get miscellaneous data
    misc_data <- url %>%
      read_html() %>% 
      html_nodes("#misc") %>%
      html_table(header = FALSE, fill = FALSE) %>%
      data.frame()
    
    names(misc_data) <- misc_data %>% 
      slice(2) %>%
      str_replace_all("%", "_pct") %>%
      str_replace_all("/", "_per_") %>%
      make.names(unique = TRUE, allow = TRUE) %>% 
      str_replace_all("[.]", "") %>%
      str_replace_all("X", "") %>% 
      tolower()
    
    misc_data %<>%
      slice(-c(1:2, nrow(.))) %>% 
      select(-rk, -arena, -attendance) %>% 
      mutate(team = str_replace_all(team, "[*]", "")) 
    
    names(misc_data)[(ncol(misc_data) - 4):ncol(misc_data)] <- 
      paste0('opp_', names(misc_data)[(ncol(misc_data) - 4):ncol(misc_data)]) 
    
    all_data <- left_join(season_data, misc_data, by = "team")
    
    #Get oppononent data
    opp_data <- url %>%
      read_html() %>% 
      html_nodes("#opponent") %>%
      html_table(header = FALSE, fill = FALSE) %>%
      data.frame()
    
    names(opp_data) <- opp_data %>% 
      slice(1) %>% 
      str_replace_all("%", "_pct") %>%
      str_replace_all("/", "_per_") %>%
      make.names(unique = TRUE, allow = TRUE) %>% 
      str_replace_all("[.]", "") %>%
      str_replace_all("X", "") %>%
      str_replace_all("[1]", "_pct") %>% 
      tolower() %>% 
      paste0("opp_", .)
    
    opp_data %<>%
      slice(-c(1, nrow(.))) %>% 
      select(-c(1, 3:4)) %>% 
      rename(team = opp_team) %>% 
      mutate(team = str_replace_all(team, "[*]", "")) 
    
    all_data <- left_join(all_data, opp_data, by = "team")
    
    #Get wins data
    wins_url <- paste0('http://www.basketball-reference.com/leagues/NBA_', year, '_standings.html')
    wins_data <- wins_url %>%
      read_html() %>% 
      html_nodes("#expanded-standings td:nth-child(2) , 
                 #expanded-standings td:nth-child(2) , 
                 #expanded-standings td:nth-child(3) , 
                 #expanded-standings .sort_default_asc+ .tooltip") %>%
      html_text()
    
    teams <- wins_data[c(TRUE, FALSE)]
    wins <- substr(wins_data[c(FALSE, TRUE)], 1, 2)
    
    wins_df <- data.frame(team = teams,
                          wins = wins)
    
    all_season_data <- left_join(all_data, wins_df, by = "team")
    
    champion <- url %>% 
      read_html() %>%
      html_nodes('.sub_index+ p a') %>% 
      html_text()
    
    all_season_data %<>%
      mutate(champion = ifelse(team == champion, "Champion", "Non-Champion"))%>% 
      select(team, playoffs, champion, everything()) %>% 
      rename(threes = `3p`, 
             three_rate = `3par`)
    
    all_season_data[,4:ncol(all_season_data)] <- sapply(all_season_data[,4:ncol(all_season_data)], as.character)
    all_season_data[,4:ncol(all_season_data)] <- sapply(all_season_data[,4:ncol(all_season_data)], as.numeric)
    
    return(all_season_data)
}

years <- 1980:2015
nba_data <- data.frame()
for (i in seq_along(years)) {
  year_data <- get_nba_season_data(years[i])
  nba_data <- rbind(nba_data, year_data)
}

nba_data %<>%
  mutate(decade = ifelse(year %in% 1980:1989, "eighties",
                         ifelse(year %in% 1990:1999, "nineties",
                                ifelse(year %in% 2000:2009, "ones", "tens"))))

saveRDS(nba_data, file = "nbadata.RDS")

