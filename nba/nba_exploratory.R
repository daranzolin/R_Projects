nba_data <- readRDS("data/nbadata.RDS")

#Start making weird plots and models stream-of-consciousness style

ggplot(nba_data, aes(threes, wins, color = decade)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~decade) +
  labs(x = "Threes Made", y = "Wins") +
  geom_text(data = nba_data_clean %>% filter(team == "Golden State Warriors", year == "2015"), aes(threes, wins, label = team)) +
  ggtitle("Wins ~ Threes Made by Decade") 

ggplot(nba_data %>% 
         group_by(year) %>% 
         summarize(meanpace = mean(pace)),
       aes(year, meanpace)) +
  geom_point() +
  geom_smooth()

ggplot(nba_data %>% 
         group_by(year) %>% 
         summarize(meanpts = mean(pts)),
       aes(year, meanpts)) +
  geom_point() +
  geom_smooth()

nba_data_clean %>% 
  group_by(decade) %>% 
  summarize(correlations = cor(threes, wins))

broomdf <- nba_data %>%
  select(year, pts, starts_with("opp")) %>%  
  group_by(year) %>% 
  do(tidy(lm(pts ~ . - year, data = .)))

ggplot(broomdf %>% filter(term != "(Intercept)"), aes(year, log(estimate))) +
  geom_point() +
  geom_smooth() +
  facet_grid(~term)

correlations <- apply(nba_data[sapply(nba_data, is.numeric)], 
                      2, 
                      function(x) with(nba_data, cor(x, pts, use = "complete.obs")))

mod_names <- correlations[correlations > 0.4]
mod_names <- na.omit(mod_names)
