library(tidylog)
library(tidyverse)
library(magrittr)
library(anonymizer)
library(nlme)
library(tidyr)
library(lubridate)
library(caTools)
library(lme4)


df <- readxl::read_xlsx("filename", sheet = 1)
glimpse(df)

df2 <- df %>%
  filter(`Full session` == 'No') %>%
  filter(!grepl("Rest|Half|Quarter", `Period Name`)) %>%
  mutate(
    `Period Name` = tolower(`Period Name`),
    `Period Name` = gsub("3g", "", `Period Name`),
    `Period Name` = gsub("\\s*\\([^\\)]+\\)","",`Period Name`),
    `Period Name` = trimws(`Period Name`, which = "both"),
    seconds = (hour(`Field Time`)*60^2) + (minute(`Field Time`)*60) + 
      second(`Field Time`),
    mod_speed_dist = `Vel Zone 2 Dist` + `Vel Zone 3 Dist` + `Vel Zone 4 Dist`,
    high_speed_dist = `Vel Zone 5 Dist` + `Vel Zone 6 Dist` + `Vel Zone 7 Dist`,
  ) %>% select(2,3,4,66 ,7,8,9,67, 68)


drill <- df2 %>%
  count(`Period Name`) %>%
  arrange(desc(n)) %>%
  filter(n > 6000)

playerlist <- df2 %>%
  mutate(
    `Player Name` = case_when(
      `Player Name` == 'XXX_XX' ~
        'XXXXXX',
      `Player Name` == 'yyYYY' ~
        'yyyyyy',
      T ~ `Player Name`
    )
  ) %>%
  count(`Player Name`) %>%
  arrange(desc(n)) %>% glimpse()
filter(n >= 2700)


listplayer <- unique(playerlist$`Player Name`)
listdrill <- unique(drill$`Period Name`)

df2 %<>%
  filter(`Player Name` %in% listplayer) %>% 
  filter(`Period Name` %in% listdrill)
mutate(
  `Player Name` = anonymize(`Player Name`),
  `Player Name` = substr(`Player Name`, 1, 5),
  `Period Name` = case_when(`Period Name` == "15v15" ~ " drill1",
                            `Period Name` == "3v3" ~ 'drill2',
                            `Period Name` == "warmup" ~ 'drill3',
                            `Period Name` == 'top' ~ 'drill4',
                            `Period Name` == 'touch' ~ 'drill5',
                            T ~ `Period Name`))


set.seed(100)
inx <- sample.split(seq_len(nrow(df2)), 0.7)
train <- df2[inx, ]
test <-  df2[!inx, ]

#####Linear Regression
model_lm <- lm(seconds ~ Odometer, data = train)
summary(model_lm)

prediction_lm <- predict(model, newdata = test, interval = "prediction")

compare_lm <- cbind(test, prediction_lm) %>%
  select(c(1:4, 6, 10:12))


View(compare_lm)

ggplot(compare_lm, aes(seconds, Odometer, colour = "test")) +
  geom_point() +
  geom_point(aes(y=fit, colour='prediction')) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  theme_minimal()
