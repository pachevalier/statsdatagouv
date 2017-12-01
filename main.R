library("readr")
library("dplyr")
library("ggplot2")
library("tricky")
library("lubridate")
library("ggtech")
library("scales")
library("ggthemes")
library("wesanderson")
library("forcats")
table_piwik <-read_csv("piwik_utf8.csv") %>%
  set_standard_names() %>%
  mutate(
    date = ymd(paste0(date, "-01"))
    ) %>%
  filter(date >= ymd("2016-01-01")) %>%
  mutate(
    month = factor(months(date), 
                   levels = c("janvier", "février", "mars", 
                              "avril", "mai", "juin", 
                              "juillet", "août", "septembre", 
                              "octobre", "novembre", "décembre")
                   ), 
    year = as.factor(year(date))
  ) 

table_piwik %>% 
  glimpse()

png(filename = "piwik.png", width = 320, height = 700)
table_piwik %>% 
  ggplot() + 
  geom_col(
    mapping = aes(x = year, y = unique_visitors, fill = year)
    ) + 
  scale_fill_manual(
    name = "Année", 
    values = wes_palette("Royal1")) +
  scale_y_continuous(labels = french_formatting) + 
  facet_wrap(facets = ~ month, ncol = 2) +
  labs(
    title = "Fréquentation de data.gouv", 
    subtitle = "Nombre de visiteurs uniques par mois") +
  theme_fivethirtyeight()
dev.off()

library("tidyr")
table_piwik %>%
  select(year, month, unique_visitors)%>%
  tidyr::spread(key = year, value = unique_visitors) %>%
  mutate(growth = 100 * `2017` / `2016` - 100) %>%
  select(month, growth)








  group_by(year, month) %>%
  su
  