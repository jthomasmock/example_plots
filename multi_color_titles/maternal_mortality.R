# Sourced from https://kochandrea.github.io/Visualization-of-Maternal-Mortality/#

WHO_data <- read.csv("data/WHO_MMR-data-1990-2015 (1)/countryresults_all.csv")
# New table of mmr point estimates:
mmr_data <- filter(WHO_data, 
                     indicator == "mmr", 
                     estimate == "point estimate",
                     rounded == "FALSE")
WorldBank_data <- read_excel("data/WB_country_region_income.xlsx")
WorldBank_data <- WorldBank_data[,-c(3,6,7)]
# Merge WHO and WB data:
merged_WHO_WB_data <- sqldf("SELECT * from mmr_data 
                  LEFT OUTER join WorldBank_data 
                  ON mmr_data.iso = WorldBank_data.Code")
# Create subsets by income, then rank countries by maternal mortality ratio (mmr):
upper_income_countries <- filter(merged_WHO_WB_data, `Income group` == "High income")
ranked_mmr_of_upper_income <- arrange(upper_income_countries, year, value) %>% 
  group_by(year) %>%              
  mutate(rank = order(value))
# Find changes in rank from 1985 to 2015 for upper income countries:
change_in_rank_upper <-  subset(ranked_mmr_of_upper_income, year %in% c(1985, 2015))
change_in_rank_upper <- change_in_rank_upper[,-c(4:11)]
change_in_rank_upper <- spread(change_in_rank_upper, year, rank)
change_in_rank_upper[3:4] <- lapply(change_in_rank_upper[3:4], as.numeric)
change_in_rank_upper <- mutate(change_in_rank_upper, rank_change = change_in_rank_upper$`1985` - change_in_rank_upper$`2015`)
    # NB: United States (USA) fell by 21 spots, Poland (POL) gained by 27 spots
# Show only every 5 years:
show_years <- c(2015, 2010, 2005, 2000, 1995, 1990, 1985)
ranked_mmr_of_upper_income$Year_formatted <- as.character(ranked_mmr_of_upper_income$year)
ranked_mmr_of_upper_income <- subset(ranked_mmr_of_upper_income, year %in% show_years)
# Note rows for USA (down) and POL (up), so that they can be highlighted later in visualization:
ranked_mmr_of_upper_income <- mutate(ranked_mmr_of_upper_income,
                                     highlight_country = case_when(iso == "USA" ~ 1,
                                                                   iso == "POL" ~ 2,
                                                                   TRUE ~ 0))


down_color <- "#7102FA"
up_color <- "#948E00" 

df <- ranked_mmr_of_upper_income

ggplot(data = df, aes(x = year, y = rank, group = iso)) +
  scale_y_reverse() +
  ## Other countries:
  geom_line(data = df %>% filter(!iso %in% c("USA", "POL")), color = "grey", size = 0.25) +
  geom_label(data = df %>% filter(!iso %in% c("USA", "POL")),
             aes(label = rank), 
             size = 4, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0, 
             color = "grey",
             fill = background_color,
             family = "PT Mono") +
  geom_text(data = df %>% filter(!iso %in% c("USA", "POL") & year == 1985),
            aes(label = iso) ,
            nudge_x = -0.8,
            vjust = 0.5,
            hjust = 1,
            size = 4, 
            color = "grey",
            family = "PT Mono") + 
  geom_text(data = df %>% filter(!iso %in% c("USA", "POL") & year == 2015),
            aes(label = iso) ,
            nudge_x = 0.8,
            vjust = 0.5,
            hjust = 0,
            size = 4, 
            color = "grey",
            family = "PT Mono") +
  ## USA:
  geom_line(data = df %>% filter(iso == "USA"), alpha = 1, color = down_color, size = 1) +
  geom_label(data = df %>% filter(iso == "USA"),
             aes(label = rank), 
             size = 4, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0, 
             color = down_color,
             fontface = "bold", 
             fill = background_color,
             family = "PT Mono") +  
  geom_text(data = df %>% filter(iso == "USA" & year == 1985),
            aes(label = iso),
            nudge_x = -0.8,
            vjust = 0.5,
            hjust = 1,
            fontface = "bold",
            size = 4, 
            color = down_color,
            family = "PT Mono") +
  geom_text(data = df %>% filter(iso == "USA" & year == 2015),
            aes(label = iso) ,
            nudge_x = 0.8,
            vjust = 0.5,
            hjust = 0,
            fontface = "bold",
            size = 4,
            color = down_color,
            family = "PT Mono") +
  ## POL:
  geom_line(data = df %>% filter(iso == "POL"), alpha = 1, color = up_color, size = 1) +
  geom_label(data = df %>% filter(iso == "POL"),
             aes(label = rank), 
             size = 4, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0, 
             color = up_color,
             fontface = "bold",
             fill = background_color,
             family = "PT Mono") +
  geom_text(data = df %>% filter(iso == "POL" & year == 1985),
            aes(label = iso) ,
            nudge_x = -0.8,
            vjust = 0.5,
            hjust = 1,
            fontface = "bold",
            size = 4, 
            color = up_color,
            family = "PT Mono") +
  geom_text(data = df %>% filter(iso == "POL" & year == 2015),
            aes(label = iso) ,
            nudge_x = 0.8,
            vjust = 0.5,
            hjust = 0,
            fontface = "bold",
            size = 4,
            color = up_color,
            family = "PT Mono") +

  scale_x_continuous(breaks = seq(1980, 2015, 5)) +
  labs(title = "",
       subtitle = "",
       caption = "*as categorized by the World Bank's 2019 fiscal year estimates \nSource(s): The World Health Organization / The World Bank") +
  xlab("Year") +
  ylab("Country ranking") +
  
  coord_cartesian(xlim = c(1985,2015), ylim = c(1,52), clip = "off") + 
  annotate("text", x = 1985, y = -6.5, hjust = 0, parse=T, label=expression(bold("United States") * phantom(bold(" falls in maternal health; ")) * phantom(bold("Poland")) * phantom(bold(" improves the most"))), color = down_color, size = 7, family = "Raleway") + 
  annotate("text", x = 1985, y = -6.5, hjust = 0, parse=T, label=expression(phantom(bold("United States")) * bold(" falls in maternal health; ") * phantom(bold("Poland")) * phantom(bold(" improves the most"))), color = "black", size = 7, family = "Raleway") + 
  annotate("text", x = 1985, y = -6.5, hjust = 0, parse=T, label=expression(phantom(bold("United States")) * phantom(bold(" falls in maternal health; ")) * bold("Poland") * phantom(bold(" improves the most"))), color = up_color, size = 7, family = "Raleway") + 
  annotate("text", x = 1985, y = -6.5, hjust = 0, parse=T, label=expression(phantom(bold("United States")) * phantom(bold(" falls in maternal health; ")) * phantom(bold("Poland")) * bold(" improves the most")), color = "black", size = 7, family = "Raleway") + 
  annotate("text", x = 1985, y = -4, hjust = 0, label = "Ranking of maternal mortality ratios of high-income* countries.  From 1985 to 2015, the United States\nhad the greatest drop in maternal mortality health rankings among high-income countries.  The U.S.\nfell by 21 spots, where as Poland increased by 27 spots.", size = 4, family = "PT Mono", lineheight = 0.8) +
  annotate("text", x=2012.5, y=47, label="HIGH\nmortality\nratio", hjust = 0.5, size = 4, color = down_color, fontface = "bold.italic", family = "PT Mono") +
  annotate("text", x=2012.5, y=6, label="LOW\nmortality\nratio", hjust = 0.5, size = 4, color = up_color, fontface = "bold.italic", family = "PT Mono") +
  
  special_theme +
  theme(
    axis.text.y = element_blank(),
    legend.position = "none", 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
