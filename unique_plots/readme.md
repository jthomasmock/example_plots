# Unique plots

# source = https://github.com/dfsnow/ama_viz/blob/master/exploratory_plots.Rmd
# https://dfsnow.github.io/ama_viz/exploratory_plots.html

# Create the facet wrap plot
nurses_rank %>%
  mutate(id_color = recode(  # Also remove this line for additive facets
    id_color, "0" = "2", "1" = "1", "2" = "2")
  ) %>%
ggplot() +
  geom_text(
    aes(x = rank, y = type, label = state, color = id_color), size = 2.5
  ) +
  geom_line(
    data = nurses_rank_select %>% filter(type %in% c("doc_count", "rn_total")),
    aes(x = rank, y = 2.5 + group_idx, group = state, color = factor(id_color)),
    size = 1.0
  ) +
  geom_segment(
    data = nurses_rank_select %>% filter(type == "doc_count"),
    aes(
      x = rank, xend = rank,
      y = 2.85, yend = 2.5 + group_idx,
      color = factor(id_color)),
    size = 1.0
  ) +
  geom_segment(
    data = nurses_rank_select %>% filter(type == "rn_total"),
    aes(
      x = rank, xend = rank,
      y = 2.5 + group_idx, yend = 2.15,
      color = factor(id_color)),
    size = 1.0
  ) +
  geom_line(
    data = nurses_rank_select %>% filter(type %in% c("rn_total", "np_total")),
    aes(x = rank, y = 1.5 + group_idx, group = state, color = factor(id_color)),
    size = 1.0
  ) +
  geom_segment(
    data = nurses_rank_select %>% filter(type == "rn_total"),
    aes(
      x = rank, xend = rank,
      y = 1.85, yend = 1.5 + group_idx,
      color = factor(id_color)),
    size = 1.0
  ) + 
  geom_segment(
    data = nurses_rank_select %>% filter(type == "np_total"),
    aes(
      x = rank, xend = rank,
      y = 1.5 + group_idx, yend = 1.15,
      color = factor(id_color)),
    size = 1.0
  ) +
  geom_text(
    data = tibble(
      id = as.factor(c("Mississippi", "Washington", "Iowa", "Oregon")),
      label = c("Doctors\n\nNurse\nPracitioners\n\nRegistered\nNurses",
                rep("Docs\n\n\nNPs\n\n\nRNs", 3))),
    aes(x = 0.3, y = 2.03, label = label), lineheight = 0.885,
    size = 3.8, hjust = 1, vjust = 0.5, color = "grey35"
  ) +
  geom_text(
    data = tibble(
      id = as.factor(c("Mississippi", "Washington", "Iowa", "Oregon")),
      label = c("Least", "", "", "")),
    aes(x = 6, y = 4.28, label = label),
    size = 5, hjust = 0, color = "grey35"
  ) +
  geom_segment(
    data = tibble(
      id = as.factor(c("Mississippi", "Washington", "Iowa", "Oregon")),
      color = c("arrow_col", "null_col", "null_col", "null_col")),
    aes(x = 5.5, xend = 1, y = 4.28, yend = 4.28, color = color),
    arrow = arrow(length = unit(0.1, "inches"), type = "closed")
  ) +
  geom_text(
    data = tibble(
      id = as.factor(c("Mississippi", "Washington", "Iowa", "Oregon")),
      label = c("Most", "", "", "")),
    aes(x = 44, y = 4.28, label = label),
    size = 5, hjust = 1, color = "grey35"
  ) +
  geom_segment(
    data = tibble(
      id = as.factor(c("Mississippi", "Washington", "Iowa", "Oregon")),
      color = c("arrow_col", "null_col", "null_col", "null_col")),
    aes(x = 44.5, xend = 49, y = 4.28, yend = 4.28, color = color),
    arrow = arrow(length = unit(0.1, "inches"), type = "closed")
  ) +
  scale_x_continuous(
    position = "top",
    expand = c(0.01, 0.0)
  ) +
  scale_y_discrete(
    expand = c(0.1, 0.1),
    labels = c("NPs", "RNs", "MDs")
  ) +
  scale_color_manual(
    values = c(
      "2" = "grey80",
      "1" = ama_colors[length(ama_colors)],
      "0" = "#fce8de",
      "arrow_col" = "grey35",
      "null_col" = "transparent")
  ) +
  scale_fill_manual(values = c("transparent", "white")) +
  facet_wrap(vars(id), ncol = 1) +
  coord_cartesian(xlim = c(1, 49), ylim = c(1, 3), clip = "off") +
  labs(
    title = "Nurses Serve as Substitutes in States with Few Doctors",
    subtitle = paste(
      "States with the lowest number of doctors per 100K people",
      "have the highest number of nurses, and visa versa"),
    y = "Type of Practitioner",
    x = "States Ranked by # of Practitioners (Per 100K Pop.)",
    caption = paste0(
      "Sources: American Medical Association Master File",
      "\nKaiser Family Foundation")
  ) +
  theme_ama() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "transparent"),
    panel.grid = element_blank(),
    panel.spacing.y = unit(0, "pt"),
    strip.background = element_rect(color = "transparent"),
    strip.text = element_text(size = 14, color = "grey55", face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 20)),
    plot.caption = element_text(lineheight = 1.1),
    axis.title.x = element_text(margin = margin(b = 10)),
    axis.title.y = element_text(margin = margin(r = 40)),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  ggsave("plots/p9_nurses.png", width = 9, height = 8)
