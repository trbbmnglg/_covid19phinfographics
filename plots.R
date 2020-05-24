#Monthly Trend
covidTrend <- coviddata %>%
  select(DateRepConf) %>%
  count(DateRepConf)
covidTrend <- covidTrend %>%
  ggplot(aes(DateRepConf, n )) +
  geom_line(color="#316caf", size=2, linetype=1) +
  scale_x_date(expand = c(0, 0)) +
  theme_ipsum(
    grid_col = "#316caf",
    axis_text_size = 50,
    axis_title_size = 18,
    axis_title_family= "Oswald",
    plot_title_family = "Oswald"
  ) +
  theme(
    axis.text.y = element_text(colour = "#316caf"),
    axis.text.x = element_text(colour = "#316caf"),
    axis.title.x = element_text(margin = margin(r = 50), colour = "#316caf"),
    axis.title.y = element_text(colour = "#316caf"),
    plot.margin = margin(0,0,0,0),
    plot.title = element_text(family="Oswald", face="plain",colour="#316caf", size="130",hjust = 0.5),
    text = element_text(family = "Oswald", face="plain"),
    panel.grid.minor.y = element_line(size = .3),
    panel.grid.major.y = element_line(size = .3, colour = "#316caf")) +
  labs(x="", y="", title="cases per day")
ggsave("www/plots/covidtrend.png", width = 14, height = 9, units = "in")

#Generate Pie chart for count per Region
nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

regCount <- coviddata %>%
  select(RegionRes) %>%
  group_by(RegionRes) %>%
  summarise(CaseCount=n()) %>%
  top_n(5) %>%
  rename("Region" = "RegionRes") %>%
  arrange(desc(CaseCount))
regCount$Region[regCount$Region==""]<-"Uncategorized"
showRegCount <- ggplot(regCount, aes(x="", y=CaseCount, fill=Region)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual(values = mycolors) +
  theme(plot.margin = margin(0,0,0,0),
        plot.title = element_text(family="Oswald", colour="#66c2a5", size="40",hjust = 0.8)
  ) +
  ggtitle("top 5 region")
ggsave("www/plots/region.png", width = 17, height = 13, dpi = 72, units = "cm", device="png")

#Count of new deaths and recovery for the latest availabe dataset
died <-coviddata %>%
  select(DateRepRem, RemovalType) %>%
  group_by(DateRepRem) %>%
  filter(RemovalType=="Died") %>%
  summarise(NewCounts = n()) %>%
  rename(DeathCount = NewCounts)
died
recovered <-coviddata %>%
  select(DateRepRem, RemovalType) %>%
  group_by(DateRepRem) %>%
  filter(RemovalType=="Recovered") %>%
  summarise(NewCounts = n()) %>%
  rename(RecoveredCount = NewCounts)
recovered

ggplot() + 
  geom_line(data = died, aes(x = DateRepRem, y = DeathCount,colour="Died" ),size = 2, linetype = 1) +
  geom_line(data = recovered, aes(x = DateRepRem, y = RecoveredCount, colour="Recovered"),size = 2, linetype = 1) +
  scale_x_date(expand = c(0, 0)) +
  scale_color_manual("",
                     labels = c("died", "recovered"),
                     values = c("#d63031", "#00b894")) +
  theme_ipsum(
    grid_col = "#837a91",
    axis_text_size = 50,
    axis_title_size = 18,
    axis_title_family= "Oswald",
    plot_title_family = "Oswald"
  ) +
  theme(
    plot.margin = margin(0,0,0,0),
    plot.title = element_text(family="Oswald", face="plain",colour="#837a91", size="130",hjust = 0.5),
    panel.grid.minor.y = element_line(size = .3),
    panel.grid.major.y = element_line(size = .3, colour = "#837a91"),
    legend.text = element_text(colour="#837a91", family="Oswald", size=30),
    text = element_text(family = "Oswald", face="plain")) +
  labs(x="", y="", title="died vs recovered")
ggsave("www/plots/recoveredvsdeath.png", width = 14, height = 9, units = "in")