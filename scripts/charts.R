theme_ef <- function () { 
  theme_minimal(base_size=12) %+replace%
    theme(
      axis.title = element_blank(),
      title = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(family = "FiraGO", face="bold", size = 14),
      text = element_text(family= "FiraGO"),
      plot.title = element_text(size=14, face="bold", family="FiraGO", hjust = 0),
      plot.subtitle = element_text(size=12, family="FiraGO", hjust=0),
      axis.text = element_text(size=12, family="FiraGO", color = "black"),
      legend.position = "none"
    )
}

# vaccinated

read.csv("tables/frequency/vaccinated.csv", header = T, sep = "\t") %>%
  setNames(., c("var", "prop")) -> vaccinated

# vaccinated crosstabs

read.csv("tables/crosstabs/vaccinated_agegroup.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop")%>%
  mutate(group = "Age groups",
         cat = str_replace(cat, "X18.34$", "18-34"),
         cat = str_replace(cat, "X35.54$", "35-54"),
         cat = str_replace(cat, "X55.$", "55+"),)  -> vaccinated_agegroup

read.csv("tables/crosstabs/vaccinated_stratum.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop") %>%
  mutate(group = "Settlement type") -> vaccinated_stratum

read.csv("tables/crosstabs/vaccinated_hied.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop") %>%
  mutate(group = "Education",
         cat = str_replace(cat, "X0", "No higher education"),
         cat = str_replace(cat, "X1", "Higher education")) -> vaccinated_hied

read.csv("tables/crosstabs/vaccinated_ownership.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop") %>%
  filter(cat %in% c("X0", "X6", "X10")) %>%
  mutate(group = "Assets ownership index",
         cat = str_replace(cat, "X0", "Lowest"),
         cat = str_replace(cat, "X6", "Median"),
         cat = str_replace(cat, "X10", "Highest"),) -> vaccinated_ownership

bind_rows(vaccinated_agegroup, vaccinated_stratum, vaccinated_hied, vaccinated_ownership) %>%
  mutate(labels = factor(labels, levels = c("Vaccinated", "Not vaccinated")),
         labels = fct_rev(labels))%>%
  filter(cat != "Total")%>%
  mutate(cat = factor(cat, levels = c("18-34", "35-54", "55+", "Capital", "Urban", "Rural",
                                      "No higher education", "Higher education", "Lowest", "Median", "Highest")),
         cat = fct_rev(cat),
         group = factor(group, levels = c("Age groups", "Settlement type", "Education", "Assets ownership index"))) -> vaccinated_demography

vaccinated_demography %>%
  ggplot(aes(cat, prop, group=group, fill=labels, label=ifelse(prop <= 0.5, "", round(prop, 0))))+
  geom_col(position = "stack")+
  scale_fill_manual(values=c("#cb997e", "#006d77"),
                    guide = guide_legend(reverse = T))+
  facet_wrap(~group, scales = "free", ncol=1)+
  coord_flip()+
  geom_text(position = position_stack(vjust = 0.5), family="FiraGO", size=4, fontface = "bold")+
  theme_ef()+
  theme(legend.position = "bottom",
        strip.text = element_text(angle=0, hjust=0.06))

ggsave("visuals/crosstabs/vaccinated_demography.png", width=12, height=5)

# lack of information

read.csv("tables/crosstabs/not_enough_information_agegroup.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop")%>%
  mutate(group = "Age groups",
         cat = str_replace(cat, "X18.34$", "18-34"),
         cat = str_replace(cat, "X35.54$", "35-54"),
         cat = str_replace(cat, "X55.$", "55+"),)  -> not_enough_information_agegroup

read.csv("tables/crosstabs/not_enough_information_stratum.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop") %>%
  mutate(group = "Settlement type") -> not_enough_information_stratum

read.csv("tables/crosstabs/not_enough_information_hied.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop") %>%
  mutate(group = "Education",
         cat = str_replace(cat, "X0", "No higher education"),
         cat = str_replace(cat, "X1", "Higher education")) -> not_enough_information_hied

read.csv("tables/crosstabs/not_enough_information_ownership.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop") %>%
  filter(cat %in% c("X0", "X6", "X10")) %>%
  mutate(group = "Assets ownership index",
         cat = str_replace(cat, "X0", "Lowest"),
         cat = str_replace(cat, "X6", "Median"),
         cat = str_replace(cat, "X10", "Highest"),) -> not_enough_information_ownership

bind_rows(not_enough_information_agegroup, not_enough_information_stratum, not_enough_information_hied, not_enough_information_ownership) %>%
  filter(cat != "Total" & labels == 1)%>%
  mutate(cat = factor(cat, levels = c("18-34", "35-54", "55+", "Capital", "Urban", "Rural",
                                      "No higher education", "Higher education", "Lowest", "Median", "Highest")),
         cat = fct_rev(cat),
         group = factor(group, levels = c("Age groups", "Settlement type", "Education", "Assets ownership index"))) -> not_enough_information_demography

not_enough_information_demography %>%
  ggplot(aes(cat, prop, group=group, fill=labels, label=ifelse(prop <= 0.5, "", round(prop, 0))))+
  geom_col(position = "stack")+

  facet_wrap(~group, scales = "free", ncol=1)+
  coord_flip()+
  geom_text(position = position_stack(vjust = 0.5), family="FiraGO", size=4, fontface = "bold")+
  theme_ef()+
  theme(legend.position = "none",
        strip.text = element_text(angle=0, hjust=0.06))

ggsave("visuals/crosstabs/not_enough_information_demography.png", width=12, height=5)


## knowledge of online booking system

# lack of information
read.csv("tables/crosstabs/know_booking_not_enough_information.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop")%>%
  mutate(group = "Enough information on the immunization process?",
         cat = str_replace(cat, "X0", "Yes, enough information"),
         cat = str_replace(cat, "X1", "No, not enough information"))  -> know_booking_not_enough_information

read.csv("tables/crosstabs/know_booking_agegroup.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop")%>%
  mutate(group = "Age groups",
         cat = str_replace(cat, "X18.34$", "18-34"),
         cat = str_replace(cat, "X35.54$", "35-54"),
         cat = str_replace(cat, "X55.$", "55+"),)  -> know_booking_agegroup

read.csv("tables/crosstabs/know_booking_stratum.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop") %>%
  mutate(group = "Settlement type") -> know_booking_stratum

read.csv("tables/crosstabs/know_booking_hied.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop") %>%
  mutate(group = "Education",
         cat = str_replace(cat, "X0", "No higher education"),
         cat = str_replace(cat, "X1", "Higher education")) -> know_booking_hied

read.csv("tables/crosstabs/know_booking_ownership.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop") %>%
  filter(cat %in% c("X0", "X6", "X10")) %>%
  mutate(group = "Assets ownership index",
         cat = str_replace(cat, "X0", "Lowest"),
         cat = str_replace(cat, "X6", "Median"),
         cat = str_replace(cat, "X10", "Highest"),) -> know_booking_ownership

read.csv("tables/crosstabs/know_booking_ethnic.csv", header = T, sep = "\t")%>%
  rename(labels=1)%>%
  pivot_longer(-labels, names_to = "cat", values_to = "prop") %>%
  mutate(group = "Ethnic identity",
         cat = str_replace(cat, "X0", "Ethnic minority"),
         cat = str_replace(cat, "X1", "Ethnic Georgian")) -> know_booking_ethnic


bind_rows(know_booking_not_enough_information, know_booking_agegroup, know_booking_stratum, know_booking_hied, know_booking_ownership, know_booking_ethnic) %>%  filter(cat != "Total" & labels == 1)%>%
  mutate(cat = factor(cat, levels = c("Yes, enough information", "No, not enough information", "18-34", "35-54", "55+", "Capital", "Urban", "Rural",
                                      "No higher education", "Higher education", "Lowest", "Median", "Highest",
                                      "Ethnic minority", "Ethnic Georgian")),
         cat = fct_rev(cat),
         group = factor(group, levels = c("Enough information on the immunization process?", "Age groups",
                                          "Settlement type", "Education", "Assets ownership index", "Ethnic identity"))) -> know_booking_demography

know_booking_demography %>%
  ggplot(aes(cat, prop, group=group, fill=labels, label=ifelse(prop <= 0.5, "", round(prop, 0))))+
  geom_col(position = "stack")+
  ylim(0, 100)+
  facet_grid(group ~ ., scales = "free", space = "free")+
  coord_flip()+
  geom_text(position = position_stack(vjust = 0.5), family="FiraGO", size=4, fontface = "bold")+
  labs(
    title = "If you needed to register on the online vaccine booking platform, do you know how to do so?",
    subtitle = "Share of those who said Yes"
  )+
  theme_ef()+
  theme(legend.position = "none",
        strip.text.y = element_text(angle = 0, size = 10),
        panel.border = element_rect(fill = NA, size = 0.5),
        strip.background.y = element_rect(fill="lightgrey"))

ggsave("visuals/crosstabs/know_booking_demography.png", width=12, height=5)



