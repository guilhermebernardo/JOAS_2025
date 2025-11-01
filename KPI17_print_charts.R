rm(list=ls())

library(dygraphs)
library(xts)
library(tidyverse)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggbump)
library(scales)
library(leaflet)
library(forcats)

your_directory <- "C:/Users/guilherme.bernardo/Desktop/JOAS"

setwd(your_directory)

load(paste0(your_directory, "/KPI17/kpi17.Rda"))

kpis <- kpi17

kpis <- kpis %>%
  distinct() %>%
  mutate( ym = format(time, "%Y%m"),
          month = format(time, "%m"),
          y = year(time))

#- linhas de media geral ------------------------------------------------------------------------------------------------------------

timeColor <- "black"
distColor <- "blue"

linechart <- kpis %>%
  group_by(ym,plan_dep) %>%
  summarise(KPI17_time = mean(kpi17_time,na.rm = T),KPI17_distance = mean(kpi17_distance,na.rm = T), 
            y = y[1], month = month(time[1]), time = time[1], speed = mean(mean_speed, na.rm = T)) %>%
  mutate(ym = ym(ym, tz = "UTC"))%>%
  ggplot(aes(x = as.Date(ym))) +
  geom_line(aes(y = KPI17_time), color = timeColor) +
  geom_line(aes(y = KPI17_distance/4), color = distColor) +
  scale_y_continuous("Vertical efficiency during climb (min)", sec.axis = sec_axis(~.*4, name = "Vertical efficiency during climb (NM)")) +
  theme(axis.title.y = element_text(color = timeColor, size=32),
        axis.title.y.right = element_text(color = distColor, size=32))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y",expand=c(0,0), "Month" )+
  facet_wrap(~plan_dep,nrow = 2) + guides(color="none")+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(legend.text = element_text(size = 12))+
  theme(panel.spacing.y = unit(5, "mm"),panel.spacing.x = unit(15, "mm"))+
  theme(axis.title  = element_text(size =32), strip.text = element_text(size =32), 
        title = element_text(size = 32))+
  theme(axis.text.x = element_text(size = 22, angle = 45, hjust = 1))+  
  theme(axis.text.y = element_text(size = 22)) +
  labs(x = "Year")

ggsave(linechart, filename = paste0(your_directory, "/media_mensal_KPI17.png"), width = 1720*3, height = 1080*3, units = "px")


#- linhas de media altitude em voo nivelado ------------------------------------------------------------------------------------------------------------

linechart <- kpis %>%
  group_by(ym,plan_dep) %>%
  summarise(KPI17_alt_leveloffs = mean(mean_alt_leveloffs,na.rm = T), 
            y = y[1], month = month(time[1]), time = time[1]) %>%
  mutate(ym = ym(ym, tz = "UTC"))%>%
  ggplot(aes(x = as.Date(ym))) +
  geom_line(aes(y = KPI17_alt_leveloffs), color = "black") +
  scale_y_continuous("Altitude (ft)") +
  theme(axis.title.y = element_text(color = "black", size= 32),
        axis.title.y.right = element_text(color = distColor, size=32))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y",expand=c(0,0), "Month" )+
  facet_wrap(~plan_dep,nrow = 2) + guides(color="none")+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(legend.text = element_text(size = 12))+
  theme(panel.spacing.y = unit(5, "mm"),panel.spacing.x = unit(15, "mm"))+
  theme(axis.title  = element_text(size =32), strip.text = element_text(size =32), 
        title = element_text(size = 32))+
  theme(axis.text.x = element_text(size = 22, angle = 45, hjust = 1.1))+  
  theme(axis.text.y = element_text(size = 22)) +
  labs(x = "Year")

ggsave(linechart, filename = paste0(your_directory, "/media_alt_leveloff_KPI17.png"), width = 1720*3, height = 1080*3, units = "px")

# media mensal por aeroporto
for (i in unique(kpis$plan_dep)){
  linechart <- kpis %>%
    subset(plan_dep  == i)%>%
    group_by(ym) %>%
    summarise(KPI17_time = mean(kpi17_time,na.rm = T),KPI17_distance = mean(kpi17_distance,na.rm = T), 
              y = y[1], month = month(time[1]), time = time[1], speed = mean(mean_speed, na.rm = T)) %>%
    mutate(ym = ym(ym, tz = "UTC"))%>%
    ggplot(aes(x = as.Date(ym))) +
    geom_line(aes(y = KPI17_time), color = timeColor) +
    geom_line(aes(y = KPI17_distance/4), color = distColor) +
    scale_y_continuous("KPI (min)", sec.axis = sec_axis(~.*4, name = "KPI (NM)")) +
    theme(axis.title.y = element_text(color = timeColor, size=24),
          axis.title.y.right = element_text(color = distColor, size=24))+
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y",expand=c(0,0), "Mês" )+ 
    theme(legend.key.size = unit(2, 'cm'))+
    theme(legend.text = element_text(size = 12))+
    theme(panel.spacing.y = unit(5, "mm"),panel.spacing.x = unit(5, "mm"))+
    theme(axis.title  = element_text(size =32), strip.text = element_text(size =26), 
          title = element_text(size = 24))+
    theme(axis.text.x = element_text(size = 18, angle = 45, hjust = 1.1))+  
    theme(axis.text.y = element_text(size = 18)) 
  linechart
  
  ggsave(linechart, filename = paste0(paste0(your_directory, "/media_mensal_KPI17"),"_",i,".png"),width = 1720*3, height = 1080*2, units = "px")
  
}

#-ranking time -----------------------------------------------------------------------------------------------------

# Create the "All" summary
rankingall <- kpis2 %>%
  group_by(plan_dep) %>%
  summarise(KPI_time = round(mean(kpi17_time, na.rm = TRUE), digits = 1)) %>%
  mutate(ym = "All")

# First create the ranking data frame with proper date formatting
ranking <- kpis2 %>%
  group_by(plan_dep, ym) %>%
  mutate(ym = as.character(ym)) %>%
  summarise(KPI_time = round(mean(kpi17_time, na.rm = TRUE), digits = 1)) %>%
  rbind(rankingall) %>%
  mutate(ym = case_when(
    ym != "All" ~ format(as.Date(paste0(substr(ym, 1, 4), "-", substr(ym, 5, 6), "-01")), "%m-%Y"),
    TRUE ~ "All"
  ))

# For "All" category, get airports ordered by KPI_time in descending order
all_ordered <- ranking %>% 
  filter(ym == "All") %>%
  arrange(KPI_time) %>%
  pull(plan_dep)

# Order the ranking data
ranking_ordered <- ranking %>%
  mutate(
    plan_dep = factor(plan_dep, levels = all_ordered),
    ym = factor(ym, levels = c("11-2024", "12-2024", "01-2025", "02-2025", "03-2025", "04-2025", "All"))
  )

# Plotting
ranking_plot <- ranking_ordered %>%
  ggplot(aes(x = plan_dep, y = KPI_time)) +
  geom_bar(stat = 'identity', position = "dodge", fill = "lightblue") +
  ylab("Vertical efficiency during climb (min)") +
  theme(axis.title = element_text(size = 32),
        axis.text.x = element_text(size = 32, angle = 0),
        axis.text.y = element_text(size = 28),
        strip.text = element_text(size = 32)) +
  geom_text(aes(label = KPI_time), 
            position = position_dodge(width = 0.9),
            size = 9, vjust = 0.5, hjust = 0.75) +
  facet_wrap(~ym, nrow = 1) +
  coord_flip() +
  xlab("Airport") +
  scale_y_continuous(labels = number_format(accuracy = 1))

# Save figure
ggsave(ranking_plot, filename = paste0(your_directory, "/ranking_media_KPI_time_17.png"),
       width = 1920*3, height = 1080*3, units = "px")

#-ranking distance -----------------------------------------------------------------------------------------------------

rankingall <- kpis %>%
  group_by(plan_dep)%>%
  summarise(KPI_distance = round(mean(kpi17_distance,na.rm = T),digits=1))%>%
  mutate(ym = "All")

ranking <- kpis %>%
  group_by(plan_dep,ym)%>%
  mutate(ym = as.character(ym))%>%
  summarise(KPI_distance = round(mean(kpi17_distance,na.rm = T),digits=1))%>%
  rbind(rankingall)%>%
  ggplot(aes(x= fct_reorder(plan_dep, KPI_distance), y=KPI_time))+
  geom_bar(stat='identity',position="dodge", fill = "lightblue")+
  ylab(" KPI 17 (min)") +
  theme(axis.title  = element_text(size =32),
        axis.text.x = element_text(size = 24,angle = 0),
        axis.text.y = element_text(size = 29), 
        strip.text = element_text(size =32))+
  geom_text(aes(label=KPI_distance), 
            position=position_dodge(width=0.9),
            size = 8, vjust=0.5, hjust=0.75) +
  facet_wrap(~y,nrow = 1) + coord_flip() +xlab("Airport") + scale_y_continuous(labels = number_format(accuracy = 1))

###### New way to plot by month

# Create the "All" summary
rankingall <- kpis2 %>%
  group_by(plan_dep) %>%
  summarise(KPI_distance = round(mean(kpi17_distance, na.rm = T), digits = 1)) %>%
  mutate(ym = "All")

# First create the ranking data frame with proper date formatting
ranking <- kpis2 %>%
  group_by(plan_dep, ym) %>%
  mutate(ym = as.character(ym)) %>%
  summarise(KPI_distance = round(mean(kpi17_distance, na.rm = T), digits = 1)) %>%
  rbind(rankingall) %>%
  mutate(ym = case_when(
    ym != "All" ~ format(as.Date(paste0(substr(ym, 1, 4), "-", substr(ym, 5, 6), "-01")), "%m-%Y"),  # Convert from YYYYMM to MM-YYYY
    TRUE ~ "All"
  ))

# For "All" category, get airports ordered by KPI_distance in descending order
all_ordered <- ranking %>% 
  filter(ym == "All") %>%
  arrange((KPI_distance)) %>%
  pull(plan_dep)

# Order the ranking data
ranking_ordered <- ranking %>%
  mutate(
    # Order plan_dep by KPI_distance in descending order for the "All" category
    plan_dep = factor(plan_dep, levels = all_ordered),
    # Keep months in chronological order
    ym = factor(ym, levels = c("11-2024", "12-2024", "01-2025", "02-2025", "03-2025", "04-2025", "All"))
  )

# Plotting
ranking <- ranking_ordered %>%
  ggplot(aes(x = plan_dep, y = KPI_distance)) +  # No need for fct_reorder since we already ordered
  geom_bar(stat = 'identity', position = "dodge", fill = "lightblue") +
  ylab("Vertical efficiency during climb (NM)") +
  theme(axis.title = element_text(size = 32),
        axis.text.x = element_text(size = 32, angle = 0),
        axis.text.y = element_text(size = 28),
        strip.text = element_text(size = 32)) +
  geom_text(aes(label = KPI_distance), 
            position = position_dodge(width = 0.9),
            size = 9, vjust = 0.5, hjust = 0.75) +
  facet_wrap(~ym, nrow = 1) +
  coord_flip() +
  xlab("Airport") +
  scale_y_continuous(labels = number_format(accuracy = 1))

ggsave(ranking, filename = paste0(your_directory, "/ranking_media_KPI_distance_17.png") ,width = 1920*3, height = 1080*3, units = "px")

#----------------- boxplot agregado -----------------------------------------------------

boxplotx <- kpis %>%
  filter(kpi17_time < quantile(kpi17_time, 0.98, na.rm = TRUE))%>%
  ggplot(aes(x= reorder(plan_dep, kpi17_time), y=kpi17_time))+
  geom_boxplot(color = 'black', fill = 'skyblue', outlier.size = 0.1,outlier.alpha = 0.02, outlier.color='black') +
  coord_flip() + 
  theme(axis.title  = element_text(size =32),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 28), 
        strip.text = element_text(size =32))+
  theme(legend.text = element_text(size = 32),
        legend.title = element_text(size = 32))+  #ggtitle("KPI 05 - dispersão sem outliers - 98%") +
  xlab("Airport") +
  ylab("Vertical efficiency during climb (min)")
ggsave(boxplotx, filename = paste0(your_directory, "/boxplot_time_agregado_17.png"),width = 1920*3, height = 1080*3, units = "px")

boxplotx <- kpis %>%
  filter(kpi17_distance < quantile(kpi17_distance, 0.98, na.rm = TRUE))%>%
  ggplot(aes(x= reorder(plan_dep, kpi17_distance), y=kpi17_distance))+
  geom_boxplot(color = 'black', fill = 'skyblue', outlier.size = 0.1,outlier.alpha = 0.02, outlier.color='black') +
  coord_flip() + 
  theme(axis.title  = element_text(size =32),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 28), 
        strip.text = element_text(size =32))+
  theme(legend.text = element_text(size = 32),
        legend.title = element_text(size = 32))+  #ggtitle("KPI 05 - dispersão sem outliers - 98%") +
  xlab("Aeroporto") +
  ylab("KPI 17 (NM)")
ggsave(boxplotx, filename = paste0(your_directory, "/boxplot_distance_agregado_17.png"),width = 1920*3, height = 1080*3, units = "px")


#------------------------ box plot por aeroporto ----------------------------------------

boxplotx <- kpis %>%
  # Convert ym to date format
  mutate(ym_date = ym(ym),
         ym_label = format(ym_date, "%m-%Y")) %>%
  filter(kpi17_time < quantile(kpi17_time, 0.98, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(ym_label), y = kpi17_time)) +
  geom_boxplot(color = 'black', fill = 'skyblue', outlier.size = 0.1, outlier.alpha = 0.02, outlier.color = 'black') +
  facet_wrap(~plan_dep, nrow = 2) +
  xlab("Month") + 
  ylab("Vertical efficiency during climb (min)") +
  theme(
    axis.title = element_text(size = 32),
    axis.text.x = element_text(size = 32, angle = 90),
    axis.text.y = element_text(size = 32),
    strip.text = element_text(size = 32),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 32)
  )
ggsave(boxplotx, filename = paste0(your_directory, "/boxplot_time_17.png"),width = 1920*3, height = 1080*3, units = "px")

boxplotx <- kpis %>%
  filter(kpi17_distance < quantile(kpi17_distance, 0.98, na.rm = TRUE))%>%
  ggplot(aes(x = factor(y), y = kpi17_distance)) +
  geom_boxplot(color = 'black', fill = 'skyblue', outlier.size = 0.1,outlier.alpha = 0.02, outlier.color='black') +
  facet_wrap(~plan_dep, nrow=2) +xlab("Aeroporto") + ylab("KPI 17") +
  theme(axis.title  = element_text(size =32),
        axis.text.x = element_text(size = 32,angle = -90),
        axis.text.y = element_text(size = 28), 
        strip.text = element_text(size =32))+
  theme(legend.text = element_text(size = 32),
        legend.title = element_text(size = 32))+
  xlab("Ano") +
  ylab("KPI 17 (NM)")
boxplotx
ggsave(boxplotx, filename = paste0(your_directory, "/boxplot_distance_17.png"),width = 1920*3, height = 1080*3, units = "px")

#----------------- boxplot por mes -----------------------

boxplotx <- kpis %>%
  filter(kpi17_time < quantile(kpi17_time, 0.98, na.rm = TRUE))%>%
  mutate(ym = sub( '(?<=.{4})', '-', ym, perl=TRUE ))%>%
  ggplot(aes(x = factor(ym), y=kpi17_time)) +
  geom_boxplot(color = 'black', fill = 'skyblue', outlier.size = 0.1,outlier.alpha = 0.02, outlier.color='black') +
  theme(axis.title  = element_text(size =32),
        axis.text.x = element_text(size = 24,angle = -90),
        axis.text.y = element_text(size = 28), 
        strip.text = element_text(size =32))+
  theme(legend.text = element_text(size = 32),
        legend.title = element_text(size = 32))+ 
  xlab("Ano-Mês") +
  ylab("KPI 17 (min)")
ggsave(boxplotx, filename = paste0(your_directory, "/boxplot_time_mes_17.png") ,width = 1920*3, height = 1080*3, units = "px")

boxplotx <- kpis %>%
  filter(kpi17_distance < quantile(kpi17_distance, 0.98, na.rm = TRUE))%>%
  mutate(ym = sub( '(?<=.{4})', '-', ym, perl=TRUE ))%>%
  ggplot(aes(x = factor(ym), y=kpi17_distance)) +
  geom_boxplot(color = 'black', fill = 'skyblue', outlier.size = 0.1,outlier.alpha = 0.02, outlier.color='black') +
  theme(axis.title  = element_text(size =32),
        axis.text.x = element_text(size = 24,angle = -90),
        axis.text.y = element_text(size = 28), 
        strip.text = element_text(size =32))+
  theme(legend.text = element_text(size = 32),
        legend.title = element_text(size = 32))+ 
  xlab("Ano-Mês") +
  ylab("KPI 17 (NM)")
ggsave(boxplotx, filename = paste0(your_directory, "/boxplot_distance_mes_17.png"),width = 1920*3, height = 1080*3, units = "px")

