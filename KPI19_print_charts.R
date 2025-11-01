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

your_directory <- "C:/Users/guilherme.bernardo/Desktop/JOAS"

setwd(your_directory)

load(paste0(your_directory, "/KPI19/kpi19.Rda"))

kpis <- kpi19

kpis <- kpis %>%
  distinct() %>%
  mutate( ym = format(time, "%Y%m"),
          month = format(time, "%m"),
          y = year(time))

#- linhas de media geral ------------------------------------------------------------------------------------------------------------

timeColor <- "black"
distColor <- "blue"

linechart <- kpis %>%
  group_by(ym,real_arr) %>%
  summarise(kpi19_time = mean(kpi19_time,na.rm = T),kpi19_distance = mean(kpi19_distance,na.rm = T), 
            y = y[1], month = month(time[1]), time = time[1], speed = mean(mean_speed, na.rm = T)) %>%
  mutate(ym = ym(ym, tz = "UTC"))%>%
  ggplot(aes(x = as.Date(ym))) +
  geom_line(aes(y = kpi19_time), color = timeColor) +
  geom_line(aes(y = kpi19_distance/4), color = distColor) +
  scale_y_continuous("Vertical efficiency during descent (min)", sec.axis = sec_axis(~.*4, name = "Vertical efficiency during descent (NM)")) +
  theme(axis.title.y = element_text(color = timeColor, size=25),
        axis.title.y.right = element_text(color = distColor, size=25))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y",expand=c(0,0), "Month" )+
  facet_wrap(~real_arr,nrow = 2) + guides(color="none")+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(legend.text = element_text(size = 12))+
  theme(panel.spacing.y = unit(5, "mm"),panel.spacing.x = unit(15, "mm"))+
  theme(axis.title  = element_text(size =32), strip.text = element_text(size =32), 
        title = element_text(size = 32))+
  theme(axis.text.x = element_text(size = 22, angle = 45, hjust = 1))+  
  theme(axis.text.y = element_text(size = 22)) +
  labs(x = "Year")

ggsave(linechart, filename = paste0(your_directory, "/media_mensal_kpi19.png"),width = 1720*3, height = 1080*3, units = "px")


#- linhas de media altitude em voo nivelado ------------------------------------------------------------------------------------------------------------

linechart <- kpis %>%
  group_by(ym,real_arr) %>%
  summarise(kpi19_alt_leveloffs = mean(mean_alt_leveloffs,na.rm = T), 
            y = y[1], month = month(time[1]), time = time[1]) %>%
  mutate(ym = ym(ym, tz = "UTC"))%>%
  ggplot(aes(x = as.Date(ym))) +
  geom_line(aes(y = kpi19_alt_leveloffs), color = "black") +
  scale_y_continuous("Altitude (ft)") +
  theme(axis.title.y = element_text(color = "black", size=25),
        axis.title.y.right = element_text(color = distColor, size=25))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y",expand=c(0,0), "Month" )+
  facet_wrap(~real_arr,nrow = 2) + guides(color="none")+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(legend.text = element_text(size = 12))+
  theme(panel.spacing.y = unit(5, "mm"),panel.spacing.x = unit(15, "mm"))+
  theme(axis.title  = element_text(size =32), strip.text = element_text(size =32), 
        title = element_text(size = 32))+
  theme(axis.text.x = element_text(size = 22, angle = 45, hjust = 1.1))+  
  theme(axis.text.y = element_text(size = 22)) +
  labs(x = "Year")

ggsave(linechart, filename = paste0(your_directory, "/media_alt_leveloff_kpi19.png"),width = 1720*3, height = 1080*3, units = "px")

# media mensal por aeroporto
for (i in unique(kpis$real_arr)){
  linechart <- kpis %>%
    subset(real_arr  == i)%>%
    group_by(ym) %>%
    summarise(kpi19_time = mean(kpi19_time,na.rm = T),kpi19_distance = mean(kpi19_distance,na.rm = T), 
              y = y[1], month = month(time[1]), time = time[1], speed = mean(mean_speed, na.rm = T)) %>%
    mutate(ym = ym(ym, tz = "UTC"))%>%
    ggplot(aes(x = as.Date(ym))) +
    geom_line(aes(y = kpi19_time), color = timeColor) +
    geom_line(aes(y = kpi19_distance/4), color = distColor) +
    scale_y_continuous("KPI (min)", sec.axis = sec_axis(~.*4, name = "KPI (NM)")) +
    theme(axis.title.y = element_text(color = timeColor, size=24),
          axis.title.y.right = element_text(color = distColor, size=24))+
    scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y",expand=c(0,0), "Mês" )+ 
    theme(legend.key.size = unit(2, 'cm'))+
    theme(legend.text = element_text(size = 12))+
    theme(panel.spacing.y = unit(5, "mm"),panel.spacing.x = unit(5, "mm"))+
    theme(axis.title  = element_text(size =32), strip.text = element_text(size =26), 
          title = element_text(size = 24))+
    theme(axis.text.x = element_text(size = 18, angle = -90))+  
    theme(axis.text.y = element_text(size = 32))
  linechart
  
  ggsave(linechart, filename = paste0(paste0(your_directory, "/media_mensal_kpi19"),"_",i,".png"),width = 1720*3, height = 1080*2, units = "px")
  
}

#-ranking distance -----------------------------------------------------------------------------------------------------

# Create "All" summary
rankingall <- kpis %>%
  group_by(real_arr) %>%
  summarise(KPI_distance = round(mean(kpi19_distance, na.rm = TRUE), digits = 1)) %>%
  mutate(y = "Todos")

# First ranking plot
ranking <- kpis %>%
  group_by(real_arr, y) %>%
  mutate(y = as.character(y)) %>%
  summarise(KPI_distance = round(mean(kpi19_distance, na.rm = TRUE), digits = 1)) %>%
  rbind(rankingall) %>%
  ggplot(aes(x = reorder(real_arr, KPI_distance), y = KPI_distance)) +
  geom_bar(stat = 'identity', position = "dodge", fill = "lightblue") +
  ylab("KPI 19 (NM)") +
  theme(axis.title = element_text(size = 32),
        axis.text.x = element_text(size = 24, angle = 0),
        axis.text.y = element_text(size = 29), 
        strip.text = element_text(size = 32)) +
  geom_text(aes(label = KPI_distance), 
            position = position_dodge(width = 0.9),
            size = 8, vjust = 0.5, hjust = 0.75) +
  facet_wrap(~y, nrow = 1) +
  coord_flip() +
  xlab("Airport")
ranking

ggsave(ranking, filename = paste0(your_directory, "/ranking_media_KPI_distance_19.png"),
       width = 1920*3, height = 1080*3, units = "px")

###### New way to plot by month

# Create "All" summary
rankingall <- kpis %>%
  group_by(real_arr) %>%
  summarise(KPI_distance = round(mean(kpi19_distance, na.rm = TRUE), digits = 1)) %>%
  mutate(ym = "All")

# Create monthly ranking
ranking <- kpis %>%
  group_by(real_arr, ym) %>%
  mutate(ym = as.character(ym)) %>%
  summarise(KPI_distance = round(mean(kpi19_distance, na.rm = TRUE), digits = 1)) %>%
  rbind(rankingall) %>%
  mutate(ym = case_when(
    ym != "All" ~ format(as.Date(paste0(substr(ym, 1, 4), "-", substr(ym, 5, 6), "-01")), "%m-%Y"),
    TRUE ~ "All"
  )) %>%
  mutate(ym = factor(ym, levels = c("11-2024", "12-2024", "01-2025", "02-2025", "03-2025", "04-2025", "All")))

# Plotting monthly ranking
ranking <- ranking %>%
  ggplot(aes(x = fct_reorder(real_arr, KPI_distance), y = KPI_distance)) +
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

ggsave(ranking, filename = paste0(your_directory, "/ranking_media_KPI_distance_19_all.png"),
       width = 1920*3, height = 1080*3, units = "px")

#-ranking time -----------------------------------------------------------------------------------------------------

rankingall <- kpis %>%
  group_by(real_arr)%>%
  summarise(KPI_time = round(mean(kpi19_time,na.rm = T),digits=1))%>%
  mutate(y = "Todos")

ranking <- kpis %>%
  group_by(real_arr,y)%>%
  mutate(y = as.character(y))%>%
  summarise(KPI_time = round(mean(kpi19_time,na.rm = T),digits=1))%>%
  rbind(rankingall)%>%
  ggplot(aes(x= reorder(real_arr, KPI_time), y=KPI_time))+
  geom_bar(stat='identity',position="dodge", fill = "lightblue")+
  ylab(" KPI 19 (min)") +
  theme(axis.title  = element_text(size =32),
        axis.text.x = element_text(size = 24,angle = 0),
        axis.text.y = element_text(size = 29), 
        strip.text = element_text(size =32))+
  geom_text(aes(label=KPI_time), 
            position=position_dodge(width=0.9),
            size = 8, vjust=0.5, hjust=0.75) +
  facet_wrap(~y,nrow = 1) + coord_flip() +xlab("Aeroporto") 
ranking

ggsave(ranking, filename = paste0(your_directory, "/ranking_media_KPI_time_19.png"),width = 1920*3, height = 1080*3, units = "px")

###### New way to plot by month

rankingall <- kpis %>%
  group_by(real_arr)%>%
  summarise(KPI_time = round(mean(kpi19_time,na.rm = T),digits=1))%>%
  mutate(ym = "All")

ranking <- kpis %>%
  group_by(real_arr, ym) %>%
  mutate(ym = as.character(ym)) %>%
  summarise(KPI_time = round(mean(kpi19_time, na.rm = T), digits = 1)) %>%
  rbind(rankingall) %>%
  mutate(ym = case_when(
    ym != "All" ~ format(as.Date(paste0(substr(ym, 1, 4), "-", substr(ym, 5, 6), "-01")), "%m-%Y"),  # Convert from YYYYMM to MM-YYYY
    TRUE ~ "All"
  )) %>%
  mutate(ym = factor(ym, levels = c("11-2024", "12-2024", "01-2025", "02-2025", "03-2025", "04-2025", "All")))

# Plotting
ranking <- ranking %>%
  ggplot(aes(x = fct_reorder(real_arr, KPI_time), y = KPI_time)) +
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

ggsave(ranking, filename = paste0(your_directory, "/ranking_media_KPI_time_19_all.png"),width = 1920*3, height = 1080*3, units = "px")

#----------------- boxplot agregado -----------------------------------------------------

boxplotx <- kpis %>%
  filter(kpi19_time < quantile(kpi19_time, 0.98, na.rm = TRUE))%>%
  ggplot(aes(x= reorder(real_arr, kpi19_time), y=kpi19_time))+
  geom_boxplot(color = 'black', fill = 'skyblue', outlier.size = 0.1,outlier.alpha = 0.02, outlier.color='black') +
  coord_flip() + 
  theme(axis.title  = element_text(size =32),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 28), 
        strip.text = element_text(size =32))+
  theme(legend.text = element_text(size = 32),
        legend.title = element_text(size = 32))+  #ggtitle("KPI 05 - dispersão sem outliers - 98%") +
  xlab("Airport") +
  ylab("Vertical efficiency during descent (min)")
ggsave(boxplotx, filename = paste0(your_directory, "boxplot_time_agregado_19.png"),width = 1920*3, height = 1080*3, units = "px")

boxplotx <- kpis %>%
  filter(kpi19_distance < quantile(kpi19_distance, 0.98, na.rm = TRUE))%>%
  ggplot(aes(x= reorder(real_arr, kpi19_distance), y=kpi19_distance))+
  geom_boxplot(color = 'black', fill = 'skyblue', outlier.size = 0.1,outlier.alpha = 0.02, outlier.color='black') +
  coord_flip() + 
  theme(axis.title  = element_text(size =32),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 28), 
        strip.text = element_text(size =32))+
  theme(legend.text = element_text(size = 32),
        legend.title = element_text(size = 32))+  #ggtitle("KPI 05 - dispersão sem outliers - 98%") +
  xlab("Aeroporto") +
  ylab("KPI 19 (NM)")
ggsave(boxplotx, filename = "C:/Users/guilherme.bernardo/Desktop/DASC 2025/Images/KPI19/boxplot_distance_agregado_19.png",width = 1920*3, height = 1080*3, units = "px")


#------------------------ box plot por aeroporto ----------------------------------------

boxplotx <- kpis %>%
  mutate(ym_date = ym(ym),
         ym_label = format(ym_date, "%m-%Y")) %>%
  filter(kpi19_time < quantile(kpi19_time, 0.98, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(ym_label), y = kpi19_time)) +
  geom_boxplot(color = 'black', fill = 'skyblue', outlier.size = 0.1, outlier.alpha = 0.02, outlier.color = 'black') +
  facet_wrap(~real_arr, nrow = 2) +
  xlab("Month") + 
  ylab("Vertical efficiency during descent (min)") +
  theme(
    axis.title = element_text(size = 32),
    axis.text.x = element_text(size = 32, angle = 90),
    axis.text.y = element_text(size = 32),
    strip.text = element_text(size = 32),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 32)
  )
# Save the plot
ggsave(boxplotx, filename = "C:/Users/guilherme.bernardo/Desktop/DASC 2025/Images/KPI19/boxplot_time_by_month_19.png", width = 1920*3, height = 1080*3, units = "px")

boxplotx <- kpis %>%
  filter(kpi19_distance < quantile(kpi19_distance, 0.98, na.rm = TRUE))%>%
  ggplot(aes(x = factor(y), y = kpi19_distance)) +
  geom_boxplot(color = 'black', fill = 'skyblue', outlier.size = 0.1,outlier.alpha = 0.02, outlier.color='black') +
  facet_wrap(~real_arr, nrow=2) +xlab("Aeroporto") + ylab("KPI 19") +
  theme(axis.title  = element_text(size =32),
        axis.text.x = element_text(size = 32,angle = -90),
        axis.text.y = element_text(size = 28), 
        strip.text = element_text(size =32))+
  theme(legend.text = element_text(size = 32),
        legend.title = element_text(size = 32))+
  xlab("Ano") +
  ylab("KPI 19 (NM)")
boxplotx
ggsave(boxplotx, filename = "~/Relatorio_2018_2023/Figs_KPI19/boxplot_distance_19.png",width = 1920*3, height = 1080*3, units = "px")

#----------------- boxplot por mes -----------------------

boxplotx <- kpis %>%
  filter(kpi19_time < quantile(kpi19_time, 0.98, na.rm = TRUE))%>%
  mutate(ym = sub( '(?<=.{4})', '-', ym, perl=TRUE ))%>%
  ggplot(aes(x = factor(ym), y=kpi19_time)) +
  geom_boxplot(color = 'black', fill = 'skyblue', outlier.size = 0.1,outlier.alpha = 0.02, outlier.color='black') +
  theme(axis.title  = element_text(size =32),
        axis.text.x = element_text(size = 24,angle = -90),
        axis.text.y = element_text(size = 28), 
        strip.text = element_text(size =32))+
  theme(legend.text = element_text(size = 32),
        legend.title = element_text(size = 32))+ 
  xlab("Ano-Mês") +
  ylab("KPI 19 (min)")
ggsave(boxplotx, filename = "~/Relatorio_2018_2023/Figs_KPI19/boxplot_time_mes_19.png",width = 1920*3, height = 1080*3, units = "px")

boxplotx <- kpis %>%
  filter(kpi19_distance < quantile(kpi19_distance, 0.98, na.rm = TRUE))%>%
  mutate(ym = sub( '(?<=.{4})', '-', ym, perl=TRUE ))%>%
  ggplot(aes(x = factor(ym), y=kpi19_distance)) +
  geom_boxplot(color = 'black', fill = 'skyblue', outlier.size = 0.1,outlier.alpha = 0.02, outlier.color='black') +
  theme(axis.title  = element_text(size =32),
        axis.text.x = element_text(size = 24,angle = -90),
        axis.text.y = element_text(size = 28), 
        strip.text = element_text(size =32))+
  theme(legend.text = element_text(size = 32),
        legend.title = element_text(size = 32))+ 
  xlab("Ano-Mês") +
  ylab("KPI 19 (NM)")
ggsave(boxplotx, filename = "~/Relatorio_2018_2023/Figs_KPI19/boxplot_distance_mes_19.png",width = 1920*3, height = 1080*3, units = "px")
