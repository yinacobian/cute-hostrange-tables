##Enterobacter phages host range
##Data for jumbo phages from Natasha 2023-10-27

#for clustering, install:
#devtools::install_github("nicolash2/ggdendroplot")

library(devtools)
library(ggdendroplot)
library(tidyverse)
library(ggplot2)
library(viridisLite)
library(patchwork)
library (ggeasy)
library(ggpubr)

setwd("C:/Users/anana/Sync/Pride Lab/Enterobacter jumbophages - Natasha/Enterophages host range/")


###PLOTS FOR ESCHERICHIA ###

escherichia_30C <- read_csv(file = "Enterojumbos-vs-escherichia-30C.csv", col_names=TRUE, col_types=cols(.default=col_character()))
escherichia_37C <- read_csv(file = "Enterojumbos-vs-escherichia-37C.csv", col_names=TRUE, col_types=cols(.default=col_character()))

T_escherichia_30C <- escherichia_30C %>%
  pivot_longer (c('Chasing Life':'Silp'), names_to = "Phage", values_to="Lysis") %>%
  mutate(Lysis =as.numeric(Lysis))

T_escherichia_37C <- escherichia_37C %>%
  pivot_longer (c('Chasing Life':'Silp'), names_to = "Phage", values_to="Lysis") %>%
  mutate(Lysis =as.numeric(Lysis))
  

##Data exploration heatmap:
A <-
  ggplot (T_escherichia_30C, aes(Bacteria,Phage, fill=Lysis)) +
  geom_tile(color = "white", lwd = .5, linetype = 1) +
  coord_fixed() +
  coord_flip() +
  scale_fill_stepsn(
    breaks=c(0,0.9,1,10,100), 
    values = scales::rescale(c(0,0.9,1,10,100)),
    colours=c('white','lavender','mediumpurple1','mediumpurple4'),
    labels=c(" ","no lysis","low", "intermediate", "high"),
    show.limits=TRUE,
    na.value="grey90") +
  ggtitle ("Escherichia 30°C") +
  ggeasy::easy_center_title() +
  scale_y_discrete(guide = guide_axis(angle = 90), position = "right") +
  scale_x_discrete(position = "top") 

B <-
  ggplot (T_escherichia_37C, aes(Bacteria,Phage, fill=Lysis)) +
  geom_tile(color = "white", lwd = .5, linetype = 1) +
  coord_fixed() +
  coord_flip() +
  scale_fill_stepsn(
    breaks=c(0,0.9,1,10,100), 
    values = scales::rescale(c(0,0.9,1,10,100)),
    colours=c('white','lavender','mediumpurple1','mediumpurple4'),
    labels=c(" ","no lysis","low", "intermediate", "high"),
    show.limits=TRUE,
    na.value="grey90") +
  ggtitle ("Escherichia 37°C") +
  ggeasy::easy_center_title() +
  scale_y_discrete(guide = guide_axis(angle = 90), position = "right") +
  scale_x_discrete(position = "top") 

ESCHERICHIAPLOT <- ggarrange(A, B,  common.legend = TRUE, legend="right")
ESCHERICHIAPLOT


### PLOTS FOR SALMONELLA ###

salmonella_30C <- read_csv(file = "Enterojumbos-vs-salmonella-30C.csv", col_names=TRUE, col_types=cols(.default=col_character()))
salmonella_37C <- read_csv(file = "Enterojumbos-vs-salmonella-37C.csv", col_names=TRUE, col_types=cols(.default=col_character()))

T_salmonella_30C <- salmonella_30C %>%
  pivot_longer (c('Chasing Life':'Silp'), names_to = "Phage", values_to="Lysis") %>%
  mutate(Lysis =as.numeric(Lysis))

T_salmonella_37C <- salmonella_37C %>%
  pivot_longer (c('Chasing Life':'Silp'), names_to = "Phage", values_to="Lysis") %>%
  mutate(Lysis =as.numeric(Lysis))

C <- 
ggplot (T_salmonella_30C, aes(Bacteria,Phage, fill=Lysis)) +
  geom_tile(color = "white", lwd = .5, linetype = 1) +
  coord_fixed() +
  coord_flip() +
  scale_fill_stepsn(
    breaks=c(0,0.9,1,10), 
    values = scales::rescale(c(0,0.9,1,10)),
    colours=c('white','lavender','mediumpurple1'),
    labels=c(" ","no lysis","low", "intermediate"),
    show.limits=TRUE,
    na.value="grey90") +
  ggtitle ("Salmonella 30°C") +
  ggeasy::easy_center_title() +
  scale_y_discrete(guide = guide_axis(angle = 90), position = "right") +
  scale_x_discrete(position = "top") 

D <-
ggplot (T_salmonella_37C, aes(Bacteria,Phage, fill=Lysis)) +
  geom_tile(color = "white", lwd = .5, linetype = 1) +
  coord_fixed() +
  coord_flip() +
  scale_fill_stepsn(
    breaks=c(0,0.9,1,10,100), 
    values = scales::rescale(c(0,0.9,1,10,100)),
    colours=c('white','lavender','mediumpurple1','mediumpurple4'),
    labels=c(" ","no lysis","low", "intermediate", "high"),
    show.limits=TRUE,
    na.value="grey90") +
  ggtitle ("Salmonella 37°C") +
  ggeasy::easy_center_title() +
  scale_y_discrete(guide = guide_axis(angle = 90), position = "right") +
  scale_x_discrete(position = "top") 

SALMONELLAPLOT <- ggarrange(C, D, legend="right")
SALMONELLAPLOT


###PLOTS FOR SERRATIA###

serratia_30C <- read_csv(file = "Enterojumbos-vs-serratia-30C.csv", col_names=TRUE, col_types=cols(.default=col_character()))
serratia_37C <- read_csv(file = "Enterojumbos-vs-serratia-37C.csv", col_names=TRUE, col_types=cols(.default=col_character()))

T_serratia_30C <- serratia_30C %>%
  pivot_longer (c('Chasing Life':'Silp'), names_to = "Phage", values_to="Lysis") %>%
  mutate(Lysis =as.numeric(Lysis))

T_serratia_37C <- serratia_37C %>%
  pivot_longer (c('Chasing Life':'Silp'), names_to = "Phage", values_to="Lysis") %>%
  mutate(Lysis =as.numeric(Lysis))


##Data exploration heatmap:
E <-
  ggplot (T_serratia_30C, aes(Bacteria,Phage, fill=Lysis)) +
  geom_tile(color = "white", lwd = .5, linetype = 1) +
  coord_fixed() +
  coord_flip() +
  scale_fill_stepsn(
    breaks=c(0,0.9,1,10,100), 
    values = scales::rescale(c(0,0.9,1,10,100)),
    colours=c('white','lavender','mediumpurple1','mediumpurple4'),
    labels=c(" ","no lysis","low", "intermediate", "high"),
    show.limits=TRUE,
    na.value="grey90") +
  ggtitle ("Serratia 30°C") +
  ggeasy::easy_center_title() +
  scale_y_discrete(guide = guide_axis(angle = 90), position = "right") +
  scale_x_discrete(position = "top") 

F <-
  ggplot (T_serratia_37C, aes(Bacteria,Phage, fill=Lysis)) +
  geom_tile(color = "white", lwd = .5, linetype = 1) +
  coord_fixed() +
  coord_flip() +
  scale_fill_stepsn(
    breaks=c(0,0.9,1,10,100), 
    values = scales::rescale(c(0,0.9,1,10,100)),
    colours=c('white','lavender','mediumpurple1','mediumpurple4'),
    labels=c(" ","no lysis","low", "intermediate", "high"),
    show.limits=TRUE,
    na.value="grey90") +
  ggtitle ("Serratia 37°C") +
  ggeasy::easy_center_title() +
  scale_y_discrete(guide = guide_axis(angle = 90), position = "right") +
  scale_x_discrete(position = "top") 

serratiaPLOT <- ggarrange(E, F,  common.legend = TRUE, legend="right")
serratiaPLOT



###PLOTS FOR SHIGELLA###

shigella_30C <- read_csv(file = "Enterojumbos-vs-shigella-30C.csv", col_names=TRUE, col_types=cols(.default=col_character()))
shigella_37C <- read_csv(file = "Enterojumbos-vs-shigella-37C.csv", col_names=TRUE, col_types=cols(.default=col_character()))

T_shigella_30C <- shigella_30C %>%
  pivot_longer (c('Chasing Life':'Silp'), names_to = "Phage", values_to="Lysis") %>%
  mutate(Lysis =as.numeric(Lysis))

T_shigella_37C <- shigella_37C %>%
  pivot_longer (c('Chasing Life':'Silp'), names_to = "Phage", values_to="Lysis") %>%
  mutate(Lysis =as.numeric(Lysis))


##Data exploration heatmap:
G <-
  ggplot (T_shigella_30C, aes(Bacteria,Phage, fill=Lysis)) +
  geom_tile(color = "white", lwd = .5, linetype = 1) +
  coord_fixed() +
  coord_flip() +
  scale_fill_stepsn(
    breaks=c(0,0.9,1), 
    values = scales::rescale(c(0,0.9,1)),
    colours=c('white','lavender'),
    labels=c(" ","no lysis","low"),
    show.limits=TRUE,
    na.value="grey90") +
  ggtitle ("Shigella 30°C") +
  ggeasy::easy_center_title() +
  scale_y_discrete(guide = guide_axis(angle = 90), position = "right") +
  scale_x_discrete(position = "top") 

H <-
  ggplot (T_shigella_37C, aes(Bacteria,Phage, fill=Lysis)) +
  geom_tile(color = "white", lwd = .5, linetype = 1) +
  coord_fixed() +
  coord_flip() +
  scale_fill_stepsn(
    breaks=c(0,0.9,1), 
    values = scales::rescale(c(0,0.9,1)),
    colours=c('white','lavender'),
    labels=c(" ","no lysis","low"),
    show.limits=TRUE,
    na.value="grey90")  +
  ggtitle ("Shigella 37°C") +
  ggeasy::easy_center_title() +
  scale_y_discrete(guide = guide_axis(angle = 90), position = "right") +
  scale_x_discrete(position = "top") 

shigellaPLOT <- ggarrange(G, H,  common.legend = TRUE, legend="right")
shigellaPLOT
