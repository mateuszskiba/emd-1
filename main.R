# INSTALATIONS
install.packages('zoo')
install.packages('dplyr')
install.packages('corrgram')
install.packages('ggplot2')
install.packages('tidyverse')
install.packages('lubridate')
install.packages('fredr')
install.packages('cowplot')
install.packages('data.table')
install.packages('gganimate')
install.packages('gifski')
install.packages('png')


# LIBRARIES
library(zoo)
library(dplyr)
library(corrgram)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(fredr)
library(cowplot)
library(data.table)
library(gganimate)
library(data.table)
library(gifski)
library(png)


# READ DATA
herrings_raw_top <- read.csv(file='https://raw.githubusercontent.com/mateuszskiba/emd-1/master/sledzie.csv?token=AFU5A4AIIMA5QAADFMACAFS55ZYIY', header=TRUE, sep=',', na.strings='?', nrows=100)
classes <- sapply(herrings_raw_top, class)
herrings_raw <- read.csv(file='https://raw.githubusercontent.com/mateuszskiba/emd-1/master/sledzie.csv?token=AFU5A4AIIMA5QAADFMACAFS55ZYIY', header=TRUE, sep=',', na.strings='?', colClasses=classes)
herrings <- 
  herrings_raw %>%
  do(na.locf(.))


# Correlations
corrgram(herrings, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt,
         main="Herrings - Variables Correlations")


# MUTATE DATA FOR ANIMATIONS
herrings_animation <- 
  herrings %>%
  mutate(yearn=ceiling(X/(max(X)/90))) %>% 
  select(yearn, length) %>%
  group_by(yearn) %>%
  summarize(length = mean(length))%>%
  mutate(yearn=ceiling(yearn/(max(yearn)/60)))


# CREATE ANIMATIONS
a <- 
  ggplot(data=herrings_animation,aes(x=yearn,y=length))+
  geom_line()+
  theme(legend.position="none")+
  scale_y_continuous(breaks=c(22, 23, 24, 25, 26, 27))+
  scale_x_continuous()+
  labs(title="Herrings Lengths Change in Time",
       x="Year",
       y="Length [cm]")+
  theme(plot.title=element_text(face="bold"),
        plot.caption=element_text(hjust=0))+
  transition_reveal(yearn)

animate(a, fps = 10, duration = 10, renderer = gifski_renderer())
