# Taxa de matricula liquida
# Author: Victor Alcantara (PPGSA/UFRJ)
# Date: 27.12.21

# 0. Packages and Setup ======================================================

library(tidyverse)
library(readxl)
library(ggplot2)

# 1. Openning data ===========================================================
# Diretório com as bases de dados que iremos trabalhar guardado no objeto 'wd'
wd <- "C:/Users/VictorGabriel/documents/00_dados/EDUCACAO/"
setwd(wd)

tx_matricula <- read_delim(paste0(wd,"tx_matricula-liquida_1980-2019.csv"),delim = ";")

data <- tx_matricula %>% gather(., 
                                key = "ano",
                                value = "taxa",
                                -c("Brasil","OPCAO"))
names(data) <- c("pais","etapa","ano","taxa")
data$ano    <- as.numeric(data$ano)
data$taxa   <- as.numeric(data$taxa)

save(data,file="tx_ML_1980-2020.RDS")

# GGPLOT
textcol <- "gray40"

set_theme <- theme(
  axis.title.x=element_text(size=12,colour="black"),
  axis.title.y=element_text(size=12,colour="black"),
  axis.text = element_text(size=14,colour=textcol),
  axis.ticks=element_line(size=0.4),
  legend.position = "bottom",
  legend.text = element_text(size=14,colour="black"),
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_line( size=.03, color="gray70")
)

data %>% na.exclude() %>% 
  ggplot(.,aes(x=ano,y=taxa,color=etapa))+
  geom_point(aes(shape=etapa),cex=2)+
  geom_line(cex=1.5)+
  scale_x_continuous(name="Ano",
                     limits = c(1980,2020),
                     breaks = seq(1980,2020,5))+
  scale_y_continuous(name="Taxa de matrícula líquida (%)",
                     limits = c(0,100),
                     breaks = seq(0,100,10))+
  scale_color_manual(values=c("#00539c","#00ff00","#ff0000"))+
  labs(color="Segmentos",shape="Segmentos")+
  theme_classic()+
  set_theme
  