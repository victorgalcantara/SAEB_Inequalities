# Title: Quantile comparison between schools by economic capital and race 
# Author: Victor Gabriel Alcantara
# Date: 07.01.22
# Github: https://github.com/victorgalcantara
# LinkedIn: https://www.linkedin.com/in/victorgalcantara/

# data: SAEB 2007-2019
# 1. Median differences (tables)
# 2. Percentiles differences (graphs)

# 0. Packages and Setup --------------------------------------------------------

library(tidyverse)
library(ggplot2)

wd <- "E:/VGA/dados/EDUCACAO/MESTRADO/LM Escolas/"
wd <- "E:/VGA/dados/EDUCACAO/MESTRADO/resultados/regs/Para corrigir"
setwd(wd)

# 1. data imput ----------------------------------------------------------------

load("data_esc_07-19.RDS")

# 2. Comparisons ---------------------------------------------------------------

# 2.1 DIFF IN MEDIAN =========================================

#################  ECONOMIC CAPITAL ######################

dif.median = data.frame("disciplina"=c("LP","LP","MT","MT")) # df to compare low/high
dif.median[,paste0(seq(2007,2019,2))] <- NA # create multiple vars

# LP ----
# Mediana por ano e grupos
d <- data %>% group_by(ano,ce_cat) %>% # group of ce for each year
  summarise(
  median = median(MEDIA_LP,na.rm = T) # median of each group
)

d <- spread(d,"ano",median) # wide to long format

# Guardando diferen�as
dB <- d %>% filter(ce_cat == "B")
dA <- d %>% filter(ce_cat == "A")

dif.median[1,2:8] = dB - dA # baixo - alto

# Diferen�a em ano de escolariza��o (~25 pontos na escala saeb)
dif.median[2,2:8] = dif.median[1,2:8]/25

# MT -----
d <- data %>% group_by(ano,ce_cat) %>% summarise(
  median = median(MEDIA_MT,na.rm = T)
)

d <- spread(d,"ano",median)

# Guardando diferen�as
dB <- d %>% filter(ce_cat == "B")
dA <- d %>% filter(ce_cat == "A")

dif.median[3,2:8] = dB - dA # baixo - alto

# Diferen�a em ano de escolariza��o (~25 pontos na escala saeb)
dif.median[4,2:8] = dif.median[3,2:8]/25

dif.median[,2:8] <- round(dif.median[,2:8],2)

write.csv(dif.median,file=paste0(wd_1,"/../difMedian_CE.csv"))

#######################  RACE  ##########################

dif.median = data.frame("disciplina"=c("LP","LP","MT","MT"))
dif.median[,paste0(seq(2007,2019,2))] <- NA

# LP
# Mediana por ano e grupos
d <- data %>% group_by(ano,raca_cat) %>% summarise(
  median = median(MEDIA_LP,na.rm = T)
)

d <- spread(d,"ano",median)

# Guardando diferen�as
dif.median[1,2:8] = d[1,2:8] - d[2,2:8]
# Diferen�a em ano de escolariza��o (~25 pontos na escala saeb)
dif.median[2,2:8] = dif.median[1,2:8]/25

# MT
d <- data %>% group_by(ano,raca_cat) %>% summarise(
  median = median(MEDIA_MT,na.rm = T)
)

d <- spread(d,"ano",median)

dif.median[3,2:8] = d[1,2:8] - d[2,2:8]
dif.median[4,2:8] = dif.median[3,2:8]/25
dif.median[,2:8] <- round(dif.median[,2:8],2)

write.csv(dif.median,file=paste0(wd_1,"/../difMedian_raca.csv"))

################### CE & RACE ##################################

dif.median = data.frame("disciplina"=c("LP","LP","MT","MT"))
dif.median[,paste0(seq(2007,2019,2))] <- NA

# LP
# Mediana por ano e grupos
d <- data %>% group_by(ano,raca_cat,ce_cat) %>% summarise(
  median = median(MEDIA_LP,na.rm = T)
)

d <- spread(d,"ano",median)

# Guardando diferen�as
dif.median[1,2:8] = d[5,3:8] - d[3,3:8]
# Diferen�a em ano de escolariza��o (~25 pontos na escala saeb)
dif.median[2,2:8] = dif.median[1,2:8]/25

# MT
d <- data %>% group_by(ano,raca_cat,ce_cat) %>% summarise(
  median = median(MEDIA_MT,na.rm = T)
)

d <- spread(d,"ano",median)

dif.median[3,2:8] = d[5,3:9] - d[3,3:9]
dif.median[4,2:8] = dif.median[3,2:8]/25
dif.median[,2:8] <- round(dif.median[,2:8],2)

write.csv(dif.median,file=paste0(wd_1,"/../difMedian_raca.csv"))

# ============= 2.2 DIFF IN P PERCENTILES =========================

dif.pp.LP = data.frame("ano"=rep(NA,707),"dif.pp"=rep(NA,707))

# LP
# p.p. por ano e grupos
d <- data %>% group_by(ano,raca_cat,ce_cat) %>% summarise(
  pp_LP = quantile(MEDIA_LP,probs=seq(0,1,0.01),na.rm = T),
  pp = seq(0,100,1)
)

d <- spread(d,"ano",pp_LP,pp)

d <- d %>% filter((raca_cat == "B" & ce_cat == "A") | 
                  (raca_cat == "NB" & ce_cat == "B") )

BA <- d$raca_cat == "B" & d$ce_cat == "A"
NBB <- d$raca_cat == "NB" & d$ce_cat == "B"

# Guardando diferen�as
dif.pp.LP[,"dif.pp"] = d[BA,"pp_LP"] - d[NBB,"pp_LP"]
dif.pp.LP$pp  = rep(seq(0,100,1),7)
dif.pp.LP$ano = rep(seq(2007,2019,2),times=rep(101,7))

########  MT

dif.pp.MT = data.frame("ano"=rep(NA,707),"dif.pp"=rep(NA,707))

# MT
# p.p. por ano e grupos
d <- data %>% group_by(ano,raca_cat,ce_cat) %>% summarise(
  pp_MT = quantile(MEDIA_MT,probs=seq(0,1,0.01),na.rm = T),
  pp = seq(0,100,1)
)

d <- spread(d,"ano",pp_MT,pp)

d <- d %>% filter((raca_cat == "B" & ce_cat == "A") | 
                    (raca_cat == "NB" & ce_cat == "B") )

BA <- d$raca_cat == "B" & d$ce_cat == "A"
NBB <- d$raca_cat == "NB" & d$ce_cat == "B"

# Guardando diferen�as
dif.pp.MT[,"dif.pp"] = d[BA,"pp_MT"] - d[NBB,"pp_MT"]
dif.pp.MT$pp  = rep(seq(0,100,1),7)
dif.pp.MT$ano = rep(seq(2007,2019,2),times=rep(101,7))

save(dif.pp.LP,dif.pp.MT,file="difpp_07-19.RDS")

# 3. GRAPHS =========================================================

dif.pp.LP$ano <- as.character(dif.pp.LP$ano)
dif.pp.MT$ano <- as.character(dif.pp.MT$ano)

textcol <- "gray40"

set_theme <- theme(
axis.title.x=element_text(size=14,colour="black"),
axis.title.y=element_text(size=14,colour="black"),
axis.text = element_text(size=14,colour=textcol),
axis.ticks=element_line(size=0.4),
legend.position = "right",
legend.text = element_text(size=14,colour="black")
)

dif.pp.LP %>%
ggplot(aes(y=dif.pp,x = pp,color=ano))+
theme_bw()+
geom_line(cex=1)+
geom_hline(yintercept = 0, colour = "black",cex=1,linetype="dotdash")+
scale_y_continuous(limits = c(-20,80), breaks = seq(-20,80,10),
name = "Diferen�a em p.p.")+
scale_x_continuous(breaks = seq(0,100,10),
name = "Quantis")+
scale_color_manual(values=heat.colors(7,alpha = .8,rev = T))+
labs(color="Ano")+
set_theme

dif.pp.MT %>%
ggplot(aes(y=dif.pp,x = pp,color=ano))+
  theme_bw()+
  geom_line(cex=1)+
  geom_hline(yintercept = 0, colour = "black",cex=1,linetype="dotdash")+
  scale_y_continuous(limits = c(-20,80), breaks = seq(-20,80,10),
                     name = "Diferen�a em p.p.")+
  scale_x_continuous(breaks = seq(0,100,10),
                     name = "Quantis")+
  scale_color_manual(values=heat.colors(7,alpha = .8,rev = T))+
  labs(color="Ano")+
  set_theme
