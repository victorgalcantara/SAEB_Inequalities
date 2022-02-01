# IDEB por Nível Socioeconômico
# Dados: IDEB e SAEB disponibilizados pelo INEP
# Author: Victor Alcantara (PPGSA/UFRJ)
# Date: 07.01.22

# 0. Packages and Setup ======================================================

library(tidyverse)
library(ggplot2)
library(geobr)
library(sf)

wd <- "E:/VGA/dados/EDUCACAO/MESTRADO/LM Escolas"
setwd(wd)

# Imput data ------------------------------------------------------

brasil <- read_country(year=2019)

# Dado já tratado conforme script abaixo
load("reg+IDEB.RDS")

#União das bases do IDEB e da regressão por escola
wd_2 <- "C:/Users/VictorGabriel/Documents/00_dados/EDUCACAO/"
load(paste0(wd_2,"data_9EF_completo.RDS"))

ano = seq(2007,2019,2)
mydata = data.frame(NULL)

for (i in 1:length(ano)){
load(paste0("reg_esc_",ano[i],".RDS"))
  d <- merge(reg_MT,reg_LP,by="ID_ESCOLA")
  d <- d %>% mutate(.,me_R2=(R2_LP+R2_MT)/2,
                      ano = ano[i])
  mydata <- bind_rows(d,mydata)
     }

d <- merge(mydata,data,by = c("ID_ESCOLA","ano"))

d <- d %>% mutate(.,
                  IDEB = as.numeric(IDEB),
                  PIDEB = as.numeric(PIDEB),
                  MEDIA_LP = ifelse(is.na(MEDIA_LP),MEDIA_9EF_LP,MEDIA_LP),
                  MEDIA_MT = ifelse(is.na(MEDIA_MT),MEDIA_9EF_MT,MEDIA_MT),
                  esc_cat = case_when(
                    IDEB < 4 & me_R2 < 0.1 ~ "Escola vulnerável",
                    IDEB >= 4 & IDEB < 6 & me_R2 < 0.1 ~ "transição",
                    IDEB >= 6 & me_R2 < 0.1 ~ "Escola eficaz",
                    IDEB < 4 & me_R2 > 0.1 & me_R2 <= 0.3 ~ "transição",
                    IDEB >= 4 & IDEB < 6 & me_R2 > 0.1 & me_R2 <= 0.3 ~ "Escola mediana",
                    IDEB > 6 & me_R2 > 0.1 & me_R2 <= 0.3 ~ "transição",
                    IDEB < 4 & me_R2 > 0.3 ~ "Escola reprodutora",
                    IDEB >= 4 & IDEB < 6 & me_R2 > 0.3 ~ "transição",
                    IDEB >= 6 & me_R2 > 0.3 ~ "Escola discriminatória"
                  ),
                  IE = IDEB-me_R2*10 # Indicador de Eficácia
)

save(d,file="data_ideb+regs_2.RDS")

# Graphs

textcol <- "gray40"

set_theme <- theme(
  axis.title.x=element_text(size=14,colour="black"),
  axis.title.y=element_text(size=14,colour="black"),
  axis.text = element_text(size=16,colour=textcol),
  axis.ticks=element_line(size=0.5),
  legend.position = "bottom",
  legend.text = element_text(size=10,colour="black")
)

plt1.0 <- d %>% filter(ano == 2017,n_validos.x > 30) %>% 
  ggplot(aes(y=IDEB,x=me_R2))+
  theme_classic()+
  geom_jitter(color="steelblue")+
  scale_x_continuous(limits=c(0,1),expand=c(0,0),
                     breaks = seq(0,1,0.1),
                     name="Coeficiente de determinação médio (R2)") +
  scale_y_continuous(breaks = seq(0,10,1),limits = c(0,10),
                     name="IDEB",expand=c(0,0))

plt1.1 <- plt1.0 +
  geom_hline(aes(yintercept = 4), linetype = "dotdash", lwd = .7)+
  geom_hline(aes(yintercept = 6), linetype = "dotdash", lwd = .7)+
  geom_vline(aes(xintercept = 0.1), linetype = "dotdash", lwd = .7)+
  geom_vline(aes(xintercept = 0.3), linetype = "dotdash", lwd = .7)+
  labs(color="Dep.Admin.")+
  set_theme

### Teste mapa

# 3. Maps ============================================================

t<- d %>% filter(ano == 2017)
prop.table(table(t$esc_cat))

my_points <- d %>% select(ano,ID_ESCOLA,NO_ESCOLA,ID_MUNICIPIO,
             NO_MUNICIPIO,IDEB,me_R2,esc_cat,
             Longitude,Latitude) %>% na.exclude()

points_coord <- my_points %>%
  select(Longitude,Latitude) %>% 
  st_as_sf(.,coords = c("Longitude", "Latitude"), crs = 4326,
           agr="constant")

points_coord_t <- st_transform(points_coord, crs=4326)

my_points$geom <- points_coord_t

Brasil <- merge(brasil, my_points, by.x = "code_state",
                by.y = "code_state")

# 3.0 Removing axis from the ggplot layers ---------------------------

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 legend.text = element_text(size=10),
                 legend.title = element_text(size=12)
)

# 3.2 Brasil ----------------------------------------------------

# Proporção de escolas eficazes nos estados
t <- d %>% group_by(ID_UF) %>% summarise(
  p_eficaz = prop.table(table(esc_cat))[2])
t <- t %>% rename(
  abbrev_state = ID_UF
)

br <- merge(brasil,t)


mp <- my_points %>% filter(esc_cat %in% c("Escola discriminatória",
                                          "Escola reprodutora",
                                          "Escola eficaz",
                                          "Escola vulnerável"))

mp1 <- mp %>% filter(ano %in% c(2017,2019))

m1 <- ggplot() +
  geom_point(data=mp1,aes(x=Longitude,y=Latitude,color=esc_cat),
             cex=.8)+
  geom_sf(data=br,color="black", size= .8,alpha=0.1)+ #try color = "Grey"
  theme_minimal()+
  scale_color_manual(values=c("gold","steelblue","red","pink"))+
  labs(color="Tipologia \n Escolar")+
  no_axis+
  guides(color = guide_legend(override.aes = list(size = 3)))
