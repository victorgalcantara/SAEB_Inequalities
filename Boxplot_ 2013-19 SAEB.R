# Relação entre Nível Socioeconõmico e Proficiência Saeb 2013-2019
# Author: Victor Alcantara (PPGSA/UFRJ)
# Date: 15.05.21

# 0. Packages and Setup -------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(cowplot)  # for bind graphs together

# 1. Import data --------------------------------------------------------------

wd <- "C:/Users/VictorGabriel/Documents/DADOS/EDUCACAO/"

ano = seq(from=2013,to=2019,by=2) # anos em que Saeb passou a ser censitário

saeb <- list(s13 = data.frame(), s15 = data.frame(), s17 = data.frame(),
             s19 = data.frame())

for(i in 1:length(ano)) {
  saeb[[i]] <- read_csv(paste0(wd,"SAEB/",ano[i],"/DADOS/TS_ESCOLA.csv")) # abra todas as bases
  gc()
}

# 2. Data management -------------------------------------------------------

# 2.1 Select and filter ----------------------------------------------------

# SAEB
my_saeb <- list(s13 = data.frame(), s15 = data.frame(), s17 = data.frame(),
                s19 = data.frame())

for(i in 1:length(ano)) {
  my_saeb[[i]] <- saeb[[i]] %>% select(ID_ESCOLA, MEDIA_9EF_LP, MEDIA_9EF_MT, 
                                       ID_DEPENDENCIA_ADM, NIVEL_SOCIO_ECONOMICO) 
  }


# 2.2 Missing data -----------------------------------------------------------
#

for(i in 1:length(ano)) {
  my_saeb[[i]] <- na.exclude(my_saeb[[i]])
}

# 2.3 Cut and mutate ---------------------------------------------------------

# Categorias: niveis de interpretacao pedagogica utilizado no SARESP 
# e pelo Mov. Todos Pela Educação para tornar mais compreensível a
# escala SAEB

for(i in 1:length(ano)) {
my_saeb[[i]]$escala_saeb_LP        <- cut(my_saeb[[i]]$MEDIA_9EF_LP,
                                          breaks = c(0, 200, 275, 325,400),
                                          labels = c("Abaixo do básico",
                                                     "Básico",
                                                     "Adequado",
                                                     "Avançado"),
                                          ordered_result = TRUE, 
                                          right = FALSE)

}

for(i in 1:length(ano)) {
  my_saeb[[i]]$escala_saeb_MT        <- cut(my_saeb[[i]]$MEDIA_9EF_MT,
                                         breaks = c(0, 225, 300, 350,400),
                                         labels = c("Abaixo do básico",
                                                    "Básico",
                                                    "Adequado",
                                                    "Avançado"),
                                         ordered_result = TRUE, # Try without this #
                                         right = FALSE)
  
}

# Indicador de Nível Socioeconômico (INSE) -------------------------------------
# Trata-se de um indicador operacionalizado por Maria T. Alves e José F. Soares,
# ambos professores da UFMG com contribuições substantivas para a área e para
# o INEP.

for(i in 1:length(ano)) {
  my_saeb[[i]] <- rename(my_saeb[[i]], 
                         INSE = NIVEL_SOCIO_ECONOMICO)
  my_saeb[[i]]$INSE <- iconv(my_saeb[[i]]$INSE, to = 'Latin1')
}

# Transformando categorias para Baixo, Medio e Alto

# 2013
          
my_saeb[[1]] <- my_saeb[[1]] %>%  
  mutate(INSE = case_when(INSE == "Grupo 1" ~ 1,
                          INSE == "Grupo 2" ~ 1, 
                          INSE == "Grupo 3" ~ 2,
                          INSE == "Grupo 4" ~ 2,
                          INSE == "Grupo 5" ~ 2, 
                          INSE == "Grupo 6" ~ 3,
                          INSE == "Grupo 7" ~ 3
  ) )

# 2015

for(i in c(2)) {
  my_saeb[[i]] <- my_saeb[[i]] %>%  
    mutate(INSE = case_when(INSE == "Muito Baixo" ~ 1,
                            INSE == "Baixo"       ~ 1,
                            INSE == "Médio Baixo" ~ 2,
                            INSE == "Médio"       ~ 2,
                            INSE == "Médio Alto"  ~ 2,
                            INSE == "Alto"        ~ 3, 
                            INSE == "Muito Alto"  ~ 3
    ) )
}

# 2017 :  Por algum motivo não está padronizado em 7 categorias 
#         como nos outros anos. Estou forçando a barra
#         recodificando as categorias para baixo, médio e alto.
#         É necessário conferir a construção dessas categorias.
#         Talvez reconstruí-las seguindo a metodologia.

my_saeb[[3]] <- my_saeb[[3]] %>%  
  mutate(INSE = case_when(INSE == "Grupo 1" ~ 1,
                          INSE == "Grupo 2" ~ 1, 
                          INSE == "Grupo 3" ~ 2,
                          INSE == "Grupo 4" ~ 2,
                          INSE == "Grupo 5" ~ 3, 
                          INSE == "Grupo 6" ~ 3
  ) )

# 2019
for(i in c(4)) {
  my_saeb[[i]] <- my_saeb[[i]] %>%  
  mutate(INSE = case_when(INSE == "Nível I"   ~ 1,
                          INSE == "Nível II"  ~ 1,
                          INSE == "Nível III" ~ 2,
                          INSE == "Nível IV"  ~ 2,
                          INSE == "Nível V"   ~ 2,
                          INSE == "Nível VI"  ~ 3, 
                          INSE == "Nível VII" ~ 3
  ) )
}

# Ordenando as categorias
for(i in 1:length(ano)) {
my_saeb[[i]]$INSE <- factor(my_saeb[[i]]$INSE,
                     labels = c("Baixo", "Medio", "Alto"),
                     levels = c(1,2,3),
                     ordered = TRUE)
}

# Union ----------------------------------------------------------------------

for(i in 1:length(ano)) {
my_saeb[[i]] <- my_saeb[[i]] %>% mutate(ano = ano[i]) # ID para o ano da avaliação
}

# Unindo todas as bases 2013-2019 em um único banco para representar a variação temporal
mydata <- union(my_saeb[[1]],my_saeb[[2]])
mydata <- union(mydata,my_saeb[[3]])
mydata <- union(mydata,my_saeb[[4]])

# 3. Gráficos ===================================================================

# Português ----
# Camada 1 : Boxplot
bpLP <- ggplot(mydata, aes(x = INSE, y = MEDIA_9EF_LP) )+
  theme_bw()+
  geom_boxplot(outlier.shape = 2)+
               #outlier.color = "darkblue"
  # stat_summary(fun.y= mean, geom="point", 
  #              shape=20, size= 2, color="red", fill="red")+
  facet_grid( cols = vars(ano))

# Camada 2 : labels
bpLP <- bpLP +
  #scale_color_manual(values = c("brown4","turquoise1","green"))+
    scale_x_discrete(
    name = "Nível Socioeconômico (INSE)") +
  scale_y_continuous(
    name = "Desempenho médio em Português",
    limits = c(100,400) )

# Camada 3 : linhas horizontais para os níveis de proficiência na escala SAEB
bpLP <- bpLP +
  # Abaixo do básico < 200
  geom_hline(yintercept = 200, color = "red1", linetype = "dotdash", lwd = .8)+
  # Básico entre 200-275
  geom_hline(yintercept = 275, color = "blue", linetype = "dotdash", lwd = .8)+
  # Adequado entre 275-325
  geom_hline(yintercept = 325, color = "cyan4", linetype = "dotdash", lwd = .8)
  # Avançado maior que 325
bpLP

# Matemática ----
# Camada 1 : Boxplot
bpMT <- ggplot(mydata, aes(x = INSE, y = MEDIA_9EF_MT) )+
  theme_bw()+
  geom_boxplot(outlier.shape = 2)+
  #outlier.color = "darkblue"
  # stat_summary(fun.y= mean, geom="point", 
  #              shape=20, size= 2, color="red", fill="red")+
  facet_grid(~ano)

# Camada 2 : labels
bpMT <- bpMT +
  #scale_color_manual(values = c("brown4","turquoise1","green"))+
    scale_x_discrete(
    name = "Nível Socioeconômico (INSE)") +
  scale_y_continuous(
    name = "Desempenho médio em Matemática",
    limits = c(100,400) )

# Camada 3 : linhas horizontais
bpMT <- bpMT +
  # Abaixo do básico < 225
  geom_hline(yintercept = 225, color = "red1", linetype = "dotdash", lwd = .8)+
  # Básico entre 225-300
  geom_hline(yintercept = 300, color = "blue", linetype = "dotdash", lwd = .8)+
  # Adequado entre 300-350
  geom_hline(yintercept = 350, color = "cyan4", linetype = "dotdash", lwd = .8)
  # Avançado: acima de 350
bpMT

# Estatística descritiva ----------------------------------------

# Diferenças em mediana ----
# Distância entre os extremos: ALTO - BAIXO

# Português
dif_med.LP <- data.frame(s13 = NA, s15 = NA, s17 = NA, s19 = NA)

for(i in 1:length(ano)) { 
  dif_med.LP[1,i] <- median(my_saeb[[i]]$MEDIA_9EF_LP[my_saeb[[i]]$INSE == "Alto"]) - median(
    my_saeb[[i]]$MEDIA_9EF_LP[my_saeb[[i]]$INSE == "Baixo"]) }

dif_med.LP

# Matemática
dif_med.MT <- data.frame(s13 = NA, s15 = NA, s17 = NA, s19 = NA)

for(i in 1:length(ano)) { 
  dif_med.MT[1,i] <- median(my_saeb[[i]]$MEDIA_9EF_LP[my_saeb[[i]]$INSE == "Alto"]) - median(
    my_saeb[[i]]$MEDIA_9EF_LP[my_saeb[[i]]$INSE == "Baixo"]) }

dif_med.MT


# Diferenças em média ----
# Distância entre os extremos: ALTO - BAIXO

# POrtuguês
dif_mean.LP <- data.frame(s13 = NA, s15 = NA, s17 = NA, s19 = NA)

for(i in 1:length(ano)) { 
  dif_mean.LP[1,i] <- mean(my_saeb[[i]]$MEDIA_9EF_LP[my_saeb[[i]]$INSE == "Alto"]) - 
    mean(my_saeb[[i]]$MEDIA_9EF_LP[my_saeb[[i]]$INSE == "Baixo"])
}
dif_mean.LP

# Matemática
dif_mean.MT <- data.frame(s13 = NA, s15 = NA, s17 = NA, s19 = NA)

for(i in 1:length(ano)) { 
      dif_mean.MT[1,i] <- mean(my_saeb[[i]]$MEDIA_9EF_MT[my_saeb[[i]]$INSE == "Alto"]) - mean(
  my_saeb[[i]]$MEDIA_9EF_MT[my_saeb[[i]]$INSE == "Baixo"])
}
dif_mean.MT

# Fizemos uma média simples da proficiência em Português e Matemática.
# Dessas médias de desempenho, fizemos uma média por INSE. Portanto,
# temos a média de desempenho condicionada às Escolas e aos níveis
# socioeconômicos do alunado. Esse não é um procedimento adequado para
# comparar proficiência, pois a escala das duas matrizes de referência
# não são equivalentes em proficiência. Contudo, é um procedimento de
# generalização útil para a comparação entre Escolas por nível socioe-
# conômico.

# Com as diferenças entre medianas, vemos que houve uma queda das distâncias
# de proficiência entre os níveis baixo e alto de 2013 para 2015. No entanto,
# nos anos posteriores a distância cresce, o que indica aumento de desigualdade
# no desempenho escolar. Essa evidência acompanha a tendência e aumento da
# desigualdade de renda no período (SOARES, SOUZA, BARBOSA, 2021; SCALON et al. 2020)

# Seguindo a escala de proficiência do Saeb, 20 pontos na avaliação é equivalente
# a um ano escolar em aprendizado cognitivo. Ainda que o desempenho médio aqui
# mensurado não possibilite a comparação exata na escala de proficiência, podemos
# estimar a distância em habilidades seguindo a escala do Saeb. Assim, temos
# uma distância média de 2 anos de escolarização entre os grupos com baixo e alto
# nível socioeconômico.

dif_meds.MT <- data.frame(Baixo = NA, Médio = NA, Alto = NA)

for(i in 1:length(ano)) { 
  dif_meds.MT[i,2] <- median(my_saeb[[i]]$MEDIA_9EF_MT[my_saeb[[i]]$INSE == "Medio"]) - median(
    my_saeb[[i]]$MEDIA_9EF_MT[my_saeb[[i]]$INSE == "Baixo"])
  dif_meds.MT[i,3] <- median(my_saeb[[i]]$MEDIA_9EF_MT[my_saeb[[i]]$INSE == "Alto"]) - median(
    my_saeb[[i]]$MEDIA_9EF_MT[my_saeb[[i]]$INSE == "Baixo"])
}
dif_meds.MT

# Distribuição pontos percentis (pp) da distribuição de proficiência ==========

pp_LP <- list(
                     s13 = data.frame(Baixo = rep(NA,100), Médio = rep(NA,100), Alto = rep(NA,100)),
                     s15 = data.frame(Baixo = rep(NA,100), Médio = rep(NA,100), Alto = rep(NA,100)),
                     s17 = data.frame(Baixo = rep(NA,100), Médio = rep(NA,100), Alto = rep(NA,100)),
                     s19 = data.frame(Baixo = rep(NA,100), Médio = rep(NA,100), Alto = rep(NA,100)))

pp_MT <- list(
  s13 = data.frame(Baixo = rep(NA,100), Médio = rep(NA,100), Alto = rep(NA,100)),
  s15 = data.frame(Baixo = rep(NA,100), Médio = rep(NA,100), Alto = rep(NA,100)),
  s17 = data.frame(Baixo = rep(NA,100), Médio = rep(NA,100), Alto = rep(NA,100)),
  s19 = data.frame(Baixo = rep(NA,100), Médio = rep(NA,100), Alto = rep(NA,100)))

for(i in 1:length(ano)) { 
  baixo <- subset(my_saeb[[i]], subset = (INSE == "Baixo"))
  medio <- subset(my_saeb[[i]], subset = (INSE == "Medio"))
  alto  <- subset(my_saeb[[i]], subset = (INSE == "Alto"))
  # Lingua Portuguesa
  pp_LP[[i]][,1] <- quantile(baixo$MEDIA_9EF_LP, probs = seq(0.01,1,0.01))
  pp_LP[[i]][,2] <- quantile(medio$MEDIA_9EF_LP, probs = seq(0.01,1,0.01))
  pp_LP[[i]][,3] <- quantile(alto$MEDIA_9EF_LP,  probs = seq(0.01,1,0.01))
  # Matematica
  pp_MT[[i]][,1] <- quantile(baixo$MEDIA_9EF_MT, probs = seq(0.01,1,0.01))
  pp_MT[[i]][,2] <- quantile(medio$MEDIA_9EF_MT, probs = seq(0.01,1,0.01))
  pp_MT[[i]][,3] <- quantile(alto$MEDIA_9EF_MT,  probs = seq(0.01,1,0.01))
}

qqdif <- list(
              s13 = data.frame(baixo_medioLP = rep(NA, 100), baixo_altoLP = rep(NA, 100),baixo_medioMT = rep(NA, 100), baixo_altoMT = rep(NA, 100)),
              s15 = data.frame(baixo_medioLP = rep(NA, 100), baixo_altoLP = rep(NA, 100),baixo_medioMT = rep(NA, 100), baixo_altoMT = rep(NA, 100)),
              s17 = data.frame(baixo_medioLP = rep(NA, 100), baixo_altoLP = rep(NA, 100),baixo_medioMT = rep(NA, 100), baixo_altoMT = rep(NA, 100)),
              s19 = data.frame(baixo_medioLP = rep(NA, 100), baixo_altoLP = rep(NA, 100),baixo_medioMT = rep(NA, 100), baixo_altoMT = rep(NA, 100)))
pltsLP <- list(s13 = NA, s15 = NA, s17 = NA, s19 = NA)
pltsMT <- list(s13 = NA, s15 = NA, s17 = NA, s19 = NA)


for(i in 1:length(ano)) {
qqdif[[i]][,1] <- pp_LP[[i]][,2] - pp_LP[[i]][,1]

qqdif[[i]][,2] <- pp_LP[[i]][,3] - pp_LP[[i]][,1]

qqdif[[i]][,3] <- pp_MT[[i]][,2] - pp_MT[[i]][,1]

qqdif[[i]][,4] <- pp_MT[[i]][,3] - pp_MT[[i]][,1]}

for(i in 1:length(ano)) {
pltsLP[[i]] <- qqdif[[i]] %>% ggplot(aes(x = seq(1,100,1)))+
  theme_bw()+
  geom_point(y = qqdif[[i]]$baixo_medioLP, colour = "blue")+
  geom_point(y = qqdif[[i]]$baixo_altoLP, colour = "red")+
  geom_hline(yintercept = 0, colour = "steelblue")+
  scale_y_continuous(limits = c(-20,60), name = "diferença")+
  scale_x_continuous(breaks = seq(0,100,20), name = "quantis")+
  labs(title=ano[i])
}

for(i in 1:length(ano)) {
  pltsMT[[i]] <- qqdif[[i]] %>% ggplot(aes(x = seq(1,100,1)))+
    theme_bw()+
    geom_point(y = qqdif[[i]]$baixo_medioMT, colour = "blue")+
    geom_point(y = qqdif[[i]]$baixo_altoMT, colour = "red")+
    geom_hline(yintercept = 0, colour = "steelblue")+
    scale_y_continuous(limits = c(-20,60), name = "diferença")+
    scale_x_continuous(breaks = seq(0,100,20), name = "quantis")+
    labs(title=ano[i])
    
  }

plot_grid(pltsLP[[1]], pltsLP[[2]], pltsLP[[3]], pltsLP[[4]],labels="AUTO")
