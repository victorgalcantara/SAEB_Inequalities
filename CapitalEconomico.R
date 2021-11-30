# Indicador de Capital Economico 2013-2019 - Rio de Janeiro
# Author: Victor Alcantara (PPGSA/UFRJ)
# Date: 15.05.21

# 0. Packages and Setup ======================================================

library(tidyverse)

# Working directory
#wd <- ("C:/Users/Victor/estatistica/dados/EDUCACAO/")
wd <- ("C:/Users/Celinha/Desktop/DADOS EDUCACAO/")

# 1. Openning data ===========================================================

ano = seq(from=2011,to=2019,by=2) # biênios de interesse

saeb <- list(s11 = data.frame(), s13 = data.frame(), s15 = data.frame(),
             s17 = data.frame(),s19 = data.frame())

for( i in c(2:4) ) {
  saeb[[i]] <- read_csv(paste0(wd,"EDUCACAO/SAEB/",ano[i],"/DADOS/TS_ALUNO_9EF.csv")) %>%  
    
    select(
      ID_ESCOLA, 
      ID_UF,ID_MUNICIPIO,
      TX_RESP_Q005, # TV em cores
      TX_RESP_Q012, # Carro
      TX_RESP_Q013, # Computador
      TX_RESP_Q014, # Banheiro 
      TX_RESP_Q015, # quarto                
      TX_RESP_Q017, # empregado doméstico 
    ) 
}

load("discr_itens.RDS")

# 2. Data management ----------------------------------------------------------

my_saeb <- list(s11 = data.frame(), s13 = data.frame(), s15 = data.frame(),
                s17 = data.frame(),s19 = data.frame())

for( i in c(2:4) ) {
  my_saeb[[i]] <- saeb[[i]] %>% mutate(., 
                                       carr = case_when(
                                         
                                         TX_RESP_Q012 == "A" ~ 0,
                                         TX_RESP_Q012 == "B" ~ 1,
                                         TX_RESP_Q012 == "C" ~ 2,
                                         TX_RESP_Q012 == "D" ~ 3,
                                         TX_RESP_Q012 == "E" ~ 3
                                       ),
                                       comp = case_when(
                                         
                                         TX_RESP_Q013 == "A" ~ 0,
                                         TX_RESP_Q013 == "B" ~ 1,
                                         TX_RESP_Q013 == "C" ~ 2,
                                         TX_RESP_Q013 == "D" ~ 3,
                                         TX_RESP_Q013 == "E" ~ 3
                                       ),
                                       banh = case_when(
                                         
                                         TX_RESP_Q014 == "A" ~ 0,
                                         TX_RESP_Q014 == "B" ~ 1,
                                         TX_RESP_Q014 == "C" ~ 2,
                                         TX_RESP_Q014 == "D" ~ 3,
                                         TX_RESP_Q014 == "E" ~ 3
                                       ),
                                       quar = case_when(
                                         
                                         TX_RESP_Q015 == "A" ~ 0,
                                         TX_RESP_Q015 == "B" ~ 1,
                                         TX_RESP_Q015 == "C" ~ 2,
                                         TX_RESP_Q015 == "D" ~ 3,
                                         TX_RESP_Q015 == "E" ~ 3
                                       ),
                                       empd = case_when(
                                         
                                         TX_RESP_Q017 == "A" ~ 0,
                                         TX_RESP_Q017 == "B" ~ 1,
                                         TX_RESP_Q017 == "C" ~ 2,
                                         TX_RESP_Q017 == "D" ~ 3,
                                         TX_RESP_Q017 == "E" ~ 3,
                                       )
  ) }

# 3. Indicador de Nivel Socioeconomico (INSE) --------------------------------

# média de aquisição dos itens ponderada pelo poder de discriminação
for(i in c(2:4)) {
  my_saeb[[i]] <- my_saeb[[i]] %>% mutate(., 
                                          CE =
                                            (   
                                              carr  * discr.itens[1,"discr"]  +
                                              comp  * discr.itens[2,"discr"]  +
                                              banh  * discr.itens[3,"discr"]  +
                                              quar  * discr.itens[4,"discr"]  +
                                              empd  * discr.itens[5,"discr"] ) / sum(discr.itens[2:6,1]),
  )}

for(i in 2:4) {
  my_saeb[[i]] <- my_saeb[[i]] %>%  
    mutate(CE2 = case_when(CE > 0 & CE < 1    ~ "Baixo",
                           CE >= 1 & CE < 2 ~ "Medio",
                           CE >= 2            ~ "Alto"
    ) )
}

# 4. Gráficos ------------------------------------------------------------------

par(mfrow=c(1,2))
hist(my_saeb[[2]]$CE, main = "Distribuição das médias", xlab = "Média de itens por aluno",ylab="Frequência")
fq.CE <- table(my_saeb[[2]]$CE2, useNA = "ifany")
barplot(fq.CE, names.arg = c("Alto","Baixo","Médio","Missing"),
        ylim = c(0,1600000),
        main = "Categorias",ylab = "frequency",xlab="Níveis")

hist(my_saeb[[3]]$CE, main = "Distribuição das médias", xlab = "Média de itens por aluno",ylab="Frequência")
fq.CE <- table(my_saeb[[3]]$CE2, useNA = "ifany")
barplot(fq.CE, names.arg = c("Alto","Baixo","Médio","Missing"),
        ylim = c(0,1600000),
        main = "Categorias",ylab = "frequency",xlab="Níveis")

hist(my_saeb[[4]]$CE, main = "Distribuição das médias", xlab = "Média de itens por aluno",ylab="Frequência")
fq.CE <- table(my_saeb[[4]]$CE2, useNA = "ifany")
barplot(fq.CE, names.arg = c("Alto","Baixo","Médio","Missing"),
        ylim = c(0,1600000),
        main = "Categorias",ylab = "frequency",xlab="Níveis")

# Salvando as variáveis de Capital Econômico em um objeto ----------------------
# CE  = variável de 0-3
# CE2 = variável de 0-10
# CE3 = variável categória ordinal (Baixo,Médio,Alto)

n.13 = nrow(my_saeb[[2]])
n.15 = nrow(my_saeb[[3]])
n.17 = nrow(my_saeb[[4]]) 

capitalEconomico <- list(s11 = data.frame(ID_ESCOLA = rep(NA,1), ID_UF = rep(NA,1), 
                                          CE = rep(NA,1), 
                                          CE2 = rep(NA,1)),
                         s13 = data.frame(ID_ESCOLA = rep(NA, n.13), ID_UF = rep(NA, n.13),ID_MUNICIPIO = rep(NA, n.13),
                                          CE  = rep(NA, n.13), 
                                          CE2 = rep(NA, n.13)),
                         s15 = data.frame(ID_ESCOLA = rep(NA,n.15), ID_UF = rep(NA,n.15),ID_MUNICIPIO = rep(NA, n.15), 
                                          CE  = rep(NA,n.15), 
                                          CE2 = rep(NA,n.15)),
                         s17 = data.frame(ID_ESCOLA = rep(NA,n.17), ID_UF = rep(NA,n.17),ID_MUNICIPIO = rep(NA, n.17), 
                                          CE  = rep(NA,n.17), 
                                          CE2 = rep(NA,n.17) ) )
for(i in 2:4) {
  capitalEconomico[[i]]$ID_ESCOLA         <- my_saeb[[i]]$ID_ESCOLA
  capitalEconomico[[i]]$ID_UF             <- my_saeb[[i]]$ID_UF
  capitalEconomico[[i]]$ID_MUNICIPIO      <- my_saeb[[i]]$ID_MUNICIPIO
  
  capitalEconomico[[i]]$CE   <- my_saeb[[i]]$CE
  capitalEconomico[[i]]$CE2  <- my_saeb[[i]]$CE2
}

save(x = capitalEconomico, file = "capitalEconomico.RDS")
write.(x = capitalEconomico, file = "capitalEconomico.csv")
