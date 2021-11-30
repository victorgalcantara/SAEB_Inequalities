# Indicador de Capital Cultural 2013-2019 - Rio de Janeiro
# Author: Victor Alcantara (PPGSA/UFRJ)
# Date: 15.05.21

# 0. Packages and Setup ======================================================

library(tidyverse)

# 1. Openning data ===========================================================
# Diretório com as bases de dados que iremos trabalhar
#wd <- ("C:/Users/Victor/estatistica/dados/EDUCACAO/")
wd <- "E:/VGA/dados/EDUCACAO/"
setwd(wd)

ano = seq(from=2011,to=2019,by=2) # biênios de interesse

saeb <- list(s11 = data.frame(), s13 = data.frame(), s15 = data.frame(),
             s17 = data.frame(),s19 = data.frame())

for( i in c(2:4) ) {
  saeb[[i]] <- read_csv(paste0(wd,"SAEB/",ano[i],"/DADOS/TS_ALUNO_9EF.csv")) %>%  
    
    select(
      ID_ESCOLA,
      ID_UF,ID_MUNICIPIO,
      TX_RESP_Q032, # jornal
      #TX_RESP_Q033, # livros geral
      TX_RESP_Q034, # livros literatura
      #TX_RESP_Q035, # revistas
      #TX_RESP_Q036, # HQ
      #TX_RESP_Q037, # revistas
      #TX_RESP_Q038, # noticias internet
      TX_RESP_Q039, # biblioteca
      TX_RESP_Q040, # cinema
      TX_RESP_Q041, # espetáculo/exposição
      #TX_RESP_Q042, # festas
      TX_RESP_Q019, # educMae
      TX_RESP_Q023, # educPai
    )
}

for( i in c(1) ) {
  saeb[[i]] <- read_delim(paste0(wd,"SAEB/",ano[i],"/DADOS/TS_ALUNO_9EF.csv"), delim = ";") %>%  
    
    select(
      ID_ESCOLA,
      ID_SERIE,
      TX_RESP_Q032, # jornal
      #TX_RESP_Q033, # livros geral
      TX_RESP_Q034, # livros literatura
      #TX_RESP_Q035, # revistas
      #TX_RESP_Q036, # HQ
      #TX_RESP_Q037, # revistas
      #TX_RESP_Q038, # noticias internet
      TX_RESP_Q039, # biblioteca
      TX_RESP_Q040, # cinema
      TX_RESP_Q041, # espetáculo/exposição
      #TX_RESP_Q042, # festas
      )  %>% 
    filter(ID_SERIE == 9)
}

# 2. Data management ----------------------------------------------------------

my_saeb <- list(s11 = data.frame(), s13 = data.frame(), s15 = data.frame(),
                s17 = data.frame(),s19 = data.frame())

for( i in c(2:4) ) {
  my_saeb[[i]] <- saeb[[i]] %>% mutate(., 
                                       
                                       jornal = case_when(

                                         TX_RESP_Q032 == "A" ~ 3,
                                         TX_RESP_Q032 == "B" ~ 2,
                                         TX_RESP_Q032 == "C" ~ 1
                                       ),

                                       livrosLiterat = case_when(
                                         TX_RESP_Q034 == "A" ~ 3,
                                         TX_RESP_Q034 == "B" ~ 2,
                                         TX_RESP_Q034 == "C" ~ 1
                                       ),

                                       biblioteca = case_when(
                                         TX_RESP_Q039 == "A" ~ 3,
                                         TX_RESP_Q039 == "B" ~ 2,
                                         TX_RESP_Q039 == "C" ~ 1
                                       ),


                                       cinema = case_when(
                                         TX_RESP_Q040 == "A" ~ 3,
                                         TX_RESP_Q040 == "B" ~ 2,
                                         TX_RESP_Q040 == "C" ~ 1
                                       ),


                                       espetaculo = case_when(
                                         TX_RESP_Q041 == "A" ~ 3,
                                         TX_RESP_Q041 == "B" ~ 2,
                                         TX_RESP_Q041 == "C" ~ 1
                                       ),
                                      
                                       educMae = 
                                         case_when(
                                           TX_RESP_Q019 == "A" ~ 0,
                                           TX_RESP_Q019 == "B" ~ 1,
                                           TX_RESP_Q019 == "C" ~ 1,
                                           TX_RESP_Q019 == "D" ~ 2,
                                           TX_RESP_Q019 == "E" ~ 2,
                                           TX_RESP_Q019 == "F" ~ 3,
                                           TX_RESP_Q019 == "G" ~ 9),
                                       
                                       educPai = 
                                         case_when(
                                           TX_RESP_Q023 == "A" ~ 0,
                                           TX_RESP_Q023 == "B" ~ 1,
                                           TX_RESP_Q023 == "C" ~ 1,
                                           TX_RESP_Q023 == "D" ~ 2,
                                           TX_RESP_Q023 == "E" ~ 2,
                                           TX_RESP_Q023 == "F" ~ 3,
                                           TX_RESP_Q023 == "G" ~ 9),
                                       educMae = ifelse(educMae == 9,"NS",educMae),
                                       educPai = ifelse(educPai == 9,"NS",educPai)
  )
}

# 2.1 Indicador de Capital Cultural -------------------------------------------

# -----------------------------------------------------------------------------------------

for(i in c(2:4)) {
  my_saeb[[i]] <- my_saeb[[i]] %>% mutate(., 
                                          educMae = ifelse(educMae == "NS", NA, educMae),
                                          educPai = ifelse(educPai == "NS", NA, educPai),
                                          educMae = as.numeric(educMae),
                                          educPai = as.numeric(educPai),
                                          CC =
                                              ( jornal        +
                                                livrosLiterat +
                                                biblioteca    +
                                                cinema        +
                                                espetaculo    +
                                                educMae       +
                                                educPai   
                                                ) / 7

  ) }

for(i in 2:4) {
  my_saeb[[i]] <- my_saeb[[i]] %>%  
    mutate(CC2 = case_when(CC < 1.5                 ~ "Baixo",
                           CC >= 1.5 & CC <= 2       ~ "Medio",
                           CC > 2                  ~ "Alto"
    ) )
}

# Gráfico ---------------------------------------------------------------------

par(mfrow=c(1,2))
hist(my_saeb[[2]]$CC, main = "Distribuição das médias", xlab = "Média de itens por aluno",ylab="Frequência")
fq.CC <- table(my_saeb[[2]]$CC2, useNA = "ifany")
barplot(fq.CC, names.arg = c("Alto","Baixo","Médio","Missing"),
        ylim = c(0,1600000),
        main = "Categorias",ylab = "frequency",xlab="Níveis")

hist(my_saeb[[3]]$CC, main = "Distribuição das médias", xlab = "Média de itens por aluno",ylab="Frequência")
fq.CC <- table(my_saeb[[3]]$CC2, useNA = "ifany")
barplot(fq.CC, names.arg = c("Alto","Baixo","Médio","Missing"),
        ylim = c(0,1600000),
        main = "Categorias",ylab = "frequency",xlab="Níveis")

hist(my_saeb[[4]]$CC, main = "Distribuição das médias", xlab = "Média de itens por aluno",ylab="Frequência")
fq.CC <- table(my_saeb[[4]]$CC2, useNA = "ifany")
barplot(fq.CC, names.arg = c("Alto","Baixo","Médio","Missing"),
        ylim = c(0,1600000),
        main = "Categorias",ylab = "frequency",xlab="Níveis")

# Salvando as variáveis de Capital Econômico em um objeto
# CE  = variável de 0-3
# CE2 = variável de 0-10
# CE3 = variável categória ordinal (Baixo,Médio,Alto)

n.13 = nrow(my_saeb[[2]])
n.15 = nrow(my_saeb[[3]])
n.17 = nrow(my_saeb[[4]]) 

capitalCultural <- list(s11 = data.frame(ID_ESCOLA = rep(NA,1), ID_UF = rep(NA,1), 
                                          CC = rep(NA,1), 
                                          CC2 = rep(NA,1)),
                         s13 = data.frame(ID_ESCOLA = rep(NA, n.13), ID_UF = rep(NA, n.13),ID_MUNICIPIO = rep(NA,n.13), 
                                          CC  = rep(NA, n.13), 
                                          CC2 = rep(NA, n.13)),
                         s15 = data.frame(ID_ESCOLA = rep(NA,n.15), ID_UF = rep(NA,n.15),ID_MUNICIPIO = rep(NA,n.15), 
                                          CC  = rep(NA,n.15), 
                                          CC2 = rep(NA,n.15)),
                         s17 = data.frame(ID_ESCOLA = rep(NA,n.17), ID_UF = rep(NA,n.17),ID_MUNICIPIO = rep(NA,n.17), 
                                          CC  = rep(NA,n.17), 
                                          CC2 = rep(NA,n.17) ) )

for(i in 2:4) {
  capitalCultural[[i]]$ID_ESCOLA         <- my_saeb[[i]]$ID_ESCOLA
  capitalCultural[[i]]$ID_UF             <- my_saeb[[i]]$ID_UF
  capitalCultural[[i]]$ID_MUNICIPIO      <- my_saeb[[i]]$ID_MUNICIPIO
  capitalCultural[[i]]$CC   <- my_saeb[[i]]$CC
  capitalCultural[[i]]$CC2  <- my_saeb[[i]]$CC2
}

setwd("E:/VGA/dados/")
save(x = capitalCultural, file = "capitalCultural.RDS")