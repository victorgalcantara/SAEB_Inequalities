# Indicador de Estrutura Escolar
# Author: Victor Alcantara (PPGSA/UFRJ)
# Date: 15.05.21

# 0. Packages and Setup ======================================================

library(tidyverse)

# 1. Openning data ===========================================================
# Diretório com as bases de dados que iremos trabalhar
wd <- ("C:/Users/VictorGabriel/Documents/00_dados/EDUCACAO/")
wd <- ("D:/VGA/dados/EDUCACAO/")

ano = seq(from=2011,to=2019,by=2) # biênios de interesse

saeb.escolas <- list(s11 = data.frame(), s13 = data.frame(), s15 = data.frame(),
             s17 = data.frame(),s19 = data.frame())

for( i in c(2:4) ) {
  saeb.escolas[[i]] <- read_csv(paste0(wd,"SAEB/",ano[i],"/DADOS/TS_ESCOLA.csv")) %>%  
    
    select(
      ID_ESCOLA,
      ID_UF,ID_MUNICIPIO,
      PC_FORMACAO_DOCENTE_FINAL, # Perc. de prof com formação na área em que atua
      NIVEL_SOCIO_ECONOMICO,     # Nvl Socioeconomico medio da Escola
      ID_DEPENDENCIA_ADM,        # Dependência administrativa
      
      TAXA_PARTICIPACAO_9EF,
      IN_PREENCHIMENTO_QUESTIONARIO,
      
      MEDIA_9EF_LP,
      MEDIA_9EF_MT,
      
      TX_RESP_Q013, # salas de aula
      TX_RESP_Q037, # computadores p/ alunos
      TX_RESP_Q038, # acesso a internet p alunos
      TX_RESP_Q045, # impressora
      TX_RESP_Q046, # retroprojetor
      TX_RESP_Q049, # tv
      TX_RESP_Q057, # biblioteca
      TX_RESP_Q058, # sala leitura
      TX_RESP_Q059, # quadra esportiva
      TX_RESP_Q060, # lab informatica
      TX_RESP_Q061, # lab ciencias
      TX_RESP_Q062, # auditorio
      TX_RESP_Q063, # sala musica
      TX_RESP_Q064, # sala artes
    )
}

# 2. Data management ----------------------------------------------------------

mysaeb <- list(s11 = data.frame(), s13 = data.frame(), s15 = data.frame(),
                s17 = data.frame(),s19 = data.frame())

for( i in c(2:4) ) {
  mysaeb[[i]] <- saeb.escolas[[i]] %>% mutate(., 
                                       
                                       salas = case_when(
                                       TX_RESP_Q013 == "A" ~ 3,
                                       TX_RESP_Q013 == "B" ~ 2,
                                       TX_RESP_Q013 == "C" ~ 1,
                                       TX_RESP_Q013 == "D" ~ 0,
                                       ),
                                       
                                       compu = case_when(
                                       TX_RESP_Q037 == "A" ~ 3,
                                       TX_RESP_Q037 == "B" ~ 2,
                                       TX_RESP_Q037 == "C" ~ 1,
                                       TX_RESP_Q037 == "D" ~ 0,
                                       ),
                                       
                                       net = case_when(
                                       TX_RESP_Q038 == "A" ~ 3,
                                       TX_RESP_Q038 == "B" ~ 2,
                                       TX_RESP_Q038 == "C" ~ 1,
                                       TX_RESP_Q038 == "D" ~ 0,
                                       ),
                                       
                                       impres = case_when(
                                       TX_RESP_Q045 == "A" ~ 3,
                                       TX_RESP_Q045 == "B" ~ 2,
                                       TX_RESP_Q045 == "C" ~ 1,
                                       TX_RESP_Q045 == "D" ~ 0,
                                       ),
                                       
                                       proj = case_when(
                                       TX_RESP_Q046 == "A" ~ 3,
                                       TX_RESP_Q046 == "B" ~ 2,
                                       TX_RESP_Q046 == "C" ~ 1,
                                       TX_RESP_Q046 == "D" ~ 0,
                                       ),
                                       
                                       tv = case_when(
                                       TX_RESP_Q049 == "A" ~ 3,
                                       TX_RESP_Q049 == "B" ~ 2,
                                       TX_RESP_Q049 == "C" ~ 1,
                                       TX_RESP_Q049 == "D" ~ 0,
                                       ),
                                       
                                       biblio = case_when(
                                       TX_RESP_Q057 == "A" ~ 3,
                                       TX_RESP_Q057 == "B" ~ 2,
                                       TX_RESP_Q057 == "C" ~ 1,
                                       TX_RESP_Q057 == "D" ~ 0,
                                       ),
                                       
                                       saleit = case_when(
                                       TX_RESP_Q058 == "A" ~ 3,
                                       TX_RESP_Q058 == "B" ~ 2,
                                       TX_RESP_Q058 == "C" ~ 1,
                                       TX_RESP_Q058 == "D" ~ 0,
                                       ),
                                       
                                       quadra = case_when(
                                       TX_RESP_Q059 == "A" ~ 3,
                                       TX_RESP_Q059 == "B" ~ 2,
                                       TX_RESP_Q059 == "C" ~ 1,
                                       TX_RESP_Q059 == "D" ~ 0,
                                       ),
                                       
                                       labinfo = case_when(
                                       TX_RESP_Q060 == "A" ~ 3,
                                       TX_RESP_Q060 == "B" ~ 2,
                                       TX_RESP_Q060 == "C" ~ 1,
                                       TX_RESP_Q060 == "D" ~ 0,
                                       ),
                                       
                                       labcie = case_when(
                                       TX_RESP_Q061 == "A" ~ 3,
                                       TX_RESP_Q061 == "B" ~ 2,
                                       TX_RESP_Q061 == "C" ~ 1,
                                       TX_RESP_Q061 == "D" ~ 0,
                                       ),
                                       
                                       audit = case_when(
                                       TX_RESP_Q062 == "A" ~ 3,
                                       TX_RESP_Q062 == "B" ~ 2,
                                       TX_RESP_Q062 == "C" ~ 1,
                                       TX_RESP_Q062 == "D" ~ 0,
                                       ),
                                       
                                       salmus = case_when(
                                       TX_RESP_Q063 == "A" ~ 3,
                                       TX_RESP_Q063 == "B" ~ 2,
                                       TX_RESP_Q063 == "C" ~ 1,
                                       TX_RESP_Q063 == "D" ~ 0,
                                       ),
                                       
                                       salart = case_when(
                                       TX_RESP_Q064 == "A" ~ 3,
                                       TX_RESP_Q064 == "B" ~ 2,
                                       TX_RESP_Q064 == "C" ~ 1,
                                       TX_RESP_Q064 == "D" ~ 0,
                                       )
  ) } 

# 2.1 Indicador de Estrutura Escolar -------------------------------------------

# Média dos itens
for(i in c(2:4)) {
  mysaeb[[i]] <- mysaeb[[i]] %>% mutate(., 
          estruturaEscola =
          (salas + compu + net + impres+ proj + tv + biblio+ saleit+ labcie+
          labinfo+ salart+ salmus+ audit+ quadra) / 14,
          
          estruturaEscola2 =
          (salas + compu + biblio+ saleit+ labcie+
           labinfo+ salart+ salmus+ audit+ quadra) / 10,
          
          estruturaEscola3 =
          (salas + biblio+ saleit+ labcie +labinfo) / 5
           )                                                  
 }

# Categorias
for(i in c(2:4)) {
  mysaeb[[i]] <- mysaeb[[i]] %>%  
    mutate(
      
      estruturaEscola4 = case_when(
      estruturaEscola >= 0 & estruturaEscola < 1   ~ "Baixo",
      estruturaEscola >= 1 & estruturaEscola < 2   ~ "Medio",
      estruturaEscola >= 2                         ~ "Alto"),
    
      estruturaEscola5 = case_when(
      estruturaEscola < 0.5   ~ "Muito Baixo",
      estruturaEscola >= 0.5 & estruturaEscola < 1     ~ "Baixo",
      estruturaEscola >= 1   & estruturaEscola < 1.5   ~ "Medio Baixo",
      estruturaEscola >= 1.5 & estruturaEscola < 2     ~ "Medio Alto",
      estruturaEscola >= 2   & estruturaEscola < 2.5   ~ "Alto",     
      estruturaEscola >= 2.5  ~ "Muito Alto")
    )
}

n.11 <- 1
n.13 <- nrow(mysaeb[[2]])
n.15 <- nrow(mysaeb[[3]])
n.17 <- nrow(mysaeb[[4]])

estruturaEscola <- list(s11 = data.frame(ID_ESCOLA = rep(NA,n.11), 
                                         ID_UF = rep(NA,n.11),ID_MUNICIPIO=rep(NA,n.11),
                                         PC_FORMACAO_DOCENTE = rep(NA,n.11),
                                         INSE = rep(NA,n.11),
                                         depAdmin = rep(NA,n.11),
                                         MEDIA_9EF_LP=rep(NA,n.11),
                                         MEDIA_9EF_MT=rep(NA,n.11),
                                         estruturaEscola  = rep(NA, n.11), 
                                         estruturaEscola2 = rep(NA, n.11),
                                         estruturaEscola3 = rep(NA, n.11), 
                                         estruturaEscola4 = rep(NA, n.11),
                                         estruturaEscola5 = rep(NA, n.11)
                                         ),
                        
                        s13 = data.frame(ID_ESCOLA = rep(NA,n.13), 
                                         ID_UF = rep(NA,n.13),ID_MUNICIPIO=rep(NA,n.13),
                                         PC_FORMACAO_DOCENTE = rep(NA,n.13),
                                         INSE = rep(NA,n.13),
                                         depAdmin = rep(NA,n.13),
                                         MEDIA_9EF_LP=rep(NA,n.13),
                                         MEDIA_9EF_MT=rep(NA,n.13),
                                         estruturaEscola  = rep(NA, n.13), 
                                         estruturaEscola2 = rep(NA, n.13),
                                         estruturaEscola3 = rep(NA, n.13), 
                                         estruturaEscola4 = rep(NA, n.13),
                                         estruturaEscola5 = rep(NA, n.13)
                                         ),
                        sn.15 = data.frame(ID_ESCOLA = rep(NA,n.15), 
                                         ID_UF = rep(NA,n.15),ID_MUNICIPIO=rep(NA,n.15),
                                         PC_FORMACAO_DOCENTE = rep(NA,n.15),
                                         INSE = rep(NA,n.15),
                                         depAdmin = rep(NA,n.15),
                                         MEDIA_9EF_LP=rep(NA,n.15),
                                         MEDIA_9EF_MT=rep(NA,n.15),
                                         estruturaEscola = rep(NA, n.15), 
                                         estruturaEscola2 = rep(NA, n.15),
                                         estruturaEscola3 = rep(NA, n.15), 
                                         estruturaEscola4 = rep(NA, n.15),
                                         estruturaEscola5 = rep(NA, n.15) 
                                         ),
                        sn.17 = data.frame(ID_ESCOLA = rep(NA,n.17), 
                                         ID_UF = rep(NA,n.17),ID_MUNICIPIO=rep(NA,n.17),
                                         PC_FORMACAO_DOCENTE = rep(NA,n.17),
                                         INSE = rep(NA,n.17),
                                         depAdmin = rep(NA,n.17),
                                         MEDIA_9EF_LP=rep(NA,n.17),
                                         MEDIA_9EF_MT=rep(NA,n.17),
                                         estruturaEscola  = rep(NA, n.17), 
                                         estruturaEscola2 = rep(NA, n.17),
                                         estruturaEscola3 = rep(NA, n.17), 
                                         estruturaEscola4 = rep(NA, n.17),
                                         estruturaEscola5 = rep(NA, n.17) 
                                         )
                        )

for(i in c(2:4)) {
  estruturaEscola[[i]]$ID_ESCOLA           <- mysaeb[[i]]$ID_ESCOLA
  estruturaEscola[[i]]$ID_UF               <- mysaeb[[i]]$ID_UF
  estruturaEscola[[i]]$ID_MUNICIPIO        <- mysaeb[[i]]$ID_MUNICIPIO
  
  estruturaEscola[[i]]$PC_FORMACAO_DOCENTE <- mysaeb[[i]]$PC_FORMACAO_DOCENTE_FINAL
  estruturaEscola[[i]]$INSE                <- mysaeb[[i]]$NIVEL_SOCIO_ECONOMICO
  estruturaEscola[[i]]$depAdmin            <- mysaeb[[i]]$ID_DEPENDENCIA_ADM
  estruturaEscola[[i]]$MEDIA_9EF_LP        <- mysaeb[[i]]$MEDIA_9EF_LP
  estruturaEscola[[i]]$MEDIA_9EF_MT        <- mysaeb[[i]]$MEDIA_9EF_MT
  estruturaEscola[[i]]$estruturaEscola     <- mysaeb[[i]]$estruturaEscola
  estruturaEscola[[i]]$estruturaEscola2    <- mysaeb[[i]]$estruturaEscola2
  estruturaEscola[[i]]$estruturaEscola3    <- mysaeb[[i]]$estruturaEscola3
  estruturaEscola[[i]]$estruturaEscola4    <- mysaeb[[i]]$estruturaEscola4
  estruturaEscola[[i]]$estruturaEscola5    <- mysaeb[[i]]$estruturaEscola5
}

setwd(wd)
save(x = estruturaEscola, file = "estruturaEscola13-17.RDS")
load("estruturaEscola13-17.RDS")
