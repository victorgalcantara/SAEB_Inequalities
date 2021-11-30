# Proficiência alunos Saeb 2013-2019 - Rio de Janeiro
# Author: Victor Alcantara (PPGSA/UFRJ)
# Date: 15.05.21

# 0. Packages and Setup ======================================================

library(tidyverse)
library(readxl)

# 1. Openning data ===========================================================
# Diretório com as bases de dados que iremos trabalhar guardado no objeto 'wd'
wd <- "C:/Users/VictorGabriel/documents/00_dados/EDUCACAO/"
wd <- "E:/VGA/dados/"
setwd(wd)

ano = seq(from=2013,to=2019,by=2) # anos em que Saeb passou a ser censitário

saeb <- list(s13 = data.frame(), s15 = data.frame(), s17 = data.frame(),
             s19 = data.frame())

for(i in c(2:3)) { # 2013-2017
  saeb[[i]] <- read_csv(paste0(wd,"SAEB/",ano[i],"/DADOS/TS_ALUNO_9EF.csv")) %>%  
    select(
           ID_UF, ID_MUNICIPIO,ID_LOCALIZACAO,ID_REGIAO, ID_ESCOLA, 
           PROFICIENCIA_LP_SAEB, PROFICIENCIA_MT_SAEB,
           #ERRO_PADRAO_LP_SAEB,ERRO_PADRAO_MT_SAEB,
           IN_PROVA_BRASIL,IN_PREENCHIMENTO_PROVA,IN_PREENCHIMENTO_QUESTIONARIO,
           TX_RESP_Q001, # Sexo
           TX_RESP_Q002, # Raça
           TX_RESP_Q004, # Idade
           TX_RESP_Q046, # Quando entrou na Escola
           
           TX_RESP_Q048, # ja foi reprovado
           TX_RESP_Q049, # ja abandonou a escola
           
           TX_RESP_Q044, # trabalho doméstico
           TX_RESP_Q045, # Trabalha atualmente
           TX_RESP_Q057  # Pretensão após EF (trabalhar, estudar, os dois)
    )
  gc()
}

for(i in c(4)) { # 2019
  saeb[[i]] <- read_csv(paste0(wd,"SAEB/",ano[i],"/DADOS/TS_ALUNO_9EF.csv")) %>%  
    select(
      ID_UF, ID_MUNICIPIO,ID_LOCALIZACAO,ID_REGIAO, ID_ESCOLA, 
      PROFICIENCIA_LP_SAEB, PROFICIENCIA_MT_SAEB,PROFICIENCIA_CN_SAEB,PROFICIENCIA_CH_SAEB,
      #ERRO_PADRAO_LP_SAEB,ERRO_PADRAO_MT_SAEB,
      #IN_PREENCHIMENTO_LP,IN_PREENCHIMENTO_MT,IN_PREENCHIMENTO_CN,IN_PREENCHIMENTO_CH,
      IN_PREENCHIMENTO_QUESTIONARIO,
      TX_RESP_Q002, # Raça
      TX_RESP_Q004, # Idade
      TX_RESP_Q013, # Quando entrou na Escola
      
      TX_RESP_Q015, # ja foi reprovado
      TX_RESP_Q016, # ja abandonou a escola
      
      #TX_RESP_Q017c,# trabalho doméstico
      #TX_RESP_Q017e,# Trabalha atualmente
      TX_RESP_Q019  # Pretensão após EF (trabalhar, estudar, os dois)
    )
  gc()
}

# Indicadores construidos
load("capitalCultural.RDS")
load("capitalEconomico.RDS")

# Regioes
regioes <- read_excel(paste0(wd,"RELATORIO_DTB_BRASIL_MUNICIPIO.xls")) %>% 
  select(UF, Nome_Mesorregião,Nome_Microrregião,Codigo_Municipio_Completo, Nome_Município) %>% 
  rename(., 
         ID_MUNICIPIO = Codigo_Municipio_Completo,
         ID_UF = UF)

# Random sample to work
# for(i in 2:5){ for(j in 1:4){ 
# randcases    <- sample(c(T,F,F,F,F),size=nrow(saeb[[j]]),replace = T)
# my_saeb[[i]] <- saeb[[j]][randcases,]
# }}
# 
# save(my_saeb,file="mysaeb_sample.RDS")

# 2. Data management ----------------------------------------------------------

my_saeb <- list(s13 = data.frame(), s15 = data.frame(),
                s17 = data.frame(),s19 = data.frame())

for(i in 1:3) {
  my_saeb[[i]] <- saeb[[i]] %>%  mutate(., 
                                           
                                           regiao = case_when(
                                             
                                             ID_REGIAO == 1 ~ "NO",
                                             ID_REGIAO == 2 ~ "NE",
                                             ID_REGIAO == 3 ~ "SE",
                                             ID_REGIAO == 4 ~ "SU",
                                             ID_REGIAO == 5 ~ "CO"
                                           ),  
                                           
                                           TX_RESP_Q020 = case_when(
                                             
                                             ID_LOCALIZACAO == 1 ~ "rural",
                                             ID_LOCALIZACAO == 2 ~ "urbana"
                                           ),
                                           
                                           fluxo = ifelse(TX_RESP_Q048 == "A" & TX_RESP_Q049 == "A","Regular","Irregular"),
                                           
                                           sexo =
                                             case_when(
                                                       TX_RESP_Q001 == "A" ~ "A-Masculino",
                                                       TX_RESP_Q001 == "B" ~ "B-Feminino"),
                                           raca = case_when(
                                             
                                             TX_RESP_Q002 == "A" ~ "branca",
                                             TX_RESP_Q002 == "B" ~ "preta",
                                             TX_RESP_Q002 == "C" ~ "parda",
                                             TX_RESP_Q002 == "D" ~ "amarela",
                                             TX_RESP_Q002 == "E" ~ "indigena",
                                             TX_RESP_Q002 == "F" ~ "ND",
                                           ),
                                           raca2 = case_when(
                                             TX_RESP_Q002 == "A" ~ "Brancos",
                                             TX_RESP_Q002 == "B" ~ "Não brancos",
                                             TX_RESP_Q002 == "C" ~ "Não brancos",
                                             TX_RESP_Q002 == "D" ~ "Brancos",
                                             TX_RESP_Q002 == "E" ~ "Não brancos",
                                             TX_RESP_Q002 == "F" ~ "ND"),
                                           trabFora =
                                             case_when(
                                                       TX_RESP_Q045 == "A" ~ "A-Sim",
                                                       TX_RESP_Q045 == "B" ~ "B-Não"),
                                           
                                           trabDom =
                                             case_when(
                                                       TX_RESP_Q044 == "A" ~ "1-2h",
                                                       TX_RESP_Q044 == "B" ~ "1-2h",
                                                       TX_RESP_Q044 == "C" ~ "3-4h",
                                                       TX_RESP_Q044 == "D" ~ "3-4h",
                                                       TX_RESP_Q044 == "E" ~ "0"),
                                           
) }

# Imputando capitais na base dos alunos
for( i in c(3) ) { for( j in c(4) ) {
  my_saeb[[i]]$CC  <- capitalCultural[[j]]$CC
  my_saeb[[i]]$CE  <- capitalEconomico[[j]]$CE
  
  my_saeb[[i]]$CC2  <- capitalCultural[[j]]$CC2
  my_saeb[[i]]$CE2  <- capitalEconomico[[j]]$CE2
}}

# Somente alunos que preencheram a prova
for(i in 1:3) {
  my_saeb[[i]]   <- my_saeb[[i]] %>% filter(IN_PREENCHIMENTO_PROVA == 1)
}

# Verificando correlação entre variáveis do modelo
# cor(data) = matriz com correlações
#    - diagonal são cor entre a mesma variável = 1
my_matrix <- list(s13 = data.frame(), s15 = data.frame(),
                  s17 = data.frame(),s19 = data.frame())



for(i in 1:3) {
my_matrix[[i]] <- my_saeb[[i]] %>% select(PROFICIENCIA_LP_SAEB,PROFICIENCIA_MT_SAEB,
                                          CC, CE) %>% na.exclude() %>% as.matrix()}
t<-cor(my_matrix[[1]])

# Regressões -------------------------------------------------------------------

regLP <- data.frame(intercepto = NA, CE = NA,
                    pardo = NA, preto = NA, amarelo = NA, indigena = NA, ND = NA,
                    feminino = NA, Regular = NA, trabalha = NA)

regMT <- data.frame(intercepto = NA, CE = NA, CC = NA,
                    pardo = NA, preto = NA, amarelo = NA, indigena = NA, ND = NA,
                    feminino = NA, Regular = NA, trabalha = NA)

for(i in 1) {
  regressao <- lm(PROFICIENCIA_LP_SAEB ~ CE + raca + sexo + fluxo + trabFora, data = my_saeb[[i]])
  
  regLP[i,] <- coef(regressao)
  print(summary(regressao))
}

for(i in 1:3) {
  regressao <- lm(PROFICIENCIA_MT_SAEB ~ CE + raca + sexo + fluxo + trabFora, data = my_saeb[[i]])
  
  regMT[i,] <- coef(regressao)
  print(summary(regressao))
}

# Invertendo a tabela para o formato longo
regressaoLP <- t(regLP)
colnames(regressaoLP) <- c("s13", "s15", "s17")

regressaoMT <- t(regMT)
colnames(regressaoMT) <- c("s13", "s15", "s17")

# Salvando tabelas com o coeficientes da regressão
setwd(wd)
write.csv(regressaoLP, file = "regressaoLP.csv")
write.csv(regressaoMT, file = "regressaoMT.csv")

# Grupos para comparação

MeninosBrancos <- my_saeb[[4]] %>% filter(sexo             == "A-Masculino",
                                          raca             == "A-Branca",
                                          trabalha         == "B-Não",
                                          fluxo            == "Regular",
                                          CE3  == "Alto",
                                          CC3  == "Alto")

MeninosNegros <- my_saeb[[4]] %>% filter(sexo             == "A-Masculino",
                                         raca             == "C-Preta",
                                         trabalha         == "A-Sim",
                                         fluxo            == "Irregular",
                                         CE3  == "Baixo",
                                         CC3  == "Baixo")

MeninasBrancas <- my_saeb[[4]] %>% filter(sexo             == "B-Feminino",
                                          raca             == "A-Branca",
                                          CapitalEconomico == "Alto",
                                          CapitalCultural  == "Alto")

MeninasNegras <- my_saeb[[4]] %>% filter(sexo             == "B-Feminino",
                                         raca             == "C-Preta",
                                         CapitalEconomico == "Baixo",
                                         CapitalCultural  == "Baixo")

prop.table(table(MeninosBrancos$GostaMT))

prop.table(table(MeninasNegras$GostaMT))

mean(MeninasBrancas$PROFICIENCIA_LP_SAEB, na.rm = T)
mean(MeninasNegras$PROFICIENCIA_LP_SAEB, na.rm = T)

prop.table(table(MeninasBrancas$pretensao))
prop.table(table(MeninasNegras$pretensao))


my_saeb[[4]] %>% group_by(sexo) %>% table(.$trabDomestico)

