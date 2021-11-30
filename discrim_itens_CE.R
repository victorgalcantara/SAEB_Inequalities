# Indicador de Capital Economico 2013-2019 - Rio de Janeiro
# Author: Victor Alcantara (PPGSA/UFRJ)
# Date: 15.05.21

# 0. Packages and Setup ======================================================

library(tidyverse)
library(readxl)
library(rio)

# 1. Openning data ===========================================================
# Diretório com as bases de dados que iremos trabalhar
wd <- "C:/Users/VictorGabriel/documents/00_dados/EDUCACAO/"
wd <- "E:/VGA/dados/"
setwd(wd)

data <- read_xlsx(paste0(wd,"ATLAS DESIG/ATLAS_2013.xlsx"),sheet = 2)

saeb <- read_csv(paste0(wd,"EDUCACAO/SAEB/2013/DADOS/TS_ALUNO_9EF.csv")) %>%  
    
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

load("capitalEconomico.RDS")

# 2. Data management ----------------------------------------------------------

# 2.1 Selecionando variáveis de interesse da base do Atlas da Desigualdade
# Variáveis: IDHM,IDHM_R,GINI,ESPVIDA,E_ANOSESTUDO,RDPC,RDPC4

atlas_muni <- data %>% select(Codmun6,Codmun7,IDHM,IDHM_R,GINI,ESPVIDA,E_ANOSESTUDO,RDPC,RDPC4)
atlas_muni <- atlas_muni[order(atlas_muni$IDHM, decreasing = T),]
atlas_muni <- atlas_muni[!duplicated(atlas_muni$Codmun6),] # rm duplicações

# 2.1.1 Testando constructo IDHM
reg<-lm(data=atlas_muni,IDHM ~ RDPC4)
summary(reg)

reg<-lm(data=atlas_muni,IDHM ~ E_ANOSESTUDO)
summary(reg)

reg<-lm(data=atlas_muni,IDHM ~ ESPVIDA)
summary(reg)

# 2.2 Selecionando os 5 muni mais pobres e os cinco mais ricos
# Critério: IDHM
muni <-head(atlas_muni,5)
tmuni <-tail(atlas_muni,5)
muni  <-bind_rows(muni,tmuni)

muni$nvl_idhm <- c(rep("alto",5),rep("baixo",5)) # Id muni por categor IDHM
cod_muni <- muni$Codmun7

# Filtrando SAEB alunos por muni ricos e pobres

mysaeb <- saeb[[2]] %>% filter(ID_MUNICIPIO %in% cod_muni) # filtro

mysaeb <- merge(mysaeb,muni,by.x="ID_MUNICIPIO",by.y="Codmun7")

# 3. Passo 1: Verificando discriminação dos itens

# filtro por IDHM
a <- mysaeb %>% filter(nvl_idhm=="alto")
b <- mysaeb %>% filter(nvl_idhm=="baixo")

itens <- c("TX_RESP_Q005","TX_RESP_Q012","TX_RESP_Q013","TX_RESP_Q014","TX_RESP_Q015",
           "TX_RESP_Q017")
itens.name <- c("tvco","carr","comp","banh","quar","empd")

discr.itens <- data.frame(discr = rep(NA,length(itens)),item=itens,item_name=itens.name)

for(i in 1:length(itens)){
# proporção de posse do item
a_posse_item  <- prop.table(table(a[,itens[i]]))
b_posse_item  <- prop.table(table(b[,itens[i]]))

a_posse_item_df <- as.data.frame(a_posse_item)
b_posse_item_df <- as.data.frame(b_posse_item)

alt <- c("A","B","C","D","E")
a_pi <- rep(NULL,length(alt))
b_pi <- rep(NULL,length(alt))
for(j in 1:5){
  a_pi[j] <- ifelse(a_posse_item_df[j,"Var1"] == alt[j],a_posse_item_df[j,2],0)
  b_pi[j] <- ifelse(b_posse_item_df[j,"Var1"] == alt[j],b_posse_item_df[j,2],0)
  
  a_pi[j] <- ifelse(is.na(a_pi[j]),0,a_pi[j])
  b_pi[j] <- ifelse(is.na(b_pi[j]),0,b_pi[j])
  }

# discriminação do item
discr.itens[i,"discr"] <- sum( abs(a_pi - b_pi) )
}

save(x=discr.itens,file = "discr_itens.RDS")
write.csv(x=discr.itens,file = "discr_itens.csv")

# 4. Testando CE: correlação entre IHDM e CE

d <- capitalEconomico[[3]] %>% group_by(ID_MUNICIPIO) %>% summarise(
  me_ce = mean(CE,na.rm=T) )

my.data <- merge(atlas_muni,d,by.x = "Codmun7",by.y = "ID_MUNICIPIO")

cor(x=my.data$IDHM,y=my.data$me_ce)
plot(my.data$IDHM,my.data$me_ce)

cor = 0.8459096