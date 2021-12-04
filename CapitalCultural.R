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
      ID_ALUNO, ID_ESCOLA, ID_TURMA,
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

# Verificando missing data por item
itens <- c("jornal","livrosLiterat","biblioteca","cinema","espetaculo","educMae","educPai")

missing_itens <- data.frame("1" = rep(NA,5),"2" = rep(NA,length(ano)),"3" = rep(NA,length(ano)),"NA" = rep(NA,length(ano)) )
missing_educ  <- data.frame("0" = rep(NA,2),"1" = rep(NA,2),"2" = rep(NA,2),"3" = rep(NA,2),
                            "NS" = rep(NA,2),"NA" = rep(NA,2) )
missing_i <- list(s11 = missing_itens, s13 = missing_itens, s15 = missing_itens,
                  s17 = missing_itens, s19 = missing_itens)
missing_e <- list(s11 = missing_educ, s13 = missing_educ, s15 = missing_educ,
                  s17 = missing_educ, s19 = missing_educ)

for(i in 2:4) { for(j in 1:5) {
  print(i)
  missing_i[[i]][j,] <- prop.table(table(my_saeb[[i]][,itens[j]],useNA="ifany"))
}}

for(i in 2:4) { for(j in 1:2) {
  print(i)
  missing_i[[i]][j,] <- prop.table(table(my_saeb[[i]][,itens[j]],useNA="ifany"))
}}

# Construção do indicador

# 1. Considerando todos os itens (incluso educ mãe e pai)
for(i in c(2:4)) {
  my_saeb[[i]] <- my_saeb[[i]] %>% mutate(., 
                                          educMae = ifelse(educMae == "NS", NA, educMae),
                                          educPai = ifelse(educPai == "NS", NA, educPai),
                                          educMae = as.numeric(educMae),
                                          educPai = as.numeric(educPai),
                                          CC0 =
                                              ( jornal        +
                                                livrosLiterat +
                                                biblioteca    +
                                                cinema        +
                                                espetaculo    +
                                                educMae       +
                                                educPai   
                                                ) / 7

  ) }

# Considerando todos os itens, exCCto educ pai e mãe
for(i in c(2:4)) {
  my_saeb[[i]] <- my_saeb[[i]] %>% mutate(., 
                                          CC =
                                            ( 
                                                jornal        +
                                                livrosLiterat +
                                                biblioteca    +
                                                cinema        +
                                                espetaculo    
                                            ) / 5
                                          
  ) }

# Considerando apenas itens com menor missing: biblioteca e cinema
for(i in c(2:4)) {
  my_saeb[[i]] <- my_saeb[[i]] %>% mutate(., 
                                          CC2 =
                                            ( 
                                                biblioteca    +
                                                cinema        +
                                                espetaculo
                                            ) / 3
                                          
  ) }

# Observando distribuição das vars para definir níveis por categorias

# hist
hist(my_saeb[[2]]$CC0)
hist(my_saeb[[2]]$CC)
hist(my_saeb[[2]]$CC2)

for(i in 2:4) {
  my_saeb[[i]] <- my_saeb[[i]] %>%  
    mutate(CCcat = case_when(
                           CC < 1.5             ~ "Baixo",
                           CC >= 1.5 & CC <= 2  ~ "Medio",
                           CC > 2               ~ "Alto"
    ) )
}

for(i in 2:4) {
  my_saeb[[i]] <- my_saeb[[i]] %>%  
    mutate(CCcat2 = case_when(
                             CC <  0.5            ~ "Baixo",
                             CC >= 0.5 & CC < 1.8 ~ "Medio Baixo",
                             CC >= 1.8 & CC < 2.4 ~ "Medio",
                             CC >= 2.4 & CC < 2.8 ~ "Medio Alto",
                             CC >= 2.8            ~ "Alto"
                             
    ) )
}

# Gráfico ---------------------------------------------------------------------

# Correlação entre CC e CC2
saeb <- my_saeb[[2]] %>% na.exclude(my_saeb[[2]]$CC)
cor(saeb$CC2,saeb$CC)

data <- data.frame()
data <- my_saeb[[2]] %>% na.exclude(my_saeb[[2]]$CC2) %>% group_by(CC2) %>% summarise(me_cc = mean(CC,na.rm=T))

plot(x=data$CC2,y=data$me_cc)
cor(data$CC2,data$me_cc)

# Distribuições dos indicadores em versão métrica e categórica

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
# CC  = variável de 0-3
# CC2 = variável de 0-10
# CC3 = variável categória ordinal (Baixo,Médio,Alto)

n.13 = nrow(my_saeb[[2]])
n.15 = nrow(my_saeb[[3]])
n.17 = nrow(my_saeb[[4]]) 

capitalCultural <- list(s11 = data.frame(ID_ALUNO = rep(NA,1), ID_UF = rep(NA,1), 
                                         CC0 = rep(NA,1),CC = rep(NA,1), CCcat = rep(NA,1),
                                          CC2 = rep(NA,1),CCcat2 = rep(NA,1)
),
s13 = data.frame(ID_ALUNO = rep(NA,n.13), ID_ESCOLA = rep(NA, n.13),
                 ID_UF = rep(NA, n.13),ID_MUNICIPIO = rep(NA, n.13),
                 CC0 = rep(NA,n.13),CC  = rep(NA, n.13), CCcat = rep(NA,n.13),
                 CC2 = rep(NA, n.13), CCcat2 = rep(NA,n.13)
),
s15 = data.frame(ID_ALUNO = rep(NA,n.15),ID_ESCOLA = rep(NA,n.15), 
                 ID_UF = rep(NA,n.15),ID_MUNICIPIO = rep(NA, n.15), 
                 CC0 = rep(NA,n.15),CC  = rep(NA,n.15),CCcat = rep(NA,n.15),
                 CC2 = rep(NA,n.15),CCcat2 = rep(NA,n.15)
),
s17 = data.frame(ID_ALUNO = rep(NA,n.17), ID_ESCOLA = rep(NA,n.17), 
                 ID_UF = rep(NA,n.17),ID_MUNICIPIO = rep(NA, n.17), 
                 CC0 = rep(NA,n.17),CC  = rep(NA,n.17),CCcat = rep(NA,n.17),
                 CC2 = rep(NA,n.17),CCcat2 = rep(NA,n.17) 
) )

for(i in 2:4) {
  capitalCultural[[i]]$ID_ALUNO          <- my_saeb[[i]]$ID_ALUNO
  capitalCultural[[i]]$ID_ESCOLA         <- my_saeb[[i]]$ID_ESCOLA
  capitalCultural[[i]]$ID_UF             <- my_saeb[[i]]$ID_UF
  capitalCultural[[i]]$ID_MUNICIPIO      <- my_saeb[[i]]$ID_MUNICIPIO
  
  capitalCultural[[i]]$CC0    <- my_saeb[[i]]$CC0
  capitalCultural[[i]]$CC     <- my_saeb[[i]]$CC
  capitalCultural[[i]]$CC2    <- my_saeb[[i]]$CC2
  capitalCultural[[i]]$CCcat  <- my_saeb[[i]]$CCcat
  capitalCultural[[i]]$CCcat2 <- my_saeb[[i]]$CCcat2
}

setwd("E:/VGA/dados/") # Diretório onde vai salvar o arquivo
save(x = capitalCultural, file = "capitalCultural.RDS")
