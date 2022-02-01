# Indicadores por escola

library(tidyverse)

data_17 <- data_17 %>% mutate(.,
                              raca3=case_when(
                                raca2 == "Brancos"     ~ 1,
                                raca2 == "Não brancos" ~ 0
                              ))

escolas <- unique(data_17$ID_ESCOLA)
model_LP <- PROFICIENCIA_LP_SAEB ~ educMae + CE + raca2 + sexo 
model_MT <- PROFICIENCIA_MT_SAEB ~ educMae + CE + raca2 + sexo

escola_i <- data_17 %>% filter(ID_ESCOLA == escolas[1])

reg_LP <- data.frame("intercept"=NA,"educMae"=NA,"CE"=NA,"Não brancos"=NA,
                     "feminino"=NA,"R2_LP"=NA,
                     "ID_ESCOLA"=NA,"n"=NA,"missing"=NA,"n_validos"=NA,"me_CE"=NA,
                     "p_B"=NA,"p_NB"=NA)

reg_MT <- data.frame("intercept"=NA,"educMae"=NA,"CE"=NA,"Não brancos"=NA,
                     "feminino"=NA,"R2_MT"=NA,
                     "ID_ESCOLA"=NA,"n"=NA,"missing"=NA,"n_validos"=NA,"me_CE"=NA,
                     "p_B"=NA,"p_NB"=NA)

for(i in 1:length(escolas)){
  escola_i <- data_17 %>% filter(ID_ESCOLA == escolas[i])
  
  reg_LP[i,"ID_ESCOLA"] <- escolas[i]
  reg_LP[i,"n"]         <- length(escola_i$ID_ESCOLA)
  reg_LP[i,"missing"]   <- sum(is.na(escola_i$CE))
  reg_LP[i,"n_validos"] <- reg_LP[i,"n"] - reg_LP[i,"missing"]
  reg_LP[i,"me_CE"]     <- mean(escola_i$CE,na.rm = T)
  reg_LP[i,"p_B"]      <- mean(escola_i$raca3,na.rm = T)
  reg_LP[i,"p_NB"]     <- 1 - mean(escola_i$raca3,na.rm = T)
  
  reg_MT[i,"ID_ESCOLA"] <- escolas[i]
  reg_MT[i,"n"]         <- length(escola_i$ID_ESCOLA)
  reg_MT[i,"missing"]   <- sum(is.na(escola_i$CE))
  reg_MT[i,"n_validos"] <- reg_MT[i,"n"] - reg_MT[i,"missing"]
  reg_MT[i,"me_CE"]     <- mean(escola_i$CE,na.rm = T)
  reg_MT[i,"p_B"]       <- mean(escola_i$raca3,na.rm = T)
  reg_MT[i,"p_NB"]      <- 1 - mean(escola_i$raca3,na.rm = T)
  
  tryCatch({
    print(i)
    if(reg_LP[i,"n_validos"] > 30){
      lm_LP      <- lm(data=escola_i,model_LP)
      lm_MT      <- lm(data=escola_i,model_MT)
      
      reg_LP[i,1:5]         <- coef(lm_LP)
      reg_LP[i,"R2_LP"]     = (var(lm_LP$model$PROFICIENCIA_LP_SAEB) - var(lm_LP$residuals)) / var(lm_LP$model$PROFICIENCIA_LP_SAEB)
      
      reg_MT[i,1:5]     <- coef(lm_MT)
      reg_MT[i,"R2_MT"] = (var(lm_MT$model$PROFICIENCIA_MT_SAEB) - var(lm_MT$residuals)) / var(lm_MT$model$PROFICIENCIA_MT_SAEB)
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

save(reg_LP,reg_MT,file="regressoes_esc_17.RDS")
