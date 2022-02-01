library(gapminder)
library(plotly)

wd <- "E:/VGA/dados/EDUCACAO/MESTRADO/LM Escolas"
setwd(wd)

save(d,file="data_ideb+regs_2.RDS")

# Descrições das escolas
ano = seq(2007,2019,2)
s07_19 = data.frame("ID_ESCOLA"=NA,"me_CE"=NA,"p_B"=NA,"p_NB"=NA,
                    "ano"=NA)

# Dados descritivos das escolas
for(i in 1:length(ano)){
  load(paste0("Para corrigir/regressoes_esc_",ano[i],".RDS"))
  s <- data.frame("ID_ESCOLA"=reg_LP$ID_ESCOLA,"me_CE"=reg_LP$me_CE,
                  "p_B"=reg_LP$p_B,"p_NB"=reg_LP$p_NB,
                  "ano"=rep(ano[i],nrow(reg_LP)))
  s07_19     <- bind_rows(s,s07_19)
  rm(reg_LP,reg_MT)
  gc()
}

names(d)
d <- merge(d,s07_19)

d <- d %>% mutate(.,
                        NO_UF   = case_when(
                          ID_UF == 11 ~ "RO",
                          ID_UF == 12 ~ "AC",
                          ID_UF == 13 ~ "AM",
                          ID_UF == 14 ~ "RR",
                          ID_UF == 15 ~ "PA",
                          ID_UF == 16 ~ "AP",
                          ID_UF == 17 ~ "TO",
                          ID_UF == 21 ~ "MA",
                          ID_UF == 22 ~ "PI",
                          ID_UF == 23 ~ "CE",
                          ID_UF == 24 ~ "RN",
                          ID_UF == 25 ~ "PB",
                          ID_UF == 26 ~ "PE",
                          ID_UF == 27 ~ "AL",
                          ID_UF == 28 ~ "SE",
                          ID_UF == 29 ~ "BA",
                          ID_UF == 31 ~ "MG",
                          ID_UF == 32 ~ "ES",
                          ID_UF == 33 ~ "RJ",
                          ID_UF == 34 ~ "SP",
                          ID_UF == 41 ~ "PR",
                          ID_UF == 42 ~ "SC",
                          ID_UF == 43 ~ "RS",
                          ID_UF == 50 ~ "MS",
                          ID_UF == 51 ~ "MT",
                          ID_UF == 52 ~ "GO",
                          ID_UF == 53 ~ "DF"
                        ),
                  ce_cat = cut(.$me_CE,
                               breaks=c(0,1,1.7,3),
                               labels=c("B","M","A"),
                               ordered_result=T),
                  IDEB = as.numeric(IDEB),
                  raca_cat = ifelse(p_B > 0.5, "B","NB")
                  )

mydata <- d %>% select(ano,NO_ESCOLA,NO_UF,NO_MUNICIPIO,depAdmin,IDEB,
                       me_R2,CE.x,CE.y,
                       n_validos.x,Não.brancos.x,Não.brancos.y,
                       feminino.x,feminino.y,tel,
                       PC_FORMACAO_DOCENTE_FINAL,IE,me_CE,
                       p_B,p_NB) %>% mutate(.,
                                            Não.brancos.x = round(Não.brancos.x,2),
                                            Não.brancos.y = round(Não.brancos.y,2),
                                            feminino.x = round(feminino.x,2),
                                            feminino.y = round(feminino.y,2))

names(mydata) <- c("ano","Nome da escola","UF","Município",
                   "Dep. admin.",
                   "IDEB","Média R2","CE (LP)","CE (MT)",
                   "casos válidos",
                   "Não brancos (LP)","Não brancos (MT)",
                   "Meninas (LP)","Meninas (MT)",
                   "tel",
                   "Docentes formados (%)",
                   "Indicador Eficácia","Média CE",
                   "Prop. brancos","Prop. não brancos"
                   )

textcol <- "gray40"

set_theme <- theme(
  axis.title.x=element_text(size=14,colour="black"),
  axis.title.y=element_text(size=14,colour="black"),
  axis.text = element_text(size=16,colour=textcol),
  axis.ticks=element_line(size=0.5),
  legend.position = "bottom",
  legend.text = element_text(size=10,colour="black")
)

plt1.0 <- mydata %>% filter(ano == 2017 & `casos válidos` > 30) %>% 
  ggplot(aes(y=IDEB,x=`Média R2`,label=`Nome da escola`,
             label1=`Dep. admin.`,label2=`Média CE`,
             label3=`casos válidos`,label4=UF,label5=Município,
             label6=`Indicador Eficácia`,label7=`Docentes formados (%)`,
             label8=tel,
             label9=`Prop. brancos`,label10=`Prop. não brancos`,
             label11=`Não brancos (LP)`,label12=`Não brancos (MT)`,
             label13=`Meninas (LP)`,label14=`Meninas (MT)`))+
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

ggplotly(plt1.1)
