# Estudo de regressão multinível
# Author: Victor G Alcantara
# Date: 03.12.21

# 0. Packages and Setup ======================================================

library(tidyverse)
library(readxl)

# 1. Openning data ===========================================================
# Diretório com as bases de dados que iremos trabalhar guardado no objeto 'wd'
wd <- "C:/Users/VictorGabriel/documents/00_dados/EDUCACAO/"
wd <- "E:/VGA/dados/"
setwd(wd)

load("mysaeb_sample_n100.RDS")
load("capitalEconomico.RDS")
load("capitalCultural.RDS")

