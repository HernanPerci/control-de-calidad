library(dplyr)
library(data.table)


for (i0 in 1:1) {
  base_pasada <- "D:/R-Perci/control-de-calidad/Archivo CSV - 08 febrero"
  base_actual_acumulada <- "D:/R-Perci/control-de-calidad/Archivo CSV - 09 febrero"
  
  ##################### cargando códigos de base pasada ########################
  base01_past <- data.frame(x=dir(print(base_pasada)))
  base03_past <- base01_past%>%filter(grepl('SSHH', x))
  
  ########################## cargando base actualizada acumulada #########################
  base01 <- data.frame(x=dir(print(base_actual_acumulada)))
  base03 <- base01%>%filter(grepl('SSHH', x))
  
  ################################################################################
  ############### Cargando bases de datos y filtrando nuevos registros  ##########
  ################################################################################
}

for (i in 1:nrow(base03)) {
  
  setwd("D:/R-Perci/control-de-calidad/Archivo CSV - 09 febrero")
  Mod01_newcod <- read.csv2(as.character(base03$x[i])) 
  if(i==3){names(Mod01_newcod)[173] <- "X_submission__id"}
  assign(paste("Mod01_newcod", i,sep = "_"), Mod01_newcod)
}

#################################################################################
##################Identificar valores únicos del total de registros #############
#################################################################################
for (i1 in 1:1) {
  Mod01_newcod$cod_dif <- paste(Mod01_newcod$cod_mod,Mod01_newcod$cod_local,Mod01_newcod$cod_edificacion,Mod01_newcod$cod_ambiente, Mod01_newcod$piso ,sep = "-")
  registros <- Mod01_newcod%>%group_by(cod_dif)%>%count(cod_dif)  #lista de n_veces de codigos registrados
  reg_duplicados <- registros[registros$n >1,] # códigos de registro repetidos/añadir toda la linea
  cod_reg_dupli <- reg_duplicados$cod_dif
  reg_duplica_newcod <- Mod01_newcod%>%filter(cod_dif%in%cod_reg_dupli ==T)
}

reg_comparados <- data.frame(col_name=colnames(reg_duplica_newcod))
for (r in 1:nrow(reg_duplicados)) {
  reg_duplica_compare <- reg_duplica_newcod%>%filter(cod_dif%in%cod_reg_dupli[r] ==T)
  compare_t <- data.table(t(reg_duplica_compare))
  compare_t$compare <- ifelse(compare_t$V1==compare_t$V2,0,1)
  names(compare_t)[(nrow(reg_duplica_compare)+1)]<- paste(cod_reg_dupli[r],"R", sep="_")
  reg_comparados <-cbind(reg_comparados,assign(paste(cod_reg_dupli[r],"R",sep = "_"),compare_t))
}


write.table(reg_duplica_newcod, file="SSHH - Duplicados - 09 febrero.csv", sep = ";", row.names = FALSE)
write.table(reg_comparados, file="SSHH - Duplicados_Comparados - 09 febrero.csv", sep = ";", row.names = FALSE)


##################################################################

for (j in 1:1) {

#######################################################################
################## caracteristicas del ambiente #######################
#######################################################################

#If p10_0 = 1 then p11 y p12 = ∅; If p10_1, p10_2, p10_3, p10_4 o p10_5 = 1 then p11 y p12 ≠ ∅ 

fun01 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p10_0 == 1 & (!is.na(Mod01_newcod$p11) | !is.na(Mod01_newcod$p12))) 
                              | ((Mod01_newcod$p10_1 == 1 | Mod01_newcod$p10_2 == 1 | Mod01_newcod$p10_3 == 1 | Mod01_newcod$p10_4 == 1 | Mod01_newcod$p10_5 == 1) & (is.na(Mod01_newcod$p11) | is.na(Mod01_newcod$p12))), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="SSHH - If p10_0 = 1 then p11 y p12 = ∅; If p10_1, p10_2, p10_3, p10_4 o p10_5 = 1 then p11 y p12 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun01()


#If p13_0 = 1 then p13_1a y p14 = ∅; If p13_1, p13_2, p13_3, p13_4, p13_5 o p13_6 = 1 then p13_1a y p14 ≠ ∅ 

fun02 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p13_0 == 1 & (Mod01_newcod$p13_1a != "" | !is.na(Mod01_newcod$p14))) 
                              | ((Mod01_newcod$p13_1 == 1 | Mod01_newcod$p13_2 == 1 | Mod01_newcod$p13_3 == 1 | Mod01_newcod$p13_4 == 1 | Mod01_newcod$p13_5 == 1 | Mod01_newcod$p13_6 == 1) & (is.na(Mod01_newcod$p13_1a) | is.na(Mod01_newcod$p14))), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="SSHH - If p13_0 = 1 then p13_1a y p14 = ∅; If p13_1, p13_2, p13_3, p13_4, p13_5 o p13_6 = 1 then p13_1a y p14 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun02()


#If p13_1_1 = 1 then p14 ≠ ∅; If p13_1_1 = 0 then p14 = ∅ 

fun03 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p13_1_1 == 1 & is.na(Mod01_newcod$p14)) 
                              | (Mod01_newcod$p13_1_1 == 0 & !is.na(Mod01_newcod$p14)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="SSHH - If p13_1_1 = 1 then p14 ≠ ∅; If p13_1_1 = 0 then p14 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun03()


#If p15 = 1 then p16_1, p16_2, p16_3, p16_4 y p16_5 ≠ ∅; If p15 = 2 then p16_1, p16_2, p16_3, p16_4 y p16_5 = ∅ 

fun04 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p15 == 1 & (Mod01_newcod$p16_1 == "" | Mod01_newcod$p16_2 == "" | Mod01_newcod$p16_3 == "" | Mod01_newcod$p16_4 == "" | Mod01_newcod$p16_5 == "")) 
                              | (Mod01_newcod$p15 == 2 & (Mod01_newcod$p16_1 != "" | Mod01_newcod$p16_2 != "" | Mod01_newcod$p16_3 != "" | Mod01_newcod$p16_4 != "" | Mod01_newcod$p16_5 != "")), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="SSHH - If p15 = 1 then p16_1, p16_2, p16_3, p16_4 y p16_5 ≠ ∅; If p15 = 2 then p16_1, p16_2, p16_3, p16_4 y p16_5 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun04()


#If p17 = 1 then p18_1 y 18_2 ≠ ∅; If p17 = 2 then p18_1 y 18_2 = ∅ 

fun05 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p17 == 1 & (Mod01_newcod$p18_1 == "" | Mod01_newcod$p18_2 == "")) 
                              | (Mod01_newcod$p17 == 2 & (Mod01_newcod$p18_1 != "" | Mod01_newcod$p18_2 != "")), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="SSHH - If p17 = 1 then p18_1 y 18_2 ≠ ∅; If p17 = 2 then p18_1 y 18_2 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun05()


##################################################
##### Servicios básicos y habitabilidad ##########
##################################################

#If p19 =1 then p20 y p21 ≠ ∅; If p19 = 2 then p20 y p21 = ∅ 

fun06 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p19 == 1 & (Mod01_newcod$p20 == "" | is.na(Mod01_newcod$p21))) 
                              | (Mod01_newcod$p19 == 2 & (Mod01_newcod$p20 != "" | !is.na(Mod01_newcod$p21))), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="SSHH - If p19 =1 then p20 y p21 ≠ ∅; If p19 = 2 then p20 y p21 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun06()


#If p22 = 1 then p25, p26 y p27 ≠ ∅; If p22 = 2 then p25, p26 y p27 = ∅

fun07 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p22 == 1 & (Mod01_newcod$p25 == "" | is.na(Mod01_newcod$p26) | Mod01_newcod$p27 == "")) 
                              | (Mod01_newcod$p22 == 2 & (Mod01_newcod$p25 != "" | !is.na(Mod01_newcod$p26) | Mod01_newcod$p27 != "")), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="SSHH - If p22 = 1 then p25, p26 y p27 ≠ ∅; If p22 = 2 then p25, p26 y p27 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun07()

}
