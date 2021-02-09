library(dplyr)
library(data.table)


for (i0 in 1:1) {
  base_pasada <- "D:/R-Perci/control-de-calidad/Archivo CSV - 08 febrero"
  base_actual_acumulada <- "D:/R-Perci/control-de-calidad/Archivo CSV - 09 febrero"
  
  ##################### cargando códigos de base pasada ########################
  base01_past <- data.frame(x=dir(print(base_pasada)))
  base03_past <- base01_past%>%filter(grepl('RESIDENCIA', x))
  
  ########################## cargando base actualizada acumulada #########################
  base01 <- data.frame(x=dir(print(base_actual_acumulada)))
  base03 <- base01%>%filter(grepl('RESIDENCIA', x))
  
  ################################################################################
  ############### Cargando bases de datos y filtrando nuevos registros  ##########
  ################################################################################
}

for (i in 1:nrow(base03)) {
  
  setwd("D:/R-Perci/control-de-calidad/Archivo CSV - 09 febrero")
  Mod01_newcod <- read.csv2(as.character(base03$x[i])) 
  if(i==3){names(Mod01_newcod)[226] <- "X_submission__id"}
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


write.table(reg_duplica_newcod, file="Residencia - Duplicados - 09 febrero.csv", sep = ";", row.names = FALSE)
write.table(reg_comparados, file="Residencia - Duplicados_Comparados - 09 febrero.csv", sep = ";", row.names = FALSE)

##################################################################

for (j in 1:1) {

#######################################################################
########################### caracteristicas generales ##############
#######################################################################

#If p1=6 then p1_ot≠∅ 

fun01 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p1 == 6 & Mod01_newcod$p1_ot == "", 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p1=6 then p1_ot≠∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun01()


#If p11=2 then p12, p12_1, p12_2, p12_3, p12_4, p12_5=∅ 

fun02 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p11 == 2 & (Mod01_newcod$p12 != "" | !is.na(Mod01_newcod$p12_1) | !is.na(Mod01_newcod$p12_2) | !is.na(Mod01_newcod$p12_3) | !is.na(Mod01_newcod$p12_4) | !is.na(Mod01_newcod$p12_5)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p11=2 then p12, p12_1, p12_2, p12_3, p12_4, p12_5=∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun02()


#If p13=0 then p14, p15=∅ 

fun03 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p13 == "0" & (!is.na(Mod01_newcod$p14) | !is.na(Mod01_newcod$p15)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p13=0 then p14, p15=∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun03()


#If p13=5 then p13_ot≠∅ 

fun04 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p13 == "5" & Mod01_newcod$p13_ot == "", 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p13=5 then p13_ot≠∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun04()


#If p16=0 then p16_1a, p16_1_0, p16_1_1, p16_1_2, p16_1_3, p16_1_4, p16_1_ot=∅ 

fun05 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p16 == "0" & (Mod01_newcod$p16_1a != "" | !is.na(Mod01_newcod$p16_1_0) | !is.na(Mod01_newcod$p16_1_1) | !is.na(Mod01_newcod$p16_1_2) | !is.na(Mod01_newcod$p16_1_3) | !is.na(Mod01_newcod$p16_1_4) | Mod01_newcod$p16_1_ot != ""), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p16=0 then p16_1a, p16_1_0, p16_1_1, p16_1_2, p16_1_3, p16_1_4, p16_1_ot=∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun05()


#If p16=0 then p17, p17_1=∅ 

fun06 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p16 == "0" & (!is.na(Mod01_newcod$p17) | !is.na(Mod01_newcod$p17_1)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p16=0 then p17, p17_1=∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun06()


#If p16_1=1 then p17≠∅ 

fun07 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p16_1 == 1 & is.na(Mod01_newcod$p17), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p16_1=1 then p17≠∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun07()


#If p17_1=7 then p17_1_ot≠∅ 

fun08 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p17_1 == 7 & Mod01_newcod$p17_1_ot != "", 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p17_1=7 then p17_1_ot≠∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun08()


#If p18=2 then p19_1, p19_1_0, p19_1_1, p19_1_2, p19_1_3, p19_1_4, p19_1_5, p19_1_6, p19_1_7, p19_1_8, p19_1_9, p19_1_10, p19_1_11, p19_1_12, p19_1_ot, 
# p19_2, p19_2_0, p19_2_1, p19_2_2, p19_2_3, p19_2_5, p19_2_11, p19_2_12, p19_2_ot, 
# p19_3, p19_3_0, p19_3_1, p19_3_3, p19_3_4, p19_3_5, p19_3_7, p19_3_8, p19_3_9, p19_3_10, p19_3_11, p19_3_12, p19_3_ot, 
# p19_4, p19_4_0, p19_4_1, p19_4_3, p19_4_4, p19_4_5, p19_4_7, p19_4_8, p19_4_9, p19_4_10, p19_4_11, p19_4_12, p19_4_ot, 
# p19_5, p19_5_0, p19_5_1, p19_5_2, p19_5_3, p19_5_4, p19_5_5, p19_5_7, p19_5_11, p19_5_12, p19_5_ot = ∅ 

fun09 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p18 == 2 & (Mod01_newcod$p19_1 != "" | !is.na(Mod01_newcod$p19_1_0) | !is.na(Mod01_newcod$p19_1_1) | !is.na(Mod01_newcod$p19_1_2) | !is.na(Mod01_newcod$p19_1_3) | !is.na(Mod01_newcod$p19_1_4) | !is.na(Mod01_newcod$p19_1_5) | !is.na(Mod01_newcod$p19_1_6) | !is.na(Mod01_newcod$p19_1_7) | !is.na(Mod01_newcod$p19_1_8) | !is.na(Mod01_newcod$p19_1_9) | !is.na(Mod01_newcod$p19_1_10) | !is.na(Mod01_newcod$p19_1_11) | !is.na(Mod01_newcod$p19_1_12) | Mod01_newcod$p19_1_ot != "" 
                              | !is.na(Mod01_newcod$p19_2) | !is.na(Mod01_newcod$p19_2_0) | !is.na(Mod01_newcod$p19_2_1) | !is.na(Mod01_newcod$p19_2_2) | !is.na(Mod01_newcod$p19_2_3) | !is.na(Mod01_newcod$p19_2_5) | !is.na(Mod01_newcod$p19_2_11) | !is.na(Mod01_newcod$p19_2_12) | Mod01_newcod$p19_2_ot != "" 
                              | Mod01_newcod$p19_3 != "" | !is.na(Mod01_newcod$p19_3_0) | !is.na(Mod01_newcod$p19_3_1) | !is.na(Mod01_newcod$p19_3_3) | !is.na(Mod01_newcod$p19_3_4) | !is.na(Mod01_newcod$p19_3_5) | !is.na(Mod01_newcod$p19_3_7) | !is.na(Mod01_newcod$p19_3_8) | !is.na(Mod01_newcod$p19_3_9) | !is.na(Mod01_newcod$p19_3_10) | !is.na(Mod01_newcod$p19_3_11) | !is.na(Mod01_newcod$p19_3_12) | Mod01_newcod$p19_3_ot != "" 
                              | !is.na(Mod01_newcod$p19_4) | !is.na(Mod01_newcod$p19_4_0) | !is.na(Mod01_newcod$p19_4_1) | !is.na(Mod01_newcod$p19_4_3) | !is.na(Mod01_newcod$p19_4_4) | !is.na(Mod01_newcod$p19_4_5) | !is.na(Mod01_newcod$p19_4_7) | !is.na(Mod01_newcod$p19_4_8) | !is.na(Mod01_newcod$p19_4_9) | !is.na(Mod01_newcod$p19_4_10) | !is.na(Mod01_newcod$p19_4_11) | !is.na(Mod01_newcod$p19_4_12) | Mod01_newcod$p19_4_ot != "" 
                              | Mod01_newcod$p19_5 != "" | !is.na(Mod01_newcod$p19_5_0) | !is.na(Mod01_newcod$p19_5_1) | !is.na(Mod01_newcod$p19_5_2) | !is.na(Mod01_newcod$p19_5_3) | !is.na(Mod01_newcod$p19_5_4) | !is.na(Mod01_newcod$p19_5_5) | !is.na(Mod01_newcod$p19_5_7) | !is.na(Mod01_newcod$p19_5_11) | !is.na(Mod01_newcod$p19_5_12) | Mod01_newcod$p19_5_ot != ""), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p18=2 then varios  p19_5_12, p19_5_ot = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun09()



#If p20=2 then p21_1, p21_1_0, p21_1_1, p21_1_2, p21_1_3, p21_1_4, p21_1_5, p21_1_6, p21_1_7, p21_1_9, p21_1_10, p21_1_11, p21_1_ot,
#p21_2, p21_2_0, p21_2_1, p21_2_2, p21_2_3, p21_2_4, p21_2_5, p21_2_6, p21_2_7, p21_2_9, p21_2_10, p21_2_11, p21_2_ot,
#p21_3, p21_3_0, p21_3_1, p21_3_2, p21_3_3, p21_3_4, p21_3_5, p21_3_6, p21_3_9, p21_3_10, p21_3_11, p21_3_ot = ∅ 

fun10 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p20 == 2 & (Mod01_newcod$p21_1 != "" | !is.na(Mod01_newcod$p21_1_0) | !is.na(Mod01_newcod$p21_1_1) | !is.na(Mod01_newcod$p21_1_2) | !is.na(Mod01_newcod$p21_1_3) | !is.na(Mod01_newcod$p21_1_4) | !is.na(Mod01_newcod$p21_1_5) | !is.na(Mod01_newcod$p21_1_6) | !is.na(Mod01_newcod$p21_1_7) | !is.na(Mod01_newcod$p21_1_9) | !is.na(Mod01_newcod$p21_1_10) | !is.na(Mod01_newcod$p21_1_11) | Mod01_newcod$p21_1_ot != "" 
                              | !is.na(Mod01_newcod$p21_2) | !is.na(Mod01_newcod$p21_2_0) | !is.na(Mod01_newcod$p21_2_1) | !is.na(Mod01_newcod$p21_2_2) | !is.na(Mod01_newcod$p21_2_3) | !is.na(Mod01_newcod$p21_2_4) | !is.na(Mod01_newcod$p21_2_5) | !is.na(Mod01_newcod$p21_2_6) | !is.na(Mod01_newcod$p21_2_7) | !is.na(Mod01_newcod$p21_2_9) | !is.na(Mod01_newcod$p21_2_10) | !is.na(Mod01_newcod$p21_2_11) | Mod01_newcod$p21_2_ot != "" 
                              | Mod01_newcod$p21_3 != "" | !is.na(Mod01_newcod$p21_3_0) | !is.na(Mod01_newcod$p21_3_1) | !is.na(Mod01_newcod$p21_3_2) | !is.na(Mod01_newcod$p21_3_3) | !is.na(Mod01_newcod$p21_3_4) | !is.na(Mod01_newcod$p21_3_5) | !is.na(Mod01_newcod$p21_3_6) | !is.na(Mod01_newcod$p21_3_9) | !is.na(Mod01_newcod$p21_3_10) | !is.na(Mod01_newcod$p21_3_11) | Mod01_newcod$p21_3_ot != ""), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p20=2 then varios p21_3_11, p21_3_ot = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun10()


#######################################################################
##### Servicios básicos, conexión a internet y habitabilidad ##########
#######################################################################


#If p22=2 then p23, p23_1, p23_2, p23_3, p23_4, p24, p25, p26, p27, p27_1, p27_2, p28, p29, p30 = ∅ 

fun11 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p22 == 2 & (Mod01_newcod$p23 != "" | !is.na(Mod01_newcod$p23_1) | !is.na(Mod01_newcod$p23_2) | !is.na(Mod01_newcod$p23_3) | !is.na(Mod01_newcod$p23_4) 
                              | !is.na(Mod01_newcod$p24) | !is.na(Mod01_newcod$p25) | !is.na(Mod01_newcod$p26) | Mod01_newcod$p27 != "" 
                              | !is.na(Mod01_newcod$p27_1) | !is.na(Mod01_newcod$p27_2) | !is.na(Mod01_newcod$p28) | !is.na(Mod01_newcod$p29) | !is.na(Mod01_newcod$p30)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p22=2 then p23, p23_1, p23_2, p23_3, p23_4, p24, p25, p26, p27, p27_1, p27_2, p28, p29, p30 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun11()



#If p24=2 then p25, p26, p27, p27_1, p27_2 =∅ 

fun12 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p24 == 2 & (!is.na(Mod01_newcod$p25) | !is.na(Mod01_newcod$p26) | Mod01_newcod$p27 != "" | !is.na(Mod01_newcod$p27_1) | !is.na(Mod01_newcod$p27_2)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p24=2 then p25, p26, p27, p27_1, p27_2 =∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun12()



#If p26=2 then p27, p27_1, p27_2 = ∅ 

fun13 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p26 == 2 & (Mod01_newcod$p27 != "" | !is.na(Mod01_newcod$p27_1) | !is.na(Mod01_newcod$p27_2)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p26=2 then p27, p27_1, p27_2 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun13()



#If p29=2,3 then p30=∅ 

fun14 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p29 == 2 | Mod01_newcod$p29 == 3) & !is.na(Mod01_newcod$p30), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p29=2,3 then p30=∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun14()



#If p31=2 then p32, p32_1, p32_2, p32_3, p32_4	, p32_ot, p33, p34, p34_1, p34_2 = ∅ 

fun15 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p31 == 2 & (Mod01_newcod$p32 != "" | Mod01_newcod$p32_1 != "" | Mod01_newcod$p32_2 != "" | Mod01_newcod$p32_3 != "" | Mod01_newcod$p32_4 != "" | Mod01_newcod$p32_ot != "" 
                              | Mod01_newcod$p33 != "" | Mod01_newcod$p34 != "" | Mod01_newcod$p34_1 != "" | Mod01_newcod$p34_2 != ""), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p31=2 then p32, p32_1, p32_2, p32_3, p32_4	, p32_ot, p33, p34, p34_1, p34_2 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun15()



#If p35=2 then p38, p38_1, p38_2, p39, p39_1, p39_2 = ∅ 

fun16 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p35 == 2 & (Mod01_newcod$p38 != "" | !is.na(Mod01_newcod$p38_1) | !is.na(Mod01_newcod$p38_2) 
                              | !is.na(Mod01_newcod$p39) | !is.na(Mod01_newcod$p39_1) | !is.na(Mod01_newcod$p39_2)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p35=2 then p38, p38_1, p38_2, p39, p39_1, p39_2 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun16()



#If p41=1,2 then p41_1, p41_1_1, p41_1_2, p41_1_3, p42 = ∅

fun17 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p41 == 1 | Mod01_newcod$p41 == 2) & (Mod01_newcod$p41_1 != "" | !is.na(Mod01_newcod$p41_1_1) | !is.na(Mod01_newcod$p41_1_2) | !is.na(Mod01_newcod$p41_1_3) | !is.na(Mod01_newcod$p42)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p41=1,2 then p41_1, p41_1_1, p41_1_2, p41_1_3, p42 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun17()


#######################################################################
################ Equipos y mobiliarios ################################
#######################################################################


#If p48=2 then p48_1, p48_2, p48_3, p48_4_1, p48_4_2, p48_5, p48_5_1, p48_5_2, p48_5_3, p48_5_4, p48_5_5, p48_5_6, p48_5_7, p48_5_8, p48_5_ot = ∅ 

fun18 <- function() {
  Mod01_newcod_0_1 <- merge(Mod01_newcod_1,Mod01_newcod,by = "X_submission__id")
  Mod01_newcod_0_1$very <- ifelse(Mod01_newcod_0_1$p48 == 2 & (Mod01_newcod_0_1$p48_1 != "" | Mod01_newcod_0_1$p48_2 != "" | !is.na(Mod01_newcod_0_1$p48_3) 
                                  | !is.na(Mod01_newcod_0_1$p48_4_1) | !is.na(Mod01_newcod_0_1$p48_4_2) 
                                  | Mod01_newcod_0_1$p48_5 != "" | !is.na(Mod01_newcod_0_1$p48_5_1) | !is.na(Mod01_newcod_0_1$p48_5_2) | !is.na(Mod01_newcod_0_1$p48_5_3) | !is.na(Mod01_newcod_0_1$p48_5_4) | !is.na(Mod01_newcod_0_1$p48_5_5) | !is.na(Mod01_newcod_0_1$p48_5_6) | !is.na(Mod01_newcod_0_1$p48_5_7) | !is.na(Mod01_newcod_0_1$p48_5_8) | Mod01_newcod_0_1$p48_5_ot != ""), 1, 0)
  reporte <- Mod01_newcod_0_1 %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p48=2 then p48_1, p48_2, p48_3, p48_4_1, p48_4_2, p48_5, p48_5_1, p48_5_2, p48_5_3, p48_5_4, p48_5_5, p48_5_6, p48_5_7, p48_5_8, p48_5_ot = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun18()


#If p49=2 then p49_1, p49_2, p49_3, p49_4_1, p49_4_2, p49_4_3 = ∅ 

fun19 <- function() {
  Mod01_newcod_0_2 <- merge(Mod01_newcod_2,Mod01_newcod,by = "X_submission__id")
  Mod01_newcod_0_2$very <- ifelse(Mod01_newcod_0_2$p49 == 2 & (Mod01_newcod_0_2$p49_1 != "" | Mod01_newcod_0_2$p49_2 != "" | !is.na(Mod01_newcod_0_2$p49_3) 
                                  | !is.na(Mod01_newcod_0_2$p49_4_1) | !is.na(Mod01_newcod_0_2$p49_4_2) | !is.na(Mod01_newcod_0_2$p49_4_3)), 1, 0)
  reporte <- Mod01_newcod_0_2 %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p49=2 then p49_1, p49_2, p49_3, p49_4_1, p49_4_2, p49_4_3 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun19()


#######################################################################
################ Tratamiento de residuos ##############################
#######################################################################


#If p50_1_2=2 then p50_1_3 ≠ ∅ 

fun20 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p50_1_2 == 2 & Mod01_newcod$p50_1_3 == "", 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p50_1_2=2 then p50_1_3 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun20()


#If p50_2_2=2 then p50_2_3 ≠ ∅

fun21 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p50_2_2 == 2 & Mod01_newcod$p50_2_3 == "", 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Residencia - If p50_2_2=2 then p50_2_3 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun21()

}
