library(dplyr)
library(data.table)


for (i0 in 1:1) {
  base_pasada <- "D:/R-Perci/control-de-calidad/Archivo CSV - 08 febrero"
  base_actual_acumulada <- "D:/R-Perci/control-de-calidad/Archivo CSV - 09 febrero"
  
  ##################### cargando códigos de base pasada ########################
  base01_past <- data.frame(x=dir(print(base_pasada)))
  base03_past <- base01_past%>%filter(grepl('AUDITORIO', x))
  
  ########################## cargando base actualizada acumulada #########################
  base01 <- data.frame(x=dir(print(base_actual_acumulada)))
  base03 <- base01%>%filter(grepl('AUDITORIO', x))
  
  ################################################################################
  ############### Cargando bases de datos y filtrando nuevos registros  ##########
  ################################################################################
}

for (i in 1:nrow(base03)) {
  
  setwd("D:/R-Perci/control-de-calidad/Archivo CSV - 09 febrero")
  Mod01_newcod <- read.csv2(as.character(base03$x[i])) 
  if(i==3){names(Mod01_newcod)[206] <- "X_submission__id"}
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


write.table(reg_duplica_newcod, file="Auditorio - Duplicados - 09 febrero.csv", sep = ";", row.names = FALSE)
write.table(reg_comparados, file="Auditorio - Duplicados_Comparados - 09 febrero.csv", sep = ";", row.names = FALSE)


##################################################################

for (j in 1:1) {

#######################################################################
########################### caracteristicas generales ##############
#######################################################################


#If p1=6 then p1_ot≠∅ 

fun01 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p1 == "6" & Mod01_newcod$p1_ot == "", 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p1=6 then p1_ot≠∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun01()


#If p9=2 then p10_1, p10_2, p10_3, p10_4, p10_5 =∅ 

fun02 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p9 == 2 & (!is.na(Mod01_newcod$p10_1) | !is.na(Mod01_newcod$p10_2) | !is.na(Mod01_newcod$p10_3) | !is.na(Mod01_newcod$p10_4) | !is.na(Mod01_newcod$p10_5)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p9=2 then p10_1, p10_2, p10_3, p10_4, p10_5 =∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun02()


#If p11_0=1 then p12, p13 =∅ 

fun03 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p11_0 == 1 & (!is.na(Mod01_newcod$p12) | !is.na(Mod01_newcod$p13)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p11_0=1 then p12, p13 =∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun03()


#If p14_0=1 then p14_1, p15=∅ 

fun04 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p14_0 == 1 & (!is.na(Mod01_newcod$p14_1) | !is.na(Mod01_newcod$p15)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p14_0=1 then p14_1, p15=∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun04()


#If p14_ot=1 then p13_ot≠∅ 



#If p14_0=1 then p16_1_0, p16_1_1, p16_1_2, p16_1_3, p16_1_4, p15=∅ 



#If p14_1_4=0 then p14_1_ot≠∅ 

fun05 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p14_1_4 == 0 & Mod01_newcod$p14_1_ot == "", 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p14_1_4=0 then p14_1_ot≠∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun05()


#If p15_1_7=1 then p15_1_ot≠∅ 





#If p16=2 then p17_1, p17_1_0, p17_1_1, p17_1_2, p17_1_3, p17_1_4, p17_1_5, p17_1_6, p17_1_7, p17_1_8, p17_1_9, p17_1_10, p17_1_11, p17_1_12, p17_1_ot,
#p17_2, p17_2_0, p17_2_1, p17_2_2, p17_2_3, p17_2_5, p17_2_11, p17_2_12, p17_2_ot,
#p17_3, p17_3_0, p17_3_1, p17_3_3, p17_3_4, p17_3_5, p17_3_7, p17_3_8, p17_3_9, p17_3_10, p17_3_11, p17_3_12, p17_3_ot,
#p17_4, p17_4_0, p17_4_1, p17_4_3, p17_4_4, p17_4_5, p17_4_7, p17_4_8, p17_4_9, p17_4_10, p17_4_11, p17_4_12, p17_4_ot,
#p17_5, p17_5_0, p17_5_1, p17_5_2, p17_5_3, p17_5_4, p17_5_5, p17_5_7, p17_5_11, p17_5_12, p17_5_ot = ∅ 

fun06 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p16 == 2 & (Mod01_newcod$p17_1 != "" | !is.na(Mod01_newcod$p17_1_0) | !is.na(Mod01_newcod$p17_1_1) | !is.na(Mod01_newcod$p17_1_2) | !is.na(Mod01_newcod$p17_1_3) | !is.na(Mod01_newcod$p17_1_4) | !is.na(Mod01_newcod$p17_1_5) | !is.na(Mod01_newcod$p17_1_6) | !is.na(Mod01_newcod$p17_1_7) | !is.na(Mod01_newcod$p17_1_8) | !is.na(Mod01_newcod$p17_1_9) | !is.na(Mod01_newcod$p17_1_10) | !is.na(Mod01_newcod$p17_1_11) | !is.na(Mod01_newcod$p17_1_12) | Mod01_newcod$p17_1_ot != "" 
                                                       | Mod01_newcod$p17_2 != "" | !is.na(Mod01_newcod$p17_2_0) | !is.na(Mod01_newcod$p17_2_1) | !is.na(Mod01_newcod$p17_2_2) | !is.na(Mod01_newcod$p17_2_3) | !is.na(Mod01_newcod$p17_2_5) | !is.na(Mod01_newcod$p17_2_11) | !is.na(Mod01_newcod$p17_2_12) | Mod01_newcod$p17_2_ot != "" 
                                                       | Mod01_newcod$p17_3 != "" | !is.na(Mod01_newcod$p17_3_0) | !is.na(Mod01_newcod$p17_3_1) | !is.na(Mod01_newcod$p17_3_3) | !is.na(Mod01_newcod$p17_3_4) | !is.na(Mod01_newcod$p17_3_5) | !is.na(Mod01_newcod$p17_3_7) | !is.na(Mod01_newcod$p17_3_8) | !is.na(Mod01_newcod$p17_3_9) | !is.na(Mod01_newcod$p17_3_10) | !is.na(Mod01_newcod$p17_3_11) | !is.na(Mod01_newcod$p17_3_12) | Mod01_newcod$p17_3_ot != "" 
                                                       | Mod01_newcod$p17_4 != "" | !is.na(Mod01_newcod$p17_4_0) | !is.na(Mod01_newcod$p17_4_1) | !is.na(Mod01_newcod$p17_4_3) | !is.na(Mod01_newcod$p17_4_4) | !is.na(Mod01_newcod$p17_4_5) | !is.na(Mod01_newcod$p17_4_7) | !is.na(Mod01_newcod$p17_4_8) | !is.na(Mod01_newcod$p17_4_9) | !is.na(Mod01_newcod$p17_4_10) | !is.na(Mod01_newcod$p17_4_11) | !is.na(Mod01_newcod$p17_4_12) | Mod01_newcod$p17_4_ot != "" 
                                                       | Mod01_newcod$p17_5 != "" | !is.na(Mod01_newcod$p17_5_0) | !is.na(Mod01_newcod$p17_5_1) | !is.na(Mod01_newcod$p17_5_2) | !is.na(Mod01_newcod$p17_5_3) | !is.na(Mod01_newcod$p17_5_4) | !is.na(Mod01_newcod$p17_5_5) | !is.na(Mod01_newcod$p17_5_7) | !is.na(Mod01_newcod$p17_5_11) | !is.na(Mod01_newcod$p17_5_12) | Mod01_newcod$p17_5_ot != ""), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p16=2 then varios  p17_5_12, p17_5_ot = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun06()


#If p18=2 then p19_1, p19_1_0, p19_1_1, p19_1_2, p19_1_3, p91_1_4, p19_1_5, p19_1_6, p21_1_7, p19_1_9, p19_1_10, p19_1_11, p19_1_ot,
#p19_2, p19_2_0, p19_2_1, p19_2_2, p19_2_3, p19_2_4, p19_2_5, p19_2_6, p19_2_7, p19_2_9, p19_2_10, p19_2_11, p19_2_ot,
#p19_3, p19_3_0, p19_3_1, p19_3_2, p19_3_3, p19_3_4, p19_3_5, p19_3_6, p19_3_9, p19_3_10, p19_3_11, p19_3_ot = ∅ 

fun07 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p18 == 2 & (Mod01_newcod$p19_1 != "" | !is.na(Mod01_newcod$p19_1_0) | !is.na(Mod01_newcod$p19_1_1) | !is.na(Mod01_newcod$p19_1_2) | !is.na(Mod01_newcod$p19_1_3) | !is.na(Mod01_newcod$p19_1_4) | !is.na(Mod01_newcod$p19_1_5) | !is.na(Mod01_newcod$p19_1_6) | !is.na(Mod01_newcod$p19_1_7) | !is.na(Mod01_newcod$p19_1_9) | !is.na(Mod01_newcod$p19_1_10) | !is.na(Mod01_newcod$p19_1_11) | Mod01_newcod$p19_1_ot != "" 
                                                       | Mod01_newcod$p19_2 != "" | !is.na(Mod01_newcod$p19_2_0) | !is.na(Mod01_newcod$p19_2_1) | !is.na(Mod01_newcod$p19_2_2) | !is.na(Mod01_newcod$p19_2_3) | !is.na(Mod01_newcod$p19_2_4) | !is.na(Mod01_newcod$p19_2_5) | !is.na(Mod01_newcod$p19_2_6) | !is.na(Mod01_newcod$p19_2_7) | !is.na(Mod01_newcod$p19_2_9) | !is.na(Mod01_newcod$p19_2_10) | !is.na(Mod01_newcod$p19_2_11) | Mod01_newcod$p19_2_ot != "" 
                                                       | Mod01_newcod$p19_3 != "" | !is.na(Mod01_newcod$p19_3_0) | !is.na(Mod01_newcod$p19_3_1) | !is.na(Mod01_newcod$p19_3_2) | !is.na(Mod01_newcod$p19_3_3) | !is.na(Mod01_newcod$p19_3_4) | !is.na(Mod01_newcod$p19_3_5) | !is.na(Mod01_newcod$p19_3_6) | !is.na(Mod01_newcod$p19_3_9) | !is.na(Mod01_newcod$p19_3_10) | !is.na(Mod01_newcod$p19_3_11) | Mod01_newcod$p19_3_ot != ""), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p18=2 then varios p19_3_11, p19_3_ot = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun07()


#######################################################################
##### Servicios básicos, conexión a internet y habitabilidad ##########
#######################################################################


#If p20 =2 then p21_1, p21_2, p21_3, p21_4, p22, p23, p24, p25, p26, p27, p28 = ∅ 

fun08 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p20 == 2 & (!is.na(Mod01_newcod$p21_1) | !is.na(Mod01_newcod$p21_2) | !is.na(Mod01_newcod$p21_3) | !is.na(Mod01_newcod$p21_4) 
                                                       | !is.na(Mod01_newcod$p22) | !is.na(Mod01_newcod$p23) | !is.na(Mod01_newcod$p24) | Mod01_newcod$p25 != "" 
                                                       | !is.na(Mod01_newcod$p26) | !is.na(Mod01_newcod$p27) | !is.na(Mod01_newcod$p28)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p20 =2 then p21_1, p21_2, p21_3, p21_4, p22, p23, p24, p25, p26, p27, p28 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun08()


#If p22=2 then p23, p24, p25 =∅ 

fun09 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p22 == 2 & (!is.na(Mod01_newcod$p23) | !is.na(Mod01_newcod$p24) | Mod01_newcod$p25 != ""), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p22=2 then p23, p24, p25 =∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun09()



#If p24=2 then p25 = ∅ 

fun10 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p24 == 2 & Mod01_newcod$p25 != "", 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p24=2 then p25 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun10()


#If p27=2,3 then p28=∅ 

fun11 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p27 == 2 | Mod01_newcod$p27 == 3) & Mod01_newcod$p28 != "", 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p27=2,3 then p28=∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun11()


#If p29=2 then p32_1, p32_2, p33_1, p33_2 = ∅ 

fun12 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p29 == 2 & (!is.na(Mod01_newcod$p32_1) | !is.na(Mod01_newcod$p32_2) | !is.na(Mod01_newcod$p33_1) | !is.na(Mod01_newcod$p33_2)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p29=2 then p32_1, p32_2, p33_1, p33_2 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun12()


#If p35=1,2 then p35_1_1, p35_1_2, p35_1_3, p36 = ∅

fun13 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p35 == 1 | Mod01_newcod$p35 == 2) & (!is.na(Mod01_newcod$p35_1_1) | !is.na(Mod01_newcod$p35_1_2) | !is.na(Mod01_newcod$p35_1_3) | !is.na(Mod01_newcod$p36)), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p35=1,2 then p35_1_1, p35_1_2, p35_1_3, p36 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun13()


#######################################################################
################ Equipo y mobiliario ################################
#######################################################################


#If p41=2 then p41_1, p41_2, p41_3, p41_4_1, p41_4_2, p41_5, p41_5_1, p41_5_2, p41_5_3, p41_5_4, p41_5_5, p41_5_6, p41_5_7, p41_5_8, p41_5_ot = ∅ 

fun14 <- function() {
  Mod01_newcod_0_1 <- merge(Mod01_newcod_1,Mod01_newcod,by = "X_submission__id")
  Mod01_newcod_0_1$very <- ifelse(Mod01_newcod_0_1$p41 == 2 & (Mod01_newcod_0_1$p41_1 != "" | Mod01_newcod_0_1$p41_2 != "" | !is.na(Mod01_newcod_0_1$p41_3) 
                                                               | !is.na(Mod01_newcod_0_1$p41_4_1) | !is.na(Mod01_newcod_0_1$p41_4_2) 
                                                               | Mod01_newcod_0_1$p41_5 != "" | !is.na(Mod01_newcod_0_1$p41_5_1) | !is.na(Mod01_newcod_0_1$p41_5_2) | !is.na(Mod01_newcod_0_1$p41_5_3) | !is.na(Mod01_newcod_0_1$p41_5_4) | !is.na(Mod01_newcod_0_1$p41_5_5) | !is.na(Mod01_newcod_0_1$p41_5_6) | !is.na(Mod01_newcod_0_1$p41_5_7) | !is.na(Mod01_newcod_0_1$p41_5_8) | Mod01_newcod_0_1$p41_5_ot != ""), 1, 0)
  reporte <- Mod01_newcod_0_1 %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p41=2 then p41_1, p41_2, p41_3, p41_4_1, p41_4_2, p41_5, p41_5_1, p41_5_2, p41_5_3, p41_5_4, p41_5_5, p41_5_6, p41_5_7, p41_5_8, p41_5_ot = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun14()


#If p42=2 then p42_1, p42_2, p42_3, p42_4_1, p42_4_2, p42_4_3=∅ 

fun15 <- function() {
  Mod01_newcod_0_2 <- merge(Mod01_newcod_2,Mod01_newcod,by = "X_submission__id")
  Mod01_newcod_0_2$very <- ifelse(Mod01_newcod_0_2$p42 == 2 & (Mod01_newcod_0_2$p42_1 != "" | Mod01_newcod_0_2$p42_2 != "" | !is.na(Mod01_newcod_0_2$p42_3) 
                                                               | !is.na(Mod01_newcod_0_2$p42_4_1) | !is.na(Mod01_newcod_0_2$p42_4_2) | !is.na(Mod01_newcod_0_2$p42_4_3)), 1, 0)
  reporte <- Mod01_newcod_0_2 %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p42=2 then p42_1, p42_2, p42_3, p42_4_1, p42_4_2, p42_4_3=∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun15()


#######################################################################
################ Tratamiento de residuos ##############################
#######################################################################

#If p43_1_1=1 then p43_1_2, p43_1_3, p43_2_1, p43_2_2, p43_2_3 ≠ ∅ 

fun16 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p43_1_1 == 1 & (!is.na(Mod01_newcod$p43_1_2) | Mod01_newcod$p43_1_3 != "" | !is.na(Mod01_newcod$p43_2_1) | !is.na(Mod01_newcod$p43_2_2) | Mod01_newcod$p43_2_3 != ""), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Auditorio - If p43_1_1=1 then p43_1_2, p43_1_3, p43_2_1, p43_2_2, p43_2_3 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun16()


}
