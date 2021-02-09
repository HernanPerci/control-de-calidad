library(dplyr)
library(data.table)


for (i0 in 1:1) {
  base_pasada <- "D:/R-Perci/control-de-calidad/Archivo CSV - 08 febrero"
  base_actual_acumulada <- "D:/R-Perci/control-de-calidad/Archivo CSV - 09 febrero"
  
  ##################### cargando códigos de base pasada ########################
  base01_past <- data.frame(x=dir(print(base_pasada)))
  base03_past <- base01_past%>%filter(grepl('BIBLIOTECA', x))
  
  ########################## cargando base actualizada acumulada #########################
  base01 <- data.frame(x=dir(print(base_actual_acumulada)))
  base03 <- base01%>%filter(grepl('BIBLIOTECA', x))
  
  ################################################################################
  ############### Cargando bases de datos y filtrando nuevos registros  ##########
  ################################################################################
}

for (i in 1:nrow(base03)) {
  
  setwd("D:/R-Perci/control-de-calidad/Archivo CSV - 09 febrero")
  Mod01_newcod <- read.csv2(as.character(base03$x[i])) 
  if(i==3){names(Mod01_newcod)[317] <- "X_submission__id"}
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


write.table(reg_duplica_newcod, file="Biblioteca - Duplicados - 09 febrero.csv", sep = ";", row.names = FALSE)
write.table(reg_comparados, file="Biblioteca - Duplicados_Comparados - 09 febrero.csv", sep = ";", row.names = FALSE)

##################################################################

for (j in 1:1) {
  
  #######################################################################
  ########################### caracteristicas del ambiente ##############
  #######################################################################
  
  #If p4 = 1 then p5 ≠ ∅; If p4 = 2 then p5 = ∅ 
  
  fun01 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p4 == 1 & Mod01_newcod$p5 == "") 
                                | (Mod01_newcod$p4 == 2 & Mod01_newcod$p5 != ""), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p4 = 1 then p5 ≠ ∅; If p4 = 2 then p5 = ∅  - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun01()
  
  
  
  #If p1 = 2 then p6 ≠ ∅; If p1 = 1 then p6 = ∅ 
  
  fun02 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p1 == 2 & Mod01_newcod$p6 == "") 
                                | (Mod01_newcod$p1 == 1 & Mod01_newcod$p6 != ""), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p1 = 2 then p6 ≠ ∅; If p1 = 1 then p6 = ∅  - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun02()
  
  
  
  #If p3_9 = 1 then p3_ot ≠ ∅; If p3_9 = 0 then p3_ot = ∅ 
  
  fun03 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p3_9 == 1 & Mod01_newcod$p3_ot == "") 
                                | (Mod01_newcod$p3_9 == 0 & Mod01_newcod$p3_ot != ""), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p3_9 = 1 then p3_ot ≠ ∅; If p3_9 = 0 then p3_ot = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun03()
  
  
  #If p10_1 = 1 y p3_1, p3_3, p3_4, p3_6 o p3_7 = 1 then p12 ≠ ∅ 
  
  fun04 <- function() {
    Mod01_newcod$very <- ifelse(Mod01_newcod$p10_1 == 1 & (Mod01_newcod$p3_1 == 1 | Mod01_newcod$p3_3 == 1 | Mod01_newcod$p3_4 == 1 | Mod01_newcod$p3_6 == 1 | Mod01_newcod$p3_7 == 1) & Mod01_newcod$p12 == "", 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p10_1 = 1 y p3_1, p3_3, p3_4, p3_6 o p3_7 = 1 then p12 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun04()
  
  
  
  #If p3_1, p3_3, p3_4, p3_5, p3_6	, p3_7, p3_8 o p3_9 = 1 then p17 ≠ ∅; If p3_2 = 1 then p17 = ∅ 
  
  fun05 <- function() {
    Mod01_newcod$very <- ifelse(((Mod01_newcod$p3_1 == 1 | Mod01_newcod$p3_3 == 1 | Mod01_newcod$p3_4 == 1 | Mod01_newcod$p3_5 == 1 | Mod01_newcod$p3_6 == 1 | Mod01_newcod$p3_7 == 1 | Mod01_newcod$p3_8 == 1 | Mod01_newcod$p3_9 == 1) & is.na(Mod01_newcod$p17)) 
                                | (Mod01_newcod$p3_2 == 1 & !is.na(Mod01_newcod$p17)), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p3_1, p3_3, p3_4, p3_5, p3_6, p3_7, p3_8 o p3_9 = 1 then p17 ≠ ∅; If p3_2 = 1 then p17 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun05()
  
  
  
  #If p3_2 = 1 then p18_1_1, p18_1_2 o p18_1_3 ≠ ∅ 
  
  fun06 <- function() {
    Mod01_newcod$very <- ifelse(Mod01_newcod$p3_2 == 1 & (is.na(Mod01_newcod$p18_1_1) | is.na(Mod01_newcod$p18_1_2) | is.na(Mod01_newcod$p18_1_3)), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p3_2 = 1 then p18_1_1, p18_1_2 o p18_1_3 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun06()
  
  
  
  #If p20 = 1 then p21 ≠ ∅; If p20 = 2 then p21 = ∅ 
  
  fun07 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p20 == 1 & Mod01_newcod$p21 == "") 
                                | (Mod01_newcod$p20 == 2 & Mod01_newcod$p21 != ""), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p20 = 1 then p21 ≠ ∅; If p20 = 2 then p21 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun07()
  
  
  
  #If p22_0 = 1 then p23 y p24 = ∅; p22_0 = 0 then p23 y p24 ≠ ∅ 
  
  fun08 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p22_0 == 1 & (!is.na(Mod01_newcod$p23) | !is.na(Mod01_newcod$p24))) 
                                | (Mod01_newcod$p22_0 == 0 & (is.na(Mod01_newcod$p23) | is.na(Mod01_newcod$p24))), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p22_0 = 1 then p23 y p24 = ∅; p22_0 = 0 then p23 y p24 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun08()
  
  
  
  #If p22_5 = 1 then p22_ot ≠ ∅; If p22_5 = 0 then p22_ot = ∅ 
  
  fun09 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p22_5 == 1 & Mod01_newcod$p22_ot == "") 
                                | (Mod01_newcod$p22_5 == 0 & Mod01_newcod$p22_ot != ""), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p22_5 = 1 then p22_ot ≠ ∅; If p22_5 = 0 then p22_ot = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun09()
  
  
  
  #If p25_0 = 1 then p25_1a y p26 = ∅; If p25_0 = 1 then p25_1a ≠ ∅ 
  
  fun10 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p25_0 == 1 & (Mod01_newcod$p25_1a != "" | !is.na(Mod01_newcod$p26))) 
                                | (Mod01_newcod$p25_0 == 0 & Mod01_newcod$p25_1a == ""), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p25_0 = 1 then p25_1a y p26 = ∅; If p25_0 = 1 then p25_1a ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun10()
  
  
  
  #If p25_1_1 = 1 then p26 ≠ ∅; If p25_1_1 = 0 then p26 = ∅ 
  
  fun11 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p25_1_1 == 1 & is.na(Mod01_newcod$p26)) 
                                | (Mod01_newcod$p25_1_1 == 0 & !is.na(Mod01_newcod$p26)), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p25_1_1 = 1 then p26 ≠ ∅; If p25_1_1 = 0 then p26 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun11()
  
  
  
  #If p26_1 = 7 then p26_1_ot ≠ ∅; If p26_1 = 1, 2, 3, 4, 5 o 6 then p26_1_ot = ∅ 
  
  fun12 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p26_1 == 7 & Mod01_newcod$p26_1_ot == "") 
                                | ((Mod01_newcod$p26_1 == 1 | Mod01_newcod$p26_1 == 2 | Mod01_newcod$p26_1 == 3 | Mod01_newcod$p26_1 == 4 | Mod01_newcod$p26_1 == 5 | Mod01_newcod$p26_1 == 6) & Mod01_newcod$p26_1_ot != ""), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p26_1 = 7 then p26_1_ot ≠ ∅; If p26_1 = 1, 2, 3, 4, 5 o 6 then p26_1_ot = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun12()
  
  
  
  #If p27 = 1 then p28_1, p28_2, p28_3, p28_4 y p28_5 ≠ ∅; If p27 = 2 then p28_1, p28_2, p28_3, p28_4 y p28_5 = ∅ 
  
  fun13 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p27 == 1 & (Mod01_newcod$p28_1 == "" | Mod01_newcod$p28_2 == "" | Mod01_newcod$p28_3 == "" | Mod01_newcod$p28_4 == "" | Mod01_newcod$p28_5 == "")) 
                                | (Mod01_newcod$p27 == 2 & (Mod01_newcod$p28_1 != "" | Mod01_newcod$p28_2 != "" | Mod01_newcod$p28_3 != "" | Mod01_newcod$p28_4 != "" | Mod01_newcod$p28_5 != "")), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p27 = 1 then p28_1, p28_2, p28_3, p28_4 y p28_5 ≠ ∅; If p27 = 2 then p28_1, p28_2, p28_3, p28_4 y p28_5 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun13()
  
  
  
  #If p29 = 1 then p30_1, p30_2 y p30_3 ≠ ∅; If p29 = 2 then p30_1, p30_2 y p30_3 = ∅ 
  
  fun14 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p29 == 1 & (Mod01_newcod$p30_1 == "" | Mod01_newcod$p30_2 == "" | Mod01_newcod$p30_3 == "")) 
                                | (Mod01_newcod$p29 == 2 & (Mod01_newcod$p30_1 != "" | Mod01_newcod$p30_2 != "" | Mod01_newcod$p30_3 != "")), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p29 = 1 then p30_1, p30_2 y p30_3 ≠ ∅; If p29 = 2 then p30_1, p30_2 y p30_3 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun14()
  
  
  #######################################################################
  ##### Servicios básicos, conexión a internet y habitabilidad ##########
  #######################################################################
  
  
  #If p31 = 1 then p32 y p33 ≠ ∅; If p33 = 1 then p34 y p35 ≠ ∅; If p35 = 1 then p36 ≠ ∅ 
  
  fun15 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p31 == 1 & (Mod01_newcod$p32 == "" | is.na(Mod01_newcod$p33))) 
                                | (Mod01_newcod$p33 == 1 & (is.na(Mod01_newcod$p34) | is.na(Mod01_newcod$p35))) 
                                | (Mod01_newcod$p35 == 1 & Mod01_newcod$p36 == ""), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p31 = 1 then p32 y p33 ≠ ∅; If p33 = 1 then p34 y p35 ≠ ∅; If p35 = 1 then p36 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun15()
  
  
  
  #If p31 = 2 then p32, p33, p34, p35 y p36 = ∅ 
  
  fun16 <- function() {
    Mod01_newcod$very <- ifelse(Mod01_newcod$p31 == 2 & (Mod01_newcod$p32 != "" | !is.na(Mod01_newcod$p33) | !is.na(Mod01_newcod$p34) | !is.na(Mod01_newcod$p35) | Mod01_newcod$p36 != ""), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p31 = 2 then p32, p33, p34, p35 y p36 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun16()
  
  
  
  #If p39 = 1 o 2 then p39_1 y p40 = ∅; If p39 = 3 o 4 then p39_1 y p40 ≠ ∅;
  
  fun17 <- function() {
    Mod01_newcod$very <- ifelse(((Mod01_newcod$p39 == 1 | Mod01_newcod$p39 == 2) & (!is.na(Mod01_newcod$p39_1) | !is.na(Mod01_newcod$p40))) 
                                | ((Mod01_newcod$p39 == 3 | Mod01_newcod$p39 == 4) & (is.na(Mod01_newcod$p39_1) | is.na(Mod01_newcod$p40))), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p39 = 1 o 2 then p39_1 y p40 = ∅; If p39 = 3 o 4 then p39_1 y p40 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun17()
  

  #######################################################################
  ################ Equipos y mobiliarios ################################
  #######################################################################
  
  
  #If p45 = 1 then p45_1, p45_2, p45_3, p45_4_1 y p45_4_2 ≠ ∅; If p45_4_2 ≥ 1 then p45_5 ≠ ∅ 
  
  fun18 <- function() {
    Mod01_newcod_0_1 <- merge(Mod01_newcod_1,Mod01_newcod,by = "X_submission__id")
    Mod01_newcod_0_1$very <- ifelse((Mod01_newcod_0_1$p45 == 1 & (Mod01_newcod_0_1$p45_1 == "" | Mod01_newcod_0_1$p45_2 == "" | is.na(Mod01_newcod_0_1$p45_3) | is.na(Mod01_newcod_0_1$p45_4_1) | is.na(Mod01_newcod_0_1$p45_4_2))) 
                                | ((Mod01_newcod_0_1$p45_4_2 > 1 | Mod01_newcod_0_1$p45_4_2 == 1) & Mod01_newcod_0_1$p45_5 == ""), 1, 0)
    reporte <- Mod01_newcod_0_1 %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p45 = 1 then p45_1, p45_2, p45_3, p45_4_1 y p45_4_2 ≠ ∅; If p45_4_2 ≥ 1 then p45_5 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun18()
  
  
  
  #If p45 = 2 then p45_1, p45_2, p45_3, p45_4_1, p45_4_2 y p45_5 = ∅ 
  
  fun19 <- function() {
    Mod01_newcod_0_1 <- merge(Mod01_newcod_1,Mod01_newcod,by = "X_submission__id")
    Mod01_newcod_0_1$very <- ifelse(Mod01_newcod_0_1$p45 == 2 & (Mod01_newcod_0_1$p45_1 != "" | Mod01_newcod_0_1$p45_2 != "" | !is.na(Mod01_newcod_0_1$p45_3) | !is.na(Mod01_newcod_0_1$p45_4_1) | !is.na(Mod01_newcod_0_1$p45_4_2) | Mod01_newcod_0_1$p45_5 != ""), 1, 0)
    reporte <- Mod01_newcod_0_1 %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p45 = 2 then p45_1, p45_2, p45_3, p45_4_1, p45_4_2 y p45_5 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun19()
  
  
  
  #If p45_5_8 = 1 then p45_5_ot ≠ ∅; If p45_5_8 = 0 then p45_5_ot = ∅ 
  
  fun20 <- function() {
    Mod01_newcod_0_1 <- merge(Mod01_newcod_1,Mod01_newcod,by = "X_submission__id")
    Mod01_newcod_0_1$very <- ifelse((Mod01_newcod_0_1$p45_5_8 == 1 & Mod01_newcod_0_1$p45_5_ot == "") 
                                    | (Mod01_newcod_0_1$p45_5_8 == 0 & Mod01_newcod_0_1$p45_5_ot != ""), 1, 0)
    reporte <- Mod01_newcod_0_1 %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p45_5_8 = 1 then p45_5_ot ≠ ∅; If p45_5_8 = 0 then p45_5_ot = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun20()
  
  
  
  #If p46 = 1 then p46_1, p46_2, 46_3, p46_4_1, p46_4_2 y p46_4_3 ≠ ∅ 
  
  fun21 <- function() {
    Mod01_newcod_0_2 <- merge(Mod01_newcod_2,Mod01_newcod,by = "X_submission__id")
    Mod01_newcod_0_2$very <- ifelse(Mod01_newcod_0_2$p46 == 1 & (Mod01_newcod_0_2$p46_1 == "" | Mod01_newcod_0_2$p46_2 == "" | is.na(Mod01_newcod_0_2$p46_3) | is.na(Mod01_newcod_0_2$p46_4_1) | is.na(Mod01_newcod_0_2$p46_4_2) | is.na(Mod01_newcod_0_2$p46_4_3)), 1, 0)
    reporte <- Mod01_newcod_0_2 %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p46 = 1 then p46_1, p46_2, 46_3, p46_4_1, p46_4_2 y p46_4_3 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun21()
  
  
  
  #If p46 = 2 then p46_1, p46_2, 46_3, p46_4_1, p46_4_2 y p46_4_3 = ∅
  
  fun22 <- function() {
    Mod01_newcod_0_2 <- merge(Mod01_newcod_2,Mod01_newcod,by = "X_submission__id")
    Mod01_newcod_0_2$very <- ifelse(Mod01_newcod_0_2$p46 == 2 & (Mod01_newcod_0_2$p46_1 != "" | Mod01_newcod_0_2$p46_2 != "" | !is.na(Mod01_newcod_0_2$p46_3) | !is.na(Mod01_newcod_0_2$p46_4_1) | !is.na(Mod01_newcod_0_2$p46_4_2) | !is.na(Mod01_newcod_0_2$p46_4_3)), 1, 0)
    reporte <- Mod01_newcod_0_2 %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p46 = 2 then p46_1, p46_2, 46_3, p46_4_1, p46_4_2 y p46_4_3 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun22()
  
  
  #######################################################################
  ################ Tratamiento de residuos ##############################
  #######################################################################
  
  
  #If p47_1_1 = 1 then p47_1_2 y p47_1_3 ≠ ∅; If p47_1_1 = 2 then p47_1_2 y p47_1_3 = ∅ 
  
  fun23 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p47_1_1 == 1 & (is.na(Mod01_newcod$p47_1_2) | Mod01_newcod$p47_1_3 == "")) 
                                | (Mod01_newcod$p47_1_1 == 2 & (!is.na(Mod01_newcod$p47_1_2) | Mod01_newcod$p47_1_3 != "")), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p47_1_1 = 1 then p47_1_2 y p47_1_3 ≠ ∅; If p47_1_1 = 2 then p47_1_2 y p47_1_3 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun23()
  
  
  #If p47_2_1 = 1 then p47_2_2 y p47_2_3 ≠ ∅; If p47_2_1 = 2 then p47_2_2 y p47_2_3 = ∅ 
  
  fun24 <- function() {
    Mod01_newcod$very <- ifelse((Mod01_newcod$p47_2_1 == 1 & (is.na(Mod01_newcod$p47_2_2) | Mod01_newcod$p47_2_3 == "")) 
                                | (Mod01_newcod$p47_2_1 == 2 & (!is.na(Mod01_newcod$p47_2_2) | Mod01_newcod$p47_2_3 != "")), 1, 0)
    reporte <- Mod01_newcod %>% filter(very == 1)
    if (nrow(reporte) > 0) {
      write.table(reporte, file="Biblioteca - If p47_2_1 = 1 then p47_2_2 y p47_2_3 ≠ ∅; If p47_2_1 = 2 then p47_2_2 y p47_2_3 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
    } else {
      print(nrow(reporte))
    }
    
  }
  
  fun24()
  
  
}
