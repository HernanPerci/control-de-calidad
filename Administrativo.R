library(dplyr)
library(data.table)


for (i0 in 1:1) {
  base_pasada <- "D:/R-Perci/control-de-calidad/Archivo CSV - 08 febrero"
  base_actual_acumulada <- "D:/R-Perci/control-de-calidad/Archivo CSV - 09 febrero"
  
  ##################### cargando códigos de base pasada ########################
  base01_past <- data.frame(x=dir(print(base_pasada)))
  base03_past <- base01_past%>%filter(grepl('ADMINISTRATIVO', x))
  
  ########################## cargando base actualizada acumulada #########################
  base01 <- data.frame(x=dir(print(base_actual_acumulada)))
  base03 <- base01%>%filter(grepl('ADMINISTRATIVO', x))
  
  ################################################################################
  ############### Cargando bases de datos y filtrando nuevos registros  ##########
  ################################################################################
}

for (i in 1:nrow(base03)) {

  setwd("D:/R-Perci/control-de-calidad/Archivo CSV - 09 febrero")
  Mod01_newcod <- read.csv2(as.character(base03$x[i])) 
  if(i==3){names(Mod01_newcod)[282] <- "X_submission__id"}
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


write.table(reg_duplica_newcod, file="Administrativo - Duplicados - 09 febrero.csv", sep = ";", row.names = FALSE)
write.table(reg_comparados, file="Administrativo - Duplicados_Comparados - 09 febrero.csv", sep = ";", row.names = FALSE)


#######################################################################
########################### caracteristicas generales #################
#######################################################################

for (j in 1:1) {

###If p10 =1 then p11 ≠ ∅; If p10 = 2 then p11 = ∅;  

fun01 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p10 == 1 & Mod01_newcod$p11 == "") 
                              | (Mod01_newcod$p10 == 2 & Mod01_newcod$p11!= ""), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p10 =1 then p11 ≠ ∅; If p10 = 2 then p11 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun01()


###If p12_0 = 1 then p13 y p14 = ∅; If p12_1, p12_2, p12_3, p12_4 o p12_5 = 1 then p13 y p14 ≠ ∅ 

fun02 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p12_0 == 1 & (!is.na(Mod01_newcod$p13) | !is.na(Mod01_newcod$p14))) 
                              | ((Mod01_newcod$p12_1 == 1 | Mod01_newcod$p12_2 == 1 |Mod01_newcod$p12_3 == 1 |Mod01_newcod$p12_4 == 1 |Mod01_newcod$p12_5 == 1) & (is.na(Mod01_newcod$p13) | is.na(Mod01_newcod$p14))), 1, 0)
  reporte <- Mod01_newcod %>% filter(very == 1)
  
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p12_0 = 1 then p13 y p14 = ∅; If p12_1, p12_2, p12_3, p12_4 o p12_5 = 1 then p13 y p14 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun02()


###If p15_0 = 1 then p16 y p17 = ∅; If p15_1, p15_2, p15_3, p15_4, p15_5 o p15_6 = 1 then p16 y p17 ≠ ∅ 

fun03 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p15_0 == 1 & (Mod01_newcod$p16 != "" | !is.na(Mod01_newcod$p17))) 
                              | ((Mod01_newcod$p15_1 == 1 | Mod01_newcod$p15_2 == 1 |Mod01_newcod$p15_3 == 1 |Mod01_newcod$p15_4 == 1 |Mod01_newcod$p15_5 == 1 |Mod01_newcod$p15_6 == 1) & (Mod01_newcod$p16 == "" | is.na(Mod01_newcod$p17))), 1, 0)
  reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
  
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p15_0 = 1 then p16 y p17 = ∅; If p15_1, p15_2, p15_3, p15_4, p15_5 o p15_6 = 1 then p16 y p17 ≠ ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun03()



###If p16_1 = 1 then p17 ≠ ∅; If p16_0, p16_2, p16_3 o p16_4 = 1 then p17 = ∅ 

fun04 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p16_1 == 1 & is.na(Mod01_newcod$p17)) 
                              | ((Mod01_newcod$p16_0 == 1 | Mod01_newcod$p16_2 == 1 |Mod01_newcod$p16_3 == 1 |Mod01_newcod$p16_4 == 1) & !is.na(Mod01_newcod$p17)), 1, 0)
  reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
  
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p16_1 = 1 then p17 ≠ ∅; If p16_0, p16_2, p16_3 o p16_4 = 1 then p17 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun04()



###If p19 = 1 then p20_1, p20_2, p20_3, p20_4 y p20_5 ≠ ∅; If p19 = 2 then p20_1, p20_2, p20_3, p20_4 y p20_5 = ∅ 

fun05 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p19 == 1 & (Mod01_newcod$p20_1 == "" | Mod01_newcod$p20_2 == "" | Mod01_newcod$p20_3 == "" | Mod01_newcod$p20_4 == "" | Mod01_newcod$p20_5 == ""))
                              | (Mod01_newcod$p19 == 2 & (Mod01_newcod$p20_1 != "" | Mod01_newcod$p20_2 != "" | Mod01_newcod$p20_3 != "" | Mod01_newcod$p20_4 != "" | Mod01_newcod$p20_5 != "")), 1, 0)
  reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
  
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p19 = 1 then p20_1, p20_2, p20_3, p20_4 y p20_5 ≠ ∅; If p19 = 2 then p20_1, p20_2, p20_3, p20_4 y p20_5 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun05()



###If p21 = 1 then p22_1, p22_2 y p22_3 ≠ ∅; If p21 = 2 then p22_1, p22_2 y p22_3 = ∅; 

fun06 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p21 == 1 & (Mod01_newcod$p22_1 == "" | Mod01_newcod$p22_2 == "" | Mod01_newcod$p22_3 == "")) 
                              |(Mod01_newcod$p21 == 2 & (Mod01_newcod$p22_1 != "" | Mod01_newcod$p22_2 != "" | Mod01_newcod$p22_3 != "")), 1, 0)
  reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
  
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p21 = 1 then p22_1, p22_2 y p22_3 ≠ ∅; If p21 = 2 then p22_1, p22_2 y p22_3 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun06()

#######################################################################
########################### Servicios b?sicos, conexi?n a internet y habitabilidad #################
#######################################################################


#If p23 = 1 then p24, p25, p26, p27, p28 y p29 ≠ ∅; If p23 = 2 then p24, p25, p26, p27, p28 y p29 = ∅ 

fun07 <- function() {
  Mod01_newcod$very <- ifelse(Mod01_newcod$p23 == 1 & (Mod01_newcod$p24 == "" | is.na(Mod01_newcod$p25) | is.na(Mod01_newcod$p26) | is.na(Mod01_newcod$p27) | Mod01_newcod$p28 == "" | is.na(Mod01_newcod$p29)) 
                              | (Mod01_newcod$p23 == 2 & (Mod01_newcod$p24 != "" | !is.na(Mod01_newcod$p25) | !is.na(Mod01_newcod$p26) | !is.na(Mod01_newcod$p27) | Mod01_newcod$p28 != "" | !is.na(Mod01_newcod$p29))), 1, 0)
  reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)

  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p23 = 1 then p24, p25, p26, p27, p28 y p29 ≠ ∅; If p23 = 2 then p24, p25, p26, p27, p28 y p29 = ∅  - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun07()


#If p25 = 1 then p26, p27 y p28 ≠ ∅; If p25 = 2 then p26, p27 y p28 = ∅ 

fun08 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p25 == 1 & (is.na(Mod01_newcod$p26) | is.na(Mod01_newcod$p27) | Mod01_newcod$p28 == "")) 
                              | (Mod01_newcod$p25 == 2 & (!is.na(Mod01_newcod$p26) | !is.na(Mod01_newcod$p27) | Mod01_newcod$p28 != "")), 1, 0)
  reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p25 = 1 then p26, p27 y p28 ≠ ∅; If p25 = 2 then p26, p27 y p28 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun08()

#If p27 = 1 then p28 ≠ ∅; If p27 = 2 then p28 = ∅

fun09 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p27 == 1 & Mod01_newcod$p28 == "") 
                                  | (Mod01_newcod$p27 == 2 & Mod01_newcod$p28 != ""), 1, 0)
  reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p27 = 1 then p28 ≠ ∅; If p27 = 2 then p28 = ∅  - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun09()



#If p30 = 1 then p31 y p32 ≠ ∅; If p30 = 2 then p31 y p32 = ∅ 

fun10 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p30 == 1 & (Mod01_newcod$p31 == "" | Mod01_newcod$p32 == "")) 
                              | (Mod01_newcod$p30 == 2 & (Mod01_newcod$p31 != "" | Mod01_newcod$p32 != "")), 1, 0)
  reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p30 = 1 then p31 y p32 ≠ ∅; If p30 = 2 then p31 y p32 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun10()


#If p34 = 1 o 2 then p35 y p36 = ∅; If p34 = 3 o 4 then p35 y p36 ≠ ∅ 

fun11 <- function() {
  Mod01_newcod$very <- ifelse(((Mod01_newcod$p34 == 1 | Mod01_newcod$p34 == 2) & (Mod01_newcod$p35 != "" | !is.na(Mod01_newcod$p36))) 
                              | ((Mod01_newcod$p34 == 3 | Mod01_newcod$p34 == 4) & (Mod01_newcod$p35 == "" | is.na(Mod01_newcod$p36))), 1, 0)
  reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p30 = 1 then p31 y p32 ≠ ∅; If p30 = 2 then p31 y p32 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun11()

#######################################################################
########################### Equipos y mobiliarios #################
#######################################################################


#If p38 = 1 then p38_1, p38_2, p38_3	, p38_4_1 y p38_4_2 ≠ ∅; If p38 = 2 then p38_1, p38_2, p38_3	, p38_4_1 y p38_4_2 = ∅ 

fun12 <- function() {
  Mod01_newcod_0_1 <- merge(Mod01_newcod_1,Mod01_newcod,by = "X_submission__id")
  Mod01_newcod_0_1$very <- ifelse((Mod01_newcod_0_1$p38 == 1 & (Mod01_newcod_0_1$p38_1 == "" | Mod01_newcod_0_1$p38_2 == "" | is.na(Mod01_newcod_0_1$p38_3) | is.na(Mod01_newcod_0_1$p38_4_1) | is.na(Mod01_newcod_0_1$p38_4_2))) 
                              | (Mod01_newcod_0_1$p38 == 2 & (Mod01_newcod_0_1$p38_1 != "" | Mod01_newcod_0_1$p38_2 != "" | !is.na(Mod01_newcod_0_1$p38_3) | !is.na(Mod01_newcod_0_1$p38_4_1) | !is.na(Mod01_newcod_0_1$p38_4_2))), 1, 0)
  reporte <- Mod01_newcod_0_1 %>% filter(Mod01_newcod_0_1$very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p38 = 1 then p38_1, p38_2, p38_3, p38_4_1 y p38_4_2 ≠ ∅; If p38 = 2 then p38_1, p38_2, p38_3, p38_4_1 y p38_4_2 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun12()


#If p38_4_2 >=  1 then p38_5 ≠ ∅; If p38_4_2 = 0 then p38_5 = ∅ 

fun13 <- function() {
  Mod01_newcod_1$very <- ifelse(((Mod01_newcod_1$p38_4_2 > 1 | Mod01_newcod_1$p38_4_2 == 1) & Mod01_newcod_1$p38_5 == "") 
                            | (Mod01_newcod_1$p38_4_2 == 0 & Mod01_newcod_1$p38_5 != ""),1 ,0)
  reporte <- Mod01_newcod_1 %>% filter(Mod01_newcod_1$very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p38_4_2 >= 1 then p38_5 ≠ ∅; If p38_4_2 = 0 then p38_5 = ∅  - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun13()


#If p39 = 1 then p39_1, p39_2, p39_3 y p39_4 ≠ ∅; If p39 = 2 then p39_1, p39_2, p39_3 y p39_4 = ∅; 

fun14 <- function() {
  Mod01_newcod_0_2 <- merge(Mod01_newcod_2,Mod01_newcod,by = "X_submission__id")
  Mod01_newcod_0_2$very <- ifelse((Mod01_newcod_0_2$p39 == 1 & (Mod01_newcod_0_2$p39_1 == "" | Mod01_newcod_0_2$p39_2 == "" | is.na(Mod01_newcod_0_2$p39_3) | is.na(Mod01_newcod_0_2$p39_4_1) | is.na(Mod01_newcod_0_2$p39_4_2) | is.na(Mod01_newcod_0_2$p39_4_3))) 
                                  | (Mod01_newcod_0_2$p39 == 2 & (Mod01_newcod_0_2$p39_1 != "" | Mod01_newcod_0_2$p39_2 != "" | !is.na(Mod01_newcod_0_2$p39_3) | !is.na(Mod01_newcod_0_2$p39_4_1) | !is.na(Mod01_newcod_0_2$p39_4_2) | !is.na(Mod01_newcod_0_2$p39_4_3))), 1, 0)
  reporte <- Mod01_newcod_0_2 %>% filter(Mod01_newcod_0_2$very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p39 = 1 then p39_1, p39_2, p39_3 y p39_4 ≠ ∅; If p39 = 2 then p39_1, p39_2, p39_3 y p39_4 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun14()

#######################################################################
########################### Tratamiento de residuos #################
#######################################################################

#If p40_1_1 = 1 then p40_1_2 y p40_1_3 ≠ ∅; If p40_1_1 = 2 then p40_1_2 y p40_1_3 = ∅; 

fun15 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p40_1_1 == 1 & (is.na(Mod01_newcod$p40_1_2) | Mod01_newcod$p40_1_3 == "")) 
                              | (Mod01_newcod$p40_1_1 == 2 & (!is.na(Mod01_newcod$p40_1_2) | Mod01_newcod$p40_1_3 != "")), 1, 0)
  reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p40_1_1 = 1 then p40_1_2 y p40_1_3 ≠ ∅; If p40_1_1 = 2 then p40_1_2 y p40_1_3 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun15()

#If p40_2_1 = 1 then p40_2_2 y p40_2_3 ≠ ∅; If p40_2_1 = 2 then p40_2_2 y p40_2_3 = ∅; 

fun16 <- function() {
  Mod01_newcod$very <- ifelse((Mod01_newcod$p40_2_1 == 1 & (is.na(Mod01_newcod$p40_2_2) | Mod01_newcod$p40_2_3 == "")) 
                              | (Mod01_newcod$p40_2_1 == 2 & (!is.na(Mod01_newcod$p40_2_2) | Mod01_newcod$p40_2_3 != "")), 1, 0)
  reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
  if (nrow(reporte) > 0) {
    write.table(reporte, file="Administrativo - If p40_2_1 = 1 then p40_2_2 y p40_2_3 ≠ ∅; If p40_2_1 = 2 then p40_2_2 y p40_2_3 = ∅ - 09 febrero.csv", sep = ";", row.names = FALSE)
  } else {
    print(nrow(reporte))
  }
  
}

fun16()

}

