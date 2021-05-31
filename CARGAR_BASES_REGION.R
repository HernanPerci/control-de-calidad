################################################################################
#.................      SCRIPT PARA DETECTAR ERRORES        ...................#
################################################################################

###############################  LIBRERIAS  ####################################

library(openxlsx)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)


for (k in c('CAP05', 'CAP02', 'CAP07', 'CAP08', 'CAP12')) {
  

################################################################################
###############################  FUNCIONES  ####################################
################################################################################
# rm(list = ls())
for (i0 in 1:1) {
  #### funcion para extraer documentos en formato *.xlsx
  extraer_excel <- function(ruta,base,i) {
    setwd(print(ruta))
    base01 <- read.xlsx(as.character(base$x[i]),rowNames = F) 
    name_col <- data.frame(name_col=colnames(base01))
    name_col <- cbind(name_col,data.frame(num_col=1:ncol(base01)))
    names(base01)[which(grepl("_id", name_col$name_col))] <- "X_submission__id"
    base01
  }
  
  
  ##### funcion para elegir modulo a analizar
  elegir_modulo <- function(ruta,modulo) {
    base01 <- data.frame(x=dir(print(ruta)))
    base02 <- base01%>%filter(grepl(modulo, x))
    base02
  }
  
  #### Funcion de concatenaciÃ³n para identificar duplicados
  paste_cod <- function(base,sep=", ") { gsub(", " ,sep, toString(base[] ) ) }
  
  #### Funcion para concatenar columnas para generar columna ID
  concatenar_newcod <- function(docs) {
    aa <- data.frame(names_col=colnames(docs))
    A <-  which( grepl('cod|piso',aa$names_col)& 
                   !grepl("cod_informante", aa$names_col, ignore.case=TRUE))
    cod_dif <- apply( docs[ , A] , 1 , paste_cod , sep="-")
    docs <- cbind(docs,cod_dif)
  }
  
  ###funciÃ³n para leer duplicados
  duplicados <- function(base_mod) {
    registros <- base_mod%>%group_by(cod_dif)%>%count(cod_dif)  #lista de n_veces de codigos registrados
    reg_duplicados <- registros[registros$n >1,] # cÃ³digos de registro repetidos/aÃ±adir toda la linea
    cod_reg_dupli <- reg_duplicados$cod_dif
    reg_duplica_newcod <- base_mod%>%filter(cod_dif%in%cod_reg_dupli ==T)
  }
  
  #### Funcion para dentificar nuevos registros (se debe introducir los 5 : 
  #ruta_pasada,ruta_acumulada,modulo,region,i)
  #### El argumento i debe dejarse tal cual como una vocal
  
  #### El argumento i debe dejarse tal cual como una vocal
  nuevos_registros <- function(ruta_pasada,ruta_acumulada,modulo,region,i) {
    if(!is.na(ruta_pasada)==T){
      setwd(print(ruta_pasada))
      Mod_base <- elegir_modulo(ruta_pasada,modulo)
      docs <-  documentos_de_modulo(ruta_pasada,Mod_base,i)
      aa <- data.frame(names_col=colnames(docs))
      names(docs)[which(grepl("X_id|X_submission__id", aa$names_col))] <- "X_submission__id"
      docs <- concatenar_newcod(docs)
      if(i==nrow(Mod_base)){
        A <- transform(docs, ID2=ave(cod_mod, cod_mod, FUN=seq_along))
        B <- transform(region, ID2=ave(cod_mod, cod_mod, FUN=seq_along))
        docs <- merge(A, B, all.x=TRUE, sort=FALSE)
      }
      cod_reg <- docs$X_submission__id 
      
      setwd(print(ruta_acumulada))
      Mod_base <- elegir_modulo(ruta_acumulada,modulo)
      docs <-  documentos_de_modulo(ruta_acumulada,Mod_base,i)
      aa <- data.frame(names_col=colnames(docs))
      names(docs)[which(grepl("X_id|X_submission__id", aa$names_col))] <- "X_submission__id"
      docs <- concatenar_newcod(docs)
      if(i==nrow(Mod_base)){
        A <- transform(docs, ID2=ave(cod_mod, cod_mod, FUN=seq_along))
        B <- transform(region, ID2=ave(cod_mod, cod_mod, FUN=seq_along))
        docs <- merge(A, B, all.x=TRUE, sort=FALSE)
      }
      Mod01_newcod <- docs%>%filter(X_submission__id%in% cod_reg==F)
      
    } else{cod_reg <- NA
    
    setwd(print(ruta_acumulada))
    Mod_base <- elegir_modulo(ruta_acumulada,modulo)
    docs <-  documentos_de_modulo(ruta_acumulada,Mod_base,i)
    aa <- data.frame(names_col=colnames(docs))
    names(docs)[which(grepl("X_id|X_submission__id", aa$names_col))] <- "X_submission__id"
    docs <- concatenar_newcod(docs)
    if(i==nrow(Mod_base)){
      A <- transform(docs, ID2=ave(cod_mod, cod_mod, FUN=seq_along))
      B <- transform(region, ID2=ave(cod_mod, cod_mod, FUN=seq_along))
      docs <- merge(A, B, all.x=TRUE, sort=FALSE)
    }
    Mod01_newcod <- docs%>%filter(X_submission__id%in% cod_reg==F)
    
    }
    
  }
  
  
  
  documentos_de_modulo <- function(ruta,base,i) {
    setwd(print(ruta))
    Mod01_newcod <- read.csv2(as.character(base$x[i])) 
    name_col <- data.frame(name_col=colnames(Mod01_newcod))
    name_col <- cbind(name_col,data.frame(num_col=1:ncol(Mod01_newcod)))
    names(Mod01_newcod)[which(grepl("_id", name_col$name_col))] <- "X_submission__id"
    Mod01_newcod
  }
  
  
  ### Funcion para evaluar variable policotomica
  evaluar_policotomica <- function(docs,variable) {
    aa <- data.frame(names_col=colnames(docs))
    A <-  which( grepl(variable,aa$names_col)&
                   !grepl("_ot", aa$names_col, ignore.case=TRUE))
    variable_poli <-  rowSums (docs[ , min(A):max(A)])
    docs <- cbind(docs,variable_poli)
  }
  
  
  ##### Funcion para comparar registros 
  
  comparar_registros <- function(reg_duplicados,cod_reg_dupli,i) {
    reg_duplica_compare <- reg_duplicados%>%filter(cod_dif%in%cod_reg_dupli[i] ==T)
    compare_t <- data.table(t(reg_duplica_compare))
    compare_t$compare <- ifelse(compare_t$V1==compare_t$V2,0,1)
    names(compare_t)[(nrow(reg_duplica_compare)+1)]<- paste(cod_reg_dupli[i],"R", sep="_")
    compare_t
  }
  
  
  ### Funcion para guardar errores o reg duplicados
  guardar_error <- function(reg_error,condicion,modulo,fecha) {
    if(nrow(reg_error) > 0){
      write.xlsx(reg_error,paste(condicion,modulo,fecha,"xlsx",sep = '.'))
    }
  }
  
}


################################################################################
########################   Cargando Bases de datos    ##########################
################################################################################

# rm(list = setdiff(ls(), lsf.str()))
fecha <- '15032021'
base_pro <- read.xlsx('D:/R-Perci/control-de-calidad/programacion.xlsx')
ruta_pasada <- NA
ruta_acumulada <- "D:/R-Perci/control-de-calidad/Archivo CSV - 15 marzo"
modulo <- k

region <- base_pro[,1:5]
names(region)[1] <- 'cod_mod'
region$cod_mod <- as.numeric(region$cod_mod)

  
  for (i in 1:nrow(elegir_modulo(ruta_acumulada,modulo))) {
    Mod01_newcod <- nuevos_registros(ruta_pasada,ruta_acumulada,modulo,region,i)
    assign(paste("Mod01_newcod", i,sep = "_"), Mod01_newcod)
  }


Mod01_newcod$DEPARTAMENTO <- with(Mod01_newcod, region$DEPARTAMENTO[match(cod_mod,region$cod_mod)])


# Los resultados de abajo estan con fechas anteriores pero se pueden cambiar
# con la herramienta Find/Replace del RStudio
if (modulo == 'CAP05') {
  for (j in 1:1) {
    
    ###If p10 =1 then p11 ≠ ∅; If p10 = 2 then p11 = ∅;  
    
    fun01 <- function() {
      Mod01_newcod$very <- ifelse((Mod01_newcod$p10 == 1 & Mod01_newcod$p11 == "") 
                                  | (Mod01_newcod$p10 == 2 & Mod01_newcod$p11 != ""), 1, 0)
      reporte <- Mod01_newcod %>% filter(very == 1)
      if (nrow(reporte) > 0) {
        write.table(reporte, file="Administrativo - If p10 =1 then p11 ≠ ∅; If p10 = 2 then p11 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Administrativo - If p12_0 = 1 then p13 y p14 = ∅; If p12_1, p12_2, p12_3, p12_4 o p12_5 = 1 then p13 y p14 ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun02()
    
    
    ###If p15_0 = 1 then p16 y p17 = ∅; If p15_1, p15_2, p15_3, p15_4, p15_5 o p15_6 = 1 then p16 ≠ ∅ 
    
    fun03 <- function() {
      Mod01_newcod$very <- ifelse((Mod01_newcod$p15_0 == 1 & (Mod01_newcod$p16 != "" | !is.na(Mod01_newcod$p17))) 
                                  | ((Mod01_newcod$p15_1 == 1 | Mod01_newcod$p15_2 == 1 |Mod01_newcod$p15_3 == 1 |Mod01_newcod$p15_4 == 1 |Mod01_newcod$p15_5 == 1 |Mod01_newcod$p15_6 == 1) & Mod01_newcod$p16 == ""), 1, 0)
      reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
      
      if (nrow(reporte) > 0) {
        write.table(reporte, file="Administrativo - If p15_0 = 1 then p16 y p17 = ∅; If p15_1, p15_2, p15_3, p15_4, p15_5 o p15_6 = 1 then p16 ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun03()
    
    
    
    ###If p16_1 = 1 then p17 ≠ ∅; If p16_0, p16_2, p16_3 o p16_4 = 1 and p16_1 = 0 then p17 = ∅ 
    
    fun04 <- function() {
      Mod01_newcod$very <- ifelse((Mod01_newcod$p16_1 == 1 & is.na(Mod01_newcod$p17)) 
                                  | (((Mod01_newcod$p16_0 == 1 | Mod01_newcod$p16_2 == 1 |Mod01_newcod$p16_3 == 1 |Mod01_newcod$p16_4 == 1) & Mod01_newcod$p16_1 == 0) & !is.na(Mod01_newcod$p17)), 1, 0)
      reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
      
      if (nrow(reporte) > 0) {
        write.table(reporte, file="Administrativo - If p16_1 = 1 then p17 ≠ ∅; If p16_0, p16_2, p16_3 o p16_4 = 1 and p16_1 = 0 then p17 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Administrativo - If p19 = 1 then p20_1, p20_2, p20_3, p20_4 y p20_5 ≠ ∅; If p19 = 2 then p20_1, p20_2, p20_3, p20_4 y p20_5 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Administrativo - If p21 = 1 then p22_1, p22_2 y p22_3 ≠ ∅; If p21 = 2 then p22_1, p22_2 y p22_3 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun06()
    
    #######################################################################
    ########################### Servicios b?sicos, conexi?n a internet y habitabilidad #################
    #######################################################################
    
    
    #If p23 = 1 then p24, p25 ≠ ∅; If p23 = 2 then p24, p25, p26, p27, p28 y p29 = ∅ 
    
    fun07 <- function() {
      Mod01_newcod$very <- ifelse(Mod01_newcod$p23 == 1 & (Mod01_newcod$p24 == "" | is.na(Mod01_newcod$p25)) 
                                  | (Mod01_newcod$p23 == 2 & (Mod01_newcod$p24 != "" | !is.na(Mod01_newcod$p25) | !is.na(Mod01_newcod$p26) | !is.na(Mod01_newcod$p27) | Mod01_newcod$p28 != "" | !is.na(Mod01_newcod$p29))), 1, 0)
      reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
      
      if (nrow(reporte) > 0) {
        write.table(reporte, file="Administrativo - If p23 = 1 then p24, p25 ≠ ∅; If p23 = 2 then p24, p25, p26, p27, p28 y p29 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun07()
    
    
    #If p25 = 1 then p26, p27 ≠ ∅; If p25 = 2 then p26, p27 y p28 = ∅ 
    
    fun08 <- function() {
      Mod01_newcod$very <- ifelse((Mod01_newcod$p25 == 1 & (is.na(Mod01_newcod$p26) | is.na(Mod01_newcod$p27))) 
                                  | (Mod01_newcod$p25 == 2 & (!is.na(Mod01_newcod$p26) | !is.na(Mod01_newcod$p27) | Mod01_newcod$p28 != "")), 1, 0)
      reporte <- Mod01_newcod %>% filter(Mod01_newcod$very == 1)
      if (nrow(reporte) > 0) {
        write.table(reporte, file="Administrativo - If p25 = 1 then p26, p27 ≠ ∅; If p25 = 2 then p26, p27 y p28 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Administrativo - If p27 = 1 then p28 ≠ ∅; If p27 = 2 then p28 = ∅  - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Administrativo - If p30 = 1 then p31 y p32 ≠ ∅; If p30 = 2 then p31 y p32 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Administrativo - If p30 = 1 then p31 y p32 ≠ ∅; If p30 = 2 then p31 y p32 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Administrativo - If p38 = 1 then p38_1, p38_2, p38_3, p38_4_1 y p38_4_2 ≠ ∅; If p38 = 2 then p38_1, p38_2, p38_3, p38_4_1 y p38_4_2 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Administrativo - If p38_4_2 >= 1 then p38_5 ≠ ∅; If p38_4_2 = 0 then p38_5 = ∅  - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Administrativo - If p39 = 1 then p39_1, p39_2, p39_3 y p39_4 ≠ ∅; If p39 = 2 then p39_1, p39_2, p39_3 y p39_4 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Administrativo - If p40_1_1 = 1 then p40_1_2 y p40_1_3 ≠ ∅; If p40_1_1 = 2 then p40_1_2 y p40_1_3 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Administrativo - If p40_2_1 = 1 then p40_2_2 y p40_2_3 ≠ ∅; If p40_2_1 = 2 then p40_2_2 y p40_2_3 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun16()
    
  }
  
} else if (modulo == 'CAP02') {
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
        write.table(reporte, file="Biblioteca - If p4 = 1 then p5 ≠ ∅; If p4 = 2 then p5 = ∅  - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p1 = 2 then p6 ≠ ∅; If p1 = 1 then p6 = ∅  - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p3_9 = 1 then p3_ot ≠ ∅; If p3_9 = 0 then p3_ot = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p10_1 = 1 y p3_1, p3_3, p3_4, p3_6 o p3_7 = 1 then p12 ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p3_1, p3_3, p3_4, p3_5, p3_6, p3_7, p3_8 o p3_9 = 1 then p17 ≠ ∅; If p3_2 = 1 then p17 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p3_2 = 1 then p18_1_1, p18_1_2 o p18_1_3 ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p20 = 1 then p21 ≠ ∅; If p20 = 2 then p21 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p22_0 = 1 then p23 y p24 = ∅; p22_0 = 0 then p23 y p24 ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p22_5 = 1 then p22_ot ≠ ∅; If p22_5 = 0 then p22_ot = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p25_0 = 1 then p25_1a y p26 = ∅; If p25_0 = 1 then p25_1a ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p25_1_1 = 1 then p26 ≠ ∅; If p25_1_1 = 0 then p26 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p26_1 = 7 then p26_1_ot ≠ ∅; If p26_1 = 1, 2, 3, 4, 5 o 6 then p26_1_ot = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p27 = 1 then p28_1, p28_2, p28_3, p28_4 y p28_5 ≠ ∅; If p27 = 2 then p28_1, p28_2, p28_3, p28_4 y p28_5 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p29 = 1 then p30_1, p30_2 y p30_3 ≠ ∅; If p29 = 2 then p30_1, p30_2 y p30_3 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p31 = 1 then p32 y p33 ≠ ∅; If p33 = 1 then p34 y p35 ≠ ∅; If p35 = 1 then p36 ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p31 = 2 then p32, p33, p34, p35 y p36 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p39 = 1 o 2 then p39_1 y p40 = ∅; If p39 = 3 o 4 then p39_1 y p40 ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p45 = 1 then p45_1, p45_2, p45_3, p45_4_1 y p45_4_2 ≠ ∅; If p45_4_2 ≥ 1 then p45_5 ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p45 = 2 then p45_1, p45_2, p45_3, p45_4_1, p45_4_2 y p45_5 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p45_5_8 = 1 then p45_5_ot ≠ ∅; If p45_5_8 = 0 then p45_5_ot = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p46 = 1 then p46_1, p46_2, 46_3, p46_4_1, p46_4_2 y p46_4_3 ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p46 = 2 then p46_1, p46_2, 46_3, p46_4_1, p46_4_2 y p46_4_3 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p47_1_1 = 1 then p47_1_2 y p47_1_3 ≠ ∅; If p47_1_1 = 2 then p47_1_2 y p47_1_3 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Biblioteca - If p47_2_1 = 1 then p47_2_2 y p47_2_3 ≠ ∅; If p47_2_1 = 2 then p47_2_2 y p47_2_3 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun24()
    
    
  }
  
} else if (modulo == 'CAP07') {
  for (j in 1:1) {
    
    #######################################################################
    ########################### caracteristicas generales ##############
    #######################################################################
    
    #If p1=6 then p1_ot≠∅ 
    
    fun01 <- function() {
      Mod01_newcod$very <- ifelse(Mod01_newcod$p1 == 6 & Mod01_newcod$p1_ot == "", 1, 0)
      reporte <- Mod01_newcod %>% filter(very == 1)
      if (nrow(reporte) > 0) {
        write.table(reporte, file="Residencia - If p1=6 then p1_ot≠∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p11=2 then p12, p12_1, p12_2, p12_3, p12_4, p12_5=∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p13=0 then p14, p15=∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p13=5 then p13_ot≠∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p16=0 then p16_1a, p16_1_0, p16_1_1, p16_1_2, p16_1_3, p16_1_4, p16_1_ot=∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun05()
    
    
    #If p16=0 then p16_1, p17=∅ 
    
    fun06 <- function() {
      Mod01_newcod$very <- ifelse(Mod01_newcod$p16 == "0" & (!is.na(Mod01_newcod$p16_1) | !is.na(Mod01_newcod$p17)), 1, 0)
      reporte <- Mod01_newcod %>% filter(very == 1)
      if (nrow(reporte) > 0) {
        write.table(reporte, file="Residencia - If p16=0 then p16_1, p17=∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p16_1=1 then p17≠∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p17_1=7 then p17_1_ot≠∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p18=2 then varios  p19_5_12, p19_5_ot = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p20=2 then varios p21_3_11, p21_3_ot = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p22=2 then p23, p23_1, p23_2, p23_3, p23_4, p24, p25, p26, p27, p27_1, p27_2, p28, p29, p30 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p24=2 then p25, p26, p27, p27_1, p27_2 =∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p26=2 then p27, p27_1, p27_2 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p29=2,3 then p30=∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p31=2 then p32, p32_1, p32_2, p32_3, p32_4	, p32_ot, p33, p34, p34_1, p34_2 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p35=2 then p38, p38_1, p38_2, p39, p39_1, p39_2 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p41=1,2 then p41_1, p41_1_1, p41_1_2, p41_1_3, p42 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p48=2 then p48_1, p48_2, p48_3, p48_4_1, p48_4_2, p48_5, p48_5_1, p48_5_2, p48_5_3, p48_5_4, p48_5_5, p48_5_6, p48_5_7, p48_5_8, p48_5_ot = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p49=2 then p49_1, p49_2, p49_3, p49_4_1, p49_4_2, p49_4_3 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p50_1_2=2 then p50_1_3 ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Residencia - If p50_2_2=2 then p50_2_3 ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun21()
    
  }
  
} else if (modulo == 'CAP08') {
  for (j in 1:1) {
    
    #######################################################################
    ########################### caracteristicas generales ##############
    #######################################################################
    
    
    #If p1=6 then p1_ot≠∅ 
    
    fun01 <- function() {
      Mod01_newcod$very <- ifelse(Mod01_newcod$p1 == "6" & Mod01_newcod$p1_ot == "", 1, 0)
      reporte <- Mod01_newcod %>% filter(very == 1)
      if (nrow(reporte) > 0) {
        write.table(reporte, file="Auditorio - If p1=6 then p1_ot≠∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p9=2 then p10_1, p10_2, p10_3, p10_4, p10_5 =∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p11_0=1 then p12, p13 =∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun03()
    
    
    #If p14_0=1 then p14_1a, p15=∅ 
    
    fun04 <- function() {
      Mod01_newcod$very <- ifelse(Mod01_newcod$p14_0 == 1 & (Mod01_newcod$p14_1a != "" | !is.na(Mod01_newcod$p15)), 1, 0)
      reporte <- Mod01_newcod %>% filter(very == 1)
      if (nrow(reporte) > 0) {
        write.table(reporte, file="Auditorio - If p14_0=1 then p14_1a, p15=∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p14_1_4=0 then p14_1_ot≠∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p16=2 then varios  p17_5_12, p17_5_ot = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p18=2 then varios p19_3_11, p19_3_ot = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p20 =2 then p21_1, p21_2, p21_3, p21_4, p22, p23, p24, p25, p26, p27, p28 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p22=2 then p23, p24, p25 =∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p24=2 then p25 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p27=2,3 then p28=∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p29=2 then p32_1, p32_2, p33_1, p33_2 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p35=1,2 then p35_1_1, p35_1_2, p35_1_3, p36 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p41=2 then p41_1, p41_2, p41_3, p41_4_1, p41_4_2, p41_5, p41_5_1, p41_5_2, p41_5_3, p41_5_4, p41_5_5, p41_5_6, p41_5_7, p41_5_8, p41_5_ot = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="Auditorio - If p42=2 then p42_1, p42_2, p42_3, p42_4_1, p42_4_2, p42_4_3=∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun15()
    
    
    #######################################################################
    ################ Tratamiento de residuos ##############################
    #######################################################################
    
    #If p43_1_1=1 then p43_1_2, p43_1_3 ≠ ∅, If p43_1_1=2 then p43_1_2, p43_1_3 = ∅
    
    fun16 <- function() {
      Mod01_newcod$very <- ifelse((Mod01_newcod$p43_1_1 == 1 & (is.na(Mod01_newcod$p43_1_2) | Mod01_newcod$p43_1_3 == "")) 
                                  | (Mod01_newcod$p43_1_1 == 2 & (!is.na(Mod01_newcod$p43_1_2) | Mod01_newcod$p43_1_3 != "")), 1, 0)
      reporte <- Mod01_newcod %>% filter(very == 1)
      if (nrow(reporte) > 0) {
        write.table(reporte, file="Auditorio - If p43_1_1=1 then p43_1_2, p43_1_3 ≠ ∅, If p43_1_1=2 then p43_1_2, p43_1_3 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun16()
    
    
    #If p43_2_1=1, then p43_2_2, p43_2_3 ≠ ∅, If p43_2_1=2 then p43_2_2, p43_2_3 = ∅
    
    fun17 <- function() {
      Mod01_newcod$very <- ifelse((Mod01_newcod$p43_2_1 == 1 & (is.na(Mod01_newcod$p43_2_2) | Mod01_newcod$p43_2_3 == "")) 
                                  | (Mod01_newcod$p43_2_1 == 2 & (!is.na(Mod01_newcod$p43_2_2) | Mod01_newcod$p43_2_3 != "")), 1, 0)
      reporte <- Mod01_newcod %>% filter(very == 1)
      if (nrow(reporte) > 0) {
        write.table(reporte, file="Auditorio - If p43_2_1=1, then p43_2_2, p43_2_3 ≠ ∅, If p43_2_1=2 then p43_2_2, p43_2_3 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun17()
    
    
  }
  
} else if (modulo == 'CAP12') {
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
        write.table(reporte, file="SSHH - If p10_0 = 1 then p11 y p12 = ∅; If p10_1, p10_2, p10_3, p10_4 o p10_5 = 1 then p11 y p12 ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun01()
    
    
    #If p13_0 = 1 then p13_1a y p14 = ∅; If p13_1, p13_2, p13_3, p13_4, p13_5 o p13_6 = 1 then p13_1a ≠ ∅ 
    
    fun02 <- function() {
      Mod01_newcod$very <- ifelse((Mod01_newcod$p13_0 == 1 & (Mod01_newcod$p13_1a != "" | !is.na(Mod01_newcod$p14))) 
                                  | ((Mod01_newcod$p13_1 == 1 | Mod01_newcod$p13_2 == 1 | Mod01_newcod$p13_3 == 1 | Mod01_newcod$p13_4 == 1 | Mod01_newcod$p13_5 == 1 | Mod01_newcod$p13_6 == 1) & is.na(Mod01_newcod$p13_1a)), 1, 0)
      reporte <- Mod01_newcod %>% filter(very == 1)
      if (nrow(reporte) > 0) {
        write.table(reporte, file="SSHH - If p13_0 = 1 then p13_1a y p14 = ∅; If p13_1, p13_2, p13_3, p13_4, p13_5 o p13_6 = 1 then p13_1a ≠ ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="SSHH - If p13_1_1 = 1 then p14 ≠ ∅; If p13_1_1 = 0 then p14 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="SSHH - If p15 = 1 then p16_1, p16_2, p16_3, p16_4 y p16_5 ≠ ∅; If p15 = 2 then p16_1, p16_2, p16_3, p16_4 y p16_5 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="SSHH - If p17 = 1 then p18_1 y 18_2 ≠ ∅; If p17 = 2 then p18_1 y 18_2 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="SSHH - If p19 =1 then p20 y p21 ≠ ∅; If p19 = 2 then p20 y p21 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
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
        write.table(reporte, file="SSHH - If p22 = 1 then p25, p26 y p27 ≠ ∅; If p22 = 2 then p25, p26 y p27 = ∅ - 15 marzo.csv", sep = ";", row.names = FALSE)
      } else {
        print(nrow(reporte))
      }
      
    }
    
    fun07()
    
  }
  
}

rm(list = ls())

}
