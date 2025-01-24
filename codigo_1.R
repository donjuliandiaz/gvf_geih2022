rm(list = ls())
library(readr)
library(dplyr)

# Lista de valores para 'mes_ev'
meses <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# Bucle para iterar por cada valor de 'mes_ev'
for (mes_ev in meses) {
  # Directorio correspondiente al mes actual
  directory <- paste0("C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/", mes_ev)
  setwd(directory)
  
  # Determinar delimitador
  delimitador <- ifelse(mes_ev == "01", ",", ";")
  
  #[1. Bases]-------------------------------------------------------------
  #[1.1. Caracteristicas generales personas]-------------------------------
  crts_gen_per <- read_delim("Características generales, seguridad social en salud y educación.CSV", 
                             delim = delimitador, escape_double = FALSE, trim_ws = TRUE)%>%
    dplyr::select(-c("REGIS"))
  #[1.2. Fuerza de trabajo]----------------------------------------------
  f_trab <- read_delim("Fuerza de trabajo.CSV", 
                       delim = delimitador, escape_double = FALSE, trim_ws = TRUE)%>%
    mutate(f_trab="si") %>% dplyr::select(-c("REGIS"))
  #[1.3. Otras actividades y ayudas]----------------------------------------
  otr_act <- read_delim("Otras formas de trabajo.CSV", 
                        delim = delimitador, escape_double = FALSE, trim_ws = TRUE)%>%
    mutate(otr_act="si") %>% dplyr::select(-c("REGIS"))%>% mutate(FEX_C18_3=FEX_C18)%>%
    dplyr::select(c("PERIODO","MES","PER","DIRECTORIO","SECUENCIA_P","ORDEN","HOGAR",
             "AREA","CLASE","FEX_C18","FEX_C18_3","DPTO","otr_act"))
  #[1.4. Ocupados]----------------------------------------------------------
  ocup <- read_delim("Ocupados.CSV", 
                     delim = delimitador, escape_double = FALSE, trim_ws = TRUE)%>%
    mutate(ocup="si") %>% dplyr::select(-c("REGIS"))
  #[1.5. No ocupados]---------------------------------------------------------
  no_ocup <- read_delim("No ocupados.CSV", 
                        delim = delimitador, escape_double = FALSE, trim_ws = TRUE)%>%
    mutate(no_ocup="si") %>% dplyr::select(-c("REGIS"))
  
  
  #[2. Cruces]-------------------------------------------------------------
  var_1=c("DIRECTORIO","SECUENCIA_P","ORDEN","HOGAR","MES","AREA","DPTO",
          "FEX_C18","PERIODO","PER","CLASE")
  var_1_3=c("DIRECTORIO","SECUENCIA_P","ORDEN","HOGAR","MES","AREA","DPTO",
            "FEX_C18","PERIODO","PER","CLASE","FFT")
  var_3_2=c("DIRECTORIO","SECUENCIA_P","ORDEN","HOGAR","MES","AREA","DPTO","FEX_C18",
            "PERIODO","PER","CLASE","FT")
  #[2.1. Fuerza de trabajo]----------------------------------------------
  base1=left_join(x=crts_gen_per,y=f_trab,by=var_1)
  #[2.2. Otras actividades y ayudas]----------------------------------------
  base2=left_join(x=base1,y=otr_act,by=var_1)
  #[2.3. Ocupados]----------------------------------------------------------
  base3=left_join(x=base2,y=ocup,by=var_3_2)
  #[2.4. No ocupados]---------------------------------------------------------
  base_final=left_join(x=base3,y=no_ocup,by=var_1_3)
  #Se crea ID
  base_final=base_final%>%mutate(id=paste0(DIRECTORIO,"-",ORDEN))
  #Nombre del documento
  
  base_final2 = base_final %>% #dplyr::select(variables)%>%
    dplyr::select(c(id,PERIODO,MES,DIRECTORIO,SECUENCIA_P,ORDEN,HOGAR,
             CLASE,#rural-urbano
             FEX_C18, #Factor de expansión
             SEXO=P3271,#Poner hombre mujer
             EDAD=P6040, #¿Cuántos años cumplidos tiene … ?
             ROL=P6050,#Rol en el hogar
             RAZA=P6080, #De acuerdo con su cultura, pueblo o rasgos físicos,  es o se reconoce como: 
             #SEXO_REC=P3039, #¿Usted se reconoce como? #sexo
             OCUP=P6450,#Estuvo ocupado(con trabajo) #Poner significado
             SUELDO=P6500, #Antes de descuentos, ¿cuánto ganó … el mes pasado en este empleo?
             TEMPTRAB=P6790,#¿cuántos meses trabajó en los últimos 12 meses?
             TRABAJO_2=P7050, #En ese segundo trabajo … es: 
             OCI,#POBLACIÓN OCUPADA
             INGRESOS=INGLABO, #Ingresos laborales
             PERIOD_TRAB=P6780, #Este trabajo es: OCASIONAL, ESTACIONAL, PERMANENTE
             PUESTO=P6430, #En este trabajo … es:
             ACTIVIDAD=P6240,#¿En que actividad ocupó … la mayor parte del tiempo la semana pasada?
             RNOTRAB=P6260S1,#¿Por qué razón no trabajó la semana pasada?
             MUNICIPIO=AREA,
             DEPARTAMENTO=DPTO,
             BUSC_TRAB=P7250,#¿Durante cuántas semanas ha estado o estuvo … buscando trabajo?
             DESEMPCOY=DSCY, #Desempleo coyuntural
             PET, #Personas en edad de trabajar
             PER, #Población económicamente activa
             OCUP_EST=ocup,#Esta ocupado
             POB_OCUP=OCI, #Población ocupada
             DESOCUP=DSI, #Desocupados
             NOOCUP_EST=no_ocup, #No esta ocupado
             FRZTRA=f_trab#Fuerza de trabajo
    ))
  
  dic_clase=data.frame(rbind(c(2,"Rural"),c(1,"Urbano")))
  colnames(dic_clase)=unclass(c("CLASE","CLASE_DIC"))
  dic_actividad=data.frame(rbind(c(6,"Otra actividad"),
                                 c(5,	"Incapacitado permanente para trabajar"),
                                 c(4,	"Oficios del hogar"),
                                 c(3,	"Estudiando"),
                                 c(2,	"Buscando trabajo"),
                                 c(1,	"Trabajando")))
  colnames(dic_actividad)=unclass(c("ACTIVIDAD","ACTIVIDAD_DIC"))
  
  dic_rol=data.frame(rbind(c(9,	"Otro no pariente"),
                           c(8,	"Trabajador"),
                           c(7,	"Pensionista"),
                           c(6,	"Empleado del servicio doméstico y sus parientes"),
                           c(5,	"Otro pariente"),
                           c(4,	"Nieto"),
                           c(3,	"Hijo y hijastro"),
                           c(2,	"Pareja, esposo, cónyuge y compañero"),
                           c(1,	"Jefe del hogar")))
  colnames(dic_rol)=unclass(c("ROL","ROL_DIC"))
  dic_rol$ROL = as.double(dic_rol$ROL)
  
  dic_raza=data.frame(rbind(
    c(1,	"Indígena"),
    c(2,	"Gitano"),
    c(3,	"Raizal"),
    c(4,	"Palenquero"),
    c(5,	"Negro, mulato, afrocolombiano"),
    c(6,	"Ning")
  ))
  colnames(dic_raza)=unclass(c("RAZA","RAZA_DIC"))
  dic_raza$RAZA = as.double(dic_raza$RAZA)
  
  
  dic_raza=data.frame(rbind(
    c(1,	"Indígena"),
    c(2,	"Gitano"),
    c(3,	"Raizal"),
    c(4,	"Palenquero"),
    c(5,	"Negro, mulato, afrocolombiano"),
    c(6,	"Ning")
  ))
  colnames(dic_raza)=unclass(c("RAZA","RAZA_DIC"))
  dic_raza$RAZA = as.double(dic_raza$RAZA)
  
  dic_puesto=data.frame(rbind(
    c(9,	"Otro"),
    c(8,	"Jornalero"),
    c(7,	"Trab sin remuneración"),
    c(6,	"Trab familiar sin remuneración"),
    c(5,	"Patrón"),
    c(4,	"Trab cuenta propia"),
    c(3,	"Emp doméstico"),
    c(2,	"Emp gobierno"),
    c(1,	"Emp empresa particular")
  ))
  colnames(dic_puesto)=unclass(c("PUESTO","PUESTO_DIC"))
  dic_puesto$PUESTO = as.double(dic_puesto$PUESTO)
  
  
  base_final3=merge(base_final2,dic_clase,by="CLASE")%>%
    mutate(CLASE=CLASE_DIC,ACTIVIDAD=as.character(ACTIVIDAD))
  base_final4=left_join(base_final3,dic_actividad,by="ACTIVIDAD")%>%
    mutate(ACTIVIDAD=ACTIVIDAD_DIC)%>%mutate(SEXO=ifelse(SEXO==1,"Hombre","Mujer"))
  
  base_final4_1=left_join(base_final4,dic_rol,by="ROL")%>%
    mutate(ROL=ROL_DIC)
  
  base_final4_2=left_join(base_final4_1,dic_raza,by="RAZA")%>%
    mutate(RAZA=RAZA_DIC)
  
  base_final4_3=left_join(base_final4_2,dic_puesto,by="PUESTO")%>%
    mutate(PUESTO=PUESTO_DIC)
  
  departamentos_codigo <- read_csv("C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/departamentos_codigo.csv")
  departamentos_codigo$DPTO_I=substr(departamentos_codigo$DPNOM , 1, 4)
  base_final4_3$DEPARTAMENTO=as.double(base_final4_3$DEPARTAMENTO)
  base_final5=left_join(base_final4_3,departamentos_codigo,by="DEPARTAMENTO")%>%dplyr::select(-c("...1" ))
  
  data = base_final5 %>% dplyr::select(-c("CLASE_DIC","ACTIVIDAD_DIC","ROL_DIC","RAZA_DIC"))
 
  # Guardar el archivo procesado
  nombre <- paste0("C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/union/", mes_ev, "_geih.csv")
  write_delim(data, file = nombre, delim = "\t")
}
