library(tidyverse) 
library(readxl)


setwd("C:/Users/ifmor/OneDrive - Universidad de los Andes/trabajo/DNP/Asociatividad/Kit/terridata/")


datos <- read.table("TerriData_Dim12.txt", sep = "|",header = T)
eat <- read_excel("EAT.xlsx", sheet = "consolidada",
                  range = "a1:j874")

dejar <- c("Miles de millones de pesos corrientes",
            "Pesos corrientes")

eat_trabajo <- eat %>% select(1,3,6,7)
trabajo <- datos %>% 
  filter(Año > 2017,
         Unidad.de.Medida %in% dejar)

variables <- c("Dimensión","Subcategoría","Indicador",
               "Año","Fuente")

para_calculo <- trabajo %>% 
  right_join(eat_trabajo, 
             by = c("Código.Entidad"="Código DANE de la ET"))

# tipos de EAT
area_metropolitana <- eat_trabajo %>% filter(`Tipo EAT`=="Área Metropolitana" ) #municipal
aso_mun <- eat_trabajo %>% filter(`Tipo EAT`=="Asociación de Municipios") #municipal
provincia_admin_plan <- eat_trabajo %>% filter(`Tipo EAT`=="Provincias Administrativas y de Planificación") #municipal
reg_admin_plan<- eat_trabajo %>% filter(`Tipo EAT`=="Regiones Administrativas y de Planificación") #departamental
reg_admin_plan_esp<- eat_trabajo %>% filter(`Tipo EAT`=="Regiones Administrativas y de Planificación Especial")
reg_plan_gest<- eat_trabajo %>% filter(`Tipo EAT`=="Regiones de Planeación y Gestión") #municipal

trabajo$es_depto <- ifelse(trabajo$Departamento==trabajo$Entidad,1,0)
trabajo$codigo_dane <- as.double(ifelse(trabajo$es_depto==1,trabajo$Código.Entidad/1000,trabajo$Código.Entidad))

trabajo$area_metropolitana <- trabajo$codigo_dane %in% area_metropolitana$`Código DANE de la ET`
trabajo$aso_mun <- trabajo$codigo_dane %in% aso_mun$`Código DANE de la ET`
trabajo$provincia_admin_plan <- trabajo$codigo_dane %in% provincia_admin_plan$`Código DANE de la ET`
trabajo$reg_admin_plan <- trabajo$codigo_dane %in% reg_admin_plan$`Código DANE de la ET`
trabajo$reg_admin_plan_esp <- trabajo$codigo_dane %in% reg_admin_plan_esp$`Código DANE de la ET`
trabajo$reg_plan_gest <- trabajo$codigo_dane %in% reg_plan_gest$`Código DANE de la ET`

#transformar dato numérico 
trabajo$Dato.Numérico <- as.numeric(gsub(",", ".", gsub("\\.", "", trabajo$Dato.Numérico)))
# se debe agregar Bogotá como depto y municipio o las cuentas no cuadran
bogota <- trabajo[trabajo$Departamento=="Bogotá",] %>% 
  mutate(Departamento= "Especial", es_depto=0)
trabajo <- rbind(trabajo, bogota)

### aqui puede estar contando cosas q no son municipios , como todo colombia
nacional_municipios <- trabajo %>% filter(es_depto==0) %>%  
  group_by(Indicador, Unidad.de.Medida) %>% 
  summarise(Nacional=sum(Dato.Numérico))  

nacional_deptos <-  trabajo %>% filter(es_depto==1, Departamento != "Colombia") %>%  
  group_by(Indicador, Unidad.de.Medida) %>% 
  summarise(Nacional=sum(Dato.Numérico))  

df <- trabajo %>% filter(es_depto==1, Departamento != "Colombia")

write.table(df, file="clipboard-16384", 
            sep="\t", col.names=NA)

##

for (i in 1:6) {
   df <-  trabajo %>% filter(trabajo[,15+i]==TRUE) %>% 
    group_by(Indicador, Unidad.de.Medida) %>% 
    summarise(Suma=sum(Dato.Numérico))  
   assign(paste0('calculo_',names(trabajo)[15+i], sep=""),df)
}

i=1
write.table(df, file="clipboard-16384",
            sep="\t", col.names=NA)



library(xlsx)
write.xlsx(calculo_area_metropolitana, file="datos.xlsx", sheetName="area_metro", row.names=FALSE)
write.xlsx(calculo_aso_mun, file="datos.xlsx", sheetName="aso_mun", append=TRUE, row.names=FALSE)
write.xlsx(calculo_provincia_admin_plan, file="datos.xlsx", sheetName="prov_admin", append=TRUE, row.names=FALSE)
write.xlsx(calculo_reg_admin_plan, file="datos.xlsx", sheetName="reg_admin_plan", append=TRUE, row.names=FALSE)
write.xlsx(calculo_reg_admin_plan_esp, file="datos.xlsx", sheetName="reg_admin_planEsp", append=TRUE, row.names=FALSE)
write.xlsx(calculo_reg_plan_gest, file="datos.xlsx", sheetName="reg_plan_gest", append=TRUE, row.names=FALSE)


# revision 
df <-  trabajo %>% filter(es_depto==1) %>%  
  group_by(Indicador, Unidad.de.Medida) %>% 
  summarise(Nacional=sum(Dato.Numérico))  

write.table(df, file="clipboard-16384", 
            sep="\t", col.names=NA)

writexl::write_xlsx()

# Población ----

poblacion <- read.table("poblaci.txt", sep = "|",header = T)

filtro <- c("Población total de hombres",                                                                                   
            "Población total de mujeres" )

df <- poblacion %>% filter( Año == 2020, Indicador %in% filtro) 

df$es_depto <- ifelse(df$Código.Entidad %% 1000==0,1,0)
df$codigo_dane <- as.double(ifelse(df$es_depto==1,df$Código.Entidad/1000,df$Código.Entidad))
df$Dato.Numérico <- as.numeric(gsub(",", ".", gsub("\\.", "", df$Dato.Numérico)))
bogota <- df[df$Departamento=="Bogotá",] %>% 
  mutate(Departamento= "Especial", es_depto=1)
df <- rbind(df, bogota)

colombia <- df[df$Departamento=="Colombia",] 


nacional_depto_p <- df %>% filter(es_depto==1, Departamento != "Colombia") %>%  
  group_by(Unidad.de.Medida) %>% 
  summarise(Nacional=sum(Dato.Numérico))

#

# agregar EAT
df$area_metropolitana <- df$codigo_dane %in% area_metropolitana$`Código DANE de la ET`
df$aso_mun <- df$codigo_dane %in% aso_mun$`Código DANE de la ET`
df$provincia_admin_plan <- df$codigo_dane %in% provincia_admin_plan$`Código DANE de la ET`
df$reg_admin_plan <- df$codigo_dane %in% reg_admin_plan$`Código DANE de la ET`
df$reg_admin_plan_esp <- df$codigo_dane %in% reg_admin_plan_esp$`Código DANE de la ET`
df$reg_plan_gest <- df$codigo_dane %in% reg_plan_gest$`Código DANE de la ET`

# crear índice de datos ----

for (i in 1:6) {
  temp <-  df %>% filter(df[,15+i]==TRUE) %>% 
  group_by(Indicador, Unidad.de.Medida) %>%
  summarise(Suma=sum(Dato.Numérico))
assign(paste0('pob_',names(df)[15+i], sep=""),temp)
}
pob_aso_mun
write.table(nacional_depto_p, file="clipboard-16384", 
            sep="\t", col.names=NA)



general <- read.table("general.txt", sep = "|",header = T)

A <- matrix(ncol=5)
catalogo <- as.data.frame(A)
colnames(catalogo) <- c("base","subcategoria","indicador","año","fuente")


econo <- read.table("datos/TerriData_Dim12.txt", sep = "|",header = T) %>% 
  mutate(municipio = case_when(Departamento==Entidad ~0,
                   Departamento != Entidad~1)) %>% 
  filter(municipio==1) %>% 
  select(Dimensión,Subcategoría,Indicador,
               Año, Fuente) %>% 
  unique() %>% 
  setNames(colnames(catalogo))
catalogo <- rbind(catalogo, econo)

pob <- read.table("poblaci.txt", sep = "|",header = T) %>% 
  mutate(municipio = case_when(Departamento==Entidad ~0,
                               Departamento != Entidad~1)) %>% 
  filter(municipio==1) %>% 
  select(Dimensión,Subcategoría,Indicador,
         Año, Fuente) %>% 
  unique() %>% 
  setNames(colnames(catalogo))
catalogo <- rbind(catalogo, pob)

gen <- read.table("general.txt", sep = "|",header = T) %>% 
  mutate(municipio = case_when(Departamento==Entidad ~0,
                               Departamento != Entidad~1)) %>% 
  filter(municipio==1) %>% 
  select(Dimensión,Subcategoría,Indicador,
         Año, Fuente) %>% 
  unique() %>% 
  setNames(colnames(catalogo))


catalogo <- rbind(catalogo, gen)

list.files()

desem_mun <- read.table("desem_mun.txt", sep = "|",header = T) %>% 
  mutate(municipio = case_when(Departamento==Entidad ~0,
                               Departamento != Entidad~1)) %>% 
  filter(municipio==1) %>% 
  select(Dimensión,Subcategoría,Indicador,
         Año, Fuente) %>% 
  unique() %>% 
  setNames(colnames(catalogo))
catalogo <- rbind(catalogo, desem_mun)

fin_pub <- read.table("fin_pub.txt", sep = "|",header = T) %>% 
  mutate(municipio = case_when(Departamento==Entidad ~0,
                               Departamento != Entidad~1)) %>% 
  filter(municipio==1) %>% 
  select(Dimensión,Subcategoría,Indicador,
         Año, Fuente) %>% unique() %>% 
  setNames(colnames(catalogo))
catalogo <- rbind(catalogo, fin_pub)

ordenam_terri <- read.table("ordenam_terri.txt", sep = "|",header = T) %>% 
  mutate(municipio = case_when(Departamento==Entidad ~0,
                               Departamento != Entidad~1)) %>% 
  filter(municipio==1) %>% 
  select(Dimensión,Subcategoría,Indicador,
         Año, Fuente) %>% unique() %>% 
  setNames(colnames(catalogo)) 
catalogo <- rbind(catalogo, ordenam_terri)

per_ciudadana <- read.table("per_ciudadana.txt", sep = "|",header = T) %>% 
  mutate(municipio = case_when(Departamento==Entidad ~0,
                               Departamento != Entidad~1)) %>% 
  filter(municipio==1) %>% 
  select(Dimensión,Subcategoría,Indicador,
         Año, Fuente) %>% unique() %>% 
  setNames(colnames(catalogo)) 
catalogo <- rbind(catalogo, per_ciudadana)

catalogo_viejo <- read_xlsx("catalogo datos.xlsx")

writexl::write_xlsx(catalogo, "catalogo_final.xlsx",col_names = T)

getwd()
# Pobreza ----

pobreza <- read.table("pobreza.txt", sep = "|",header = T)

filtro <- c("Población en condición de miseria")
view(unique(pobreza$Unidad.de.Medida))

df <- pobreza %>% filter( Subcategoría=="Indicadores de pobreza") 

df$es_depto <- ifelse(df$Código.Entidad %% 1000==0,1,0)
df$codigo_dane <- as.double(ifelse(df$es_depto==1,df$Código.Entidad/1000,df$Código.Entidad))
df$Dato.Numérico <- as.numeric(gsub(",", ".", gsub("\\.", "", df$Dato.Numérico)))
bogota <- df[df$Departamento=="Bogotá",] %>% 
  mutate(Departamento= "Especial", es_depto=1)
df <- rbind(df, bogota)

colombia <- df[df$Departamento=="Colombia",] 


nacional_depto_p <- df %>% filter(es_depto==1, Departamento != "Colombia") %>%  
  group_by(Unidad.de.Medida) %>% 
  summarise(Nacional=sum(Dato.Numérico))




# revisar diccionarios ----
library(readstata13)
setwd("C:/Users/ifmor/OneDrive - Universidad de los Andes/trabajo/DNP/Asociatividad/Kit/visor/pagina")

departamento <- read.table(list.files()[2], header = T)
municipio <- read.table(list.files()[3], header = T)

view(municipio %>% filter(MunicipioAId==5001) )

View(datos)







