# Calculo de índices de similaridad 
library(tidyverse)
library(readstata13)

# Convertir datos de stata a formato r 
datos_stata <- readstata13::read.dta13(file = "C:/Users/ifmor/OneDrive - Universidad de los Andes/trabajo/DNP/Asociatividad/Kit/visor/bases de datos/Dij_y_Cij_definitiva.dta")
datos_stata <- datos_stata %>% 
  filter(codigo_mun_i!=codigo_mun_j)
saveRDS(datos_stata, file = "Dij_y_Cij_definitiva.rds")

# filtrar con códigos de eat
datos <- readRDS(file = "Dij_y_Cij_definitiva.rds")

# cambiar número 

lista_municipios <- c(76109,52835,52390,19318,52473,52490,52520,52696,52250,27495,27372,27077,19418,27075,19809,27250)

# Promedios nacionales

est_dij_1_r <- 0.0728613
est_dij_2_r <- 0.0105399
est_dij_3_r <- 0.0333425
est_dij_4_r <- 0.0075092
est_dij_6_r <- 0.0260567

# Construcción de índices
datos_p <- datos %>% 
  filter(datos$codigo_mun_i %in% lista_municipios,
         datos$codigo_mun_j %in% lista_municipios) %>% 
  select(1,2,
         dij_1_, dij_2_,dij_3_, dij_4_,dij_6_) %>% 
  # convertir disimilaridad en similaridad
  mutate(dij_1_=1-dij_1_,
         dij_2_=1-dij_2_, 
         dij_3_=1-dij_3_, 
         dij_4_=1-dij_4_, 
         dij_6_=1-dij_6_) %>% 
  #promedio de cada municipio
  group_by(codigo_mun_i) %>% 
  mutate(m_dij_1_=mean(dij_1_),
         m_dij_2_=mean(dij_2_),
         m_dij_3_=mean(dij_3_),
         m_dij_4_=mean(dij_4_),
         m_dij_6_=mean(dij_6_)) %>% 
  ungroup() %>%  select(1,2, m_dij_1_,
                        m_dij_2_, m_dij_3_,
                        m_dij_4_, m_dij_6_) %>%
  distinct(codigo_mun_i, .keep_all = T) %>% 
  # Intensidad: definida como el promedio sobre el dato nacional
  mutate(i_dij_1_=m_dij_1_/est_dij_1_r,
         i_dij_2_=m_dij_2_/est_dij_2_r,
         i_dij_3_=m_dij_3_/est_dij_3_r,
         i_dij_4_=m_dij_4_/est_dij_4_r,
         i_dij_6_=m_dij_6_/est_dij_6_r) %>% 
  select(1,8:12) %>% 
  mutate(func_ambiental = case_when(i_dij_1_ <0.5 ~ "Baja",
                                    i_dij_1_ >=0.5 & i_dij_1_ < 1 ~ "Medio-baja",
                                    i_dij_1_ >=1 & i_dij_1_ < 1.5 ~"Medio-alta" ,
                                    i_dij_1_ >=1.5 ~"Alta" ),
         func_poblacional =case_when(i_dij_2_ <0.1 ~ "Baja",
                                     i_dij_2_ >=0.1 & i_dij_2_ < 0.5 ~"Medio-baja" ,
                                     i_dij_2_ >=0.5 & i_dij_2_ < 1.5 ~"Medio-alta" ,
                                     i_dij_2_ >=1.5 ~"Alta" ),
         func_economica =case_when(i_dij_3_ <0.5 ~ "Baja",
                                   i_dij_3_ >=0.5 & i_dij_3_ < 1 ~"Medio-baja" ,
                                   i_dij_3_ >=1 & i_dij_3_ < 1.5 ~"Medio-alta" ,
                                   i_dij_3_ >=1.5 ~ "Alta"),
         func_laboral = case_when(i_dij_4_ <0.5 ~ "Baja",
                                  i_dij_4_ >=0.5 & i_dij_4_ < 1 ~"Medio-baja" ,
                                  i_dij_4_ >=1 & i_dij_4_ < 1.5 ~"Medio-alta" ,
                                  i_dij_4_ >=1.5 ~"Alta"),
         func_social =case_when(i_dij_6_ <0.5 ~ "Baja",
                                i_dij_6_ >=0.5 & i_dij_6_ < 1 ~"Medio-baja" ,
                                i_dij_6_ >=1 & i_dij_6_ < 1.5 ~"Medio-alta" ,
                                i_dij_6_ >=1.5 ~"Alta" ))
  

names(datos)

