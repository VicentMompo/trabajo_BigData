library(tidyverse)
library(klippy)   #remotes::install_github("rlesur/klippy")
library(knitr)
library(rio)
library(readxl)
library(countrycode)
library(ggflags)
library(rnaturalearth)
library(rnaturalearthdata)
library(hrbrthemes)
library(kableExtra)
options(knitr.table.format = "html")
library(streamgraph)
library(viridis)
library(DT)
library(plotly)
library(gganimate)
library(treemap)
library(tidyverse)
library(DT)
library(gt) #-remotes::install_github("rstudio/gt")
library(ggflags)
library(countrycode)

#ideas de gráficos

#Gráfico Líneal de la deuda pública ESP

deuda <- rio::import("./datos/Deuda.xlsx")

aa <- ggplot() +
  geom_col(data = deuda, aes(x = Date, y = Deuda_porcentaje_PIB, fill = Deuda_porcentaje_PIB)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_continuous(low="#F2D7D5", high="#641E16") + 
  theme(panel.background = element_rect(fill = "transparent")) +
  labs(title = "Evolución de la Deuda Española (%PIB)",
       caption = "Fuente: Elaboración propia con datos de INEbase") +
  labs (y = NULL) +
  labs(x = NULL) +
  guides (fill = FALSE) 
  
aa


#Piramide de Población

Pob_21 <- rio::import("./datos/Piramide.xlsx")

library(readxl)
Piramide <- read_excel("./datos/Piramide.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "numeric", "numeric"))
Pob_21 <- Pob_21 %>% 
  select(Pob,Sexo,Grupo_quinquenal) %>%
  mutate(Pob_Porcentaje = Pob/42018307)


  
aa <- ggplot(Pob_21, aes(x = Grupo_quinquenal,
                        y = Pob_Porcentaje,
                        fill = Sexo)) +
      geom_col(data = subset(Pob_21, Sexo == "Hombres") %>% 
             mutate(Pob_Porcentaje = -Pob_Porcentaje),
           width = 0.5, fill = "#0B615E") +
      geom_col(data = subset(Pob_21, Sexo == "Mujeres"),
           width = 0.5, fill = "#04B4AE") + 
      coord_flip() +
  theme(panel.background = element_rect(fill = "transparent")) +
  scale_y_continuous(
    breaks = c(seq(-0.06, -0.02, by = 0.02), 
               seq(0, 0.06, by = 0.02)),
    labels = scales::percent (c(seq(-0.06, -0.02, by = 0.02) * -1, 
               seq(0, 0.06, by = 0.02)))) +
  labs(y = "Hombre/Mujer población en %") +
  labs(x= NULL) +
  labs(title = "Pirámide de la población empadronada en España",
       caption = "Fuente: Elaboración propia con datos de INEbase") +
  annotate(geom = "text",
           x= "85 a 89",
           y = 0.035,
           label = "Mujeres",
           size = 4) +
  annotate(geom = "text",
           x= "85 a 89",
           y = - 0.035,
           label = "Hombres",
           size = 4) +
  geom_vline(xintercept = "35 a 39",
             size = 0.5,
             colour = "red",
             linetype = "dashed") +
  geom_vline(xintercept = "60 a 64",
             size = 0.5,
             colour = "red",
             linetype = "dashed")

aa

  
#Gráfico Principales Componentes de Gasto

Partidas_g_pub <- rio::import("./datos/Partidas_Gasto.xlsx")

library(readxl)
Partidas_g_pub <- read_excel("./datos/Partidas_Gasto.xlsx", 
                             col_types = c("text", "numeric", "numeric"))
library(treemap)

Partidas_g_pub_2021 <- Partidas_g_pub %>%
      filter(Time == "2021") %>%
      select("Partidas_Gasto", "Gasto_millones") %>%
      slice_max (Gasto_millones , n = 16)


  
componentes_europa <- ggplot() + 
  geom_treemap(Partidas_g_pub_2021,
                              index="Partidas_Gasto",
                              vSize="Gasto_millones",
                              type="index",
                              palette="Spectral",
                              border.col=c("white"), 
                              border.lwds=2, 
                              fontface.labels=1,
                              bg.labels=c("transparent"),              
                              align.labels=c("center", "center"),                                  
                              overlap.labels=0.75) 

Partidas_g_pub_2016 <- Partidas_g_pub %>%
  filter(Time == "2016") %>%
  select("Partidas_Gasto", "Gasto_millones") %>%
  slice_max (Gasto_millones , n = 16)


componentes_europa <- treemap(Partidas_g_pub_2016,
                              index="Partidas_Gasto",
                              vSize="Gasto_millones",
                              type="index",
                              title="Principales partidas del Presupuestos Generales del Estado 2016",
                              palette="Spectral",
                              border.col=c("white"), 
                              border.lwds=2, 
                              fontface.labels=1,
                              bg.labels=c("transparent"),              
                              align.labels=c("center", "center"),                                  
                              overlap.labels=0.75) 
  
#Ingresos/Gastos/Déficit

cuentas_pub <- rio::import("./datos/cuentas_pub.xlsx")

Cuentas_pub <- read_excel("./datos/Cuentas_pub.xlsx", 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric"))


cuentas_pub_long <- cuentas_pub %>% 
  tidyr::pivot_longer(cols = 2:27, names_to = "año")

cuentas_pub_long_gasto <- cuentas_pub_long %>%
  filter(Tipo == "Gastos") 

cuentas_pub_long_ingresos <- cuentas_pub_long %>%
  filter(Tipo == "Ingresos") 

cuentas_pub_long_df <- cuentas_pub_long %>%
  filter(Tipo == "Deficit/Superavit") 


Deficit_PIB <- rio::import("./datos/Deficit_PIB.xlsx")

Deficit_PIB <- read_excel("./datos/Deficit_PIB.xlsx", 
                          col_types = c("text", "numeric", "numeric"))



bb <- ggplot() +
  geom_point(data = cuentas_pub_long_gasto, aes(x = año, y = value, color = Tipo,shape = Tipo), size = 3, color= "#A90729") +
  geom_line(data = cuentas_pub_long_gasto, aes(x = año, y = value, color = Tipo), size = 1.5, group = "value", color= "#A90729") +
  geom_point(data = cuentas_pub_long_ingresos, aes(x = año, y = value, shape = Tipo, color = Tipo),size = 4, color= "#E48297") +
  geom_line(data = cuentas_pub_long_ingresos, aes(x = año, y = value, color = Tipo), size = 1.5, group = "value", color= "#E48297") +
  scale_shape_manual(values = c(15,18)) +
  geom_col(dat = cuentas_pub_long_df, aes(x = año, y = value, fill = value)) +
  scale_fill_continuous(low="#86E1E7", high="#0597A1") +
  theme(panel.background = element_rect(fill = "transparent")) +
  labs(y = NULL) +
  labs(x= NULL) +
  guides (color = FALSE) +
  guides (shape = FALSE) +
  guides (fill = FALSE) +
  geom_text(data = Deficit_PIB, aes(Time, Value, label = Value), y= 50000, size = 3) +
  geom_text(data = Deficit_PIB, aes(Time, Value, label = "%"), y= 50000, hjust = -1.75, size = 2.5) +
  annotate(geom = "text",
           x= 1,
           y = 80000,
           label = "Déficit/PIB",
           size = 3.5) +
  scale_y_continuous(breaks=c(200000, 400000, 600000), 
                     labels=c("200.000 €", "400.000 €", "600.000 €")) +
  theme(axis.text.y = element_text(size=rel(0.6))) +
  labs(title = "Evolución Ingresos/Gastos totales en millones de € (1995-2020)",
       caption = "Fuente: Elaboración propia con datos de Eurostat DataBase") 


bb

#Mapas Presión

#aa <- rio::import("./datos/PF.xlsx")

#rio::export(aa, "./datos/PF.csv")

#PF <- read_csv("./datos/PF.csv", 
#                            col_types = cols(date = col_date(format = "%Y/%m")))

#PF_long <- PF %>% 
# tidyr::pivot_longer(cols = 2:13, names_to = "time")

#rio::export(PF_long, "./datos/PF_long.csv")

PF_long <- rio::import("./datos/PF_long.csv")

library(readr)
PF_long <- read_csv("./datos/PF_long.csv", 
                    col_types = cols(time = col_date(format = "%Y/%m"), 
                                     value = col_number()))

library(tidyverse)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- world %>% filter(subregion != "Antarctica") %>% filter(admin != "Greenland")
world <- world %>% dplyr::select(name, iso_a3, geometry)

PF_geom_europ <- left_join(PF_long, world, by = c("country" = "name")) 


mapa_2 <- ggplot() +
  geom_sf(data = PF_geom_europ, aes(geometry = geometry, fill = value )) +
  theme_void()  +
  theme(plot.subtitle = element_text(family = "serif", face = "italic"),
        plot.title = element_text(family = "serif", face =  "italic")) +
  labs(title = "Mapa: Presión Fiscal", fill = "Ingresos/PIB")  +
  scale_fill_continuous(low="#FEE5FB",high="#63015A") +
  lims(y =c(35,70)) +
  lims(x = c(-20, 40)) + transition_manual(time) +
  labs(title = "Año: {current_frame}",
       caption = "Datos de Eurostat")

gganimate::animate(mapa_2, nframes = 200, fps = 25, end_pause = 6)



#dependencia
library(ggflags)
library(countrycode)
#aa <- rio::import("./datos/tasa_dep.csv")

#tasa_dep <- aa %>%
#  select(geo,TIME_PERIOD, OBS_VALUE) 

#rio::export(tasa_dep, "./datos/tasa_dep.xlsx")

#aa <- rio::import("./datos/tasa_dep.csv")

aa <- rio::import("./datos/tasa_dep.xlsx")

library(readxl)
tasa_dep <- read_excel("./datos/tasa_dep.xlsx", 
                       col_types = c("text", "numeric", "numeric", 
                                     "text"))
tasa_dep <- tasa_dep %>%
  select(TIME_PERIOD, geo_1, OBS_VALUE)

tasa_dep$code<-tolower(countrycode(tasa_dep$geo_1, origin = 'iso2c', destination = 'iso2c'))


aa <- ggplot(tasa_dep, aes(x = geo_1, OBS_VALUE, country = code, size = 10)) +
  scale_fill_brewer(palette = "Spectral") +
  geom_flag(aes(geo_1, OBS_VALUE, country = code), size = 10) +
  transition_manual(TIME_PERIOD) +
  coord_flip() +
  theme(panel.background = element_rect(fill = "#303637")) + 
  labs (x = NULL) +
  labs (y = "Tasa de dependencia vejez en %") +
  theme(panel.grid.major = element_line(colour = NA)) +
  labs(title = "Estimación para el año: {current_frame}",
       caption = "Datos de Eurostat")  + theme(axis.line = element_line(colour = "white"),
    axis.ticks = element_line(colour = "white"),
    panel.grid.major = element_line(colour = "azure3"),
    panel.grid.minor = element_line(colour = "azure3"),
    axis.title = element_text(colour = "white"),
    axis.text = element_text(colour = "azure3"),
    axis.text.y = element_text(colour = "azure3"),
    axis.text.x = element_text(colour = "white"),
    plot.title = element_text(colour = "white"),
    legend.text = element_text(colour = "azure3"),
    legend.title = element_text(colour = "azure3"),
    panel.background = element_rect(fill = "azure3",
        colour = "azure3"), plot.background = element_rect(fill = "azure3",
        colour = "azure3"), legend.key = element_rect(fill = "azure3",
        colour = "azure3"), legend.background = element_rect(fill = "azure3",
        colour = "azure3")) + labs(colour = "white") 

gganimate::animate(aa, nframes = 100, fps = 25, end_pause = 1)


#Esfuerzo/Presión fiscal

PF_long <- rio::import("./datos/PF_long.csv")

library(readr)
PF_long <- read_csv("./datos/PF_long.csv", 
                    col_types = cols(time = col_date(format = "%Y/%m"), 
                                     value = col_number()))
PF_long <- PF_long %>%
  filter(time == "2020-01-01")

#Pib_cap_eu <- rio::import("./datos/Pib_cap_eu.xlsx")

#library(readxl)

#Pib_cap_eu <- read_csv("./datos/Pib_cap_eu.xlsx", 
#                        col_types = c("text", "numeric"))

#rio::export(Pib_cap_eu, "./datos/Pib_cap_eu.csv")

Pib_cap_eu <- rio::import("./datos/Pib_cap_eu.csv")

Pib_cap_eu <- Pib_cap_eu %>%
  rename("country" = "Countries")

E_P <- left_join(Pib_cap_eu, PF_long, by = "country") 

E_P_1 <- E_P %>%
  filter(!country %in% c("Czech Republic", "malt")) %>%
  rename("Presion_fisc" = "value") %>%
  mutate(esfuerzo_fisc = Presion_fisc/PIB_cap * 100) %>%
  mutate(Presion_fisc = Presion_fisc/100) %>%
  mutate(dif = Presion_fisc - esfuerzo_fisc) %>%
  tidyr::pivot_longer(cols = 4:5, names_to = "tipo")
  

pf <- E_P_1  %>%
  filter(tipo == "Presion_fisc")

ef <- E_P_1  %>%
  filter(tipo == "esfuerzo_fisc")


p <- ggplot(E_P_1) +
  geom_segment(data = ef ,
               aes(x = value, y = country,
                   yend = pf$country, xend = pf$value), 
               color = "#8D8E91",
               size = 3.5, 
               alpha = .5) +
  geom_point(aes(x = value, y = country, color = tipo), size = 5, show.legend = TRUE) +
  scale_color_manual(breaks = c("Presion_fisc","esfuerzo_fisc"), 
                     values = c("#FF0000", "#0040FF"), 
                     labels = c("Presión Fiscal", "Esfuerzo Fiscal")) +
  ggtitle("Presión Fiscal vs Esfuerzo Fiscal para la Eurozona 2020") +
  labs(caption = "Elaboración propia con datos de Eurostat") +
  theme(panel.background = element_rect(fill = "transparent")) +
  labs(x = NULL) +
  labs(y = NULL) +
  guides(color= guide_legend(title = "Indicador")) +
  theme(axis.text.x = element_text(face="bold", vjust=-0.5, size= 10)) +
  theme(axis.text.y = element_text(face="bold", vjust=1.5, size= 10)) +
  geom_text(data = ef ,
            aes(x = value, y = country, label = scales::percent(value, 2), colour = tipo), vjust = 0.25,hjust = 1.45, size = 3) +
  geom_text(data = pf ,
            aes(x = value, y = country, label = scales::percent(value, 2), colour = tipo), vjust = 0.25,hjust = - 0.45, size = 3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(vjust = 0))+
  theme(legend.position = "bottom")

p


#Tabla tipo de interés medio 

coste_deuda <- rio::import("./datos/coste_deuda.xlsx")

knitr::kable(coste_deuda, digits = 2) %>%
  kableExtra::kable_styling(fixed_thead = list(enabled = T,
                                               background = "#088A85"),
                            font_size = 15,
                            ) %>%
  column_spec(6,  color = "#000000", background = "#A9F5F2") 



bb <- coste_deuda

coste_deuda_long <- coste_deuda %>% 
  tidyr::pivot_longer(cols = 2:6, names_to = "tipo") %>%
  mutate(value_1 = value/100)

aa <- ggplot() +
  geom_point(data = coste_deuda_long, aes(x = FECHA, y = value_1, shape = tipo, color = tipo), size = 3.5) +
  geom_line(data = coste_deuda_long, aes(x = FECHA, y = value_1, group = tipo, color = tipo), size = 1.5) +
  scale_shape_manual(values = c(15, 16, 17,18, 19)) +
  scale_color_manual(breaks = c("LETRAS", "BONOS", "OBLIGACIONES", "RESTO", "TIPO MEDIO"),
                    values=c("#03A6FD", "#024B73", "#15017A", "#684BF9", "#04F3F7")) +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.background = element_rect(fill = "transparent")) +
  labs(title = "Evolución coste de la Deuda Española",
       caption = "Fuente: Elaboración propia con datos del Tesoro Público") +
  labs (y = NULL) +
  labs(x = NULL) +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10)) +
  guides (shape = FALSE) +
  geom_hline(yintercept = 0,
             size = 0.75,
             colour = "#F70413",
             linetype = "twodash")
            
aa

#Tenedores

aa <- rio::import("./datos/Distribucion_tenedores.xlsx")

Distribucion_tenedores <- aa %>% pivot_longer(cols = 2:12, names_to = "tenedor") %>%
rename("date" = "FECHA")

#rio::export(Distribucion_tenedores,"./datos/Distribucion_tenedores.csv")

library(readr)
Dist_tenedores <- read_csv("./datos/Distribucion_tenedores.csv", 
                                   col_types = cols(date = col_date(format = "%Y/%m"), 
                                                    value = col_number()))


Dist_tenedores <- Dist_tenedores %>%
  filter (!tenedor == "TOTAL")

aa <- ggplot() +
  geom_col(data = Dist_tenedores, aes(x = reorder(tenedor, value), y = value, fill = value)) +
  scale_fill_continuous(low="#5CD1FE", high="#025271") + 
  coord_flip() +
  guides (fill = FALSE)+
  labs (y = NULL) +
  labs(x = NULL) +
  theme(axis.text.x = element_text(size = 7.5, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold")) +
  theme(panel.background = element_rect(fill = "transparent")) +
  labs(title = "Distribución por tenedores de la deuda (2002-2020)",
       caption = "Fuente: Elaboración propia con datos del Tesoro Público") +
  transition_manual(date) +
  scale_y_continuous(labels = scales::unit_format(unit = "M€", scale = 1e+0))
 


gganimate::animate(aa, nframes = 100, fps = 10, end_pause = 50)

#Paro

#inflacion

inflacion_esp <- read_excel("datos/inflacion_esp.xlsx")

inflacion_esp <- inflacion_esp  %>% pivot_longer(cols = 2:25, names_to = "date")


#rio::export(inflacion_esp,"./datos/inflacion_esp.csv")

inflacion_esp <- read_csv("datos/inflacion_esp.csv", 
                          col_types = cols(date = col_date(format = "%Y/%m")))




inflacion_esp_gen <- inflacion_esp %>% filter(tipo == "general")
inflacion_esp_sub <- inflacion_esp %>% filter(tipo == "subyacente")


esp <- ggplot() +
  
  geom_line(data = inflacion_esp_gen, aes(date, value, colour = tipo), size = 1.5) +
  geom_point(data = inflacion_esp_gen, aes(date, value,  colour = tipo, shape = tipo), size = 3) +
  geom_text(data = inflacion_esp_gen, aes(date, value, label = value, colour = tipo), vjust = 2.5,hjust = 0.5, size = 3) +
  geom_text(data = inflacion_esp_gen, aes(date, value, label = "%", colour = tipo), vjust = 2.5,hjust = -1, size = 3) +
  
  geom_line(data = inflacion_esp_sub, aes(date, value,  colour = tipo), size = 1.5) +
  geom_point(data = inflacion_esp_sub, aes(date, value, colour = tipo, shape = tipo), size = 3) +
  geom_text(data = inflacion_esp_sub, aes(date, value, label = value, colour = tipo), vjust = -1.5,hjust = 0.75, size = 3) +
  geom_text(data = inflacion_esp_sub, aes(date, value, label = "%", colour = tipo), vjust = -1.5,hjust = -0.5, size = 3) +
  scale_shape_manual(values = c(15,17)) +
  
  scale_color_manual(breaks = c("general", "subyacente"),
                     values=c("#7C2203", "#F99F5C")) +
  
  geom_hline(yintercept = 0,
             size = 0.25,
             colour = "black",
             linetype = "dashed") +
  
  scale_x_date(date_labels="%b %y", date_breaks  = "1 month") +
  
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  
  lims(y= c(-1, 7)) +
  labs(x = NULL) +
  labs(caption = "Fuente: Elaboración propia con datos de INEbase") +
  theme(panel.grid.major = element_line(colour = NA)) + 
  theme(panel.background = element_rect(fill = "transparent")) +
  theme(legend.title=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(colour = "General")  +
  theme(plot.subtitle = element_text(colour = "gray24"),
                                    panel.grid.major = element_line(colour = "white"),
                                    panel.grid.minor = element_line(colour = "white"),
                                    panel.background = element_rect(colour = "white"),
                                    legend.position = "bottom", legend.background = element_rect(fill = "transparent"),
                                    legend.direction = "horizontal") +
  labs(title = "Evolución anual del IPC", subtitle = "Índice general y subyacente.") +
  guides(shape = FALSE) 

esp



