# Creando Dashboard con data de Airbnb

## 1. Descripción

En este proyecto analizaremos algunos datos sobre las propiedades que
la empresa **Airbnb** tiene disponible en la República Dominicana.

<img src="Reports/airbnb-logo.png" class="center">

Para reproducir el proyecto solo se necesita clonar el repositorio y
ejecutar `R/main.R` usando como motor [Microsoft R Open
4.0.2](https://mran.microsoft.com/open) y los maquetes listados en `renv.lock`
dentro de este repositorio.

## 2. Proceso de investigación

Para poder identificar los patrones que siguen los precios de las
diferentes propiedades dentro del conjunto de datos suministrados hemos 
utilizado la técnica de **re-sampleo bootstraping** para calcular 1,000 regresiones 
lineales por cada tipo de propiedad y evitar así los problemas desencadenados 
por no cumplir los supuestos de normalidad. Los resultados obtenidos 
mediante este procedimiento son los que a su vez serán comunicados de manera 
sencilla en el dashboard.

Como se puede ver a continuación, el año y el mes en que se hace la publicación
así como la provincia donde está ubicada la propiedad tienen un efecto
significativo sobre la variable de respuesta para muchos tipos de propiedades.

<img src="Reports/model-summary.png" class="center">


## 3. Presentación de resultados

Una vez conocido los efectos de las variables sobre los precios era
necesario diseñar visualizaciones que facilitaran la comunicación los
resultados con el resto del público y agruparlas en el siguiente dashboard
también disponible mediante los siguientes enlaces [público](https://app.powerbi.com/view?r=eyJrIjoiODEyYWFlZjMtYTVjYi00NzY4LTljMmEtOTc5ODg0YzI0MmI0IiwidCI6IjY4NTYxODFmLWRhZjgtNDcyNS1hYzUxLWRkOWY3ZGZlMmYyYiIsImMiOjF9) y [privado](https://app.powerbi.com/reportEmbed?reportId=57a7f0c0-fb26-4fb8-84e5-aca69f114c94&autoAuth=true&ctid=6856181f-daf8-4725-ac51-dd9f7dfe2f2b).

El archivo original y pdf de muestra están en la carpeta `Reports`
de este repositorio.

El cuadro de mando está dirigido a **personas que están planeando sus vacaciones y les gustaría encontrar un lugar para pasar algunas semanas al menor costo posible**.

<img src="Reports/dashboard.png" class="center">

### 3.1. Precio por cada tipo de propiedad y año

``` r
dataset <- AirbnbPrecios

RedColor <- "#d52121"
GreenColor <- "#4DC966"
GreyColor <- "#AAAAAA"

library(data.table)
library(ggplot2)
library(scales)
library(ggtext)
glue <- glue::glue
`%>%` <- magrittr::`%>%`

setDT(dataset)

PreciosYear <-
  dataset[, .(median_precio = median(precio, na.rm = TRUE)),
          by = c("tipo","year")]


PreciosYearCoef <-
  PreciosYear %>%
  lm(formula = median_precio ~ year) %>%
  coef() 


PreciosYearColor <-
  fifelse(PreciosYearCoef[2] < 0,
          GreenColor, RedColor)


PreciosYearSentido <-
  fifelse(PreciosYearCoef[2] < 0,
          "disminuyen", "incrementan")


PreciosYearTitle <-
  glue("Los precios <span style='color:{PreciosYearColor}'>**{PreciosYearSentido}**</span>",
       " en promedio ",
       "<span style='color:{PreciosYearColor}'>**",
       PreciosYearCoef[2] %>% abs() %>% dollar(accuracy = 1),
       "**</span> cada año")


ggplot(PreciosYear,
       aes(year, median_precio))+
  geom_blank(aes(x = year +0.65))+
  geom_line(aes(group = tipo),
            color = GreyColor)+
  geom_point(color = GreyColor, size = 0.75)+
  geom_smooth(method = "lm", 
              se = FALSE,
              lwd = 1,
              color = fifelse(PreciosYearCoef[2] > 0,
                              RedColor,
                              GreenColor) )+
  geom_text(aes(label = tipo,
                x = year +0.45), 
            color = GreyColor,
            check_overlap = TRUE,
            size = 3.5,
            data = PreciosYear[year == max(year)])+
  scale_y_log10(breaks = breaks_log(7),
                labels = dollar_format(accuracy = 1))+
  labs(title = PreciosYearTitle,
       subtitle = "Precio por cada tipo de propiedad y año",
       x = "Año",
       y = "Precio de las propiedades")+
  theme_classic()+
  theme(plot.title = element_markdown(size = 14))
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### 3.2. Precio de las propiedades por provincia

``` r
dataset <- AirbnbPrecios

RedColor <- "#d52121"
GreenColor <- "#4DC966"
GreyColor <- "#AAAAAA"

library(data.table)
library(ggplot2)
library(scales)
library(forcats)
library(ggtext)

glue <- glue::glue
`%>%` <- magrittr::`%>%`

setDT(dataset)

PreciosProvincia <-
  dataset[, .(median_precio = median(precio, na.rm = TRUE)),
          by = "provincia"
  ][median_precio > 0
  ][,`:=`(provincia = fct_reorder(provincia, median_precio, sum, .desc = TRUE),
          higher_value = median_precio == max(median_precio))]


PreciosProvinciaTitle <-
  glue("<span style='color:{RedColor}'>**",
       PreciosProvincia[higher_value == TRUE, 
                        as.character(provincia)],
       "**</span> tiene los precios más elevados")


ggplot(PreciosProvincia,
       aes(median_precio, fct_rev(provincia),
           fill = higher_value,
           color = higher_value))+
  geom_blank(aes(x = median_precio*1.15))+
  geom_col(width = 0.8)+
  geom_text(aes(label = dollar(median_precio, accuracy = 1)),
            hjust = -0.155)+
  scale_fill_manual(values = c("TRUE" = RedColor,
                               "FALSE" = GreyColor))+
  scale_color_manual(values = c("TRUE" = RedColor,
                                "FALSE" = GreyColor))+
  scale_x_continuous(labels = dollar_format(accuracy = 1))+
  labs(title = PreciosProvinciaTitle,
       subtitle = "Precio de las propiedades por provincia",
       x = "Precio de las propiedades",
       y = "Provincias")+
  theme_classic()+
  theme(legend.position = "none",
        plot.title = element_markdown(size = 14))
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### 3.3. Precio de las propiedades por mes

``` r
dataset <- AirbnbPrecios

RedColor <- "#d52121"
GreenColor <- "#4DC966"
GreyColor <- "#AAAAAA"

library(data.table)
library(ggplot2)
library(scales)
library(forcats)
library(ggtext)

glue <- glue::glue
`%>%` <- magrittr::`%>%`

setDT(dataset)
dataset[, month := factor(month, levels = month.abb)]


PreciosMonth <-
  dataset[,.(mediana_precio = median(precio)),
          by = "month"
  ][,`:=`(max_month = mediana_precio == max(mediana_precio) |
            mediana_precio / max(mediana_precio) >= 0.95,
          min_month = mediana_precio == min(mediana_precio) |
            mediana_precio / min(mediana_precio) <= 1.05) 
  ][,{ var_to_report <- 
       if(sum(max_month) < sum(min_month)){"max_month"}else{"min_month"}
    
    copy(.SD)[, `:=`(selected_var = var_to_report,
                     result_month = .SD[[var_to_report]])] }]


MonthColor <-
  if(unique(PreciosMonth$selected_var) == "max_month") {
    RedColor
  }else{
    GreenColor
  }


PreciosMonthTitle <-
  PreciosMonth[result_month == TRUE, 
               month.name[as.integer(month)]] %>%
  paste0(glue("<span style='color:{MonthColor}'>**"),
         . ,
         "**</span>") %>%
  (function(x) if(length(x) == 1){
        paste0(x, " es el mes ")
        }else{
          paste0(x[-length(x)], collapse = ", ") %>%
                  paste0(" y ", tail(x,1)," son los meses ")
        })() %>%
  paste0(fifelse(MonthColor == RedColor, 
                 "con mayor precio",
                 "con menor precio"))


ggplot(PreciosMonth,
       aes(mediana_precio,
           fct_rev(month), 
           color = result_month))+
  geom_blank(aes(x = mediana_precio * 1.1))+
  geom_point(size = 4)+
  geom_segment(aes(yend = month, xend = mediana_precio),
               x = 0,
               lwd = 1.5)+
  geom_text(aes(label = dollar(mediana_precio, accuracy = 1)),
            hjust = -0.40)+
  scale_x_continuous(labels = dollar_format(accuracy = 1))+
  scale_color_manual(values = c("FALSE" = GreyColor,
                                "TRUE" =  MonthColor))+
  labs(title = PreciosMonthTitle,
       subtitle = "Precio de las propiedades por mes",
       x = "Precio de las propiedades",
       y = "Meses")+
  theme_classic()+
  theme(legend.position = "none",
        plot.title = element_markdown())
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### 3.4. Habitaciones disponibles por provincia

``` r
dataset <- HabitacionesActivas

library(data.table)
library(ggplot2)
library(scales)
library(ggtext)

glue <- glue::glue
`%>%` <- magrittr::`%>%`

setDT(dataset)
dataset[, color_level := as.character(color_level)]

HabitacionesActivasTitle <-
  dataset[, .(total_level = sum(as.integer(color_level))),
          by = "provincia"
  ][total_level == max(total_level), provincia] %>%
  paste0("<span style='color:#d52121'>**", provincia = ., "**</span>") %>%
  (function(x) if(length(x) == 1){
    paste0(x, " provincia ")
  }else{
    paste0(x[-length(x)], collapse = ", ") %>%
      paste0(" y ", tail(x,1)," son las provincias ")
  })() %>%
  paste0("con la mayor disponibilidad")


ggplot(dataset,
       aes(n_habitaciones, provincia))+
  geom_tile(aes(fill = color_level),
            color = "white")+
  geom_text(aes(label = comma(propiedades_activas, accuracy = 1),
                color = color_level))+
  scale_fill_manual(values = c("white","#c6c6c6","#c88983","#bc4646") )+
  scale_color_manual(values = c("#AAAAAA","grey50", "white", "white") )+
  labs(title = HabitacionesActivasTitle,
       subtitle = "Habitaciones disponibles por provincia",
       x = "Número de habitaciones",
       y = "Provincias") +
  theme_classic()+
  theme(legend.position = "none",
        axis.line = element_blank(),
        plot.title = element_markdown(size = 13.3))
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
