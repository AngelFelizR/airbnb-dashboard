

# 1. Importing data ----

AirbnbData <-
  read_excel("Raw-data/Datos Airbnb.xlsx",
             skip = 2,
             col_names = ColNames) %>%
  as.data.table() %>%
  (\(DT) DT[, `:=`(fecha = as_date(fecha),
                   year = year(fecha),
                   month = factor(month.abb[month(fecha)],
                                  levels = month.abb))])()


# 2. Data reshaping ----


AirbnbPrecios <-
  AirbnbData[, .SD, .SDcols = !patterns("activas_\\dhab$|fecha|promedio$")
  ][, melt(.SD, 
           measure.vars = patterns("^precio"),
           variable.name = "tipo",
           variable.factor = FALSE,
           value.name = "precio")
  ][, `:=`(tipo = sub(pattern = "^precio_", 
                      replacement = "", 
                      x = tipo),
           provincia = fct_relevel(provincia, "Santo Domingo"))
  ][precio > 0]


HabitacionesActivas <-
  melt(AirbnbData, 
       id.vars = c("year","month","provincia"),
       measure.vars = patterns(propiedades_activas = "^propiedades_activas_\\d"),
       variable.name = "n_habitaciones",
       na.rm = TRUE
  )[, .(propiedades_activas = median(propiedades_activas)),
    by = .(provincia,
           n_habitaciones = as.integer(n_habitaciones))
  ][, color_level := 
      fcase(propiedades_activas >= quantile(propiedades_activas, 0.75),
            "4",
            propiedades_activas >= quantile(propiedades_activas, 0.5),
            "3",
            propiedades_activas >= quantile(propiedades_activas, 0.25),
            "2",
            default = "1")]

