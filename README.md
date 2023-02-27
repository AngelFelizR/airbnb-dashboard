<style>

code{color: #0c5bd1;}

.center {
  display: block;
  margin-left: auto;
  margin-right: auto;
  width: 50%;
}

</style>

# 1. Introduction

Airbnb es una empresa

<img src="Raw-data/airbnb-logo.png" class="center">

# 2. Analysis results

## 2.1. La rentas estan aumentando cada año

``` r
PreciosYear <-
  AirbnbPrecios[, .(median_precio = median(precio, na.rm = TRUE)),
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
  glue("Los precios <span style='color:{PreciosYearColor};font-weight:bold'>{PreciosYearSentido}</span>",
       " en promedio ",
       "<span style='color:{PreciosYearColor};font-weight:bold'>",
       PreciosYearCoef[2] %>% abs() %>% dollar(accuracy = 1),
       "</span> cada año",
       "<br><sup>Precio por cada tipo de propiedad y año</sup>")


PreciosYearPlot <-
  ggplot(PreciosYear,
         aes(year, median_precio))+
  geom_line(aes(group = tipo),
            color = GreyColor)+
  geom_point(aes(text = paste0(tipo,": ",
                              dollar(median_precio, accuracy = 1))),
             color = GreyColor, size = 0.75)+
  geom_smooth(method = "lm", 
              se = FALSE,
              lwd = 1,
              color = fifelse(PreciosYearCoef[2] > 0,
                              RedColor,
                              GreenColor) )+
  geom_text(aes(label = tipo,
                x = year +0.25), 
            color = GreyColor,
            size = 3.5,
            data = PreciosYear[year == max(year)])+
  scale_y_log10(breaks = breaks_log(7),
                labels = dollar_format(accuracy = 1))+
  expand_limits(x = max(PreciosYear$year)+1)
```

    ## Warning in geom_point(aes(text = paste0(tipo, ": ", dollar(median_precio, : Ignoring unknown
    ## aesthetics: text

``` r
ggplotly(PreciosYearPlot, tooltip = "text") %>%
  layout(title = list(text = PreciosYearTitle,
                      xref="paper",
                      x=0,
                      font = list(size = 16)),
         xaxis = list(title = list(text = "Año",
                                   font = list(size = 14))),
         yaxis = list(title = list(text = "Precio de las propiedades",
                                   font = list(size = 14))))
```

    ## `geom_smooth()` using formula = 'y ~ x'

<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-4b0b11e0df7d1c98a12f" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-4b0b11e0df7d1c98a12f">{"x":{"data":[{"x":[2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022,null,2018,2019,2020,2021,2022],"y":[2.59019514831387,2.68987688926349,2.68975269613916,2.8245000644282,2.78867027820869,null,2.88761730033574,2.78461729263288,2.81438079446994,2.8877916581929,2.78070965641034,null,2.9103255245379,2.91513590662201,2.93196611472817,2.9900745563689,2.90247883055661,null,3.20180664295702,3.27743844226938,3.27474998017366,3.39954047125991,3.34061660852481,null,3.46962617126377,3.43609900948743,3.49594029460812,3.67662914639674,3.57890268604763,null,4.65018119987901,4.84361269743152,5.16250765181643,5.50585445658425,4.7003488695529,null,3.14860265480609,3.08564728829686,2.91089108864453,3.38524868240322,3.45606222445495,null,1.72370189399127,1.74927240829842,1.76908178711822,1.78561452494682,1.78411781646292,null,1.84565605998354,1.87311706943305,1.82840207849159,1.87526388687479,1.8850217948623,null,1.90725008288133,1.90308998699194,1.97680833733807,2.07918124604762,2.08163530150295,null,4.17068699831318,4.24870873560092,4.52559542844724,4.83370795248156,4.84355977777714,null,2.25959387888595,2.23552844690755,2.17324455906157,2.19648016770015,2.20491997583963,null,1.60632761246719,1.59239884611556,1.605574376061,1.66238002001625,1.68690426956818,null,3.12057393120585,2.92711361193376,3.06818586174616,3.56502092834529,3.68806369694634],"text":"","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(170,170,170,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[2018,2019,2020,2021,2022,2018,2019,2020,2021,2022,2018,2019,2020,2021,2022,2018,2019,2020,2021,2022,2018,2019,2020,2021,2022,2018,2019,2020,2021,2022,2018,2019,2020,2021,2022,2018,2019,2020,2021,2022,2018,2019,2020,2021,2022,2018,2019,2020,2021,2022,2018,2019,2020,2021,2022,2018,2019,2020,2021,2022,2018,2019,2020,2021,2022,2018,2019,2020,2021,2022],"y":[1.60632761246719,1.59239884611556,1.605574376061,1.66238002001625,1.68690426956818,1.72370189399127,1.74927240829842,1.76908178711822,1.78561452494682,1.78411781646292,1.84565605998354,1.87311706943305,1.82840207849159,1.87526388687479,1.8850217948623,1.90725008288133,1.90308998699194,1.97680833733807,2.07918124604762,2.08163530150295,2.25959387888595,2.23552844690755,2.17324455906157,2.19648016770015,2.20491997583963,2.59019514831387,2.68987688926349,2.68975269613916,2.8245000644282,2.78867027820869,2.88761730033574,2.78461729263288,2.81438079446994,2.8877916581929,2.78070965641034,2.9103255245379,2.91513590662201,2.93196611472817,2.9900745563689,2.90247883055661,3.20180664295702,3.27743844226938,3.27474998017366,3.39954047125991,3.34061660852481,3.46962617126377,3.43609900948743,3.49594029460812,3.67662914639674,3.57890268604763,4.17068699831318,4.24870873560092,4.52559542844724,4.83370795248156,4.84355977777714,3.12057393120585,2.92711361193376,3.06818586174616,3.56502092834529,3.68806369694634,3.14860265480609,3.08564728829686,2.91089108864453,3.38524868240322,3.45606222445495,4.65018119987901,4.84361269743152,5.16250765181643,5.50585445658425,4.7003488695529],"text":["presupuesto: $40","presupuesto: $39","presupuesto: $40","presupuesto: $46","presupuesto: $49","económica: $53","económica: $56","económica: $59","económica: $61","económica: $61","escala media: $70","escala media: $75","escala media: $67","escala media: $75","escala media: $77","exclusiva: $81","exclusiva: $80","exclusiva: $95","exclusiva: $120","exclusiva: $121","lujo: $182","lujo: $172","lujo: $149","lujo: $157","lujo: $160","1hab: $389","1hab: $490","1hab: $490","1hab: $668","1hab: $615","2hab: $772","2hab: $609","2hab: $652","2hab: $772","2hab: $604","3hab: $813","3hab: $822","3hab: $855","3hab: $977","3hab: $799","4hab: $1,592","4hab: $1,894","4hab: $1,883","4hab: $2,509","4hab: $2,191","5hab: $2,949","5hab: $2,730","5hab: $3,133","5hab: $4,749","5hab: $3,792","house/villa: $14,814","house/villa: $17,730","house/villa: $33,542","house/villa: $68,188","house/villa: $69,752","unique: $1,320","unique: $846","unique: $1,170","unique: $3,673","unique: $4,876","b&b: $1,408","b&b: $1,218","b&b: $814","b&b: $2,428","b&b: $2,858","apartment: $44,687","apartment: $69,761","apartment: $145,381","apartment: $320,520","apartment: $50,159"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(170,170,170,1)","opacity":1,"size":2.83464566929134,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(170,170,170,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[2018,2018.05063291139,2018.10126582278,2018.15189873418,2018.20253164557,2018.25316455696,2018.30379746835,2018.35443037975,2018.40506329114,2018.45569620253,2018.50632911392,2018.55696202532,2018.60759493671,2018.6582278481,2018.70886075949,2018.75949367089,2018.81012658228,2018.86075949367,2018.91139240506,2018.96202531646,2019.01265822785,2019.06329113924,2019.11392405063,2019.16455696203,2019.21518987342,2019.26582278481,2019.3164556962,2019.36708860759,2019.41772151899,2019.46835443038,2019.51898734177,2019.56962025316,2019.62025316456,2019.67088607595,2019.72151898734,2019.77215189873,2019.82278481013,2019.87341772152,2019.92405063291,2019.9746835443,2020.0253164557,2020.07594936709,2020.12658227848,2020.17721518987,2020.22784810127,2020.27848101266,2020.32911392405,2020.37974683544,2020.43037974684,2020.48101265823,2020.53164556962,2020.58227848101,2020.63291139241,2020.6835443038,2020.73417721519,2020.78481012658,2020.83544303797,2020.88607594937,2020.93670886076,2020.98734177215,2021.03797468354,2021.08860759494,2021.13924050633,2021.18987341772,2021.24050632911,2021.29113924051,2021.3417721519,2021.39240506329,2021.44303797468,2021.49367088608,2021.54430379747,2021.59493670886,2021.64556962025,2021.69620253165,2021.74683544304,2021.79746835443,2021.84810126582,2021.89873417722,2021.94936708861,2022],"y":[2.80149739748798,2.80423351484947,2.80696963221099,2.80970574957249,2.81244186693399,2.81517798429549,2.817914101657,2.8206502190185,2.82338633638,2.82612245374152,2.82885857110301,2.83159468846451,2.83433080582603,2.83706692318752,2.83980304054903,2.84253915791054,2.84527527527203,2.84801139263354,2.85074750999505,2.85348362735655,2.85621974471805,2.85895586207955,2.86169197944106,2.86442809680256,2.86716421416406,2.86990033152557,2.87263644888708,2.87537256624857,2.87810868361008,2.88084480097159,2.88358091833308,2.8863170356946,2.8890531530561,2.89178927041759,2.89452538777911,2.89726150514061,2.89999762250211,2.90273373986362,2.90546985722511,2.90820597458662,2.91094209194812,2.91367820930964,2.91641432667113,2.91915044403262,2.92188656139415,2.92462267875564,2.92735879611715,2.93009491347865,2.93283103084015,2.93556714820166,2.93830326556316,2.94103938292467,2.94377550028616,2.94651161764767,2.94924773500918,2.95198385237067,2.95471996973218,2.95745608709369,2.96019220445518,2.96292832181669,2.9656644391782,2.96840055653971,2.9711366739012,2.97387279126272,2.97660890862421,2.97934502598571,2.98208114334723,2.98481726070872,2.98755337807023,2.99028949543174,2.99302561279323,2.99576173015474,2.99849784751623,3.00123396487776,3.00397008223925,3.00670619960074,3.00944231696226,3.01217843432376,3.01491455168527,3.01765066904677],"text":"","type":"scatter","mode":"lines","name":"fitted values","line":{"width":3.77952755905512,"color":"rgba(240,128,128,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[2022.25,2022.25,2022.25,2022.25,2022.25,2022.25,2022.25,2022.25,2022.25,2022.25,2022.25,2022.25,2022.25,2022.25],"y":[1.68690426956818,1.78411781646292,1.8850217948623,2.08163530150295,2.20491997583963,2.78867027820869,2.78070965641034,2.90247883055661,3.34061660852481,3.57890268604763,4.84355977777714,3.68806369694634,3.45606222445495,4.7003488695529],"text":["presupuesto","económica","escala media","exclusiva","lujo","1hab","2hab","3hab","4hab","5hab","house/villa","unique","b&b","apartment"],"hovertext":["","","","","","","","","","","","","",""],"textfont":{"size":13.2283464566929,"color":"rgba(170,170,170,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"visible":false,"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":72.3287671232877},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[2017.75,2023.25],"tickmode":"array","ticktext":["2018","2019","2020","2021","2022","2023"],"tickvals":[2018,2019,2020,2021,2022,2023],"categoryorder":"array","categoryarray":["2018","2019","2020","2021","2022","2023"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"Año","font":{"color":"rgba(0,0,0,1)","family":"","size":14}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1.39672606559213,5.70152723710769],"tickmode":"array","ticktext":["$30","$100","$300","$1,000","$3,000","$10,000","$30,000","$100,000","$300,000"],"tickvals":[1.47712125471966,2,2.47712125471966,3,3.47712125471966,4,4.47712125471966,5,5.47712125471966],"categoryorder":"array","categoryarray":["$30","$100","$300","$1,000","$3,000","$10,000","$30,000","$100,000","$300,000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"Precio de las propiedades","font":{"color":"rgba(0,0,0,1)","family":"","size":14}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative","title":{"text":"Los precios <span style='color:#F08080;font-weight:bold'>incrementan<\/span> en promedio <span style='color:#F08080;font-weight:bold'>$3,159<\/span> cada año<br><sup>Precio por cada tipo de propiedad y año<\/sup>","xref":"paper","x":0,"font":{"size":16}}},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"4c2c1c47a2f":{"x":{},"y":{},"type":"scatter"},"4c2c13bb471d":{"x":{},"y":{},"text":{}},"4c2c63df514":{"x":{},"y":{}},"4c2c21a945ac":{"x":{},"y":{},"label":{}},"4c2c3d0355b4":{"x":{}}},"cur_data":"4c2c1c47a2f","visdat":{"4c2c1c47a2f":["function (y) ","x"],"4c2c13bb471d":["function (y) ","x"],"4c2c63df514":["function (y) ","x"],"4c2c21a945ac":["function (y) ","x"],"4c2c3d0355b4":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

## 2.2. Los precios cambian de provincia en provincia

``` r
PreciosProvincia <-
  AirbnbPrecios[, .(median_precio = median(precio, na.rm = TRUE),
                    tipos_no_disponibles = 
                      unique(AirbnbPrecios$tipo) %>% 
                      sort() %>% 
                      setdiff(tipo[precio>0]) %>%
                      paste0(collapse = ", ")),
                by = "provincia"
  ][median_precio > 0
  ][,`:=`(provincia = fct_reorder(provincia, median_precio, sum, .desc = TRUE),
          higher_value = median_precio == max(median_precio))]


PreciosProvinciaPlot <-
  ggplot(PreciosProvincia,
         aes(median_precio, fct_rev(provincia)))+
  geom_col(aes(fill = higher_value,
               text = 
                 fifelse(tipos_no_disponibles %like% "\\w",
                         paste0("<br>Propiedades ausentes: ", 
                                tipos_no_disponibles),
                         "") %>%
                 paste0("Mediana: ", dollar(median_precio, accuracy = 1),
                        next_line = .)))+
  scale_fill_manual(values = c("TRUE" = RedColor,
                               "FALSE" = GreyColor))+
  scale_x_continuous(breaks = breaks_width(200),
                     labels = dollar_format(accuracy = 1))+
  theme(legend.position = "none")
```

    ## Warning in geom_col(aes(fill = higher_value, text = fifelse(tipos_no_disponibles %like% :
    ## Ignoring unknown aesthetics: text

``` r
PreciosProvinciaTitle <-
  glue("<span style='color:{RedColor};font-weight:bold'>",
       PreciosProvincia[higher_value == TRUE, 
                        as.character(provincia)],
       "</span> tiene los precios más elevados",
       "<br><sup>Precio de las propiedades por provincia</sup>")


ggplotly(PreciosProvinciaPlot, tooltip = "text") %>%
  layout(title = list(text = PreciosProvinciaTitle,
                      xref="paper",
                      x=0,
                      font = list(size = 16)),
         xaxis = list(title = list(text = "Precio de las propiedades",
                                   font = list(size = 14))),
         yaxis = list(title = list(text = "Provincias",
                                   font = list(size = 14))))
```

<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-f57fe81533a271f27868" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-f57fe81533a271f27868">{"x":{"data":[{"orientation":"v","width":[947.58,205,398.14,260,320,988.875,409,682.5,275.24,463.32,154.25],"base":[9.55,1.55,5.55,2.55,4.55,10.55,6.55,8.55,3.55,7.55,0.55],"x":[473.79,102.5,199.07,130,160,494.4375,204.5,341.25,137.62,231.66,77.125],"y":[0.899999999999999,0.9,0.9,0.9,0.9,0.899999999999999,0.9,0.899999999999999,0.9,0.899999999999999,0.9],"text":["Mediana: $948","Mediana: $205<br>Propiedades ausentes: b&b","Mediana: $398<br>Propiedades ausentes: b&b","Mediana: $260","Mediana: $320<br>Propiedades ausentes: b&b","Mediana: $989","Mediana: $409<br>Propiedades ausentes: 1hab, b&b, unique","Mediana: $682<br>Propiedades ausentes: b&b","Mediana: $275<br>Propiedades ausentes: b&b","Mediana: $463<br>Propiedades ausentes: b&b","Mediana: $154<br>Propiedades ausentes: b&b"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(170,170,170,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"FALSE","legendgroup":"FALSE","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":1413.41,"base":11.55,"x":[706.705],"y":[0.899999999999999],"text":"Mediana: $1,413","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(240,128,128,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"TRUE","legendgroup":"TRUE","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":165.844748858448},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-70.6705,1484.0805],"tickmode":"array","ticktext":["$0","$200","$400","$600","$800","$1,000","$1,200","$1,400"],"tickvals":[0,200,400,600,800,1000,1200,1400],"categoryorder":"array","categoryarray":["$0","$200","$400","$600","$800","$1,000","$1,200","$1,400"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"Precio de las propiedades","font":{"color":"rgba(0,0,0,1)","family":"","size":14}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,12.6],"tickmode":"array","ticktext":["Valverde","San Francisco de Macorís","San José de Ocoa","Santiago Rodríguez","San Juan","San Cristóbal","Sánchez Ramírez","Santo Domingo Este","Santiago","Santo Domingo","San Pedro de Macorís","Samaná"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11,12],"categoryorder":"array","categoryarray":["Valverde","San Francisco de Macorís","San José de Ocoa","Santiago Rodríguez","San Juan","San Cristóbal","Sánchez Ramírez","Santo Domingo Este","Santiago","Santo Domingo","San Pedro de Macorís","Samaná"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"Provincias","font":{"color":"rgba(0,0,0,1)","family":"","size":14}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative","title":{"text":"<span style='color:#F08080;font-weight:bold'>Samaná<\/span> tiene los precios más elevados<br><sup>Precio de las propiedades por provincia<\/sup>","xref":"paper","x":0,"font":{"size":16}}},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"4c2c12ea660c":{"x":{},"y":{},"fill":{},"text":{},"type":"bar"}},"cur_data":"4c2c12ea660c","visdat":{"4c2c12ea660c":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

## 2.3. Algunos precios cambian en ciertos meses y provincias

``` r
PreciosMonth <-
  AirbnbPrecios[tipo == fread(here("model-output/propiedades-monthly-efect.csv"))[[1]][6] &
                  provincia == "Valverde",
                .(mediana_precio = median(precio)),
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
  paste0(glue("<span style='color:{MonthColor};font-weight:bold'>"),
         month = ., "</span>") %>%
  (\(x) if(length(x) == 1){
        paste0(x, " es el mes ")
        }else{
          paste0(x[-length(x)], collapse = ", ") %>%
                  paste0(" y ", tail(x,1)," son los meses ")
        })() %>%
  paste0(fifelse(MonthColor == RedColor, 
                 "con mayor precio",
                 "con menor precio"),
         "<br><sup>Precio de las propiedades por mes</sup>")


PreciosMonthPlot <-
  PreciosMonth %>%
  ggplot(aes(mediana_precio,
             fct_rev(month), 
             color = result_month))+
  geom_point(aes(text = dollar(mediana_precio, accuracy = 1)),
             size = 4)+
  geom_segment(aes(yend = month, xend = mediana_precio),
               x = 0,
               linewidth = 1.5)+
  scale_x_continuous(labels = dollar_format(accuracy = 1))+
  scale_color_manual(values = c("FALSE" = GreyColor,
                                "TRUE" =  MonthColor))+
  theme(legend.position = "none")
```

    ## Warning in geom_point(aes(text = dollar(mediana_precio, accuracy = 1)), : Ignoring unknown
    ## aesthetics: text

``` r
ggplotly(PreciosMonthPlot, tooltip = "text") %>%
  layout(title = list(text = PreciosMonthTitle,
                      xref="paper",
                      x=0,
                      font = list(size = 16)),
         xaxis = list(title = list(text = "Precio de las propiedades",
                                   font = list(size = 14))),
         yaxis = list(title = list(text = "Meses",
                                   font = list(size = 14))))
```

<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-f7372bc7955b86ed2d35" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-f7372bc7955b86ed2d35">{"x":{"data":[{"x":[390.5,824,425,491,696.5,469,949,836.5,616.5,641.5,446.5],"y":[8,7,6,5,4,3,2,1,12,10,9],"text":["$390","$824","$425","$491","$696","$469","$949","$836","$616","$642","$446"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(170,170,170,1)","opacity":1,"size":15.1181102362205,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(170,170,170,1)"}},"hoveron":"points","name":"FALSE","legendgroup":"FALSE","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[218],"y":[11],"text":"$218","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(77,201,102,1)","opacity":1,"size":15.1181102362205,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(77,201,102,1)"}},"hoveron":"points","name":"TRUE","legendgroup":"TRUE","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,390.5,null,0,824,null,0,425,null,0,491,null,0,696.5,null,0,469,null,0,949,null,0,836.5,null,0,616.5,null,0,641.5,null,0,446.5],"y":[8,8,null,7,7,null,6,6,null,5,5,null,4,4,null,3,3,null,2,2,null,1,1,null,12,12,null,10,10,null,9,9],"text":"","type":"scatter","mode":"lines","line":{"width":5.66929133858268,"color":"rgba(170,170,170,1)","dash":"solid"},"hoveron":"points","name":"FALSE","legendgroup":"FALSE","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,218],"y":[11,11],"text":"","type":"scatter","mode":"lines","line":{"width":5.66929133858268,"color":"rgba(77,201,102,1)","dash":"solid"},"hoveron":"points","name":"TRUE","legendgroup":"TRUE","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":43.1050228310502},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[181.45,985.55],"tickmode":"array","ticktext":["$200","$400","$600","$800"],"tickvals":[200,400,600,800],"categoryorder":"array","categoryarray":["$200","$400","$600","$800"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"Precio de las propiedades","font":{"color":"rgba(0,0,0,1)","family":"","size":14}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,12.6],"tickmode":"array","ticktext":["Dec","Nov","Oct","Sep","Aug","Jul","Jun","May","Apr","Mar","Feb","Jan"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11,12],"categoryorder":"array","categoryarray":["Dec","Nov","Oct","Sep","Aug","Jul","Jun","May","Apr","Mar","Feb","Jan"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"Meses","font":{"color":"rgba(0,0,0,1)","family":"","size":14}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative","title":{"text":"<span style='color:#4DC966;font-weight:bold'>February<\/span> es el mes con menor precio<br><sup>Precio de las propiedades por mes<\/sup>","xref":"paper","x":0,"font":{"size":16}}},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"4c2c21a02ecf":{"x":{},"y":{},"colour":{},"text":{},"type":"scatter"},"4c2c772113f1":{"x":{},"y":{},"colour":{},"yend":{},"xend":{}}},"cur_data":"4c2c21a02ecf","visdat":{"4c2c21a02ecf":["function (y) ","x"],"4c2c772113f1":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

## 2.4. Las habitaciones disponibles son diferentes en cada provincia

``` r
HabitacionesActivasTitle <-
  HabitacionesActivas[, .(total_level = sum(color_level == "4")),
                      by = "provincia"
  ][total_level == max(total_level), provincia] %>%
  paste0("<b>", provincia = ., "</b>") %>%
  (\(x) if(length(x) == 1){
    paste0(x, " es la provincia ")
  }else{
    paste0(x[-length(x)], collapse = ", ") %>%
      paste0(" y ", tail(x,1)," son las provincias ")
  })() %>%
  paste0("con la mayor disponibilidad de habitaciones",
         "<br><sup>Habitaciones disponibles por provincia</sup>")

HabitacionesActivasPlot <-
  ggplot(HabitacionesActivas,
         aes(n_habitaciones, provincia))+
  geom_tile(aes(fill = color_level),
            color = "white")+
  geom_text(aes(label = comma(propiedades_activas, accuracy = 1),
                color = color_level))+
  scale_fill_manual(values = c("white","#c6c6c6","#c88983","#bc4646") )+
  scale_color_manual(values = c(GreyColor,"grey50", "white", "white") )+
  theme(legend.position = "none",
        axis.line = element_blank())


ggplotly(HabitacionesActivasPlot, tooltip = "text") %>%
  layout(title = list(text = HabitacionesActivasTitle,
                      xref="paper",
                      x=0,
                      font = list(size = 16)),
         xaxis = list(title = list(text = "Número de habitaciones",
                                   font = list(size = 14))),
         yaxis = list(title = list(text = "Provincia",
                                   font = list(size = 14))))
```

<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-945f2e6f49467c759320" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-945f2e6f49467c759320">{"x":{"data":[{"x":[0.5,0.5,1.5,1.5,0.5,null,0.5,0.5,1.5,1.5,0.5,null,2.5,2.5,3.5,3.5,2.5,null,3.5,3.5,4.5,4.5,3.5,null,3.5,3.5,4.5,4.5,3.5,null,3.5,3.5,4.5,4.5,3.5,null,3.5,3.5,4.5,4.5,3.5,null,4.5,4.5,5.5,5.5,4.5,null,4.5,4.5,5.5,5.5,4.5,null,4.5,4.5,5.5,5.5,4.5,null,4.5,4.5,5.5,5.5,4.5],"y":[6.5,7.5,7.5,6.5,6.5,null,8.5,9.5,9.5,8.5,8.5,null,6.5,7.5,7.5,6.5,6.5,null,2.5,3.5,3.5,2.5,2.5,null,4.5,5.5,5.5,4.5,4.5,null,6.5,7.5,7.5,6.5,6.5,null,8.5,9.5,9.5,8.5,8.5,null,4.5,5.5,5.5,4.5,4.5,null,6.5,7.5,7.5,6.5,6.5,null,8.5,9.5,9.5,8.5,8.5,null,11.5,12.5,12.5,11.5,11.5],"text":"","type":"scatter","mode":"lines","line":{"width":0.377952755905512,"color":"rgba(255,255,255,1)","dash":"solid"},"fill":"toself","fillcolor":"rgba(255,255,255,1)","hoveron":"fills","name":"1","legendgroup":"1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.5,0.5,1.5,1.5,0.5,null,0.5,0.5,1.5,1.5,0.5,null,0.5,0.5,1.5,1.5,0.5,null,0.5,0.5,1.5,1.5,0.5,null,1.5,1.5,2.5,2.5,1.5,null,1.5,1.5,2.5,2.5,1.5,null,1.5,1.5,2.5,2.5,1.5,null,1.5,1.5,2.5,2.5,1.5,null,1.5,1.5,2.5,2.5,1.5,null,1.5,1.5,2.5,2.5,1.5,null,2.5,2.5,3.5,3.5,2.5,null,2.5,2.5,3.5,3.5,2.5,null,2.5,2.5,3.5,3.5,2.5,null,2.5,2.5,3.5,3.5,2.5,null,3.5,3.5,4.5,4.5,3.5,null,3.5,3.5,4.5,4.5,3.5,null,4.5,4.5,5.5,5.5,4.5,null,4.5,4.5,5.5,5.5,4.5],"y":[2.5,3.5,3.5,2.5,2.5,null,3.5,4.5,4.5,3.5,3.5,null,4.5,5.5,5.5,4.5,4.5,null,11.5,12.5,12.5,11.5,11.5,null,2.5,3.5,3.5,2.5,2.5,null,3.5,4.5,4.5,3.5,3.5,null,4.5,5.5,5.5,4.5,4.5,null,6.5,7.5,7.5,6.5,6.5,null,8.5,9.5,9.5,8.5,8.5,null,11.5,12.5,12.5,11.5,11.5,null,3.5,4.5,4.5,3.5,3.5,null,4.5,5.5,5.5,4.5,4.5,null,8.5,9.5,9.5,8.5,8.5,null,11.5,12.5,12.5,11.5,11.5,null,3.5,4.5,4.5,3.5,3.5,null,11.5,12.5,12.5,11.5,11.5,null,2.5,3.5,3.5,2.5,2.5,null,3.5,4.5,4.5,3.5,3.5],"text":"","type":"scatter","mode":"lines","line":{"width":0.377952755905512,"color":"rgba(255,255,255,1)","dash":"solid"},"fill":"toself","fillcolor":"rgba(198,198,198,1)","hoveron":"fills","name":"(2,1)","legendgroup":"(2,1)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.5,0.5,1.5,1.5,0.5,null,0.5,0.5,1.5,1.5,0.5,null,0.5,0.5,1.5,1.5,0.5,null,1.5,1.5,2.5,2.5,1.5,null,2.5,2.5,3.5,3.5,2.5,null,2.5,2.5,3.5,3.5,2.5,null,3.5,3.5,4.5,4.5,3.5,null,3.5,3.5,4.5,4.5,3.5,null,3.5,3.5,4.5,4.5,3.5,null,3.5,3.5,4.5,4.5,3.5,null,3.5,3.5,4.5,4.5,3.5,null,4.5,4.5,5.5,5.5,4.5,null,4.5,4.5,5.5,5.5,4.5,null,4.5,4.5,5.5,5.5,4.5,null,4.5,4.5,5.5,5.5,4.5,null,4.5,4.5,5.5,5.5,4.5],"y":[1.5,2.5,2.5,1.5,1.5,null,5.5,6.5,6.5,5.5,5.5,null,10.5,11.5,11.5,10.5,10.5,null,1.5,2.5,2.5,1.5,1.5,null,2.5,3.5,3.5,2.5,2.5,null,1.5,2.5,2.5,1.5,1.5,null,9.5,10.5,10.5,9.5,9.5,null,1.5,2.5,2.5,1.5,1.5,null,5.5,6.5,6.5,5.5,5.5,null,7.5,8.5,8.5,7.5,7.5,null,10.5,11.5,11.5,10.5,10.5,null,9.5,10.5,10.5,9.5,9.5,null,1.5,2.5,2.5,1.5,1.5,null,5.5,6.5,6.5,5.5,5.5,null,7.5,8.5,8.5,7.5,7.5,null,10.5,11.5,11.5,10.5,10.5],"text":"","type":"scatter","mode":"lines","line":{"width":0.377952755905512,"color":"rgba(255,255,255,1)","dash":"solid"},"fill":"toself","fillcolor":"rgba(200,137,131,1)","hoveron":"fills","name":"(3,1)","legendgroup":"(3,1)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.5,0.5,1.5,1.5,0.5,null,0.5,0.5,1.5,1.5,0.5,null,0.5,0.5,1.5,1.5,0.5,null,1.5,1.5,2.5,2.5,1.5,null,1.5,1.5,2.5,2.5,1.5,null,1.5,1.5,2.5,2.5,1.5,null,1.5,1.5,2.5,2.5,1.5,null,1.5,1.5,2.5,2.5,1.5,null,2.5,2.5,3.5,3.5,2.5,null,2.5,2.5,3.5,3.5,2.5,null,2.5,2.5,3.5,3.5,2.5,null,2.5,2.5,3.5,3.5,2.5,null,2.5,2.5,3.5,3.5,2.5,null,3.5,3.5,4.5,4.5,3.5,null,4.5,4.5,5.5,5.5,4.5],"y":[9.5,10.5,10.5,9.5,9.5,null,0.5,1.5,1.5,0.5,0.5,null,7.5,8.5,8.5,7.5,7.5,null,9.5,10.5,10.5,9.5,9.5,null,0.5,1.5,1.5,0.5,0.5,null,5.5,6.5,6.5,5.5,5.5,null,7.5,8.5,8.5,7.5,7.5,null,10.5,11.5,11.5,10.5,10.5,null,9.5,10.5,10.5,9.5,9.5,null,0.5,1.5,1.5,0.5,0.5,null,5.5,6.5,6.5,5.5,5.5,null,7.5,8.5,8.5,7.5,7.5,null,10.5,11.5,11.5,10.5,10.5,null,0.5,1.5,1.5,0.5,0.5,null,0.5,1.5,1.5,0.5,0.5],"text":"","type":"scatter","mode":"lines","line":{"width":0.377952755905512,"color":"rgba(255,255,255,1)","dash":"solid"},"fill":"toself","fillcolor":"rgba(188,70,70,1)","hoveron":"fills","name":"(4,1)","legendgroup":"(4,1)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,1,3,4,4,4,4,5,5,5,5],"y":[7,9,7,3,5,7,9,5,7,9,12],"text":["0","0","0","0","0","0","0","0","0","0","0"],"hovertext":["","","","","","","","","","",""],"textfont":{"size":14.6645669291339,"color":"rgba(170,170,170,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"1","legendgroup":"1","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,1,1,1,2,2,2,2,2,2,3,3,3,3,4,4,5,5],"y":[3,4,5,12,3,4,5,7,9,12,4,5,9,12,4,12,3,4],"text":["6","2","1","2","9","2","2","1","2","1","4","2","6","2","1","2","1","1"],"hovertext":["","","","","","","","","","","","","","","","","",""],"textfont":{"size":14.6645669291339,"color":"rgba(127,127,127,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"(2,1)","legendgroup":"(2,1)","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,1,1,2,3,3,4,4,4,4,4,5,5,5,5,5],"y":[2,6,11,2,3,2,10,2,6,8,11,10,2,6,8,11],"text":["15","142","102","24","23","17","56","13","70","42","18","36","10","33","24","10"],"hovertext":["","","","","","","","","","","","","","","",""],"textfont":{"size":14.6645669291339,"color":"rgba(255,255,255,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"(3,1)","legendgroup":"(3,1)","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,1,1,2,2,2,2,2,3,3,3,3,3,4,5],"y":[10,1,8,10,1,6,8,11,10,1,6,8,11,1,1],"text":["1,132","492","189","634","596","470","255","147","431","479","244","522","200","240","188"],"hovertext":["","","","","","","","","","","","","","",""],"textfont":{"size":14.6645669291339,"color":"rgba(255,255,255,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"(4,1)","legendgroup":"(4,1)","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":165.844748858448},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.25,5.75],"tickmode":"array","ticktext":["1","2","3","4","5"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["1","2","3","4","5"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"Número de habitaciones","font":{"color":"rgba(0,0,0,1)","family":"","size":14}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,12.6],"tickmode":"array","ticktext":["Samaná","San Cristóbal","San Francisco de Macorís","San José de Ocoa","San Juan","San Pedro de Macorís","Sánchez Ramírez","Santiago","Santiago Rodríguez","Santo Domingo","Santo Domingo Este","Valverde"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11,12],"categoryorder":"array","categoryarray":["Samaná","San Cristóbal","San Francisco de Macorís","San José de Ocoa","San Juan","San Pedro de Macorís","Sánchez Ramírez","Santiago","Santiago Rodríguez","Santo Domingo","Santo Domingo Este","Valverde"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"Provincia","font":{"color":"rgba(0,0,0,1)","family":"","size":14}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative","title":{"text":"<b>Samaná<\/b> es la provincia con la mayor disponibilidad de habitaciones<br><sup>Habitaciones disponibles por provincia<\/sup>","xref":"paper","x":0,"font":{"size":16}}},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"4c2cf7a402a":{"x":{},"y":{},"fill":{},"type":"scatter"},"4c2ce993e61":{"x":{},"y":{},"label":{},"colour":{}}},"cur_data":"4c2cf7a402a","visdat":{"4c2cf7a402a":["function (y) ","x"],"4c2ce993e61":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
