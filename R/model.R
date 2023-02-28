

# Creating model ----

set.seed(2018)

ModelResults <-
  AirbnbPrecios[, rsample::bootstraps(.SD, times = 1000),
                by = "tipo"
  ][, lm(precio ~ ., data = splits[[1]]) %>% broom::tidy(),
    by = c("tipo","id")
  ][!is.na(estimate), 
    .(lower = quantile(estimate, 0.025),
      median = median(estimate),
      upper = quantile(estimate, 0.975)),
    by = c("tipo","term")
  ][, significativo := 
      dplyr::case_when(0 >= lower & 0 <= upper ~ "No effect",
                       0 < lower ~ "Positive effect",
                       0 > upper ~ "Negative effect",
                       TRUE~NA_character_) %>%
      factor(levels = c("No effect", 
                        "Negative effect",
                        "Positive effect"))]


# Saving results ----

ModelSummaryPlot <-
  ggplot(ModelResults,
         aes(tipo, term, fill = significativo))+
  geom_tile(color = "white")+
  scale_fill_manual(values = c("Positive effect" = GreenColor,
                               "Negative effect" = RedColor,
                               "No effect" = GreyColor))+
  labs(title = "Resultados del modelo",
       fill = "Term effect over price",
       x = "Tipo de propiedad", y = "Terminos del modelo")+
  theme_classic()+
  theme(axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave("Reports/model-summary.png",
       plot = ModelSummaryPlot,
       width = 13, height = 6)
