pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","PerformanceAnalytics","correlation","see","ggraph",
             "psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
             "equatiomatic","corrplot","reshape2","nlme","lmtest","fastDummies",
             "msm","lmeInfo","jtools","gganimate","ggridges","viridis","hrbrthemes","car","ggplot2")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


bd <- read.csv("BD_municipios_ano_criminalidade_desigualdade - bd.csv")
bd <- remove_missing(bd)
bd$ano <- as.factor(bd$ano)
bd$cod <- as.factor(bd$cod)
bd$uf <- as.factor(bd$uf)

summary(bd)

bd %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 22)



correlation <- cor(select(bd,-cod, -ano, -uf), use = "na.or.complete")
write_excel_csv(as.data.frame(correlation), file = "correlacao.csv")
corrplot(correlation, type = "lower", method = "number")

modelo_lm <- lm(formula = taxa_homicidio ~ .-cod -ano -uf , bd)
summary(modelo_lm)
loglik_modelo_lm <- logLik(modelo_lm)
sf.test(modelo_lm$residuals) #Como p-value é menor que 0.05 rejeitamos H0 e portanto os resíduos não seguem uma distribuição normal. Sendo assim, vamos aplicar a transformação de Box-cox.

lambda_BC <- powerTransform(bd$taxa_homicidio)
lambda_BC

bd$bc_taxa_homicidio <- (((bd$taxa_homicidio ^ lambda_BC$lambda) - 1) / 
                           lambda_BC$lambda)

modelo_lm_bc <- lm(formula = bc_taxa_homicidio ~ .-bd$tx_estupro -taxa_homicidio -cod -ano -uf -quantidade_estupro -quantidade_roubo_furto_veiculos -quantidade_roubos -tx_posse_entorpecente -tx_trafico_entorpecente -tx_roubo_furto_veiculos -tx_roubos -tx_estupro, bd)
summary(modelo_lm_bc)
loglik_modelo_lm_bc <- logLik(modelo_lm_bc)

step_modelo_lm_bc <- step(modelo_lm_bc, k = 3.841459)
summary(step_modelo_lm_bc)
loglik_step_modelo_lm_bc <-logLik(step_modelo_lm_bc)
bd$yhat_modelo_bc <- (((step_modelo_lm_bc$fitted.value*(lambda_BC$lambda))+
                            1))^(1/(lambda_BC$lambda))
sf.test(step_modelo_lm_bc$residuals)

bd$residuals <- bd$yhat_modelo_bc - bd$taxa_homicidio

sf.test(bd$residuals)

ggplotly(
  ggplot(bd, aes(x = yhat_modelo_bc, y = taxa_homicidio )) + 
    geom_point()
)



bd %>%
  group_by(uf) %>%
  mutate(rotulo = paste(uf, taxa_homicidio)) %>%
  ggplot(aes(x = as.numeric(ano), y = taxa_homicidio, label = rotulo)) +
  geom_point(aes(x = ano, y = taxa_homicidio), color = "#FDE725FF", alpha = 0.5, size = 3) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Região",
       y = "Corruption Perception Index") +
  geom_text_repel() +
  theme_bw()



step_modelo_lm <- step(modelo_lm, k = 3.841459)
summary(step_modelo_lm)
loglik_modelo_lm_step <- logLik(step_modelo_lm)


bd$yhat <- step_modelo_lm$fitted.values
bd$error <- step_modelo_lm$residuals


media_tx<-bd %>%
  group_by(uf) %>%
  summarise(avg = mean(taxa_homicidio, na.rm = TRUE)) %>%
  as.data.frame()

media_tx <-rename(media_tx, "Taxa de Homicídio Média" = "avg")

add_lines(ggplotly(
  ggplot(media_tx, aes(ano, taxa_homicidio)) + 
    add_lines()
  ))


ggplot(media_tx, x = ~ano, y = ~avg) %>%
  add_lines()


ggplotly(
  ggplot(bd, aes(x = bd$taxa_homicidio)) +
    geom_density(aes(x = taxa_homicidio), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 30) +
    theme_classic()
)


ggplotly(
  ggplot(bd, aes(x = theil.L, y = taxa_homicidio, colour = ano)) + 
    geom_point()
  )

sf.test(modelo_lm$residuals)

modelo_intercept_inclin_hlm2 <- lme(fixed = taxa_homicidio ~ atkinson + renda_per_capita_quinto_mais_pobre + renda_per_capita_quinto_mais_rico + populacao,
                                    random = ~ 1  | uf,
                                    data = bd,
                                    method = "REML")

summary(modelo_intercept_inclin_hlm2)

log<-modelo_intercept_inclin_hlm2$logLik

step_modelo_intercept_inclin_hlm2 <-(modelo_intercept_inclin_hlm2)

summary(step_modelo_intercept_inclin_hlm2)

