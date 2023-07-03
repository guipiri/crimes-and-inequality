pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","PerformanceAnalytics","correlation","see","ggraph",
             "psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
             "corrplot","reshape2","nlme","lmtest","fastDummies",
             "msm","lmeInfo","jtools","gganimate","ggridges","viridis","hrbrthemes",
             "car","ggplot2","udunits2","units","geobr","sf","ggplot2","cowplot",
             "RColorBrewer","dplyr","rnaturalearth", "psych", "rgdal","RColorBrewer","rgeos","plyr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

########################### Carregando os dados e limpando #################

bd <- read.csv("BD_municipios_ano_criminalidade_desigualdade - bd.csv")
bd <- remove_missing(bd)
bd$ano <- as.factor(bd$ano)
bd$cod <- as.factor(bd$cod)
bd$uf <- as.factor(bd$uf)
bd[,20:25]<-NULL
bd[,21:22]<-NULL

########################### Histograma - Tx. Homicidio #####################

hist(bd$taxa_homicidio) #Imagem 1

########################### Gráfico: Tx. Brasil por ano ###########

bd$quantidade_homicidios <- (bd$populacao*bd$taxa_homicidio)/100000
bd$quantidade_trafico <- (bd$populacao*bd$tx_trafico_entorpecente)/100000
bd$quantidade_posse <- (bd$populacao*bd$tx_posse_entorpecente)/100000

tx_brasil <- data.frame(ano = c('2016','2017','2018','2019'))
tx_brasil$ano <- as.factor(tx_brasil$ano)

frame_aux <- bd[,c('populacao', 'quantidade_homicidios', 'ano')]
frame_aux <- remove_missing((frame_aux))
frame_aux <- frame_aux %>%
  group_by(ano) %>%
  summarise(qtd_homicidio = sum(quantidade_homicidios), pop = sum(populacao)) %>%
  merge(tx_brasil)
tx_brasil$tx_homc <- (frame_aux$qtd_homicidio*100000)/frame_aux$pop

frame_aux <- bd[,c('populacao', 'quantidade_roubos', 'ano')]
frame_aux <- remove_missing((frame_aux))
frame_aux <- frame_aux %>%
  group_by(ano) %>%
  summarise(qtd_roubo = sum(quantidade_roubos), pop = sum(populacao)) %>%
  merge(tx_brasil)
tx_brasil$tx_roubo <- (frame_aux$qtd_roubo*100000)/frame_aux$pop

frame_aux <- bd[,c('populacao', 'quantidade_posse', 'ano')]
frame_aux <- remove_missing((frame_aux))
frame_aux <- frame_aux %>%
  group_by(ano) %>%
  summarise(qtd_posse = sum(quantidade_posse), pop = sum(populacao)) %>%
  merge(tx_brasil)
tx_brasil$tx_posse <- (frame_aux$qtd_posse*100000)/frame_aux$pop

frame_aux <- bd[,c('populacao', 'quantidade_trafico', 'ano')]
frame_aux <- remove_missing((frame_aux))
frame_aux <- frame_aux %>%
  group_by(ano) %>%
  summarise(qtd_traf = sum(quantidade_trafico), pop = sum(populacao)) %>%
  merge(tx_brasil)
tx_brasil$tx_traf <- (frame_aux$qtd_traf*100000)/frame_aux$pop
  
ggplot(tx_brasil, aes(x = ano, y= tx_homc, group=1)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, title = "Tx. Homicídio", y = NULL, labs.size = 15) +
  geom_text(label = scales::comma(tx_brasil$tx_homc), nudge_x = 0.25, size = 8)+
  theme(plot.title = element_text(size = 20), axis.text = element_text(size = 20))

ggplot(tx_brasil, aes(x = ano, y= tx_roubo, group=1)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, title = "Tx. Roubo", y = NULL) +
  geom_text(label = scales::comma(tx_brasil$tx_roubo), nudge_x = 0.25, size = 8)+
  theme(plot.title = element_text(size = 20), axis.text = element_text(size = 20))

ggplot(tx_brasil, aes(x = ano, y= tx_traf, group=1)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, title = "Tx. Tráfico", y = NULL) +
  geom_text(label = scales::comma(tx_brasil$tx_traf), nudge_x = 0.25, size = 8)+
  theme(plot.title = element_text(size = 20), axis.text = element_text(size = 20))

ggplot(tx_brasil, aes(x = ano, y= tx_posse, group=1)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, title = "Tx. Posse Entorp.", y = NULL) +
  geom_text(label = scales::comma(tx_brasil$tx_posse), nudge_x = 0.25, size = 8)+
  theme(plot.title = element_text(size = 20), axis.text = element_text(size = 20))

########################### Mapa Brasil - Tx. Homicidio ####################

estados <- read_state(code_state="all")
estados$code_state <-as.factor(estados$code_state)

media_hmc_uf <- bd %>%
  group_by(cod) %>%
  summarise(avg = mean(taxa_homicidio, na.rm = TRUE))

media_hmc_uf <- rename(media_hmc_uf,"code_state"="cod")

estados <-inner_join(estados, media_hmc_uf, by = "code_state")
estados <- rename(estados, "tx_homicidio"="avg")

ggplot(estados)+
  geom_sf(aes(fill = tx_homicidio))

########################### Tabela com as estat. desc. #########################

summary(bd$taxa_homicidio)

describe <- describe(bd[,c('taxa_homicidio')], trim = .1)
describe<-print(describe, digits = 2)
describe[,c('vars', 'n', 'trimmed', 'skew', 'kurtosis','se', 'mad')] <- NULL

row.names(describe) <- "Taxa de Homicídio"

kable(describe)%>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

########################### Mapas da Criminalidade #############################

#-------------------------- coisas do mapa ------------------------------------#

shape <- readOGR("BR_UF_2022/", "BR_UF_2022")
shape@data
centroides <- as.data.frame(rgeos::gCentroid(shape, byid = TRUE))
centroides$Regiao <- shape@data$SIGLA_UF
shape.f <- fortify(shape, region = 'SIGLA_UF')

#-------------------------- calculando as medias por estado -------------------#
media_hmc_uf <- bd %>%
  group_by(uf) %>%
  summarise(avg = mean(taxa_homicidio, na.rm = TRUE)) %>%
  rename("Regiao"="uf", "Tx.Homicidio"="avg")

media_rb_uf <- bd %>%
  group_by(uf) %>%
  summarise(avg = mean(tx_roubos, na.rm = TRUE)) %>%
  rename("Regiao"="uf", "Tx.Roubo"="avg")

media_trf_uf <- bd %>%
  group_by(uf) %>%
  summarise(avg = mean(tx_trafico_entorpecente, na.rm = TRUE)) %>%
  rename("Regiao"="uf", "Tx.Trafico"="avg")

media_poss_uf <- bd %>%
  group_by(uf) %>%
  summarise(avg = mean(tx_posse_entorpecente, na.rm = TRUE)) %>%
  rename("Regiao"="uf", "Tx.Posse"="avg")

medias<-merge(media_hmc_uf, media_rb_uf)
medias<-merge(medias, media_trf_uf)
medias<-merge(medias, media_poss_uf)

#-------------------------- Salvando os mapas ---------------------------------#

ggplot(merge(medias, centroides)) +
  geom_map(map = shape.f, aes(map_id = Regiao, fill = Tx.Homicidio)) +
  geom_text(aes(x, y, label = Regiao), color = 'white') +
  expand_limits(x = shape.f$long, y = shape.f$lat) +
  coord_map() +
  theme_void()+
  theme(legend.title = element_text(size = 25), legend.text = element_text(size = 0))
  

ggplot(merge(medias, centroides)) +
  geom_map(map = shape.f, aes(map_id = Regiao, fill = Tx.Roubo)) +
  geom_text(aes(x, y, label = Regiao), color = 'white') +
  expand_limits(x = shape.f$long, y = shape.f$lat) +
  coord_map() +
  theme_void()+
  theme(legend.title = element_text(size = 25), legend.text = element_text(size = 0))

ggplot(merge(medias, centroides)) +
    geom_map(map = shape.f, aes(map_id = Regiao, fill = Tx.Trafico)) +
    geom_text(aes(x, y, label = Regiao), color = 'white') +
    expand_limits(x = shape.f$long, y = shape.f$lat) +
    coord_map() +
    theme_void()+
  theme(legend.title = element_text(size = 25), legend.text = element_text(size = 0))

ggplot(merge(medias, centroides)) +
    geom_map(map = shape.f, aes(map_id = Regiao, fill = Tx.Posse)) +
    geom_text(aes(x, y, label = Regiao), color = 'white') +
    expand_limits(x = shape.f$long, y = shape.f$lat) +
    coord_map() +
    theme_void()+
  theme(legend.title = element_text(size = 25), legend.text = element_text(size = 0))
########################### Análise das Correlações ############################

correlation <- cor(select(bd2,-cod, -ano, -uf), use = "na.or.complete")
correlation<-as.data.frame(correlation)
correlation[,1:16]<-NULL
correlation[,2:4]<-NULL
correlation$row_names <-row.names(correlation)
correlation <-correlation[order(correlation$taxa_homicidio,decreasing = c(FALSE)),]
row.names(correlation)<-c("Renda_quinto_pobre",
                          "Idhm",
                          "Idhm_renda",
                          "Idhm_edc",
                          "Renda_vuln_pobreza",
                          "Renda_quinto_rico",
                          "Pop",
                          "Dens_demo",
                          "Area",
                          "Renda_pobres",
                          "Prop_renda_20_por_40",
                          "Prop_renda_10_por_40",
                          "Theil",
                          "Gini",
                          "Renda_extrm_pobres",
                          "Extrm_pobres_pcent",
                          "Atkinson",
                          "Pobres_pcent",
                          "Vuln_pobreza_pcent",
                          "Tx_Homicidio")
correlation$row_names<-NULL
colnames(correlation)<-c("Tx_Homicidio")
kable(correlation)%>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 22)

write_excel_csv(as.data.frame(correlation), file = "correlacao.csv")
corrplot(correlation, type = "lower", method = "number")

########################### Modelo para Tx. Homicidio ##########################

lm_homicidio <- lm(taxa_homicidio ~ . -cod -uf -ano, bd)

summary(lm_homicidio)
summ(lm_homicidio, confint = T, digits = 4, ci.width = .95)
export_summs(lm_homicidio, scale = F, digits = 4)

step_lm_homicidio <- step(lm_homicidio, k = 3.841459)
summary(step_lm_homicidio)

bd$yhat_step_lm_homicidio <- step_lm_homicidio$fitted.values

########################### termos de erro aderentes a normalidade? ############

#Shapiro-Francia: n > 30
sf.test(step_lm_homicidio$residuals) #função 'sf.test' do pacote 'nortest'

#Plotando os resíduos do modelo step_lm_homicidio
bd %>%
  mutate(residuos = step_lm_homicidio$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequência") + 
  theme_bw()

#Acrescentando uma curva normal teórica para comparação entre as distribuições
bd %>%
  mutate(residuos = step_lm_homicidio$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_lm_homicidio$residuals),
                            sd = sd(step_lm_homicidio$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

########################### plot do y x yhat ###################################

lambda_BC <- powerTransform(bd$taxa_homicidio)
lambda_BC

bd$bc_taxa_homicidio <- (((bd$taxa_homicidio ^ lambda_BC$lambda) - 1) / 
                           lambda_BC$lambda)

modelo_lm_bc <- lm(formula = bc_taxa_homicidio ~ .-yhat_step_lm_homicidio -taxa_homicidio -cod -ano -uf, bd)
summary(modelo_lm_bc)

step_lm_bc <- step(modelo_lm_bc, k = 3.841459)
summary(step_lm_bc)

bd$yhat_step_lm__bc_homicidio <- (((step_lm_bc$fitted.values*(lambda_BC$lambda))+
                                                                    1))^(1/(lambda_BC$lambda))

sf.test(step_lm_bc$residuals)

ggplot(bd, aes(y = yhat_step_lm_homicidio, x = taxa_homicidio )) + 
  geom_point()
ggplot(bd, aes(y = yhat_step_lm__bc_homicidio, x = taxa_homicidio )) + 
  geom_point()

########################### cálculo dos logliks ################################

data.frame(OLS = logLik(lm_homicidio),
           OLS_STEP = logLik(step_lm_homicidio),
           BC = logLik(modelo_lm_bc),
           BC_STEP = logLik(step_lm_bc)) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 6) +
  labs(title = "Comparação do Ganho de LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","coral4","coral")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

lrtest(step_lm_homicidio, step_lm_bc)


