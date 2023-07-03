pacotesMaps <- c("udunits2","units","geobr","sf","ggplot2","cowplot",
             "RColorBrewer","dplyr","rnaturalearth")

if(sum(as.numeric(!pacotesMaps %in% installed.packages())) != 0){
  instalador <- pacotesMaps[!pacotesMaps %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotesMaps, require, character = T) 
} else {
  sapply(pacotesMaps, require, character = T) 
}

estados <- read_state(code_state="all")
estados$code_state <-as.factor(estados$code_state)

media_tx <- bd2 %>%
  group_by(cod) %>%
  summarise(avg = mean(taxa_homicidio, na.rm = TRUE))

media_tx <- rename(media_tx,"code_state"="cod")

estados <-inner_join(estados,media_tx,by = "code_state")
estados <- rename(estados, "tx_homicidio"="avg")

ggplot(estados)+
  geom_sf(aes(fill = tx_homicidio))

          