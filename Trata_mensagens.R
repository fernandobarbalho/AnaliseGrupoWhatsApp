library(tidyverse)
library(readr)5


ConversasWhatsApp <- read_delim("ConversasWhatsApp.txt", 
                                "&", escape_double = FALSE, col_names = FALSE, 
                                skip = 2)

names(ConversasWhatsApp) <- "Texto_Bruto"
conversas_trabalho<-ConversasWhatsApp 

conversas_trabalho$Refugo <-0 

conversas_trabalho <- conversas_trabalho %>% 
                      mutate(Refugo=replace(Refugo,substr(Texto_Bruto,3,3)!="/", 1))  

indice_refugo <- which(conversas_trabalho$Refugo==1)

inicio <- indice_refugo[1]-1
prim_sequencia <- indice_refugo[1] - 1

for (indice in indice_refugo){
  if (indice== prim_sequencia + 1 ){
    conversas_trabalho$Texto_Bruto[inicio]<- paste(conversas_trabalho$Texto_Bruto[inicio], conversas_trabalho$Texto_Bruto[indice])
    prim_sequencia <- indice
  } else {
    inicio<- indice -1
    prim_sequencia <- indice
    conversas_trabalho$Texto_Bruto[inicio]<- paste(conversas_trabalho$Texto_Bruto[inicio], conversas_trabalho$Texto_Bruto[indice])
  }

}

conversas_trabalho <- conversas_trabalho %>%
                      filter(Refugo == 0)

conversas_trabalho <- conversas_trabalho %>%
                      mutate(data_hora = substr(Texto_Bruto,1,regexpr(" - ", Texto_Bruto))) %>%
                      mutate(usuario = substr(Texto_Bruto,regexpr(" - ",Texto_Bruto)+3,regexpr(": ", Texto_Bruto)-1)) %>%
                      mutate(mensagem = substr(Texto_Bruto,regexpr(": ", Texto_Bruto)+2,10000) )

conversas_trabalho <- conversas_trabalho[, c(3:5)]


conversas_trabalho$data_hora  <- gsub("da manhã", "AM",conversas_trabalho$data_hora) 
conversas_trabalho$data_hora  <- gsub("da madrugada", "AM",conversas_trabalho$data_hora) 
conversas_trabalho$data_hora  <- gsub("da tarde", "PM",conversas_trabalho$data_hora) 
conversas_trabalho$data_hora  <- gsub("da noite", "PM",conversas_trabalho$data_hora) 

save(conversas_trabalho, file = "conversas_trabalho.RData")


analise_mensagem_dia %>% ggplot(mapping = aes(x=data, y=numero_mensagem)) +
  geom_bar(stat = "identity")
