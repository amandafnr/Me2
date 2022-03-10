#Importando amostra 202064835
data <- read.csv("amostra_202064835.csv")
# Pacotes
library(statsr)
library(tidyverse)
# Gerais
z_95 <- qnorm(0.975)
# Amostras n = 20
amostras_20 <- data %>% rep_sample_n(size = 20, reps = 50, replace = FALSE)
# Amostras n = 200
amostras_200 <- data %>% rep_sample_n(size = 200, reps = 50, replace = TRUE)
# Proporção de alunos do interior n=20
Interior <- (nrow(data[data$AREA == "2", ])/2000)
amostra_20interior<-amostras_20 %>% filter(AREA == "2") 

amostra_20interiorP<-amostra_20interior %>% summarise(inferior = (length(replicate)/20) - z_95 * sqrt(((length(replicate)/20)*(1 - (length(replicate)/20))/20)),
                                                    superior = (length(replicate)/20) + z_95 * sqrt(((length(replicate)/20)*(1 - (length(replicate)/20))/20))) %>%
  mutate(legenda = ifelse(inferior < Interior & superior > Interior, "Sim", "Não")) %>%
  mutate(prop = (nrow(data[data$AREA == "2", ])/2000))

ggplot(amostra_20interiorP, aes(replicate, prop )) +       
  geom_point(size = 1) +  labs(y="Intervalos de confiança", x="Amostras") +  theme_light() +
  geom_errorbar(aes(ymin = inferior, ymax = superior, color = legenda))+
  scale_colour_manual(name="O IC contém o parâmetro?", values = c("#33CCFF", "#660066")) +
  theme(legend.position = "top")
ggsave("interior20.pdf", width = 170, height = 95, units = "mm")
# Proporção de alunos do interior n=200
Interior <- (nrow(data[data$AREA == "2", ])/2000)
amostra_200interior<-amostras_200 %>% filter(AREA == "2") 

amostra_200interiorP<-amostra_200interior %>% summarise(inferior = (length(replicate)/200) - z_95 * sqrt(((length(replicate)/200)*(1 - (length(replicate)/200))/200)),
                                                      superior = (length(replicate)/200) + z_95 * sqrt(((length(replicate)/200)*(1 - (length(replicate)/200))/200))) %>%
  mutate(legenda = ifelse(inferior < Interior & superior > Interior, "Sim", "Não")) %>%
  mutate(prop = (nrow(data[data$AREA == "2", ])/2000))

ggplot(amostra_200interiorP, aes(replicate, prop )) +       
  geom_point(size = 1) +  labs(y="Intervalos de confiança", x="Amostras") +  theme_light() +
  geom_errorbar(aes(ymin = inferior, ymax = superior, color = legenda))+
  scale_colour_manual(name="O IC contém o parâmetro?", values = c("#33CCFF", "#660066")) +
  theme(legend.position = "top")
ggsave("interior200.pdf", width = 170, height = 95, units = "mm")
# Proporção de alunAs n=20
Feminino <- (nrow(data[data$SEXO == "B", ])/2000)
amostra_20Meninas<-amostras_20 %>% filter(SEXO == "B") 

amostra_20MeninasP<-amostra_20Meninas %>% summarise(inferior = (length(replicate)/20) - z_95 * sqrt(((length(replicate)/20)*(1 - (length(replicate)/20))/20)),
                                                  superior = (length(replicate)/20) + z_95 * sqrt(((length(replicate)/20)*(1 - (length(replicate)/20))/20))) %>%
mutate(legenda = ifelse(inferior < Feminino & superior > Feminino, "Sim", "Não")) %>%
  mutate(prop = (nrow(data[data$SEXO == "B", ])/2000))

ggplot(amostra_20MeninasP, aes(replicate, prop )) +       
  geom_point(size = 1) +  labs(y="Intervalos de confiança", x="Amostras") +  theme_light() +
  geom_errorbar(aes(ymin = inferior, ymax = superior, color = legenda))+
  scale_colour_manual(name="O IC contém o parâmetro?", values = c("#33CCFF", "#660066")) +
  theme(legend.position = "top")
ggsave("sexo20.pdf", width = 170, height = 95, units = "mm")
# Proporção de alunAs n=200
Feminino <- (nrow(data[data$SEXO == "B", ])/2000)
amostra_200Meninas<-amostras_200 %>% filter(SEXO == "B") 

amostra_200MeninasP<-amostra_200Meninas %>% summarise(inferior = (length(replicate)/200) - z_95 * sqrt(((length(replicate)/200)*(1 - (length(replicate)/200))/200)),
                                                    superior = (length(replicate)/200) + z_95 * sqrt(((length(replicate)/200)*(1 - (length(replicate)/200))/200))) %>%
  mutate(legenda = ifelse(inferior < Feminino & superior > Feminino, "Sim", "Não")) %>%
  mutate(prop = (nrow(data[data$SEXO == "B", ])/2000))

ggplot(amostra_200MeninasP, aes(replicate, prop )) +       
  geom_point(size = 1) +  labs(y="Intervalos de confiança", x="Amostras") +  theme_light() +
  geom_errorbar(aes(ymin = inferior, ymax = superior, color = legenda))+
  scale_colour_manual(name="O IC contém o parâmetro?", values = c("#33CCFF", "#660066")) +
  theme(legend.position = "top")
ggsave("sexo200.pdf", width = 170, height = 95, units = "mm")
# Média das notas de português n=20
mediapt<- mean(data$NOTA_LP)


ptamostra20 <- amostras_20 %>%  summarise(inferior = mean(NOTA_LP) - z_95 * (sd(NOTA_LP) / sqrt(20)), 
                                                                              superior = mean(NOTA_LP) + z_95 * (sd(NOTA_LP) / sqrt(20)))%>%
  mutate(legenda = ifelse(inferior < mediapt & superior > mediapt, "Sim", "Não")) %>%
  mutate(media = mean(data$NOTA_LP))

ggplot(ptamostra20, aes(replicate, media )) +       
  geom_point(size=1) +  labs(y="Intervalos de confiança", x="Amostras") +  theme_light() +
  geom_errorbar(aes(ymin = inferior, ymax = superior, color = legenda))+
  scale_colour_manual(name="O IC contém o parâmetro?", values = c("#33CCFF", "#660066")) +
  theme(legend.position = "top")
ggsave("pt20.pdf", width = 170, height = 95, units = "mm")


# Média das notas de português n=200
mediapt<- mean(data$NOTA_LP)


ptamostra200 <- amostras_200 %>%  summarise(inferior = mean(NOTA_LP) - z_95 * (sd(NOTA_LP) / sqrt(200)), 
                                          superior = mean(NOTA_LP) + z_95 * (sd(NOTA_LP) / sqrt(200)))%>%
  mutate(legenda = ifelse(inferior < mediapt & superior > mediapt, "Sim", "Não")) %>%
  mutate(media = mean(data$NOTA_LP))

ggplot(ptamostra200, aes(replicate, media )) +       
  geom_point(size=1) +  labs(y="Intervalos de confiança", x="Amostras") +  theme_light() +
  geom_errorbar(aes(ymin = inferior, ymax = superior, color = legenda))+
  scale_colour_manual(name="O IC contém o parâmetro?", values = c("#33CCFF", "#660066")) +
  theme(legend.position = "top")
ggsave("pt200.pdf", width = 170, height = 95, units = "mm") 
# Média das notas de matemática n=20
mediaMT<- mean(data$NOTA_MT)


MTamostra20 <- amostras_20 %>%  summarise(inferior = mean(NOTA_MT) - z_95 * (sd(NOTA_MT) / sqrt(20)), 
                                          superior = mean(NOTA_MT) + z_95 * (sd(NOTA_MT) / sqrt(20)))%>%
  mutate(legenda = ifelse(inferior < mediaMT & superior > mediaMT, "Sim", "Não")) %>%
  mutate(media = mean(data$NOTA_MT))

ggplot(MTamostra20, aes(replicate, media )) +       
  geom_point(size=1) +  labs(y="Intervalos de confiança", x="Amostras") +  theme_light() +
  geom_errorbar(aes(ymin = inferior, ymax = superior, color = legenda))+
  scale_colour_manual(name="O IC contém o parâmetro?", values = c("#33CCFF", "#660066")) +
  theme(legend.position = "top")
ggsave("MT20.pdf", width = 170, height = 95, units = "mm")


# Média das notas de português n=200
mediaMT<- mean(data$NOTA_MT)


MTamostra200 <- amostras_200 %>%  summarise(inferior = mean(NOTA_MT) - z_95 * (sd(NOTA_MT) / sqrt(200)), 
                                            superior = mean(NOTA_MT) + z_95 * (sd(NOTA_MT) / sqrt(200)))%>%
  mutate(legenda = ifelse(inferior < mediaMT & superior > mediaMT, "Sim", "Não")) %>%
  mutate(media = mean(data$NOTA_MT))

ggplot(MTamostra200, aes(replicate, media )) +       
  geom_point(size=1) +  labs(y="Intervalos de confiança", x="Amostras") +  theme_light() +
  geom_errorbar(aes(ymin = inferior, ymax = superior, color = legenda))+
  scale_colour_manual(name="O IC contém o parâmetro?", values = c("#33CCFF", "#660066")) +
  theme(legend.position = "top")
ggsave("MT200.pdf", width = 170, height = 95, units = "mm") 
