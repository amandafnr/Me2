dados<-read.csv("amostra_202064835.csv")
pacman::p_load(tidyverse, statsr, lawstat, goftest, nortest, EnvStats, car, leaflet,leaflet.extras, BSDA )
set.seed(202064)
amostra_30<- dados %>% rep_sample_n(size= 30, rep=1, replace = FALSE)
set.seed(202064)
amostra_100<- dados %>% rep_sample_n(size= 100, rep=1, replace = FALSE)
# Notas de matemática segundo a localização da escola do estudante
amostra_30 <- amostra_30 %>%
  mutate(LOCALIZACAO = LOCALIZACAO %>%
           str_replace_all('1', 'Urbana') %>%
           str_replace_all('2', 'Rural'))

ggplot(amostra_30, aes(x=LOCALIZACAO, y=NOTA_MT)) + geom_boxplot(fill=c("pink"), width = 2) +
  labs(x="Localização", y="Notas de Matemática") +
  stat_summary(fun=mean, geom="point", shape=15, size=2) +
  theme_bw()
ggsave("MT30.png", width = 170, height = 95, units = "mm")


## Testando normalidade
shapiro.test(amostra_30$NOTA_MT) 

### Apresenta normalidade
## Testando homocedasticidade

urbana_30 <- amostra_30[amostra_30$LOCALIZACAO == "Urbana",]
rural_30 <- amostra_30[amostra_30$LOCALIZACAO == "Rural", ]

var.test(urbana_30$NOTA_MT, rural_30$NOTA_MT) ### Variâncias diferentes


t.test(urbana_30$NOTA_MT, rural_30$NOTA_MT, alternative = "greater", var.equal = F)
ks.test(rural_30$NOTA_MT, urbana_30$NOTA_MT, alternative = "two.sided")
wilcox.test(urbana_30$NOTA_MT, rural_30$NOTA_MT, alternative = "two.sided")
### Aceita-se H0
## Medidas de dispersão e variabilidade
b<-urbana_30 %>% 
  group_by(LOCALIZACAO) %>% 
  summarise(media = mean(NOTA_MT),
            desvio = sd(NOTA_MT),
            min = min(NOTA_MT),
            Q25 = quantile(NOTA_MT, probs =.25),
            mediana = quantile(NOTA_MT, probs= .5),
            max = max(NOTA_MT),
            Q75 = quantile(NOTA_MT, probs = .75))
a<-rural_30 %>% 
  group_by(LOCALIZACAO) %>% 
  summarise(media = mean(NOTA_MT),
            desvio = sd(NOTA_MT),
            min = min(NOTA_MT),
            Q25 = quantile(NOTA_MT, probs =.25),
            mediana = quantile(NOTA_MT, probs= .5),
            max = max(NOTA_MT),
            Q75 = quantile(NOTA_MT, probs = .75))

# Notas de português segundo a categoria administrativa da escola 
amostra_30 <- amostra_30 %>%
  mutate(DEPENDENCIA_ADM  = DEPENDENCIA_ADM  %>%
           str_replace_all('1', "Federal") %>%
           str_replace_all('2', "Estadual")%>%
           str_replace_all('3', "Municipal"))

amostra_30$DEPENDENCIA_ADM %<>%
  str_replace_all("^Federal$", "Estadual e Federal") %>%
  str_replace_all("^Estadual$", "Estadual e Federal")

ggplot(amostra_30, aes(x=DEPENDENCIA_ADM, y=NOTA_LP)) +
  geom_boxplot(fill=c("PINK"), width = 0.5) +
  labs(x="Categoria Administrativa", y="Notas de Português") +
  stat_summary(fun=mean, geom="point", shape=15, size=2) +
  theme_bw() 
ggsave("LP30.png", width = 170, height = 95, units = "mm")

shapiro.test(amostra_30$NOTA_MT) # Dados normais

# Testando homocedasticidade


estadual_30<- amostra_30 %>% filter(DEPENDENCIA_ADM == "Estadual e Federal")
municipal_30 <- amostra_30 %>% filter(DEPENDENCIA_ADM == "Municipal")


var.test(estadual_30$NOTA_LP, municipal_30$NOTA_LP) # Variâncias iguais


t.test(estadual_30$NOTA_LP, municipal_30$NOTA_LP, alternative = "two.sided", var.equal = F)
ks.test(estadual_30$NOTA_LP, municipal_30$NOTA_LP, alternative = "two.sided")
wilcox.test(estadual_30$NOTA_LP, municipal_30$NOTA_LP, alternative = "two.sided")
b<-estadual_30 %>% 
  group_by(DEPENDENCIA_ADM) %>% 
  summarise(media = mean(NOTA_MT),
            desvio = sd(NOTA_MT),
            min = min(NOTA_MT),
            Q25 = quantile(NOTA_MT, probs =.25),
            mediana = quantile(NOTA_MT, probs= .5),
            max = max(NOTA_MT),
        Q75 = quantile(NOTA_MT, probs = .75))
a<-municipal_30 %>% 
  group_by(DEPENDENCIA_ADM) %>% 
  summarise(media = mean(NOTA_MT),
            desvio = sd(NOTA_MT),
            min = min(NOTA_MT),
            Q25 = quantile(NOTA_MT, probs =.25),
            mediana = quantile(NOTA_MT, probs= .5),
            max = max(NOTA_MT),
            Q75 = quantile(NOTA_MT, probs = .75))

#AMOSTRA 100
# Notas de matemática segundo a localização da escola do estudante
amostra_100 <- amostra_100 %>%
  mutate(LOCALIZACAO = LOCALIZACAO %>%
           str_replace_all('1', 'Urbana') %>%
           str_replace_all('2', 'Rural'))

ggplot(amostra_100, aes(x=LOCALIZACAO, y=NOTA_MT)) + geom_boxplot(fill=c("pink"), width = 2) +
  labs(x="Localização", y="Notas de Matemática") +
  stat_summary(fun=mean, geom="point", shape=15, size=2) +
  theme_bw()
ggsave("MT100.png", width = 170, height = 95, units = "mm")

## Medidas de dispersão e variabilidade
amostra_100 %>% 
  group_by(LOCALIZACAO) %>% 
  summarise(media = mean(NOTA_MT),
            desvio = sd(NOTA_MT),
            min = min(NOTA_MT),
            Q25 = quantile(NOTA_MT, probs =.25),
            mediana = quantile(NOTA_MT, probs= .5),
            max = max(NOTA_MT),
            Q75 = quantile(NOTA_MT, probs = .75))
## Testando normalidade
shapiro.test(amostra_100$NOTA_MT) 

### Apresenta normalidade
## Testando homocedasticidade

urbana_100 <- amostra_100[amostra_100$LOCALIZACAO == "Urbana",]
rural_100 <- amostra_100[amostra_100$LOCALIZACAO == "Rural", ]

var.test(urbana_30$NOTA_MT, rural_30$NOTA_MT) ### Variâncias diferentes


t.test(urbana_100$NOTA_MT, rural_100$NOTA_MT, alternative = "two.sided", var.equal = F)
ks.test(rural_100$NOTA_MT, urbana_100$NOTA_MT, alternative = "two.sided")
wilcox.test(urbana_100$NOTA_MT, rural_100$NOTA_MT, alternative = "two.sided")
### Aceita-se H0

## Medidas de dispersão e variabilidade
b<-urbana_100 %>% 
  group_by(LOCALIZACAO) %>% 
  summarise(media = mean(NOTA_MT),
            desvio = sd(NOTA_MT),
            min = min(NOTA_MT),
            Q25 = quantile(NOTA_MT, probs =.25),
            mediana = quantile(NOTA_MT, probs= .5),
            max = max(NOTA_MT),
            Q75 = quantile(NOTA_MT, probs = .75))
a<-rural_100 %>% 
  group_by(LOCALIZACAO) %>% 
  summarise(media = mean(NOTA_MT),
            desvio = sd(NOTA_MT),
            min = min(NOTA_MT),
            Q25 = quantile(NOTA_MT, probs =.25),
            mediana = quantile(NOTA_MT, probs= .5),
            max = max(NOTA_MT),
            Q75 = quantile(NOTA_MT, probs = .75))

# Notas de português segundo a categoria administrativa da escola 
amostra_100 <- amostra_100 %>%
  mutate(DEPENDENCIA_ADM  = DEPENDENCIA_ADM  %>%
           str_replace_all('1', "Federal") %>%
           str_replace_all('2', "Estadual")%>%
           str_replace_all('3', "Municipal"))

amostra_100$DEPENDENCIA_ADM %<>%
  str_replace_all("^Federal$", "Estadual e Federal") %>%
  str_replace_all("^Estadual$", "Estadual e Federal")

ggplot(amostra_100, aes(x=DEPENDENCIA_ADM, y=NOTA_LP)) +
  geom_boxplot(fill=c("PINK"), width = 0.5) +
  labs(x="Categoria Administrativa", y="Notas de Português") +
  stat_summary(fun=mean, geom="point", shape=15, size=2) +
  theme_bw() 
ggsave("LP100.png", width = 170, height = 95, units = "mm")

shapiro.test(amostra_100$NOTA_MT) # Dados normais

# Testando homocedasticidade


estadual_100<- amostra_100 %>% filter(DEPENDENCIA_ADM == "Estadual e Federal")
municipal_100 <- amostra_100 %>% filter(DEPENDENCIA_ADM == "Municipal")


var.test(estadual_100$NOTA_LP, municipal_100$NOTA_LP) # Variâncias iguais


t.test(estadual_100$NOTA_LP, municipal_100$NOTA_LP, alternative = "two.sided", var.equal = T)
ks.test(estadual_100$NOTA_LP, municipal_100$NOTA_LP, alternative = "two.sided")
wilcox.test(estadual_100$NOTA_LP, municipal_100$NOTA_LP, alternative = "two.sided")


b<-estadual_100 %>% 
  group_by(DEPENDENCIA_ADM) %>% 
  summarise(media = mean(NOTA_MT),
            desvio = sd(NOTA_MT),
            min = min(NOTA_MT),
            Q25 = quantile(NOTA_MT, probs =.25),
            mediana = quantile(NOTA_MT, probs= .5),
            max = max(NOTA_MT),
            Q75 = quantile(NOTA_MT, probs = .75))
a<-municipal_100 %>% 
  group_by(DEPENDENCIA_ADM) %>% 
  summarise(media = mean(NOTA_MT),
            desvio = sd(NOTA_MT),
            min = min(NOTA_MT),
            Q25 = quantile(NOTA_MT, probs =.25),
            mediana = quantile(NOTA_MT, probs= .5),
            max = max(NOTA_MT),
            Q75 = quantile(NOTA_MT, probs = .75))

# Para a amostra de tamanho 30, verifique se há diferença entre as notas de língua portuguesa e matemática.
# Considere os testes: t de Student, Wilcoxon e Sinais. Comente os resultados.

# Testando normalidade
ks.test(amostra_30$NOTA_MT, amostra_30$NOTA_LP) # Dados não normais

var.test(amostra_30$NOTA_MT, amostra_30$NOTA_LP) # Variâncias diferentes

t.test(amostra_30$NOTA_MT, amostra_30$NOTA_LP, alternative = "two.sided", var.equal = F)


SIGN.test(amostra_30$NOTA_LP, amostra_30$NOTA_MT, md=0, alternative="two.sided") 
wilcox.test(amostra_30$NOTA_LP, amostra_30$NOTA_MT, paired=TRUE,alternative = "two.sided") #teste de postos com sinais de wilcoxon
t.test(amostra_30$NOTA_LP, amostra_30$NOTA_MT, paired=TRUE,alternative = "two.sided") #teste t para amostras pareadas

