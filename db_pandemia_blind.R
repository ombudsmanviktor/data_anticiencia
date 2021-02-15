
# REQUISITAR PACOTES
library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)
library(dplyr)
library(hrbrthemes)

### TABELAS DE CONTINGÊNCIA

# PRINCIPAIS TELEFONES
db_pandemia_tels <- db_pandemia %>% 
  count(telefone) %>% 
  drop_na()

# INTERNACIONAL OU BRASIL
db_pandemia_internacional <- db_pandemia %>% 
  count(internacional)

# PRINCIPAIS PAÍSES
db_pandemia_ddi <- db_pandemia %>% 
  count(DDI)

# PRINCIPAIS ESTADOS
db_pandemia_ddd <- db_pandemia %>% 
  count(DDD)

# PRINCIPAIS TURNOS DO DIA
db_pandemia_turno <- db_pandemia %>% 
  count(turno)

# CONTAR MENSAGENS
db_pandemia_msgs <- db_pandemia %>% 
  count()
db_pandemia_msgs

db_pandemia_grupo <- db_pandemia %>% 
  count(grupo)

db_pandemia_grupo_tel <- db_pandemia %>% 
  count(grupo, telefone)

### GRÁFICOS

# HISTOGRAMA DE PRINCIPAIS SENDERS

db_pandemia_tels %>%
  filter( n<25 ) %>%
  ggplot( aes(x=n)) +
  geom_histogram( binwidth=1, alpha=0.9, fill="deepskyblue") +
  scale_fill_brewer(palette="Set3")+ guides(fill=FALSE) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  ) +
  labs(title = "Histograma dos principais remetentes de mensagens",
       subtitle = "",
       x = "Remetentes",
       y = "Mensagens enviadas",
       caption = "Fonte: [Omitido para revisão por pares]")


# BARPLOT DOS PRINCIPAIS GRUPOS RECIPIENTES

db_pandemia_grupo %>% 
  filter(n > 200) %>% 
  ggplot(aes(x=reorder(grupo, -n), y=n)) + 
  geom_bar(stat = "identity", fill="deepskyblue", alpha=.8, width=.9) +
  ylim(0, 2100) +
  #coord_flip() +
  theme_ipsum() + 
  theme (axis.text.x=element_blank(),
         axis.ticks=element_blank()) +
  labs(title = "Principais grupos recipientes de mensagens", 
       subtitle = "",
       x = "Grupo",
       y = "Mensagens enviadas",
       caption = "Fonte: [Omitido para revisão por pares]\n* Os nomes dos grupos foram anonimizados.")


# BARPLOT DOS PRINCIPAIS ESTADOS

db_pandemia_ddd %>% 
  filter(n > 200) %>% 
  ggplot(aes(x=reorder(DDD, n), y=n)) + 
  geom_bar(stat = "identity", fill="deepskyblue", alpha=.8, width=.9) +
  ylim(0, 5500) +
  coord_flip() +
  theme_ipsum() + 
  #theme (axis.text.x=element_blank(),
  #       axis.ticks=element_blank()) +
  labs(title = "Principais estados emissores de mensagens", 
       subtitle = "",
       x = "Estado",
       y = "Mensagens enviadas",
       caption = "Fonte: [Omitido para revisão por pares]")


# BARPLOT PARA TURNOS

db_pandemia_turno  %>%  ggplot(aes(x=reorder(turno, n), y=n)) + 
  geom_bar(stat = "identity", fill="deepskyblue", alpha=.8, width=.9) +
  ylim(0, 11000) +
  coord_flip() +
  theme_ipsum() + 
  #theme (axis.text.y=element_blank(),
  #       axis.ticks=element_blank()) +
  labs(title = "Turnos do dia com mais envios de mensagens", 
       subtitle = "",
       x = "Turno",
       y = "Mensagens enviadas",
       caption = "Fonte: [Omitido para revisão por pares]")


# SÉRIE TEMPORAL

db_pandemia %>% count(dia) %>% 
  ggplot( aes(x=as.Date(dia), y=n, group="anyClass")) +
  geom_line(stat = "identity", color = "deepskyblue", alpha=.8, size = .9) +
  #scale_color_viridis(discrete = FALSE) +
  theme_ipsum() +
  scale_x_date(date_breaks = "weeks" , date_labels = "%d-%b") +
  #theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  labs(title = "Série temporal de mensagens enviadas",
       subtitle = "",
       x = "Data",
       y = "Mensagens enviadas",
       caption = "Fonte: [Omitido para revisão por pares]")


# MENSAGENS QUE MAIS SE REPETEM

db_pandemia_msgs <- db_pandemia  %>%  
  count(texto) %>% 
  filter(n > 25 & n < 110)

db_pandemia_msgs %>% 
  ggplot(aes(x=reorder(texto, n), y=n)) + 
  geom_bar(stat = "identity", fill="deepskyblue", alpha=.8, width=.9) +
  ylim(0, 100) +
  coord_flip() +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) +
  theme_ipsum() + 
  theme (axis.text.y = element_text(size=7.5)) +
  #       axis.text.y=element_blank(),
  #       axis.ticks=element_blank()) +
  labs(title = "Mensagens mais enviadas", 
       subtitle = "",
       x = "Mensagem",
       y = "Frequência",
       caption = "Fonte: [Omitido para revisão por pares]")


# MENSAGENS QUE MAIS SE REPETEM POR USUÁRIO

db_pandemia_msgs_tels <- db_pandemia  %>%  
  count(texto,telefone)


# TABELAS DE AC
tab1 <- read.csv("~/Downloads/db_pandemia_corpus/tab1.csv")
tab2 <- read.csv("~/Downloads/db_pandemia_corpus/tab2.csv")
tab3 <- read.csv("~/Downloads/db_pandemia_corpus/tab3.csv")
tab4 <- read.csv("~/Downloads/db_pandemia_corpus/tab4.csv")
tab5 <- read.csv("~/Downloads/db_pandemia_corpus/tab5.csv")
tab6 <- read.csv("~/Downloads/db_pandemia_corpus/tab6.csv")

tab1 %>% 
  ggplot(aes(x=Formato.da.Mensagem, y=Percentual)) + 
  geom_bar(stat = "identity", fill="deepskyblue", alpha=.8, width=.9) +
  ylim(0, 100) +
  geom_text(aes(label=Percentual), color = "#4a4a4a", size = 3, vjust=-1) +
  theme_ipsum() + 
  labs(title = "", 
       subtitle = "",
       x = "Formato Textual",
       y = "Frequência % (N = 908)",
       caption = "")

tab2 %>% 
  ggplot(aes(x=Tipo.de.link, y=Percentual)) + 
  geom_bar(stat = "identity", fill="deepskyblue", alpha=.8, width=.9) +
  ylim(0, 100) +
  geom_text(aes(label=Percentual), color = "#4a4a4a", size = 3, vjust=-1) +
  theme_ipsum() + 
  labs(title = "", 
       subtitle = "",
       x = "Tipo de link",
       y = "Frequência % (N = 908)",
       caption = "")

tab3 %>% 
  ggplot(aes(x=Tipo.de.manifestação.do.descrédito, y=Percentual)) + 
  geom_bar(stat = "identity", fill="deepskyblue", alpha=.8, width=.9) +
  ylim(0, 100) +
  geom_text(aes(label=Percentual), color = "#4a4a4a", size = 3, hjust=-1) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position="none",
        axis.text.x = element_text(angle = 80, hjust=-1)) +
  coord_flip() +
  theme_ipsum() + 
  labs(title = "", 
       subtitle = "",
       x = "Manifestação de descrédito",
       y = "Frequência % (N = 282)",
       caption = "")

tab4 %>% 
  ggplot(aes(x=Fonte.de.legitimidade, y=Percentual)) + 
  geom_bar(stat = "identity", fill="deepskyblue", alpha=.8, width=.9) +
  ylim(0, 100) +
  geom_text(aes(label=Percentual), color = "#4a4a4a", size = 3, hjust=-1) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position="none",
        axis.text.x = element_text(angle = 80, hjust=-1)) +
  coord_flip() +
  theme_ipsum() + 
  labs(title = "", 
       subtitle = "",
       x = "Fonte de legitimidade",
       y = "Frequência % (N = 908)",
       caption = "")

tab5 %>% 
  ggplot(aes(x=Abordagens, y=Percentual)) + 
  geom_bar(stat = "identity", fill="deepskyblue", alpha=.8, width=.9) +
  ylim(0, 100) +
  geom_text(aes(label=Percentual), color = "#4a4a4a", size = 3, hjust=-1) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position="none",
        axis.text.x = element_text(angle = 80, hjust=-1)) +
  coord_flip() +
  theme_ipsum() + 
  labs(title = "", 
       subtitle = "",
       x = "Abordagens",
       y = "Frequência %",
       caption = "")

tab6 %>% 
  ggplot(aes(x=Link, y=Percentual)) + 
  geom_bar(stat = "identity", fill="deepskyblue", alpha=.8, width=.9) +
  ylim(0, 10) +
  geom_text(aes(label=Percentual), color = "#4a4a4a", size = 3, hjust=-1) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position="none",
        axis.text.x = element_text(angle = 80, hjust=-1)) +
  coord_flip() +
  theme_ipsum() + 
  labs(title = "", 
       subtitle = "",
       x = "Fonte",
       y = "Frequência %",
       caption = "")
