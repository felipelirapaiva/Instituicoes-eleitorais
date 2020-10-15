# Artigo de Instituições Políticas Comparadas
# Autor: Felipe Lira Paiva

library(tidyverse)
library(foreign)
library(haven)
library(ggthemes)
library(knitr)
library(readr)
library(ggplot2)


# Baixando o banco de dados para o primeiro descritivo

link <- "https://github.com/felipelirapaiva/Instituicoes-eleitorais/blob/main/Totais.csv?raw=true"
download.file(link, "Totais.csv")
totais <- read.csv("Totais.csv", dec = ",")

 totais1 <- totais %>%
    rename(Poder = poder)

# Gráfico de frequência das respostas
ggplot(totais1, aes(fill=Poder, y=freq, x=grupo)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  scale_y_continuous(breaks = seq(0,50,10))+
  labs(title = "Figura 1",
       subtitle = "Frequência das respostas",
       x = " ",
       y = " ",
       caption = "Fonte: Elaboração própria a partir dos surveys")


########################################################################

# Baixando o banco 2 do GitHub

link <- "https://github.com/felipelirapaiva/Instituicoes-eleitorais/blob/main/Base_artigo.csv?raw=true"

download.file(link, "base_artigo.csv")

banco_embs <- read.csv("base_artigo.csv", dec = ",")

# Filtrando o banco de dados para retirar os descartados e os NA.

  embs <- banco_embs %>%
    filter(Tipo_emp_1 != "Descartado") %>%
    filter(vdem_aut_2017 != "NA")

# Vendo os descritivos
  embs %>%
    summarise(media = mean(vdem_aut_2017, na.rm = TRUE), 
              mediana = median(vdem_aut_2017, na.rm = TRUE), 
              minimo = min(vdem_aut_2017, na.rm = TRUE),
              maximo = max(vdem_aut_2017, na.rm = TRUE))
  
  embs %>%
    summarise(media = mean(vdem_aut_2017_m, na.rm = TRUE), 
              mediana = median(vdem_aut_2017_m, na.rm = TRUE), 
              minimo = min(vdem_aut_2017_m, na.rm = TRUE),
              maximo = max(vdem_aut_2017_m, na.rm = TRUE))

# Histograma - Apêndice
  ggplot(embs, aes(vdem_aut_2017)) +
    geom_histogram(bins = 10)+
    theme_classic()+
    labs(title = "Figura 6",
         subtitle = "Histograma da independência de facto (reescalada)",
         x = "Independência de facto",
         y = "Frequência das respostas",
         caption = "Fonte: Elaboração própria a partir dos surveys")
  
  ggplot(embs, aes(vdem_aut_2017_m))+
    geom_histogram(bins = 10)+
    theme_classic()+
    labs(title = "Figura 7",
         subtitle = "Histograma da independência de facto (média)",
         x = "Independência de facto (média)",
         y = "Frequência das respostas",
         caption = "Fonte: Elaboração própria a partir dos surveys")


########################
  
# Filtrando mais para retirar os que atendem aos critérios e renomeando
  embs1 <- embs %>%
    filter(Tipo_emp_1 != "SoCivil") %>%
    filter(Tipo_emp_1 != "SePublico") %>%
    filter(Tipo_emp_1 != "Partidos") %>%
    rename(Classificação1 = Tipo_emp_2)%>%
    rename(Classificação2 = Tipo_emptec_2)
    
# V-DEM 1 + Classificação 1
    ggplot(embs1, aes(Classificação1, vdem_aut_2017))+
            geom_boxplot()+
            theme_classic()+
        labs(title = "Figura 2",
                   subtitle = "Boxplot da classificação 1 e independência de facto reescalada",
                   y = "Independência de facto",
                   x = "",
                   caption = "Fonte: Elaboração própria a partir dos surveys e do V-DEM")
  

# V-DEM 1 + Classificação 2
    ggplot(embs1, aes(Classificação2, vdem_aut_2017))+
      geom_boxplot()+
      theme_classic()+
      labs(title = "Figura 3",
           subtitle = "Boxplot da classificação 2 e independência de facto reescalada",
           y = "Independência de facto",
           x = "",
           caption = "Fonte: Elaboração própria a partir dos surveys e do V-DEM")
  
    
# V-DEM 2 + Classificação 1
    ggplot(embs1, aes(Classificação1, vdem_aut_2017_m))+
      geom_boxplot()+
      theme_classic()+
      labs(title = "Figura 4",
           subtitle = "Boxplot da classificação 1 e independência de facto média",
           y = "Independência de facto",
           x = "",
           caption = "Fonte: Elaboração própria a partir dos surveys e do V-DEM")
  
# V-DEM 2 + Classificação 2
    ggplot(embs1, aes(Classificação2, vdem_aut_2017_m))+
      geom_boxplot()+
      theme_classic()+
      labs(title = "Figura 5",
           subtitle = "Boxplot da classificação 2 e independência de facto média",
           y = "Independência de facto",
           x = "",
           caption = "Fonte: Elaboração própria a partir dos surveys e do V-DEM")

    
########1    
    kruskal.test(vdem_aut_2017 ~ Classificação1, data = embs1)

    pairwise.wilcox.test(embs1$vdem_aut_2017, embs1$Classificação1,
                         p.adjust.method = "BH")
    
########2
    kruskal.test(vdem_aut_2017 ~ Classificação2, data = embs1)
    
    pairwise.wilcox.test(embs1$vdem_aut_2017, embs1$Classificação2,
                         p.adjust.method = "BH")
#######3  
    kruskal.test(vdem_aut_2017_m ~ Classificação1, data = embs1)

    pairwise.wilcox.test(embs1$vdem_aut_2017_m, embs1$Classificação1,
                         p.adjust.method = "BH")
#######4  
    kruskal.test(vdem_aut_2017_m ~ Classificação2, data = embs1)
    
    pairwise.wilcox.test(embs1$vdem_aut_2017_m, embs1$Classificação2,
                         p.adjust.method = "BH")
