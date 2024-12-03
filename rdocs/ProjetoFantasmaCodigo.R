#codigo que funciona
#Pacotes

library(readxl)
library(dplyr)
library(ggplot2)
library(pacman)
library(knitr)
library(kableExtra)
library(tinytex)
library(gt)

#Padronizações Estat


suppressMessages(suppressWarnings(
  suppressPackageStartupMessages(
    pacman::p_load(
      tidyverse, data.table, readxl, readr, ggcorrplot, cowplot,
      RColorBrewer, scales, nortest, xlsx,
      skimr, xtable, geobr, sf, ggrepel, abjutils, grDevices
    )
  )
))

cores_estat <- c(
  "#A11D21", "#003366", "#CC9900", "#663333", "#FF6600",
  "#CC9966", "#999966", "#006606", "#008091", "#041835",
  "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 10),
      axis.title.x = ggplot2::element_text(colour = "black", size = 10),
      axis.text = ggplot2::element_text(colour = "black", size = 10),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      text = element_text(family = "sans", size = 12),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat),
      scale_y_continuous(
        labels = scales::number_format(decimal.mark = ',',
                                       #accuracy = 0.01,
                                       big.mark = "."))
    )
  )
}

#Importação do banco de dados


Athenas <- (read_excel("~/PS/Olimpiadas 2000 - 2016.xlsx", sheet =1))
London <- (read_excel("~/PS/Olimpiadas 2000 - 2016.xlsx", sheet =2))
Rio <- (read_excel("~/PS/Olimpiadas 2000 - 2016.xlsx", sheet =3))
Sydney <- (read_excel("~/PS/Olimpiadas 2000 - 2016.xlsx", sheet =4))
Beijing <- (read_excel("~/PS/Olimpiadas 2000 - 2016.xlsx", sheet =5))

#Padronização das Colunas por Name, Sex, Age, Height, Weight, Team, Sport, Event, Medal e Year 

colnames(Athenas)[colnames(Athenas) == "Names"] <- "Name"
colnames(Athenas)[colnames(Athenas) == "Gender"] <- "Sex"
colnames(Athenas)[colnames(Athenas) == "Height (cm)"] <- "Height"
colnames(Athenas)[colnames(Athenas) == "Weight (lbs)"] <- "Weight"
Athenas$Year <- 2004

colnames(Beijing)[colnames(Beijing) == "Names"] <- "Name"
Beijing$Year <- 2008

London$Year <- 2012

colnames(Rio)[colnames(Rio) == "Age_year"] <- "Age"
colnames(Rio)[colnames(Rio) == "Height_cm"] <- "Height"
colnames(Rio)[colnames(Rio) == "Weight_lbs"] <- "Weight"
colnames(Rio)[colnames(Rio) == "Country"] <- "Team"
Rio$Year <- 2016

colnames(Sydney)[colnames(Sydney) == "N4m3"] <- "Name"
colnames(Sydney)[colnames(Sydney) == "S3x"] <- "Sex"
colnames(Sydney)[colnames(Sydney) == "4g3"] <- "Age"
colnames(Sydney)[colnames(Sydney) == "H31ght"] <- "Height"
colnames(Sydney)[colnames(Sydney) == "W31ght"] <- "Weight"
colnames(Sydney)[colnames(Sydney) == "T34m"] <- "Team"
colnames(Sydney)[colnames(Sydney) == "Sp0rt"] <- "Sport"
colnames(Sydney)[colnames(Sydney) == "3v3nt"] <- "Event"
colnames(Sydney)[colnames(Sydney) == "M3d4l"] <- "Medal"
Sydney$Year <- 2000

Olimpiadas <- rbind(Athenas, Beijing, London, Rio, Sydney)

write_xlsx(Olimpiadas,"C:/Users/Nathalia/OneDrive/Documentos/Labest")

write.table(Olimpiadas)

install.packages("writexl")
library(writexl)
write_xlsx(Olimpiadas, "olimpiadas_nova_tabela.xlsx")

## Analise 1 - Top paises medalhistas

Medalhistas_mulheres <- Olimpiadas %>%
  filter(Sex == "F" & !is.na(Medal)) %>%
  group_by(Team) %>%
  summarise(total_medalhas = n()) %>%
  arrange(desc(total_medalhas))

top_5 <- head(Medalhistas_mulheres, 5)

top_5_grafico <- ggplot(top_5, aes(x = reorder(Team, total_medalhas), y = total_medalhas)) + 
  geom_bar(stat = "identity", fill = "#a11d21") +  # Cor das barras
   coord_flip() +
  labs(title =,
       x = "País",
       y = "Número de Medalhas") +
  theme_estat() +
  geom_text(aes(label = total_medalhas), 
            position = position_stack(vjust = 1), 
            hjust = -0.1,  # Ajuste horizontal para posicionar o texto
            color = "black")  # Cor do texto
print(top_5_grafico)

## Analise 2 - Análise 2 - Valor IMC por esporte

#Calculo IMC

Olimpiadas$Weight_kg <- Olimpiadas$Weight * 0.453592  # Converter peso para kg
Olimpiadas$Height_m <- Olimpiadas$Height / 100     # Converter altura para m
Olimpiadas$IMC <- Olimpiadas$Weight_kg / (Olimpiadas$Height_m^2)# Calcular IMC

#iIMC por esporte

esportes_desejados <- c("Gymnastics", "Football", "Judo", "Athletics", "Badminton")

IMC_filtrado <- Olimpiadas %>%
  filter(Sport %in% esportes_desejados) %>%  # Filtra os esportes
  group_by(Sport) %>%                        # Agrupa por esporte
  summarise(
    media_imc = mean(IMC, na.rm = TRUE),      # Média do IMC
    sd_imc = sd(IMC, na.rm = TRUE),          # Desvio padrão do IMC
    n = n()                                   # Número de atletas por esporte
  )

# Visualização

IMC_por_esporte_grafico <- ggplot(Olimpiadas %>% filter(Sport %in% esportes_desejados), aes(x = Sport, y = IMC)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 2, fill = "white"
  ) +
  labs(x = "Esportes",
       y = "IMC") +
  scale_x_discrete(labels = c("Badminton" = "Badminton", 
                              "Athletics" = "Atletismo", 
                              "Gymnastics" = "Ginástica", 
                              "Judo" = "Judô")) +
  theme_estat()
##resumo de valores para melhor entendimento

tabela_IMC <- Olimpiadas %>%
  filter(Sport %in% esportes_desejados) %>%
  group_by(Sport) %>%
  summarise(
    Media_IMC = mean(IMC, na.rm = TRUE),
    Variancia_IMC = var(IMC, na.rm = TRUE)
  ) %>%
  mutate(
    Sport = recode(Sport,
                   "Badminton" = "Badminton", 
                   "Athletics" = "Atletismo", 
                   "Gymnastics" = "Ginástica", 
                   "Judo" = "Judô"),
    Media_IMC = round(Media_IMC, 2),
    Variancia_IMC = round(Variancia_IMC, 2)
  ) %>%
  rename(
    Esporte = Sport,
    `Média IMC` = Media_IMC,
    `Variância IMC` = Variancia_IMC
  )


#Analise 3


medalhistas_an3 <- Olimpiadas[!is.na(Olimpiadas$Medal), ]

# Contar as ocorrências de cada nome na coluna names
name_counts <- table(medalhistas_an3)

# Encontrar o nome que mais se repete
most_frequent_name <- names(name_counts)[which.max(name_counts)]

# Exibir o resultado
most_frequent_name

