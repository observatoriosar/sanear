#======================================#
# OBSERVATORIO DE SANEAMENTO E         #
# MEIO AMBIENTE DO RECIFE (OSAR)       #
#======================================#
# Saneamento em Belo Jardim (PE)       #
#--------------------------------------#

# instalar pacotes

# carregar pacotes
library(readxl); library(ggmap); library(ggplot2)

# tema para grafico
theme_arretado<- function (base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour= "black",size=11,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=11,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(9, "mm"),
          legend.text = element_text(size = 9, hjust = 3, vjust = 3),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"))
}

# importar dados
munic_PE_1_ <-  read_excel("Dados/belo jardim total.xlsx")


#====================#
#===== graficos =====#

# produxir grafico em ggplot
belo1 <- ggplot(data = munic_PE_1_)+
  geom_line(aes(x = `Ano de Referência`, y = `AG021 - Quantidade de ligações totais de água (Ligações)`,
                group =1), size = 1, colour = "darkblue")+
  geom_text(aes( x = `Ano de Referência`, 
                 y = `AG001 - População total atendida com abastecimento de água (Habitantes)`,angle = 20 , vjust = -0.9, hjust = 0.3, fontface = "bold"),
            label = munic_PE_1_$`AG001 - População total atendida com abastecimento de água (Habitantes)`, 
            size = 3)+
  labs(x = "", title = "Abastecimento de Agua e Coleta de Esgoto em Belo Jardim",y = "População")+
  theme_arretado() 

# visualizar grafico
print(belo1)

ggsave("belo1.png",  width = 9, height = 6)

#--- grafico de linha consumo ---#

belo2 <- ggplot(data = munic_PE_1_, 
                aes(x = `Ano de Referência`, 
                    y = munic_PE_1_$`IN022 - Consumo médio percapita de água (l/hab./dia)`,
                    group =1))+
  geom_line(size = 1, colour = "darkblue")+
  labs(x = "", title = "Consumo Médio de Água (l/hab./dia)",y = "Consumo")+
  geom_text(aes( angle = 20 , vjust = -0.9, hjust = 0.3, fontface = "bold"),
            label = munic_PE_1_$`IN022 - Consumo médio percapita de água (l/hab./dia)`, 
            size = 3)+
  theme_arretado() 
belo2

ggsave("belo2.png",  width = 9, height = 6)


