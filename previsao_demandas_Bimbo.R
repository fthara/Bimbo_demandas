getwd()
setwd("/Users/fernando/Google Drive/DSA/BigDataRAzure/Projetos/Bimbo_demandas")

library(data.table)
library(dplyr)
library(tidyr)

df <- fread("train.csv")
head(df)
str(df)

sum(is.na(df) == TRUE)
# Análise Exploratória

# Como o data set é muito grande vamos trablhar na análise exploratória com uma
# amostragem. Assim o trabalho fica mais rápido e não exige muita memória do R.
require(caret)
set.seed(123)
index <- createDataPartition(df$Demanda_uni_equil, p = 0.1,list = FALSE,  times = 1)
df <- df[index, ]

# Demanda_uni_equil
summary(df$Demanda_uni_equil)

# Podemos perceber que o valor máximo dessa variável está muito distante da mediana,
# esse valor pode ser considerado um outlier. Vamos analizá-lo pelo boxplot.
library(ggplot2)
ggplot(df, aes(y=Demanda_uni_equil)) +
  geom_boxplot()

# Vamos ver quantos valores acima de 100 existem
nrow(df[df$Demanda_uni_equil > 100])

# é uma taxa considerável, por isso decidi não excluir do meu data set. Agora veremos
# como os dados estão distribuídos separadamente em um histograma (valores acima de
# 100 e abaixo de 100)
df %>%
  select(Demanda_uni_equil) %>%
  filter(Demanda_uni_equil < 100) %>%
  ggplot(aes(Demanda_uni_equil)) +
  geom_histogram(bins = 30) +
  ggtitle("Histograma Demandas Abaixo de 100") +
  xlab("Demanda") +
  ylab("Quantidade") +
  theme(plot.title = element_text(hjust = 0.5))

# Ainda assim podemos perceber que a maior parte dos valores se concentram entre 0 e
# 45 aproximadamente. Vamos filtrar mais ainda nosso histograma
df %>%
  select(Demanda_uni_equil) %>%
  filter(Demanda_uni_equil < 45) %>%
  ggplot(aes(Demanda_uni_equil)) +
  geom_histogram(bins = 30) +
  ggtitle("Histograma Demandas Abaixo de 45") +
  xlab("Demanda") +
  ylab("Quantidade") +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  select(Demanda_uni_equil) %>%
  filter(Demanda_uni_equil > 45) %>%
  ggplot(aes(Demanda_uni_equil)) +
  geom_histogram(bins = 1000) +
  ggtitle("Histograma Demandas Acima de 45") +
  xlab("Demanda") +
  ylab("Quantidade") +
  theme(plot.title = element_text(hjust = 0.5))

nrow(df[df$Demanda_uni_equil > 1000]) 
# Analizando melhor o histograma percebemos que a maior parte da demanda está entre
# 0 e 10, também temos uma quantidade interessante entre 100 e 1000, mas a partir daí
# a quantidade diminui consideravelmente. De qualquer forma acredito que não podemos
# desconsiderar esses valores ainda, pois esses altos valores podem estar atrelados a 
# grandes fornecedores da fábrica.

# Vamos analizar os produtos e relacioná-los com a demanda.
df_prod <- fread('producto_tabla.csv')
head(df_prod)
nrow(df_prod)
df_prod[101:150, 'NombreProducto']
df_prod[1, 'NombreProducto'] = "No Identificado 0g NO 0"

# Os produtos estão nomeados em strings com varios significados, como o nome,
# peso, número de pedaços, marca, etc.. Vamos dividir essas strings para tentar
# tirar mais informaçoes do data set.
library(tidyr)
library(stringr)
# Extraindo somente o nome do produto.
df_prod <- df_prod %>%
  mutate(Nome_Prod = str_extract(NombreProducto, '^(\\D*)'))
df_prod$Nome_Prod = trimws(df_prod$Nome_Prod)
sum(is.na(df_prod$Nome_Prod) == TRUE)

# Extraindo o peso do produto
df_prod <- df_prod %>%
  mutate(Peso = str_extract(NombreProducto, '(\\d+)(kg|g)'))
df_prod$Peso = trimws(df_prod$Peso)
sum(is.na(df_prod$Peso) == TRUE)
# Valores NA em 0.
df_prod[is.na(df_prod$Peso) == TRUE, 'Peso'] = '0kg'
# Separando o peso da unidade
df_prod <- df_prod %>%
  separate(Peso, into=c('Peso', 'Unidade_Peso'), sep="(?<=[A-Za-z])(?=[0-9])|(?<=[0-9])(?=[A-Za-z])")
df_prod$Peso <- as.numeric(df_prod$Peso)

# Transformando gramas em kilos
n <- c()
for(i in 1:nrow(df_prod)){
  if(df_prod$Unidade_Peso[i] == 'g')
    n[i] <- df_prod$Peso[i]/1000
  else
    n[i] <- df_prod$Peso[i]
}
df_prod$Peso <- n
# Excluindo o campo de unidade do peso
df_prod$Unidade_Peso <- NULL

# Extraindo o número de pedaços do produto
df_prod <- df_prod %>%
  mutate(Pedacos = str_extract(NombreProducto, '(\\d+)(p)'))
# Remover espaços branco
df_prod$Pedacos = trimws(df_prod$Pedacos)
# Valores NA igual a 0
sum(is.na(df_prod$Pedacos) == TRUE)
df_prod[is.na(df_prod$Pedacos) == TRUE, 'Pedacos'] = 0

# Extraindo a marca do produto.
df_prod <- df_prod %>%
  mutate(Marca = str_extract(NombreProducto, "\\s[A-Z]*\\s"))
df_prod$Marca = trimws(df_prod$Marca)
sum(is.na(df_prod$Marca) == TRUE)
df_prod[is.na(df_prod$Marca) == TRUE, 'Marca'] = 'NA'

head(df_prod)

# Juntando os dois data sets.
df <- df %>%
  inner_join(df_prod)

head(df)

# Analizando a marca pelo total da demanda
df %>%
  group_by(Marca) %>%
  summarise(sum_dem = sum(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(Marca, -sum_dem), y=sum_dem)) +
    geom_col() +
    ggtitle("Marca pelo Total da Demanda") +
    xlab("Marca") +
    ylab("Total da Demanda") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 60))

# Analizando a marca pela mediana da demanda
df %>%
  group_by(Marca) %>%
  summarise(median_dem = median(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(Marca, -median_dem), y=median_dem)) +
  geom_col() +
  ggtitle("Marca pela Mediana da Demanda") +
  xlab("Marca") +
  ylab("Mediana da Demanda") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60))

median(df[df$Marca == 'CC', 'Demanda_uni_equil'])
# Vamos renomear as marcas com mediana menor que 7 como outros.
median_Marca <- df %>%
  group_by(Marca) %>%
  summarise(median_dem = median(Demanda_uni_equil)) %>%
  filter(median_dem > 7)
marcas <- sapply(df$Marca, 
            function(marca, col){
              if(marca %in% col)
                m = marca
              else
                m = 'outros'
              return(m)
            }, median_Marca$Marca)

df$Marca <- marcas

# Analizando a marca pela mediana da demanda
df %>%
  group_by(Marca) %>%
  summarise(median_dem = median(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(Marca, -median_dem), y=median_dem)) +
  geom_col() +
  ggtitle("Marca pela Mediana da Demanda") +
  xlab("Marca") +
  ylab("Mediana da Demanda") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60))

# Analizando o Peso
summary(df$Peso)
hist(df$Peso)
cor(df[, c('Peso', 'Demanda_uni_equil')])
# A correlação entre o Peso e a Demanda é baixa. Mas não pode ser descartado, pois
# junto com os produtos ele pode dar informações importantes

head(df)
head(df_prod)

#Extraindo o primeiro nome de Nome_Prod
 df_prod <- df_prod %>%
  separate(Nome_Prod, into=c("P_Nome_Prod", "S_Nome_Prod"), "\\s", extra = "merge")

sum(is.na(df_prod$P_Nome_Prod))
sum(is.na(df_prod$S_Nome_Prod))

length(df_prod$P_Nome_Prod)
length(unique(df_prod$P_Nome_Prod))

df_prod[df_prod$P_Nome_Prod == '', ]

# Existem alguns nomes que não foram capturados por começarem com números. Esses valores
# serão colocados manualmente.
df_prod[c(58, 2103, 2481), 'P_Nome_Prod'] = '7 Granos'
df_prod[c(366, 2081), 'P_Nome_Prod'] = 'Whole'
df_prod[c(1820, 1861), 'P_Nome_Prod'] = 'Tarima'
df_prod[c(2210, 2480), 'P_Nome_Prod'] = '12 Granos'

# Pelo primeiro nome conseguimos agrupar reduzindo para 300 tipos, vamos unir com o
# df e ver se tem um relação com a demanda
head(df_prod)
prod <- df_prod[, c(1,3,4)]
head(prod)
df <- df %>%
  inner_join(prod)
rm(prod)

head(df)

# Analizando o produto pelo total da demanda
df %>%
  group_by(P_Nome_Prod) %>%
  summarise(sum_dem = sum(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(P_Nome_Prod, -sum_dem), y=sum_dem)) +
  geom_col() +
  ggtitle("Tipo de Produto pelo Total da Demanda") +
  xlab("Tipo de Produto") +
  ylab("Total da Demanda") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60))

# Analizando o produto pela mediana da demanda
df %>%
  group_by(P_Nome_Prod) %>%
  summarise(median_dem = median(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(P_Nome_Prod, -median_dem), y=median_dem)) +
  geom_col() +
  ggtitle("Tipo de Produt pela Mediana da Demanda") +
  xlab("Tipo de Produt") +
  ylab("Mediana da Demanda") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60))

# Existem muitos produtos e alguns deles têm em poucas quantidades no data set,
# portanto aqueles que tem menos de 100 foram renomeados como outros.
produtos <- df %>%
  group_by(P_Nome_Prod) %>%
  summarise(n = n())

produtos[produtos$n < 100, 'P_Nome_Prod'] = 'Outros'

select_prod <- function(df, produtos){
  prod<- sapply(df$P_Nome_Prod, 
                      function(prod, produtos){
                        if(prod %in% produtos$P_Nome_Prod)
                          p = prod
                        else
                          p = 'Outros'
                        return(p)
                      },produtos)
}
df$P_Nome_Prod_Outros <- select_prod(df, produtos)

# Analizando o produto pela mediana da demanda
df %>%
  group_by(P_Nome_Prod) %>%
  summarise(median_dem = median(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(P_Nome_Prod, -median_dem), y=median_dem)) +
  geom_col() +
  ggtitle("Tipo de Produt pela Mediana da Demanda") +
  xlab("Tipo de Produt") +
  ylab("Mediana da Demanda") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60))


# Podemos perceber que alguns produtos têm uma mediana de demanda bem alta, enquanto outros
# não tem quase nenhuma demanda.

# Vamos analizar agora os clientes.
df_cli <- fread('cliente_tabla.csv')
nrow(df_cli)
nrow(df_cli %>%
       group_by(NombreCliente) %>%
       summarise(n = n()))

clientes <- df_cli[order(NombreCliente)]
clientes[1:50, ]
View(df_cli[1:200, ])

nrow(df_cli[df_cli$NombreCliente == 'SIN NOMBRE', ]) 
sum(is.na(df_cli$NombreCliente == TRUE))

df_cli$NombreCliente = toupper(df_cli$NombreCliente)
# Unir ao data frame e agrupar por valores parecidos

# Existem algumas palavras que são frequentes nos nomes dos clientes, como SUPER, que
# acredito ser supermercados, farmacia, hotel, OXXO... Vamos ver quais são as palavras 
# mais frequentes neste data frame.

freq_pal_cli <- df_cli$NombreCliente %>%
  strsplit(split = " ") %>% # or strsplit(split = "\\W") 
  unlist() %>%
  table() %>%
  sort(decreasing = TRUE)

View(freq_pal_cli)

# Como existem muitas palavras, vamos agrupando aos poucos elas até reduzirmos o 
# número de clientes.

head(df_cli)

df_cli[df_cli$NombreCliente == 'SIN NOMBRE', 'NombreCliente'] = 'NO IDENTIFICADO'

df_cli[str_detect(df_cli$NombreCliente, 'OXXO'),
       'NombreCliente'] = 'OXXO'

df_cli[str_detect(df_cli$NombreCliente, 'ABARROTES'),
       'NombreCliente'] = 'ABARROTES'
df_cli[str_detect(df_cli$NombreCliente, 'MISCELANEA'),
       'NombreCliente'] = 'ABARROTES'
df_cli[str_detect(df_cli$NombreCliente, 'TIEND'),
       'NombreCliente'] = 'ABARROTES'
df_cli[str_detect(df_cli$NombreCliente, 'ABTS'),
       'NombreCliente'] = 'ABARROTES'
df_cli[str_detect(df_cli$NombreCliente, 'MISCELANIA'),
       'NombreCliente'] = 'ABARROTES'

df_cli[str_detect(df_cli$NombreCliente, 'FARMACIA'),
       'NombreCliente'] = 'FARMACIA'

df_cli[str_detect(df_cli$NombreCliente, 'HOSPITAL'),
       'NombreCliente'] = 'HOSPITAL'

df_cli[str_detect(df_cli$NombreCliente, 'HOTEL'),
       'NombreCliente'] = 'HOTEL'

df_cli[str_detect(df_cli$NombreCliente, 'PAPELERIA'),
       'NombreCliente'] = 'PAPELERIA'

df_cli[str_detect(df_cli$NombreCliente, 'ESCUELA'),
       'NombreCliente'] = 'ESCUELA'

df_cli[str_detect(df_cli$NombreCliente, 'PUESTO'),
       'NombreCliente'] = 'PUESTO'


df_cli[str_detect(df_cli$NombreCliente, 'COOPERATIVA'),
       'NombreCliente'] = 'MERCADO'
df_cli[str_detect(df_cli$NombreCliente, 'MERCADO'),
       'NombreCliente'] = 'MERCADO'
df_cli[str_detect(df_cli$NombreCliente, 'MINI'),
       'NombreCliente'] = 'MERCADO'

df_cli[str_detect(df_cli$NombreCliente, 'SUPER'),
       'NombreCliente'] = 'SUPERMERCADO'
df_cli[str_detect(df_cli$NombreCliente, 'SAMS'),
       'NombreCliente'] = 'SUPERMERCADO'
df_cli[str_detect(df_cli$NombreCliente, 'WALLMART'),
       'NombreCliente'] = 'SUPERMERCADO'


df_cli[str_detect(df_cli$NombreCliente, 'HAMB'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'BURG'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'TACO'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'RESTAU'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'COMED'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'PIZZ'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'LANCH'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'CAFETERIA'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'COCA'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'ALIMENTOS'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'MARISCOS'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'REFRESCOS'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'CREMERIA'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'TORTILLERIA'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'FRUTAS'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'REFRESQUERIA'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'LIMON'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'DULCE'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'TORTA'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'VINO'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'EXPRESS'),
       'NombreCliente'] = 'COMEDORIA'

cliente <- c('NO IDENTIFICADO', 'OXXO', 'ABARROTES', 'FARMACIA', 'HOSPITAL', 
             'HOTEL', 'PAPELERIA', 'ESCUELA', 'MERCADO', 'SUPERMERCADO',
             'PUESTO', 'COMEDORIA')

# Função para renomear os clientes
cli <- sapply(df_cli$NombreCliente,
              function(cli, cliente){
                if(cli %in% cliente)
                  c <- cli
                else
                  c <- 'OUTROS'
                return(c)
              }, cliente)

df_cli$NombreCliente <- cli

# Agrupando por nomes
df_cli %>%
  group_by(NombreCliente) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# Portanto agora temos 14 tipos de clientes. Vamos juntar ao data set original e ver se
# eles têm um relação com a variável target
df <- df %>%
  inner_join(df_cli)
head(df)

# Analizandoo cliente pelo total da demanda
df %>%
  group_by(NombreCliente) %>%
  summarise(sum_dem = sum(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(NombreCliente, -sum_dem), y=sum_dem)) +
  geom_col() +
  ggtitle("Tipo de Cliente pelo Total da Demanda") +
  xlab("Tipo de Cliente") +
  ylab("Total da Demanda") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60))

# Analizandoo cliente pela mediana da demanda
df %>%
  group_by(NombreCliente) %>%
  summarise(median_dem = median(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(NombreCliente, -median_dem), y=median_dem)) +
  geom_col() +
  ggtitle("Tipo de Cliente pela Mediana da Demanda") +
  xlab("Tipo de Cliente") +
  ylab("Mediana da Demanda") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60))

# Conseguimos um ótimo resultado visualizando graficamente, talvez seja preciso
# refinar um pouco mais, mas mesmo assim ja temos uma ideia boa de quanto se entrega
# por tipo de cliente.

# Analizando a variável agencia_ID
df_ag <- fread('town_state.csv')
head(df_ag)

# Agrupando por estado
states <- df_ag %>%
  group_by(State) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

View(states)  

# Agrupando por cidades
towns <- df_ag %>%
  group_by(Town) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

View(towns)  

# Olhando para este conjunto de dados percebi que town não é a cidade onde a agencia
# se encontra, é como se fosse o nome da agencia, mas quando existe mais de uma em uma
# cidade por exemplo eles são diferenciados por norte, sul, etc..
# Vamos limpar o nome das cidades, tirando algumas palavras como AG., numeros
# e BIMBO
  
df_ag$Town <- toupper(df_ag$Town)

df_ag <- df_ag %>%
  mutate(Town_filter = str_remove_all(Town, '\\d'))

df_ag <- df_ag %>%
  mutate(Town_filter = str_remove_all(Town_filter, 'AG. '))

df_ag <- df_ag %>%
  mutate(Town_filter = str_remove_all(Town_filter, 'BIMBO'))
df_ag$Town_filter <- trimws(df_ag$Town_filter)

df_ag[df_ag$Town_filter == 'A PRIETA', 'Town_filter'] = 'AGUA PRIETA'

# Agora vamos arrumar algumas localizações com o noem correto das cidades e juntar
# algumas delas.
towns_f <- df_ag %>%
  group_by(Town_filter) %>%
  summarise(n = n()) %>%
  arrange(Town_filter)

View(towns_f)

df_ag[str_detect(df_ag$Town_filter, 'AG.HUEJUTLA'),
      'Town_filter'] = 'HUEJUTLA'

df_ag[str_detect(df_ag$Town_filter, 'AG.IZTAPALAPA'),
      'Town_filter'] = 'IZTAPALAPA'

df_ag[str_detect(df_ag$Town_filter, 'AG.MIXQUIAHUALA'),
      'Town_filter'] = 'MIXQUIAHUALA'

df_ag[str_detect(df_ag$Town_filter, 'AG.PACHUCA'),
      'Town_filter'] = 'PACHUCA'

df_ag[str_detect(df_ag$Town_filter, 'AG.TULANCINGO'),
      'Town_filter'] = 'TULANCINGO'

df_ag[str_detect(df_ag$Town_filter, 'ACAPULCO'),
       'Town_filter'] = 'ACAPULCO'

df_ag[str_detect(df_ag$Town_filter, 'ALTAMIRANO'),
      'Town_filter'] = 'CIUDAD ALTAMIRANO'

df_ag[str_detect(df_ag$Town_filter, 'AGUASCALIENTES'),
      'Town_filter'] = 'AGUASCALIENTES'

df_ag[str_detect(df_ag$Town_filter, 'APIZACO'),
      'Town_filter'] = 'APIZACO'

df_ag[str_detect(df_ag$Town_filter, 'ARRIAGA'),
      'Town_filter'] = 'ARRIAGA'

df_ag[str_detect(df_ag$Town_filter, 'AUTLAN'),
      'Town_filter'] = 'AUTLAN'

df_ag[str_detect(df_ag$Town_filter, 'AZCAPOTZALCO'),
      'Town_filter'] = 'AZCAPOTZALCO'

df_ag[str_detect(df_ag$Town_filter, 'SAN FRANCISCO DEL RINCON'),
      'Town_filter'] = 'SAN FRANCISCO DEL RINCON'

df_ag[str_detect(df_ag$Town_filter, 'GUASAVE'),
      'Town_filter'] = 'GUASAVE'

df_ag[str_detect(df_ag$Town_filter, 'CANCUN'),
      'Town_filter'] = 'CANCUN'

df_ag[str_detect(df_ag$Town_filter, 'CELAYA'),
      'Town_filter'] = 'CELAYA'

df_ag[str_detect(df_ag$Town_filter, 'CHALCO'),
      'Town_filter'] = 'CHALCO'

df_ag[str_detect(df_ag$Town_filter, 'CHIHUAHUA'),
      'Town_filter'] = 'CHIHUAHUA'

df_ag[str_detect(df_ag$Town_filter, 'VERACRUZ'),
      'Town_filter'] = 'VERACRUZ'

df_ag[str_detect(df_ag$Town_filter, 'CRUCE DE ANDEN'),
      'Town_filter'] = 'CRUCE DE ANDEN'

df_ag[str_detect(df_ag$Town_filter, 'CRUCE DE ANDÉN'),
      'Town_filter'] = 'CRUCE DE ANDEN'

df_ag[str_detect(df_ag$Town_filter, 'CUERNAVACA'),
      'Town_filter'] = 'CUERNAVACA'

df_ag[str_detect(df_ag$Town_filter, 'CULIACAN'),
      'Town_filter'] = 'CULIACAN'

df_ag[str_detect(df_ag$Town_filter, 'IRAPUATO'),
      'Town_filter'] = 'IRAPUATO'

df_ag[str_detect(df_ag$Town_filter, 'JOJUTLA'),
      'Town_filter'] = 'JOJUTLA'

df_ag[str_detect(df_ag$Town_filter, 'LEON'),
      'Town_filter'] = 'LEON'

df_ag[str_detect(df_ag$Town_filter, 'MAZATLAN'),
      'Town_filter'] = 'MAZATLAN'

df_ag[str_detect(df_ag$Town_filter, 'MERIDA'),
      'Town_filter'] = 'MERIDA'

df_ag[str_detect(df_ag$Town_filter, 'MEXICALI'),
      'Town_filter'] = 'MEXICALI'

df_ag[str_detect(df_ag$Town_filter, 'MORELIA'),
      'Town_filter'] = 'MORELIA'

df_ag[str_detect(df_ag$Town_filter, 'NAUCALPAN'),
      'Town_filter'] = 'NAUCALPAN'

df_ag[str_detect(df_ag$Town_filter, 'NOGALES'),
      'Town_filter'] = 'NOGALES'

df_ag[str_detect(df_ag$Town_filter, 'OAXACA'),
      'Town_filter'] = 'OAXACA'

df_ag[str_detect(df_ag$Town_filter, 'PACHUCA'),
      'Town_filter'] = 'PACHUCA'

df_ag[str_detect(df_ag$Town_filter, 'PUEBLA'),
      'Town_filter'] = 'PUEBLA'

df_ag[str_detect(df_ag$Town_filter, 'QUERETARO'),
      'Town_filter'] = 'QUERETARO'

df_ag[str_detect(df_ag$Town_filter, 'REYNOSA'),
      'Town_filter'] = 'REYNOSA'

df_ag[str_detect(df_ag$Town_filter, 'SALTILLO'),
      'Town_filter'] = 'SALTILLO'

df_ag[str_detect(df_ag$Town_filter, 'SAN LUIS'),
      'Town_filter'] = 'SAN LUIS POTOSI'

df_ag[str_detect(df_ag$Town_filter, 'SANTA CLARA'),
      'Town_filter'] = 'SANTA CLARA'

df_ag[str_detect(df_ag$Town_filter, 'TAMPICO'),
      'Town_filter'] = 'TAMPICO'

df_ag[str_detect(df_ag$Town_filter, 'TIJUANA'),
      'Town_filter'] = 'TIJUANA'

df_ag[str_detect(df_ag$Town_filter, 'TOLUCA'),
      'Town_filter'] = 'TOLUCA'

df_ag[str_detect(df_ag$Town_filter, 'ZACATECAS'),
      'Town_filter'] = 'ZACATECAS'

df_ag[str_detect(df_ag$Town_filter, 'LA VILLA'),
      'Town_filter'] = 'LA VILLA (CD. MEXICO)'

towns_f <- df_ag %>%
  group_by(Town_filter) %>%
  summarise(n = n()) %>%
  arrange(Town_filter)

head(towns_f)

# Agora que temos o nome das cidades vamos introduzir um data set criado por mim
# com os habitantes de cada cidade do mexico.
df_pop <- fread('Habitantes_Cidades_Mexico.csv')

towns_f$Poblacion <- df_pop$Poblacion

head(towns_f)
rm(df_pop)

# Separando as cidades por faixa de habitantes.
towns_f$Faixa_habitantes <- findInterval(towns_f$Poblacion,
                                         c(0, 1, 5000, 20000, 40000, 80000,
                                           150000, 300000, 600000, 1000000,
                                           10000000000))
towns_f<-towns_f %>% 
  rename(Faixa_habitantes=Faixa_habitantes) %>% 
  mutate(Faixa_habitantes=as.factor(Faixa_habitantes))
levels(towns_f$Faixa_habitantes)<-c("Outros", "até 5 mil", "5 a 20 mil", 
                                    "20 a 40 mil", "40 a 80 mil", 
                                    "80 a 150 mil", "150 a 300 mil",
                                    "300 a 600 mil", "600 a 1 milhão",
                                    "mais de 1 milhão")

towns_f <- towns_f[, c(1,4)]
df_ag <- df_ag %>%
  inner_join(towns_f)


df_ag <- df_ag[, c(1,5)]

# Unindo com o data set original
df <- df %>%
  inner_join(df_ag)

head(df)

# Analizando a faixa de habitantes pelo total da demanda
df %>%
  group_by(Faixa_habitantes) %>%
  summarise(sum_dem = sum(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(Faixa_habitantes, -sum_dem), y=sum_dem)) +
  geom_col() +
  ggtitle("Habitantes no Local da Agencia pelo Total da Demanda") +
  xlab("Habitantes no Local da Agencia") +
  ylab("Total da Demanda") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60))

# Analizando a faixa de habitantes pela mediana da demanda
df %>%
  group_by(Faixa_habitantes) %>%
  summarise(median_dem = median(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(Faixa_habitantes, -median_dem), y=median_dem)) +
  geom_col() +
  ggtitle("Habitantes no Local da Agencia pela Mediana da Demanda") +
  xlab("Habitantes no Local da Agencia") +
  ylab("Mediana da Demanda") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60))

# Analizando a faixa de habitantes pela media da demanda    
df %>%
  group_by(Faixa_habitantes) %>%
  summarise(mean_dem = mean(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(Faixa_habitantes, -mean_dem), y=mean_dem)) +
  geom_col() +
  ggtitle("Habitantes no Local da Agencia pela Média da Demanda") +
  xlab("Habitantes no Local da Agencia") +
  ylab("Média da Demanda") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60))

# Agora que temos uma boa modificação do nosso data set vamos treinar e ver os resultados.

# Como o data set ainda é muito grande, vamos diminuí-lo novamente para podermos realizar
# a predição sem que demore horas rodando.
# Diminuindo o data set para realizar a predição.
indexTraining <- createDataPartition(df$Demanda_uni_equil, p = 0.1,list = FALSE,  times = 1)
dft <- df[indexTraining, ]
nrow(dft)
head(dft)

# Split do Data Set
trainIndex <- createDataPartition(dft$Demanda_uni_equil, p = 0.7,list = FALSE,  times = 1)
train <- dft[trainIndex,]
test <- dft[-trainIndex,]


# Treinamento do Modelo
# Criando os modelos preditivos
formula_v1 <- as.formula('Demanda_uni_equil ~ Peso + Marca + P_Nome_Prod_Outros + 
                         NombreCliente + Faixa_habitantes')
# Treinando o modelo com o algoritmo de regressão logística
model_glm_v1 <- glm(formula = formula_v1, data = train, family = "gaussian")
# Verificando alguns resultados do modelo treinado
summary(model_glm_v1)
# Realizando a predição com o modelo treinado
pred_glm_v1 <- predict(model_glm_v1, test, type="response")
# Arredondando
pred_glm_v1 <- round(pred_glm_v1)
#install.packages("hydroGOF")
#Load Library
library(hydroGOF)
#Cálculo do RMSE
rmse(test$Demanda_uni_equil,pred_glm_v1)


# Treinando o modelo com o algoritmo de regressão linear
model_lm_v1 <- lm(formula = formula_v1, data = train)
# Verificando alguns resultados do modelo treinado
summary(model_lm_v1)
# Realizando a predição com o modelo treinado
pred_lm_v1 <- predict(model_lm_v1, test, type="response")
# Arredondand
pred_lm_v1 <- round(pred_lm_v1)
#Cálculo do RMSE
rmse(test$Demanda_uni_equil,pred_lm_v1)

library(rpart)
# treinando o modelo
modelo_tree_v1 = rpart(formula = formula_v1, data = train, 
                       control = rpart.control(cp = .0005)) 
# Previsões nos dados de teste
pred_tree_v1 = predict(modelo_tree_v1, test, type='vector')
#Cálculo do RMSE
rmse(test$Demanda_uni_equil,pred_tree_v1)

# O resultado não foi muito bom. Acredito que o alto valor da demanda em alguns produtos
# e clientes estejam atrapalhando nessa previsão. Vamos tentar analizá-los de uma melhor 
# forma.

dem_maior_1000 <- df[df$Demanda_uni_equil > 1000, c('P_Nome_Prod', 'NombreCliente', 
                                       'Demanda_uni_equil')]
nrow(dem_maior_1000)
View(dem_maior_1000)

# Vendo os produtos e clientes com demandas maiores que 1000 percebemos um certo padrão.
# Nos produtos conseguimos ter um grupo até que bem definido deles, vemos muitas coca-colas
# tortilhas, salmas, etc, mas nos clientes muitos deles estão em outros, então provavelmente
# teremos que fazer uma análise melhor desses clientes para tentar categorizá-los de uma 
# melhor forma.

df_cliente <- fread('cliente_tabla.csv')

names(df_cliente)[names(df_cliente) == 'NombreCliente'] = 'Nome_Completo'
head(df_cliente)

dem_maior_1000 <- df[df$Demanda_uni_equil > 1000, c('P_Nome_Prod', 'NombreCliente', 
                                                    'Demanda_uni_equil', 'Cliente_ID')]

dem_maior_1000 <- dem_maior_1000 %>%
  inner_join(df_cliente)

head(dem_maior_1000)

dem_maior_1000$Cliente_ID = NULL

# Analizando esses dados percebi que alguns clientes realmente ficaram mal distribuídos,
# como o Wall Mart, e também percebi alguns clientes que aparecem com uma certa frequência
# dentre os que estão com grandes demandas. Vamos fazer a divisão novamente para e ver como
# fica o novo algoritmo.

df_cli <- fread('cliente_tabla.csv')

nrow(df_cli)

nrow(df_cli %>%
       group_by(NombreCliente) %>%
       summarise(n = n()))

x <- df_cli[order(NombreCliente)]

df_cli$NombreCliente = toupper(df_cli$NombreCliente)

head(df_cli)

df_cli[df_cli$NombreCliente == 'SIN NOMBRE', 'NombreCliente'] = 'NO IDENTIFICADO'

df_cli[str_detect(df_cli$NombreCliente, 'OXXO'),
       'NombreCliente'] = 'OXXO'

df_cli[str_detect(df_cli$NombreCliente, 'ABARROTES'),
       'NombreCliente'] = 'ABARROTES'
df_cli[str_detect(df_cli$NombreCliente, 'MISCELANEA'),
       'NombreCliente'] = 'ABARROTES'
df_cli[str_detect(df_cli$NombreCliente, 'TIEND'),
       'NombreCliente'] = 'ABARROTES'
df_cli[str_detect(df_cli$NombreCliente, 'ABTS'),
       'NombreCliente'] = 'ABARROTES'
df_cli[str_detect(df_cli$NombreCliente, 'MISCELANIA'),
       'NombreCliente'] = 'ABARROTES'

df_cli[str_detect(df_cli$NombreCliente, 'FARMACIA'),
       'NombreCliente'] = 'FARMACIA'

df_cli[str_detect(df_cli$NombreCliente, 'HOSPITAL'),
       'NombreCliente'] = 'HOSPITAL'

df_cli[str_detect(df_cli$NombreCliente, 'HOTEL'),
       'NombreCliente'] = 'HOTEL'

df_cli[str_detect(df_cli$NombreCliente, 'PAPELERIA'),
       'NombreCliente'] = 'PAPELERIA'

df_cli[str_detect(df_cli$NombreCliente, 'ESCUELA'),
       'NombreCliente'] = 'ESCUELA'

df_cli[str_detect(df_cli$NombreCliente, 'PUESTO'),
       'NombreCliente'] = 'PUESTO'


df_cli[str_detect(df_cli$NombreCliente, 'COOPERATIVA'),
       'NombreCliente'] = 'MERCADO'
df_cli[str_detect(df_cli$NombreCliente, 'MERCADO'),
       'NombreCliente'] = 'MERCADO'
df_cli[str_detect(df_cli$NombreCliente, 'MINI'),
       'NombreCliente'] = 'MERCADO'

df_cli[str_detect(df_cli$NombreCliente, 'SUPER'),
       'NombreCliente'] = 'SUPERMERCADO'
df_cli[str_detect(df_cli$NombreCliente, 'SAMS'),
       'NombreCliente'] = 'SUPERMERCADO'
df_cli[str_detect(df_cli$NombreCliente, 'WALLMART'),
       'NombreCliente'] = 'SUPERMERCADO'
df_cli[str_detect(df_cli$NombreCliente, 'WAL MART'),
       'NombreCliente'] = 'SUPERMERCADO'

df_cli[str_detect(df_cli$NombreCliente, 'HAMB'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'BURG'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'TACO'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'RESTAU'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'COMED'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'PIZZ'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'LANCH'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'CAFETERIA'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'COCA'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'ALIMENTOS'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'MARISCOS'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'REFRESCOS'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'CREMERIA'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'TORTILLERIA'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'FRUTAS'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'REFRESQUERIA'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'LIMON'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'DULCE'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'TORTA'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'VINO'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'EXPRESS'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'WINGS'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'BANQUETES'),
       'NombreCliente'] = 'COMEDORIA'
df_cli[str_detect(df_cli$NombreCliente, 'COCINAS'),
       'NombreCliente'] = 'COMEDORIA'

df_cli[str_detect(df_cli$NombreCliente, 'PUEBLA REMISION'),
       'NombreCliente'] = 'OUTROS_GRANDES'
df_cli[str_detect(df_cli$NombreCliente, 'ARTEAGA REMISION'),
       'NombreCliente'] = 'OUTROS_GRANDES'
df_cli[str_detect(df_cli$NombreCliente, 'BOD  AURRERA'),
       'NombreCliente'] = 'OUTROS_GRANDES'
df_cli[str_detect(df_cli$NombreCliente, 'AUTOBUSES DE LA PIEDAD'),
       'NombreCliente'] = 'OUTROS_GRANDES'
df_cli[str_detect(df_cli$NombreCliente, 'EMBOTELLADORA DE CULIACAN'),
       'NombreCliente'] = 'OUTROS_GRANDES'
df_cli[str_detect(df_cli$NombreCliente, 'BEBIDAS ENVASADAS DEL PACIFICO'),
       'NombreCliente'] = 'OUTROS_GRANDES'
df_cli[str_detect(df_cli$NombreCliente, 'BEBIDAS MUNDIALES'),
       'NombreCliente'] = 'OUTROS_GRANDES'
df_cli[str_detect(df_cli$NombreCliente, 'CAMIONERA DEL GOLFO'),
       'NombreCliente'] = 'OUTROS_GRANDES'
df_cli[str_detect(df_cli$NombreCliente, 'CARLS JR  MONTERREY'),
       'NombreCliente'] = 'OUTROS_GRANDES'
df_cli[str_detect(df_cli$NombreCliente, 'CARLS JR MONTERREY'),
       'NombreCliente'] = 'OUTROS_GRANDES'
df_cli[str_detect(df_cli$NombreCliente, 'OASIS RESORTS'),
       'NombreCliente'] = 'OUTROS_GRANDES'
df_cli[str_detect(df_cli$NombreCliente, 'PRODUCTOS SEREL'),
       'NombreCliente'] = 'OUTROS_GRANDES'
 

cliente <- c('NO IDENTIFICADO', 'OXXO', 'ABARROTES', 'FARMACIA', 'HOSPITAL', 
             'HOTEL', 'PAPELERIA', 'ESCUELA', 'MERCADO', 'SUPERMERCADO',
             'PUESTO', 'COMEDORIA', 'OUTROS_GRANDES')

cli <- sapply(df_cli$NombreCliente,
              function(cli, cliente){
                if(cli %in% cliente)
                  c <- cli
                else
                  c <- 'OUTROS'
                return(c)
              }, cliente)

df_cli$NombreCliente <- cli

df$NombreCliente = NULL
df <- df %>%
  inner_join(df_cli)

# Analizando o cliente pelo total da demanda
df %>%
  group_by(NombreCliente) %>%
  summarise(sum_dem = sum(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(NombreCliente, -sum_dem), y=sum_dem)) +
  geom_col() +
  ggtitle("Tipo de Cliente pelo Total da Demanda") +
  xlab("Tipo de Cliente") +
  ylab("Total da Demanda") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60))

# Analizando o cliente pela mediana da demanda
df %>%
  group_by(NombreCliente) %>%
  summarise(median_dem = median(Demanda_uni_equil)) %>%
  ggplot(aes(x=reorder(NombreCliente, -median_dem), y=median_dem)) +
  geom_col() +
  ggtitle("Tipo de Cliente pela Mediana da Demanda") +
  xlab("Tipo de Cliente") +
  ylab("Mediana da Demanda") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60))


# Diminuindo o data set para realizar a segunda predição.
indexTraining <- createDataPartition(df$Demanda_uni_equil, p = 0.1,list = FALSE,  times = 1)
dft <- df[indexTraining, ]
nrow(dft)
head(dft)
dft$Marca <- as.factor(dft$Marca)
dft$P_Nome_Prod_Outros <- as.factor(dft$P_Nome_Prod_Outros)
dft$NombreCliente <- as.factor(dft$NombreCliente)

# Split do Data Set
trainIndex <- createDataPartition(dft$Demanda_uni_equil, p = 0.7,list = FALSE,  times = 1)
train <- dft[trainIndex,]
test <- dft[-trainIndex,]

# Treinando o modelo com o algoritmo de regressão logística
model_glm_v2 <- glm(formula = formula_v1, data = train, family = "gaussian")
# Verificando alguns resultados do modelo treinado
summary(model_glm_v2)
# Realizando a predição com o modelo treinado
pred_glm_v2 <- predict(model_glm_v2, test, type="response")
# Arredondando
pred_glm_v2 <- round(pred_glm_v2)
#install.packages("hydroGOF")
#Load Library
library(hydroGOF)
#Cálculo do RMSE
rmse(test$Demanda_uni_equil,pred_glm_v2)


# Treinando o modelo com o algoritmo de regressão linear
model_lm_v2 <- lm(formula = formula_v1, data = train)
# Verificando alguns resultados do modelo treinado
summary(model_lm_v2)
# Realizando a predição com o modelo treinado
pred_lm_v2 <- predict(model_lm_v2, test, type="response")
# Arredondand
pred_lm_v2 <- round(pred_lm_v2)
#Cálculo do RMSE
rmse(test$Demanda_uni_equil,pred_lm_v2)

library(rpart)
# treinando o modelo
modelo_tree_v2 = rpart(formula = formula_v1, data = train, 
                       control = rpart.control(cp = .0005)) 
# Previsões nos dados de teste
pred_tree_v2 = predict(modelo_tree_v2, test, type='vector')
#Cálculo do RMSE
rmse(test$Demanda_uni_equil,pred_tree_v2)

# Ainda não obtivemos um resultado muito satisfatório, vamos inseri a variável Pedaços,
# mas para isso vamos primeiro trocar os valores 0 por 1, pois existe sempre pelo menos 
# 1 pedaço do produto e tirar o valor p para que tenhamos um valor numérico.
head(df)
sum(is.na(df)==TRUE)

df[df$Pedacos == 0, 'Pedacos'] = 1
df[, 'Pedacos'] = gsub("([0-9]+).*$", "\\1", df$Pedacos)
df$Pedacos = as.numeric(df$Pedacos)

# Diminuindo o data set para realizar a predição.
indexTraining <- createDataPartition(df$Demanda_uni_equil, p = 0.1,list = FALSE,  times = 1)
dft <- df[indexTraining, ]
nrow(dft)
head(dft)

# Split do Data Set
trainIndex <- createDataPartition(dft$Demanda_uni_equil, p = 0.7,list = FALSE,  times = 1)
train <- dft[trainIndex,]
test <- dft[-trainIndex,]

# Treinamento do Modelo
# Criando os modelos preditivos
formula_v2 <- as.formula('Demanda_uni_equil ~ Peso + Marca + Pedacos + P_Nome_Prod_Outros + 
                         NombreCliente + Faixa_habitantes')
# Treinando o modelo com o algoritmo de regressão logística
model_glm_v3 <- glm(formula = formula_v2, data = train, family = "gaussian")
# Verificando alguns resultados do modelo treinado
summary(model_glm_v3)
# Realizando a predição com o modelo treinado
pred_glm_v3 <- predict(model_glm_v3, test, type="response")
# Arredondando
pred_glm_v3 <- round(pred_glm_v3)
#install.packages("hydroGOF")
#Load Library
library(hydroGOF)
#Cálculo do RMSE
rmse(test$Demanda_uni_equil,pred_glm_v3)

# Treinando o modelo com o algoritmo de regressão linear
model_lm_v3 <- lm(formula = formula_v2, data = train)
# Verificando alguns resultados do modelo treinado
summary(model_lm_v3)
# Realizando a predição com o modelo treinado
pred_lm_v3 <- predict(model_lm_v3, test, type="response")
# Arredondand
pred_lm_v3 <- round(pred_lm_v3)
#Cálculo do RMSE
rmse(test$Demanda_uni_equil,pred_lm_v3)

library(rpart)
# treinando o modelo
modelo_tree_v3 = rpart(formula = formula_v2, data = train, 
                       control = rpart.control(cp = .0005)) 
# Previsões nos dados de teste
pred_tree_v3 = predict(modelo_tree_v3, test, type='vector')
#Cálculo do RMSE
rmse(test$Demanda_uni_equil,pred_tree_v3)

# O resultado ainda não está bom, vamos analizar melhor o produto, pois ainda existem muitos
# tipos de produtos. Vamos tentar diminuir isso um pouco mais.

produtos <- df %>%
  group_by(P_Nome_Prod_Outros) %>%
  summarise(n = n())

nrow(produtos)
View(produtos)

# Criando uma nova variável de P_Nome_Prod
df$P_Nome_Prod_Outros_Mod <- df$P_Nome_Prod_Outros

# Fazendo algumas alterações com relação a variável mãe.
df[df$P_Nome_Prod_Outros_Mod == 'Barras', 'P_Nome_Prod_Outros_Mod'] = 'Barra'
df[df$P_Nome_Prod_Outros_Mod == 'Barri', 'P_Nome_Prod_Outros_Mod'] = 'Barra'
df[df$P_Nome_Prod_Outros_Mod == 'Barritas', 'P_Nome_Prod_Outros_Mod'] = 'Barra'
df[df$P_Nome_Prod_Outros_Mod == 'SuperBollos', 'P_Nome_Prod_Outros_Mod'] = 'Bollo'
df[df$P_Nome_Prod_Outros_Mod == 'Bollos', 'P_Nome_Prod_Outros_Mod'] = 'Bollo'
df[df$P_Nome_Prod_Outros_Mod == 'Dona', 'P_Nome_Prod_Outros_Mod'] = 'Donas'
df[df$P_Nome_Prod_Outros_Mod == 'Mantecada', 'P_Nome_Prod_Outros_Mod'] = 'Mantecadas'
df[df$P_Nome_Prod_Outros_Mod == 'Pastiseta', 'P_Nome_Prod_Outros_Mod'] = 'Pastisetas'
df[df$P_Nome_Prod_Outros_Mod == 'Pinguis', 'P_Nome_Prod_Outros_Mod'] = 'Pinguinos'
df[df$P_Nome_Prod_Outros_Mod == 'Polvoron', 'P_Nome_Prod_Outros_Mod'] = 'Polvoroncitos'
df[df$P_Nome_Prod_Outros_Mod == 'Polvorones', 'P_Nome_Prod_Outros_Mod'] = 'Polvoroncitos'
df[df$P_Nome_Prod_Outros_Mod == 'Semita', 'P_Nome_Prod_Outros_Mod'] = 'Semitas'
df[df$P_Nome_Prod_Outros_Mod == 'Tortillas', 'P_Nome_Prod_Outros_Mod'] = 'Tortilla'
df[df$P_Nome_Prod_Outros_Mod == 'Tortillina', 'P_Nome_Prod_Outros_Mod'] = 'Tortilla'
df[df$P_Nome_Prod_Outros_Mod == 'Tortillinas', 'P_Nome_Prod_Outros_Mod'] = 'Tortilla'
df[df$P_Nome_Prod_Outros_Mod == 'Tortinachos', 'P_Nome_Prod_Outros_Mod'] = 'Tortilla'
df[df$P_Nome_Prod_Outros_Mod == 'Tostadita', 'P_Nome_Prod_Outros_Mod'] = 'Tostada'
df[df$P_Nome_Prod_Outros_Mod == 'Tostado', 'P_Nome_Prod_Outros_Mod'] = 'Tostada'
df[df$P_Nome_Prod_Outros_Mod == 'Tostachos', 'P_Nome_Prod_Outros_Mod'] = 'Tostada'
df[df$P_Nome_Prod_Outros_Mod == 'Totopo', 'P_Nome_Prod_Outros_Mod'] = 'Totopos'
df[df$P_Nome_Prod_Outros_Mod == 'Twinkies', 'P_Nome_Prod_Outros_Mod'] = 'Tuinky'
df[df$P_Nome_Prod_Outros_Mod == 'TuinKids', 'P_Nome_Prod_Outros_Mod'] = 'Tuinky'
df[df$P_Nome_Prod_Outros_Mod == 'Tri', 'P_Nome_Prod_Outros_Mod'] = 'Triki'
df[df$P_Nome_Prod_Outros_Mod == 'Roles', 'P_Nome_Prod_Outros_Mod'] = 'Rollo'
df[df$P_Nome_Prod_Outros_Mod == 'Panque', 'P_Nome_Prod_Outros_Mod'] = 'Panquecito'
df[df$P_Nome_Prod_Outros_Mod == 'Panitos', 'P_Nome_Prod_Outros_Mod'] = 'Pan'
df[df$P_Nome_Prod_Outros_Mod == 'Panditas', 'P_Nome_Prod_Outros_Mod'] = 'Pan'
df[df$P_Nome_Prod_Outros_Mod == 'Panera', 'P_Nome_Prod_Outros_Mod'] = 'Pan'
df[df$P_Nome_Prod_Outros_Mod == 'Panquecito', 'P_Nome_Prod_Outros_Mod'] = 'Pan'
df[df$P_Nome_Prod_Outros_Mod == 'Galletas', 'P_Nome_Prod_Outros_Mod'] = 'Galleta'
df[df$P_Nome_Prod_Outros_Mod == 'Galleton', 'P_Nome_Prod_Outros_Mod'] = 'Galleta'
df[df$P_Nome_Prod_Outros_Mod == 'Empanizador', 'P_Nome_Prod_Outros_Mod'] = 'Empanaditas'
df[df$P_Nome_Prod_Outros_Mod == 'Donitas', 'P_Nome_Prod_Outros_Mod'] = 'Donas'
df[df$P_Nome_Prod_Outros_Mod == 'DonitasEspolvoreadas', 'P_Nome_Prod_Outros_Mod'] = 'Donas'
df[df$P_Nome_Prod_Outros_Mod == 'Colchones', 'P_Nome_Prod_Outros_Mod'] = 'Conchas'
df[df$P_Nome_Prod_Outros_Mod == 'Conchitas', 'P_Nome_Prod_Outros_Mod'] = 'Conchas'
df[df$P_Nome_Prod_Outros_Mod == 'Chocochispas', 'P_Nome_Prod_Outros_Mod'] = 'Choco'
df[df$P_Nome_Prod_Outros_Mod == 'Chocodonitas', 'P_Nome_Prod_Outros_Mod'] = 'Choco'
df[df$P_Nome_Prod_Outros_Mod == 'Chocotorro', 'P_Nome_Prod_Outros_Mod'] = 'Choco'
df[df$P_Nome_Prod_Outros_Mod == 'Bimbolunch', 'P_Nome_Prod_Outros_Mod'] = 'Bimbollos'
df[df$P_Nome_Prod_Outros_Mod == 'Bimbunuelos', 'P_Nome_Prod_Outros_Mod'] = 'Bimbollos'

produtos <- df %>%
  group_by(P_Nome_Prod_Outros_Mod) %>%
  summarise(n = n())

nrow(produtos)

# Treinamento do Modelo
# Criando os modelos preditivos
formula_v3 <- as.formula('Demanda_uni_equil ~ Peso + Marca + Pedacos + P_Nome_Prod_Outros_Mod + 
                         NombreCliente + Faixa_habitantes')
# Treinando o modelo com o algoritmo de regressão logística
model_glm_v4 <- glm(formula = formula_v3, data = train, family = "gaussian")
# Verificando alguns resultados do modelo treinado
summary(model_glm_v4)
# Realizando a predição com o modelo treinado
pred_glm_v4 <- predict(model_glm_v4, test, type="response")
# Arredondando
pred_glm_v4 <- round(pred_glm_v4)
#install.packages("hydroGOF")
#Load Library
library(hydroGOF)
#Cálculo do RMSE
rmse(test$Demanda_uni_equil,pred_glm_v4)


# Treinando o modelo com o algoritmo de regressão linear
model_lm_v4 <- lm(formula = formula_v3, data = train)
# Verificando alguns resultados do modelo treinado
summary(model_lm_v4)
# Realizando a predição com o modelo treinado
pred_lm_v4 <- predict(model_lm_v4, test, type="response")
# Arredondand
pred_lm_v4 <- round(pred_lm_v4)
#Cálculo do RMSE
rmse(test$Demanda_uni_equil,pred_lm_v4)

library(rpart)
# treinando o modelo
modelo_tree_v4 = rpart(formula = formula_v3, data = train, 
                       control = rpart.control(cp = .0005)) 
# Previsões nos dados de teste
pred_tree_v4 = predict(modelo_tree_v4, test, type='vector')
#Cálculo do RMSE
rmse(test$Demanda_uni_equil,pred_tree_v4)


# Não houve melhoras significativas nessas mudanças, talvez para melhorar o código
# devemos organizar melhor os clientes e produtos, pois acredito que a demanda
# depende muito deles.
# De qualquer forma, foi um aprendizado muito grande manipular strings complexas
# como a nome do produto, onde existem varias informações em uma string só.
