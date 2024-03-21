
#-------------------------------------------------------------------------------
#
# MÉTODOS ESTATÍSTICOS EM PESQUISA CIENTÍFICA - MEPC
# OFERTA 2023
# APOIO COMPUTACIONAL EM LINGUAGEM R
#
#-------------------------------------------------------------------------------
# 
# Prof. Lineu Alberto Cavazani de Freitas
# Laboratório de Estatística e Geoinformação
# Departamento de Estatística
# Universidade Federal do Paraná
# URL: https://lineu96.github.io/
#
# Visualização de dados com ggplot2
#
#-------------------------------------------------------------------------------

# Instalação e documentação

# install.packages('ggplot2')
library(ggplot2)
?ggplot()

#-------------------------------------------------------------------------------

# Gerando um conjunto de dados

set.seed(1)

v1 = rnorm(1500, 20, 5)
v2 = rnorm(1500, 20, 5 )

x = v1*2
y = v1+v2

z = as.factor(rep(c('a','b'), 750))

df <- data.frame(x=x,y=y,z=z)

head(df)

#-------------------------------------------------------------------------------

# Análise com o R base

par(mfrow = c(1,3))
hist(df$y,probability = T, main = 'Histograma',col=3)
box()
plot(y~z, df, col = 4, main='Boxplot')
plot(y~x,df, col=z, main='Gráfico de Dispersão')
abline(lm(y~x, df), lwd=3, col=4)

#-------------------------------------------------------------------------------
# Análise com ggplot2
#-------------------------------------------------------------------------------

## Análises univariadas

### Histograma
ggplot(data=df, mapping = aes(x=x)) + 
  geom_histogram()

ggplot(data=df, mapping = aes(x=x)) + 
  geom_histogram(col=1, fill='#00FFFF', alpha=0.3)

### Densidade
ggplot(data=df, mapping = aes(x=y))+
  geom_density()

ggplot(data=df, mapping = aes(x=y)) +
  geom_density(col=4,
               fill='#088A85',
               alpha=0.3)

### Boxplot
ggplot(data=df, mapping = aes(y=y)) + 
  geom_boxplot()

ggplot(data=df, mapping = aes(y=y)) + 
  geom_boxplot(col=4, fill=3, alpha=0.4)

ggplot(data=df, mapping = aes(y=y)) + 
  geom_boxplot(col=4, fill=3, alpha=0.4) + 
  coord_flip()

### Gráfico de barras
tabela <- as.data.frame(table(df$z))

ggplot(data=tabela, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity")

ggplot(data=tabela, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", 
           fill=5, 
           col=4, 
           alpha=0.2,
           lwd=1)

### Gráfico de setores
ggplot(tabela, aes(x="", y=Freq, fill=Var1))+
  geom_bar(stat = "identity")+ 
  coord_polar("y", start=0)

#-------------------------------------------------------------------------------

## Análises bivariadas/trivariadas

### Boxplot para níveis de um fator
ggplot(data=df, mapping = aes(x=z,y=y)) + 
  geom_boxplot()

### Gráficos de dispersão
ggplot(data=df, mapping = aes(x=x,y=y)) + 
  geom_point()

ggplot(data=df, mapping = aes(x=x,y=y,col=z)) +
  geom_point(alpha=0.8) + 
  geom_smooth(method = 'lm', col=1)

#-------------------------------------------------------------------------------

## Adicionando eixos e títulos
ggplot(data=df, mapping = aes(x=x,y=y))+
  geom_point(alpha=0.5)

ggplot(data=df, mapping = aes(x=x,y=y))+
  geom_point(alpha=0.5)+
  ggtitle('Título')+
  xlab('Eixo x')+
  ylab('Eixo y')

#-------------------------------------------------------------------------------

## Facets
ggplot(data=df, mapping = aes(x=x,y=y)) + 
  geom_point(alpha=0.5)+
  geom_smooth(se=F, lwd=1.5, col=2) + 
  facet_wrap(~z)

#-------------------------------------------------------------------------------

## Gráficos em painel

# install.packages("gridExtra")
library(gridExtra)

g1 <- ggplot(data=df, mapping = aes(x=y))+
  geom_density(col=2, fill=2, alpha=0.3)

g2 <- ggplot(data=df, mapping = aes(x=y))+
  geom_histogram(col=6, fill=6, alpha=0.3)

g3 <- ggplot(data=df, mapping = aes(x=x,y=y,col=z))+
  geom_point(alpha=0.5)+
  geom_smooth(col=1)

g4 <- ggplot(data=df, mapping = aes(x=z,y=y,col=z)) +
  geom_boxplot(col=c(3,4),
               fill=c(3,4),
               alpha=0.3)
grid.arrange(g1,g2,g3,g4,nrow=2,ncol=2)

#-------------------------------------------------------------------------------

## Alguns temas

ggplot(data=df, mapping = aes(x=x,y=y))+
  geom_point(alpha=0.5)+
  ggtitle('Título')+
  xlab('Eixo x')+
  ylab('Eixo y') + 
  theme_bw()

ggplot(data=df, mapping = aes(x=x,y=y))+
  geom_point(alpha=0.5)+
  ggtitle('Título')+
  xlab('Eixo x')+
  ylab('Eixo y') + 
  theme_classic()

ggplot(data=df, mapping = aes(x=x,y=y))+
  geom_point(alpha=0.5)+
  ggtitle('Título')+
  xlab('Eixo x')+
  ylab('Eixo y') + 
  theme_dark()

ggplot(data=df, mapping = aes(x=x,y=y))+
  geom_point(alpha=0.5)+
  ggtitle('Título')+
  xlab('Eixo x')+
  ylab('Eixo y') + 
  theme_gray()

ggplot(data=df, mapping = aes(x=x,y=y))+
  geom_point(alpha=0.5)+
  ggtitle('Título')+
  xlab('Eixo x')+
  ylab('Eixo y') + 
  theme_light()

ggplot(data=df, mapping = aes(x=x,y=y))+
  geom_point(alpha=0.5)+
  ggtitle('Título')+
  xlab('Eixo x')+
  ylab('Eixo y') + 
  theme_minimal()

ggplot(data=df, mapping = aes(x=x,y=y))+
  geom_point(alpha=0.5)+
  ggtitle('Título')+
  xlab('Eixo x')+
  ylab('Eixo y') + 
  theme_test()

ggplot(data=df, mapping = aes(x=x,y=y))+
  geom_point(alpha=0.5)+
  ggtitle('Título')+
  xlab('Eixo x')+
  ylab('Eixo y') + 
  theme_void()

#-------------------------------------------------------------------------------

## Gráficos interativos

grafico <- ggplot(data=df, mapping = aes(x=x,y=y,col=z)) +
  geom_point(alpha=0.8) + 
  geom_smooth(method = 'lm', col=1)

grafico

#install.packages('plotly')
library(plotly)
ggplotly(grafico)

#-------------------------------------------------------------------------------