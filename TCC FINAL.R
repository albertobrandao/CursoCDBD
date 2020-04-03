##### PONTIFICIA UNIVERSIDADE CATOLICA DE MINAS GERAIS - PUC/MG #####
##### PÓS-GRADUACAO EM CIENCIA DE DADOS E BIG DATA #####
##### TRABALHO DE CONCLUSAO DE CURSO #####
##### AUTOR: JOSE ALBERTO DE SIQUEIRA BRANDAO #####

##### SCRIPT #####

##### ANALISE PREDITIVA DA CARGA HORARIA DE UM PROGRAMA DE FORMACAO NO SETOR PUBLICO COM USO DE SERIES TEMPORAIS #####



##### ETL #####

# define o diretorio onde estao os arquivos

setwd('E:/TCC_DATA')   

# carga dos arquivos anuais do PFC

install.packages('readxl')
library(readxl)        

d14 <- read_excel("E:/TCC_DATA/PFC_ANUAL/PFC_2014.xlsx", sheet="BD_PFC_(2014)")
d15 <- read_excel("E:/TCC_DATA/PFC_ANUAL/PFC_2015.xlsx", sheet="BD_PFC_(2015)")
d16 <- read_excel("E:/TCC_DATA/PFC_ANUAL/PFC_2016.xlsx", sheet="BD_PFC_(2016)")
d17 <- read_excel("E:/TCC_DATA/PFC_ANUAL/PFC_2017.xlsx", sheet="BD_PFC_(2017)")
d18 <- read_excel("E:/TCC_DATA/PFC_ANUAL/PFC_2018.xlsx", sheet="BD_PFC_(2018)")
d19 <- read_excel("E:/TCC_DATA/PFC_ANUAL/PFC_2019.xlsx", sheet="BD_PFC_(2019)")

# empilhamento dos dados por linha criando um dataset ds

ds <- rbind(d14,d15,d16,d17,d18,d19)

# construção de subconjunto com GGOVs

dsg = subset(ds, TIPOSERV != "Não" & CPF !="0")
attach(dsg)


##### ESTATISTICA DESCRITIVA #####

# calcula medidas de posicao

summary(CHV)

# calcula medidas de dispersao (variancia e desvio padrao)

var(CHV)
sd(CHV) # ou sqrt(var(dsg$CHV))

# calculando a amplitude total

ampdsg <- max(CHV)-min(CHV)
ampdsg

# calculando o coeficiente de variacao

sd(CHV)/mean(CHV)*100


##### GRÁFICOS - ESTATISTICA DESCRITIVA #####

# construindo histograma de observacoes por ano

install.packages("UsingR")
library(UsingR)

# construindo graficos de pizza

par(mfrow=c(1,2))

#Grafico1

chtipo <- table(TIPO)
chtipoval <- signif(chtipo/sum(chtipo)*100, 3)
chtipoval
pie(chtipoval, main = "Tipo de capacitacao", labels = paste(chtipoval,"%",sep=""), col = c("steelblue1","dodgerblue3","orange", "lightgreen", "red", "pink", "palevioletred2", "green", "gold"))
textot <- c("Congresso", "Curso", "Disciplina", "Forum", "Instrutoria", "Oficina", "Palestra", "Seminario", "Simposio")
legend(x = "topright", legend = textot, fill = c("steelblue1","dodgerblue3","orange", "lightgreen", "red", "pink", "palevioletred2", "green", "gold"), cex = 0.50)

#Grafico2
chforma <- table(FORMA)
chformaval <- signif(chforma/sum(chforma)*100, 3)
chformaval
pie(chformaval, main = "Forma de capacitacao", labels = paste(chformaval,"%",sep=""), col = c("steelblue1","dodgerblue3","green"))
textof <- c("Instrutoria", "Ofertado", "Validado")
legend(x = "topright", legend = textof, fill = c("steelblue1","dodgerblue3","green"), cex = 0.65)

#Grafico3
chmodal <- table(MODALIDADE)
chmodalval <- signif(chmodal/sum(chmodal)*100, 3)
chmodalval
pie(chmodalval, main = "Modalidade de capacitacao", labels = paste(chmodalval,"%",sep=""), col = c("steelblue1","dodgerblue3"))
textom <- c("EAD", "Presencial")
legend(x = "topright", legend = textom, fill = c("steelblue1","dodgerblue3"), cex = 0.65)

#Grafico4
chlot <- table(LOTACAO)
chlotval <- signif(chlot/sum(chlot)*100, 3)
chlotval
pie(chlotval, main = "Lotacao", labels = paste(chlotval,"%",sep=""), col = c("steelblue1","dodgerblue3","orange", "lightgreen", "red", "pink", "palevioletred2", "green", "gold", "gray"))
textol <- c("Cedido", "Condepe", "Gabinete", "IGPE", "SEAM", "SECOGE", "SEGES", "SEPOC", "SEGPR", "SGTG")
legend(x = "topright", legend = textol, fill = c("steelblue1","dodgerblue3","orange", "lightgreen", "red", "pink", "palevioletred2", "green", "gold", "gray"), cex = 0.50)


##### AGRUPAMENTO DOS DADOS #####

# acumular dados por mes e ano

tempo_POSIX <- strptime(DATAI, format = "%d.%m.%Y %H:%M", tz = "GMT")
x <- as.POSIXct(c(DATAI))
da <- strftime(x, "01")
mo <- strftime(x, "%m")
yr <- strftime(x, "%Y")
chv <- runif(3)
dd <- data.frame(da, mo, yr, CHV)

dsg.agr <- aggregate(CHV ~ da + mo + yr, dd, FUN = sum)
detach(dsg)
attach(dsg.agr)
dsg.agr$DMA <- as.POSIXct(paste(dsg.agr$yr, dsg.agr$mo, dsg.agr$da, sep = "-"))

# gerando gráfico de série temporal mensal e anual

par(mfrow = c(1, 2))

boxplot(dsg.agr$CHV~dsg.agr$mo, xlab="Mes", ylab = "Carga Horaria", main ="Carga Horaria Mensal - 2014-2019")

boxplot(dsg.agr$CHV~dsg.agr$yr, xlab="Mes", ylab = "Carga Horaria", main ="Carga Horaria Anual - 2014-2019")


##### CRIACAO DA SERIE TEMPORAL #####

install.packages('xts')
library(xts)

st <- ts(CHV, start = c(2014,01), end = c(2019,12), frequency = 12)
st

# grafico em grade da serie temproal

par(mfrow = c(1, 1))

install.packages("dygraphs")
library(dygraphs)
dygraph(st,xlab="Ano", ylab = "Carga horaria",main="Serie Temporal - 2014-2019")

#gráfico mensal radar

install.packages("fpp2")
library(fpp2)
ggseasonplot(st, season.labels = NULL,
             year.labels = FALSE,
             year.labels.left = FALSE,
             main = "Distribuição da Série Temporal por Ano e Mês",
             continuous = FALSE,
             polar = TRUE,
             labelgap = 0.25)

#gráfico de variacao mensal

monthplot(st, xlab="Ano", ylab = "Carga horaria",main="Variacao Mensal da Serie Temporal - 2014-2019")


##### PROJECAO SERIE TEMPORAL - MODELO SNAIVE #####

install.packages('forecast')
library(forecast)

summary(snaive(st,h=12))
plot(snaive(st,h=12),include=200, xlab="Ano", ylab = "Carga horaria",main="Projecao da Serie Temporal conforme Modelo SNAIVE- 2020")


##### MODELAGEM AUTO-ARIMA SERIE TEMPORAL ORIGINAL #####

autoARIMAst <- auto.arima(st)
autoARIMAst
prevARIMAst <- forecast(autoARIMAst, h=12)
prevARIMAst
plot(prevARIMAst, xlab="Ano", ylab = "Carga horaria",main="Projecao da Serie Temporal conforme Modelo ARIMA - 2020")


##### DECOMPOSICAO DA SERIE TEMPORAL #####

dec <- decompose(st)
plot(dec)

lag.plot(st, lags = 4, do.lines = FALSE)

ggtsdisplay(st,xlab="Ano", main="Serie temporal 2014-2019 com ACF e PACF")


##### TESTE DICKEY-FURLEY AUMENTADO APLICADO A SERIE TEMPORAL ORIGINAL #####

install.packages('urca')
library(urca)

install.packages('tseries')
library(tseries)

adf.drift <- ur.df(y = st, lags = 24, selectlags = "AIC")
adf.test(st, alternative="stationary", k=0)
acf(adf.drift@res)
adf.drift@teststat 
adf.drift@cval #valores tabulados por MacKinnon (1996)
summary(adf.drift)


##### TRANSFORMACAO DA SERIE TEMPORAL (COM DIFERENCIACAO LOGARITMICA) #####

ggtsdisplay(diff(log(st)),xlab="Ano", main="Serie temporal Transformada 2014-2019 com ACF e PACF")

##### TESTE DICKEY-FURLEY AUMENTADO APLICADO A SERIE TEMPORAL TRANSFORMADA (COM DIFERENCIACAO LOGARITMICA) #####

adf.driftd <- ur.df(y = diff(log(st)), type = c("drift"), lags = 24, selectlags = "AIC")
adf.test(diff(log(st)), alternative="stationary", k=0)
adf.driftd@teststat 
adf.drift@cval #valores tabulados por MacKinnon (1996)
summary(adf.driftd)


##### IDENTIFICACAO DA SERIE TEMPORAL TRANSFORMADA #####

install.packages('lmtest')
library(lmtest)

Box.test(diff(log(st)), lag = 24, type = "Ljung-Box")


##### MODELAGEM AUTO-ARIMA SERIE TEMPORAL TRANSFORMADA #####

autoARIMAstd <- auto.arima(diff(log(st)))
autoARIMAstd
prevARIMAstd <- forecast(autoARIMAstd, h=12)
prevARIMAstd
plot(prevARIMAstd, xlab="Ano", ylab = "Carga horaria",main="Projecao da Serie Temporal Transformada conforme Modelo ARIMA - 2020")

##### ESTIMACAO DA SERIE TEMPORAL #####


##### MODELAGEM ARIMA SERIE TEMPORAL TRANSFORMADA #####


fit.modelARIMA <- arima(log(st), c(0,1,3), seasonal = list(order=c(0,1,3), period=12))
summary(fit.modelARIMA)



##### DIAGNOSTICO DO MODELO ARIMA (0,1,3) DA SERIE TEMPORAL TRANSFORMADA #####


tsdiag(fit.modelARIMA, gof.lag = 20)

Box.test(x = fit.modelARIMA$residuals, lag = 24, type = "Ljung-Box", fitdf = 2)

install.packages('FinTS')
library(FinTS)
ArchTest(fit.modelARIMA$residuals,lags = 24) 

install.packages('normtest')
library(normtest)
jb.norm.test(fit.modelARIMA$residuals, nrepl=1000)


AIC(autoARIMAstd)
AIC(fit.modelARIMA)

logLik(autoARIMAstd)
logLik(fit.modelARIMA)

accuracy(autoARIMAstd)
accuracy(fit.modelARIMA)


##### PREVISAO #####


pred <- predict(fit.modelARIMA, n.ahead = 24, level = 0.95)
ts.plot(st,exp(pred$pred), log = "y", lty = c(1,3),xlab="Ano", ylab = "Carga horaria",main="Projecao Final da Serie Temporal 2020/2021 Modelo ARIMA (0,1,3)")

previsao <- exp(pred$pred)
previsao

##### FIM #####

