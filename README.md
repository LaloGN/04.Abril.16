# 04.Abril.16
##Una vez que tenemos y conocemos la grafica 
#para determinar con mayor precision
##Las tendencias y estacionalidad se utiliza la funcion season plot 
#que esta en la libreria fpp

install.packages ("fpp")
require (fpp)
ica <- read.csv ("C:\\Users\\SALA-C18\\Desktop\\table (3).csv", header = T)
icats <- ts(ica [ , 5],start= 2000, frequency = 12)
seasonplot(icats,s= 12, year.labels=TRUE,
           main="Valor acciones ICA",
           ylab="Valor cierre",col=rainbow(26),year.labels.left=TRUE,pch=15)

bim <- read.csv ("C:\\Users\\SALA-C18\\Desktop\\table (4).csv", header = T)
bimts <- ts(bim [ , 5],start= 2000, frequency = 12)
seasonplot(bimts,s= 12, year.labels=TRUE,
           main="Valor acciones BIMBO",
           ylab="Valor cierre",col=rainbow(22),year.labels.left=TRUE,pch=8)

ce <- read.csv ("C:\\Users\\SALA-C18\\Desktop\\table (7).csv", header = T)
cets <- ts(ce [ , 5],start= 2000, frequency = 12)
seasonplot(cets,s= 12, year.labels=TRUE,
           main="Valor acciones CEMEX",
           ylab="Valor cierre",col=rainbow(20),year.labels.left=TRUE,pch=36)

##con monthplot pódemos hacer un analisis de la st dependiendo del periodo 
#de los datos por ejemplo si tenemos trimestral se pueden analizar los 4 trimestres...en grafica

monthplot(icats,ylab="valor cierre",xlab="Month",
          main="Valor acciones ICA")

###FUNCIONES UTILIZADAS###
ST graficas
plot or plot.ts
Grafica de temporalidad 
seasonplot
graficas de subseries temporales 
monthplot

###CORRELACION Y COVARIANZA
#Covarianza y correlación : medida del grado de relación lineal entre dos variables (y Y x).
###AUTO-COVARIANZA Y AUTO-CORRELACION
###Autocovariancia y autocorrelación : medida relación lineal entre los valores,
# retardados de una series de tiempo y.
##Se mide  la relación entre: 
#yt and yt−1
#yt and yt−2
#yt and yt−3, etc.



##Para desestacionalizar y eliminar la tendencia necesitamos conocer la correlacion de 
##las variables para realizar mejores pronosticos de nuestra serie de tiempo.

###En muchas casos las variables estan correlacionadas... entonces si nosotros logramos 
#identificar la correlacion podemos mejorar los pronosticos, si las correlaciones son altas st 
#la correlacion estructura en ST esta determinada por la función de correlacion y se estima
# para la ST observada. 

###AUTOCORRELACION
##A veces sucede que  los valores que toma una variable en el tiempo no
#son independientes entre sí, sino que un valor determinado depende de los valores anteriores,
#para obtener la correlación se necesita obytener primero la covarianza.

###COVARIANZA
##Ejercicio en r verifiquen que la formula de la covarianza y la correlacion den el mismo resultado 
##que las funciones cov(x,y) y cor(x,y).
##La covarianza es una medida de asociación entre dos variables
### Cov(x, y) =(x i − x (media))(y i − y ((media)))/(n − 1)

##EJERCICIO
sum (((icats) - mean(icats))*(bimts - mean(bimts))) / (192-1)
cov(icats, bimts)
cov(icats, bimts) / (sd(icats)*sd(bimts))
cor(icats, bimts)
length(cets)


sum((x) - mean(x))*(y - mean(y))) / (n - 1) ##Calculo de la varianza con formula
cov(x, y) ###Funcion de R para obtener la covarianza

##La correlacion tambien es una medida de relacion entre dos variables que permite 
#Cor(x, y) = Cov(x, y) \ sd(x)sd(y)
cov(x,y) / (sd(x)*sd(y)) #calculamos la correlacion con formula
cor(x,y) # funcion de R para obtener la correlacion 



##EJERCICIOS
#1)Analizar los años de las tres empresas elegidas(seansonplot) 
#y elegir los tres años que ubiquen con estacionalidad,
# ciclo o tendencia...intersectar estos tres años 
##2)Calcular con la formula y la funcion de covarianza y 
#correlacion en la combinacion de las tres empresas,
#es decir empresa 1=x, empresa2=y y empresa3=z, calcular las correlaciones de
#cor(x,y), cor(y,z), cor(x,z)
layout(1:1)
seasonplot(icats,s= 12, year.labels=F,
           main="Valor acciones ICA",
           ylab="Valor cierre",col=rainbow(2),year.labels.left=TRUE,pch=15)
seasonplot(bimts,s= 12, year.labels=TRUE,
           main="Valor acciones BIMBO",
           ylab="Valor cierre",col=rainbow(3),year.labels.left=TRUE,pch=8)
seasonplot(cets,s= 12, year.labels=TRUE,
           main="Valor acciones CEMEX",
           ylab="Valor cierre",col=rainbow(4),year.labels.left=TRUE,pch=36)
##Para los años 2006-2009 las empresas ica, bimbo, celmex muestran algunos ciclos, 
##no tienen tendencia y pueden llegar a
#terner un poco de estacionalidad, que se prodia analizar mas a fondo.
##Puseto que con el seasonplot las grafcas presentan una variacion 
##similar en este periodo especificamente en los meses dabril a septiembre


##2
sum (((icats) - mean(icats))*(bimts - mean(bimts))) / (192-1)
cov(icats, bimts)
cov(icats, bimts) / (sd(icats)*sd(bimts))
cor(icats, bimts)

sum (((bimts) - mean(bimts))*(cets - mean(cets))) / (192-1)
cov(bimts, cets)
cov(bimts, cets) / (sd(bimts)*sd(cets))
cor(bimts, cets)

sum (((icats) - mean(icats))*(cets - mean(cets))) / (192-1)
cov(icats, cets)
cov(icats, cets) / (sd(icats)*sd(cets))
cor(icats, cets)
