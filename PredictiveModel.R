########################## LIMPIEZA DE ENTORNO Y CARGA DE LIBRERIAS #################################
rm(list=ls())
library(readr)
library(plotly)
library(readr)
library(randomForest)
##################################### CARGA DE DATASET ############################################
train <- read_csv("D:/Datathon18/Dataset_Salesforce_Predictive_Modelling_TRAIN.txt")
train = as.data.frame(train)
################################## ANÁLISIS EXPLORATORIO #########################################
###########################           DATOS DE CONSUMO          ##################################
datosimporte = train[,c(2:18,89)]
# En primer lugar realizamos una división del mercado, para ver las franjas del poder adquisitivo
mercados = kmeans(datosimporte, 3, nstart = 4)
cluster = mercados$cluster
datosimporte$Mercado = cluster
datosimporte$Mercado = as.factor(datosimporte$Mercado)

mercado1 = datosimporte$Poder_Adquisitivo[datosimporte$Mercado==1,]
mercado2 = datosimporte$Poder_Adquisitivo[datosimporte$Mercado==2,]
mercado3 = datosimporte$Poder_Adquisitivo[datosimporte$Mercado==3,]

# Observamos las diferencias entre los diferentes estamentos en cuanto al consumo
for (i in 1:17) {
  print(paste0("Producto ",i))
  print(summary(mercado1[,i]))
  print(summary(mercado2[,i]))
  print(summary(mercado3[,i]))
}

# Ploteamos los consumos realizados para entender visualmente las variables
for (i in 1:17){
  VariableName = paste0("producto",i)
  plot = plot_ly(x = datosimporte$Mercado,y = (datosimporte[,i]),type = "box")
  assign(VariableName,plot)
}
subplot(producto1,producto5,producto13,producto15,producto16,producto17,nrows = 6)
# Realizamos un análisis de la varianza de cada franja de poder adquisitivo con respecto a los consumos en cada tarjeta
ANOVA = function(numProducto, datosimporte){
  datos = datosimporte[,c(numProducto,ncol(datosimporte))]
  names(datos) = c("var1","var2")
  ANOVAmodel = aov(data = datos, var1~var2)
  Tukmodel = TukeyHSD(ANOVAmodel)
  Tukmodel = Tukmodel$var2
  diferencia1_2 = Tukmodel[1,4]
  diferencia1_3 = Tukmodel[2,4]
  diferencia2_3 = Tukmodel[3,4]
  ANOVAmodel = summary(ANOVAmodel)[[1]]
  p_valor = ANOVAmodel$`Pr(>F)`[1]
  if(diferencia1_2 >0.05){print("1-2 iguales")}else{print("1-2 distintos")}
  if(diferencia1_3 >0.05){print("1-3 iguales")}else{print("1-3 distintos")}
  if(diferencia2_3 >0.05){print("2-3 iguales")}else{print("2-3 distintos")}
  if(p_valor >0.05){print("Desecha la variable")}else{print("Hay diferencias notables en al menos un grupo")}
}
for(i in 1:17){
  print(paste0("Producto ",i))
  ANOVA(i,datosimporte)
}
###########################       DATOS SOCIO-DEMOGRÁFICOS       ##################################
datossocio = train[,84:89]
datossocio$Socio_Demo_01 = as.factor(datossocio$Socio_Demo_01)
datossocio$Socio_Demo_02 = as.factor(datossocio$Socio_Demo_02)
summary(datossocio$Socio_Demo_01)
summary(datossocio$Socio_Demo_02)
summary(datossocio$Socio_Demo_03)
summary(datossocio$Socio_Demo_04)
summary(datossocio$Socio_Demo_05)
socio1 = plot_ly(data = datossocio,x = ~Socio_Demo_01,y = ~Poder_Adquisitivo,type = "scatter",mode = "markers")
socio2 = plot_ly(data = datossocio,x = ~Socio_Demo_02,y = ~Poder_Adquisitivo,type = "box")
socio3 = plot_ly(data = datossocio,x = ~Socio_Demo_03,y = ~Poder_Adquisitivo,type = "scatter",mode = "markers")
socio4 = plot_ly(data = datossocio,x = ~Socio_Demo_04,y = ~Poder_Adquisitivo,type = "box", color = ~Socio_Demo_02)%>%
  layout(boxmode = "group")
socio5 = plot_ly(data = datossocio,x = ~Socio_Demo_05,y = ~Poder_Adquisitivo,type = "scatter", mode = "markers")

# Análisis de la varianza en la variable demográfica 2 arroja diferencias significativas
aovsoc2 = aov(data = datossocio, Poder_Adquisitivo~Socio_Demo_02)

datossocio

socio1 = plot_ly(data = train,x = ~Imp_Sal_21,y = ~Poder_Adquisitivo,type = "scatter",mode = "markers", color = ~Imp_Sal_10)

###########################       DATOS NUMERO DE OPERACIONES       ##################################
# Realizamos la matriz de correlaciones entre saldos ,numero de operaciones y poder adquisitivo
operaciones_saldo = train[,c(19:39,64:83,89)]
correlacion = rcorr(as.matrix(operaciones_saldo))
MatrizCorrelacion = correlacion$r
corrplot(MatrizCorrelacion, type="lower")
# De ella podemos observar que están relacionados los números de operaciones muy directamente,
# los intentamos agrupar con los saldos relacionados y generar una nueva variable de saldo por operacion, 
# que resulta no tener mucha relación.
plot_ly(data = operaciones_saldo,x = ~Imp_Saldo_Key,y = ~Poder_Adquisitivo,type = "scatter",mode = "markers")
operaciones_saldo$Sal_Oper_15 = operaciones_saldo$Imp_Sal_15/operaciones_saldo$Num_Oper_08
operaciones_saldo$Sal_Oper_15[which(is.na(operaciones_saldo$Sal_Oper_15) | is.infinite(operaciones_saldo$Sal_Oper_15))]=0
operaciones_saldo$Sal_Oper_16 = operaciones_saldo$Imp_Sal_16/operaciones_saldo$Num_Oper_08
operaciones_saldo$Sal_Oper_16[which(is.na(operaciones_saldo$Sal_Oper_16) | is.infinite(operaciones_saldo$Sal_Oper_16))]=0
operaciones_saldo$Sal_Oper_17 = operaciones_saldo$Imp_Sal_17/operaciones_saldo$Num_Oper_08
operaciones_saldo$Sal_Oper_17[which(is.na(operaciones_saldo$Sal_Oper_17) | is.infinite(operaciones_saldo$Sal_Oper_17))]=0
operaciones_saldo$Sal_Total = operaciones_saldo$Imp_Sal_15 + operaciones_saldo$Imp_Sal_17 + operaciones_saldo$Imp_Sal_16
operaciones_saldo$Imp_Saldo_Key = operaciones_saldo$Imp_Sal_02 + operaciones_saldo$Imp_Sal_08 + operaciones_saldo$Imp_Sal_09 +operaciones_saldo$Imp_Sal_19+operaciones_saldo$Imp_Sal_21
operaciones_saldo$Imp_Cons_Key = train$Imp_Cons_01 + train$Imp_Cons_04 + train$Imp_Cons_06 +train$Imp_Cons_11+train$Imp_Cons_12+train$Imp_Cons_15
operaciones_saldo$Sal_Total = operaciones_saldo$Imp_Sal_15 + operaciones_saldo$Imp_Sal_17 + operaciones_saldo$Imp_Sal_16

operaciones_saldo$Imp_Saldo_Total = operaciones_saldo$Imp_Sal_01+operaciones_saldo$Imp_Sal_02+operaciones_saldo$Imp_Sal_03+operaciones_saldo$Imp_Sal_04+operaciones_saldo$Imp_Sal_05+operaciones_saldo$Imp_Sal_06+operaciones_saldo$Imp_Sal_07+operaciones_saldo$Imp_Sal_08+operaciones_saldo$Imp_Sal_09+operaciones_saldo$Imp_Sal_10+operaciones_saldo$Imp_Sal_11+operaciones_saldo$Imp_Sal_12+operaciones_saldo$Imp_Sal_13+operaciones_saldo$Imp_Sal_14+operaciones_saldo$Imp_Sal_15+operaciones_saldo$Imp_Sal_16+operaciones_saldo$Imp_Sal_17+operaciones_saldo$Imp_Sal_18+operaciones_saldo$Imp_Sal_19+operaciones_saldo$Imp_Sal_20+operaciones_saldo$Imp_Sal_21
operaciones_saldo$Num_Oper_Total = operaciones_saldo$Num_Oper_01+operaciones_saldo$Num_Oper_02+operaciones_saldo$Num_Oper_03+operaciones_saldo$Num_Oper_04+operaciones_saldo$Num_Oper_05+operaciones_saldo$Num_Oper_06+operaciones_saldo$Num_Oper_07+operaciones_saldo$Num_Oper_08+operaciones_saldo$Num_Oper_09+operaciones_saldo$Num_Oper_10+operaciones_saldo$Num_Oper_11+operaciones_saldo$Num_Oper_12+operaciones_saldo$Num_Oper_13+operaciones_saldo$Num_Oper_14+operaciones_saldo$Num_Oper_15+operaciones_saldo$Num_Oper_16+operaciones_saldo$Num_Oper_17+operaciones_saldo$Num_Oper_18+operaciones_saldo$Num_Oper_19+operaciones_saldo$Num_Oper_20
operaciones_saldo$Saldo_Oper_Total = operaciones_saldo$Imp_Saldo_Total/operaciones_saldo$Num_Oper_Total
operaciones_saldo$Saldo_Oper_Total[which(is.na(operaciones_saldo$Saldo_Oper_Total) | is.infinite(operaciones_saldo$Saldo_Oper_Total))]=0
operaciones_saldo$Ind_Key = train$Ind_Prod_08+train$Ind_Prod_11+train$Ind_Prod_16+train$Ind_Prod_18+train$Ind_Prod_24

operaciones_saldo$Imp_Saldo_Key1 = operaciones_saldo$Imp_Sal_08+operaciones_saldo$Imp_Sal_09+operaciones_saldo$Imp_Sal_19+operaciones_saldo$Imp_Sal_21
cor(operaciones_saldo$Imp_Saldo_Key1,train$Poder_Adquisitivo)# Hay una alta correlacion entre ambas

############################## DATOS DE TENENCIA DE PRODUCTOS #####################################
train[,40:63] = as.factor(train[,40:63])
# Dividimos los datos en las franjas obtenidas inicialmente
train$Rico_Pobre = 0
train$Rico_Pobre[train$Poder_Adquisitivo>675000] = 2
train$Rico_Pobre[train$Poder_Adquisitivo<=675000 & train$Poder_Adquisitivo>94000] = 1

# plot_ly(type="box", x=train$Rico_Pobre, y=as.factor(train$Ind_Prod_01), color=as.factor(train$Ind_Prod_01))%>%
#   layout(boxmode="group")
# Vemos el porcentaje de gente de cada franja que tiene o no tiene productos (el tuvo tres meses no afecta demaiado)
PorcentajeDeTenencia = function(datos,mercado){
  Ricos = datos[mercado==2]
  Medio = datos[mercado==1]
  Pobres = datos[mercado == 0]
  print("Ricos")
  CalculaPorcentaje(Ricos)
  print("Medio")
  CalculaPorcentaje(Medio)
  print("Pobres")
  CalculaPorcentaje(Pobres)
}

CalculaPorcentaje = function(Ricos){
  NoTiene = sum(Ricos==0)/length(Ricos)*100
  Tiene = sum(Ricos==1)/length(Ricos)*100
  Tuvo = sum(Ricos==2)/length(Ricos)*100
  print(NoTiene)
  print(Tiene)
  print(Tuvo)
}
for (i in 40:63){
  print(paste("Producto",i-39))
  PorcentajeDeTenencia(train[,i],train$Rico_Pobre)
}
# Modificamos la tenencia a 0 - No tiene y 1 - Tiene (El 2 no afecta demasiado)
for (i in 40:63){
  train[train[,i]==2,i]=1
}
# Creamos un indice que representa el numero de productos "ricos" (que tienen los ricos pero apenas los pobres)
indice = 0
for (i in c(39+1,39+4,39+13,39+15,39+17,39+22,39+23)){
  indice = indice+train[,i]
}
train$indice = indice
# Este otro indice representa el numero de productos "medios" (que tienen los medios, pero apenas pobres)
indice = 0
for (i in c(39+2,39+10,39+11,39+14,39+16,39+21)){
  indice = indice+train[,i]
}
train$indice2 = indice
train$indice2[train$indice==5]=8
train$indice2[train$indice==4]=7
train$indice2[train$indice==3]=6
train$indice2[train$indice==2]=5
train$indice2[train$indice==1]=4
plot_ly(type='box',x=train$indice2,y=train$Poder_Adquisitivo)

# Variable suma de consumos
suma=0 
for (i in c(2:18)){suma=suma+train[,i]}
train$total_cons =suma
# Variable suma de saldos clave (los saldos 8,9,19,21)
suma=0 
for (i in c(26:27,37,39)){suma=suma+train[,i]}
train$total_sal =suma
# Agrupamos el dataset de train con las siguientes variables
# Ind_Cons_06
# Socio_Demo_02 
# Total_Cons
# Total_Sal
# Indice2
# Poder_Adquisitivo
train = train[,c(7,85,90:92,89)]
muestras = sample(nrow(train), 0.75*nrow(train))
# Dividimos el training en train y cross-validation para escoger el numero de arboles
traindef = train[muestras,]
crossval = train[-muestras,]

##################### ENTRENAMIENTO DEL MODELO CON SELECCIÓN DE PARÁMETROS DEL ALGORITMO #############
# Leemos el traindef guardado anteriormente
traindef = read_delim("train_def.csv",";", escape_double = FALSE, trim_ws = TRUE)
traindef$Socio_Demo_02 = as.factor(traindef$Socio_Demo_02)
traindef$indice2 = as.factor(traindef$indice2)

muestras = sample(nrow(traindef), 0.3*nrow(traindef))
traindef2 = traindef[muestras,]
for(numberfeatures in c(2,3)){
  for(numbertrees in c(500,1000,1500)){
    modelo = randomForest(data = traindef2, Poder_Adquisitivo~., ntrees = numbertrees, mtry = numberfeatures)
  save(modelo,file=paste0("modelo_",numberfeatures,"_",numbertrees,".RData"))
} 
}
##################### EVALUACION DEL MEJOR MODELO CON EL DATASET DE CROSS-VALIDATION #############
cv = read_delim("crossval.csv",";", escape_double = FALSE, trim_ws = TRUE)
cv$Socio_Demo_02 = as.factor(cv$Socio_Demo_02)
cv$indice2 = as.factor(cv$indice2)
#Cargamos el modelo
load("modelo_2_500.RData")
pred = predict(modelo, newdata = cv)
real = cv$Poder_Adquisitivo
mse = sum((pred - real)^2)/length(real)
rsquared = 1-(sum((pred - real)^2)/sum((real - mean(real))^2))
mae = sum(abs(pred - real))/length(real)
print(paste0("MSE = ",mse," MAE = ",mae," R2 = ",rsquared))
##################### EVALUACION DEL EN EL CONJUNTO DE TEST #############
test = read_csv("D:/Datathon18/Dataset_Salesforce_Predictive_Modelling_TEST.txt")
test = as.data.frame(test)
# Ingenieria inversa para transformar las variables del test
indice = 0
for (i in c(39+1,39+4,39+13,39+15,39+17,39+22,39+23)){
  indice = indice+test[,i]
}
test$indice = indice
indice = 0
for (i in c(39+2,39+10,39+11,39+14,39+16,39+21)){
  indice = indice+test[,i]
}
test$indice2 = indice
test$indice2[test$indice==5]=8
test$indice2[test$indice==4]=7
test$indice2[test$indice==3]=6
test$indice2[test$indice==2]=5
test$indice2[test$indice==1]=4

suma=0 
for (i in c(2:18)){suma=suma+test[,i]}
test$total_cons =suma
suma=0 
for (i in c(26:27,37,39)){suma=suma+test[,i]}
test$total_sal =suma
test = test[,c(7,85,90:92)]
test$Socio_Demo_02 = as.factor(test$Socio_Demo_02)
test$indice2 = as.factor(test$indice2)
test$indice2[test$indice2==1|test$indice2==2]=0
test$indice2[test$indice2==3]=4
test$indice2 = as.factor(test$indice2)
test$indice2 = factor(test$indice2, labels = c(0,4,5,6,7,8))
#Cargamos el modelo
load("modelo_2_500.RData")
PA_Est = as.numeric(predict(modelo, newdata = test))
ID_Customer = test[,1]
DATA = data.frame(ID_Customer,PA_Est)
write.csv(DATA,file="Test_Mission.txt",sep=",")
