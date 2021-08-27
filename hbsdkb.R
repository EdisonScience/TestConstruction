setwd("D:/")
library(readxl)
library(psych)
Datos<-read_excel("rtas.xlsx")
Datos<-data.frame(Datos)

View(Datos)
describe(Datos)
options(max.print = 10000)
#analisis de correlacion
cor<-corr.test(Datos)
write.csv2(corpearson, "namefile.csv") #para guardar un df cmo csv
corpearson<-data.frame(cor[["r"]])
cor.plot(A)
library(ggplot2)
library(ggcorrplot)
corrplot()#?????????????
ggcorrplot(corpearson,hc.order = F,type = "lower",lab=T)
#analisis de bartlett
bartlett.test(Datos) #debe ser cercano a cero
cortest.bartlett(Datos) #<-----ES ESTE
#analisis KMO debe ser un dataframe limpio
KMO(Datos[1:37,]) #debe der mayor a .8
#simulacion de datos para AF con minimos residual
fa.parallel(fa = "both", Datos, cor = "cor",fm="minres", SMC = T) #grafico de gradientes
#ANALISIS FACTORIAL
AFE<-fa(Datos, nfactors = 4, cor = "cor", scores = "Bartlett", rotate = "oblimin") #AF Exploratorio
AFE #buscar proportion var y with factor corr of (debe ser bajo)
AFE$weights
AFE$e.values
#######
factanal(Datos[1:37,],factors=4, rotation="varimax",scores = "Bartlett") #no porque no está bien 
#correlacionados los datos, y no es una matriz invertible
fa.diagram(AFE,rsize=.05) #diagrama factorial
nfactors(Datos, n=4,rotate ="oblimin") #grafica..
#SRMR debe ser menor a .08
scree(Datos) #grafico de sedimentacion
princomp(Datos[1:37,],scores=T,cor=T)%>%plot(main="Análisis de componentes principales")
#ESTIMACION DE PUNTAJES FACTORIALES
factor.scores(Datos, AFE) #por persona
library(CTT)
library(parameters)
#ALGUNOS DESCRIPTIVOS
reliability(Datos[1:37,])#???????
cons<-itemAnalysis(Datos) #debe ser un dataframe
cons #IMPORTANTE
cons$itemReport #discriminacion y otros
cons[["itemReport"]][["pBis"]]#sirve para la corelacion bispuntualstem(Datos$item1) #diagrama de arbol ¿convertir en factor?
cons[["alpha"]]#alpha
alpha(Datos)
library(likert)
library(dplyr)
#ESTADISTICOS MAS ESPECIFICOS
Datos[,37]<-NULL

Datoz<-Datos%>%
  mutate_if(is.numeric,as.factor)#convierto en factor
rlikert<-likert(Datoz)
plot(rlikert) #simple
plot(likert(Datoz), centered=T,group.order = colnames( rlikert$items ),
     legend.position = "right" ) +
  theme( axis.text.x = element_text( size = 12 ),
         axis.text.y = element_text( size = 10, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) ) 
plot(rlikert, type = "density")

#Rasch politómicos
library(eRm)
RM() #solo sirve para dicotomicos
rtas0<-read_excel("rtas0.xlsx")
rasch<-RSM(rtas0)#debe empezar con 0 (cero) la db
coef(rasch)
ppar<-person.parameter(rasch)
ppar$theta.table
itemfit(ppar)
personfit(ppar)
#graficos rasch
plotPImap(rasch)#nooooooo
plotPWmap(rasch,pmap = F,imap = T) #tampoco
plotICC(rasch,item.subset=5:8,mplot = T,legpos="left")
plotjointICC(rasch) #solo sirve con dicotomicos
##########
det(cor(Datos, use = "pairwise.complete.obs"))#determinante de corrr
###indice correlacion item-prueba
library(corrr)
Datos$score<-rowMeans(Datos)
item_total<-Datos %>% correlate() %>% focus(score)
item_total
mean(item_total$score) #ÉSTE
item_total%>%
  ggplot(aes(x=score)) +
    geom_histogram(bins=10,alpha=.5)+
    geom_vline(xintercept = mean(item_total$score),color="red")+
    xlab("Media correlación item-prueba")+
    theme_bw()
#analisis de confiablidad por mitades
splitHalf(Datos)

#Matriz anti -imagen (objeto A) 
invrcor <- solve(corpearson)
A <- matrix(1,nrow(invrcor),ncol(invrcor))
for (i in 1:nrow(invrcor)){
  for (j in (i+1):ncol(invrcor)){
    A[i,j] <- invrcor[i,j]/sqrt (invrcor [i,i]*invrcor [j,j])
    A[j,i] <- A[i,j]
  }
}
colnames(A) <- colnames (Datos)
rownames(A) <- colnames (Datos)
print(A)
