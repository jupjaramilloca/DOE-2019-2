rm(list=ls(all=TRUE))
library(gmodels)
library(daewr)
library(agricolae)
library(lsmeans)


#----------------------#
#Plan de aleatorización#
#----------------------#

ni=3 #Replicas por cada tratamiento

num_trat <- 8 #Número de tratamientos 

#Repetir cada tratamiento 3 veces

tratamientos = factor(rep(c('-1','a','b','c','ab', 'ac', 'bc', 'abc'), each=ni))
tratamientos

N = ni*num_trat #Total de unidades experimentas 

#Sembrando una semilla 
set.seed(12356)
fac = sample(tratamientos, N) #Selección de los tratamientos aleatoriamente(m.a.s) 
UE = 1:N #Enumerar las unidades experimentales

corrida = data.frame('Unidad'=UE, 'Tratamientos'=fac)
corrida

#------------#
# Resultados #
#------------#

data_exp = data.frame(scan(what=list(A=0,B=0,C=0, Y=0)))
-1 -1 -1 0.0905
1 -1 -1 0.7805 
-1 1 -1 0.0505
1 1 -1 0.0305
-1 -1 1 0.2805
1 -1 1 0.1005
-1 1 1 0.0705
1 1 1 0.1605
-1 -1 -1 0.1505
1 -1 -1 0.2405
-1 1 -1 0.0205
1 1 -1 0.0005
-1 -1 1 0.1405
1 -1 1 0.5501 
-1 1 1 0.0505
1 1 1 0.1805
-1 -1 -1 0.0805
1 -1 -1 0.1805
-1 1 -1 0.0405
1 1 -1 0.0505
-1 -1 1 0.2805
1 -1 1 0.1405
-1 1 1 0.0205
1 1 1 0.2005

data_exp


attach(data_exp)


#-------------------------#
# Grafico de interacciones#
#-------------------------#
win.graph()
matriz <- rbind(c(1,1,1,2,2,2,3,3,3),c(4,4,4,5,5,5,6,6,6))

layout(matriz, widths = rep.int(1, ncol(mat)),
       heights = rep.int(1, nrow(mat)), respect = FALSE)

interaction.plot(A[C==1],B[C==1],Y[C==1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=1,cex.lab=1,legend=F,main="(AB|C=1)",cex.main=1.5)
legend("bottomright",legend=c("B[C==1]=-1","B[C==1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=1.5)


interaction.plot(A[B==1],C[B==1],Y[B==1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=1.5,cex.lab=1.5,legend=F,main="(AC|B=1)",cex.main=1.5)
legend("topleft",legend=c("C[B==1]=-1","C[B==1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=1.5)


interaction.plot(B[A==1],C[A==1],Y[A==1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=1.5,cex.lab=1.5,legend=F,main="(BC|A=1)",cex.main=1.5)
legend("topright",legend=c("C[A==1]=-1","C[A==1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=1.5)



interaction.plot(A[C==-1],B[C==-1],Y[C==-1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=1.5,cex.lab=1.5,legend=F,main="(AB|C=-1)",cex.main=1.5)
legend("topleft",legend=c("B[C==-1]=-1","B[C==-1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=1.5)

interaction.plot(A[B==-1],C[B==-1],Y[B==-1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=1.5,cex.lab=1.5,legend=F,main="(AC|B=-1)",cex.main=1.5)
legend("topleft",legend=c("C[B==-1]=-1","C[B==-1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=1.5)


interaction.plot(B[A==-1],C[A==-1],Y[A==-1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=1.5,cex.lab=1.5,legend=F,main="(BC|A=-1)",cex.main=1.5)
legend("topright",legend=c("C[A==-1]=-1","C[A==-1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=1.5)

# Interacciones dobles 

#GRÁFICOS DE INTERACCIONES DOBLES
win.graph()

layout(matrix(data = c(1,2,3),nrow = 1,ncol = 3))

interaction.plot(A,B,Y,type="b",pch=c(1,2),col=c("black","red"),
                 lwd=4,cex=2,cex.lab=1.5,legend=F,main="Interacción AB",cex.main=1.5)
legend("topleft",legend=c("B=-1","B=+1"),col=1:2,pch=1:2,
       lwd=2,lty=c(2,1),bty="n",cex=2)

interaction.plot(A,C,Y,type="b",pch=c(1,2),col=c("black","red"),
                 lwd=4,cex=2,cex.lab=1.5,legend=F,main="Interacción AC",cex.main=1.5)
legend("topleft",legend=c("C=-1","C=+1"),col=1:2,pch=1:2,
       lwd=2,lty=c(2,1),bty="n",cex=2)

interaction.plot(B,C,Y,type="b",pch=c(1,2),col=c("black","red"),
                 lwd=4,cex=2,cex.lab=1.5,legend=F,main="Interacción BC",cex.main=1.5)
legend("topright",legend=c("C=-1","C=+1"),col=1:2,pch=1:2,
       lwd=2,lty=c(2,1),bty="n",cex=2)

#Gráficos a nivel marginal 
mediaA = sapply(split(Y, A), mean)
mediaB = sapply(split(Y, B), mean)
mediaC = sapply(split(Y, C), mean)

win.graph()

par(mfrow=c(1,3))

boxplot(Y~A,boxwex=0.4,xlab="A",ylab="Y")
lines(1:2, mediaA, col=2, lty=2, type = 'b', pch=19)

boxplot(Y~B,boxwex=0.4,xlab="B",ylab="Y")
lines(1:2, mediaB, col=2, lty=2, type = 'b', pch=19)

boxplot(Y~C,boxwex=0.4,xlab="C",ylab="Y")
lines(1:2, mediaC, col=2, lty=2, type = 'b', pch=19)





#modelos 


mod1 <- lm( Y ~ A*B*C,data = data_exp)
summary(mod1)
# Datos con innputacion de datos atipicos



#---------------------------#
# Datos con imputaciones    #
#---------------------------#
rm(list=ls(all=TRUE))



data_exp1 = data.frame(scan(what=list(A=0,B=0,C=0, Y=0)))
-1 -1 -1 0.0905
1 -1 -1 0.2105 
-1 1 -1 0.0505
1 1 -1 0.0305
-1 -1 1 0.2805
1 -1 1 0.1005
-1 1 1 0.0705
1 1 1 0.1605
-1 -1 -1 0.1505
1 -1 -1 0.2405
-1 1 -1 0.0205
1 1 -1 0.0005
-1 -1 1 0.1405
1 -1 1 0.1205
-1 1 1 0.0505
1 1 1 0.1805
-1 -1 -1 0.0805
1 -1 -1 0.1805
-1 1 -1 0.0405
1 1 -1 0.0505
-1 -1 1 0.2805
1 -1 1 0.1405
-1 1 1 0.0205
1 1 1 0.2005

attach(data_exp1)

win.graph()
matriz <- rbind(c(1,1,1,2,2,2,3,3,3),c(4,4,4,5,5,5,6,6,6))

layout(matriz, widths = rep.int(1, ncol(matriz)),
       heights = rep.int(1, nrow(matriz)), respect = FALSE)

interaction.plot(A[C==1],B[C==1],Y[C==1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=1,cex.lab=1,legend=F,main="(AB|C=1)",cex.main=1.5)
legend("bottomright",legend=c("B[C==1]=-1","B[C==1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=1.5)


interaction.plot(A[B==1],C[B==1],Y[B==1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=1.5,cex.lab=1.5,legend=F,main="(AC|B=1)",cex.main=1.5)
legend("topleft",legend=c("C[B==1]=-1","C[B==1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=1.5)


interaction.plot(B[A==1],C[A==1],Y[A==1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=1.5,cex.lab=1.5,legend=F,main="(BC|A=1)",cex.main=1.5)
legend("topright",legend=c("C[A==1]=-1","C[A==1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=1.5)



interaction.plot(A[C==-1],B[C==-1],Y[C==-1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=1.5,cex.lab=1.5,legend=F,main="(AB|C=-1)",cex.main=1.5)
legend("topleft",legend=c("B[C==-1]=-1","B[C==-1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=1.5)

interaction.plot(A[B==-1],C[B==-1],Y[B==-1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=1.5,cex.lab=1.5,legend=F,main="(AC|B=-1)",cex.main=1.5)
legend("topleft",legend=c("C[B==-1]=-1","C[B==-1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=1.5)


interaction.plot(B[A==-1],C[A==-1],Y[A==-1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=1.5,cex.lab=1.5,legend=F,main="(BC|A=-1)",cex.main=1.5)
legend("topright",legend=c("C[A==-1]=-1","C[A==-1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=1.5)




#GRÁFICOS DE INTERACCIONES DOBLES
win.graph()

layout(matrix(data = c(1,2,3),nrow = 1,ncol = 3))

interaction.plot(A,B,Y,type="b",pch=c(1,2),col=c("black","red"),
                 lwd=4,cex=2,cex.lab=1.5,legend=F,main="Interacción AB",cex.main=1.5)
legend("bottomright",legend=c("B=-1","B=+1"),col=1:2,pch=1:2,
       lwd=2,lty=c(2,1),bty="n",cex=2)

interaction.plot(A,C,Y,type="b",pch=c(1,2),col=c("black","red"),
                 lwd=4,cex=2,cex.lab=1.5,legend=F,main="Interacción AC",cex.main=1.5)
legend("topleft",legend=c("C=-1","C=+1"),col=1:2,pch=1:2,
       lwd=2,lty=c(2,1),bty="n",cex=2)

interaction.plot(B,C,Y,type="b",pch=c(1,2),col=c("black","red"),
                 lwd=4,cex=2,cex.lab=1.5,legend=F,main="Interacción BC",cex.main=1.5)
legend("topright",legend=c("C=-1","C=+1"),col=1:2,pch=1:2,
       lwd=2,lty=c(2,1),bty="n",cex=2)

#Gráficos a nivel marginal 
mediaA = sapply(split(Y, A), mean)
mediaB = sapply(split(Y, B), mean)
mediaC = sapply(split(Y, C), mean)

win.graph()

par(mfrow=c(1,3))

boxplot(Y~A,boxwex=0.4,xlab="A",ylab="Y")
lines(1:2, mediaA, col=2, lty=2, type = 'b', pch=19)

boxplot(Y~B,boxwex=0.4,xlab="B",ylab="Y")
lines(1:2, mediaB, col=2, lty=2, type = 'b', pch=19)

boxplot(Y~C,boxwex=0.4,xlab="C",ylab="Y")
lines(1:2, mediaC, col=2, lty=2, type = 'b', pch=19)

