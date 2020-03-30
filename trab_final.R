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
#0.7805 <- 38
##0.5505 <- 50
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





attach(data_exp1)


#-------------------------#
# Grafico de interacciones#
#-------------------------#
win.graph()
mat <- rbind(c(1,1,1,2,2,2,3,3,3),c(4,4,4,5,5,5,6,6,6))

layout(mat,widths = rep.int(1,ncol(mat)), heights = rep.int(1,nrow(mat)))



interaction.plot(A[C==1],B[C==1],Y[C==1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=0.7,cex.lab=0.7,legend=F)
legend("bottomright",legend=c("B[C==1]=-1","B[C==1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=0.7)


interaction.plot(A[B==1],C[B==1],Y[B==1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=0.7,cex.lab=0.7,legend=F)
legend("topleft",legend=c("C[B==1]=-1","C[B==1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=0.7)


interaction.plot(B[A==1],C[A==1],Y[A==1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=0.7,cex.lab=0.7,legend=F)
legend("topright",legend=c("C[A==1]=-1","C[A==1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=0.7)



interaction.plot(A[C==-1],B[C==-1],Y[C==-1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=0.7,cex.lab=0.7,legend=F)
legend("topleft",legend=c("B[C==-1]=-1","B[C==-1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=0.7)

interaction.plot(A[B==-1],C[B==-1],Y[B==-1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=0.7,cex.lab=0.7,legend=F)
legend("topleft",legend=c("C[B==-1]=-1","C[B==-1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=0.7)




interaction.plot(B[A==-1],C[A==-1],Y[A==-1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=0.7,cex.lab=0.7,legend=F)
legend("topright",legend=c("C[A==-1]=-1","C[A==-1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=0.7)

# Interacciones dobles 

#GRÁFICOS DE INTERACCIONES DOBLES
win.graph()
interaction.plot(A,B,Y,type="b",pch=c(1,2),col=c("black","red"),
                 lwd=4,cex=2,cex.lab=1.5,legend=F)
legend("topleft",legend=c("B=-1","B=+1"),col=1:2,pch=1:2,
       lwd=2,lty=c(2,1),bty="n",cex=2)
win.graph()
interaction.plot(A,C,Y,type="b",pch=c(1,2),col=c("black","red"),
                 lwd=4,cex=2,cex.lab=1.5,legend=F)
legend("topleft",legend=c("C=-1","C=+1"),col=1:2,pch=1:2,
       lwd=2,lty=c(2,1),bty="n",cex=2)
win.graph()
interaction.plot(B,C,Y,type="b",pch=c(1,2),col=c("black","red"),
                 lwd=4,cex=2,cex.lab=1.5,legend=F)
legend("topright",legend=c("C=-1","C=+1"),col=1:2,pch=1:2,
       lwd=2,lty=c(2,1),bty="n",cex=2)

#Gráficos a nivel marginal 
mediaA = sapply(split(Y, A), mean)
mediaB = sapply(split(Y, B), mean)
mediaC = sapply(split(Y, C), mean)

win.graph()
boxplot(Y~A,boxwex=0.4,xlab="A",ylab="Y")
lines(1:2, mediaA, col=2, lty=2, type = 'b', pch=19)
win.graph()
boxplot(Y~B,boxwex=0.4,xlab="B",ylab="Y")
lines(1:2, mediaB, col=2, lty=2, type = 'b', pch=19)
win.graph()
boxplot(Y~C,boxwex=0.4,xlab="C",ylab="Y")
lines(1:2, mediaC, col=2, lty=2, type = 'b', pch=19)





#modelos 


mod1 <- lm( Y ~ A*B*C,data = data_exp)
summary(mod1)
# Datos con innputacion de datos atipicos



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


win.graph()
mat <- rbind(c(1,1,1,2,2,2,3,3,3),c(4,4,4,5,5,5,6,6,6))

layout(mat,widths = rep.int(1,ncol(mat)), heights = rep.int(1,nrow(mat)))



interaction.plot(A[C==1],B[C==1],Y[C==1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=0.7,cex.lab=0.7,legend=F)
legend("bottomright",legend=c("B[C==1]=-1","B[C==1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=0.7)


interaction.plot(A[B==1],C[B==1],Y[B==1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=0.7,cex.lab=0.7,legend=F)
legend("topleft",legend=c("C[B==1]=-1","C[B==1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=0.7)


interaction.plot(B[A==1],C[A==1],Y[A==1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=0.7,cex.lab=0.7,legend=F)
legend("topright",legend=c("C[A==1]=-1","C[A==1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=0.7)



interaction.plot(A[C==-1],B[C==-1],Y[C==-1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=0.7,cex.lab=0.7,legend=F)
legend("topleft",legend=c("B[C==-1]=-1","B[C==-1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=0.7)

interaction.plot(A[B==-1],C[B==-1],Y[B==-1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=0.7,cex.lab=0.7,legend=F)
legend("topleft",legend=c("C[B==-1]=-1","C[B==-1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=0.7)




interaction.plot(B[A==-1],C[A==-1],Y[A==-1],type="b",pch=c(1,2),col=c("black","red"),
                 lwd=2,cex=0.7,cex.lab=0.7,legend=F)
legend("topright",legend=c("C[A==-1]=-1","C[A==-1]=+1"),col=1:2,pch=1:2,lwd=2,
       lty=c(2,1),bty="n",cex=0.7)

