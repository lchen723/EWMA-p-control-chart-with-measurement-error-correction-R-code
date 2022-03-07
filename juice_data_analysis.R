######example and chart######
library(qcr)
data = orangejuice
pt = data[,1]/data[,3]
IC = pt[31:54]
OC = pt[1:30]

hat_p0s = mean(IC)
lambda = 0.2 #0.05
pi = 0.95 #0.99
n = 50
pss = (hat_p0s-(1-pi))/(1-2*(1-pi))
v = hat_p0s*(1-hat_p0s)/(1-2*pi)^2
L = L1andARLf(lambda,pss,v,n,ARL)
(UCL = p+L$L1*sqrt(lambda*v/(2-lambda)/n))

######IC chart
lambda = 0.2
pi = 0.95
n = 50
UCL = 0.101
p = (hat_p0s-(1-pi))/(1-2*(1-pi))
IC2 = (IC-(1-pi))/(1-2*(1-pi))
IC2[IC2<0] = 0
E = ewmaRcpp(IC2,lambda,p)
color = rep('blue',length(IC))
color[(E-UCL)>0] = 'red'
plot(E,type = 'b',col = color,pch = 18,ylim = c(0.03,0.2),
     ylab = 'EWMA', xlab = 't',lty = 3)
abline(h = UCL,lty = 3,col = 'blue')
text(x = 23.2, y = 0.090, expression(paste('UCL','**', '=0.101')))

lambda = 0.2
pi = 0.99
n = 50
UCL = 0.142
p = (hat_p0s-(1-pi))/(1-2*(1-pi))
IC2 = (IC-(1-pi))/(1-2*(1-pi))
IC2[IC2<0] = 0
E = ewmaRcpp(IC2,lambda,p)
color = rep('orange',length(IC))
color[(E-UCL)>0] = 'red'
par(new = T)
plot(E,type = 'b',col = color,pch = 17,ylim = c(0.03,0.2),
     ylab = 'EWMA', xlab = 't',lty = 2)
abline(h = UCL,lty = 2,col = 'orange')
text(x = 23.2, y = 0.137, expression(paste('UCL','**', '=0.142')))

UCL = 0.151
lambda = 0.2
n = 50
p = hat_p0s
E = ewmaRcpp(IC,lambda,p)
color = rep('black',length(IC))
color[(E-UCL)>0] = 'red'
par(new = T)
plot(E,type = 'b',col = color,pch = 16,ylim = c(0.03,0.2),
     ylab = 'EWMA', xlab = 't',lty = 1)
abline(h = UCL,lty = 1)
text(x = 23.2, y = 0.156, expression(paste('UCL','*', '=0.151')))
text.legend = c(expression(paste(pi,' = 1.00   ')),
                expression(paste(pi,' = 0.99   ')),
                expression(paste(pi,' = 0.95   ')))
legend("topleft", legend = text.legend,
       pch = c(16,17,18),lty = c(1,2,3),col = c('black','orange','blue'))

######OC chart
UCL = 0.101
p = (hat_p0s-(1-pi))/(1-2*(1-pi))
OC2 = (OC-(1-pi))/(1-2*(1-pi))
OC2[OC2<0] = 0
E = ewmaRcpp(OC2,lambda,p)
color = rep('blue',length(OC))
color[(E-UCL)>0] = 'red'
plot(E,type = 'b',col = color,pch = 18,ylim = c(0.05,0.33),
     ylab = 'EWMA', xlab = 't',lty = 3)
abline(h = UCL,lty = 3,col = 'blue')
text(x = 28.2, y = 0.108, expression(paste('UCL','**', '=0.101')))

UCL = 0.142
p = (hat_p0s-(1-pi))/(1-2*(1-pi))
OC2 = (OC-(1-pi))/(1-2*(1-pi))
OC2[OC2<0] = 0
E = ewmaRcpp(OC2,lambda,p)
color = rep('orange',length(OC))
color[(E-UCL)>0] = 'red'
par(new = T)
plot(E,type = 'b',col = color,pch = 17,ylim = c(0.05,0.33),
     ylab = 'EWMA', xlab = 't' ,lty = 2)
abline(h = UCL,lty = 2,col = 'orange')
text(x = 28.2, y = 0.136, expression(paste('UCL','**', '=0.142')))

UCL = 0.151
p = hat_p0s
E = ewmaRcpp(OC,lambda,p)
color = rep('black',length(OC))
color[(E-UCL)>0] = 'red'
par(new = T)
plot(E,type = 'b',col = color,pch = 16,ylim = c(0.05,0.33),
     ylab = 'EWMA', xlab = 't',lty = 1)
abline(h = UCL,lty = 1)
text(x = 28.2, y = 0.158, expression(paste('UCL','*', '=0.151')))
legend("topleft", legend = text.legend,
       pch = c(16,17,18),lty = c(1,2,3),col = c('black','orange','blue'))
txt = as.character(round(UCL,3))
text(x = 28.2, y = 0.144, paste('UCL=', txt))