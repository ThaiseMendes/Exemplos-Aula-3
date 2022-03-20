### Conjunto de dados Saudi Arabia Real Estate (AQAR)
setwd('C:\\Gustavo\\Consultorias\\Previdência\\Aulas\\Aula 3')
library(tidyverse)
dados_orig<-read_csv('Saudi Arabia real estate.csv',locale = locale(encoding = "UTF-8"))

# الرياض    - Al Riyadh
# جدة       - Jeddah
# الدمام    - Dammam
# الخبر     - Al-Khobar

dados_orig$city[which(dados_orig$city=='الرياض')]<-'Al Riyadh'
dados_orig$city[which(dados_orig$city=='جدة')]<-'Jeddah'
dados_orig$city[which(dados_orig$city=='الدمام')]<-'Dammam'
dados_orig$city[which(dados_orig$city=='الخبر')]<-'Al Khobar'
table(dados_orig$city)

## Vamos estudar como o preço do aluguel de cada imóvel ('price') se relaciona com as variáveis 'city' e  'size'.

dados<-subset(dados_orig,select = c('price','city','size'))
attach(dados)
library(pastecs)
dados_num<-dados[,-which(names(dados) %in% c('city'))]
descritivas<-data.frame(round(stat.desc(dados_num),4))
plot(size,price)

# Duas observações caóticas: tamanho mínimo (1m^2) e tamanho máximo (95000m^2, mesmo valor de price: suspeito), ambas em Jeddah.
# Geralmente, vamos atrás para saber se essas observações estão corretas ou não.
# Se estiverem, ajustamos o modelo com elas e vemos suas influências no ajuste.
# Se for necessário, as removemos e nos lembramos sempre que o modelo foi ajustado sem tais outliers.

dados_limpos<-dados[-c(which.min(dados$size),which.max(dados$size)),]
plot(dados_limpos$size,dados_limpos$price,xlab='size',ylab='price')
cor(dados_limpos$size,dados_limpos$price)
cor(size,price)

dados_num_limpos<-dados_limpos[,-which(names(dados_limpos) %in% c('city'))]
descritivas_limpas<-data.frame(round(stat.desc(dados_num_limpos),4))

hist(dados_limpos$price,xlab='price',main = "")
hist(dados_limpos$size,xlab='price',main = "")

dados_Riyadh<-subset(dados_limpos,city=='Al Riyadh')
dados_Jeddah<-subset(dados_limpos,city=='Jeddah')
dados_Dammam<-subset(dados_limpos,city=='Dammam')
dados_Khobar<-subset(dados_limpos,city=='Al Khobar')

descritivas_price_cidades<-data.frame(price_Riyadh=round(stat.desc(dados_Riyadh$price),1),
                                      price_Jeddah=round(stat.desc(dados_Jeddah$price),1),
                                      price_Dammam=round(stat.desc(dados_Dammam$price),1),
                                      price_Khobar=round(stat.desc(dados_Khobar$price),1))

descritivas_size_cidades<-data.frame(size_Riyadh=round(stat.desc(dados_Riyadh$size),1),
                                     size_Jeddah=round(stat.desc(dados_Jeddah$size),1),
                                     size_Dammam=round(stat.desc(dados_Dammam$size),1),
                                     size_Khobar=round(stat.desc(dados_Khobar$size),1))

par(mfrow=c(2,2))
hist(dados_Riyadh$size,xlim = c(0,5000),main='Riyadh - size',xlab = 'size (m²)',ylab='Frequência',breaks = seq(0,5000,length.out=50))
hist(dados_Jeddah$size,xlim = c(0,5000),main='Jeddah - size',xlab = 'size (m²)',ylab='Frequência',breaks = seq(0,5000,length.out=50))
hist(dados_Dammam$size,xlim = c(0,5000),main='Dammam - size',xlab = 'size (m²)',ylab='Frequência',breaks = seq(0,5000,length.out=50))
hist(dados_Khobar$size,xlim = c(0,5000),main='Khobar - size',xlab = 'size (m²)',ylab='Frequência',breaks = seq(0,5000,length.out=50))

par(mfrow=c(2,2))
hist(dados_Riyadh$price,xlim = c(0,1700000),main='Riyadh - price',xlab = 'price (em ?? Rial)',ylab='Frequência',breaks = seq(0,1700000,length.out=50))
hist(dados_Jeddah$price,xlim = c(0,1700000),main='Jeddah - price',xlab = 'price (em ?? Rial)',ylab='Frequência',breaks = seq(0,1700000,length.out=50))
hist(dados_Dammam$price,xlim = c(0,1700000),main='Dammam - price',xlab = 'price (em ?? Rial)',ylab='Frequência',breaks = seq(0,1700000,length.out=50))
hist(dados_Khobar$price,xlim = c(0,1700000),main='Khobar - price',xlab = 'price (em ?? Rial)',ylab='Frequência',breaks = seq(0,1700000,length.out=50))

par(mfrow=c(2,2))
plot(dados_Riyadh$size,dados_Riyadh$price,xlab = 'size',ylab='price',main=paste0('Riyadh - Cor Pe = ',round(cor(dados_Riyadh$size,dados_Riyadh$price,method = 'pearson'),3)))
plot(dados_Jeddah$size,dados_Jeddah$price,xlab = 'size',ylab='price',main=paste0('Jeddah - Cor Pe = ',round(cor(dados_Jeddah$size,dados_Jeddah$price,method = 'pearson'),3)))
plot(dados_Dammam$size,dados_Dammam$price,xlab = 'size',ylab='price',main=paste0('Dammam - Cor Pe = ',round(cor(dados_Dammam$size,dados_Dammam$price,method = 'pearson'),3)))
plot(dados_Khobar$size,dados_Khobar$price,xlab = 'size',ylab='price',main=paste0('Riyadh - Cor Pe = ',round(cor(dados_Khobar$size,dados_Khobar$price,method = 'pearson'),3)))

par(mfrow=c(2,2))
plot(dados_Riyadh$size,dados_Riyadh$price,xlab = 'size',ylab='price',main=paste0('Riyadh - Cor Sp = ',round(cor(dados_Riyadh$size,dados_Riyadh$price,method = 'spearman'),3)))
plot(dados_Jeddah$size,dados_Jeddah$price,xlab = 'size',ylab='price',main=paste0('Jeddah - Cor Sp = ',round(cor(dados_Jeddah$size,dados_Jeddah$price,method = 'spearman'),3)))
plot(dados_Dammam$size,dados_Dammam$price,xlab = 'size',ylab='price',main=paste0('Dammam - Cor Sp = ',round(cor(dados_Dammam$size,dados_Dammam$price,method = 'spearman'),3)))
plot(dados_Khobar$size,dados_Khobar$price,xlab = 'size',ylab='price',main=paste0('Riyadh - Cor Sp = ',round(cor(dados_Khobar$size,dados_Khobar$price,method = 'spearman'),3)))

##############
## Modelo 1 ##
##############

modelo1<-lm(price~city+size,data = dados_limpos)

summary(modelo1)
summary_modelo1<-summary(modelo1)
anova(modelo1)
anova_modelo1<-anova(modelo1)
matriz_modelo1<-model.matrix(modelo1)

dist_cook_modelo1<-data.frame(Dist_Cook=cooks.distance(modelo1),
                      size=dados_limpos$size,
                      price=dados_limpos$price)
#
dist_cook_modelo1_ord<-dist_cook[order(dist_cook$Dist_Cook,decreasing = T),]
#

par(mfrow=c(1,1))

#
plot(dados_limpos$size,dados_limpos$price,
     xlab='size',ylab='price',cex.lab=1.8,cex.axis=1.5)

points(dist_cook_modelo1_ord$size[1:6],
       dist_cook_modelo1_ord$price[1:6],col='green',lwd=3)


hist(rstandard(modelo1),xlab='Resíduo',prob=T,breaks = 20,xlim = c(-4,4),ylim = c(0,1),main = 'Resíduo Padronizado',ylab='Densidade')
lines(density(modelo1$residuals/summary(modelo1)$sigma),col='red',lwd=2)
lines(x=seq(-4,4,length.out=100),y=dnorm(seq(-4,4,length.out=100)),col='blue',lwd=2)
qqnorm(rstandard(modelo1),xlim=c(-8,8),ylim=c(-8,8)); qqline(rstandard(modelo1),distribution = qnorm)

plot(fitted(modelo1), rstandard(modelo1))
plot(dados_limpos$size, rstandard(modelo1),xlab = 'size')

cooks.distance(modelo1)
plot(cooks.distance(modelo1))

anova_modelo1



##############
## Modelo 2 ##
##############

modelo2<-lm(price~size+city,data = dados_limpos)

summary(modelo2)
summary_modelo2<-summary(modelo2)
anova(modelo2)
anova_modelo2<-anova(modelo2)
matriz_modelo2<-model.matrix(modelo2)



##############
## Modelo 3 ##
##############

modelo3<-lm(price~city+size+city:size,data = dados_limpos)

summary(modelo3)
summary_modelo3<-summary(modelo3)
anova(modelo3)
anova_modelo3<-anova(modelo3)
matriz_modelo3<-model.matrix(modelo3)

dist_cook_modelo3<-data.frame(Dist_Cook=cooks.distance(modelo3),
                              size=dados_limpos$size,
                              price=dados_limpos$price)
#
dist_cook_modelo3_ord<-dist_cook_modelo3[order(dist_cook_modelo3$Dist_Cook,decreasing = T),]
#

plot(dados_limpos$size,dados_limpos$price,
     xlab='size',ylab='price',cex.lab=1.8,cex.axis=1.5)

points(dist_cook_modelo3_ord$size[1:6],
       dist_cook_modelo3_ord$price[1:6],col='green',lwd=3)


hist(rstandard(modelo3),xlab='Resíduo',prob=T,breaks = 20,xlim = c(-4,4),ylim = c(0,1),main = 'Resíduo Padronizado',ylab='Densidade')
lines(density(modelo3$residuals/summary(modelo3)$sigma),col='red',lwd=2)
lines(x=seq(-4,4,length.out=100),y=dnorm(seq(-4,4,length.out=100)),col='blue',lwd=2)
qqnorm(rstandard(modelo3),xlim=c(-8,8),ylim=c(-8,8)); qqline(rstandard(modelo3),distribution = qnorm)

plot(fitted(modelo3), rstandard(modelo3))
resid_reduz<-data.frame(ajustado=fitted(modelo3),residuo=rstandard(modelo3))
resid_reduz<-subset(resid_reduz,residuo>-4 & residuo<4 & (ajustado<3*10^5) )
plot(resid_reduz$ajustado, resid_reduz$residuo,xlab='ajustado',ylab='residuo')

#cooks.distance(modelo3)
plot(cooks.distance(modelo3))

efeito_size_mod1<-139.09         # O mesmo para todas as cidades.
efeito_size_Riyadh_mod3<-114.307+23.060
efeito_size_Dammam_mod3<-114.307+3.822
efeito_size_Jeddah_mod3<-114.307+35.571
efeito_size_Khobar_mod3<-   #??????

# Testar igualdade das interações size*Riyadh e size*Jeddah
Lambda <- matrix(c(0,0,0,0,0,1,0,-1), nrow=1)
Lambda
library(car)
linearHypothesis(modelo3, Lambda, test=c('F'))

# IC's
# Para os betas
confint(modelo3,level=0.95)

dados_Khobar$predito<-predict(modelo3,dados_Khobar)
dados_Dammam$predito<-predict(modelo3,dados_Dammam)
dados_Jeddah$predito<-predict(modelo3,dados_Jeddah)
dados_Riyadh$predito<-predict(modelo3,dados_Riyadh)

plot(dados_limpos$size,dados_limpos$price,
     xlab='size',ylab='price',cex.lab=1.8,cex.axis=1.5)
lines(dados_Khobar$size,dados_Khobar$predito,col=2,lwd=2)
lines(dados_Dammam$size,dados_Dammam$predito,col=3,lwd=2)
lines(dados_Jeddah$size,dados_Jeddah$predito,col=4,lwd=2)
lines(dados_Riyadh$size,dados_Riyadh$predito,col=5,lwd=2)

plot(dados_Khobar$size,dados_Khobar$price,
     xlab='size',ylab='price',cex.lab=1.8,cex.axis=1.5,ylim=c(0,200000))
lines(dados_Khobar$size,dados_Khobar$predito,col=2,lwd=2)
dados_Khobar$IC_lwr<-data.frame(predict(modelo3,dados_Khobar,interval = 'confidence'))$lwr
dados_Khobar$IC_upr<-data.frame(predict(modelo3,dados_Khobar,interval = 'confidence'))$upr
dados_Khobar<-dados_Khobar[order(dados_Khobar$size),]
lines(dados_Khobar$size,dados_Khobar$IC_lwr,col=4,lwd=2,lty=3)
lines(dados_Khobar$size,dados_Khobar$IC_upr,col=4,lwd=2,lty=3)

dados_Khobar$IP_lwr<-data.frame(predict(modelo3,dados_Khobar,interval = 'prediction'))$lwr
dados_Khobar$IP_upr<-data.frame(predict(modelo3,dados_Khobar,interval = 'prediction'))$upr
dados_Khobar<-dados_Khobar[order(dados_Khobar$size),]
lines(dados_Khobar$size,dados_Khobar$IP_lwr,col=5,lwd=2,lty=3)
lines(dados_Khobar$size,dados_Khobar$IP_upr,col=5,lwd=2,lty=3)

  
##############
## Modelo 4 ##
##############

modelo4<-lm(price~size+city++city:size,data = dados_limpos)

summary(modelo4)
summary_modelo4<-summary(modelo4)
anova(modelo4)
anova_modelo4<-anova(modelo4)
matriz_modelo4<-model.matrix(modelo4)









