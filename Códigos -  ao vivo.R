## Teste de alteração.

############################################
### Comparando resultados de anova e summary
set.seed(735)
x1<-runif(50,0,50)
x2<-runif(50,30,40) - 0.2*x1
y <- 5 + 2*x1 + x2 + 0.5*(x2/x1) + rnorm(50,0,2)
plot(x1,y)
plot(x2,y)
plot(x1,x2)
cor(x1,y)
cor(x2,y)
cor(x1,x2)

modelo1<-lm(y~x1+x2+x1:x2)

summary_modelo1<-summary(modelo1)
summary_modelo1

t_x1<-summary_modelo1$coefficients['x1','t value']
t_x2<-summary_modelo1$coefficients['x2','t value']
t_x1.x2<-summary_modelo1$coefficients['x1:x2','t value']

anova_modelo1<-anova(modelo1)
anova_modelo1

F_x1<-(anova_modelo1$`Sum Sq`[1]/1)/(anova_modelo1$`Sum Sq`[4]/(anova_modelo1$Df[4]))
F_x2<-(anova_modelo1$`Sum Sq`[2]/1)/(anova_modelo1$`Sum Sq`[4]/(anova_modelo1$Df[4]))
F_x1.x2<-(anova_modelo1$`Sum Sq`[3]/1)/(anova_modelo1$`Sum Sq`[4]/(anova_modelo1$Df[4]))

t_x1
F_x1
t_x1^2

t_x2
F_x2
t_x2^2

t_x1.x2
F_x1.x2
t_x1.x2^2

## O QUE ESTÁ ACONTECENDO AQUI? POR QUE APENAS O TERMO DE INTERAÇÃO ESTÁ DANDO IGUAL?

##########################################
#  Variável  ###########  SSR  ###########
################ summary ###### anova ####
#    x1      # x1|x2,x1:x2 #      x1     #
#    x2      # x2|x1,x1:x2 #    x2|x1    #
#  x1:x2     # x1:x2|x1,x2 # x1:x2|x1,x2 #
##########################################


library(car)
Anova(modelo1,type = 3)
t_x1^2
t_x2^2

####

Lambda<-matrix(c(0,1,-1,0,
                 0,1,0,-1),
               byrow = T,nrow = 2)
Lambda
linearHypothesis(modelo1,Lambda,test = 'F')

#####

#### ICs

# Para os betas
confint(modelo1,level=0.95)
confint(modelo1,level=0.99)

# Para E(y)

# Perceba que no caso de regressão com múltiplas variáveis numéricas,
# nossa reta de regressão é generalizada a um hiperplano.
# Por esse motivo, para facilitar a visualização, iremos deixar uma
# das covariáveis livres e fixar as demais.

x2_fixo1<-mean(x2)
x2_fixo2<-mean(x2)-sd(x2)
x2_fixo3<-mean(x2)+sd(x2)
x2_fixo4<-mean(x2)-2*sd(x2)
x2_fixo5<-mean(x2)+2*sd(x2)
predito_x2_fixo1<-predict(modelo1,data.frame(x1=x1,x2=x2_fixo1,'x1:x2'=x1*x2_fixo1))
predito_x2_fixo2<-predict(modelo1,data.frame(x1=x1,x2=x2_fixo2,'x1:x2'=x1*x2_fixo2))
predito_x2_fixo3<-predict(modelo1,data.frame(x1=x1,x2=x2_fixo3,'x1:x2'=x1*x2_fixo3))
predito_x2_fixo4<-predict(modelo1,data.frame(x1=x1,x2=x2_fixo4,'x1:x2'=x1*x2_fixo4))
predito_x2_fixo5<-predict(modelo1,data.frame(x1=x1,x2=x2_fixo5,'x1:x2'=x1*x2_fixo5))

summary(modelo1)

-13.905103 + 2.655392*x1[1] + 1.713370*x2_fixo1 -0.024251*x1[1]*x2_fixo1

predito_x2_fixo1[1]


plot(x1,y)
lines(x1,predito_x2_fixo1,col=2,lwd=2)
lines(x1,predito_x2_fixo2,col=3,lwd=2)
lines(x1,predito_x2_fixo3,col=4,lwd=2)
lines(x1,predito_x2_fixo4,col=5,lwd=2)
lines(x1,predito_x2_fixo5,col=6,lwd=2)


plot(x1,y)
IC_x2_fixo1<-predict(modelo1,data.frame(x1=x1,x2=x2_fixo1,'x1:x2'=x1*x2_fixo1),interval = 'confidence')
lines(x1,IC_x2_fixo1[,'fit'],lwd=2,col=2)
lines(x1,IC_x2_fixo1[,'lwr'],lwd=1,col=4)
lines(x1,IC_x2_fixo1[,'upr'],lwd=1,col=4)

IP_x2_fixo1<-predict(modelo1,data.frame(x1=x1,x2=x2_fixo1,'x1:x2'=x1*x2_fixo1),interval = 'prediction')
lines(x1,IP_x2_fixo1[,'lwr'],lwd=1,col=3)
lines(x1,IP_x2_fixo1[,'upr'],lwd=1,col=3)


