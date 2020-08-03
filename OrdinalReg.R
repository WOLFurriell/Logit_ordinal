rm(list = ls())
#===================================================================================================================
# Regres?o Logistica Ordinal
library(VGAM);library(foreign);library(dplyr);
library(naniar);library(xtable);library(generalhoslem)
#===================================================================================================================
# Importando banco SPSS
dados <- read.spss("C:/Users/T.I/Dropbox/Dados Categ�tericos/Dados/corrup/04423.SAV", 
                   to.data.frame = T, use.value.labels = F)

#dados <- read.spss("/home/furriel/Dropbox/Dados Categótericos/Dados/corrup/04423.SAV", 
#                   to.data.frame = T, use.value.labels = F)
# Tranformando 99 em NA
dados <- dados %>% replace_with_na_all(condition = ~.x == 99) %>% as.data.frame()
head(dados)
dim(dados)

#Recodifica??o
dados$p8[dados$p8==95 | dados$p8==96 | dados$p8==97] <- 3
dados$rendaf[dados$rendaf == 97] <- NA
dados$p21a[dados$p21a == 98]     <- NA

# Transformando as class em factor
cols <- c("p8", "p15", "p18", "p16", "p21a")
dados[cols] <- lapply(dados[cols], factor)

dados$p20a2 <- ifelse(dados$p20a==3, NA, dados$p20a)
dados$p20a3 <- ifelse(dados$p20a==2 | dados$p20a==3 | dados$p20a==4, 3, dados$p20a)

table(dados$p20a); table(dados$p20a2); table(dados$p20a3)

dados <- dados[c("p20a3", "p15", "p16", "p21a","escola")]
dados <- na.omit(dados)

#===================================================================================================================
# Modelo de regress?o ordinal

mlc   <- vglm(p20a3 ~ p15 + p16 + p21a  + escola, cumulative(parallel=F,reverse=F), dados)
summary(mlc)     

mop   <- vglm(p20a3 ~ p15 + p16 + p21a  + escola, cumulative(parallel=T,reverse=F), dados)
resul <- summaryvglm(mop)

xtable(coef(summary(resul)), digits = 4)

#===================================================================================================================
# Ajuste alternativo
library(reshape2)

tabela <- dcast(dados, p15 + p16 + p21a  + escola ~ p20a3, sum)
names(tabela) <- c("p15", "p16", "p21a","escola","n1","n2","n3")

mop2   <- vglm(cbind(n1,n2,n3) ~ factor(p15) + factor(p16) + factor(p21a)  + escola, 
               cumulative(parallel=T,reverse=F), tabela)
summaryvglm(mop2)

obs   <- mop2@y[,1] + mop2@y[,2]
fit   <- fitted(mop2)[,1] + fitted(mop2)[,2]
res   <- obs - fit
probs <- cbind(obs, fit, res)%>%as.data.frame()

xtable(probs, digits = 6)


# Gráfico resíduo
jpeg("/home/furriel/Dropbox/Dados Categótericos/Trabalhos/Multinomial ordinal/rplot.jpeg", 
     width = 1250, height = 580, res = 100)
par(mfrow=c(1,2),bg = "white")
rp <- resid(mop2, type = "pearson")
plot(rp[,1], xlab="Índice", ylab="Resíduos de Pearson")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#E8E8E8")
points(rp[,1], pch=20, col= "#003366")
abline(h=0, lty=3, col = "red", lwd = 2)
title("Logit 1")
plot(rp[,2], xlab="Índice", ylab="Resíduos de Pearson")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#E8E8E8")
points(rp[,1], pch=20, col= "#003366")
abline(h=0, lty=3, col = "red", lwd = 2)
title("Logit 2")
dev.off()

#===================================================================================================================
# Teste para verificar as chances proporcionais
TRV <- 2*(logLik(mlc)-logLik(mop))
gl  <- length(coef(mlc))-length(coef(mop)); p<-1-pchisq(TRV,gl)
b   <-cbind(TRV, gl, p)

vars <- c("p15","p16","p21a","escola")
tab <- matrix(ncol=3, nrow=length(vars))
j <- 0
#Testado as vari?veis do modelo uma a uma
for (i in vars){
j <- j + 1
dados$aux   <- dados[,paste(i)]
mopx   <- vglm(p20a3 ~ aux, cumulative(parallel=T,reverse=F), dados)
mlcx   <- vglm(p20a3 ~ aux, cumulative(parallel=F,reverse=F), dados)
    TRV<-2*(logLik(mlcx)-logLik(mopx)); gl<-length(coef(mlcx))-length(coef(mopx))
    p<-1-pchisq(TRV,gl)
    # Resultado
    print(paste(i))
    tab[j,] <- cbind(TRV, gl, p)
}

tabela <- rbind(b, tab)%>%as.data.frame()
rownames(tabela) <- c("geral",vars)

xtable(tabela, digits = 4)

#=============================================================================================
# Qualidade do ajuste

r <- length(dados$p20a3%>%as.factor()%>%levels())
q <- dim(coef(summary(resul)))[1]-2
s <- 135

gl <- (r-1)*(s-1)-q

QL <- deviance(mop); QL
1 - pchisq(QL,gl)

rp<-residuals(mop, type="pearson")
Qp<-sum(rp[,1]^2) + sum(rp[,2]^2)
Qp
1-pchisq(Qp,4000)

# Lipsitz teste -------------------------------------------------------

library(ordinal)
dados$p20a3 <- as.factor(dados$p20a3)
model1 <- polr(p20a3 ~ p15 + p16 + p21a  + escola, data=dados)
summary(model1)
model2 <- clm(p20a3 ~ p15 + p16 + p21a  + escola, data=dados)
summary(model2)

lipsitz.test(model2)


x11()
plot(rp[,2], pch=20, xlab="?ndice", ylab="Res?duos de Pearson")
abline(h=0, lty=3)

#---------------------------------------------------------------------------
# Preditos-----------------------------------------------------------
x1           <- seq(min(dados$escola, na.rm = T), max(dados$escola, na.rm = T))
x2           <- levels(dados$p15)
x3           <- levels(dados$p16)
x4           <- levels(dados$p21a)
teste        <- expand.grid(x1, x2, x3, x4)
names(teste) <- c("escola", "p15", "p16", "p21a")

preditos <- predict(mop, newdata = teste, type = "response")

# Observados
saida    <- cbind(teste, preditos = preditos)%>%as.data.frame()
rownames(saida) <- NULL
apply(preditos, 1, sum)
xtable(saida, digits = 4,include.rownames=FALSE)
