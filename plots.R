library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
#---------------------------------------------------------------------------
#- Resposta -----------------------------------------------
df <- table(dados$p20a3)%>%as.data.frame()
names(df)<-c("p20a3", "Freq"); df

df      <-df[order(df$p20a3),]
df$soma <- sum(df$Freq)
df$fr   <-df$Freq/df$soma

p20a3<-ggplot(df, aes(x=p20a3,y=fr*100,fill=p20a3)) +
  geom_bar(stat = "identity",position=position_dodge(), colour="black") + 
  xlab("Se um governante administra bem o país, não importa se ele é corrupto ou não") + ylab("%") +
  scale_fill_manual(values= c("#cc6699", "#0000cc", "#339966"), 
                    name="Rouba, mas faz",
                    breaks=c(1,3,5),
                    labels=c("Concordo", "Neutro", "Discordo")) +
  scale_x_discrete(breaks=c(1,3,5),
                   labels=c("Concordo", "Neutro", "Discordo")) +
  geom_text(data=df,aes(label=Freq),vjust=-1) +
  ggsave("C:/Users/T.I/Dropbox/Dados Categótericos/Trabalhos/Multinomial ordinal/draft/freq.jpeg", 
         device = "jpeg", height = 7, width = 10)
x11()
p20a3

#---------------------------------------------------------------------------
#- p15 -----------------------------------------------
df <- table(dados$p20a3,dados$p15)%>%as.data.frame()
names(df)<-c("p20a3", "p15", "Freq"); df

df<-df[order(df$p20a3),] 
aux<-aggregate(Freq ~ p20a3, sum,data = df)
n<-length(levels(df$p15))
df$soma<-rep(aux[,2],each=n)
df$fr<-df$Freq/df$soma

p15<-ggplot(df, aes(x=p15,y=fr*100,fill=p20a3)) +
  geom_bar(stat = "identity",position=position_dodge(), colour="black") + 
  xlab("Depois da Opera??o Lava-Jato a corrup??o no Brasil ir?") + ylab("%") +
  scale_fill_manual(values= c("#cc6699", "#0000cc", "#339966"), 
                    name="Rouba, mas faz",
                    breaks=c(1,3,5),
                    labels=c("Concordo", "Neutro", "Discordo")) +
  scale_x_discrete(breaks=1:3,
                     labels=c("Ir? diminuir","Ir? aumentar","Continuar? na mesma"))
x11()
p15

#---------------------------------------------------------------------------
#- p16 -----------------------------------------------
df <- table(dados$p20a3,dados$p16)%>%as.data.frame()
names(df)<-c("p20a3", "p16", "Freq"); df

df<-df[order(df$p20a3),] 
aux<-aggregate(Freq ~ p20a3, sum,data = df)
n<-length(levels(df$p16))
df$soma<-rep(aux[,2],each=n)
df$fr<-df$Freq/df$soma

p16<-ggplot(df, aes(x=p16,y=fr*100,fill=p20a3)) +
  geom_bar(stat = "identity",position=position_dodge(), colour="black") + 
  xlab("Na sua opini?o, Lula deveria ou n?o ser preso") + ylab("%") +
  scale_fill_manual(values= c("#cc6699", "#0000cc", "#339966"), 
                    name="Rouba, mas faz",
                    breaks=c(1,3,5),
                    labels=c("Concordo", "Neutro", "Discordo")) +
  scale_x_discrete(breaks=1:2,
                   labels=c("Sim, deveria","N?o deveria"))
x11()
p16

#---------------------------------------------------------------------------
#- p21a -----------------------------------------------
df <- table(dados$p20a3,dados$p21a)%>%as.data.frame()
levels(df$Var2) <- c("1","2","3")
names(df)<-c("p20a3", "p21a", "Freq"); df

df  <-df[order(df$p20a3),] 
aux <-aggregate(Freq ~ p20a3, sum,data = df)
n   <-length(levels(df$p21a))
df$soma <- rep(aux[,2],each=n)
df$fr<-df$Freq/df$soma

p21a<-ggplot(df, aes(x=p21a,y=fr*100,fill=p20a3)) +
  geom_bar(stat = "identity",position=position_dodge(), colour="black") + 
  xlab("Com qual dessas tr?s afirma??es voc? concorda mais") + ylab("%") +
  scale_fill_manual(values= c("#cc6699", "#0000cc", "#339966"), 
                    name="Rouba, mas faz",
                    breaks=c(1,3,5),
                    labels=c("Concordo", "Neutro", "Discordo")) +
  scale_x_discrete(breaks=1:4,
                   labels=c("a democracia ? sempre melhor",
                            "? melhor uma ditadura", "tanto faz","outros"))
x11()
p21a

#---------------------------------------------------------------------------
#- escola -----------------------------------------------
df <- table(dados$p20a3,dados$escola)%>%as.data.frame()
names(df)<-c("p20a3", "escola", "Freq"); df

df  <-df[order(df$p20a3),] 
aux <-aggregate(Freq ~ p20a3, sum,data = df)
n   <-length(levels(df$escola))
df$soma <- rep(aux[,2],each=n)
df$fr<-df$Freq/df$soma

escola<-ggplot(df, aes(x=escola,y=fr*100,fill=p20a3)) +
  geom_bar(stat = "identity",position=position_dodge(), colour="black") + 
  xlab("Escolaridade") + ylab("%") +
  scale_fill_manual(values= c("#cc6699", "#0000cc", "#339966"), 
                    name="Rouba, mas faz",
                    breaks=c(1,3,5),
                    labels=c("Concordo", "Neutro", "Discordo")) 
x11()
escola

#---------------------------------------------------------------------------
#- saida -----------------------------------------------
x11()
gg    <- grid.arrange(p15, p16, p21a, escola, ncol=2)

saida <- "C:/Users/T.I/Dropbox/Dados Categ?tericos/Trabalhos/Multinomial ordinal/draft/gg.jpeg"
ggsave(gg,file= saida, width = 10, height = 6, dpi = 300 )

#---------------------------------------------------------------------------
# Predi??o -----------------------------------------------------------
x1           <- seq(min(dados$escola, na.rm = T), max(dados$escola, na.rm = T))
x2           <- levels(dados$p15)
x3           <- levels(dados$p16)
x4           <- levels(dados$p21a)
teste        <- expand.grid(x1, x2, x3, x4)
names(teste) <- c("escola", "p15", "p16", "p21a")

preditos <- predict(mop, newdata = teste, type = "response")
saida    <- cbind(teste, preditos = preditos)
apply(preditos, 1, sum)

saida[,2:4] <- lapply(saida[,2:4], as.factor)
levels(saida$p15)  <- c("Irá diminuir", "Irá aumentar","Continuará na mesma")
levels(saida$p16)  <- c("Lula deveria ser preso, Sim", "Lula deveria ser preso, Não")
levels(saida$p21a) <- c("A democracia é sempre melhor",
                        "Em alguns casos é melhor ditadura",
                        "Tanto faz")

saida$predito <- saida$preditos.3 + saida$preditos.1

x11()
ggplot(data = saida, aes(x = escola, y = predito, color = p15)) +
  geom_point(size = 2) + geom_line(linetype = 3) + 
  facet_wrap(p16 ~ p21a) +
  labs(x = "Escolaridade", y = "Probabilidade", color = "Após a Lava-Jato\na corrupção") +
  ggtitle("P(Y < 3)") + 
  ggsave("C:/Users/T.I/Dropbox/Dados Categótericos/Trabalhos/Multinomial ordinal/draft/prob.jpeg", 
         device = "jpeg", height = 5, width = 10)

