
library(ggplot2)

nivel<-c(reac.P(),reac.K(),reac.Ca(),reac.Mg(),reac.S(),reac.B(),reac.Cu(),reac.Fe(),reac.Mn(),reac.Zn())
elem<-c("P","K","Ca","Mg","S","B","Cu","Fe","Mn","Zn")

col.niv<-gsub("Muito alto","darkolivegreen4",nivel)
col.niv<-gsub("Alto","darkolivegreen3",col.niv)
col.niv<-gsub("Medio","darkgoldenrod1",col.niv)
col.niv<-gsub("Baixo","brown3",col.niv)
col.niv<-gsub("Muito baixo","brown4",col.niv)

Num.niv<-gsub("Muito alto",5,nivel)
Num.niv<-gsub("Alto",4,Num.niv)
Num.niv<-gsub("Medio",3,Num.niv)
Num.niv<-gsub("Baixo",2,Num.niv)
Num.niv<-gsub("Muito baixo",1,Num.niv)

dados<-cbind(nivel,elem,col.niv,Num.niv)
colnames(dados)<-c("nivel","elem","col.niv","Num.niv")
dados<-as.data.frame(dados)
dados$Num.niv<-as.numeric(as.character(dados$Num.niv))
dados$nivel<-as.character(dados$nivel)
dados$elem<-as.factor(dados$elem)
dados$col.niv<-as.character(dados$col.niv)

ggplot(dados, aes(x = elem , y = Num.niv))+
  geom_bar(stat = "identity", alpha=0.8,
           position = 'dodge',
           aes(color=col.niv,
           fill=col.niv)) +
  scale_fill_manual(breaks = c("Muito alto","Alto","Medio","Baixo","Muito baixo"), 
                    values=c("brown3","brown4","darkgoldenrod1","darkolivegreen3","darkolivegreen4"))+
  scale_color_manual(breaks = c("Muito alto","Alto","Medio","Baixo","Muito baixo"), 
                    values=c("brown3","brown4","darkgoldenrod1","darkolivegreen3","darkolivegreen4"))+
  geom_text(aes(label=nivel), position=position_dodge(width=0.9), vjust=-0.20,size=5)+
  #scale_y_continuous(breaks=seq(0,250,by=50), limits = c(0, 220))+
  theme_bw()+
  xlab("")+
  ylab("Perfil da Análise de Solo")+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.text = element_text(color="black",size=18),
        legend.title = element_text(color="black",size=18),
        axis.text.y = element_text(color="black",size=18),
        axis.title = element_text(color="black",size=18),
        axis.text.x = element_text(color="black",size=16,angle = 0, hjust = 0.5,vjust = 0.5),
        strip.text = element_text(color="black",size=18),
        strip.background = element_rect(fill="white"))

