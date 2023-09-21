
Data1<-cbind(paste(TABE.1[,5],TABE.1[,4],sep = "-"),TABE.1[,c(6:dim(TABE.1)[2])])

tratamento.1=c(as.character(unique(Data1[,1])))
tratamento.2<-apply(Data1[,c(2:dim(Data1)[2])],2,function(x){
  #unique(ave( as.numeric(as.character(x)), Data1[,1], FUN = function(y){mean(y,na.rm=T)}))
  
  a1<-unique(paste(Data1[,c(1)],ave( as.numeric(as.character(x)), Data1[,1], FUN = function(y){mean(y,na.rm=T)}),sep = "_"))
  a2<-(do.call(rbind,strsplit(a1,split = "_"))[,2])
  as.numeric(as.character(a2))
})

Data2<-cbind(Trat=tratamento.1,tratamento.2)

#write.table(Data2,"DadosExperimentoUreia2.txt",col.names = TRUE,row.names = F,quote = F, sep = "\t")

library("factoextra")

data.fim<-Data2[,-1]
data.fim1<-apply(data.fim,2,function(x){
  as.numeric(as.character(x))
})
rownames(data.fim1)<-Data2[,1]

res.pca <- prcomp(data.fim1,  scale = TRUE)

fviz_pca_biplot(res.pca, label="all",pointshape = 16,
                    addEllipses=TRUE, ellipse.level=0.6,pointsize = 4,arrowsize = 1,labelsize = 5)+theme(legend.position = "none",
                                                                                                         legend.direction = "vertical",
                                                                                                         legend.text = element_text(color="black",size=18),
                                                                                                         legend.title = element_text(color="black",size=18),
                                                                                                         axis.text.y = element_text(color="black",size=18),
                                                                                                         axis.title = element_text(color="black",size=18),
                                                                                                         axis.text.x = element_text(color="black",size=18),
                                                                                                         strip.text = element_text(color="black",size=18),
                                                                                                         strip.background = element_rect(fill="white"))

