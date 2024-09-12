#'@export
SSplots_EPI<- function (data,lower.lt=20,upper.lt=80, tsplots=FALSE,MA=FALSE)
{
  if (MA==TRUE){
    data1<-data.frame(zoo::rollmean(data,k=3))
  }else{
    data1<-data.frame(data)
  }

  data2<-data1[,-1]
  Year<-data1[,1]
  lw<-0
  up<-0
  mat = matrix(ncol = ncol(data1), nrow = nrow(data1))
  colnames(mat)=colnames(data1)
  mat[,1]=data1[,1]
  outFname<-"Time Series Plot-"
  for (i in 1:ncol(data2))
  {
    lw[i]<-max(data1[,i+1])*(lower.lt/100)
    up[i]<-max(data1[,i+1])*(upper.lt/100)
    if (tsplots==TRUE)
    {
      ggplot2::ggplot(data1, ggplot2::aes(x = factor(Year), y = data2[ , i],group=1))+ggplot2::xlab("Year")+
        ggplot2::geom_area(color="darkblue",fill="lightblue",linetype=1,linewidth=1)+
        ggplot2::geom_hline(ggplot2::aes(yintercept = lw[i], color=paste(lower.lt,"% of the maximum landings")),linetype=2,lwd=1)+
        ggplot2::geom_hline(ggplot2::aes(yintercept = up[i], color=paste(upper.lt,"% of the maximum landings")),linetype="twodash",lwd=1)+
        ggplot2::ggtitle("Time Series Plot")+ggplot2::ylab("Catch/Landings")+ggplot2::labs(color="")+ggplot2::theme(legend.position = "bottom")
      file_name=colnames(data2)[i]
      file_name=gsub("\\."," ",file_name)
      ggplot2::ggsave(filename=paste(outFname,file_name,".png",sep=" "),height=5,width=7)

    }
    ####################Change
    for (j in 1:nrow(data2)){
      if(data1[j,1]<= data1[which(grepl(max(data1[,i+1]),data1[,i+1])),1] & data2[j,i]<=lw[i])
        mat[j,i+1]<-1
      else{
        if((data2[j,i]>=lw[i] & data2[j,i]<=up[i]))
          mat[j,i+1]<-2
        else{
          if((data1[j,1]>=data1[which(grepl(max(data1[,i+1]),data1[,i+1])),1] & (data2[j,i]<=lw[i])) || (data2[j,i]>=up[i]))
            mat[j,i+1]<-3
        }
      }
    }

  }
  mat1<-data.frame(mat[ ,-1 ])
  mat11<-data.frame(mat[ ,1])
  colnames(mat11)<-"Year"
  #Underdeveloped<-rowSums(mat1==1)
  Developing<-rowSums(mat1==1)
  FullyExploited<-rowSums(mat1==2)
  Overfished<-rowSums(mat1==3)
 # Collapsed<-rowSums(mat1==5)
  mat2<-data.frame(cbind(Developing,FullyExploited,Overfished))
  mat21<-(mat2/rowSums(mat2))*100
  mat4<-utils::stack(data.frame(mat21))
  Year1<-utils::stack(data.frame(rep(mat11,3))) ### 5 replaced by 3
  Year<-as.numeric(Year1$values)
  Pecentage_Count<-data.frame(cbind(Year,mat4))
  colnames(Pecentage_Count)<-c("Year","Count","Status")
  Pecentage_Count[order(Pecentage_Count$Year, decreasing = F), ]

  #mat4 <- Pecentage_Count  %>%
  #  +     group_by(Year, Status) %>%
  #  +     summarise(n = sum(Count)) %>%
  #  +     mutate(percentage = (n / sum(n)*100))

  #write.csv(mat, file = "CodedDataGroupIndia.csv")

  #tiff(filename = "IndiaGroupNumFSS1.tiff",width = 7,height = 8,units = "in",res = 300)
  Count=Pecentage_Count$Count
  Status=Pecentage_Count$Status
  plot_Number<-ggplot2::ggplot(Pecentage_Count, ggplot2::aes(x=factor(Year), y=Count, fill=Status,group=Status)) + ggplot2::geom_area() +ggplot2::scale_fill_brewer(palette="RdYlGn",direction=-1)+
    ggplot2::theme(legend.position = "bottom")+ggplot2::xlab("Year")+ggplot2::ylab("Number of Stocks by Status (%)")
  plot(plot_Number)
  #dev.off()

  mat0<-data.frame(mat)
  nn<-colnames(mat0)
  rr<-ncol(mat0)
  tt3<-reshape2::melt(mat0,id.vars=nn[1],measure.vars=nn[c(2:rr)])
  nn<-names(data1)
  tt4<-reshape2::melt(data1,id.vars=nn[1],measure.vars=nn[c(2:rr)])
  tt5<-merge(tt3,tt4,by=c("Year","variable"))
  tt6<-tapply(tt5$value.y,c(list(tt5$Year),list(tt5$value.x)),FUN="sum")
  tt7<-data.frame("Year"=as.numeric(dimnames(tt6)[[1]]),"Developing"=rep(0,nrow(tt6)),"FullyExploited"=rep(0,nrow(tt6)),"Overfished"=rep(0,nrow(tt6)))
  for(i in 1:ncol(tt6))
  {
    tt7[,i+1]=tt6[,i]
  }
  tt7[is.na(tt7)]<-0
  mat2_c<-tt7[,-1]
  mat21_c<-(mat2_c/rowSums(mat2_c))*100
  mat4_c<-utils::stack(data.frame(mat21_c))
  Year1<-utils::stack(data.frame(rep(mat11,3)))##5 is replaced by 3
  Year<-as.numeric(Year1$values)
  Pecentage_Catch<-data.frame(cbind(Year,mat4_c))
  colnames(Pecentage_Catch)<-c("Year","Catch","Status")
  Pecentage_Catch[order(Pecentage_Catch$Year, decreasing = F), ]
  Catch=Pecentage_Catch$Catch
  #tiff(filename = "IndiaGroupCatchFSS1.tiff",width = 7,height = 8,units = "in",res = 300)
  plot_Catch<-ggplot2::ggplot(Pecentage_Catch, ggplot2::aes(x=factor(Year), y=Catch, fill=Status,group=Status)) + ggplot2::geom_area() +ggplot2::scale_fill_brewer(palette="RdYlGn",direction=-1)+
    ggplot2::theme(legend.position = "bottom")+ggplot2::xlab("Year")+ggplot2::ylab("Catch by Stock Status (%)")
 # print(plot_Number)
  print(plot_Catch)
  #dev.off()
}
