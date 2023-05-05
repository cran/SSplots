
# @importFrom zoo rollmean
# @importFrom ggplot2 ggplot aes geom_area ggtitle  xlab ylab labs theme ggsave geom_hline scale_fill_brewer
# @importFrom reshape2 melt
#' @export
SSplots_kleisner<- function (data,lower.lt,upper.lt, tsplots,MA)
{
  data0<-as.data.frame(data)
  if (MA==TRUE)
  {
    data1<-as.data.frame(zoo::rollmean(data0,k=3))
  }
  else
  {
    data1<-as.data.frame(data0)
  }
  data2<-data1[,-1]
  Year<-data1[,1]
  lw<-0
  up<-0
  Post.max.min<-0
  Post.max.min.Yr<-0
  mat = matrix(ncol = ncol(data1), nrow = nrow(data1))
  colnames(mat)=colnames(data1)
  mat[,1]=data1[,1]
  outFname<-"Time Series Plot-"
  for (l in 1:ncol(data2))
  {
    lw[l]<-max(data1[,l+1])*(lower.lt/100)
    up[l]<-max(data1[,l+1])*(upper.lt/100)
    if (tsplots==TRUE)
    {
      ggplot2::ggplot(data1, ggplot2::aes(x = factor(Year), y = data2[ , l],group=1))+
        ggplot2::geom_area(color="darkblue",fill="lightblue",linetype=1,size=1)+
        ggplot2::geom_hline(ggplot2::aes(yintercept = lw[l], color=paste(lower.lt,"% of the maximum landings")),linetype=2,lwd=1)+
        ggplot2::geom_hline(ggplot2::aes(yintercept = up[l], color=paste(upper.lt,"% of the maximum landings")),linetype="twodash",lwd=1)+
        ggplot2::ggtitle("Time Series Plot")+ggplot2::xlab("Year")+ggplot2::ylab("Catch/Landings")+ggplot2::labs(color="")+ggplot2::theme(legend.position = "bottom")
      file_name=colnames(data2)[l]
      file_name=gsub("\\."," ",file_name)
       ggplot2::ggsave(filename=paste(outFname,file_name,".png",sep=""),height=5,width=7)

    }

    k=l
    for (m in 1:nrow(data2))
    {

      if(k<=ncol(data2))
      {

        if (data1[m,1]>data1[which(grepl(max(data1[,k+1]),data1[,k+1])),1])
        {
          Post.max.min[k] = min(data1[m:nrow(data1),k+1])
          Post.max.min.Yr[k] = data1[which(data1[,k+1]==min(data1[m:nrow(data1),k+1])),1]
          k=k+1

        }
        else{
          if (data1[nrow(data1),1]==data1[which(grepl(max(data1[,k+1]),data1[,k+1])),1])
          {
            Post.max.min[k] = 0
            Post.max.min.Yr[k] = data1[which(grepl(max(data1[,k+1]),data1[,k+1])),1]
            k=k+1
          }
        }

      }
    }
  }
  for (i in 1:ncol(data2))
  {
    lw[i]<-max(data1[,i+1])*(lower.lt/100)
    up[i]<-max(data1[,i+1])*(upper.lt/100)

    for (j in 1:nrow(data2))
    {
      if(data1[j,1]>Post.max.min.Yr[i] & Post.max.min[i]<lw[i] & (data2[j,i]>=lw[i] & data2[j,i]<=up[i]))
        mat[j,i+1]<-1
      else{
        if(data1[j,1]<data1[which(grepl(max(data1[,i+1]),data1[,i+1])),1] & (data2[j,i]<=up[i]) || (data1[which(grepl(max(data1[,i+1]),data1[,i+1])),1]==data1[nrow(data1),1]))
          mat[j,i+1]<-2
        else{
          if(data1[j,1]>data1[which(grepl(max(data1[,i+1]),data1[,i+1])),1] & (data2[j,i]>=lw[i] & data2[j,i]<=up[i]))
            mat[j,i+1]<-4
          else{
            if(data1[j,1]>data1[which(grepl(max(data1[,i+1]),data1[,i+1])),1] & data2[j,i]<up[i])
              mat[j,i+1]<-5
            if(is.na(mat[j,i+1])==T)
            {
              if(data1[j,i+1]>up[i])
                mat[j,i+1]<-3
            }
          }
        }
      }
    }


  }
  mat1<-as.data.frame(mat[ ,-1 ])
  mat11<-as.data.frame(mat[ ,1])
  colnames(mat11)<-"Year"
  Rebuilding<-rowSums(mat1==1)
  Developing<-rowSums(mat1==2)
  Exploited<-rowSums(mat1==3)
  Overexploited<-rowSums(mat1==4)
  Collapsed<-rowSums(mat1==5)
  mat2<-as.data.frame(cbind(Rebuilding,Developing,Exploited,Overexploited,Collapsed))
  mat21<-(mat2/rowSums(mat2))*100
  mat4<-utils::stack(as.data.frame(mat21))
  Year1<-utils::stack(as.data.frame(rep(mat11,5)))
  Year<-as.numeric(Year1$values)
  Pecentage_Count<-as.data.frame(cbind(Year,mat4))
  colnames(Pecentage_Count)<-c("Year","Count","Status")
  Pecentage_Count[order(Pecentage_Count$Year, decreasing = F), ]

  #mat4 <- Pecentage_Count  %>%
  #  +     group_by(Year, Status) %>%
  #  +     summarise(n = sum(Count)) %>%
  #  +     mutate(percentage = (n / sum(n)*100))

  #write.csv(mat, file = "CodedData.csv")
  Count=Pecentage_Count$Count
  Status=Pecentage_Count$Status
  #tiff(filename = "IndiaGroupNumFSS1.tiff",width = 7,height = 8,units = "in",res = 300)
  plot_Number<-ggplot2::ggplot(Pecentage_Count, ggplot2::aes(x=factor(Year), y=Count, fill=Status,group=Status)) + ggplot2::geom_area() +ggplot2::scale_fill_brewer(palette="RdYlGn",direction=-1)+
    ggplot2::theme(legend.position = "bottom")+ggplot2::xlab("Year")+ggplot2::ylab("Number of Stocks by Status (%)")

  #dev.off()

  mat0<-as.data.frame(mat)
  nn<-colnames(mat0)
  rr<-ncol(mat0)
  tt3<-reshape2::melt(mat0,id.vars=nn[1],measure.vars=nn[c(2:rr)])
  nn<-names(data1)
  tt4<-reshape2::melt(data1,id.vars=nn[1],measure.vars=nn[c(2:rr)])
  tt5<-merge(tt3,tt4,by=c("Year","variable"))
  tt6<-tapply(tt5$value.y,c(list(tt5$Year),list(tt5$value.x)),FUN="sum")
  tt7<-data.frame("Year"=as.numeric(dimnames(tt6)[[1]]),"Rebuilding"=rep(0,nrow(tt6)),"Developing"=rep(0,nrow(tt6)),"Exploited"=rep(0,nrow(tt6)),"Overexploited"=rep(0,nrow(tt6)),"Collapsed"=rep(0,nrow(tt6)))
  for(i in 1:ncol(tt6))
  {
    tt7[,i+1]=tt6[,i]
  }
  tt7[is.na(tt7)]<-0
  mat2_c<-tt7[,-1]
  mat21_c<-(mat2_c/rowSums(mat2_c))*100
  mat4_c<-utils::stack(as.data.frame(mat21_c))
  Year1<-utils::stack(as.data.frame(rep(mat11,5)))
  Year<-as.numeric(Year1$values)
  Pecentage_Catch<-as.data.frame(cbind(Year,mat4_c))
  colnames(Pecentage_Catch)<-c("Year","Catch","Status")
  Pecentage_Catch[order(Pecentage_Catch$Year, decreasing = F), ]
  Catch=Pecentage_Catch$Catch
  #tiff(filename = "IndiaGroupCatchFSS1.tiff",width = 7,height = 8,units = "in",res = 300)
  plot_Catch<-ggplot2::ggplot(Pecentage_Catch, ggplot2::aes(x=factor(Year), y=Catch, fill=Status,group=Status)) + ggplot2::geom_area() +ggplot2::scale_fill_brewer(palette="RdYlGn",direction=-1)+
    ggplot2::theme(legend.position = "bottom")+ggplot2::xlab("Year")+ggplot2::ylab("Catch by Stock Status (%)")
  print(plot_Number)
  print(plot_Catch)
  #dev.off()
}

