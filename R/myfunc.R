#������С���----
rowMax<-function(x){if (NCOL(x)>1)
  {y<-apply(x,1,function(t){max(t,na.rm = T)})}
  else y=x
return(y)
}
rowMed<-function(x){if (NCOL(x)>1)
  {y<-apply(x,1,function(t){median(t,na.rm = T)})}
  else y=x
return(y)
}
rowMin<-function(x){if (NCOL(x)>1)
  {y<-apply(x,1,function(t){min(t,na.rm = T)})}
  else y=x
return(y)
}

max.t=function(x){max(x,na.rm = T)}
min.t=function(x){min(x,na.rm = T)}
mean.t=function(x){mean(x,na.rm = T)}
sum.t=function(x){sum(x,na.rm = T)}
#1�ǣ�����0 ����----
mto1<-function(x){x=ifelse(is.na(x),0,ifelse(x==1,1,0))}
#1�ǣ�����2 ����----
mto2<-function(x){x=ifelse(is.na(x),0,ifelse(x==2,2,0));x=ifelse(x==2,1,x)}

#ɾ���й���ʡ���ƺ�׺----
proz<-function(x){
  x=gsub('ʡ','',x)%>%gsub('������','',.)%>%
    gsub('�ر�������','',.)%>%gsub('ά���','',.)%>%
    gsub('����','',.)%>%gsub('׳��','',.)%>%gsub('��','',.)
  return(x)
}

#ͳ��ָ��ϵ��----
dmean<-function(x){mean=round(mean(x[!is.na(x)]),2)}
dme.sd<-function(x){c(mean=round(mean(x[!is.na(x)]),2),
                      sd=round(sd(x[!is.na(x)]),2))}
dsd<-function(x){sd=round(sd(x[!is.na(x)]),2)}
dn.me.sd<-function(x){round(c(n=length(x[!is.na(x)]),
                              mean=mean(x[!is.na(x)]),
                              sd=sd(x[!is.na(x)])),2)}
dM.IQR<-function(x){c(med=round(median(x),2),IQR=round(IQR(x),2))}
Iqr<-function(x){IQR=round(IQR(x),2)}
cv<-function(x){round(100*sd(x[!is.na(x)])/mean(x[!is.na(x)]),2)}
dme.cv<-function(x){round(c(mean=mean(x[!is.na(x)]),
                            cv=100*sd(x[!is.na(x)])/mean(x[!is.na(x)])),2)}
dn.me.cv<-function(x){c(n=length(x[!is.na(x)]),
                        round(c(mean=mean(x[!is.na(x)]),
                                cv=100*sd(x[!is.na(x)])/mean(x[!is.na(x)])),2))}
dn.mi.M.ma<-function(x){c(n=length(x[!is.na(x)]),
                          min=min(x,na.rm=T),
                          median=median(x,na.rm = T),
                          max=max(x,na.rm = T),
                          na=length(x[is.na(x)]))}
dn.all<-function(x){c(N=length(x[!is.na(x)]),
                      mean=round(mean(x,na.rm=T),2),
                      sd=round(sd(x,na.rm = T),2),
                      median=median(x,na.rm = T),
                      IQR=IQR(x,na.rm = T),
                      min=min(x,na.rm=T),
                      max=max(x,na.rm = T))}
dM.all<-function(x){c(N=length(x[!is.na(x)]),
                      median=median(x,na.rm = T),
                      IQR=IQR(x,na.rm = T),
                      min=min(x,na.rm=T),
                      max=max(x,na.rm = T)
)}

#����֤ת�������ں���----
id2bir<-function(x) {
  lx=nchar(x)
  y=substr(x,7,10)
  m=substr(x,11,12)
  d=substr(x,13,14)
  ymd=paste(y,m,d,sep="-")
  x=as.Date(ymd,"%Y-%m-%d")
  x[!lx==18]=NA
  return(x)
}

#������ϵ����������쳣�������� �������ʹ�ã����ݼ�n2----
clean<-function(x,y){
  num1=which(names(n2)==x)
  num2=which(names(n2)==y)
  new=n2%>%filter(n2[,num1]>n2[,num2])
  new=new[,c(4,num1,num2)]
}
#������������ ����POSIXct ��ʽ����ת��  Date ��ʽ
daclean<-function(data){vlist=names(data)
for(i in vlist){if (class(data[,i])[1]=="POSIXct") data[,i]=as.Date(data[,i])}
return(data)
}

#����ת��ϵ��----
date.spss2R<-function(x){
  x0<-as.Date("1582-10-14")
  x1<-x/(24*3600)
  x2<-as.Date(x0+x1)
}
date.FtoD<-function(x){
  x<-as.character(x)
  x<-as.Date(x)
}
date.FtoN<-function(x){
  x<-as.character(x)
  x<-as.numeric(x)
}
date.excel2R<-function(x){
  x<-as.character(x)
  x<-as.numeric(x)
  x<-as.Date(x,origin="1899-12-30") #����ʼ���� ����Ϊ1899-12-30
}

#�꼶תѧ��----
gradeclass<-function(x){
  oldgrade<-c('С��','�а�','ѧǰ��','���','Сһ','С��','С��','С��','С��',
              'С��','��һ','����','����','��һ','�߶�','����','ְ��һ',
              'ְ�߶�','ְ����')
  gradeclass<-c('ѧǰ�׶�','ѧǰ�׶�','ѧǰ�׶�','ѧǰ�׶�','Сѧ�׶�','Сѧ�׶�',
                'Сѧ�׶�','Сѧ�׶�','Сѧ�׶�','Сѧ�׶�','���н׶�','���н׶�',
                '���н׶�','���н׶�','���н׶�','���н׶�','���н׶�','���н׶�',
                '���н׶�')
  x<-mapvalues(x,oldgrade,gradeclass)
  x<-factor(x,levels=c("ѧǰ�׶�","Сѧ�׶�","���н׶�","���н׶�"))
}


#��ѯ�ظ�ֵ����----
chongfu.yes<-function(dd) {
  tt<-table(dd$pid)
  qvar<-tt[tt>1]
  return(dd[dd$pid %in% names(qvar),])
}
#�޳��ظ�ֵ����----
chongfu.no<-function(dd) {
  tt<-table(dd$pid)
  qvar<-tt[tt==1]
  return(dd[dd$pid %in% names(qvar),])
}



#��y ��x�����еİٷ�λ��λ�ã�digit��������С����----
R100<-function(x,y,digit){
  i=100
  while (quantile(x,i/100,na.rm = T)-y>digit) {i;i=i-1}
  i
}

#3��������ɣ�ʹ��tapply�����3�� ----
tppform<-function(x){
  resu<-data.frame()
  for (i in 1:length(x)){
    t1<-x[i]%>%as.data.frame%>%t
    resu<-rbind(resu,t1)
  }
  return(resu)
}

myCI<-function (B)
{
  RI = c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49,
         1.51)
  Wi = weight(B)
  n = length(Wi)
  if (n > 2) {
    W = matrix(Wi, ncol = 1)
    A = matrix(B, nrow = sqrt(length(B)), ncol = sqrt(length(B)),
               byrow = TRUE)
    AW = A %*% W
    aw = as.vector(AW)
    la_max = sum(aw/Wi)/n
    CI = (la_max - n)/(n - 1)
    CR = CI/RI[n]
    if (CR <= 0.1) {
      cat(" Consistency test is OK��\n")
      result<-cbind(Wi,CI,CR,la_max) %>% round(.,4)
    }
    else {
      result<-data.frame(Wi)
    }
  }
  else if (n <= 2) {
    return(Wi)
  }
}

#����Ԫ���ھ����е�λ�ã���������----
wei_zhi<-function(x,data){
  s<-c(t(data))
  n<-dim(data)
  m<-which(s==x)
  row<-m%/%n[2]+1
  col<-m-m%/%n[2]*n[2]
  c(row,col)}

#�ٶȲ��о������----
#disget�������� �ٶ�api ��ȡ���о���
disget<-function(ori,des){
  require(rjson)
  require(jsonlite)
  require(philentropy)
  uhead="http://api.map.baidu.com/routematrix/v2/walking?output=json"
  myAK="FWSL3MSwWw18qOqMvizGSmlyxrnSEgSA"
  url<-paste(uhead,"&origins=",ori,"&destinations=",des,"&ak=",myAK,sep = "")
  x1<-fromJSON(url)
  x2<-x1$result$distance$value
  return(x2)}

#����֤У�飬������ȷ----
jsuan<-function(x){
  x1=as.numeric(substr(x,1,1))*7;
  x2=as.numeric(substr(x,2,2))*9;
  x3=as.numeric(substr(x,3,3))*10;
  x4=as.numeric(substr(x,4,4))*5;
  x5=as.numeric(substr(x,5,5))*8;
  x6=as.numeric(substr(x,6,6))*4;
  x7=as.numeric(substr(x,7,7))*2;
  x8=as.numeric(substr(x,8,8))*1;
  x9=as.numeric(substr(x,9,9))*6;
  x10=as.numeric(substr(x,10,10))*3;
  x11=as.numeric(substr(x,11,11))*7;
  x12=as.numeric(substr(x,12,12))*9;
  x13=as.numeric(substr(x,13,13))*10;
  x14=as.numeric(substr(x,14,14))*5;
  x15=as.numeric(substr(x,15,15))*8;
  x16=as.numeric(substr(x,16,16))*4;
  x17=as.numeric(substr(x,17,17))*2;
  x18=sum(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17)%%11
  x18=mapvalues(x18,c("0","1","2","3","4","5","6","7","8","9","10"),c("1","0","X","9","8","7","6","5","4","3","2"))
  x<-paste(substr(x,1,17),x18,sep = "")
  return(x)
}

#�Ա�ͳ�����ݿ������ȱʧ�������----
tab_miss=function(x){
  t1=lapply(x, function(y){table(is.na(y))})%>%bind_rows
  t1$����=names(x)
  return(t1[,c(3,1,2)])
}