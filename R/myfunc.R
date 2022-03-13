#行列最小最大----
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
med.t=function(x){median(x,na.rm = T)}
mean.t=function(x){mean(x,na.rm = T)}
sum.t=function(x){sum(x,na.rm = T)}
#1是，其他0 函数----
mto1<-function(x){x=ifelse(is.na(x),0,ifelse(x==1,1,0))}
#1是，其他2 函数----
mto2<-function(x){x=ifelse(is.na(x),0,ifelse(x==2,2,0));x=ifelse(x==2,1,x)}

#删除中国各省名称后缀----
proz<-function(x){
  x=gsub('省','',x)%>%gsub('自治区','',.)%>%
    gsub('特别行政区','',.)%>%gsub('维吾尔','',.)%>%
    gsub('回族','',.)%>%gsub('壮族','',.)%>%gsub('市','',.)
  return(x)
}

#统计指标系列----
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

#身份证转出生日期函数----
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

#变量关系清理，输出异常大于条件 特殊分析使用，数据集n2----
clean<-function(x,y){
  num1=which(names(n2)==x)
  num2=which(names(n2)==y)
  new=n2%>%filter(n2[,num1]>n2[,num2])
  new=new[,c(4,num1,num2)]
}
#日期清理函数 ，把POSIXct 格式批量转成  Date 格式
daclean<-function(data){
 data=as.data.frame(data)
for(i in 1:ncol(data)){
  if (class(data[,i])[1]=="POSIXct")
  data[,i]=as.Date(data[,i])}
return(data)
}

#日期转换系列----
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
  x<-as.Date(x,origin="1899-12-30") #即起始日期 设置为1899-12-30
}

#年级转学段----
gradeclass<-function(x){
  oldgrade<-c('小班','中班','学前班','大班','小一','小二','小三','小四','小五',
              '小六','初一','初二','初三','高一','高二','高三','职高一',
              '职高二','职高三')
  gradeclass<-c('学前阶段','学前阶段','学前阶段','学前阶段','小学阶段','小学阶段',
                '小学阶段','小学阶段','小学阶段','小学阶段','初中阶段','初中阶段',
                '初中阶段','高中阶段','高中阶段','高中阶段','高中阶段','高中阶段',
                '高中阶段')
  x<-mapvalues(x,oldgrade,gradeclass)
  x<-factor(x,levels=c("学前阶段","小学阶段","初中阶段","高中阶段"))
}

xzclass=function(x){
  mapvalues(x,c('大班','小一','小二','小三','小四','小五','小六',
                '初一','初二','初三','高一','高二','高三'),
            c('幼儿园大班','小学','小学','小学','小学','小学','小学',
              '初中','初中','初中','高中','高中','高中'))
}


#查询重复值函数----
chongfu.yes<-function(dd) {
  tt<-table(dd$pid)
  qvar<-tt[tt>1]
  return(dd[dd$pid %in% names(qvar),])
}
#剔除重复值函数----
chongfu.no<-function(dd) {
  tt<-table(dd$pid)
  qvar<-tt[tt==1]
  return(dd[dd$pid %in% names(qvar),])
}



#求y 在x向量中的百分位数位置，digit是数字最小粒度----
R100<-function(x,y,digit){
  i=100
  while (quantile(x,i/100,na.rm = T)-y>digit) {i;i=i-1}
  i
}

#3类表格生成，使用tapply结果分3类 ----
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
      cat(" Consistency test is OK！\n")
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

#查找元素在矩阵中的位置，返回行列----
wei_zhi<-function(x,data){
  s<-c(t(data))
  n<-dim(data)
  m<-which(s==x)
  row<-m%/%n[2]+1
  col<-m-m%/%n[2]*n[2]
  c(row,col)}

#百度步行距离测算----
#disget函数利用 百度api 获取步行距离
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

#身份证校验，返回正确----
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

#自编统计数据框各变量缺失情况函数----
tab_miss=function(x){
  t1=lapply(x, function(y){table(is.na(y))})%>%bind_rows
  t2=data.frame(变量=names(x),t1)
  return(t2)
}

#身份证转男女
id2sex=function(x){
  if (!is.na(x)) {
    s=substr(x,17,17) %>% as.numeric
    s1=s%%2
    sex=ifelse(s1==1,'男','女')
  }
}
#自编统计分类函数----
df.10=function(x){
  N=length(x)
  y=length(x[x==1])
  n=length(x[x==0])
  yp=round(y*100/N,1)%>% sprintf('%1.1f',.)
  np=round(n*100/N,1)%>% sprintf('%1.1f',.)
  yes=paste(y," ( ",yp," ) ",sep='')
  no=paste(n," ( ",np," ) ",sep='')
  return(c(阳性=yes,阴性=no,合计=N))
}


#根据logi回归结果 计算OR及CI
Logioutput<-function(fit){
  #取P值
  p<-summary(fit)$coefficients[,4]
  #wald值
  wald<-summary(fit)$coefficients[,3]^2
  #B值
  valueB<-coef(fit)
  #OR值
  valueOR<-exp(coef(fit))
  #OR值得95%CI
  confitOR<-exp(confint(fit))
  data.frame(
    B=round(valueB,3),
    Wald=round(wald,3),
    OR_with_CI=paste(round(valueOR,3),"(",
                     round(confitOR[,1],3),"~",round(confitOR[,2],3),")",sep=""),
    P=format.pval(p,digits = 3,eps=0.001)
  )
}
#生成3月8日这种格式日期----
mddate=function(x){
  require(lubridate)
  m=month(x);d=day(x)
  return(paste(m,'月',d,'日',sep=''))
}
