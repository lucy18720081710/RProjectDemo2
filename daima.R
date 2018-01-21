#第一章
install.packages("tm",dependencies=TRUE)
library(tm)
install.packages("XML",suggests=TRUE)
#下载并安装初始安装过程所依赖的程序包
print(require(XML))
install.packages("RCurl_1.5-0.tar.gz",
                 repos=NULL,type="source")
#不要使用CRAN中任意一个资源库，使用源代码安装
install.packages("arm")
install.packages("ggplot2")
install.packages("glmnet")
install.packages("igraph")
install.packages("lme4")
install.packages("lubridata")
install.packages("reshape")
install.packages("RJSONIO")
source("package_installer.R")


?read.delim
??base::delim
help.search("delimited")
RSiteSearch("parsing text")

setwd("D:/R/Rjiaoben/anlijiexi")#更改工作路径
getwd()#查看当前工作目录
library(ggplot2)

#告诉函数这个.tsv文件用制表符进行分割
#字符串默认为factor类型，因此stringsAsFactors置FALSE防止转换
#header置FALSE防止将第一行当做表头
#定义空元素定义为特殊字符NA：na.strings = ""
ufo <- read.delim("01-Introduction/data/ufo/ufo_awesome.tsv",
                  sep="\t",stringsAsFactors=FALSE,
                  header=FALSE,na.strings="")

head(ufo)
tail(ufo)#查看后六条数据

#name()函数既可以写入列名，也可以读取列名
names(ufo) <- c("DateOccurred","DateReported","Location",
                "ShortDescription","Duration","LongDecription")

#as.Date()函数可以将日期字符串转换为日期数据类型，也就是Date对象
ufo$DateOccurred <- as.Date(ufo$DateOccurred,
                            format="%Y%m%d")
#错误，输入过长，考虑有畸形数据
#畸形数据处理
head(ufo[which(nchar(ufo$DateOccurred)!=8
               |nchar(ufo$DateReported)!=8),1])

#新建向量，ifelse()函数来构建一个布尔向量，布尔值F为不符合要求的行
#计数不符要求的行数，并只留下符合要求的行
good.rows <- ifelse(nchar(ufo$DateOccurred)!=8
                    |nchar(ufo$DateReported)!=8,FALSE,TRUE)
length(which(!good.rows))
ufo <- ufo[good.rows,]

#继续转换
ufo$DateOccurred <- as.Date(ufo$DateOccurred,format="%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported,format="%Y%m%d")
head(ufo)


#首先定义一个输入为字符串的函数，然后执行数目击地点清洗工作
#strsplit用于分割字符串，在遇到不符条件的字符串会抛出异常，有tryCatch捕获，并返回缺失
#gsub将原始数据中的空格去掉（通过替换）
#条件语句用于检查是否多个逗号，返回缺失
get.location <- function(l){
  split.location <- tryCatch(strsplit(l,",")[[1]],
                             error=function(e) return(c(NA,NA)))
  clean.location <- gsub("^ ","",split.location)
  if(length(clean.location)>2){
    return(c(NA,NA))
  }
  else{
    return(clean.location)
  }
}

#lapply(也就是list-apply)将function逐一用到向量元素上，并返回链表(list)
city.state <- lapply(ufo$Location,get.location)
head(city.state)

#把城市和州信息作为不同的两列加入到数据框中
#将list转换成matrix
#do.call在一个list上执行一个函数调用
#rbind函数会把city.state链表中的所有向量一行一行的合并起来
#transform函数给ufo创建两个新列，tolower函数将大写变小写，为了统一格式
location.matrix <- do.call(rbind,city.state)
ufo <- transform(ufo,USCity=location.matrix[,1],
                 USState=tolower(location.matrix[,2]),
                 stringdAsFactoes=FALSE)

#构造一个美国各州缩写的向量，让USState列数据来匹配这个向量
#把匹配上的保留下来，从而识别非美国地名，并置为NA
us.states <- c("ak", "al", "ar", "az", "ca", "co", "ct", "de", "fl", "ga", "hi", "ia", "id",
               "il", "in", "ks", "ky", "la", "ma", "md", "me", "mi", "mn", "mo", "ms", "mt",
               "nc", "nd", "ne", "nh", "nj", "nm", "nv", "ny", "oh", "ok", "or", "pa", "ri",
               "sc", "sd", "tn", "tx", "ut", "va", "vt", "wa", "wi", "wv", "wy")
ufo$USState <- us.states[match(ufo$USState,us.states)]
ufo$USCity[is.na(ufo$USState)] <-NA

#用subset命令创建一个新数据框，只保留发生在美国的数据记录
ufo.us <- subset(ufo,!is.na(USState))
head(ufo.us)

summary(ufo.us$DateOccurred)
#构建一个直方图，把数据按照时间进行区间划分
#x轴设定为DateOccurred列
#geom_histogram()函数添加一个直方图层
#scale_x_date()函数将x轴标签的时间周期改为50年
quick.hist <- ggplot(ufo.us,aes(x=DateOccurred))+geom_histogram()+scale_x_date(date_breaks="50 years")
print(quick.hist)

#subset()函数把符合条件的数据挑出来以构建一个新的数据框
#取出1990年后的数据并作图
ufo.us <- subset(ufo.us,DateOccurred>=as.Date("1990-01-01"))
nrow(ufo.us)
quick.hist.new <- ggplot(ufo.us,aes(x=DateOccurred))+geom_histogram()+scale_x_date(date_breaks="50 years")
print(quick.hist.new)

#统计每个年—月的目击个数
#strftime()函数把日期对象转换成一个"YYYY-MM"格式的字符串
ufo.us$YearMonth <- strftime(ufo.us$DateOccurred,format="%Y-%m")

#用nrow函数按照行数来化简每组数据
library(plyr)
sightings.counts <- ddply(ufo.us,.(USState,YearMonth),nrow)

#用seq.Date函数创建一个日期序列，把格式设定为数据库的日期格式
date.range <- seq.Date(from=as.Date(min(ufo.us$DateOccurred)),
                       to=as.Date(max(ufo.us$DateOccurred)),by="month")
date.strings <- strftime(date.range,"%Y-%m")

#新建一个包含所有年-月和州的数据框，用这个数据框去匹配ufo目击数据
#先用lapply函数穿件列，再用do.call函数将其转换成矩阵并最终转换成数据框
states.dates <- lapply(us.states,function(s) cbind(s,date.strings))
states.dates <- date.frame(do.call(rbind,states.dates),stringsAsFactors=FALSE)
head(states.dates)


#mrege()函数将两个数据框合并
#传入两个数据框，可以将相同的列合并，by.x和by.y指定列名
#all=TRUE将未匹配处填充为NA
all.sightings <- merge(states.dates,sightings.counts,
                       by.x=c("s","date.strings"),
                       by.y=c("USState","YearMonth"),all=TRUE)
head(all.sightings)


#把all.sightings数据框的列名改为有意义的名称
#把NA值改为0
#把年-月字符串转换为Date对象，rep函数创建一个和date.range一模一样的向量
#as.factor把州名的字符串形式改为分类变量
names(all.sightings) <- c("State","YearMonth","Sightings")
all.sightings$Sightings[id.na(all.sightings$Sightings)] <- 0
all.sightings$YearMonth <- as.Date(rep(date.range,length(us.states)))
all.sightings$State <- as.factor(toupper(all.sightings$State))


#可视化方法分析数据
#geom_line表示曲线图，facet_wrap用于创建绘制的图形，并使用分类变量State
#theme_bw设定了图形背景主题
#scale_color_manual定义第二行中字符串"darkblue"的值，这个值相当于"darkblue"对应的值
state.plot <- ggplot(all.sightings,
                     aes(x=YearMonth,y=Sightings))+
  geom_line(aes(color="darkblue"))+
  facet_wrap(~State,nrow=10,ncol=5)+
  theme_bw()+
  scale_color_manual(values=c("darkblue"="darkblue"),guide="none")+
  scale_x_date(breaks="10 years")+
  xlab("Time")+
  ylab("Number of Sightings")+
  ggtitle("Number of ufo sightings by month-year and U.S. state (1990-2010)")
print(state.plot)


#第2章：数据分析

setwd("D:/R/Rjiaoben/anlijiexi/02-Exploration")
getwd()
data.file <- file.path("data","01_heights_weights_genders.csv")
heights.weights <- read.csv(data.file,header=TRUE,sep=",")
#前面两行代码可以合并成下面这条代码
heights.weights <- read.csv("data/01_heights_weights_genders.csv",header=TRUE,sep=",")
names(heights.weights) <- c("gender","height","weight")
summary(heights.weights$height)
min(heights.weights$height)
max(heights.weights$weight)
c(min(heights.weights$height),max(heights.weights$height))
renge(heights.weights$height)
quantile(heights.weights$height)
quantile(heights.weights$height,prob=seq(0,1,by=0.2))
#用sep函数在0~1之间产生一个步长为0.2的序列
seq(0,1,by=0.2)
c(quantile(heights.weights$height,probs=0.25),quantile(heights.weights$height,probs=0.75))
c(mean(heights.weights$height)-sd(heights.weights$height),mean(heights.weights$height)+sd(heights.weights$height))

library("ggplot2")
#不同区间宽度的直方图
ggplot(heights.weights,aes(x=height))+geom_histogram(binwidth=1)
ggplot(heights.weights,aes(x=height))+geom_histogram(binwidth=5)
ggplot(heights.weights,aes(x=height))+geom_histogram(binwidth=0.001)

#密度曲线图
ggplot(heights.weights,aes(x=height))+geom_density()

#峰值处平坦，考虑图像有结构缺失，根据性别分别绘制密度曲线图
#构建一个密度曲线图，它由两个密度曲线叠加而成
ggplot(heights.weights,aes(x=height,fill=gender))+geom_density()

#把密度曲线分片，就能看到两个单独的钟形曲线
ggplot(heights.weights,aes(x=weight,fill=gender))+geom_density()+facet_grid(gender~.)


m <- 0
s <- 1
ggplot(data.frame(X=rnorm(100000,m,s)),aes(x=X))+geom_density()

#正态分布：钟形的窄尾分布，单峰对称
#柯西分布：钟形的重尾分布，单峰对称
set.seed(1)
normal.values <- rnorm(250,0,1)
cauchy.values <- rcauchy(250,0,1)
range(normal.values)
range(cauchy.values)
ggplot(data.frame(X=normal.values),aes(x=X))+geom_density()
ggplot(data.frame(X=cauchy.values),aes(x=X))+geom_density()

#有点偏斜的伽玛分布,伽玛分布只有正值
gamma.values <- rgamma(100000,1,0.001)
ggplot(data.frame(X=gamma.values),aes(x=X))+geom_density()
#非常偏斜的指数分布，数据集中频数最高的是0，并且只有非负值出现

#身高和体重的散点图
ggplot(heights.weights,aes(x=height,y=weight))+geom_point()
#带平滑线性拟合的身高和体重散点图
ggplot(heights.weights,aes(x=height,y=weight))+geom_point()+geom_smooth()
#移除一些数据，观察模式如何变得越来越不明显
ggplot(heights.weights[1:20,],aes(x=height,y=weight))+geom_point()+geom_smooth()
ggplot(heights.weights[1:200,],aes(x=height,y=weight))+geom_point()+geom_smooth()
ggplot(heights.weights[1:2000,],aes(x=height,y=weight))+geom_point()+geom_smooth()

#根据性别来划分两个不同的群体
ggplot(heights.weights,aes(x=height,y=weight,color=gender))+geom_point()

#从身高体重来预测性别（分类器）

heights.weights <- transform(heights.weights,Male=ifelse(
  gender=="Male",1,0))
logit.model <- glm(Male~height+weight,data=heights.weights,
                   family=binomial(link="logit"))
ggplot(heights.weights,aes(x=height,y=weight,color=gender))+
  geom_point()+
  geom_abline(intercept=-coef(logit.model)[1]/coef(logit.model)[2],
              slope=-coef(logit.model)[3]/coef(logit.model)[2],
              geom="abline",color="black")                                              



#第三章  分类：垃圾过滤
#本章使用的文本分类算法叫做朴素贝叶斯分类器

#初始化工作：为所有的邮件数据文件设置路径变量
library(tm)
library(ggplot2)
getwd()
setwd("D:/R/Rjiaoben/anlijiexi")
spam.path <- "ML_for_Hackers-master/03-Classification/data/spam/"
spam2.path <- "ML_for_Hackers-master/03-Classification/data/spam_2/"
easyham.path <- "ML_for_Hackers-master/03-Classification/data/easy_ham/"
easyham2.path <- "ML_for_Hackers-master/03-Classification/data/easy_ham_2/"
hardham.path <- "ML_for_Hackers-master/03-Classification/data/hard_ham/"
hardham2.path <- "ML_for_Hackers-master/03-Classification/data/hard_ham_2"

#将两种类型邮件转换成文本语料库，构建垃圾邮件和正常邮件的特征词项类别知识库
#写一个函数用它来打开每一个文件，找到空行，并将该空行之后的文本返回为一个字符串向量
#这个向量只有一个元素，就是空行之后的所有文本拼接之后的字符串
#file用于打开文件，rt代表以文本形式读取
#由于邮件可能包含非ACSII码字符，设置encoding="latin1"
#readLines按行读入文件
#定位到第一个空行""并抽取后面的所有文本
#有些文件中未包含空行，会抛出错误，因此用tryCatch捕获这些错误并返回NA
#关闭文件，将所有行合并为一行并返回该向量
get.msg <- function(path){
  con <- file(path,open="rt",encoding="latin1")
  text <- readLines(con)
  msg <- tryCatch(text[seq(which(text=="")[1]+1,length(text),1)],
                  error=function(e) return(NA))
  close(con)
  return(paste(msg,collapse="\n"))
}

#创建向量保存所有正文，向量的每个元素就是一封邮件的内容
#用dir函数得到路径下所有文件名列表，除掉cmds文件
#应用sapply函数时，先传入一个无名函数，目的是用paste函数把文件名和适当的路径拼接起来
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs!="cmds")]
all.spam <- sapply(spam.docs,
                   function(p) get.msg(paste(spam.path,p,sep="")))
head(all.spam)

#定义一个函数get.tdm,该函数输入文本向量，输出TDM(N*M矩阵)
#矩阵行表示词汇，列表示文档，元素[i,j]代表词汇i在文档j中出现的次数
#Corput函数用于构建语料库(corput对象)
#VectorSource用向量构建source对象
#source对象是用来创建语料库的数据源对象
#control变量是一个选项列表，用于设定提取文本的清洗规则
#stopwords移除停用词，removePunctuation,removeNumbers分别移除标点和数字
#minDocFreq设定最小两次出现的词才出最终出现在TDM中
library(tm)
get.tdm <- function(doc.vec){
  doc.corpus <- Corpus(VectorSource(doc.vec))
  control <- list(stomwords=TRUE,removePuctuation=TRUE,
                  removeNumbers=TRUE,minDocFreq=2)
  doc.dtm <- TermDocumentMatrix(doc.corpus,control)
  return(doc.dtm)}

spam.tdm <- get.tdm(all.spam)

#用TDM来构建垃圾邮件的训练数据:构建数据框保存所有特征词在垃圾邮件中的条件概率
#先将spam.tdm转为标准矩阵，rowSums创建一个包含每个特征在所有文档中总频数的向量
#注意禁止字符自动转为因子
#修改列名，frequency转数字因子
spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts),as.numeric(spam.counts)),
                      stringsAsFactors=FALSE)
names(spam.df) <- c("term","frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)

#关键训练数据1：计算一个特定特征词所出现的文档在所有文档中所占比例
#sapply函数将行号传入无名函数，计算该行值为整数的元素个数，再除以文档总数(列数)
#关键训练数据2：统计整个语料库中每个词项的频数(不用于分类，但是可以通过对比频次知道某些词是否影响结果
#用transform函数把向量spam.occurrence和spam.density加入数据框中
spam.occurrence <- sapply(1:nrow(spam.matrix),
                          function(i){length(which(spam.matrix[i,]>0))/ncol(spam.matrix)})
spam.density <- spam.df$frequency/sum(spam.df$frequency)

spam.df <- transform(spam.df,density=spam.density,
                     occurrence=spam.occurrence)

#按照occurrence列降序排列并显示前6条
head(spam.df[with(spam.df,order(-occurrence)),])

#构建正常邮件的特征词项
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs!="cmds")]
#注意为了平衡数据，将正常邮件数据限定在500
easyham.docs <- easyham.docs[1:500]
all.easyham <- sapply(easyham.docs,
                      function(p) get.msg(paste(easyham.path,p,sep="")))
easyham.tdm <- get.tdm(all.easyham)

easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts),
                               as.numeric(easyham.counts)),stringsAsFactors=FALSE)
names(easyham.df) <- c("term","frequency")
easyham.df$frequency <-as.numeric(easyham.df$frequency)

easyham.occurrence <- sapply(1:nrow(easyham.matrix),
                             function(i) {length(which(easyham.matrix[i, ]>0))/ncol(easyham.matrix)})
easyham.density <- easyham.df$frequency/sum(easyham.df$frequency)
easyham.df <- transform(easyham.df,density=easyham.density,
                        occurrence=easyham.occurence)

#按照occurrence列降序排列并显示前6条
head(easyham.df[with(easyham.df,order(-occurrence)),])

#构造函数classify.email:输入文本返回这封邮件是垃圾邮件的贝叶斯概率估计值
#抽取正文、转换成TDM、计算特征词项频率
#先验概率默认为50%，未出现词的概率设为0.0001%
classify.email <- function(path,training.df,prior=0.5,c=1e-6){
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  #find intersections of words找到邮件中的词项盒现在训练中的词项的交集
  msg.match <- intersect(names(msg.freq),training.df$term)
  if(length(msg.match) <- 1){
    #如果没有任何词出现在垃圾邮件集中
    #length(msg.freq)是词的个数
    #返回的值很小，因为没有训练集中出现过的词 无法判断
    return(prior*c^(length(msg.freq)))
  }else{
    #交集中词的频率放到match.probs
    #用这些词的特征概率，计算这封邮件是训练集中对用类别的条件概率
    #返回值=是垃圾邮件的鲜艳概率*各重合词在垃圾邮件训练集中的概率积*缺失词项的小概率积
    match.probs <- training.df$occurrence[match(msg.match,training.df$tern)]
    return(prior*prod(match.probs)*c^(length(msg.freq)-length(msg.match)))
  }
}

#用不易分类的正常邮件进行测试
#依次得到所有文件的路径，再用sapply封装对垃圾邮件和正常邮件的测试，最后用不易识别的正常邮件测试分类器
#向量hardham.spamtes和hardham.hamtest中分别保存了每一封邮件再给定对应训练数据的前提下是垃圾或正常邮件的条件概率计算结果
#用ifelse命令比较这两个向量中的概率值，如果hardham.spamtest中的概率大于hardham.hamtest中对应的概率，
#那么分类器就判断为垃圾邮件，反之是正常邮件
hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs!="cmds")]
hardham.spamtest <- sapply(hardham.docs,
                           function(p) classify.email(file.path(hardham.path,p),
                                                      training.df=spam.df))
hardham.hamtest <- sapply(hardham.docs,
                          function(p) classify.email(file.path(hardham.path,p),
                                                     training.df=easyham.df))
hardham.res <- ifelse(hardham.spamtest>hardham.hamtest,TRUE,FALSE)
summary(hardham.res)


#用三种类型的邮件下标为2的邮件集进行测试

#spam.classifier函数将根据训练数据spam.df和easyham.df判断邮件是否为垃圾邮件
#如果是的概率大，函数就返回1，否则返回0
spam.classifier <- function(path){
  pr.spam <- classify.email(path,spam.df)
  pr.ham <- classify.email(path,easyham.df)
  return(c(pr.spam,pr.ham,ifelse(pr.spam>pr.ham,1,0)))
}


#用简易分类器测试垃圾邮件、易识别的正常邮件和不易识别的正常邮件的第二套数据
#把spam.classifier含糊封装再lapply函数中，传入文件路径，生成一个数据框
spam2.docs <- dir(spam2.path)
spam2.docs <- spam2.docs[which(spam2.docs!="cmds")]
easyham2.docs <- dir(easyham2.path)
easyham2.docs <- easyham2.docs[which(easyham2.docs!="cmds")]
hardhan2.docs <- dir(hardhamw.path)
hardham2.docs <- hardham2.docs[which(hardham2.docs!="cmds")]

spam2.class <- suppressWarnings(lapply(spam2.docs,
                                       function(p) spam.classifier(file.path(spam2.path,p))))
easyham2.class <- suppressWarnings(lapply(easyham2.docs,
                                          function(p) spam.classifier(file.path(easyham2.path,p))))
hardham2.class <- suppressWarnings(lapply(hardham2.docs,
                                          function(p) spam.classifier(file.path(hardham2.path,p))))

#lapply返回的是列表对象，需要转换为矩阵
class(hardham2.class)#查看数据类型

#将列表转化为矩阵
easyham2.matrix <- do.call(rbind,easyham2.class)
easyham2.final <- cbind(easyham2.matrix,"EASYHAM")
hardham2.matrix <- do.call(rbind,hardham2.class)
hardham2.final <- cbind(hardham2.matrix,"HARDHAM")
spam2.matrix <- do.call(rbind,spam2.class)
spam2.final <- cbind(spam2.matrix,"SPAM")

#组合所有的矩阵，将他们转化为数据框，给他们重新命名
class.matrix <- rbind(easyham2.final,hardham2.final,spam2.final)
class.df <- data.frame(class.matrix,stringsAsFactor=FALSE)
names(class.df) <- c("Pr.SPAM","Pr.HAM","Class","Type")

#设置stringAsFactor=FALSE后，数据框所有元素类型均为"character",因此需要单独更改
class(class.df$Pr.SPAM)#查看数据类型

class.df$Pr.SPAM <- as.numeric(class.df$Pr.spam)
class.df$Pr.HAM <- as.numeric(class.df$Pr.HAM)
class.df$Class <- as.logical(as.numeric(class.df$Class))
class.df$Type <- a.factor(class.df$Type)

#直线的绘制，需要使用"geom_abline"命令，设定截距使用"intercept"参数
class.plot <- ggplot(class.df,aes(x=log(Pr.HAM),log(Pr.SPAM)))+
  geom_point(aes(shape=Type,alpha=0.5))+
  geom_abline(intercept=0,slope=1)+
  scale_shape_manual(values=c("EASYHAM"=1,"HARDHAM"=2,"SPAM"=3),
                     name="Email Type")+
  scale_alpha(guide="none")+
  xlab("log[Pr(HAM)]"+ylab("log[Pr(SPAM)]")+theme_bw()+
         theme(axis.text.x=element_black(),axis.text.y=element_blank()))
print(class.plot)

#创建一个结果的表格
get.results <- function(bool.vector){
  results >- c(length(bool.vector[which(bool.vector==FALSE)])/lenght(bool.vectoe),
               lenght(bool.vector[which(bool.vector==TRUE)])/lenght(bool.vector))
  return(results)
}
easyham2.col <- get.results(subset(class.df,Type=="EASYHAM")$Class)
hardham2.col <- get.results(subset(class.df,type=="HARDHAM")$Class)
spam2.res <- get.resylts(subset(class.df,Type=="SPAM")$Class)
class.res <- rbind(easyham2.col,hardham2.col,spam2.col)
colnames(class.res) <- c("NOT SPAM","SPAM")
print(class.res)


#效果改进，更改先验概率
spam.classifier.new <- function(path){
  pr.spam <- classify.email(path,spam.df,prior=0.2)
  pr.ham <- classify.email(path,easyham.df,prior=0.8)
  return(c(pr.spam,pr.ham,ifelse(pr.spam>pr.ham,1,0)))
}
spam2.class <- suppressWarnings(lapply(spam2.docs,
                                       function(p) spam.classifier.new(file.path(spam2.path,p))))
easyham2.class <- suppressWarnings(lapply(easyham2.docs,
                                          function(p) spam.classifier.new(file.path(easyham2.path,p))))
hardham2.class <- suppressWarnings(lapply(hardham2.docs,
                                          function(p) spam.classifier.new(file.path(hardham2.path,p))))

print(class.res)                                

#第三章已完成，但是结果运行不出来



#第四章  排序：智能收件箱

#有监督学习与无监督学习：有监督学习已有明确的输出实例；无监督学习在开始处理数据时预先并没有已知的输出实例。

#理论上邮件的优先级特征：
#社交特征：收件人与发件人之间的交互程度
#内容特征：收件人对邮件采取行为（回复、标记等）与某些特征词之间相关
#线程特征：记录用户在当前线程下的交互行为
#标签特征：检查用户通过过滤器给邮件赋予的标签（标记）
#由于数据量不足，本文用于替代的优先级特征：（需要抽取的元素）

#社交特征：来自某一发件人的邮件量（发件人地址）
#时间量度：（接受时间日期）
#线程特征：是否活跃（主题）
#内容特征：高频词分析（邮件正文）

library(tm)
library(ggplot2)
getwd()
setwd("D:/R/Rjiaoben/anlijiexi")
data.path <- "ML_for_Hackers-master/03-Classification/data/"
easyham.path <- paste(data.path,"easy_ham/",sep="")


#抽取特征集合
#输入邮件路径，返回特征信息
#把路径作为数据的最后一列保存，可以使测试阶段的排序更容易
parse.email <- function(path){
  full.msg <- msg.full(path)
  date <- get.date(full.msg)
  from <- get.from(full.msg)
  subj <- get.subject(full.msg)
  msg <- get.msg(full.msg)
  return(c(date, from, subj, msg, path))
}

#辅助函数的编写
#读取内容
#按行读取，每一行的内容对应向量中的一个元素
msg.full <- function(path){
  con <- file(path, open = "rt", encoding = "latin1")
  msg <- readLines(con)
  close(con)
  return(msg)
}

#正则表达式抽取信息
#抽取发件地址：
#两种格式：From: JM<j@xx.com>; From: j@xx.com(JM)
#grepl()以"From: "作为匹配条件，返回是(1)否(0)匹配；匹配的一行存入from中
#方括号创建字符集：冒号、尖括号、空格，作为拆分文本的标志，存入列表中的第一个元素
#将空元素过滤掉
#查找包含"@"字符的元素并返回
get.from <- function(msg.vec){
  from <- msg.vec[grepl("From: ", msg.vec)]
  from <- strsplit(from, '[":<> ]')[[1]]
  from <- from[which(from != "" & from != " ")]
  return(from[grepl("@", from)][1])
}

#抽取正文
#在msg.vec中查找第一个空行，返回空行之后的所有元素
#用paste函数把这些向量转换成一个字符串元素的向量后返回
get.msg <- function(msg.vec){
  msg <- msg.vec[seq(which(msg.vec == "")[1]+1, length(msg.vec), 1)]
  return(paste(msg, collapse = "\n"))
}

#抽取主题
#正则匹配主题特征（有的邮件没有主题）
#如果长度大于0，返回第2 个元素(第一个元素是"Subject")否则返回空字符
#如果不设条件，在grepl()一类得不到匹配时，会返回一个指定的值
#如integer(0)或character(0)
get.subject <- function(msg.vec){
  subj <- msg.vec[grepl("Subject: ", msg.vec)]
  if(length(subj) > 0){
    return(strsplit(subj, "Subject: ")[[1]][2])
  }else{
    return("")
  }
}

#抽取日期，需要解决的问题：
#1.邮件头会有许多行与"Date:"匹配，但是真正需要的是只有字符串首部出现的
#利用这点，要求正则表达式只匹配在字符串首部的"Date:",使用脱字节"^Date:"
#有可能在正文中也匹配上，所以只需要保存第一次匹配成功的字符串即可
#3.处理文本：拆分字符：加号或减号或冒号
#4.将首部或者尾部的空白字符替换掉
#5.只返回符合格式的部分，除去25字符以后的内容
get.date <- function(msg.vec){
  date.grep <- grepl("^Date: ", msg.vec)
  date.grep <- which(date.grep == TRUE)
  date <- msg.vec[date.grep[1]]
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
  date <- gsub("^\\s+|\\s+$", "", date)
  return(strtrim(date, 25))
}

#处理邮件
#创建一个向量，包含所有"易识别的正常邮件"的文件路径
#并把其中多余的"cmds"文件路径从向量中移除
#使用lapply函数对每一个邮件文件应用parse.email函数
#因为使用的是前一章的数据路径，使用要在lapply函数内部用paste函数把文件的相对路径和easyham.path变量连接起来
#do.call函数把lapply函数返回的向量列表转换成一个矩阵
#再转换成一个包含多个字符串向量的数据框，并设置名称
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
easyham.parse <- lapply(easyham.docs,
                        function(p) parse.email(paste(easyham.path, p, sep = "")))
ehparse.matrix <- do.call(rbind, easyham.parse)
allparse.df <- data.frame(ehparse.matrix, stringsAsFactors = FALSE)
names(allparse.df) <- c("Date", "From.EMail", "Subject", "Message", "Path")

head(allparse.df)


#时间格式不统一，需要进一步处理，转换为R中的POSIX对象，以便按照时间对邮件排序
#将形如(Wed, 04 Dec 2002 11:36:32)和(04 Dec 2002 11:49:23)格式的字符串转换成POSIX格式
#定义date.converter函数，用于输入两个不同的POSIX模式串以及一个日期字符串向量
#当传给strptime的模式串并不匹配传给它的日期字符串时，默认返回NA值
#利用这个值，把第一次转换得到NA值的元素用第二次转换的结果代替，从而将两个转换结果结合起来
#strptime函数将字符串转换成POSIX格式
date.converter <- function(dates, pattern1, pattern2){
  pattern1.convert <- strptime(dates, pattern1)
  pattern2.convert <- strptime(dates, pattern2)
  pattern1.convert[is.na(pattern1.convert)] <- pattern2.convert[is.na(pattern2.convert)]
  return(pattern1.convert)
}

#指明需要转换的格式
pattern1 <- "%a, %d %b %Y %H:%M:%S"
pattern2 <- "%d %b %Y %H:%M:%S"

#系统区域设置，”LC_TIME“的设置会对AS.POSIXlt()和strptime()的表现造成影响
#如果不进行设置，会使后面的处理后结果Date列全部缺失
Sys.setlocale("LC_TIME", "C")
allparse.df$Date <- date.converter(allparse.df$Date, pattern1, pattern2)

#清洗工作的最后一步是把主题(Subject字段)和发件人(From字段)这两列的字符串向量全部转换为小写
#保证在训练阶段所有数据条目的格式尽量
allparse.df$Subject <- tolower(allparse.df$Subject)
allparse.df$From.EMail <- tolower(allparse.df$From.EMail)

#结合使用with和order命令按照时间对数据进行排序
#产生训练集：前一半作为训练集
priority.df <- allparse.df[with(allparse.df, order(Date)), ]
priority.train <- priority.df[1:(round(nrow(priority.df) / 2)), ]


#设置权重策略

library(reshape2)
from.weight <- melt(with(priority.train, table(From.EMail)), value.name="Freq")

#结果可视化
#排序、过滤频数小于6的观测，绘图图形
from.weight <- from.weight[with(from.weight, order(Freq)), ]
from.ex <- subset(from.weight, Freq > 6)
from.scales <- ggplot(from.ex) +
  geom_rect(aes(xmin = 1:nrow(from.ex) - 0.5,
                xmax = 1:nrow(from.ex) + 0.5,
                ymin = 0,
                ymax = Freq,
                fill = "lightgrey",
                color = "darkblue")) +
  scale_x_continuous(breaks = 1:nrow(from.ex), labels = from.ex$From.EMail) +
  coord_flip() +
  scale_fill_manual(values = c("lightgrey" = "lightgrey"), guide = "none") +
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  ylab("Number of Emails Received (truncated at 6)") +
  xlab("Sender Address") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 5, hjust = 1))
print(from.scales)

#根据条形图，只有几个人的数量很大，属于特殊情况，会导致权重偏移
#解决方案：尺度变换：不会因为极个别情况影响整体的阈值计算
#对数变换：观察绝对值权重、自然对数变换权重及常用对数变换权重
#任何数的0次幂都是1，当观测值为1时，对数变换就会变成0 ，为了避免出现这种情况，我们在对数变换前总是对任何观测值都加1
from.weight <- transform(from.weight,
                         Weight = log(Freq + 1),
                         log10Weight = log10(Freq + 1))
from.rescaled <- ggplot(from.weight, aes(x = 1:nrow(from.weight))) +
  geom_line(aes(y = Weight, linetype = "ln")) +
  geom_line(aes(y = log10Weight, linetype = "log10")) +
  geom_line(aes(y = Freq, linetype = "Absolute")) +
  scale_linetype_manual(values = c("ln" = 1,
                                   "log10" = 2,
                                   "Absolute" = 3),
                        name = "Scaling") +
  xlab("") +
  ylab("Number of emails Receieved") +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())
print(from.rescaled)

#绝对值权重过于陡峭，常用对数变换差异太弱，选择自然对数变换更加合理
#警告：不允许特征集合中出现值为0的观测记录，否则log()会返回-lnf(负无穷)，破坏整个结果


#线程活跃度的权重计算
#按照线程对邮件进行分组，然后衡量每个线程开始之后的活跃程度
#在这份数据中，邮件并没有明确的线程ID，但是在训练数据中识别线程的一种逻辑方式是查找具有共同主题的邮件
#把训练数据中的主题都用"re:"进行拆分，通过拆分后第一个元素是空字符的字符串向量来找到各个线程
#针对"re:"回复操作，可以找到相关线程
#返回所有带有"re:"的邮件的发件人和初始线程的主题
find.threads <- function(email.df){
  response.threads <- strsplit(email.df$Subject, "re: ")
  is.thread <- sapply(response.threads, function(subj) ifelse(subj[1] == "", TRUE, FALSE))
  threads <- response.threads[is.thread]
  senders <- email.df$From.EMail[is.thread]
  threads <- sapply(threads, function(t) paste(t[2:length(t)], collapse = "re: "))
  return(cbind(senders, threads))
}
threads.matrix <- find.threads(priority.train)


#根据线程中活跃的发件人赋予权重，增加在发件人上，但是只关注出现在threads.matrix中的发件人
#仍采用自然对数变换的权重
#email.thread函数的但忽然是threads.matrix,然后生成第二个基于数量的权重
#table函数统计发件人在线程中出现的频率
email.thread <- function(threads.matrix){
  senders <- threads.matrix[, 1]
  senders.freq <- table(senders)
  senders.matrix <- cbind(names(senders.freq), senders.freq, log(senders.freq + 1))
  senders.df <- data.frame(senders.matrix, stringsAsFactors = FALSE)
  row.names(senders.df) <- 1:nrow(senders.df)
  names(senders.df) <- c("From.EMail", "Freq", "Weight")
  senders.df$Freq <- as.numeric(senders.df$Freq)
  senders.df$Weight <- as.numeric(senders.df$Weight)
  return(senders.df)
}
senders.df <- email.thread(threads.matrix)


#基于已知的活跃线程，追加权重：假设这个线程已知，用户会觉得这些更活跃的线程更重要
#unique()得到所有线程名称，thread.counts里存放了所有线程对应的活跃度及权重
#最后合并，返回线程名、频数、间隔时间和权重
get.threads <- function(threads.matrix, email.df){
  threads <- unique(threads.matrix[, 2])
  thread.counts <- lapply(threads,
                          function(t) thread.counts(t, email.df))
  thread.matrix <- do.call(rbind, thread.counts)
  return(cbind(threads, thread.matrix))
}


#输入线程主题和训练数据，通过所有邮件日期和时间戳，计算训练数据中这个线程接收了多少邮件
#thread.times找到了线程的时间戳，其向量长度就是该线程接收邮件的频数
#time.span是线程在训练数据中存在的时间：为了计算活跃度
#log.trans.weight是常用对数的权重
#一个线程中只有一条邮件记录的情况：训练数据开始收集数据时线程结束或训练数据结束手机时线程开始
#要剔除这种情况，返回缺失值
#实际情况中，频数小而间隔大，意味着trans.weight是远小于1的值，对她进行对数变换时，结果为负
#为了将权重计算不引入负值，进行仿射变换，即加10
thread.counts <- function(thread, email.df){
  thread.times <- email.df$Date[which(email.df$Subject == thread |
                                        email.df$Subject == paste("re:", thread))]
  freq <- length(thread.times)
  min.time <- min(thread.times)
  max.time <- max(thread.times)
  time.span <- as.numeric(difftime(max.time, min.time, units = "secs"))
  if(freq < 2){
    return(c(NA, NA, NA))
  }else{
    trans.weight <- freq / time.span
    log.trans.weight <- 10 + log(trans.weight, base = 10)
    return(c(freq, time.span, log.trans.weight))
  }
}

#生成权重数据，并做一定处理，目的是与其它权重数据框中的名称保持一致，最后用subset剔除缺失行
thread.weights <- get.threads(threads.matrix,priority.train)
thread.weights <- data.frame(thread.weights,stringsAsFactors=FALSE)
names(thread.weights) <- c("Thread","Freq","Response","Weight")
thread.weights$Freq <- as.numeric(thread.weights$Freq)
thread.weights$Response <- as.numeric(thread.weights$Response)
thread.weights$Weight <- as.numeric(thread.weights$Weight)
thread.weights <- subset(thread.weights,is.na(thread.weights$Freq)==FALSE)

head(thread.weights)

#从结果中可以看到即使有相同频数，因为响应时间不同，赋予的权重也不同
#虽然对于一些人来说可能不会用到这种方式排序，但是作为一直通用解决方案，需要这种量化的手段


#线程中的高频词权重：假设出现在活跃线程邮件主题中的高频词比低频词和出现在不活跃线程中的词项重要
#term.counts():输入词项向量和TDM选项列表，返回词项的TDM并抽取所有线程中的词项频次
term.counts <- function(term.vec,control){
  vec.corpus <- Corpus(VectorSource(term.vec))
  vec.tdm <- TermDocumentMatrix(vec.corpus,control=control)
  return (rowSums(as.matrix(vec.tdm)))
}

#计算词频，并只留下词项
#对词项进行赋予权重，该权重=该词项所在所有线程权重的平均值
#将向量转数据框，词项提取为名称，行名改为行号
thread.terms <- term.counts(thread.weights$Thread,
                            control=list(stopwords=stopwords()))
thread.terms <- names(thread.terms)
term.weights <- sapply(thread.terms,
                       function(t) mean(thread.weights$Weight[grepl(t,
                                                                    thread.weights$Thread,fixed=TRUE)]))
term.weights <- data.frame(list(Term=names(term.weights),
                                Weight=term.weights),stringsAsFactors=FALSE,row.names=1:length(term.weights))


#最后一份权重数据的产生要基于训练数据的所有邮件中的词项频数
#取词频的对数变换值作为词的权重值
#在数据框msg.weights中存在一个隐含的假设：和已读过邮件相似的新邮件比那些完全陌生的邮件更重要
msg.terms <- term.counts(priority.train$Message,
                         control=list(stopwords=stopwords(),
                                      removePunctuation=TRUE,removeNumber=TRUE))
msg.weights <- data.frame(list(Term=names(msg.terms),
                               Weight=log(msg.terms,base=10)),
                          stringAsFactors=FALSE,
                          row.names=1:length(msg.terms))
msg.weights <- subset(msg.weights,Weight>0)


#训练和测试排序算法

#思路是给每封邮件都产生一个有限等级，这就是要将前面提到的各个权重相乘
#因此需要对每封邮件都进行解析、抽取特征、匹配权重数据框并查找权重值
#用这些权重值的乘积作为排序的依据
#首先执行权重查找，即主题和正文词项
#输入三个参数：待查找词项(字符串)、查找对象(权重数据框)、查找类型(T为词项，F为线程)
#返回权重值
#查找失败的情况
#1.查找输入get.weights()的待查找词项长度是否大于0，如果输入无效则返回1不影响乘积运算
#2.match()对于没有匹配上的元素返回了NA，要将NA替换为1，通过判断match.weights为0即可
get.weights <- function(search.term,weight.df,term=TRUE){
  if (length(search.term)>0){
    if (term){
      term.match <- match(names(search.term),weight.df$Term)
    }else{
      term.match <- match(search.term,weight.df$Thread)
    }
    match.weights <- weight.df$Weight[which(!is.na(term.match))]
    if (length(match.weights)<1){
      return(1)
    } else {return(mean(match.weights))
    }
  } else {
    return(1)
  }}


#输入邮件路径，返回排序权重(rank)
rank.message <- function(path){
  #抽取四个特征:
  #msg[]1日期2发言人3主题4正文5路径
  msg <- parse.email(path)
  #查找发件人地址权重，未匹配的返回1
  from <- ifelse(length(which(from.weight$From.EMail==msg[2]))>0,
                 from.weight$Weight[which(from.weight$From.EMail==msg[2])],1)
  #查找发件人活跃度权重，未匹配返回1 
  thread.from <- ifelse(length(which(senders.df$From.EMmail==msg[2]))>0,
                        senders.df$Weight[which(senders.df$From.EMail==msg[2])],1)
  #解析主题是否在线程内
  subj <- strsplit(tolower(msg[3]),"re:")
  is.thread <- ifelse(subj[[1]][1]=="",TRUE,FALSE)
  #线程活跃度查找并匹配
  if(is.thread){
    activity <- get.weights(subj[[1]][2],thread.weights,term=FALSE)
  } else {
    activity <-1
  }
  #活跃线程词项权重匹配
  thread.terms <- term.counts(msg[3],control=list(stopwords=TRUE))
  thread.terms.weights <- get.weights(thread.terms,term.weights)
  #正文词项权重匹配
  msg.terms <- term.counts(msg[4],control=list(stopwords=TRUE,
                                               removePunctuation=TRUE,removeNumbers=TRUE))
  msg.weights <- get.weights(msg.terms,msg.weights)
  #排序依据是所有查找到的权重的乘积
  rank <- prod(from,thread.from,activity,thread.terms.weights,msg.weights)
  #返回日期、发件人、主题、排序重积
  return(c(msg[1],msg[2],msg[3],rank))
}

#启动排序算法
#安时间分为训练数据和测试数据，注意round()在处理0.5时，返回的值是最靠近的偶数
train.paths <- priority.df$Path[1:(round(nrow(priority.df)/2))]
test.paths <- priority.df$Path[((round(nrow(priority.df)/2))+1):nrow(priority.df)]

#对训练数据进行处理，返回排序值
#警告可以忽略，应用suppressWarning()函数即可
train.ranks <-lapply(train.paths,rank.message)
train.ranks.matrix <- do.call(rbind,train.ranks)
train.ranks.matrix <- cbind(train.paths,train.ranks.matrix,"TRAINING")
train.ranks.df <- data.frame(train.ranks.matrix,stringsAsFactors=FALSE)
names(train.ranks.df) <- c("Message","Date","From","Subj","Rank","Type")
train.ranks.df$Rank <- as.numeric(train.ranks.df$Rank)

#计算有限邮件的阈值(取中位数)新建列设置是否推荐
priority.threshold <- median(train.ranks.df$Rank)
train.ranks.df$Priority <- ifelse(train.ranks.df$Rank>=priority.threshold,1,0)

#设置阈值的结果可视化
threshold.plot <- ggplot(train.ranks.df,aes(x=Rank))+
  stat_density(aes(fill="darkred"))+
  geom_vline(xintercept=priority.threshold,linetype=2)+
  scale_fill_manual(values=c("darkred"="darkred"),guide="none")+
  theme_bw()
print(threshold.plot)

#从图中可以看到阈值约为24，拍寻结果是明显的重尾分布，说明排序算法在训练集上表现不错
#书中提到与标准差作为阈值，此时阈值为90，中位数的做法有较大包容性，而标准差的方式将大部分邮件排除在外
#测试集测试效果

test.ranks <- suppressWarnings(lapply(test.paths,rank.message))
test.ranks.matrix <- do.call(rbind,test.ranks)
test.ranks.matrix <- cbind(test.paths,test.ranks.matrix,"TESTING")
test.ranks.df <- data.frame(test.ranks.matrix,stringsAsFactors=FALSE)
names(test.ranks.df) <- c("Message","Date","From","Subj","Rank","Type")
test.ranks.df$Rank <- as.numeric(test.ranks.df$Rank)
test.ranks.da$Priority <- ifelse(test.raks.df$Rank>=priority.threshold,1,0)







#第五章  回归模型：预测网页访问量

#画出密度图来比较吸烟者和非吸烟者
getwd()
setwd("D:/R/Rjiaoben/anlijiexi")
library(ggplot2)
ages <- read.csv("ML_for_Hackers-master/05-Regression/data/longevity.csv")
ggplot(ages,aes(x=AgeAtDeath,fill=factor(Smokes)))+
  geom_density()+facet_grid(Smokes~.)













































































































































































































































