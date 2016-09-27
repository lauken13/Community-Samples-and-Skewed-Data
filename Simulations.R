library(WRS)
library(PairedData)
library(dplyr)
library(ggplot2)
library(MASS)
library(Rfit)
library(Rmisc)

create.data<-function(sample.size,effect.size,distribution,outcome='same',method,number='two'){
  if (number=='two') {
    if(method=='mean'){
      if (effect.size=='None'){
        effect<-0
      }else if(effect.size=='Small'){
        effect<-0.3
      } else if (effect.size=='Medium'){
        effect<-0.5
      } else if (effect.size=='Large'){
        effect<-.9
      }  
      if(distribution=='Low Skew'){
        x1<-ghdist(sample.size,.2,0)
        x2<-ghdist(sample.size,.2,0)+effect
      }else if(distribution=='Med Skew'){
        x1<-ghdist(sample.size,.5,0)
        x2<-ghdist(sample.size,.5,0)+effect
      }else if (distribution=='High Skew'){
        x1<-ghdist(sample.size,1.5,0)
        x2<-ghdist(sample.size,1.5,0)+effect
      }else{
        x1<-rnorm(sample.size,0,1)
        x2<-rnorm(sample.size,effect,1)
      }
      dat<-data.frame(value=c(x1,x2),group=c(rep('one',sample.size),rep('two',sample.size)))
      return(dat)
    }
    
    if(method=='correlation'){
      if (effect.size=='None'){
        effect<-0
      } else if (effect.size=='Small'){
        effect<-0.2
      } else if (effect.size=='Medium'){
        effect<-0.5
      }else if (effect.size=='Large'){
        effect<-.8
      }
      if(effect==0){
        x<-cbind(rnorm(sample.size,0,1),rnorm(sample.size,0,1))
      }else{
        x<-mvrnorm(sample.size,c(0,0),cbind(c(1,effect),c(effect,1)))
      }
      
      if(outcome=='same'){
        if(distribution=='Low Skew'){
          g<-.2
          x1<-(exp(g*x[,1])-1)/g
          x2<-(exp(g*x[,2])-1)/g
        }else if (distribution=='Med Skew'){
          g<-0.5
          x1<-(exp(g*x[,1])-1)/g
          x2<-(exp(g*x[,2])-1)/g
        }else if (distribution=='High Skew'){
          g<-1.5
          x1<-(exp(g*x[,1])-1)/g
          x2<-(exp(g*x[,2])-1)/g
        }else{
          g<-0
          x1<-x[,1]
          x2<-x[,2]
        }
      }else if(outcome=='normal'){
        if(distribution=='low skew'){
          g<-.5
          x1<-(exp(g*x[,1])-1)/g
          x2<-x[,2]
        }else if (distribution=='high skew'){
          g<-1.5
          x1<-(exp(g*x[,1])-1)/g
          x2<-x[,2]
        }else{
          g<-0
          x1<-x[,1]
          x2<-x[,2]
        }
      }
      dat<-data.frame(v1=x1,v2=x2)
      return(dat)
    }
  }else{
    if(method=='correlation'){
      if (effect.size=='None'){
        effect<-0
      } else if (effect.size=='Small'){
        effect<-0.2
      } else if (effect.size=='Medium'){
        effect<-0.5
      }else if (effect.size=='Large'){
        effect<-.8
      }
      if(effect==0){
        x<-cbind(rnorm(sample.size,0,1),rnorm(sample.size,0,1))
      }else{
        x<-mvrnorm(sample.size,c(0,0),cbind(c(1,effect),c(effect,1)))
      }
      
      if(outcome=='same'){
        if(distribution=='Low Skew'){
          g<-.2
          x1<-(exp(g*x[,1])-1)/g
          x2<-(exp(g*x[,2])-1)/g
        } else if(distribution=='Med Skew'){
          g<-.5
          x1<-(exp(g*x[,1])-1)/g
          x2<-(exp(g*x[,2])-1)/g
        }else if (distribution=='High Skew'){
          g<-1.5
          x1<-(exp(g*x[,1])-1)/g
          x2<-(exp(g*x[,2])-1)/g
        }else{
          g<-0
          x1<-x[,1]
          x2<-x[,2]
        }
      }else if(outcome=='normal'){
        if(distribution=='low skew'){
          g<-.5
          x1<-(exp(g*x[,1])-1)/g
          x2<-x[,2]
        }else if (distribution=='high skew'){
          g<-1.5
          x1<-(exp(g*x[,1])-1)/g
          x2<-x[,2]
        }else{
          g<-0
          x1<-x[,1]
          x2<-x[,2]
        }
      }
      x3<-rnorm(sample.size,0,1)
      x4<-rnorm(sample.size,0,1)
      x5<-rnorm(sample.size,0,1)
      dat<-data.frame(v1=x1,v2=x2,v3=x3,v4=x4,v5=x5)
      return(dat)
    }
  }
  
}

datafull<-expand.grid(analysis=c('t-test','Yuens','Kruskal','Mann'),
                      #analysis=c('correlation-pearson','correlation-spearman','t-test','yuens t-test','lm-binomial','lm-bootstrap'),
                      sample.size=c(10,25,50,100,200,300,400,500),
                      effect.size=c('None','Small','Medium','Large'),
                      distribution=c('Normal','Low Skew','Med Skew','High Skew'),
                      iteration=1:1000)
datafull$significance<-rep(0,nrow(datafull))
datafull$correct<-rep('AAA',nrow(datafull))
for (r in 1:nrow(datafull)){
  dat.temp<-create.data(sample.size=datafull$sample.size[r],effect.size=datafull$effect.size[r],distribution=datafull$distribution[r],method='mean',outcome='same')
  if(datafull$analysis[r]=='t-test'){
    datafull$significance[r]<-t.test(value~group,data=dat.temp, var.equal=TRUE)$p.value
  }else if(datafull$analysis[r]=='Yuens'){
    datafull$significance[r]<-yuen.t.test(value~group,data=dat.temp)$p.value
  }else if(datafull$analysis[r]=='Kruskal'){
    datafull$significance[r]<-kruskal.test(value~group,data=dat.temp)$p.value
  }else if(datafull$analysis[r]=='Mann'){
    datafull$significance[r]<-wilcox.test(value~group,data=dat.temp)$p.value
  }
  
  
  if(datafull$effect.size[r]=='None'){
    if(datafull$significance[r]<.05){
      datafull$correct[r]<-'NO'
    }else if(datafull$significance[r]>.05){
      datafull$correct[r]<-'YES'
    }
  }else{
    if(datafull$significance[r]<.05){
      datafull$correct[r]<-'YES'
    }else if(datafull$significance[r]>.05){
      datafull$correct[r]<-'NO'
    }
  }
  print(r/nrow(datafull))
}

df.comparingmeans<-datafull
save(df.comparingmeans, file="T_TestAlternatives.Rdata")

df<-datafull %>%
  group_by(distribution,sample.size,effect.size,analysis) %>%
  summarize(prob=sum(correct=='YES')/n()) %>%
  ungroup()

df$sample.size<-as.factor(df$sample.size)
plot1<-ggplot(data=df, aes(x=sample.size, prob, group=effect.size, colour=effect.size)) +facet_grid(analysis ~ distribution)+
  geom_point()+geom_line()+theme_bw()+ggtitle("t-test success rate")
plot1  


datafull<-expand.grid(analysis=c('Pearson','Spearman','HC4'),
                      #analysis=c('correlation-pearson','correlation-spearman','t-test','yuens t-test','lm-binomial','lm-bootstrap'),
                      sample.size=c(10,25,50,100,200,300,400,500),
                      effect.size=c('None','Small','Medium','Large'),
                      distribution=c('Normal','Low Skew','Med Skew','High Skew'),
                      iteration=1:1000,skew1=-1,skew2=-1)
datafull$significance<-rep(0,nrow(datafull))
datafull$correct<-rep('AAA',nrow(datafull))
for (r in 1:nrow(datafull)){
  dat.temp<-create.data(sample.size=datafull$sample.size[r],effect.size=as.character(datafull$effect.size[r]),distribution=as.character(datafull$distribution[r]),outcome='same',method='correlation')
  if(datafull$analysis[r]=='Pearson'){
    datafull$significance[r]<-summary(lm(dat.temp$v1~dat.temp$v2))$coefficients[[8]]
  }else if(datafull$analysis[r]=='Spearman'){
    datafull$significance[r]<-cor.test(x=dat.temp$v1,y=dat.temp$v2, method='spearman')$p.value
  }else if(datafull$analysis[r]=='HC4'){
    datafull$significance[r]<-olshc4(x=dat.temp$v1,y=dat.temp$v2, xout=T)$ci[10]
  }
  datafull$skew1[r]<-skew(dat.temp$v1)$skew
  datafull$skew2[r]<-skew(dat.temp$v2)$skew
  if(datafull$effect.size[r]=='None'){
    if(datafull$significance[r]<.05){
      datafull$correct[r]<-'NO'
    }else if(datafull$significance[r]>.05){
      datafull$correct[r]<-'YES'
    }
  }else{
    if(datafull$significance[r]<.05){
      datafull$correct[r]<-'YES'
    }else if(datafull$significance[r]>.05){
      datafull$correct[r]<-'NO'
    }
  }
  print(r/nrow(datafull))
}

df.single.predictor<-datafull
save(df.single.predictor,file="linear_single.predictor.Rdata")

##########################Multiple Regression################################
datafull<-expand.grid(analysis=c('Linear Model','HC4',Ranked),
                      #analysis=c('correlation-pearson','correlation-spearman','t-test','yuens t-test','lm-binomial','lm-bootstrap'),
                      sample.size=c(10,25,50,100,200,300,400,500),
                      effect.size=c('None','Small','Medium','Large'),
                      distribution=c('Normal','Low Skew','Med Skew','High Skew'),
                      iteration=1:1000,skew1=-1,skew2=-1)
datafull$significance<-rep(0,nrow(datafull))
datafull$correct<-rep('AAA',nrow(datafull))
for (r in 1:nrow(datafull)){
  dat.temp<-create.data(sample.size=datafull$sample.size[r],effect.size=as.character(datafull$effect.size[r]),distribution=as.character(datafull$distribution[r]),outcome='same',method='correlation',number='multiple')
  if(datafull$analysis[r]=='Linear Model'){
    datafull$significance[r]<-summary(lm(v1~v2 +v3 +v4+v5,data=dat.temp))$coefficients[[17]]
  }else if(datafull$analysis[r]=='HC4'){
    datafull$significance[r]<-olshc4(y=dat.temp$v1,x=dat.temp[,2:5], xout=T)$ci[22]
  }
}else if(datafull$analysis[r]=='Ranked'){
  datafull$significance[r]<-summary(rfit(v1~v2 +v3 +v4+v5,data=dat.temp))$coefficients[[17]]
  
}
datafull$skew1[r]<-skew(dat.temp$v1)$skew
datafull$skew2[r]<-skew(dat.temp$v2)$skew
if(datafull$effect.size[r]=='None'){
  if(datafull$significance[r]<.05){
    datafull$correct[r]<-'NO'
  }else if(datafull$significance[r]>.05){
    datafull$correct[r]<-'YES'
  }
}else{
  if(datafull$significance[r]<.05){
    datafull$correct[r]<-'YES'
  }else if(datafull$significance[r]>.05){
    datafull$correct[r]<-'NO'
  }
}
print(r/nrow(datafull))
}

df.multiple.predictor<-datafull
save(df.multiple.predictor,file="linear_multiple.predictor.Rdata")

###########Create Sample Distributions
numberSims=250
normal_data<-create.data(sample.size=numberSims,effect.size='None',distribution='Normal',outcome='same',method='correlation')$v1
low_skew<-create.data(sample.size=numberSims,effect.size='None',distribution='Low Skew',outcome='same',method='correlation')$v1
med_skew<-create.data(sample.size=numberSims,effect.size='None',distribution='Med Skew',outcome='same',method='correlation')$v1
high_skew<-create.data(sample.size=numberSims,effect.size='None',distribution='High Skew',outcome='same',method='correlation')$v1


distrib<-data.frame(
  values= c(normal_data,low_skew,med_skew,high_skew),
  Type=c(rep('Normal',numberSims),rep('Low Skew',numberSims),rep('Med Skew',numberSims),rep('High Skew',numberSims))
)
save(distrib,file="distributions_ex.Rdata")

