###################BDI PROJECT#########################################
############SCRIPT TO PRODUCE FINAL FIGURES############################
###################Lauren Kennedy 22/04/2016###########################

##########Dependencies /Data Load #####################################
load("~/PhD/2016/BDI Write up/Final simulations/distributions_ex.Rdata")
load("~/PhD/2016/BDI Write up/Final simulations/linear_single.predictor.Rdata")
load("~/PhD/2016/BDI Write up/Final simulations/T_TestAlternatives.Rdata")
load("~/PhD/2016/BDI Write up/Final simulations/linear_multiple.predictor.Rdata")

########################Packages Required#############################
library(ggplot2)
library(dplyr)
library(WRS)
###################Figure 1 ##########################################


#Generate the simualated BDI Data
x<-ghdist(10000,1,0)*5   Loaded in for consistency
x<-x+min(x)
x<-x[x<64]
y<-rnorm(10000,30,10)
y<-y[y>=0 & y<=63]
y<-y[1:length(x)]
comb<-data.frame(Skewed=x,Normal=y)
save(comb,file="SimulatedBDI.Rdata" )
Check it fulfills the requirements
length(SimulatedBDIData$x[SimulatedBDIData$x>13])/length(SimulatedBDIData$x)

rects<-data.frame(xstart=c(-0.5,13.5,19.5,27.5),xend=c(13.5,19.5,27.5,63.5), col= c("Normal","Mild","Moderate","Severe"))
labs.1<-data.frame(
  x = c(6.5,16.5,23.5,45.5),
  y = rep(.03,4),
  text = c("Normal","Mild","Moderate","Severe"))
labs.2<-data.frame(
  x = c(6.5,16.5,23.5,45.5),
  y = rep(.11,4),
  text = c("Normal","Mild","Moderate","Severe"))
group.colors<-c("Normal"="#f7f7f7","Mild"="#cccccc",
                "Moderate"="#969696","Severe"="#525252")
plot_sim_BDI<-ggplot() + 
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.6,colour="black") + scale_fill_manual(values=group.colors)+
  geom_density(data=comb, aes(x=Normal),alpha=.9,
                 colour="black", fill="white",size=1) +
  geom_label(data=labs.1,aes(x, y,label = text),size=15)+
  
  theme_classic() +
  scale_x_continuous(breaks=seq(0, 63,5),limits=c(-.5,63.5), name="",expand = c(0, 0))+
  scale_y_continuous(breaks=NULL,name="",expand = c(0, 0)) +
  ggtitle("Expected BDI administered to a clinical population") + 
  theme(axis.title.x = element_text(face="bold", colour="Black", size=40),
        axis.text.x  = element_text(size=35),
        axis.title.y = element_text(face="bold", colour="Black", size=40),
        axis.text.y = element_text(size=35),
        plot.title = element_text(size=45, face="bold"),
        legend.position="none")
plot_sim_BDI2<-ggplot() + 
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.6,colour="black") + scale_fill_manual(values=group.colors)+
  geom_density(data=comb, aes(x=Skewed),alpha=.9,
               colour="black", fill="white",size=1) +
  geom_label(data=labs.2,aes(x, y,label = text),size=15)+
  theme_classic() +
  scale_x_continuous(breaks=seq(0, 63,5),limits=c(-.5,63.5), name="BDI Score",expand = c(0, 0))+
  scale_y_continuous(breaks=NULL,name="",expand = c(0, 0)) +
  ggtitle("Expected BDI administered to a non clinical population") +
  theme(axis.title.x = element_text(face="bold", colour="Black", size=40),
        axis.text.x  = element_text(size=35),
        axis.title.y = element_text(face="bold", colour="Black", size=40),
        axis.text.y = element_text(size=35),
        plot.title = element_text(size=45, face="bold"),
        legend.position="none")

sim_BDI<-grid.arrange(plot_sim_BDI,plot_sim_BDI2, ncol=1)

sim_BDI
ggsave("Expected_Distribution.pdf", width = 40, height = 40, units = "cm")

#Hand saved as png size 1600x1000


####################FIGURE 2###########################################
#This required data that we do not have the permission to publish
######################FIGURE 3##########################################


###################Figure 4#############################################
  
    
#####################Figure 6#############################################
  #Simulations of t-test with levels of skew

df.t<-df.comparingmeans%>%
  filter(analysis=="t-test") %>%
  group_by(distribution,sample.size,effect.size,analysis) %>%
  summarize(prob=sum(correct=='YES')/n()) %>%
  mutate(Prob_Reject_Null = ifelse(effect.size=="None", 1-prob,prob)) %>%
  ungroup()


p <- ggplot()

p <- p + geom_line(data=df.t, 
                   mapping=aes(x=sample.size, 
                               y=Prob_Reject_Null, 
                               group=effect.size))+scale_colour_manual(values=c('None'='#f7f7f7','Small'='#cccccc','Medium'='#969696','Large'='#525252'),name="Effect")
p <- p + scale_shape_manual(values=c(23,24,21,22),name="Effect")
p <- p + geom_point(data=df.t, 
                    mapping=aes(x=sample.size, 
                                y=Prob_Reject_Null,     
                                group=effect.size,
                                fill=effect.size,
                                shape=effect.size),
                    size=8)+scale_fill_manual(values=c('None'='#f7f7f7','Small'='#cccccc','Medium'='#969696','Large'='#525252'),name="Effect")


p <- p + facet_grid(. ~ distribution)

p <- p + theme_bw()
p <- p + xlab("Sample Size") + ylab("Prob. of Rejecting the Null")+
  theme(axis.title.y = element_text(size=30,angle = 90,hjust=.5),
        axis.text.y  = element_text(size=25),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=30),
        axis.text.x  = element_text(size=25),
        legend.title = element_text(size=30,face="bold"),
        legend.text = element_text(size = 30),
        strip.text.x = element_text(size = 30),
        legend.key.size = unit(3, "lines"),
        panel.margin.x=unit(1.5, "lines"))



plot(p)
ggsave("t_test_skew.pdf", width = 50, height = 20, units = "cm")

#####################Figure 7#############################################
#Simulations of correlation with levels of skew

df.cor<-df.single.predictor%>%
  filter(analysis=="Pearson") %>%
  group_by(distribution,sample.size,effect.size,analysis) %>%
  summarize(prob=sum(correct=='YES')/n()) %>%
  ungroup()%>%
  mutate(Prob_Reject_Null = ifelse(effect.size=="None", 1-prob,prob))

p <- ggplot()

# p<-p+geom_errorbar(data=smalldata, 
#                  mapping=aes(x=sample.size, 
#                              group=model,
#                              ymin=Correct.mean-ci, 
#                              ymax=Correct.mean+ci), 
#                  width=.1) 

p <- p + geom_line(data=df.cor, 
                   mapping=aes(x=sample.size, 
                               y=Prob_Reject_Null, 
                               group=effect.size))+scale_colour_manual(values=c('None'='#f7f7f7','Small'='#cccccc','Medium'='#969696','Large'='#525252'),name="Effect")
p <- p + scale_shape_manual(values=c(23,24,21,22),name="Effect")
p <- p + geom_point(data=df.cor, 
                    mapping=aes(x=sample.size, 
                                y=Prob_Reject_Null,     
                                group=effect.size,
                                fill=effect.size,
                                shape=effect.size),
                    size=8)+scale_fill_manual(values=c('None'='#f7f7f7','Small'='#cccccc','Medium'='#969696','Large'='#525252'),name="Effect")

p <- p + facet_grid(. ~ distribution)
p <- p + theme_bw()
p <- p + xlab("Sample Size") + ylab("Prob. of Rejecting the Null")+
  theme(axis.title.y = element_text(size=30,angle = 90,hjust=.5),
        axis.text.y  = element_text(size=25),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=30),
        axis.text.x  = element_text(size=25),
        legend.title = element_text(size=30,face="bold"),
        legend.text = element_text(size = 30),
        strip.text.x = element_text(size = 30),
        legend.key.size = unit(3, "lines"),
        panel.margin.x=unit(1.5, "lines"))
plot(p)
ggsave("lm_skew.pdf", width = 50, height = 20, units = "cm")



#####################Figure 9#############################################

df.t.alt<-df.comparingmeans%>%
  filter(analysis %in% c("t-test","Yuens","Mann")) %>%
  group_by(distribution,sample.size,effect.size,analysis) %>%
  summarize(prob=sum(correct=='YES')/n()) %>%
  ungroup()%>%
  mutate(Prob_Reject_Null = ifelse(effect.size=="None", 1-prob,prob))

p <- ggplot()

p <- p + geom_line(data=df.t.alt, 
                   mapping=aes(x=sample.size, 
                               y=Prob_Reject_Null, 
                               group=effect.size))+scale_colour_manual(values=c('None'='#f7f7f7','Small'='#cccccc','Medium'='#969696','Large'='#525252'),name="Effect")
p <- p + scale_shape_manual(values=c(23,24,21,22),name="Effect")
p <- p + geom_point(data=df.t.alt, 
                    mapping=aes(x=sample.size, 
                                y=Prob_Reject_Null,     
                                group=effect.size,
                                fill=effect.size,
                                shape=effect.size),
                    size=12)+scale_fill_manual(values=c('None'='#f7f7f7','Small'='#cccccc','Medium'='#969696','Large'='#525252'),name="Effect")

p <- p + facet_grid(analysis ~ distribution)
p <- p + theme_bw()
p <- p + xlab("Sample Size") + ylab("Prob. of Rejecting the Null")+
  theme(axis.title.y = element_text(size=40,angle = 90,hjust=.5),
        axis.text.y  = element_text(size=35),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=40),
        axis.text.x  = element_text(size=35),
        legend.title = element_text(size=40,face="bold"),
        legend.text = element_text(size = 40),
        strip.text.x = element_text(size = 40),
        strip.text.y = element_text(size = 40),
        legend.key.size = unit(3, "lines"),
        panel.margin.x=unit(1.5, "lines"),
        panel.margin.y=unit(1.5, "lines"))
plot(p)
ggsave("t_test_alt.pdf", width = 75, height = 50, units = "cm")

#####################Figure 10#############################################
df.sp<-df.single.predictor%>%
  group_by(distribution,sample.size,effect.size,analysis) %>%
  summarize(prob=sum(correct=='YES')/n()) %>%
  ungroup()%>%
  mutate(Prob_Reject_Null = ifelse(effect.size=="None", 1-prob,prob))


df.sp$analysis = factor(df.sp$analysis,levels(df.sp$analysis)[c(1,3,2)])

p <- ggplot()

p <- p + geom_line(data=df.sp, 
                   mapping=aes(x=sample.size, 
                               y=Prob_Reject_Null, 
                               group=effect.size))+scale_colour_manual(values=c('None'='#f7f7f7','Small'='#cccccc','Medium'='#969696','Large'='#525252'),name="Effect")
p <- p + scale_shape_manual(values=c(23,24,21,22),name="Effect")
p <- p + geom_point(data=df.sp, 
                    mapping=aes(x=sample.size, 
                                y=Prob_Reject_Null,     
                                group=effect.size,
                                fill=effect.size,
                                shape=effect.size),
                    size=12)+scale_fill_manual(values=c('None'='#f7f7f7','Small'='#cccccc','Medium'='#969696','Large'='#525252'),name="Effect")


p <- p + facet_grid(analysis~ distribution)
p <- p + theme_bw()
p <- p + xlab("Sample Size") + ylab("Prob. of Rejecting the Null")+
  theme(axis.title.y = element_text(size=40,angle = 90,hjust=.5),
        axis.text.y  = element_text(size=35),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=40),
        axis.text.x  = element_text(size=35),
        legend.title = element_text(size=40,face="bold"),
        legend.text = element_text(size = 40),
        strip.text.x = element_text(size = 40),
        strip.text.y = element_text(size= 40),
        legend.key.size = unit(3, "lines"),
        panel.margin.x=unit(1.5, "lines"),
        panel.margin.y=unit(1.5, "lines"))
plot(p)

ggsave("relationship_rob_alt_single.pdf", width = 75, height = 50, units = "cm")


#####################Figure 11#############################################

df.mp<-df.multiple.predictor%>%
  group_by(distribution,sample.size,effect.size,analysis) %>%
  summarize(prob=sum(correct=='YES')/n()) %>%
  ungroup()%>%
  mutate(Prob_Reject_Null = ifelse(effect.size=="None", 1-prob,prob))

p <- ggplot()

p <- p + geom_line(data=df.mp, 
                   mapping=aes(x=sample.size, 
                               y=Prob_Reject_Null, 
                               group=effect.size))+scale_colour_manual(values=c('None'='#f7f7f7','Small'='#cccccc','Medium'='#969696','Large'='#525252'),name="Effect")
p <- p + scale_shape_manual(values=c(23,24,21,22),name="Effect")
p <- p + geom_point(data=df.mp, 
                    mapping=aes(x=sample.size, 
                                y=Prob_Reject_Null,     
                                group=effect.size,
                                fill=effect.size,
                                shape=effect.size),
                    size=12)+scale_fill_manual(values=c('None'='#f7f7f7','Small'='#cccccc','Medium'='#969696','Large'='#525252'),name="Effect")



p <- p + facet_grid(analysis~ distribution)
p <- p + theme_bw()
p <- p + xlab("Sample Size") + ylab("Prob. of Rejecting the Null")+
  theme(axis.title.y = element_text(size=40,angle = 90,hjust=.5),
        axis.text.y  = element_text(size=35),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=40),
        axis.text.x  = element_text(size=35),
        legend.title = element_text(size=40,face="bold"),
        legend.text = element_text(size = 40),
        strip.text.x = element_text(size = 40),
        strip.text.y = element_text(size= 40),
        legend.key.size = unit(3, "lines"),
        panel.margin.x=unit(1.5, "lines"),
        panel.margin.y=unit(1.5, "lines"))
plot(p)

ggsave("relationship_rob_alt_multiple.pdf", width = 75, height = 50, units = "cm")


########################Distributions#######################################################
plot_sim_hist<-ggplot() + 
   geom_histogram(data=distrib, aes(x=values),binwidth=1, center=0,
                 colour="black", fill="black",size=1) +facet_grid(Type~.) + theme_classic() +
  scale_x_continuous(breaks=seq(0, max(55),5),limits=c(-5,60),name='')+
  scale_y_continuous(breaks=seq(0, 25,110), limits=c(0,110),name='',expand = c(0, 0)) +
  theme(axis.title.x = element_text(face="bold", colour="Black", size=35),
        axis.text.x  = element_text(size=26),
        axis.title.y = element_blank(),
        strip.text.y = element_text(size = 30),
        axis.text.y = element_blank(),
        legend.position="none")
plot_sim_hist
ggsave("sim_data_hist.pdf", width = 40, height = 20, units = "cm")

