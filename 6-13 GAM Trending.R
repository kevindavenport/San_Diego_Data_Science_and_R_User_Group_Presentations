rm(list=ls()) 
libraries <- c("MASS","car","ggplot2","plyr","psych","doBy","RColorBrewer","data.table")
lapply(libraries, require, character.only=T)
getwd()
#setwd("~/")

dev.off()

df <- read.csv ("df.csv", header = TRUE, sep = ",", quote="\"", dec=".",
  fill = FALSE,colClasses=c("Audit_Completion_Date"="Date"))

str(df)

df$score_scale <- ave(df$Audit_Score, df$Round_Title, FUN = scale) 

#groupSubset <- df[ df$Group %in% c("Group 2","Group 1"), ] 

#Raw score
ggplot(data = df, aes(x = Audit_Completion_Date, y = Audit_Score, group = Group)) + 
  geom_point(aes(color = Group),
  subset = .(Group == "Group 1" | Group == "Group 2"), size=2.5, alpha = .1)

#Scaled score
ggplot(data=df, aes(x=Audit_Completion_Date,y=score_scale, group = Group)) +
  geom_point(aes(color = Group), size=2.5,alpha = .05)  

df <- subset(df, Audit_Completion_Date < "2012-12-19 ")
df <- subset(df, Audit_Score > 60)


#All data  gam (group 1)
#last_plot() + stat_smooth(aes(group = 1),fill = "darkgray", se=TRUE,col="darkorchid4", size = .9, level =.95,fullrange = FALSE,lty=1) 

#gam by group
last_plot() + stat_smooth(aes(color = Group),
  fill = "darkgray", se=F, size = 1,
  level =.95,fullrange = TRUE) + 
  scale_color_brewer(palette="Set2")


#All data LM (Group 1) 
last_plot() + stat_smooth(aes(group = 1), method="lm",
  se=F,col="black", size = 1, level =.95,fullrange = FALSE) 

#connected line of one specific unit (uptrend)
last_plot() + geom_line(subset = .(Audit_Location_Name == 18908),size = .9, col="dodgerblue") + geom_point(subset = .(Audit_Location_Name == 18908),size = 3.5,col="dodgerblue") 

last_plot() + geom_line(subset = .(Audit_Location_Name == 20864),size = .9, col="dodgerblue2") + geom_point(subset = .(Audit_Location_Name == 20864),size = 3.5,col="dodgerblue2") 

last_plot() + geom_line(subset = .(Audit_Location_Name == 027572),size = .9, col="dodgerblue3") + geom_point(subset = .(Audit_Location_Name == 027572),size = 3.5,col="dodgerblue3") 

last_plot() + geom_line(subset = .(Audit_Location_Name == 019572),size = .9, col="dodgerblue1") + geom_point(subset = .(Audit_Location_Name == 019572),size = 3.5,col="dodgerblue1") 

last_plot() + geom_line(subset = .(Audit_Location_Name == 003372),size = .9, col="cornflowerblue") + geom_point(subset = .(Audit_Location_Name == 003372),size = 3.5,col="cornflowerblue") 


#connected line of one specific unit (downtrend)
last_plot() + geom_line(subset = .(Audit_Location_Name == 028088),size = .9, col="tomato") + geom_point(subset = .(Audit_Location_Name == 028088),size = 3.5,col="tomato") 

last_plot() + geom_line(subset = .(Audit_Location_Name == 025444),size = .9, col="tomato") + geom_point(subset = .(Audit_Location_Name == 025444),size = 3.5,col="tomato") 

#last_plot() + geom_line(subset = .(Audit_Location_Name == 003441),size = .9, col="tomato1") + geom_point(subset = .(Audit_Location_Name == 003441),size = 3.5,col="tomato2") 

#last_plot() + geom_line(subset = .(Audit_Location_Name == 005553),size = .9, col="tomato3") + geom_point(subset = .(Audit_Location_Name == 005553),size = 3.5,col="tomato3") 

#last_plot() + geom_line(subset = .(Audit_Location_Name == 023232),size = .9, col="tomato2") + geom_point(subset = .(Audit_Location_Name == 023232),size = 3.5,col="tomato2") 


#scaled
ggplot<- ggplot(data=df, aes(x=Audit_Completion_Date,y=score_scale, group = Group)) #Scaled
ggplot + geom_point(aes(color = Group), size=3.5,alpha = I(.5)) + scale_colour_brewer(type="qual", palette="Dark2") + stat_hline(geom = "hline", position = "identity", yintercept = 0)


