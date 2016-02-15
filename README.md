# dota2-meta
Exploring 6.86 Dota2 data
#DOTA2 patch 6.86 meta data investigation
library(ggplot2)
library(reshape)

dota<-read.csv('dota2_6.86_metadata.csv',stringsAsFactors = FALSE)
dota['skill_diff']<- dota['WIN_5.']-dota['WIN_Sub2k']
dota['avgwin']<- (dota['WIN_Sub2k']+dota['WIN_2.3']+dota['WIN_3.4']+dota['WIN_4.5']+dota['WIN_5.'])/5
#as expected, IO, CHEN, EARTHSPIRIT require the most skill to play, 16% increased win rate from sub2k to 5k + bracket
#code heroes as Agi, Str, Int
#Type (i.e. carry, nuker, disable)

#first two return numeric vectors. Single bracket (3rd line) will only return sublist-- still a dataframe of dim (1,x)
#class(dota[,'WIN_5.'])
#class(dota[['WIN_5.']])
#class(dota['WIN_5.'])

#find min/max values at certain levels
#dota[dota['WIN_5.']== min(dota[['WIN_5.']])]

#order the dataframe
#attach(dota)
#dota<- dota[order(WIN_3.4,WIN_4.5),]


#avg of win columns are all around 49%, this is because lower win rate heroes get picked less, needs to be weighted for it to be 50%

#fxn to plot a heroes win rate at each bracket
heroplot<- function(hero,type='win'){
  require(ggplot2)
  require(reshape)
  wins<- subset(dota,Hero==hero,select=c(3,5,7,9,11))
  pick<- subset(dota,Hero==hero,select=c(2,4,6,8,10))
  wins<- melt(wins[,1:5])
  pick<- melt(pick[,1:5])
  if(type == 'win'){
  winplots<- ggplot(wins,aes(x=variable,y=value,group=1,label=wins[['value']]*100)) + 
    geom_line(size=1,colour='orange') +
    geom_label()+
    annotate('text',label=paste(hero,'Win Pct vs MMR Bracket'),size=7,colour='tomato1',x=2,y=.53)+
    xlab('Ranked MMR Bracket')+
    ylab('Pct Games Won')
    return(winplots)}
  else if (type =='pick'){
      pickplot<-ggplot(pick,aes(x=variable,y=value,group=1,label=pick[['value']]*100)) + 
        geom_line(size=1,colour='orange') +
        geom_label()+
        annotate('text',label=paste(hero,'Pick Pct vs MMR Bracket'),size=7,colour='tomato1',x=2,y=.53)+
        xlab('Ranked MMR Bracket')+
        ylab('Pct Games Picked')
      return(pickplot)
    }
}



group <- aggregate(dota[,2:11],by=list(dota$Attribute),FUN=mean)

attributeplot<- function(){
melted<- melt(group[,c(1,3,5,7,9,11)])
ggplot(melted,aes(x=variable,y=value))+
  geom_point(aes(colour=factor(Group.1)),size=6)+
  scale_colour_manual(name ='Primary Attribute',values=c('#00BA38','#619CFF','#F8766D'))+
  xlab('Ranked MMR Bracket')+
  ylab('Group Win Pct')
}
#look at the colour codes in order to manually adjust them per attribute type
#ggplot_build(ggplot(melted,aes(x=variable,y=value,group=2))+
        #       geom_point(aes(colour=factor(Group.1)),size=4))$data


#OUTSTANDING ITEMS (FUNCTIONS TO CREATE)
#1) function to plot a vector of heroes on same graph, as lines
#2) function to webscrape dotabuff.com in order to keep data up to date on each use


