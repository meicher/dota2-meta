#DOTA2 patch 6.86 meta data investigation
library(ggplot2)
library(reshape)

#SCRAPING DOTA BUFF FOR MOST CURRENT DATA------------------
#USE THE OUTPUTTED DATAFRAME FOR ANALYSIS-----------------
scrapedotabuff<- function(){
  require(rvest)
  crnt_dota<- read_html('http://www.dotabuff.com/heroes/meta')
  
  #get heroes
  Hero<- html_nodes(crnt_dota,'.link-type-hero') 
  Hero<- html_text(Hero)
  
  
  #get sub2k data
  a <- crnt_dota %>%
    html_nodes('td.r-group-1') %>%
    html_text()
  #split to vectors
  WIN_Sub2k<- a[seq(2,length(a),2)]
  PICK_Sub2k<- a[seq(1,length(a),2)]
  rm(a)
  
  #get 2-3k data
  a <- crnt_dota %>%
    html_nodes('td.r-group-2') %>%
    html_text()
  WIN_2.3<- a[seq(2,length(a),2)]
  PICK_2.3<- a[seq(1,length(a),2)]
  rm(a)
  
  #get 3-4
  a <- crnt_dota %>%
    html_nodes('td.r-group-3') %>%
    html_text()
  WIN_3.4<- a[seq(2,length(a),2)]
  PICK_3.4<- a[seq(1,length(a),2)]
  rm(a)
  
  #get 4-5
  a <- crnt_dota %>%
    html_nodes('td.r-group-4') %>%
    html_text()
  WIN_4.5<- a[seq(2,length(a),2)]
  PICK_4.5<- a[seq(1,length(a),2)]
  rm(a)
  
  #get 5+
  a <- crnt_dota %>%
    html_nodes('td.r-group-5') %>%
    html_text()
  WIN_5.<- a[seq(2,length(a),2)]
  PICK_5.<- a[seq(1,length(a),2)]
  rm(a)
  
  #convert str % fields to numeric decimals, then combine str hero column with cleaned int columns
  #store in updated frame
  updated<- cbind(PICK_Sub2k,WIN_Sub2k,PICK_2.3,WIN_2.3,PICK_3.4,WIN_3.4,PICK_4.5,WIN_4.5,PICK_5.,WIN_5.)
  updated<- apply(updated,2,function(x) as.numeric(sub("%", "", x))/100)
  updated<- cbind.data.frame(Hero,updated)
  updated<- data.frame(updated,stringsAsFactors = FALSE)
  return(updated) #END FUNCTION, OUTPUT FRAME----------------------------
}



#fxn to plot a heroes win rate at each bracket
#dota is the data frame
heroplot<- function(hero,type='win',Dota=dota){
  require(ggplot2)
  require(reshape)
  wins<- subset(Dota,Hero==hero,select=c(4,6,8,10,12))
  pick<- subset(Dota,Hero==hero,select=c(3,5,7,9,11))
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
#geom_point(aes(colour=factor(Group.1)),size=4))$data



dota<-scrapedotabuff()
dota['skill_diff']<- dota['WIN_5.']-dota['WIN_Sub2k']
dota['avgwin']<- (dota['WIN_Sub2k']+dota['WIN_2.3']+dota['WIN_3.4']+dota['WIN_4.5']+dota['WIN_5.'])/5
new<-read.csv('dota2_6.86_metadata.csv')
dota<- merge(new,dota)
rm(new)
group <- aggregate(dota[,3:14],by=list(dota$Attribute),FUN=mean)
#produce data.frame = dota, for analysis and plotting



#find min/max values at certain levels
#dota[dota['WIN_5.']== min(dota[['WIN_5.']])]

#order the dataframe
#attach(dota)
#dota<- dota[order(WIN_3.4,WIN_4.5),]



multiplot<- function(hero_vec,type='win',Dota=dota){
  require(ggplot2)
  require(reshape)
  wins<- subset(Dota,Hero %in% hero_vec,select=c(1,4,6,8,10,12))
  pick<- subset(Dota,Hero %in% hero_vec,select=c(1,3,5,7,9,11))
  wins<- melt(wins[,1:6])
  pick<- melt(pick[,1:6])
  if(type == 'win'){
  winplots<- ggplot(wins,aes(x=variable,y=value,group=Hero,colour=Hero)) + 
    geom_line(size=1) +
    geom_point(size=3,shape=21,fill='white')+
    xlab('Ranked MMR Bracket')+
    ylab('Pct Games Won')
    return(winplots)}
  else if(type == 'pick'){
    pickplot<- ggplot(pick,aes(x=variable,y=value,group=Hero,colour=Hero)) + 
      geom_line(size=1) +
      geom_point(size=3,shape=21,fill='white')+
      xlab('Ranked MMR Bracket')+
      ylab('Pct Games Picked')
    return(pickplot)}
}
