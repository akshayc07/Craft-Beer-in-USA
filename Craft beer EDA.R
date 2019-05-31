Beers<-read.csv("beers.csv")

Brewary<-read.csv("breweries.csv")

colnames(Brewary)<-c("brewery_id","name","city","state")

main<-Beers%>%full_join(Brewary,by="brewery_id")
glimpse(main)

#relation between the alcohol content and the bitterness of beer

ggplot(main,aes(abv,ibu))+geom_point()+geom_smooth()+
  xlab("Content of alcohol")+
  ylab("Innternational bittering unit")
#Number of breweries per state
main%>%group_by(state)%>%summarise(n=n_distinct(name.y))%>%ggplot(aes(x=state,y=n))+geom_col()+coord_flip()+
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(face="bold",size=5))+
  xlab("Number of Brewery")+
  ylab("States")

#unique styles of beer per state
main%>%group_by(state)%>%summarise(unique_style=n_distinct(style))%>%
  ggplot(aes(state,unique_style))+geom_col(colour="black")+
  xlab("State")+
  ylab("Style of Beers")+
  theme(axis.text.x = element_text(size=8,angle=90),
        axis.text.y = element_text(size=15))

#states with strongest breweries
main%>%group_by(state)%>%summarise(alcohol.content.average=mean(abv,na.rm = TRUE))%>%
  arrange(desc(alcohol.content.average))%>%top_n(10)%>%
  ggplot(aes(x=state,y=alcohol.content.average))+geom_bar(stat = "identity")


