#HW12: Extra Credit (20 pts)

#Q1. Use `world` map to select the country of your birth
#together with bordering countries. Make a plot of that region
#and add a labeled point to indicate the town/city where you
#were born.


#If you do not wish to share your birth place, select Nepal and
#bordering countries. Make a plot of that region
#and add a labeled point to indicate the capital of Nepal.

################ write your script here #################
#### the requested output should show up in R studio ####
###### when I select and run your lines of code #########

#start with
library(ggplot2)
library(ggmap)

world<-map_data('world')
# choose neighbouring countries of India
Neigbours<-c("India","Pakistan", "Bhutan", "Bangldesh","Pakistan","China","Nepal")
Indiabordcon<-filter(Neigbours, region %in% test )
Indiabordcon<-world[(world$region=="India" |world$region=="Pakistan"|
                       world$region=="China"|
                       world$region=="Bhutan" |world$region=="Nepal"),]
# fill it with light green
p<-ggplot(Indiabordcon)+geom_polygon(aes(x=long,y=lat, group=group),fill="Light green", color="black")
p # show plot
sites<-c("Coimbatore") # choose my city
MySites<-geocode(sites)
rownames(MySites)<-sites
# show city on the Map
p+geom_point(data=MySites, aes(lon, lat),color='red', size=2)+
  geom_text(data=MySites,
            aes(x=lon, y=lat, label=rownames(MySites)),
            position= position_nudge(0, 2),
            color='darkblue')+
            coord_map()+  
            theme_nothing()
################### END OF SCRIPT #########################

#Q2. Use data from myDemData_clean.txt. 
#a)Write a user-defined function that will compute the mean
#poverty level for user-specified state.
#b)Provide an example of function call
#(that is, show how you would use this function)

############# write your functions here #################
MeanPov<- function(ds,st) # pass Dataset & state
{
  Sum<-0;res<-0;count<-0
  for (i in 1:nrow(ds))
  {
    if(ds$State[i]==st) # choose records where ds-state= input state
    {
      Sum<-Sum+ds$Poverty[i] # add the poverty
      count<-count+1 # add count
    }
    
  }
  if (count!=0)
  {
    return (Sum/count) # div if count not 0
  }
  else
  {
    return('No data found in Demdata dataset for the state');
  }
}


################# call your function ####################

DD_clean<-read.csv("DemData_clean.txt", sep="\t") # read the dataset
MeanPov(DD_clean,"AZ") # Example call to the function
