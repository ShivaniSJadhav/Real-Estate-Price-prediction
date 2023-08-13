setwd("C:/R/Codes/Data sets")
house_train = read.csv("housing_train.csv")
house_test = read.csv("housing_test.csv")

View(house_train)
View(house_test)

library(dplyr)

glimpse(house_train)

house_test$Price = NA 

house_train$data = 'train'
house_test$data = 'test'

house_all = rbind(house_train, house_test)
View(house_all)
house_all$SellerG = NULL
glimpse(house_all)


colSums(is.na(house_all))
table(house_all$Bathroom)
table(house_all$Bedroom2)
table(house_all$Car)
table(house_all$Landsize)
table(house_all$BuildingArea)
table(house_all$YearBuilt)

house_all$Bedroom2[is.na(house_all$Bedroom2)] = round(mean(house_all$Bedroom2, na.rm = T),6)

house_all$Bathroom[is.na(house_all$Bathroom)] = mean(house_all$Bathroom, na.rm = T)

house_all$Car[is.na(house_all$Car)] = mean(house_all$Car, na.rm = T)

house_all$Landsize[is.na(house_all$Landsize)] = mean(house_all$Landsize, na.rm = T)

house_all$BuildingArea[is.na(house_all$BuildingArea)] = mean(house_all$BuildingArea, na.rm = T)

house_all$YearBuilt[is.na(house_all$YearBuilt)] = mean(house_all$YearBuilt, na.rm = T)


colSums(is.na(house_all))
              
glimpse(house_all)        

table(house_all$Type)

house_all = house_all %>%
           mutate(Type_h = as.numeric(Type =="h"),
                  Type_t = as.numeric(Type == "t")) %>%
          select(-Type)
glimpse(house_all)

table(house_all$Method)500
table(house_all$CouncilArea)100
table(house_all$Suburb)50




CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}
house_all=CreateDummies(house_all ,"Method",500)
house_all=CreateDummies(house_all,"CouncilArea",100)
house_all=CreateDummies(house_all,"Suburb",50)

sum(is.na(house_all))

house_train = house_all %>% filter(data=='train') %>% select(-data,-Address)
house_test = house_all %>% filter(data=='test') %>% select (-data,-Price,-Address)

write.csv(house_train,"house_train_clean.csv", row.names = F)
write.csv(house_test,"house_test_clean.csv", row.names = F)


house_train = read.csv("house_train_clean.csv")
house_test = read.csv("house_test_clean.csv")


View(house_train)
View(house_test)

set.seed(2)
s=sample(1:nrow(house_train), 0.8*nrow(house_train))
house_train1=house_train[s,] 
house_train2=house_train[-s,] 


fit = lm(Price ~ ., data = house_train1)

summary(fit)

library(car)

vif(fit)
tail(sort(vif(fit)))

# Removing CouncilArea_ with vif  13.698067 

fit = lm(Price ~ .-CouncilArea_, data = house_train1)

sort(vif(fit), decreasing = T)[1:5]
tail(sort(vif(fit)))

# Removing  Postcode with vif 8.895405 
fit = lm(Price ~ .-CouncilArea_ - Postcode , data = house_train1)
tail(sort(vif(fit)))

# Removing Distance with vif 7.213655
fit = lm(Price ~ .-CouncilArea_ - Postcode - Distance , data = house_train1)
tail(sort(vif(fit)))
summary(fit)

sort((summary(fit)$coefficients)[,4])
sort((summary(fit)$coefficients)[,4], decreasing = T)[1:5]

fit = lm(Price ~ (Type_h + Rooms+Bathroom + Suburb_Toorak + Suburb_Brighton +Suburb_Reservoir+ 
                    Suburb_Malvern +YearBuilt+Suburb_Armadale+BuildingArea +Suburb_Glenroy + CouncilArea_PortPhillip +
                    Suburb_Balwyn +Car + Suburb_Kew + Type_t + Suburb_KeilorEast+  
                    Suburb_KeilorEast + Suburb_Camberwell+Suburb_Preston+ Suburb_SouthYarra +Suburb_SunshineWest + 
                    Suburb_Hampton + Suburb_Fawkner + Suburb_SunshineNorth + Suburb_GlenIris + Suburb_Hadfield +    
                    CouncilArea_Brimbank + Suburb_AvondaleHeights + Suburb_AirportWest + Bedroom2 +
                    Suburb_BrightonEast + Suburb_AltonaNorth + Suburb_MalvernEast + CouncilArea_Yarra +  
                    CouncilArea_HobsonsBay + CouncilArea_Banyule +Suburb_OakleighSouth + Suburb_Williamstown +   
                    CouncilArea_Melbourne + Suburb_HawthornEast +Suburb_Hawthorn +Method_S +
                    CouncilArea_Maribyrnong +Suburb_TemplestoweLower+ Suburb_PascoeVale+
                    Suburb_Moorabbin+ Suburb_HeidelbergWest +Suburb_Prahran+Suburb_CoburgNorth+ 
                    CouncilArea_Boroondara + Suburb_BalwynNorth +Suburb_BentleighEast + Suburb_Ivanhoe +
                    Suburb_HeidelbergHeights +Suburb_Maidstone +  Suburb_Kensington+      
                    Suburb_Coburg + Suburb_Sunshine + CouncilArea_Moreland + Landsize + Suburb_WestFootscray +
                    Suburb_SurreyHills +  Suburb_Bulleen + CouncilArea_MooneeValley+ 
                    Suburb_Niddrie +Suburb_Strathmore+Suburb_Rosanna +Suburb_Ashburton +
                    CouncilArea_Darebin + Suburb_Melbourne +Suburb_Essendon +Suburb_Northcote+
                    CouncilArea_Whitehorse + CouncilArea_Bayside +  Method_PI + Suburb_Doncaster + 
                    Method_SP + Suburb_Thornbury + Suburb_Ormond + Suburb_Abbotsford + 
                    Suburb_Burwood +  CouncilArea_Stonnington +  Suburb_AscotVale  + Suburb_StKilda +
                    Suburb_Bentleigh + Suburb_Elwood +  Suburb_MooneePonds + Suburb_SouthMelbourne +
                    Suburb_BrunswickEast + Suburb_Murrumbeena +  Suburb_BrunswickWest + Suburb_FitzroyNorth + 
                    CouncilArea_GlenEira +Suburb_Carnegie +  Suburb_Newport + Suburb_PortMelbourne + Suburb_NorthMelbourne +          
                    Suburb_Richmond + Suburb_Brunswick + Suburb_Yarraville + Suburb_Footscray + CouncilArea_Manningham),data = house_train1)




summary(fit)
round(sort((summary(fit)$coefficients)[,4]),3)

# Removing  Suburb_Richmond with vif 0.973
fit = lm(Price ~ (Type_h + Rooms+Bathroom + Suburb_Toorak + Suburb_Brighton +Suburb_Reservoir+ 
                    Suburb_Malvern +YearBuilt+Suburb_Armadale+BuildingArea +Suburb_Glenroy + CouncilArea_PortPhillip +
                    Suburb_Balwyn +Car + Suburb_Kew + Type_t + Suburb_KeilorEast+  
                    Suburb_KeilorEast + Suburb_Camberwell+Suburb_Preston+ Suburb_SouthYarra +Suburb_SunshineWest + 
                    Suburb_Hampton + Suburb_Fawkner + Suburb_SunshineNorth + Suburb_GlenIris + Suburb_Hadfield +    
                    CouncilArea_Brimbank + Suburb_AvondaleHeights + Suburb_AirportWest + Bedroom2 +
                    Suburb_BrightonEast + Suburb_AltonaNorth + Suburb_MalvernEast + CouncilArea_Yarra +  
                    CouncilArea_HobsonsBay + CouncilArea_Banyule +Suburb_OakleighSouth + Suburb_Williamstown +   
                    CouncilArea_Melbourne + Suburb_HawthornEast +Suburb_Hawthorn +Method_S +
                    CouncilArea_Maribyrnong +Suburb_TemplestoweLower+ Suburb_PascoeVale+
                    Suburb_Moorabbin+ Suburb_HeidelbergWest +Suburb_Prahran+Suburb_CoburgNorth+ 
                    CouncilArea_Boroondara + Suburb_BalwynNorth +Suburb_BentleighEast + Suburb_Ivanhoe +
                    Suburb_HeidelbergHeights +Suburb_Maidstone +  Suburb_Kensington+      
                    Suburb_Coburg + Suburb_Sunshine + CouncilArea_Moreland + Landsize + Suburb_WestFootscray +
                    Suburb_SurreyHills +  Suburb_Bulleen + CouncilArea_MooneeValley+ 
                    Suburb_Niddrie +Suburb_Strathmore+Suburb_Rosanna +Suburb_Ashburton +
                    CouncilArea_Darebin + Suburb_Melbourne +Suburb_Essendon +Suburb_Northcote+
                    CouncilArea_Whitehorse + CouncilArea_Bayside +  Method_PI + Suburb_Doncaster + 
                    Method_SP + Suburb_Thornbury + Suburb_Ormond + Suburb_Abbotsford + 
                    Suburb_Burwood +  CouncilArea_Stonnington +  Suburb_AscotVale  + Suburb_StKilda +
                    Suburb_Bentleigh + Suburb_Elwood +  Suburb_MooneePonds + Suburb_SouthMelbourne +
                    Suburb_BrunswickEast + Suburb_Murrumbeena +  Suburb_BrunswickWest + Suburb_FitzroyNorth + 
                    CouncilArea_GlenEira +Suburb_Carnegie +  Suburb_Newport + Suburb_PortMelbourne + Suburb_NorthMelbourne +          
                    Suburb_Brunswick + Suburb_Yarraville + Suburb_Footscray + CouncilArea_Manningham),data = house_train1)

summary(fit)
round(sort((summary(fit)$coefficients)[,4]),3)

# Removing  Suburb_BrunswickWest with vif 0.875 

fit = lm(Price ~ (Type_h + Rooms+Bathroom + Suburb_Toorak + Suburb_Brighton +Suburb_Reservoir+ 
                    Suburb_Malvern +YearBuilt+Suburb_Armadale+BuildingArea +Suburb_Glenroy + CouncilArea_PortPhillip +
                    Suburb_Balwyn +Car + Suburb_Kew + Type_t + Suburb_KeilorEast+  
                    Suburb_KeilorEast + Suburb_Camberwell+Suburb_Preston+ Suburb_SouthYarra +Suburb_SunshineWest + 
                    Suburb_Hampton + Suburb_Fawkner + Suburb_SunshineNorth + Suburb_GlenIris + Suburb_Hadfield +    
                    CouncilArea_Brimbank + Suburb_AvondaleHeights + Suburb_AirportWest + Bedroom2 +
                    Suburb_BrightonEast + Suburb_AltonaNorth + Suburb_MalvernEast + CouncilArea_Yarra +  
                    CouncilArea_HobsonsBay + CouncilArea_Banyule +Suburb_OakleighSouth + Suburb_Williamstown +   
                    CouncilArea_Melbourne + Suburb_HawthornEast +Suburb_Hawthorn +Method_S +
                    CouncilArea_Maribyrnong +Suburb_TemplestoweLower+ Suburb_PascoeVale+
                    Suburb_Moorabbin+ Suburb_HeidelbergWest +Suburb_Prahran+Suburb_CoburgNorth+ 
                    CouncilArea_Boroondara + Suburb_BalwynNorth +Suburb_BentleighEast + Suburb_Ivanhoe +
                    Suburb_HeidelbergHeights +Suburb_Maidstone +  Suburb_Kensington+      
                    Suburb_Coburg + Suburb_Sunshine + CouncilArea_Moreland + Landsize + Suburb_WestFootscray +
                    Suburb_SurreyHills +  Suburb_Bulleen + CouncilArea_MooneeValley+ 
                    Suburb_Niddrie +Suburb_Strathmore+Suburb_Rosanna +Suburb_Ashburton +
                    CouncilArea_Darebin + Suburb_Melbourne +Suburb_Essendon +Suburb_Northcote+
                    CouncilArea_Whitehorse + CouncilArea_Bayside +  Method_PI + Suburb_Doncaster + 
                    Method_SP + Suburb_Thornbury + Suburb_Ormond + Suburb_Abbotsford + 
                    Suburb_Burwood +  CouncilArea_Stonnington +  Suburb_AscotVale  + Suburb_StKilda +
                    Suburb_Bentleigh + Suburb_Elwood +  Suburb_MooneePonds + Suburb_SouthMelbourne +
                    Suburb_BrunswickEast + Suburb_Murrumbeena  + Suburb_FitzroyNorth + 
                    CouncilArea_GlenEira +Suburb_Carnegie +  Suburb_Newport + Suburb_PortMelbourne + Suburb_NorthMelbourne +          
                    Suburb_Brunswick + Suburb_Yarraville + Suburb_Footscray + CouncilArea_Manningham),data = house_train1)

summary(fit)
round(sort((summary(fit)$coefficients)[,4]),3)

# Removing  Suburb_SouthMelbourne  with vif 0.835  
fit = lm(Price ~ (Type_h + Rooms+Bathroom + Suburb_Toorak + Suburb_Brighton +Suburb_Reservoir+ 
                    Suburb_Malvern +YearBuilt+Suburb_Armadale+BuildingArea +Suburb_Glenroy + CouncilArea_PortPhillip +
                    Suburb_Balwyn +Car + Suburb_Kew + Type_t + Suburb_KeilorEast+  
                    Suburb_KeilorEast + Suburb_Camberwell+Suburb_Preston+ Suburb_SouthYarra +Suburb_SunshineWest + 
                    Suburb_Hampton + Suburb_Fawkner + Suburb_SunshineNorth + Suburb_GlenIris + Suburb_Hadfield +    
                    CouncilArea_Brimbank + Suburb_AvondaleHeights + Suburb_AirportWest + Bedroom2 +
                    Suburb_BrightonEast + Suburb_AltonaNorth + Suburb_MalvernEast + CouncilArea_Yarra +  
                    CouncilArea_HobsonsBay + CouncilArea_Banyule +Suburb_OakleighSouth + Suburb_Williamstown +   
                    CouncilArea_Melbourne + Suburb_HawthornEast +Suburb_Hawthorn +Method_S +
                    CouncilArea_Maribyrnong +Suburb_TemplestoweLower+ Suburb_PascoeVale+
                    Suburb_Moorabbin+ Suburb_HeidelbergWest +Suburb_Prahran+Suburb_CoburgNorth+ 
                    CouncilArea_Boroondara + Suburb_BalwynNorth +Suburb_BentleighEast + Suburb_Ivanhoe +
                    Suburb_HeidelbergHeights +Suburb_Maidstone +  Suburb_Kensington+      
                    Suburb_Coburg + Suburb_Sunshine + CouncilArea_Moreland + Landsize + Suburb_WestFootscray +
                    Suburb_SurreyHills +  Suburb_Bulleen + CouncilArea_MooneeValley+ 
                    Suburb_Niddrie +Suburb_Strathmore+Suburb_Rosanna +Suburb_Ashburton +
                    CouncilArea_Darebin + Suburb_Melbourne +Suburb_Essendon +Suburb_Northcote+
                    CouncilArea_Whitehorse + CouncilArea_Bayside +  Method_PI + Suburb_Doncaster + 
                    Method_SP + Suburb_Thornbury + Suburb_Ormond + Suburb_Abbotsford + 
                    Suburb_Burwood +  CouncilArea_Stonnington +  Suburb_AscotVale  + Suburb_StKilda +
                    Suburb_Bentleigh + Suburb_Elwood +  Suburb_MooneePonds  +
                    Suburb_BrunswickEast + Suburb_Murrumbeena  + Suburb_FitzroyNorth + 
                    CouncilArea_GlenEira +Suburb_Carnegie +  Suburb_Newport + Suburb_PortMelbourne + Suburb_NorthMelbourne +          
                    Suburb_Brunswick + Suburb_Yarraville + Suburb_Footscray + CouncilArea_Manningham),data = house_train1)

summary(fit)
round(sort((summary(fit)$coefficients)[,4]),3)

# Removing  Suburb_NorthMelbourne  with vif   0.765 
fit = lm(Price ~ (Type_h + Rooms+Bathroom + Suburb_Toorak + Suburb_Brighton +Suburb_Reservoir+ 
                    Suburb_Malvern +YearBuilt+Suburb_Armadale+BuildingArea +Suburb_Glenroy + CouncilArea_PortPhillip +
                    Suburb_Balwyn +Car + Suburb_Kew + Type_t + Suburb_KeilorEast+  
                    Suburb_KeilorEast + Suburb_Camberwell+Suburb_Preston+ Suburb_SouthYarra +Suburb_SunshineWest + 
                    Suburb_Hampton + Suburb_Fawkner + Suburb_SunshineNorth + Suburb_GlenIris + Suburb_Hadfield +    
                    CouncilArea_Brimbank + Suburb_AvondaleHeights + Suburb_AirportWest + Bedroom2 +
                    Suburb_BrightonEast + Suburb_AltonaNorth + Suburb_MalvernEast + CouncilArea_Yarra +  
                    CouncilArea_HobsonsBay + CouncilArea_Banyule +Suburb_OakleighSouth + Suburb_Williamstown +   
                    CouncilArea_Melbourne + Suburb_HawthornEast +Suburb_Hawthorn +Method_S +
                    CouncilArea_Maribyrnong +Suburb_TemplestoweLower+ Suburb_PascoeVale+
                    Suburb_Moorabbin+ Suburb_HeidelbergWest +Suburb_Prahran+Suburb_CoburgNorth+ 
                    CouncilArea_Boroondara + Suburb_BalwynNorth +Suburb_BentleighEast + Suburb_Ivanhoe +
                    Suburb_HeidelbergHeights +Suburb_Maidstone +  Suburb_Kensington+      
                    Suburb_Coburg + Suburb_Sunshine + CouncilArea_Moreland + Landsize + Suburb_WestFootscray +
                    Suburb_SurreyHills +  Suburb_Bulleen + CouncilArea_MooneeValley+ 
                    Suburb_Niddrie +Suburb_Strathmore+Suburb_Rosanna +Suburb_Ashburton +
                    CouncilArea_Darebin + Suburb_Melbourne +Suburb_Essendon +Suburb_Northcote+
                    CouncilArea_Whitehorse + CouncilArea_Bayside +  Method_PI + Suburb_Doncaster + 
                    Method_SP + Suburb_Thornbury + Suburb_Ormond + Suburb_Abbotsford + 
                    Suburb_Burwood +  CouncilArea_Stonnington +  Suburb_AscotVale  + Suburb_StKilda +
                    Suburb_Bentleigh + Suburb_Elwood +  Suburb_MooneePonds  +
                    Suburb_BrunswickEast + Suburb_Murrumbeena  + Suburb_FitzroyNorth + 
                    CouncilArea_GlenEira +Suburb_Carnegie +  Suburb_Newport + Suburb_PortMelbourne  +          
                    Suburb_Brunswick + Suburb_Yarraville + Suburb_Footscray + CouncilArea_Manningham),data = house_train1)

round(sort((summary(fit)$coefficients)[,4]),3)

# Removing Suburb_Newport  with vif 0.758 
fit = lm(Price ~ (Type_h + Rooms+Bathroom + Suburb_Toorak + Suburb_Brighton +Suburb_Reservoir+ 
                    Suburb_Malvern +YearBuilt+Suburb_Armadale+BuildingArea +Suburb_Glenroy + CouncilArea_PortPhillip +
                    Suburb_Balwyn +Car + Suburb_Kew + Type_t + Suburb_KeilorEast+  
                    Suburb_KeilorEast + Suburb_Camberwell+Suburb_Preston+ Suburb_SouthYarra +Suburb_SunshineWest + 
                    Suburb_Hampton + Suburb_Fawkner + Suburb_SunshineNorth + Suburb_GlenIris + Suburb_Hadfield +    
                    CouncilArea_Brimbank + Suburb_AvondaleHeights + Suburb_AirportWest + Bedroom2 +
                    Suburb_BrightonEast + Suburb_AltonaNorth + Suburb_MalvernEast + CouncilArea_Yarra +  
                    CouncilArea_HobsonsBay + CouncilArea_Banyule +Suburb_OakleighSouth + Suburb_Williamstown +   
                    CouncilArea_Melbourne + Suburb_HawthornEast +Suburb_Hawthorn +Method_S +
                    CouncilArea_Maribyrnong +Suburb_TemplestoweLower+ Suburb_PascoeVale+
                    Suburb_Moorabbin+ Suburb_HeidelbergWest +Suburb_Prahran+Suburb_CoburgNorth+ 
                    CouncilArea_Boroondara + Suburb_BalwynNorth +Suburb_BentleighEast + Suburb_Ivanhoe +
                    Suburb_HeidelbergHeights +Suburb_Maidstone +  Suburb_Kensington+      
                    Suburb_Coburg + Suburb_Sunshine + CouncilArea_Moreland + Landsize + Suburb_WestFootscray +
                    Suburb_SurreyHills +  Suburb_Bulleen + CouncilArea_MooneeValley+ 
                    Suburb_Niddrie +Suburb_Strathmore+Suburb_Rosanna +Suburb_Ashburton +
                    CouncilArea_Darebin + Suburb_Melbourne +Suburb_Essendon +Suburb_Northcote+
                    CouncilArea_Whitehorse + CouncilArea_Bayside +  Method_PI + Suburb_Doncaster + 
                    Method_SP + Suburb_Thornbury + Suburb_Ormond + Suburb_Abbotsford + 
                    Suburb_Burwood +  CouncilArea_Stonnington +  Suburb_AscotVale  + Suburb_StKilda +
                    Suburb_Bentleigh + Suburb_Elwood +  Suburb_MooneePonds  +
                    Suburb_BrunswickEast + Suburb_Murrumbeena  + Suburb_FitzroyNorth + 
                    CouncilArea_GlenEira +Suburb_Carnegie  + Suburb_PortMelbourne  +          
                    Suburb_Brunswick + Suburb_Yarraville + Suburb_Footscray + CouncilArea_Manningham),data = house_train1)


round(sort((summary(fit)$coefficients)[,4]),3)

# Removing Suburb_FitzroyNorth  with vif 0.755
fit = lm(Price ~ (Type_h + Rooms+Bathroom + Suburb_Toorak + Suburb_Brighton +Suburb_Reservoir+ 
                    Suburb_Malvern +YearBuilt+Suburb_Armadale+BuildingArea +Suburb_Glenroy + CouncilArea_PortPhillip +
                    Suburb_Balwyn +Car + Suburb_Kew + Type_t + Suburb_KeilorEast+  
                    Suburb_KeilorEast + Suburb_Camberwell+Suburb_Preston+ Suburb_SouthYarra +Suburb_SunshineWest + 
                    Suburb_Hampton + Suburb_Fawkner + Suburb_SunshineNorth + Suburb_GlenIris + Suburb_Hadfield +    
                    CouncilArea_Brimbank + Suburb_AvondaleHeights + Suburb_AirportWest + Bedroom2 +
                    Suburb_BrightonEast + Suburb_AltonaNorth + Suburb_MalvernEast + CouncilArea_Yarra +  
                    CouncilArea_HobsonsBay + CouncilArea_Banyule +Suburb_OakleighSouth + Suburb_Williamstown +   
                    CouncilArea_Melbourne + Suburb_HawthornEast +Suburb_Hawthorn +Method_S +
                    CouncilArea_Maribyrnong +Suburb_TemplestoweLower+ Suburb_PascoeVale+
                    Suburb_Moorabbin+ Suburb_HeidelbergWest +Suburb_Prahran+Suburb_CoburgNorth+ 
                    CouncilArea_Boroondara + Suburb_BalwynNorth +Suburb_BentleighEast + Suburb_Ivanhoe +
                    Suburb_HeidelbergHeights +Suburb_Maidstone +  Suburb_Kensington+      
                    Suburb_Coburg + Suburb_Sunshine + CouncilArea_Moreland + Landsize + Suburb_WestFootscray +
                    Suburb_SurreyHills +  Suburb_Bulleen + CouncilArea_MooneeValley+ 
                    Suburb_Niddrie +Suburb_Strathmore+Suburb_Rosanna +Suburb_Ashburton +
                    CouncilArea_Darebin + Suburb_Melbourne +Suburb_Essendon +Suburb_Northcote+
                    CouncilArea_Whitehorse + CouncilArea_Bayside +  Method_PI + Suburb_Doncaster + 
                    Method_SP + Suburb_Thornbury + Suburb_Ormond + Suburb_Abbotsford + 
                    Suburb_Burwood +  CouncilArea_Stonnington +  Suburb_AscotVale  + Suburb_StKilda +
                    Suburb_Bentleigh + Suburb_Elwood +  Suburb_MooneePonds  +
                    Suburb_BrunswickEast + Suburb_Murrumbeena  + 
                    CouncilArea_GlenEira +Suburb_Carnegie  + Suburb_PortMelbourne  +          
                    Suburb_Brunswick + Suburb_Yarraville + Suburb_Footscray + CouncilArea_Manningham),data = house_train1)

round(sort((summary(fit)$coefficients)[,4]),3)

# Removing Suburb_Bentleigh  with vif  0.728 
fit = lm(Price ~ (Type_h + Rooms+Bathroom + Suburb_Toorak + Suburb_Brighton +Suburb_Reservoir+ 
                    Suburb_Malvern +YearBuilt+Suburb_Armadale+BuildingArea +Suburb_Glenroy + CouncilArea_PortPhillip +
                    Suburb_Balwyn +Car + Suburb_Kew + Type_t + Suburb_KeilorEast+  
                    Suburb_KeilorEast + Suburb_Camberwell+Suburb_Preston+ Suburb_SouthYarra +Suburb_SunshineWest + 
                    Suburb_Hampton + Suburb_Fawkner + Suburb_SunshineNorth + Suburb_GlenIris + Suburb_Hadfield +    
                    CouncilArea_Brimbank + Suburb_AvondaleHeights + Suburb_AirportWest + Bedroom2 +
                    Suburb_BrightonEast + Suburb_AltonaNorth + Suburb_MalvernEast + CouncilArea_Yarra +  
                    CouncilArea_HobsonsBay + CouncilArea_Banyule +Suburb_OakleighSouth + Suburb_Williamstown +   
                    CouncilArea_Melbourne + Suburb_HawthornEast +Suburb_Hawthorn +Method_S +
                    CouncilArea_Maribyrnong +Suburb_TemplestoweLower+ Suburb_PascoeVale+
                    Suburb_Moorabbin+ Suburb_HeidelbergWest +Suburb_Prahran+Suburb_CoburgNorth+ 
                    CouncilArea_Boroondara + Suburb_BalwynNorth +Suburb_BentleighEast + Suburb_Ivanhoe +
                    Suburb_HeidelbergHeights +Suburb_Maidstone +  Suburb_Kensington+      
                    Suburb_Coburg + Suburb_Sunshine + CouncilArea_Moreland + Landsize + Suburb_WestFootscray +
                    Suburb_SurreyHills +  Suburb_Bulleen + CouncilArea_MooneeValley+ 
                    Suburb_Niddrie +Suburb_Strathmore+Suburb_Rosanna +Suburb_Ashburton +
                    CouncilArea_Darebin + Suburb_Melbourne +Suburb_Essendon +Suburb_Northcote+
                    CouncilArea_Whitehorse + CouncilArea_Bayside +  Method_PI + Suburb_Doncaster + 
                    Method_SP + Suburb_Thornbury + Suburb_Ormond + Suburb_Abbotsford + 
                    Suburb_Burwood +  CouncilArea_Stonnington +  Suburb_AscotVale  + Suburb_StKilda +
                    Suburb_Elwood +  Suburb_MooneePonds  +
                    Suburb_BrunswickEast + Suburb_Murrumbeena  + 
                    CouncilArea_GlenEira +Suburb_Carnegie  + Suburb_PortMelbourne  +          
                    Suburb_Brunswick + Suburb_Yarraville + Suburb_Footscray + CouncilArea_Manningham),data = house_train1)

round(sort((summary(fit)$coefficients)[,4]),3)

# Removing Suburb_Carnegie with vif  0.763
fit = lm(Price ~ (Type_h + Rooms+Bathroom + Suburb_Toorak + Suburb_Brighton +Suburb_Reservoir+ 
                    Suburb_Malvern +YearBuilt+Suburb_Armadale+BuildingArea +Suburb_Glenroy + CouncilArea_PortPhillip +
                    Suburb_Balwyn +Car + Suburb_Kew + Type_t + Suburb_KeilorEast+  
                    Suburb_KeilorEast + Suburb_Camberwell+Suburb_Preston+ Suburb_SouthYarra +Suburb_SunshineWest + 
                    Suburb_Hampton + Suburb_Fawkner + Suburb_SunshineNorth + Suburb_GlenIris + Suburb_Hadfield +    
                    CouncilArea_Brimbank + Suburb_AvondaleHeights + Suburb_AirportWest + Bedroom2 +
                    Suburb_BrightonEast + Suburb_AltonaNorth + Suburb_MalvernEast + CouncilArea_Yarra +  
                    CouncilArea_HobsonsBay + CouncilArea_Banyule +Suburb_OakleighSouth + Suburb_Williamstown +   
                    CouncilArea_Melbourne + Suburb_HawthornEast +Suburb_Hawthorn +Method_S +
                    CouncilArea_Maribyrnong +Suburb_TemplestoweLower+ Suburb_PascoeVale+
                    Suburb_Moorabbin+ Suburb_HeidelbergWest +Suburb_Prahran+Suburb_CoburgNorth+ 
                    CouncilArea_Boroondara + Suburb_BalwynNorth +Suburb_BentleighEast + Suburb_Ivanhoe +
                    Suburb_HeidelbergHeights +Suburb_Maidstone +  Suburb_Kensington+      
                    Suburb_Coburg + Suburb_Sunshine + CouncilArea_Moreland + Landsize + Suburb_WestFootscray +
                    Suburb_SurreyHills +  Suburb_Bulleen + CouncilArea_MooneeValley+ 
                    Suburb_Niddrie +Suburb_Strathmore+Suburb_Rosanna +Suburb_Ashburton +
                    CouncilArea_Darebin + Suburb_Melbourne +Suburb_Essendon +Suburb_Northcote+
                    CouncilArea_Whitehorse + CouncilArea_Bayside +  Method_PI + Suburb_Doncaster + 
                    Method_SP + Suburb_Thornbury + Suburb_Ormond + Suburb_Abbotsford + 
                    Suburb_Burwood +  CouncilArea_Stonnington +  Suburb_AscotVale  + Suburb_StKilda +
                    Suburb_Elwood +  Suburb_MooneePonds  +
                    Suburb_BrunswickEast + Suburb_Murrumbeena  + 
                    CouncilArea_GlenEira   + Suburb_PortMelbourne  +          
                    Suburb_Brunswick + Suburb_Yarraville + Suburb_Footscray + CouncilArea_Manningham),data = house_train1)
round(sort((summary(fit)$coefficients)[,4]),3)

# Removing  Suburb_Elwood  with vif  0.661 
fit = lm(Price ~ (Type_h + Rooms+Bathroom + Suburb_Toorak + Suburb_Brighton +Suburb_Reservoir+ 
                    Suburb_Malvern +YearBuilt+Suburb_Armadale+BuildingArea +Suburb_Glenroy + CouncilArea_PortPhillip +
                    Suburb_Balwyn +Car + Suburb_Kew + Type_t + Suburb_KeilorEast+  
                    Suburb_KeilorEast + Suburb_Camberwell+Suburb_Preston+ Suburb_SouthYarra +Suburb_SunshineWest + 
                    Suburb_Hampton + Suburb_Fawkner + Suburb_SunshineNorth + Suburb_GlenIris + Suburb_Hadfield +    
                    CouncilArea_Brimbank + Suburb_AvondaleHeights + Suburb_AirportWest + Bedroom2 +
                    Suburb_BrightonEast + Suburb_AltonaNorth + Suburb_MalvernEast + CouncilArea_Yarra +  
                    CouncilArea_HobsonsBay + CouncilArea_Banyule +Suburb_OakleighSouth + Suburb_Williamstown +   
                    CouncilArea_Melbourne + Suburb_HawthornEast +Suburb_Hawthorn +Method_S +
                    CouncilArea_Maribyrnong +Suburb_TemplestoweLower+ Suburb_PascoeVale+
                    Suburb_Moorabbin+ Suburb_HeidelbergWest +Suburb_Prahran+Suburb_CoburgNorth+ 
                    CouncilArea_Boroondara + Suburb_BalwynNorth +Suburb_BentleighEast + Suburb_Ivanhoe +
                    Suburb_HeidelbergHeights +Suburb_Maidstone +  Suburb_Kensington+      
                    Suburb_Coburg + Suburb_Sunshine + CouncilArea_Moreland + Landsize + Suburb_WestFootscray +
                    Suburb_SurreyHills +  Suburb_Bulleen + CouncilArea_MooneeValley+ 
                    Suburb_Niddrie +Suburb_Strathmore+Suburb_Rosanna +Suburb_Ashburton +
                    CouncilArea_Darebin + Suburb_Melbourne +Suburb_Essendon +Suburb_Northcote+
                    CouncilArea_Whitehorse + CouncilArea_Bayside +  Method_PI + Suburb_Doncaster + 
                    Method_SP + Suburb_Thornbury + Suburb_Ormond + Suburb_Abbotsford + 
                    Suburb_Burwood +  CouncilArea_Stonnington +  Suburb_AscotVale  + Suburb_StKilda +
                     Suburb_MooneePonds  +
                    Suburb_BrunswickEast + Suburb_Murrumbeena  + 
                    CouncilArea_GlenEira   + Suburb_PortMelbourne  +          
                    Suburb_Brunswick + Suburb_Yarraville + Suburb_Footscray + CouncilArea_Manningham),data = house_train1)
round(sort((summary(fit)$coefficients)[,4]),3)

# Removing CouncilArea_Manningham  with vif  0.548 
fit = lm(Price ~ (Type_h + Rooms+Bathroom + Suburb_Toorak + Suburb_Brighton +Suburb_Reservoir+ 
                    Suburb_Malvern +YearBuilt+Suburb_Armadale+BuildingArea +Suburb_Glenroy + CouncilArea_PortPhillip +
                    Suburb_Balwyn +Car + Suburb_Kew + Type_t + Suburb_KeilorEast+  
                    Suburb_KeilorEast + Suburb_Camberwell+Suburb_Preston+ Suburb_SouthYarra +Suburb_SunshineWest + 
                    Suburb_Hampton + Suburb_Fawkner + Suburb_SunshineNorth + Suburb_GlenIris + Suburb_Hadfield +    
                    CouncilArea_Brimbank + Suburb_AvondaleHeights + Suburb_AirportWest + Bedroom2 +
                    Suburb_BrightonEast + Suburb_AltonaNorth + Suburb_MalvernEast + CouncilArea_Yarra +  
                    CouncilArea_HobsonsBay + CouncilArea_Banyule +Suburb_OakleighSouth + Suburb_Williamstown +   
                    CouncilArea_Melbourne + Suburb_HawthornEast +Suburb_Hawthorn +Method_S +
                    CouncilArea_Maribyrnong +Suburb_TemplestoweLower+ Suburb_PascoeVale+
                    Suburb_Moorabbin+ Suburb_HeidelbergWest +Suburb_Prahran+Suburb_CoburgNorth+ 
                    CouncilArea_Boroondara + Suburb_BalwynNorth +Suburb_BentleighEast + Suburb_Ivanhoe +
                    Suburb_HeidelbergHeights +Suburb_Maidstone +  Suburb_Kensington+      
                    Suburb_Coburg + Suburb_Sunshine + CouncilArea_Moreland + Landsize + Suburb_WestFootscray +
                    Suburb_SurreyHills +  Suburb_Bulleen + CouncilArea_MooneeValley+ 
                    Suburb_Niddrie +Suburb_Strathmore+Suburb_Rosanna +Suburb_Ashburton +
                    CouncilArea_Darebin + Suburb_Melbourne +Suburb_Essendon +Suburb_Northcote+
                    CouncilArea_Whitehorse + CouncilArea_Bayside +  Method_PI + Suburb_Doncaster + 
                    Method_SP + Suburb_Thornbury + Suburb_Ormond + Suburb_Abbotsford + 
                    Suburb_Burwood +  CouncilArea_Stonnington +  Suburb_AscotVale  + Suburb_StKilda +
                    Suburb_MooneePonds  +
                    Suburb_BrunswickEast + Suburb_Murrumbeena  + 
                    CouncilArea_GlenEira   + Suburb_PortMelbourne  +          
                    Suburb_Brunswick + Suburb_Yarraville + Suburb_Footscray ),data = house_train1)
round(sort((summary(fit)$coefficients)[,4]),3)

# Removing Suburb_MooneePonds  with vif  0.517 
fit = lm(Price ~ (Type_h + Rooms+Bathroom + Suburb_Toorak + Suburb_Brighton +Suburb_Reservoir+ 
                    Suburb_Malvern +YearBuilt+Suburb_Armadale+BuildingArea +Suburb_Glenroy + CouncilArea_PortPhillip +
                    Suburb_Balwyn +Car + Suburb_Kew + Type_t + Suburb_KeilorEast+  
                    Suburb_KeilorEast + Suburb_Camberwell+Suburb_Preston+ Suburb_SouthYarra +Suburb_SunshineWest + 
                    Suburb_Hampton + Suburb_Fawkner + Suburb_SunshineNorth + Suburb_GlenIris + Suburb_Hadfield +    
                    CouncilArea_Brimbank + Suburb_AvondaleHeights + Suburb_AirportWest + Bedroom2 +
                    Suburb_BrightonEast + Suburb_AltonaNorth + Suburb_MalvernEast + CouncilArea_Yarra +  
                    CouncilArea_HobsonsBay + CouncilArea_Banyule +Suburb_OakleighSouth + Suburb_Williamstown +   
                    CouncilArea_Melbourne + Suburb_HawthornEast +Suburb_Hawthorn +Method_S +
                    CouncilArea_Maribyrnong +Suburb_TemplestoweLower+ Suburb_PascoeVale+
                    Suburb_Moorabbin+ Suburb_HeidelbergWest +Suburb_Prahran+Suburb_CoburgNorth+ 
                    CouncilArea_Boroondara + Suburb_BalwynNorth +Suburb_BentleighEast + Suburb_Ivanhoe +
                    Suburb_HeidelbergHeights +Suburb_Maidstone +  Suburb_Kensington+      
                    Suburb_Coburg + Suburb_Sunshine + CouncilArea_Moreland + Landsize + Suburb_WestFootscray +
                    Suburb_SurreyHills +  Suburb_Bulleen + CouncilArea_MooneeValley+ 
                    Suburb_Niddrie +Suburb_Strathmore+Suburb_Rosanna +Suburb_Ashburton +
                    CouncilArea_Darebin + Suburb_Melbourne +Suburb_Essendon +Suburb_Northcote+
                    CouncilArea_Whitehorse + CouncilArea_Bayside +  Method_PI + Suburb_Doncaster + 
                    Method_SP + Suburb_Thornbury + Suburb_Ormond + Suburb_Abbotsford + 
                    Suburb_Burwood +  CouncilArea_Stonnington +  Suburb_AscotVale  + Suburb_StKilda +
                    Suburb_BrunswickEast + Suburb_Murrumbeena  + CouncilArea_GlenEira   + Suburb_PortMelbourne  +          
                    Suburb_Brunswick + Suburb_Yarraville + Suburb_Footscray ),data = house_train1)
round(sort((summary(fit)$coefficients)[,4]),3)
summary(fit)


formula(fit)

val.pred <- predict(fit, newdata = house_train2)
val.errors <- house_train2$Price - val.pred

val.errors**2 %>% mean() %>% sqrt()

train.pred <- predict(fit, newdata = house_train1)
train_error <- house_train1$Price - train.pred


train_error**2 %>% mean() %>% sqrt()

Score =212467/RMSE

Score =212467/

fit.final=lm(Price ~ .,data=house_train)

sort(vif(fit.final), decreasing = T)[1:5]



sort(vif(fit.final), decreasing = T)[1:5]


fit.final=step(fit.final)

summary(fit.final)
formula(fit.final)

  

test.pred <- predict(fit.final, newdata = house_test)  
  
write.csv(test.pred, "submision1.csv", row.names = F)










