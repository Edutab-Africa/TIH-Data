setwd("C:/Users/Edutab User/Desktop/Kenya TIH_Child Observations 28.10.2021")




library(ggplot2)
library(forcats)
library(tidyr)
library(mudata2)
library(tidyverse)
library(openxlsx)
library("xlsx")
library(dplyr)
library(readxl)
library(plotly)
library(leaflet)
library(scales)


TIH <- read_excel("Child observation_raw data_28.10.2021.xlsx",sheet = "ChildObservationData_28.10.2021")
Social_demographic <- read_excel("Social Demographic survey.xlsx",sheet = "Social_demographic")



#a <- as.data.frame(TIH$HH_ID)

people_present <- TIH %>% 
  filter(Country == "Ethiopia") %>%
  select(HH_ID,Person_No1_Relation,Person_No1_Gender,Person_No1_Age,Person_No1_Older,Person_No1_Comment,
         Person_No2_Relation,Person_No2_Gender,Person_No2_Age,Person_No2_Older,Person_No2_Comment,
         Person_No3_Relation,Person_No3_Gender,Person_No3_Age,Person_No3_Older,Person_No3_Comment,
         Person_No4_Relation,Person_No4_Gender,Person_No4_Age,Person_No4_Older,Person_No4_Comment,
         Person_No5_Relation,Person_No5_Gender,Person_No4_Age,Person_No5_Older,Person_No5_Comment,Person_No5_Age)

Relation <- people_present %>%
  select(HH_ID,Person_No1_Relation,Person_No2_Relation,Person_No3_Relation,
         Person_No4_Relation,Person_No5_Relation) %>%
  pivot_longer (!c(HH_ID), names_to = "Present", values_to = "Relation") %>%
  mutate(Present = case_when(Present == "Person_No1_Relation" ~ "Person_1",
                             Present == "Person_No2_Relation" ~ "Person_2",
                             Present == "Person_No3_Relation" ~ "Person_3",
                             Present == "Person_No4_Relation" ~ "Person_4",
                             Present == "Person_No5_Relation" ~ "Person_5")) %>%
  mutate(Relation = case_when(Relation == "Assistant teachers " ~ "Teacher",
                              Relation == "brother " ~ "Sibling",
                              Relation == "Brother" ~ "Sibling",
                              Relation == "Father" ~ "Parent",
                              Relation == "Main teachers" ~ "Teacher",
                              Relation == "Mother" ~ "Parent",
                              Relation == "neighbor" ~ "Neighbor",
                              Relation == "Neighbouring child" ~ "Neighbor",
                              Relation == "sister" ~ "Sibling",
                              Relation == "Sister" ~ "Sibling"))

Age <- people_present %>%
  select(HH_ID,Person_No1_Age,Person_No2_Age,Person_No3_Age,
         Person_No4_Age,Person_No5_Age) %>%
  pivot_longer (!c(HH_ID), names_to = "Present", values_to = "Age")%>%
  mutate(Present = case_when(Present == "Person_No1_Age" ~ "Person_1",
                             Present == "Person_No2_Age" ~ "Person_2",
                             Present == "Person_No3_Age" ~ "Person_3",
                             Present == "Person_No4_Age" ~ "Person_4",
                             Present == "Person_No5_Age" ~ "Person_5"))


Gender <- people_present %>%
  select(HH_ID,Person_No1_Gender,Person_No2_Gender,Person_No3_Gender,
         Person_No4_Gender,Person_No5_Gender) %>%
  pivot_longer (!c(HH_ID), names_to = "Present", values_to = "Gender")%>%
  mutate(Present = case_when(Present == "Person_No1_Gender" ~ "Person_1",
                             Present == "Person_No2_Gender" ~ "Person_2",
                             Present == "Person_No3_Gender" ~ "Person_3",
                             Present == "Person_No4_Gender" ~ "Person_4",
                             Present == "Person_No5_Gender" ~ "Person_5"))%>%
  mutate(Present = case_when(Present == "Person_No1_Older" ~ "Person_1",
                             Present == "Person_No2_Older" ~ "Person_2",
                             Present == "Person_No3_Older" ~ "Person_3",
                             Present == "Persteacherson_No4_Older" ~ "Person_4",
                             Present == "Person_No5_Older" ~ "Person_5"))
summarise(n = n())


Older <- people_present %>%
  select(HH_ID,Person_No1_Older,Person_No2_Older,Person_No3_Older,
         Person_No4_Older,Person_No5_Older) %>%
  pivot_longer (!c(HH_ID), names_to = "Present", values_to = "Older")%>%
  mutate(Present = case_when(Present == "Person_No1_Older" ~ "Person_1",
                             Present == "Person_No2_Older" ~ "Person_2",
                             Present == "Person_No3_Older" ~ "Person_3",
                             Present == "Persteacherson_No4_Older" ~ "Person_4",
                             Present == "Person_No5_Older" ~ "Person_5"))


a <- merge(Relation,Age, by = c("HH_ID", "Present"))

a <- merge(a,Gender, by = c("HH_ID", "Present"))

a <- merge(a,Older, by = c("HH_ID", "Present"))

write.csv(a,"a.csv")



#present_ethiopia <- merge(Relation,Age, by =c( "HH_ID", "Present"))

###########################################################################

Ethiopia <- Social_demographic %>% 
  filter(Country == "Ethiopia") %>%
  rename(Country=Country,
  Date="Date",
  HOUSEHOLD_ID ="HOUSEHOLD ID",
  NAME_DATA_COLLECTOR ="NAME OF DATA COLLECTOR",
  HOUSEHOld_HEAD_ID ="HOUSEHOLD HEAD ID",
  No_People_Household ="TOTAL NUMBER OF PEOPLE IN A HOUSEHOLD",
  Language_Quest ="LANGUAGE OF QUESTIONNAIRE:",
  Langauge_Inter="LANGUAGE OF INTERVIEW:",
  Language_Home ="LANGUAGE USED AT HOME",
  ADULT_No ="ADULT No",
  Household_relation="HOUSEHOLD RELATIONSHIP",
  GENDER ="GENDER",
  RESIDENCE="RESIDENCE",
  AGE_BRACKET="AGE BRACKET",
  OCCUPATION="OCCUPATION",
  Adult_number ="Adult number",
  Relation_Pair ="Relationship with the pair",
  No_Children ="Number of Children staying in the house",
  Child_No1 ="Child number1",
  AGE1 ="Child's age...20",
  Gender1 ="Male/Female...21",
  Attend_School1 ="Is [child name] attending school?...22",
  Year_Join_School1 ="When did the child join school?...23",
  Grade1="What is the child's current grade?...24",
  Child_No2 ="Child number2",
  AGE2="Child's age...26",
  Gender2 ="Male/Female...27",
  Attend_School2="Is [child name] attending school?...28",
  Year_Join_School ="When did the child join school?...29",
  Grade2 ="What is the child's current grade?...30",
  Child_No3 ="Child number3",
  AGE3 ="Child's age...32",
  Gender3="Male/Female...33",
  Attend_School3="Is [child name] attending school?...34",
  Year_Join_School3="When did the child join school?...35",
  Grade3 ="What is the child's current grade?...36",
  Child_No4= "Child number4",
  AGE4 ="Child's age...38",
  Gender4="Male/Female...39",
  Attend_School4="Is [child name] attending school?...40",
  Year_Join_School4 ="When did the child join school?...41",
  Grade4 ="What is the child's current grade?...42",
  Source_drinkwater="What is the main source of drinking water for the members of your household",
  main_source_water ="What is the main source of water used by your household for other purposes such as cooking?",
  Location_water="Where is the location of the water source?" ,
  Time_water="How much time does it usually take you to bring water to the house?",
  Toilet_facility ="What kind of toilet facility do members of your households usually use?" ,
  Type_Fuel="What type of fuel does your household mainly use for cooking?",
  Sleep_sameroom ="Do your children sleep in the same room?",                                                  
  Livestock =  "Does this household own any livestock, herds, other farm animals, or poultry?",             
  Livestock_type = "Which livestock, herds, other farm animals, or poultry does this household have?",          
  Agricultural_land = "Does any member of this household own any agricultural land?",                              
  Shamba = "Do you own any piece of land (shamba)?",                                                  
  shamba_size = "How big is your land (shamba)?",                                                            
  Household_own = "Does your household have:",                                                               
  Electricity = "Electricity" ,                                                                               
  radio = "radio",                                                                                     
  television = "television",
  Telephone="smart or ordinary telephone",
  computer="computer",
  refrigerator="refrigerator",
  Member_own="Does any member of this household own:",
  bicycle = "bicycle",
  Motorbike="motorcycle or motor scooter",
  Cart="An animal-drawn cart",
  Vehicle="A car, tractor, truck",
  Floors_material ="Observe the materials used on the floors of the dwelling?",
  Plastered="Plastered",
  Tiled="Tiled",
  Carpeted ="Carpeted",
  Dung="Dung",
  Other_mater="Other...72",
  Roof_Materials="Observe main material of the roof of the dwelling",
  Thatched="Thatched",
  Iron_sheets="Iron sheets",
  Cement_floor="Cement...76",
  Rustic_mat="Rustic mat",
  Other="Other...78",
  Wall_Material="Observe the main material of the exterior walls of the dwelling.",
  Dirt="Dirt",
  Plywood="Plywood",
  Cardboard ="Cardboard",
  Reused_wood="Reused wood",
  Cement="Cement...84",
  Bricks="Bricks",
  Wood_planks="Wood planks",
  Other_wall="Other...87",
  Comments="ANY OTHER COMMENTS:") 

Householdhead=Ethiopia%>%
  select(HOUSEHOld_HEAD_ID, Household_relation,GENDER,RESIDENCE,AGE_BRACKET,OCCUPATION)%>%
  
  






names(Ethiopia)

###########################################################################################################################

#Parents survey

library(ggplot2)
library(forcats)
library(tidyr)
library(mudata2)
library(tidyverse)
library(openxlsx)
library("xlsx")
library(dplyr)
library(readxl)
library(plotly)
library(leaflet)
library(scales)

Parents_Assessment <- read_excel("Parents Interview.xlsx",sheet = "Sheet1")


Children_household <- Parents_Assessment %>% 
  filter(Country == "Ethiopia") %>%
  select(Country,"Household ID","Child number1",Age1,Gender1,Comments1,"Child number2",
         Age2,Gender2,Comments2,"Child number3",Age3,Gender3,Comments3,"Child number4",
         Age4,Gender4,Comments4) 

child1 <- Children_household %>%
  select ("Household ID",Age1,Gender1,Comments1) %>%
  rename(HH_ID ="Household ID",
         Age ="Age1",
         Gender ="Gender1",
         Comment ="Comments1")



child2 <- Children_household %>%
  select ("Household ID",Age2,Gender2,Comments2) %>%
  rename(HH_ID ="Household ID",
         Age ="Age2",
         Gender ="Gender2",
         Comment ="Comments2")



child3 <- Children_household %>%
  select ("Household ID",Age3,Gender3,Comments3) %>%
  rename(HH_ID ="Household ID",
         Age ="Age3",
         Gender ="Gender3",
         Comment ="Comments3")



child4 <- Children_household %>%
  select ("Household ID",Age4,Gender4,Comments4) %>%
  rename(HH_ID ="Household ID",
         Age ="Age4",
         Gender ="Gender4",
         Comment ="Comments4")


Children_household <-rbind  ( child1, child2, child3, child4)


Children_household %>%
  group_by(Gender) %>%
  summarise(gender_count=n())

################################################################################################
#jointactivities



Joint_Activities <- Parents_Assessment %>% 
  filter(Country == "Ethiopia") %>%
  select("Household ID","Time_Joint activities","Joint activity...20","Place of joint activity...21",
         "Day...22","Time...23","Joint activity...24","Place of joint activity...25","Day...26",
         "Time...27","Joint activity...28","Place of joint activity...29","Day...30","Time...31",
         "Joint activity...32","Place of joint activity...33","Time...34","Joint activity...35",
         "Place of joint activity...36","Time...37","Joint activity...38","Place of joint activity...39",
         "Time...40","Joint activity...41","Place of joint activity...42")

Joint_Activity1 <- Joint_Activities %>%
  select("Household ID","Time_Joint activities","Joint activity...20","Place of joint activity...21")%>%
  rename(HH_ID="Household ID",
         Time ="Time_Joint activities",
         Activity="Joint activity...20",
         Place="Place of joint activity...21")%>%
  mutate(Day=c("all days","all days","all days","weekdays","weekdays","all days"),
         Time=c("morning","evening","whole day","morning","morning","evening"),
         activity="1")%>%
  select(HH_ID,activity,Day,Time,Place,Activity)
  


Joint_Activity2 <- Joint_Activities %>%
  select("Household ID","Day...22","Time...23","Joint activity...24","Place of joint activity...25")%>%
  rename(HH_ID="Household ID",
         Day="Day...22",
         Time="Time...23",
         Activity="Joint activity...24",
         Place="Place of joint activity...25")%>%
  mutate(Day=c("all days","all days","all days","weekdays","weekdays","all days"),
         Time=c("Afternoon","evening","evening","evening","Afternoon","whole day"),
         activity="2")%>%
  select(HH_ID,activity,Day,Time,Place,Activity)



Joint_Activity3 <- Joint_Activities %>%
  select("Household ID" ,"Day...26","Time...27","Joint activity...28","Place of joint activity...29")%>%
  rename(HH_ID="Household ID" ,
         Day="Day...26",
         Time="Time...27",
         Activity="Joint activity...28",
         Place="Place of joint activity...29")%>%
  mutate(Day=c("all days","all days","weekdays","Weekends","Weekends","weekdays"),
         Time=c("Afternoon","evening","whole day","morning","morning","whole day"),
         activity="3")%>%
  select(HH_ID,activity,Day,Time,Place,Activity)


Joint_Activity4 <- Joint_Activities %>%
  select("Household ID","Day...30","Time...31","Joint activity...32","Place of joint activity...33")%>%
  rename(HH_ID="Household ID",
         Day="Day...30",
         Time="Time...31",
         Activity="Joint activity...32",
         Place="Place of joint activity...33")%>%
  mutate(Day=c("all days","Weekends","all days","Weekends","Weekends","all days"),
         Time=c("Afternoon","Afternoon","evening","Afternoon","Afternoon","evening"),
         activity="4")%>%
  select(HH_ID,activity,Day,Time,Place,Activity)




Joint_Activity5 <- Joint_Activities %>%
  select("Household ID","Time...34","Joint activity...35","Place of joint activity...36")%>%
  rename(HH_ID="Household ID",
         Time="Time...34",
         Activity="Joint activity...35",
         Place="Place of joint activity...36")%>%
  mutate(Day=c("all days","","","","all days","weekends"),
         Time=c("Afternoon","","","","evening","afternoon"),
         activity="5")%>%
  select(HH_ID,activity,Day,Time,Place,Activity)


Joint_Activity6 <- Joint_Activities %>%
  select("Household ID","Time...37","Joint activity...38","Place of joint activity...39")%>%
  rename(HH_ID="Household ID",
         Time="Time...37",
         Activity="Joint activity...38",
         Place="Place of joint activity...39")%>%
  mutate(Day=c("all days","","","","",""),
         Time=c("evening","","","","",""),
         activity="6")%>%
  select(HH_ID,activity,Day,Time,Place,Activity)

  
Joint_Activity7 <- Joint_Activities %>%
  select("Household ID","Time...40","Joint activity...41","Place of joint activity...42")%>%
  rename(HH_ID="Household ID",
         Time="Time...40",
         Activity="Joint activity...41",
         Place="Place of joint activity...42")%>%
  mutate(Day=c("all days","","","","",""),
         Time=c("evening","","","","",""),
         activity="7") %>%
  select(HH_ID,activity,Day,Time,Place,Activity)


 Joint_Activities<-rbind(Joint_Activity1,Joint_Activity2,Joint_Activity3,
                         Joint_Activity4,Joint_Activity5,Joint_Activity6,Joint_Activity7)
 
 
 Joint_Activities <- Joint_Activities %>%
   mutate(Day = case_when(Day == "all days" ~ "All_day",
                          Day == "weekdays" ~ "Weekdays",
                          Day == "weekends" ~ "Weekends",
                          Day == "Weekends" ~ "Weekends"))
 
 
 
 Joint_Activities %>%
   group_by(Day) %>%
   summarise(Day_count=n())
 
 Joint_Activities %>%
   group_by(Place) %>%
   summarise(Place_count=n())
   
   
   write.csv(Joint_Activities,"Joint_Activities.csv")

 
 ##############################################################################################################################################
 
 monday_morning <- Parents_Assessment %>%
   select("Household ID","Time...44","Location child 1...45","Activity child 1...46",
          "Location child 2...47","Activity child 2...48","Location child 3...49",
          "Activity child 3...50","Comments...51")%>%
   rename(HH_ID ="Household ID",Time="Time...44",Locationch1="Location child 1...45",Activitych1="Activity child 1...46",
          Locationch2= "Location child 2...47",Activitych2="Activity child 2...48",Locationch3="Location child 3...49",
          Activitych3=  "Activity child 3...50",Comment="Comments...51")
 

monday_midday <-Parents_Assessment %>%
   select ("Household ID","Time...52","Location child 1...53",
          "Activity child 1...54","Location child 2...55","Activity child 2...56",
          "Location child 3...57","Activity child 3...58","Comments...59")%>%
   rename(HH_ID ="Household ID",Time="Time...52",Locationch1="Location child 1...53",
          Activitych1= "Activity child 1...54",Locationch2="Location child 2...55",Activitych2="Activity child 2...56",
          Locationch3= "Location child 3...57",Activitych3="Activity child 3...58",Comment="Comments...59")
          
          
monday_afternoon <-Parents_Assessment %>%
  select("Household ID","Time...60",
          "Location child 1...61","Activity child 1...62","Location child 2...63",
          "Activity child 2...64","Location child 3...65","Activity child 3...66",
          "Comments...67")%>%
  rename(HH_ID ="Household ID",Time="Time...60",Locationch1= "Location child 1...61",Activitych1="Activity child 1...62",Locationch2="Location child 2...63",
         Activitych2=  "Activity child 2...64",Locationch3="Location child 3...65",Activitych3="Activity child 3...66",
         Comment=  "Comments...67")
          
          
monday_evening <-Parents_Assessment %>%
  select("Household ID","Time...68","Location child 1...69","Activity child 1...70",
          "Location child 2...71","Activity child 2...72","Location child 3...73",
          "Activity child 3...74","Comments...75" )%>%
  rename(HH_ID ="Household ID",Time="Time...68",Locationch1="Location child 1...69",Activitych1="Activity child 1...70",
         Locationch2=  "Location child 2...71",Activitych2="Activity child 2...72",Locationch3="Location child 3...73",
         Activitych3= "Activity child 3...74",Comment="Comments...75")
  
   
monday <- rbind(monday_morning,monday_midday, monday_afternoon, monday_evening)
monday <- monday %>% 
  mutate(Day = "Monday")

##############################################################################################################################################
 
Tuesday_morning <- Parents_Assessment %>%
   select("Household ID","Time...77","Location child 1...78",
          "Activity child 1...79","Location child 2...80","Activity child 2...81",
          "Location child 3...82","Activity child 3...83","Comments...84")  %>%
  rename(HH_ID ="Household ID",Time="Time...77",Locationch1="Location child 1...78",Activitych1="Activity child 1...79",
         Locationch2= "Location child 2...80",Activitych2="Activity child 2...81",Locationch3="Location child 3...82",
         Activitych3=  "Activity child 3...83",Comment="Comments...84")

Tuesday_midday <- Parents_Assessment %>%
  select("Household ID","Time...85",
          "Location child 1...86","Activity child 1...87","Location child 2...88",
          "Activity child 2...89","Location child 3...90","Activity child 3...91","Comments...92") %>%
  rename(HH_ID ="Household ID",Time="Time...85",Locationch1="Location child 1...86",
         Activitych1= "Activity child 1...87",Locationch2="Location child 2...88",Activitych2="Activity child 2...89",
         Locationch3= "Location child 3...90",Activitych3="Activity child 3...91",Comment="Comments...92")

Tuesday_afternoon <- Parents_Assessment %>%
  select("Household ID","Time...93","Location child 1...94","Activity child 1...95","Location child 2...96",
          "Activity child 2...97","Location child 3...98","Activity child 3...99","Comments...100") %>%
  rename(HH_ID ="Household ID",Time="Time...93",
         Locationch1= "Location child 1...94",Activitych1="Activity child 1...95",Locationch2="Location child 2...96",
         Activitych2=  "Activity child 2...97",Locationch3="Location child 3...98",Activitych3="Activity child 3...99",
         Comment=  "Comments...100")
          
Tuesday_evening <-Parents_Assessment %>%
  select("Household ID","Time...101","Location child 1...102","Activity child 1...103","Location child 2...104",
          "Activity child 2...105","Location child 3...106","Activity child 3...107","Comments...108") %>%
  rename(HH_ID ="Household ID",Time="Time...101",Locationch1="Location child 1...102",Activitych1="Activity child 1...103",
         Locationch2=  "Location child 2...104",Activitych2="Activity child 2...105",Locationch3="Location child 3...106",
         Activitych3= "Activity child 3...107",Comment="Comments...108")

Tuesday<- rbind(Tuesday_morning,Tuesday_midday, Tuesday_afternoon, Tuesday_evening)
Tuesday <- Tuesday %>% 
  mutate(Day = "Tuesday")

#########################################################################################################################################
Wednesday_morning <- Parents_Assessment %>%
   select("Household ID","Time...110","Location child 1...111","Activity child 1...112","Location child 2...113",
          "Activity child 2...114","Location child 3...115","Activity child 3...116","Comments...117") %>%
  rename(HH_ID ="Household ID",Time="Time...110",Locationch1="Location child 1...111",Activitych1="Activity child 1...112",
         Locationch2= "Location child 2...113",Activitych2="Activity child 2...114",Locationch3="Location child 3...115",
         Activitych3=  "Activity child 3...116",Comment="Comments...117")
          
Wednesday_midday <- Parents_Assessment %>%
            select("Household ID","Time...118","Location child 1...119","Activity child 1...120","Location child 2...121",
          "Activity child 2...122","Location child 3...123","Activity child 3...124","Comments...125") %>%
  rename(HH_ID ="Household ID",Time="Time...118",Locationch1="Location child 1...119",
         Activitych1= "Activity child 1...120",Locationch2="Location child 2...121",Activitych2="Activity child 2...122",
         Locationch3= "Location child 3...123",Activitych3="Activity child 3...124",Comment="Comments...125")
          
Wednesday_afternoon <- Parents_Assessment %>%
  select("Household ID","Time...126","Location child 1...127","Activity child 1...128","Location child 2...129",
          "Activity child 2...130","Location child 3...131","Activity child 3...132","Comments...133") %>%
  rename(HH_ID ="Household ID",Time="Time...126",Locationch1= "Location child 1...127",Activitych1="Activity child 1...128",Locationch2="Location child 2...129",
         Activitych2=  "Activity child 2...130",Locationch3="Location child 3...131",Activitych3="Activity child 3...132",
         Comment=  "Comments...133")
          
Wednesday_evening <- Parents_Assessment %>%
  select("Household ID","Time...134","Location child 1...135","Activity child 1...136","Location child 2...137",
          "Activity child 2...138","Location child 3...139","Activity child 3...140","Comments...141") %>%
  rename(HH_ID ="Household ID",Time="Time...134",Locationch1="Location child 1...135",Activitych1="Activity child 1...136",
         Locationch2=  "Location child 2...137",Activitych2="Activity child 2...138",Locationch3="Location child 3...139",
         Activitych3= "Activity child 3...140",Comment="Comments...141")

Wednesday<- rbind(Wednesday_morning,Wednesday_midday, Wednesday_afternoon, Wednesday_evening)
Wednesday <- Wednesday %>% 
  mutate(Day = "Wednesday")

######################################################################################################################################################################################         
Thursday_morning<- Parents_Assessment %>%
   select("Household ID","Time...143","Location child 1...144","Activity child 1...145","Location child 2...146",
                     "Activity child 2...147","Location child 3...148","Activity child 3...149","Comments...150") %>%
  rename(HH_ID ="Household ID",Time="Time...143",Locationch1="Location child 1...144",Activitych1="Activity child 1...145",
         Locationch2= "Location child 2...146",Activitych2="Activity child 2...147",Locationch3="Location child 3...148",
         Activitych3=  "Activity child 3...149",Comment="Comments...150")
           
Thursday_midday <- Parents_Assessment %>%
  select("Household ID","Time...151",
                     "Location child 1...152","Activity child 1...153","Location child 2...154","Activity child 2...155",
                     "Location child 3...156","Activity child 3...157","Comments...158") %>%
  rename(HH_ID ="Household ID",Time="Time...151",Locationch1="Location child 1...152",
         Activitych1= "Activity child 1...153",Locationch2="Location child 2...154",Activitych2="Activity child 2...155",
         Locationch3= "Location child 3...156",Activitych3="Activity child 3...157",Comment="Comments...158")
          
Thursday_afternoon <- Parents_Assessment %>%
  select("Household ID","Time...159","Location child 1...160",
                     "Activity child 1...161","Location child 2...162","Activity child 2...163","Location child 3...164",
                     "Activity child 3...165","Comments...166") %>%
  rename(HH_ID ="Household ID",Time="Time...159", Locationch1= "Location child 1...160",Activitych1="Activity child 1...161",
         Locationch2="Location child 2...162",Activitych2=  "Activity child 2...163",Locationch3="Location child 3...164",
         Activitych3="Activity child 3...165",Comment=  "Comments...166")
          
Thursday_evening <- Parents_Assessment %>%
  select("Household ID","Time...167","Location child 1...168","Activity child 1...169",
                     "Location child 2...170","Activity child 2...171","Location child 3...172","Activity child 3...173",
                     "Comments...174") %>%
  rename(HH_ID ="Household ID",Time="Time...167",Locationch1="Location child 1...168",Activitych1="Activity child 1...169",
         Locationch2=  "Location child 2...170",Activitych2="Activity child 2...171",Locationch3="Location child 3...172",
         Activitych3= "Activity child 3...173",Comment="Comments...174")

Thursday<- rbind(Thursday_morning,Thursday_midday, Thursday_afternoon, Thursday_evening)
Thursday <- Thursday %>% 
  mutate(Day = "Thursday")


#########################################################################################################################################
Friday_morning <- Parents_Assessment %>%
   select("Household ID","Time...176","Location child 1...177","Activity child 1...178",
          "Location child 2...179","Activity child 2...180","Location child 3...181","Activity child 3...182",
          "Comments...183") %>%
  rename(HH_ID ="Household ID",Time="Time...176",Locationch1="Location child 1...177",Activitych1="Activity child 1...178",
         Locationch2= "Location child 2...179",Activitych2="Activity child 2...180",Locationch3="Location child 3...181",
         Activitych3=  "Activity child 3...182",Comment="Comments...183")
          
Friday_midday <- Parents_Assessment %>%
  select("Household ID","Time...184","Location child 1...185","Activity child 1...186","Location child 2...187",
          "Activity child 2...188","Location child 3...189","Activity child 3...190","Comments...191") %>%
  rename(HH_ID ="Household ID",Time="Time...184",Locationch1="Location child 1...185",
         Activitych1= "Activity child 1...186",Locationch2="Location child 2...187",Activitych2="Activity child 2...188",
         Locationch3= "Location child 3...189",Activitych3="Activity child 3...190",Comment="Comments...191")
          
Friday_afternoon <- Parents_Assessment %>%
  select("Household ID","afternoon","Location child 1...193","Activity child 1...194","Location child 2...195","Activity child 2...196",
          "Location child 3...197","Activity child 3...198","Comments...199") %>%
  rename(HH_ID ="Household ID", Time="afternoon", Locationch1= "Location child 1...193",Activitych1="Activity child 1...194",
         Locationch2="Location child 2...195",
         Activitych2=  "Activity child 2...196",Locationch3="Location child 3...197",Activitych3="Activity child 3...198",
         Comment=  "Comments...199")
          
Friday_evening <- Parents_Assessment %>%
  select("Household ID","Time...200","Location child 1...201",
          "Activity child 1...202","Location child 2...203","Activity child 2...204","Location child 3...205",
          "Activity child 3...206","Comments...207") %>%
  rename(HH_ID ="Household ID",Time="Time...200",Locationch1="Location child 1...201",Activitych1="Activity child 1...202",
         Locationch2=  "Location child 2...203",Activitych2="Activity child 2...204",Locationch3="Location child 3...205",
         Activitych3= "Activity child 3...206",Comment="Comments...207")

Friday<- rbind(Friday_morning,Friday_midday, Friday_afternoon, Friday_evening)
Friday <- Friday %>% 
  mutate(Day = "Friday")

###################################################################################################################################################################
Saturday_morning <- Parents_Assessment %>%
   select("Household ID","Time...209","Location child 1...210","Activity child 1...211",
          "Location child 2...212","Activity child 2...213","Location child 3...214",
          "Activity child 3...215","Comments...216") %>%
  rename(HH_ID ="Household ID",Time="Time...209",Locationch1="Location child 1...210",Activitych1="Activity child 1...211",
         Locationch2= "Location child 2...212",Activitych2="Activity child 2...213",Locationch3="Location child 3...214",
         Activitych3=  "Activity child 3...215",Comment="Comments...216")
          
Saturday_midday <- Parents_Assessment %>%
  select("Household ID","Time...217","Location child 1...218","Activity child 1...219",
         "Location child 2...220","Activity child 2...221","Location child 3...222",
         "Activity child 3...223","Comments...224") %>%
  rename(HH_ID ="Household ID",Time="Time...217",Locationch1="Location child 1...218",
         Activitych1= "Activity child 1...219",Locationch2="Location child 2...220",Activitych2="Activity child 2...221",
         Locationch3= "Location child 3...222",Activitych3="Activity child 3...223",Comment="Comments...224")
          
Saturday_afternoon <- Parents_Assessment %>%
  select("Household ID","Time...225","Location child 1...226","Activity child 1...227",
         "Location child 2...228", "Activity child 2...229","Location child 3...230",
         "Activity child 3...231","Comments...232") %>%
  rename(HH_ID ="Household ID",Time="Time...225",
         Locationch1= "Location child 1...226",Activitych1="Activity child 1...227",Locationch2="Location child 2...228",
         Activitych2=  "Activity child 2...229",Locationch3="Location child 3...230",Activitych3="Activity child 3...231",
         Comment=  "Comments...232")
          
Saturday_evening <- Parents_Assessment %>%
  select("Household ID","Time...233","Location child 1...234","Activity child 1...235",
         "Location child 2...236","Activity child 2...237","Location child 3...238",
         "Activity child 3...239","Comments...240") %>%
  rename(HH_ID ="Household ID",Time="Time...233",Locationch1="Location child 1...234",Activitych1="Activity child 1...235",
         Locationch2=  "Location child 2...236",Activitych2="Activity child 2...237",Locationch3="Location child 3...238",
         Activitych3= "Activity child 3...239",Comment="Comments...240")

Saturday <- rbind(Saturday_morning,Saturday_midday, Saturday_afternoon, Saturday_evening)
Saturday <- Saturday %>% 
  mutate(Day = "Saturday")
#####################################################################################################################################################         
Sunday_morning <- Parents_Assessment %>%
   select("Household ID","Time...242","Location child 1...243",
          "Activity child 1...244","Location child 2...245","Activity child 2...246","Location child 3...247",
          "Activity child 3...248","Comments...249") %>%
  rename(HH_ID ="Household ID",Time="Time...242",Locationch1="Location child 1...243",Activitych1="Activity child 1...244",
         Locationch2= "Location child 2...245",Activitych2="Activity child 2...246",Locationch3="Location child 3...247",
         Activitych3=  "Activity child 3...248",Comment="Comments...249")
          
Sunday_midday <- Parents_Assessment %>%
  select("Household ID","Time...250","Location child 1...251","Activity child 1...252",
          "Location child 2...253","Activity child 2...254","Location child 3...255","Activity child 3...256",
          "Comments...257") %>%
  rename(HH_ID ="Household ID",Time="Time...250",Locationch1="Location child 1...251",
         Activitych1= "Activity child 1...252",Locationch2="Location child 2...253",Activitych2="Activity child 2...254",
         Locationch3= "Location child 3...255",Activitych3="Activity child 3...256",Comment="Comments...257")
          
Sunday_afternoon <- Parents_Assessment %>%
  select("Household ID","Time...258","Location child 1...259","Activity child 1...260","Location child 2...261",
          "Activity child 2...262","Location child 3...263","Activity child 3...264","Comments...265") %>%
  rename(HH_ID ="Household ID",Time="Time...258",Locationch1= "Location child 1...259",Activitych1="Activity child 1...260",Locationch2="Location child 2...261",
         Activitych2=  "Activity child 2...262",Locationch3="Location child 3...263",Activitych3="Activity child 3...264",
         Comment=  "Comments...265")
          
Sunday_evening <- Parents_Assessment %>%
  select("Household ID","Time...266","Location child 1...267","Activity child 1...268","Location child 2...269","Activity child 2...270",
          "Location child 3...271","Activity child 3...272","Comments...273") %>%
  rename(HH_ID ="Household ID",Time="Time...266",Locationch1="Location child 1...267",Activitych1="Activity child 1...268",
         Locationch2=  "Location child 2...269",Activitych2="Activity child 2...270",Locationch3="Location child 3...271",
         Activitych3= "Activity child 3...272",Comment="Comments...273")

Sunday <- rbind(Sunday_morning,Sunday_midday, Sunday_afternoon, Sunday_evening)
Sunday <- Sunday %>% 
  mutate(Day = "Sunday")
#######################################################################################################################
Activity_Child<-rbind(monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday)
 
write.csv(Activity_Child,"Activity_Child.csv")
 
#################################################################################################################
 #timespenttogether
 
 Time_Spent_Together <- Parents_Assessment %>% 
   filter(Country == "Ethiopia") %>%
   select("Household ID","At what times are your children typically together on a weekday/weekend/holiday?",
     "What do they usually do when they are together (work, play, eat, sleep...?) You can also ask an older child who may be having the answer.",
     "Is one of your children typically in charge of the other(s)? If yes, at what days/times?") %>%
   rename(HH_ID="Household ID",
          Time="At what times are your children typically together on a weekday/weekend/holiday?",
          Activity="What do they usually do when they are together (work, play, eat, sleep...?) You can also ask an older child who may be having the answer.",
          Child_incharge="Is one of your children typically in charge of the other(s)? If yes, at what days/times?")
 
 write.csv(Time_Spent_Together,"Time_Spent_Together.csv")
 
 #################################################################################
 
 #Schooldays
 
 School_days <- Parents_Assessment %>% 
   filter(Country == "Ethiopia") %>%
   select("Household ID","Do both children go to school together in the morning?",
          "At what time does each of them arrive home from school?",
          "When do the children eat?",
          "Does everyone eat at the same time and in the same place?",
          "Which of the children are usually together while doing homework?",
          "Do they help each other with their homework?",
          "What chores do the children regularly do around the house?",
          "Are these chores joint activities (one child is holding the cow while the other one is milking, carrying things together) or individual ones (one child is sweeping the house alone)?",
         "How much time do the children spend playing together per day?",
         "Where do they typically play?")%>%
   rename(HH_ID="Household ID",School_together="Do both children go to school together in the morning?",
          Time_arrival="At what time does each of them arrive home from school?",
          Time_eat="When do the children eat?",
          Eat_same_time="Does everyone eat at the same time and in the same place?",
          child_together_homework="Which of the children are usually together while doing homework?",
          help_homework="Do they help each other with their homework?",
          chores="What chores do the children regularly do around the house?",
          joint_individual_activity="Are these chores joint activities (one child is holding the cow while the other one is milking, carrying things together) or individual ones (one child is sweeping the house alone)?",
          time_spent="How much time do the children spend playing together per day?",
          place_play="Where do they typically play?")
 
 write.csv(School_days,"School_days.csv")


  




















  
  
  








