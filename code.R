
# The data is loaded. 

  data_s2 <- read.csv("~/Desktop/RD/data_s2.csv", stringsAsFactors = F)

######
# 1. Create a new data frame with the total population living in each Spanish province (50) in 2010.
#######

# The "dplyr" library is loaded to manipulate data.

  library("dplyr")

# It is filtered by year and grouped by province.

 df1 <- dplyr::filter(data_s2, YEAR=="2010") %>%
  select("PROVINCE", "POP_SPANISH", "POP_LATINAMERICA",
         "POP_WESTERNEUROPE", "POP_EASTERNEUROPE", "POP_AFRICA",
         "POP_ASIA", "POP_OTHERS") %>%
  group_by(PROVINCE) %>%
  summarise(across(everything(), sum))
  
 df1

#######
# 2. Produce a bar plot with the ranking of the ten more populated provinces in Spain in 2010.
#######

 df2 <- df1 %>%
  mutate(TOTAL = (POP_SPANISH + POP_LATINAMERICA +
                       POP_WESTERNEUROPE + POP_EASTERNEUROPE +
                       POP_AFRICA + POP_ASIA + POP_OTHERS)) %>%
  group_by(PROVINCE)  %>%
  slice(1)
  

 df2 <- df2[with(df2, order(-df2$TOTAL)), ]
 df2 <- head(df2, 10)
 df2

 library("ggplot2")

 ggplot(data=df2, aes(x=reorder(PROVINCE,-TOTAL), y=TOTAL)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=TOTAL), vjust=1.6, color="white", size=3.5) +
  ggtitle("Most populated provinces in Spain (2010)") + 
  xlab("Province") +
  ylab("Total population") +
  scale_y_continuous(labels = function(x) format(x, scientific = F)) +
  theme_minimal()


#######
# 3. Create a new dataframe for the municipality of Barcelona reflecting the evolution of all population groups available in data_2 between 2000 and 2016.
#######

 # The municipality Barcelona is filtered and then the graph is plotted. 
 
 df3 <- filter(data_s2, MUNICIPALITY=="Barcelona")


 ggplot(df3) +
  ggtitle("Barcelona (2000 - 2016)") + 
  xlab("Year") +
  ylab("Population") +
  geom_line(aes(x = as.character(YEAR), y = POP_LATINAMERICA, 
                group = 1, color="POP_LATINAMERICA")) +
  geom_line(aes(x = as.character(YEAR), y = POP_SPANISH, 
                group = 1, color="POP_SPANISH")) +
  geom_line(aes(x = as.character(YEAR), y = POP_WESTERNEUROPE, 
                group = 1, color="POP_WESTERNEUROPE")) +
  geom_line(aes(x = as.character(YEAR), y = POP_EASTERNEUROPE, 
                group = 1, color="POP_EASTERNEUROPE")) +
  geom_line(aes(x = as.character(YEAR), y = POP_AFRICA, 
                group = 1, color="POP_AFRICA")) +
  geom_line(aes(x = as.character(YEAR), y = POP_ASIA, 
                group = 1, color="POP_ASIA")) +
  geom_line(aes(x = as.character(YEAR), y = POP_OTHERS, 
              group = 1, color="POP_OTHERS")) +
   scale_y_continuous(labels = function(x) format(x, scientific = F)) +
  theme_minimal()

#######
# 4. Create a multigroup line plot with the evolution of the Latin-American and Western European population living in Barcelona between 2000 and 2016.
#######

# In this case we work with the whole time period and select POP_LATINAMERICA and POP_WESTERNEUROPE.
 
 df4 <- df3

 ggplot(df4) +
  ggtitle("Barcelona (2000 - 2016)") + 
  xlab("Year") +
  ylab("Population") +
  geom_line(aes(x = as.character(YEAR), y = POP_LATINAMERICA, 
                group = 1, color="POP_LATINAMERICA")) +

  geom_line(aes(x = as.character(YEAR), y = POP_WESTERNEUROPE, 
                group = 1, color="POP_WESTERNEUROPE")) +
  theme_minimal()


#######
# 5. Plot into a scatter plot columns LON (y-axis) and LAT (x-axis) for all the municipalities of Catalonia in any year you want.
#######

 
# The year and province are filtered, then the scatter plot is drawn. Each point represents a geographical coordinate.  
 
 df5 <- dplyr::filter(data_s2, YEAR=="2016") %>%
  filter(PROVINCE=="Barcelona")


  ggplot(df5, aes(x=LAT, y=LON)) +
    ggtitle("Catalonia (2016)") + 
    xlab("LAT") +
    ylab("LON") +
    geom_point(colour = "blue")

#######
# 6. Create a stacked-bar plot with the Spanish and the total foreign-born population living in the municipality of Madrid in 2000, 2005, 2010 and 2015.
#######
  
# The municipality is filtered, the numerical variables are selected and all the data for foreigners are added up. The table is sorted and the graph is plotted.   
  
  df6 <- filter(data_s2, MUNICIPALITY == "Madrid") %>%
  filter(YEAR %in% c("2000", "2005","2010","2015")) %>%
    select("PROVINCE", "POP_SPANISH", "POP_LATINAMERICA",
           "POP_WESTERNEUROPE", "POP_EASTERNEUROPE", "POP_AFRICA",
           "POP_ASIA", "POP_OTHERS","YEAR") %>%
    mutate(TOTAL_EXT = (POP_LATINAMERICA +
         POP_WESTERNEUROPE  +  POP_EASTERNEUROPE  +  POP_AFRICA  +
           POP_ASIA  +  POP_OTHERS))
  
  A <- select(df6, YEAR, PROVINCE, POP_SPANISH) %>%
    mutate(POP="POP_SPANISH") %>%
    rename(POPULATION = POP_SPANISH)
  
  
  B <- select(df6, YEAR, PROVINCE, TOTAL_EXT) %>%
    mutate(POP="TOTAL_EXT") %>%
    rename(POPULATION = TOTAL_EXT)
  
  df6 <- rbind(A, B)
  
  df6
  
  ggplot(df6, aes(x = YEAR, y = POPULATION, fill = POP)) +
    geom_col(position = "dodge") +
    ggtitle("Population in Madrid (2000, 2005, 2010 and 2015)") + 
    scale_y_continuous(labels = function(x) format(x, scientific = F)) +
    xlab("Year") +
    ylab("Population") 
  
  
#######
# 7. The mean proportion of foreign population living in the municipalities of Andalusia in 2015
#######

# The community is selected which contains all municipalities. The numerical variables of those born in Spain and foreigners are selected. Subsequently, the proportion is calculated for each municipality.   
  
  df7 <- filter(data_s2, COM == "Andalusia")  %>%
    filter(YEAR %in% c("2015")) %>%
    select("MUNICIPALITY", "POP_SPANISH", "POP_LATINAMERICA",
           "POP_WESTERNEUROPE", "POP_EASTERNEUROPE", "POP_AFRICA",
           "POP_ASIA", "POP_OTHERS","YEAR") %>%
    mutate(TOTAL_EXT = (POP_LATINAMERICA +
                          POP_WESTERNEUROPE  +  POP_EASTERNEUROPE  +  POP_AFRICA  +
                          POP_ASIA  +  POP_OTHERS)) %>%
    select("MUNICIPALITY", "POP_SPANISH", "TOTAL_EXT") %>%
    group_by(MUNICIPALITY) %>%
    summarise(across(everything(), sum)) %>%
    mutate(PROPORTION = (TOTAL_EXT*100)/(POP_SPANISH + TOTAL_EXT)) 
 
    df7
  

  
