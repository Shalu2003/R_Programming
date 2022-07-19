library(tidyverse)
Khalsa_college=
  read_csv("C:\\Users\\student.GNIC\\Downloads\\college.csv")

Khalsa_college %>%
  view()  
Khalsa_college=Khalsa_college %>%
  mutate(region=as.factor(region))
Khalsa_college

ggplot()
ggplot(data=Khalsa_college) + 
  geom_smooth(mapping = aes(x=tuition,y=sat_avg,color=region))
