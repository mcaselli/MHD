
library(readxl)
library(tidyverse)
library(ggrepel)
library(gghighlight)
library(ggpubr)
library(scales)
library(here)

#read data
rev <- read_xlsx(here("data", "RevBySource.xlsx"),
                 skip=1,
                 .name_repair = "universal")
inc<- read_xlsx(here("data", "DOR_Income_EQV_Per_Capita.xlsx"), .name_repair = "universal")
sal<- read.csv(here("data", "TeacherSalaries.csv"))
age<- read.csv(here("data", "EducatorsbyAgeGroupsReport.csv"))

#rename sal column District.Name to Municipality
sal <- sal %>% rename(Municipality = District.Name)


#make a new column in age that calculates the sum of all the columns that start with X
age <- age %>% mutate(TotalEducators = rowSums(select(age, starts_with("X"))))

#make a new column for each column that starts with X that takes the value in that column and divides it by Total Educators
age <- age %>% mutate(across(starts_with("X"), ~ . / FTE.Count))

#mutate the colunn called Over.64.yrs.... to divide it by TotalEducators
age <- age %>% mutate(Over.64.yrs.... = Over.64.yrs.... / FTE.Count)

#rename the column called X.26.yrs..... to <26
age <- age %>% rename('<26' = X.26.yrs.....)

#rename the column called Over.64.yrs.... to Older
age <- age %>% rename('64 or more' = Over.64.yrs....)

#rename the columns starting with X so they are just the first two digit number that appears in the name
age <- age %>% rename_with(~str_extract(., "\\d{2}"), starts_with("X"))

#turn the data into longform making a row for columns <26, 26, 33, 41, 49, 57, Older
age_long <- age %>%
  pivot_longer(cols = c(`<26`, `26`, `33`, `41`, `49`, `57`, `64 or more`), names_to = "AgeGroup", values_to = "Percent")


#plot age_long AgeGroup vs Count using ggplot
plot1 <- ggplot(age_long, aes(x = AgeGroup, y = Percent)) +
  geom_point(alpha = .01) +
  theme_minimal() +
  guides(fill="none")+
  #geom_point(data = age_long %>% filter(District.Name == "Marblehead"), aes(x = AgeGroup, y = Percent, color = ""#F8766D""))+
  geom_label(data = age_long %>% filter(District.Name == "Marblehead"), aes(label = round(Percent, digits = 2), x = AgeGroup, y = Percent), color = "#F8766D")+
  labs(x = "Age Group",
       y = "Proportion of Educators")+
    theme(legend.position="none")
  
#save plot4 as a png
ggsave(here("figures", "age.png"), plot1, width = 4, height = 2, units = "in")

  
#find the sal that are not in inc
sal[!(sal$Municipality %in% inc$Municipality),]

#merge income and salaries
merged <- merge(inc, sal, by = "Municipality")

#make Average.Salary numeric
merged$Average.Salary <- as.numeric(merged$Average.Salary)
merged$DOR.Income.Per.Capita <- as.numeric(merged$DOR.Income.Per.Capita)

#make a column in merge for Fill where if Municipality is Marblehead, fill is red otherwise it is skyblue
merged <- merged %>%
  mutate(FillCol = ifelse(Municipality == "Marblehead", "#00BFC4", "#F8766D"))


#plot DOR.Income by Average.Salary using ggplot
plot2 <- ggplot(merged, aes(x = DOR.Income.Per.Capita, y = Average.Salary)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "loess", se = FALSE, color = "#7CAE00") +
  theme_minimal()+
  geom_text_repel(data = merged %>% filter( !(Municipality == "Marblehead")), aes(label = Municipality), max.overlaps = 20, size = 3, segment.color = 'grey80') +
  geom_label_repel(data = merged %>% filter(Municipality == "Marblehead"), aes(label = Municipality), color = "#F8766D", position = position_nudge_repel(x =100, y =15))+
  geom_label_repel(data = merged %>% filter(Municipality == "Gloucester" |Municipality == "Beverly"), aes(label = Municipality), color = "#3B9AB2") +
  geom_point(data = merged %>% filter(Municipality == "Marblehead"), color = "#F8766D", size = 3) +
  geom_point(data = merged %>% filter(Municipality == "Gloucester" |Municipality == "Beverly"), color = "#00BFC4", size = 2) +
  scale_x_continuous(labels = scales::label_dollar()) +
  scale_y_continuous(labels = scales::label_dollar())+
  labs(x = "Per Capita Income",
       y = "Average Teacher Salary", 
       caption = "*DOR Income Per Capita represents the annual income as \nreported on annual tax returns divided by the US Census Population.")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        axis.text=element_text(size=12),
        axis.title=element_text(size=18))

ggsave(here("figures", "sal.png"), plot2, width = 9, height = 9, units = "in")



merged <- merged %>%
    mutate(income_percentile=rank(DOR.Income.Per.Capita)/n(), 
           salary_percentile=rank(Average.Salary)/n())
  
  
mhd_percentiles <- merged %>% 
  filter(Municipality == "Marblehead") %>% 
  select(contains("percentile"))

mhd_income_peers <- merged %>%
  filter(between(income_percentile,
                 mhd_percentiles$income_percentile - 0.05,
                 mhd_percentiles$income_percentile + 0.05))

mhd_income_peers %>%
  arrange(desc(Average.Salary)) %>%
  select(Municipality, Average.Salary, contains("percentile"))

#make a bar graph of the average salary of Marblehead and its income peers
plot3<- mhd_income_peers %>%
  ggplot(aes(y = reorder(Municipality, Average.Salary), x = Average.Salary, fill = FillCol)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Average Teacher Salary",
       y = "",
       x = "Average Teacher Salary",
       caption = "*Districts with per capita income rates \nsimilar to Marblehead (+/- 5 percentage points)") +
  geom_text(aes(label = scales::dollar(Average.Salary)), hjust = -0.1, size = 4)+
   guides(fill="none")+
   coord_cartesian(xlim = c(0, 150000)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        title=element_text(size=18))

#make a bar graph of the average salary of Marblehead and its income peers
plot4<- mhd_income_peers %>%
  ggplot(aes(y = reorder(Municipality, DOR.Income.Per.Capita), x = DOR.Income.Per.Capita, fill = FillCol)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Per Capita Income",
         y = "Municipality",
       x = "DOR Income Per Capita",
       caption = "*Districts with per capita income rates \nsimilar to Marblehead (+/- 5 percentage points)") +
  geom_text(aes(label = scales::dollar(DOR.Income.Per.Capita)), hjust = -0.1, size = 4)+
  guides(fill="none")+
  coord_cartesian(xlim = c(0, 260000)) +
  theme(plot.caption = element_text(color = "white"),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title=element_text(size=18),
        axis.text=element_text(size=14),
        title=element_text(size=18))
 
 
 comp <- ggarrange( plot4, plot3, ncol = 2, nrow = 1, widths = c(.5, .5))
 
 ggsave(here("figures", "comp.png"), comp, width = 12, height = 12, units = "in")
 

 #merge tax and income data
 taxinc <- rev %>%
   filter(Fiscal.Year == 2023) %>%
   left_join(inc, by=c("Municipality")) %>%
   mutate(pci_percentile=rank(DOR.Income.Per.Capita)/n(),
          per_capita_receipts = Total.Receipts...9/Population,
          pc_receipts_percentile = rank(per_capita_receipts)/n(),
          pc_levy_percentile = rank(Tax.Levy)/n())
 
 plot5 <- taxinc %>%
   mutate(per_capita_receipts = Total.Receipts...9/Population) %>%
   ggplot(aes(x=DOR.Income.Per.Capita, y=per_capita_receipts, label=Municipality)) +
   geom_point(colour = "darkgrey", alpha = .5) +
   geom_label_repel(data = taxinc %>% filter(Municipality == "Marblehead"), aes(label = Municipality), color = "#F8766D", position = position_nudge_repel(x =100, y =15))+
   geom_point(data = taxinc %>% filter(Municipality == "Marblehead"), color = "#F8766D", size = 3) +
   scale_x_continuous(labels=label_dollar()) +
   scale_y_continuous(labels=label_dollar())+ 
   theme_minimal()+
   labs(x= "DOR Income Per Capita",
        y = "Taxes Per Capita")+
   theme(plot.caption.position = "plot",
         plot.caption = element_text(hjust = 0),
         axis.text=element_text(size=12),
         axis.title=element_text(size=18))
 
 ggsave(here("figures", "tax.png"), plot5, width = 6, height = 3, units = "in")
 
 #merge taxinc with sal
 taxinc_sal <- taxinc %>%
   left_join(sal, by=c("Municipality")) 


plot6<-  taxinc_sal %>% 
  filter(per_capita_receipts < 12000) %>%
   ggplot(aes(x=per_capita_receipts, y=Average.Salary, label=Municipality)) +
   geom_point(colour = "darkgrey", alpha = .5) +
  #geom_text_repel(data = taxinc_sal %>% filter( !(Municipality == "Marblehead")), aes(label = Municipality), max.overlaps = 5, size = 3, segment.color = 'grey80') +
  geom_label_repel(data = taxinc_sal %>% filter(Municipality == "Marblehead"), aes(label = Municipality), color = "#F8766D")+
   geom_point(data = taxinc_sal %>% filter(Municipality == "Marblehead"), color = "#F8766D", size = 3) +
   scale_x_continuous(labels=label_dollar(), limits = c(0, 15000)) +
   scale_y_continuous(labels=label_dollar())+ 
   theme_minimal()+
   labs(x= "Taxes Per Capita",
        y = "Average Teacher Salary")+
   theme(plot.caption.position = "plot",
         plot.caption = element_text(hjust = 0),
         axis.text=element_text(size=12),
         axis.title=element_text(size=18))
       
ggsave(here("figures", "taxsal.png"), plot6, width = 7, height = 3.5, units = "in")


taxinc_sal <- taxinc_sal %>%
  filter(!is.na(Average.Salary)) %>%
  mutate(tax_percentile=rank(per_capita_receipts)/n(), 
         salary_percentile=rank(Average.Salary)/n())

mhd_tax_percentiles <- taxinc_sal %>%
  filter(Municipality == "Marblehead") %>%
  select(tax_percentile, salary_percentile)
mhd_tax_percentiles

mhd_tax_peers <- taxinc_sal %>%
  filter(!is.na(Average.Salary)) %>%
  filter(between(tax_percentile,
                 mhd_tax_percentiles$tax_percentile - 0.05,
                 mhd_tax_percentiles$tax_percentile + 0.05))



mhd_tax_peers <- mhd_tax_peers %>%
  mutate(FillCol = ifelse(Municipality == "Marblehead", "#00BFC4", "#F8766D"))

#make a bar graph of the average salary of Marblehead and its income peers
plot7<- mhd_tax_peers %>%
  ggplot(aes(y = reorder(Municipality, Average.Salary), x = Average.Salary, fill = FillCol)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Average Teacher Salary",
       y = "",
       x = "Average Teacher Salary",
       caption = "*Districts with per capita tax rates \nsimilar to Marblehead (+/- 5 percentage points)") +
  geom_text(aes(label = scales::dollar(Average.Salary)), hjust = -0.1, size = 4)+
  guides(fill="none")+
  coord_cartesian(xlim = c(0, 150000)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        title=element_text(size=18))

#make a bar graph of the average salary of Marblehead and its income peers
plot8<- mhd_tax_peers %>%
  ggplot(aes(y = reorder(Municipality, per_capita_receipts), x = per_capita_receipts, fill = FillCol)) +
  geom_col() +
  theme_minimal() +
   labs(title = "Per Capita Taxes",
        y = "Municipality",
        x = "Taxes Per Capita",
        caption = "*Districts with per capita tax rates \nsimilar to Marblehead (+/- 5 percentage points)") +
  geom_text(aes(label = scales::dollar(per_capita_receipts)), hjust = -0.1, size = 4)+
  guides(fill="none")+
  coord_cartesian(xlim = c(0, 8000)) +
  theme(plot.caption = element_text(color = "white"),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title=element_text(size=18),
        axis.text=element_text(size=14),
        title=element_text(size=18))

comp2 <- ggarrange( plot8, plot7, ncol = 2, nrow = 1, widths = c(.5, .5))
ggsave(here("figures", "taxsalpeers.png"), comp2, width = 12, height = 12, units = "in")
