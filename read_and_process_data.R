here::i_am("read_and_process_data.R")
library(here)
library(readxl)
library(tidyverse)

## ----------------------------------------------------------------------------------------------------------------------------------
#read data
rev <- read_xlsx(here("data", "RevBySource.xlsx"),
                 skip=1,
                 .name_repair = "universal") %>%
  mutate(across(DOR.Code:Fiscal.Year, as.factor))

inc <- read_xlsx(here("data", "DOR_Income_EQV_Per_Capita.xlsx"), 
                .name_repair = "universal") %>% 
  mutate(across(DOR.Code:Cherry.Sheet.FY, as.factor))

enrollment <- read_xlsx(here("data", "district-grade.xlsx"), 
                        skip=5,
                        .name_repair = "universal")

## ----------------------------------------------------------------------------------------------------------------------------------
# check that the municipalities in the two datasets are set-equivalent
setequal(unique(inc$Municipality), unique(rev$Municipality))


## ----------------------------------------------------------------------------------------------------------------------------------
#check that the length of the two datasets are equal
nrow(inc) == nrow(rev)


## ----------------------------------------------------------------------------------------------------------------------------------
# merge revenue and income data, and calculate percentiles
rev_inc <- rev %>%
  left_join(inc, by=c("DOR.Code", "Municipality")) %>%
  mutate(pci_rank = rank(DOR.Income.Per.Capita),
         pci_percentile=pci_rank/n(),
         per_capita_receipts = Total.Receipts...9/Population,
         pc_receipts_rank = rank(per_capita_receipts),
         pc_receipts_percentile = pc_receipts_rank/n(),
         pc_levy = Tax.Levy/Population,
         pc_levy_rank = rank(pc_levy),
         pc_levy_percentile = pc_levy_rank/n())


# find districts with full k-12 enrollment
enrollment <- enrollment %>%
  mutate(k_6=rowSums(pick(K:Gr.6)),
         Gr7_8 = rowSums(pick(Gr.7:Gr.8)),
         Gr9_12 = rowSums(pick(Gr.9:Gr.12)),
         full_k_12 = k_6 > 0 & Gr7_8 > 0 & Gr9_12)

k_12_districts <- enrollment %>%
  filter(full_k_12) %>%
  pull(District_NAME)

## ----------------------------------------------------------------------------------------------------------------------------------
mhd_percentiles <- rev_inc %>%
  filter(Municipality == "Marblehead") %>%
  select(contains("percentile"), contains("rank"))
mhd_percentiles


## ----------------------------------------------------------------------------------------------------------------------------------
pctile_half_width <- 0.05

# flag marblehead peers in the rev_inc data
rev_inc <-
  rev_inc %>%
  mutate(pci_peer = between(pci_percentile,
                 mhd_percentiles$pci_percentile - pctile_half_width,
                 mhd_percentiles$pci_percentile + pctile_half_width),
         ns_peer = pci_peer & County=="ESSEX")


## ----------------------------------------------------------------------------------------------------------------------------------
# teacher salary data
sal<- read.csv(here("data", "TeacherSalaries.csv")) %>%
  #rename sal column District.Name to Municipality
  rename(Municipality = District.Name) %>%
  mutate(across(!Municipality & !District.Code & where(is.character), parse_number))

age<- read.csv(here("data", "EducatorsbyAgeGroupsReport.csv"))


#make a new column in age that calculates the sum of all the columns that start with X
age <- age %>% mutate(TotalEducators = rowSums(select(age, starts_with("X")))) %>%
  #make a new column for each column that starts with X that takes the value in that column and divides it by Total Educators
  mutate(across(starts_with("X"), ~ . / FTE.Count)) %>%
  #mutate the colunn called Over.64.yrs.... to divide it by TotalEducators
  mutate(Over.64.yrs.... = Over.64.yrs.... / FTE.Count) %>%
  #rename the column called X.26.yrs..... to <26
  rename('<26' = X.26.yrs.....) %>%
  #rename the column called Over.64.yrs.... to Older
  rename('64 or more' = Over.64.yrs....) %>%
  #rename the columns starting with X so they are just the first two digit number that appears in the name
  rename_with(~str_extract(., "\\d{2}"), starts_with("X"))

#turn the data into longform making a row for columns <26, 26, 33, 41, 49, 57, Older
age_long <- age %>%
  pivot_longer(cols = c(`<26`, `26`, `33`, `41`, `49`, `57`, `64 or more`), names_to = "AgeGroup", values_to = "Percent")


## ----------------------------------------------------------------------------------------------------------------------------------
# merge income and teacher salary data, and calculate percentiles
inc_sal <- merge(inc, sal, by = "Municipality") %>%
  mutate(across(c(DOR.Income.Per.Capita, Average.Salary), as.numeric),
         income_percentile=rank(DOR.Income.Per.Capita)/n(), 
         salary_percentile=rank(Average.Salary)/n())



## ----------------------------------------------------------------------------------------------------------------------------------
mhd_inc_sal_percentiles <- inc_sal %>% 
  filter(Municipality == "Marblehead") %>% 
  select(contains("percentile"))

mhd_income_peers <- inc_sal %>%
  filter(between(income_percentile,
                 mhd_inc_sal_percentiles$income_percentile - 0.05,
                 mhd_inc_sal_percentiles$income_percentile + 0.05))


## ----------------------------------------------------------------------------------------------------------------------------------
# 3-way merge of revenue, income, and teacher salary
rev_inc_sal <- rev %>%
  left_join(inc, by=c("DOR.Code", "Municipality")) %>%
  left_join(sal, by="Municipality") %>%
  filter(Municipality %in% k_12_districts) %>%
  filter(!is.na(Average.Salary)) %>%
  mutate(pci_rank = rank(DOR.Income.Per.Capita),
         pci_percentile=pci_rank/n(),
         per_capita_receipts = Total.Receipts...9/Population,
         pc_receipts_rank = rank(per_capita_receipts),
         pc_receipts_percentile = pc_receipts_rank/n(),
         pc_levy = Tax.Levy/Population,
         pc_levy_rank = rank(pc_levy),
         pc_levy_percentile = pc_levy_rank/n(),
         salary_rank = rank(Average.Salary),
         salary_percentile=salary_rank/n())




# flag marblehead peers in the rev_inc data
rev_inc <-
  rev_inc %>%
  mutate(pci_peer = between(pci_percentile,
                            mhd_percentiles$pci_percentile - pctile_half_width,
                            mhd_percentiles$pci_percentile + pctile_half_width),
         ns_peer = pci_peer & County=="ESSEX")


## ----------------------------------------------------------------------------------------------------------------------------------
mhd_3_way_percentiles <- rev_inc_sal %>%
  filter(Municipality == "Marblehead") %>%
  select(contains("percentile"), contains("rank"))


## ----------------------------------------------------------------------------------------------------------------------------------
# flag pci peers in the rev_inc_sal data
rev_inc_sal <-
  rev_inc_sal %>%
  mutate(pci_peer = between(pci_percentile,
                            mhd_3_way_percentiles$pci_percentile - pctile_half_width,
                            mhd_3_way_percentiles$pci_percentile + pctile_half_width),
         ns_peer = pci_peer & County=="ESSEX")


mhd_tax_peers <- rev_inc_sal %>%
  filter(!is.na(Average.Salary)) %>%
  filter(between(pc_receipts_percentile,
                 mhd_3_way_percentiles$pc_receipts_percentile - 0.05,
                 mhd_3_way_percentiles$pc_receipts_percentile + 0.05))

