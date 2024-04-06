library(tidyverse)
library(scales)
library(rstatix)

raw_data <- read_csv("RawData/Survey_Raw_Data_May 11_2022_07_22.csv") %>% 
  filter(StartDate != "Start Date" & StartDate != '{"ImportId":"startDate","timeZone":"America/Denver"}')

#___________________________________________________________________________
#Demographics
#Q1 Gender, Q2 Age group, Q3 Direct Experience?

demo_tidy <- raw_data %>% 
  select(Q1, Q2, Q3) %>%  
  pivot_longer(Q1:Q3,names_to = "Question", values_to = "Response") %>% 
  filter(!is.na(Response)) %>% 
  mutate(Question = case_when(grepl("1", Question)~ "Gender",
                              grepl("2", Question)~ "Age",
                              grepl("3", Question)~ "Experience with the Black Summer")) %>% 
  mutate(Question = factor(Question, c("Gender", "Age", "Experience with the Black Summer")))

demo_tidy %>% 
  filter(Question != "Experience with the Black Summer") %>% 
  ggplot(aes(x = Response, fill = "red")) +
  geom_histogram(stat = "count", position = "dodge")+
  theme(axis.text.x = element_text(angle = -90, hjust = 0))+
  facet_wrap(~Question, scales = "free_x")

ages <- raw_data %>% 
  select(Q2) %>% 
  count(Q2) %>% 
  rename("Interviewees"= Q2)

genders <- raw_data %>% 
  select(Q1) %>% 
  count(Q1) %>% 
  rename("Interviewees"= Q1)

directly_impacted <- raw_data %>% 
  select(Q3) %>% 
  count(Q3) %>% 
  rename("Interviewees"= Q3)

demo_table <- tribble(~Interviewees, ~n, "Total interviewees", 37) %>% 
  rbind(ages) %>% 
  rbind(genders) %>% 
  rbind(directly_impacted)
write_csv(demo_table, "Figures/demographics_table_raw.csv")
#___________________________________________________________________________
#Perceived risk and impacts

#Perceived risk of bushfires occurring (before and after Black Summer):
# i.	Occurrence on one’s property; Q17_1 (before), Q18_1 (after)
# ii.	Occurrence in one’s community; Q17_2 (before), Q18_2 (after)
# iii.	Occurrence in the Australian landscape; Q17_3 (before), Q18_3 (after)

risks_data <- raw_data %>% select(Q17_1:Q18_3) %>% 
  mutate_all(~ case_when(. == "Extremely Low" ~ 1,
                         . == "Low" ~ 2,
                         . == "Medium" ~ 3,
                         . == "High" ~ 4,
                         . == "Extremely High" ~ 5)) 

tR1 <- t.test(risks_data$Q17_1, risks_data$Q18_1)
tR1
tR2 <- t.test(risks_data$Q17_2, risks_data$Q18_2)
tR2
tR3 <- t.test(risks_data$Q17_3, risks_data$Q18_3)
tR3

risks_tidy <- raw_data %>% 
  select(Q17_1:Q18_3) %>% 
  pivot_longer(Q17_1:Q18_3,names_to = "Question", values_to = "Response") %>% 
  filter(!is.na(Response)) %>% 
  mutate(Treatment = ifelse(grepl("17", Question), "Before", "After")) %>%
  mutate(Question = case_when(grepl("_1", Question)~ "On my property",
                              grepl("_2", Question)~ "In my community",
                              grepl("_3", Question)~ "In Australia")) %>% 
  mutate(Treatment = factor(Treatment, c("Before", "After"))) %>% 
  mutate(Response = factor(Response, c("Extremely Low", "Low", "Medium", "High", "Extremely High", "No Opinion")))


risks_tidy %>% 
  group_by(Treatment) %>% 
  ggplot(aes(x = Response, fill = Treatment)) +
  geom_histogram(position = "dodge", stat = "count") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0))+
  facet_wrap(~Question)

risks_b4 <- risks_tidy %>% 
  mutate(Response = as.character(Response)) %>% 
  mutate(Response = ifelse(Response == "Extremely Low", 
                           "Low", 
                           ifelse(Response == "Extremely High", 
                                  "High", 
                                  Response))) %>% 
  filter(Treatment == "Before") %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  rename("Before" = n) %>% 
  mutate(Before = paste0(Before, " (", round(Before/35*100, 2), "%)"))
  
risks_after <- risks_tidy %>% 
  mutate(Response = as.character(Response)) %>% 
  mutate(Response = ifelse(Response == "Extremely Low", "Low", 
                           ifelse(Response == "Extremely High", "High", Response))) %>% 
  filter(Treatment == "After") %>% 
  group_by(Question) %>% 
  count(Response)%>% 
  rename("After" = n)%>% 
  mutate(After = paste0(After, " (", round(After/35*100, 2), "%)"))

risks_table <- risks_b4 %>% 
  left_join(risks_after) %>% 
  mutate(Response = factor(Response, c("Low", "Medium", "High"))) %>% 
  mutate(Question = factor(Question, c("On my property", "In my community", "In Australia"))) %>% 
  arrange(Question, Response) 

#_____________________________________________________________________________________
impacts_data <- raw_data %>% select(Q19_1:Q20_4) %>% 
  mutate_all(~ case_when(. == "Negative" ~ -1,
                         . == "No Impact" ~ 0,
                         . == "Positive" ~ 1))

tI1 <- t.test(impacts_data$Q19_1, impacts_data$Q20_1)
tI1
tI2 <- t.test(impacts_data$Q19_2, impacts_data$Q20_2)
tI2
tI3 <- t.test(impacts_data$Q19_3, impacts_data$Q20_3)
tI3
tI4 <- t.test(impacts_data$Q19_4, impacts_data$Q20_4)
tI4

impacts_tidy <- raw_data %>% 
  select(Q19_1:Q20_4) %>% 
  pivot_longer(Q19_1:Q20_4,names_to = "Question", values_to = "Response") %>% 
  filter(!is.na(Response)) %>% 
  mutate(Treatment = ifelse(grepl("19", Question), "Before", "After")) %>% 
  mutate(Question = case_when(grepl("_1", Question)~ "Native vegetation",
                              grepl("_2", Question)~ "Cropland productivity",
                              grepl("_3", Question)~ "Grassland productivity",
                              grepl("_4", Question)~ "Soil function")) %>% 
  mutate(Treatment = factor(Treatment, c("Before", "After"))) %>% 
  mutate(Response = factor(Response, c("Negative", "No Impact", "Positive", "No Opinion")))

impacts_tidy %>% 
  group_by(Treatment) %>% 
  ggplot(aes(x = Response, fill = Treatment)) +
  geom_histogram(stat = "count", position = "dodge")+
  theme(axis.text.x = element_text(angle = -90, hjust = 0))+
  facet_wrap(~Question)

impacts_tidy %>% 
  filter(Treatment == "After") %>% 
  ggplot(aes(x = Response, fill = "red")) +
  geom_histogram(stat = "count", position = "dodge")+
  theme(axis.text.x = element_text(angle = -90, hjust = 0),
        text=element_text(family="sans"))+
  ylab("n")+
  facet_wrap(~Question, scales = 'free', nrow = 1)

impacts_b4 <- impacts_tidy %>% 
  filter(Treatment == "Before") %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  rename("Before" = n) %>% 
  mutate(Before = paste0(Before, " (", round(Before/34*100, 2), "%)"))

impacts_after <- impacts_tidy %>% 
  filter(Treatment == "After") %>% 
  group_by(Question) %>% 
  count(Response)%>% 
  rename("After" = n)%>% 
  mutate(After = paste0(After, " (", round(After/34*100, 2), "%)"))

impacts_table <- impacts_b4 %>% 
  left_join(impacts_after) %>% 
  mutate(Response = factor(Response, c("Negative", "No Impact", "Positive", "No Opinion"))) %>% 
  mutate(Question = factor(Question, c("Native vegetation",
                                       "Cropland productivity",
                                       "Grassland productivity", 
                                       "Soil function"))) %>% 
  arrange(Question, Response) %>% 
  mutate(Before = ifelse(is.na(Before), 0, Before)) %>% 
  mutate(After = ifelse(is.na(After), 0, After)) 
#___________________________________________________________________________
#Chi-squared Tests
risks_b4_2 <- risks_tidy %>% 
  mutate(Response = as.character(Response)) %>% 
  mutate(Response = ifelse(Response == "Extremely Low", 
                           "Low", 
                           ifelse(Response == "Extremely High", 
                                  "High", 
                                  Response))) %>% 
  filter(Treatment == "Before") %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  rename("Before" = n)
risks_after_2 <- risks_tidy %>% 
  mutate(Response = as.character(Response)) %>% 
  mutate(Response = ifelse(Response == "Extremely Low", "Low", 
                           ifelse(Response == "Extremely High", "High", Response))) %>% 
  filter(Treatment == "After") %>% 
  group_by(Question) %>% 
  count(Response)%>% 
  rename("After" = n)
risks_table_2 <- risks_b4_2 %>% 
  left_join(risks_after_2) %>% 
  mutate(Response = factor(Response, c("Low", "Medium", "High"))) %>% 
  mutate(Question = factor(Question, c("On my property", "In my community", "In Australia"))) %>% 
  rbind(tribble(~Question, ~Response, ~Before, ~After, 
                "In Australia", "Low", 0,0)) %>% 
  arrange(Question, Response)

impacts_b4_2 <- impacts_tidy %>% 
  filter(Treatment == "Before") %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  rename("Before" = n)
impacts_after_2 <- impacts_tidy %>% 
  filter(Treatment == "After") %>% 
  group_by(Question) %>% 
  count(Response)%>% 
  rename("After" = n)
impacts_table_2 <- impacts_b4_2 %>% 
  left_join(impacts_after_2) %>% 
  mutate(Response = factor(Response, c("Negative", "No Impact", "Positive", "No Opinion"))) %>% 
  mutate(Question = factor(Question, c("Native vegetation",
                                       "Cropland productivity",
                                       "Grassland productivity", 
                                       "Soil function"))) %>% 
  mutate(Before = ifelse(is.na(Before), 0, Before)) %>% 
  mutate(After = ifelse(is.na(After), 0, After)) %>% 
  arrange(Question, Response)

risks_table_chi <- risks_table_2 %>% 
  mutate(chi = ifelse(Before == 0 & After == 0, 0, ((After-Before)^2)/Before)) %>% 
  group_by(Question) %>%
  mutate(degrees_of_freedom = length(unique(.$Response))-1) %>% 
  summarise(chi_squared = sum(chi), degrees_of_freedom = degrees_of_freedom) %>% 
  mutate(pval = 1 - pchisq(chi_squared, df=degrees_of_freedom, lower.tail=TRUE)) %>% 
  distinct()

impacts_table_chi <- impacts_table_2 %>% 
  mutate(chi = ifelse(Before == 0 & After == 0, 0, ((After-Before)^2)/Before)) %>% 
  group_by(Question) %>% 
  mutate(degrees_of_freedom = length(unique(.$Response))-1) %>% 
  summarise(chi_squared = sum(chi), degrees_of_freedom = degrees_of_freedom) %>% 
  mutate(pval = 1 - pchisq(chi_squared, df=degrees_of_freedom, lower.tail=TRUE))%>% 
  distinct()

risks_table %>% left_join(risks_table_chi) %>% 
  write_csv("Figures/perceptions_risks_table_raw.csv")
  
impacts_table %>% left_join(impacts_table_chi) %>% 
  write_csv("Figures/perceptions_impacts_table_raw.csv")
#_______________________________________________________________________________
#Impacts on interests

interests_table <- raw_data %>% 
  select(Q21) %>% 
  count(Q21) %>% 
  filter(!is.na(Q21))

other_interests <- raw_data %>% 
  select(Q21_5_TEXT) %>% 
  filter(!is.na(Q21_5_TEXT))

#___________________________________________________________________________
#Behaviours

behaviour_data <- raw_data %>% select(Q24_1:Q24_6) %>% 
  mutate_all(~ case_when(. == "Yes" ~ 1,
                         . == "No" ~ 0)) 

tB1 <- t.test(behaviour_data$Q24_1, behaviour_data$Q24_2)
tB1
tB2 <- t.test(behaviour_data$Q24_3, behaviour_data$Q24_4)
tB2
tB3 <- t.test(behaviour_data$Q24_5, behaviour_data$Q24_6)
tB3

behaviours_data_2 <- raw_data %>% select(Q25_1:Q25_6) %>% 
  mutate_all(~ case_when(. == "Yes" ~ 1,
                         . == "No" ~ 0)) %>% 
  rowwise() %>% 
  mutate(Behaviours_changed = sum(Q25_1, Q25_2, Q25_3, Q25_4, Q25_5, Q25_6,na.rm=TRUE)) 

behaviours_data_3 <- behaviours_data_2 %>% 
  mutate(None = ifelse(Behaviours_changed == 0, 1, 0)) %>% 
  mutate(OnePlus = ifelse(Behaviours_changed > 0, 1, 0)) %>%
  select(-Behaviours_changed) %>% 
  pivot_longer(Q25_1:OnePlus,names_to = "Question", values_to = "Response") %>% 
  filter(!is.na(Response) & Response > 0) %>% 
  mutate(Question = case_when(grepl("_1", Question)~ "Fire management plan",
                              grepl("_2", Question)~ "Seeking information",
                              grepl("_3", Question)~ "Hotspots workshop",
                              grepl("_4", Question)~ "Post-fire restoration",
                              grepl("_5", Question)~ "Community involvement",
                              grepl("_6", Question)~ "Other actions",
                              grepl("None", Question)~ "No changes",
                              grepl("OnePlus", Question)~ "1+ change(s)")) %>% 
  mutate(Question = factor(Question, c("No changes",
                                       "1+ change(s)",
                                       "Hotspots workshop", 
                                       "Community involvement",
                                       "Seeking information",
                                       "Fire management plan",
                                       "Post-fire restoration",
                                       "Other actions")))
  

behaviours_data_2 %>% 
  ggplot(aes(x = Behaviors_changed)) +
  geom_histogram(stat = "count", position = "dodge")+
  theme(axis.text.x = element_text(angle = -90, hjust = 0))

behaviours_data_3 %>% 
  filter(!Question %in% c("No changes", "1+ change(s)")) %>% 
  ggplot(aes(x = Question, fill = Question)) +
  geom_histogram(stat = "count", position = "dodge")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0))+
  ylab("n")+
  xlab("")

behaviours_data_3 %>% 
  filter(Question %in% c("No changes", "1+ change(s)")) %>% 
  ggplot(aes(x = Question, fill = Question)) +
  geom_histogram(stat = "count", position = "dodge")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0))+
  ylab("n")+
  xlab("")

behaviours_tidy <- raw_data %>% 
  select(Q24_1:Q24_6) %>% 
  pivot_longer(Q24_1:Q24_6,names_to = "Question", values_to = "Response") %>% 
  filter(!is.na(Response)) %>% 
  mutate(Treatment = case_when(grepl("_1", Question)~ "Before",
                               grepl("_3", Question)~ "Before",
                               grepl("_5", Question)~ "Before",
                               grepl("_2", Question)~ "After",
                               grepl("_4", Question)~ "After",
                               grepl("_6", Question)~ "After")) %>% 
  mutate(Question = case_when(grepl("_1", Question)~ "Fire management plan",
                              grepl("_2", Question)~ "Fire management plan",
                              grepl("_3", Question)~ "Fire fighting",
                              grepl("_4", Question)~ "Fire fighting",
                              grepl("_5", Question)~ "Post-fire restoration",
                              grepl("_6", Question)~ "Post-fire restoration")) %>% 
  mutate(Treatment = factor(Treatment, c("Before", "After"))) %>% 
  mutate(Response = factor(Response, c("No", "Yes", "NA/Unknown")))


behaviours_tidy %>% 
  group_by(Treatment) %>% 
  ggplot(aes(x = Response, fill = Treatment)) +
  geom_histogram(stat = "count", position = "dodge")+
  theme(axis.text.x = element_text(angle = -90, hjust = 0))+
  facet_wrap(~Question)

behaviours_b4 <- behaviours_tidy %>% 
  filter(Treatment == "Before") %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  rename("Before" = n) %>% 
  mutate(Before = paste0(Before, " (", round(Before/33*100, 2), "%)"))

behaviours_after <- behaviours_tidy %>% 
  filter(Treatment == "After") %>% 
  group_by(Question) %>% 
  count(Response)%>% 
  rename("After" = n)%>% 
  mutate(After = paste0(After, " (", round(After/33*100, 2), "%)"))

behaviours_table <- behaviours_b4 %>% 
  left_join(behaviours_after) %>% 
  mutate(Response = factor(Response, c("Yes", "No", "NA/Unknown"))) %>% 
  mutate(Question = factor(Question, c("Fire management plan",
                                       "Fire fighting",
                                       "Post-fire restoration"))) %>% 
  arrange(Question, Response) 
#_______________________________________________________________________________
#Chi-squared

behaviours_b4_2 <- behaviours_tidy %>% 
  filter(Treatment == "Before") %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  rename("Before" = n)
behaviours_after_2 <- behaviours_tidy %>% 
  filter(Treatment == "After") %>% 
  group_by(Question) %>% 
  count(Response)%>% 
  rename("After" = n)
behaviours_table_2 <- behaviours_b4_2 %>% 
  left_join(behaviours_after_2) %>% 
  mutate(Response = factor(Response, c("Yes", "No", "NA/Unknown"))) %>% 
  mutate(Question = factor(Question, c("Fire management plan",
                                       "Fire fighting",
                                       "Post-fire restoration"))) %>% 
  arrange(Question, Response) %>% 
  arrange(Question, Response)

behaviours_table_chi <- behaviours_table_2 %>% 
  mutate(chi = ifelse(Before == 0 & After == 0, 0, ((After-Before)^2)/Before)) %>% 
  group_by(Question) %>% 
  mutate(degrees_of_freedom = length(unique(.$Response))-1) %>% 
  summarise(chi_squared = sum(chi), degrees_of_freedom = degrees_of_freedom) %>% 
  mutate(pval = 1 - pchisq(chi_squared, df=degrees_of_freedom, lower.tail=TRUE))%>% 
  distinct() 

behaviours_table %>% left_join(behaviours_table_chi) %>% 
  write_csv("Figures/behaviours_table_raw.csv")

#_______________________________________________________________________________________

behaviours_tidy2 <-   raw_data %>% 
  select(Q25_1:Q25_6) %>% 
  pivot_longer(Q25_1:Q25_6,names_to = "Question", values_to = "Response") %>% 
  filter(!is.na(Response)) %>% 
  mutate(Question = case_when(grepl("_1", Question)~ "Fire management plan",
                              grepl("_2", Question)~ "Seeking information",
                              grepl("_3", Question)~ "Hotspots workshop",
                              grepl("_4", Question)~ "Post-fire restoration",
                              grepl("_5", Question)~ "Community involvement",
                              grepl("_6", Question)~ "Other actions")) %>% 
  mutate(Response = factor(Response, c("No", "Yes", "NA/Unknown"))) %>% 
  mutate(Question = factor(Question, c("Hotspots workshop", 
                                       "Community involvement",
                                       "Seeking information",
                                       "Fire management plan",
                                       "Post-fire restoration",
                                       "Other actions")))

behaviours_table2 <- behaviours_tidy2 %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  pivot_wider(names_from = Response, values_from = n) %>% 
  select(Question, Yes, No, `NA/Unknown`) %>% 
  mutate(n = Yes + No + `NA/Unknown`) %>% 
  mutate(Yes = paste0(Yes, " (", round(Yes/33*100, 2), "%)"),
         No = paste0(No, " (", round(No/33*100, 2), "%)"),
         `NA/Unknown` = paste0(`NA/Unknown`, " (", round(`NA/Unknown`/33*100, 2), "%)")) %>% 
  select(-n)

behaviours_chi <- behaviours_tidy2 %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  pivot_wider(names_from = Response, values_from = n) %>% 
  select(Question, Yes, No, `NA/Unknown`) %>% 
  mutate(n = Yes + No + `NA/Unknown`) %>% 
  mutate(not_Yes = No + `NA/Unknown`) %>% 
  mutate(chi_squared = prop.test(x = Yes, n = n, alternative = "two.sided")$statistic[[1]]) %>% 
  mutate(pval = prop.test(x = Yes, n = n, alternative = "two.sided")[3][[1]]) %>% 
  select(Question, chi_squared, pval)

behaviours_table2 %>% left_join(behaviours_chi) %>%  
  write_csv("Figures/behaviours2_table_raw.csv")

#_________________________________________________________________________________________
##Binomial Tests
# binom.test(x=3, n=10, p=.5)

behaviours_binomial <- behaviours_tidy2 %>% 
  group_by(Response, Question) %>% 
  count(Response) %>% 
  group_by(Question) %>% 
  mutate(Total = sum(n))

filter(behaviours_binomial, Question == "Fire management plan" & Response == "Yes")$n[1] %>% 
  binom.test(n = behaviours_binomial$Total[1], p = 0)

filter(behaviours_binomial, Question == "Seeking information" & Response == "Yes")$n[1] %>% 
  binom.test(n = behaviours_binomial$Total[1], p = 0)

filter(behaviours_binomial, Question == "Hotspots workshop" & Response == "Yes")$n[1] %>% 
  binom.test(n = behaviours_binomial$Total[1], p = 0)

filter(behaviours_binomial, Question == "Community involvement" & Response == "Yes")$n[1] %>% 
  binom.test(n = behaviours_binomial$Total[1], p = 0)

filter(behaviours_binomial, Question == "Seeking information" & Response == "Yes")$n[1] %>% 
  binom.test(n = behaviours_binomial$Total[1], p = 0)

filter(behaviours_binomial, Question == "Other actions" & Response == "Yes")$n[1] %>% 
  binom.test(n = behaviours_binomial$Total[1], p = 0)

behaviours_tidy2 %>% 
  ggplot(aes(x = Response, fill = "red")) +
  geom_histogram(stat = "count")+
  theme(axis.text.x = element_text(angle = -90, hjust = 0),
        text=element_text(family="sans"))+
  ylab("n")+
  facet_wrap(~Question, scales = 'free')

behaviours_tidy2 %>% 
  filter(Response != "NA/Unknown") %>% 
  ggplot(aes(x = Response, fill = "red")) +
  geom_histogram(stat = "count")+
  theme(axis.text.x = element_text(angle = -90, hjust = 0),
        text=element_text(family="sans"))+
  ylab("n")+
  facet_wrap(~Question, scales = 'free')

behaviours_tidy2 %>% 
  filter(Response != "NA/Unknown") %>% 
  ggplot(aes(x = Response, fill = Response)) +
  geom_histogram(stat = "count")+
  theme(axis.text.x = element_text(angle = -90, hjust = 0),
        text=element_text(family="sans"))+
  ylab("n")+
  facet_wrap(~Question, scales = 'free') +
  theme_bw() +
  scale_fill_manual(values=c("#9900CC", "#339933"))

#___________________________________________________________________________
#Soil impacts


soil_tidy <- raw_data %>% 
  select(Q14_1:Q15_9) %>% 
  pivot_longer(Q14_1:Q15_9,names_to = "Question", values_to = "Response") %>% 
  filter(!is.na(Response)) %>% 
  mutate(Treatment = case_when(grepl("Q14", Question)~ "Managed",
                               grepl("Q15", Question)~ "Nonmanaged"))%>% 
  mutate(Question = case_when(grepl("14_2", Question)~ "Fine root density",
                              grepl("14_3", Question)~ "# of soil animal types",
                              grepl("14_4", Question)~ "# of worms",
                              grepl("14_5", Question)~ "Soil water repellency",
                              grepl("14_6", Question)~ "Erosion",
                              grepl("14_7", Question)~ "Soil hardness",
                              grepl("14_8", Question)~ "Subsurface compaction",
                              grepl("14_9", Question)~ "Soil nutrient levels",
                              grepl("14_10", Question)~ "pH",
                              grepl("15_1", Question)~ "Fine root density",
                              grepl("15_2", Question)~ "# of soil animal types",
                              grepl("15_3", Question)~ "# of worms",
                              grepl("15_4", Question)~ "Soil water repellency",
                              grepl("15_5", Question)~ "Erosion",
                              grepl("15_6", Question)~ "Soil hardness",
                              grepl("15_7", Question)~ "Subsurface compaction",
                              grepl("15_8", Question)~ "Soil nutrient levels",
                              grepl("15_9", Question)~ "pH")) %>% 
  mutate(Question = ifelse(is.na(Question), "Crop health", Question))%>% 
  mutate(Question = factor(Question, c("Erosion", 
                                       "Soil hardness",
                                       "Soil water repellency",
                                       "Subsurface compaction",
                                       "# of soil animal types", 
                                       "# of worms", 
                                       "Fine root density",
                                       "pH", 
                                       "Soil nutrient levels",
                                       "Crop health"
                                       ))) %>% 
  mutate(Treatment = factor(Treatment, c("Nonmanaged", "Managed"))) %>% 
  mutate(Response = factor(Response, c("Decreased", "Not changed", "Increased", "NA/Unknown")))

##GGPLOTS

soil_tidy_percent <- soil_tidy %>% 
  group_by(Question, Treatment) %>% 
  count(Response) %>% 
  summarise(total = sum(n), n = n, Response = Response, Question = Question, Treatment = Treatment) %>%
  mutate(percent = n/total)

#by percentage on y axis

soil_tidy_percent %>% 
  filter(Question %in% c("Soil nutrient levels", "pH")) %>% 
  group_by(Treatment) %>% 
  ggplot(aes(x = Response, y = percent, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = -90, hjust = 0))+
  scale_y_continuous(n.breaks = 10) +
  facet_wrap(~Question)
soil_tidy_percent %>% 
  filter(Question %in% c("Erosion", "# of soil animal types")) %>% 
  group_by(Treatment) %>% 
  ggplot(aes(x = Response, y = percent, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = -90, hjust = 0))+
  scale_y_continuous(n.breaks = 10) +
  facet_wrap(~Question)

soils_plot <- soil_tidy_percent %>% 
  filter(Question %in% c("Erosion", "# of soil animal types","Soil nutrient levels", "pH")) %>% 
  mutate(Question = as.character(Question)) %>% 
  mutate(Question = ifelse(Question == "# of soil animal types", "Soil animal types (#)", Question)) %>% 
  mutate(Question = factor(Question, c("Erosion", "Soil animal types (#)", "pH", "Soil nutrient levels"))) %>% 
  arrange(Question) %>% 
  filter(Treatment == "Managed") %>% 
  group_by(Treatment) %>% 
  ggplot(aes(x = Response, y = percent, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_grey()+
  scale_y_continuous(n.breaks = 6, limits = c(0,1), label = percent) +
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_blank(),
        # axis.text = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = -90, 
                                   vjust = 0.1,
                                   # face = c('plain', 'plain', 'bold', 'plain', 
                                   #          'bold', 'plain', 'plain','plain',
                                   #          'plain', 'plain', 'plain', 'bold',
                                   #          'plain', 'plain', 'plain', 'bold')
                                            ))+
  facet_wrap(~Question, nrow = 1)

soils_plot 
ggsave("Figures/soils_plot.png", plot = soils_plot, height = 2.5, width = 6)


library(tidyverse)
library(magick)
img1 <- image_read("Figures/Soils_blank.png") %>% 
  image_annotate("(A)", size = 80, location = "+240+0") %>% #location = "-350+0"
  image_annotate("(B)", size = 80, location = "+990+0") %>% 
  image_annotate("(C)", size = 80, location = "+1730+0") %>% 
  image_annotate("(D)", size = 80, location = "+2465+0") %>% 
  image_scale("x100")

img1
img2 <- image_read("Figures/soils_plot.png") %>% 
  image_scale("x1000")

image_append(c(img1, img2), stack = TRUE) %>% 
  image_write(path = "Figures/soils_plot_final.png", format = "png")

#by count (n) on y axis


soil_tidy %>% 
  filter(Question %in% c("Soil nutrient levels", "pH")) %>% 
  group_by(Treatment) %>% 
  ggplot(aes(x = Response, fill = Treatment)) +
  geom_histogram(stat = "count", position = "dodge")+
  theme(axis.text.x = element_text(angle = -90, hjust = 0))+
  facet_wrap(~Question)

soil_tidy %>% 
  filter(Question %in% c("Erosion", "# of soil animal types")) %>% 
  filter(Treatment == "Nonmanaged") %>% 
  group_by(Treatment) %>% 
  ggplot(aes(x = Response, fill = Response)) +
  geom_histogram(stat = "count", position = "dodge")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0),
        text=element_text(family="sans"))+
  ylab("n")+
  facet_wrap(~Question)

soil_tidy %>% 
  filter(Question %in% c("Soil nutrient levels", "pH")) %>% 
  filter(Treatment == "Nonmanaged") %>% 
  group_by(Treatment) %>% 
  ggplot(aes(x = Response, fill = Response)) +
  geom_histogram(stat = "count", position = "dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = -90, hjust = 0),
        text=element_text(family="sans"))+
  ylab("n")+
  facet_wrap(~Question)

soil_tidy %>% 
  filter(Question %in% c("Erosion", "# of soil animal types","Soil nutrient levels", "pH")) %>% 
  filter(Treatment == "Nonmanaged") %>% 
  group_by(Treatment) %>% 
  ggplot(aes(x = Response, fill = Response)) +
  geom_histogram(stat = "count", position = "dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = -90, hjust = 0),
        text=element_text(family="sans"))+
  ylab("n")+
  facet_wrap(~Question, nrow = 1)

soil_tidy %>% 
  filter(Question %in% c("Erosion", "# of soil animal types","Soil nutrient levels", "pH")) %>% 
  group_by(Treatment) %>% 
  ggplot(aes(x = Response, fill = Treatment)) +
  scale_fill_grey()+
  geom_histogram(stat = "count", position = "dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = -90, hjust = 0),
        text=element_text(family="sans"))+
  ylab("n")+
  facet_wrap(~Question, nrow = 1)

soils_plot <- soil_tidy %>% 
  filter(Question %in% c("Erosion", "# of soil animal types","Soil nutrient levels", "pH")) %>% 
  filter(Treatment == "Managed") %>% 
  group_by(Treatment) %>% 
  ggplot(aes(x = Response, fill = Response)) +
  scale_fill_grey()+
  geom_histogram(stat = "count", position = "dodge", color = "grey28")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = -90, vjust = 0.1),
        axis.title = element_blank(),
        text=element_text(family="sans"),
        legend.position = "none")+
  # ylab("n")+
  scale_y_continuous(breaks = c(seq(from = 0, to = 14, by = 2)),
                     limits = c(0,14),
                     expand = c(0, 0)) +
  facet_wrap(~Question, nrow = 1)

# ggsave("Figures/soils_plot.png", plot = soils_plot, height = 2.5, width = 6)

##Tables

soil_M <- soil_tidy %>% 
  filter(Treatment == "Managed") %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  rename("Managed" = n) %>% 
  group_by(Question) %>% 
  mutate(n = sum(Managed))%>% 
  mutate(Managed = paste0(Managed, " (", round(Managed/n*100, 2), "%)"))%>% 
  select(-n) %>% 
  pivot_wider(names_from = Response, values_from = Managed) %>% 
  mutate(Treatment = "Managed") %>% 
  select(Treatment, everything())

soil_NM <- soil_tidy %>% 
  filter(Treatment == "Nonmanaged") %>% 
  group_by(Question) %>% 
  count(Response)%>% 
  rename("Nonmanaged" = n)%>% 
  group_by(Question) %>% 
  mutate(n = sum(Nonmanaged))%>% 
  mutate(Nonmanaged = paste0(Nonmanaged, " (", round(Nonmanaged/n*100, 2), "%)"))%>%
  select(-n) %>% 
  pivot_wider(names_from = Response, values_from = Nonmanaged) %>% 
  mutate(Treatment = "Nonmanaged") %>% 
  select(Treatment, everything())

soil_table <- soil_M %>% 
  rbind(soil_NM) %>% 
  mutate(Treatment = factor(Treatment, c("Managed", "Nonmanaged"))) %>% 
  arrange(Question, Treatment) %>% 
  select(Treatment, Question, `NA/Unknown`, Decreased, `Not changed`, Increased) %>% 
  mutate(Decreased = ifelse(is.na(Decreased), "0", Decreased),
         `Not changed` = ifelse(is.na(`Not changed`), "0", `Not changed`),
         Increased = ifelse(is.na(Increased), "0", Increased),
         `NA/Unknown` = ifelse(is.na(`NA/Unknown`), "0", `NA/Unknown`))%>% 
  write_csv("Figures/soils_table_raw.csv")

soil_M_2 <- soil_tidy %>% 
  filter(Treatment == "Managed") %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  rename("Managed" = n) %>% 
  pivot_wider(names_from = Response, values_from = Managed) %>% 
  mutate(Treatment = "Managed") %>% 
  select(Treatment, everything())
soil_NM_2 <- soil_tidy %>% 
  filter(Treatment == "Nonmanaged") %>% 
  group_by(Question) %>% 
  count(Response)%>% 
  rename("Nonmanaged" = n)%>% 
  pivot_wider(names_from = Response, values_from = Nonmanaged) %>% 
  mutate(Treatment = "Nonmanaged") %>% 
  select(Treatment, everything())
soil_table_2 <- soil_M_2 %>% 
  rbind(soil_NM_2) %>% 
  mutate(Treatment = factor(Treatment, c("Managed", "Nonmanaged"))) %>% 
  arrange(Question, Treatment) %>% 
  select(Treatment, Question, `NA/Unknown`, Decreased, `Not changed`, Increased) %>% 
  mutate(Decreased = ifelse(is.na(Decreased), 0, Decreased),
         `Not changed` = ifelse(is.na(`Not changed`), 0, `Not changed`),
         Increased = ifelse(is.na(Increased), 0, Increased),
         `NA/Unknown` = ifelse(is.na(`NA/Unknown`), 0, `NA/Unknown`))  
# soil_chi_increased <- soil_table_2 %>% 
#   group_by(Question) %>% 
#   mutate(not_increased = Decreased + `Not changed` ) %>% 
#   select(Question, not_increased, Increased)
# 
# soil_chi_decreased <- soil_table_2 %>% 
#   group_by(Question) %>% 
#   mutate(not_decreased = Increased + `Not changed` ) %>% 
#   select(Treatment, Question, not_decreased, Decreased)
# 
# fisher_test(soil_chi_decreased, x =  detailed = TRUE)
#_______________________________________________________________________________________
#Vegetation
vegetation_tidy <- raw_data %>% 
  select(Q3, Q12_1:Q13_5) %>% 
  pivot_longer(Q12_1:Q13_5,names_to = "Question", values_to = "Response") %>% 
  filter(!is.na(Response)) %>% 
  mutate(Treatment = case_when(grepl("Q12", Question)~ "Managed",
                               grepl("Q13", Question)~ "Nonmanaged"))%>% 
  mutate(Question = case_when(grepl("12_1", Question)~ "Native grasses",
                              grepl("12_2", Question)~ "Shrubs",
                              grepl("12_3", Question)~ "Trees",
                              grepl("12_4", Question)~ "Weeds",
                              grepl("12_5", Question)~ "Total ground cover",
                              grepl("13_1", Question)~ "Native grasses",
                              grepl("13_2", Question)~ "Shrubs",
                              grepl("13_3", Question)~ "Trees",
                              grepl("13_4", Question)~ "Weeds",
                              grepl("13_5", Question)~ "Total ground cover")) %>%
  mutate(Question = factor(Question, c("Trees", 
                                       "Shrubs",
                                       "Native grasses",
                                       "Weeds",
                                       "Total ground cover"
  ))) %>% 
  mutate(Treatment = factor(Treatment, c("Nonmanaged", "Managed"))) %>% 
  mutate(Response = factor(Response, c("Decreased", "Not changed", "Increased", "NA/Unknown")))

vegetation_M <- vegetation_tidy %>% 
  filter(Treatment == "Managed") %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  rename("Managed" = n) %>% 
  group_by(Question) %>% 
  mutate(n = sum(Managed))%>% 
  mutate(Managed = paste0(Managed, " (", round(Managed/n*100, 2), "%)"))%>% 
  select(-n) %>% 
  pivot_wider(names_from = Response, values_from = Managed) %>% 
  mutate(Treatment = "Managed") %>% 
  select(Treatment, everything())
  
vegetation_NM <- vegetation_tidy %>% 
    filter(Treatment == "Nonmanaged") %>% 
    group_by(Question) %>% 
    count(Response)%>% 
    rename("Nonmanaged" = n)%>% 
    group_by(Question) %>% 
    mutate(n = sum(Nonmanaged))%>% 
    mutate(Nonmanaged = paste0(Nonmanaged, " (", round(Nonmanaged/n*100, 2), "%)"))%>%
    select(-n) %>% 
    pivot_wider(names_from = Response, values_from = Nonmanaged) %>% 
    mutate(Treatment = "Nonmanaged") %>% 
    select(Treatment, everything())

vegetation_table <- vegetation_M %>% 
  rbind(vegetation_NM) %>% 
  mutate(Treatment = factor(Treatment, c("Managed", "Nonmanaged"))) %>% 
  arrange(Question, Treatment) %>% 
  select(Treatment, Question, `NA/Unknown`, Decreased, `Not changed`, Increased) %>% 
  mutate(Decreased = ifelse(is.na(Decreased), "0", Decreased),
         `Not changed` = ifelse(is.na(`Not changed`), "0", `Not changed`),
         Increased = ifelse(is.na(Increased), "0", Increased),
         `NA/Unknown` = ifelse(is.na(`NA/Unknown`), "0", `NA/Unknown`))%>% 
  write_csv("Figures/vegetation_table_raw.csv")


#Only residents with direct experience (de)
vegetation_tidy_de <- vegetation_tidy %>% 
  filter(Q3 == "Yes")

vegetation_M_de <- vegetation_tidy_de %>% 
  filter(Treatment == "Managed") %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  rename("Managed" = n) %>% 
  group_by(Question) %>% 
  mutate(n = sum(Managed))%>% 
  mutate(Managed = paste0(Managed, " (", round(Managed/n*100, 2), "%)"))%>% 
  select(-n) %>% 
  pivot_wider(names_from = Response, values_from = Managed) %>% 
  mutate(Treatment = "Managed") %>% 
  select(Treatment, everything())

vegetation_NM_de <- vegetation_tidy_de %>% 
  filter(Treatment == "Nonmanaged") %>% 
  group_by(Question) %>% 
  count(Response)%>% 
  rename("Nonmanaged" = n)%>% 
  group_by(Question) %>% 
  mutate(n = sum(Nonmanaged))%>% 
  mutate(Nonmanaged = paste0(Nonmanaged, " (", round(Nonmanaged/n*100, 2), "%)"))%>%
  select(-n) %>% 
  pivot_wider(names_from = Response, values_from = Nonmanaged) %>% 
  mutate(Treatment = "Nonmanaged") %>% 
  select(Treatment, everything())

vegetation_table_de <- vegetation_M_de %>% 
  rbind(vegetation_NM_de) %>% 
  mutate(Treatment = factor(Treatment, c("Managed", "Nonmanaged"))) %>% 
  arrange(Question, Treatment) %>% 
  select(Treatment, Question, `NA/Unknown`, Decreased, `Not changed`, Increased) %>% 
  mutate(Decreased = ifelse(is.na(Decreased), "0", Decreased),
         `Not changed` = ifelse(is.na(`Not changed`), "0", `Not changed`),
         Increased = ifelse(is.na(Increased), "0", Increased),
         `NA/Unknown` = ifelse(is.na(`NA/Unknown`), "0", `NA/Unknown`))%>% 
  write_csv("Figures/vegetation_de_table_raw.csv")

#_______________________________________________________________________________________
#Compared to previous fire seasons....
fire_season_tidy <- raw_data %>% 
  select(Q22_1:Q22_4) %>% 
  pivot_longer(Q22_1:Q22_4,names_to = "Question", values_to = "Response") %>% 
  filter(!is.na(Response)) %>% 
  mutate(Question = case_when(grepl("22_1", Question)~ "Total area burned",
                              grepl("22_2", Question)~ "Intensity",
                              grepl("22_3", Question)~ "Damage to vegetation",
                              grepl("22_4", Question)~ "Damage to property and lives")) %>% 
  mutate(Question = factor(Question, c("Total area burned", 
                                       "Intensity",
                                       "Damage to vegetation",
                                       "Damage to property and lives"
  ))) %>% 
  mutate(Response = factor(Response, c("Less than typical", "Typical", "More than typical", "NA/Unknown")))

fire_season_table <- fire_season_tidy %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  group_by(Question) %>% 
  mutate(total = sum(n))%>% 
  mutate(n = paste0(n, " (", round(n/total*100, 2), "%)"))%>% 
  select(-total) %>% 
  arrange(Question, Response) %>% 
  pivot_wider(names_from = Response, values_from = n) %>% 
  select(Question, `Less than typical`, Typical, `More than typical`, `NA/Unknown`) %>% 
  write_csv("Figures/fire_season_comparison_table_raw.csv")

#_______________________________________________________________________________________
#Quantifying the knowledge gap (% Unknown people, vegetation, and soil)

knowledge_gap_answers <- raw_data %>% 
  select(Q9: Q10_2, Q12_1:Q12_5, Q14_1:Q14_10, Q22_3) %>% 
  filter(!is.na(Q9))

knowledge_gap_percent<- knowledge_gap_answers %>% 
  pivot_longer(everything(), names_to = "Question", values_to = "Response") %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  mutate(percent = n/sum(n)*100)

knowledge_gap_percent_unknown<- knowledge_gap_percent %>% 
  filter(Response == "NA/Unknown" | is.na(Response)) %>% 
  mutate(total_percent = sum(percent)) %>% 
  select(Question, total_percent) %>% 
  unique() %>% 
  mutate(Q_Type = ifelse(grepl("Q14", Question), "Soil", "Vegetation"))

knowledge_gap_percent_unknown %>% 
  group_by(Q_Type) %>% 
  count(Q_Type) %>% 
  left_join(knowledge_gap_percent_unknown) %>% 
  summarise(Mean = mean(total_percent), SE = sd(total_percent)/sqrt(n)) %>% 
  unique()



knowledge_gap_percent_known<- knowledge_gap_percent %>% 
  filter(Response != "NA/Unknown" & !is.na(Response)) %>% 
  mutate(total_percent = sum(percent)) %>% 
  select(Question, total_percent) %>% 
  unique() %>% 
  mutate(Q_Type = ifelse(grepl("Q14", Question), "Soil", "Vegetation"))

knowledge_gap_percent_known %>% 
  group_by(Q_Type) %>% 
  count(Q_Type) %>% 
  left_join(knowledge_gap_percent_known) %>% 
  summarise(Mean = mean(total_percent), SE = sd(total_percent)/sqrt(n)) %>% 
  unique()


no_opinion_answers <- raw_data %>% 
  # filter(!is.na(Q9)) %>% 
  select(Q18_1, Q18_2, Q22_4)

no_opinion_percent<- no_opinion_answers %>% 
  pivot_longer(everything(), names_to = "Question", values_to = "Response") %>% 
  group_by(Question) %>% 
  count(Response) %>% 
  mutate(percent = n/sum(n)*100)

no_opinion_percent2<- no_opinion_percent %>% 
  filter(Response %in% c("NA/Unknown", "No Opinion") | is.na(Response)) %>% 
  mutate(total_percent = sum(percent)) %>% 
  select(Question, total_percent) %>% 
  unique() %>% 
  mutate(Q_Type = "People")

no_opinion_percent2 %>% 
  group_by(Q_Type) %>% 
  count(Q_Type) %>% 
  left_join(no_opinion_percent2) %>% 
  summarise(Mean = mean(total_percent), SE = sd(total_percent)/sqrt(n)) %>% 
  unique()

opinion_percent<- no_opinion_percent %>% 
  filter(Response != "NA/Unknown" & Response != "No Opinion" & !is.na(Response)) %>% 
  mutate(total_percent = sum(percent)) %>% 
  select(Question, total_percent) %>% 
  unique() %>% 
  mutate(Q_Type = "People")

opinion_percent %>% 
  group_by(Q_Type) %>% 
  count(Q_Type) %>% 
  left_join(opinion_percent) %>% 
  summarise(Mean = mean(total_percent), SE = sd(total_percent)/sqrt(n)) %>% 
  unique()
