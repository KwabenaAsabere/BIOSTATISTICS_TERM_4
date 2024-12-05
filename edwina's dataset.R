data("diamonds")
head(diamonds)
diamonds

diamonds %>% distinct() %>% 
  add_count(cut) %>% 
  arrange(-n)


diamonds %>% distinct() %>% 
  group_by(cut) %>% 
  count()


library(readxl)

edwina <- read_excel("Book5.xlsx")
head(edwina)

edwina_wide <- edwina %>% na.omit() %>% 
  pivot_wider(id_cols = c(ORG_CODE,COUNTY),
              names_from = GENDER,
              values_from = Gr.10) 

edwina_2 <- read_csv("District-GradeRace.csv")
edwina_wide2 <- edwina_2 %>% 
  filter(GRADE == "Gr.10")

edwina_merge1 <- edwina_full_wide %>%
  mutate(ORG_CODE = as.character(ORG_CODE)) %>% 
  full_join(edwina_wide2, by = "ORG_CODE")



edwina_full <- edwina_full %>% 
  select(c(ORG_CODE:GENDER,Gr.10))

edwina_income <- read_excel("MEDIAN HOUSEHOLD INCOME VARIABLE POP AND BMI DATA.xlsx")

edwina_income %>% 
  distinct(Geography) %>% 
  count()


write.csv(edwina_merge1,"edwina_merged.csv",row.names = TRUE)  


edwina_again<- edwina_income %>%
  mutate(Geography = gsub("School District, MA", "", Geography))

edwina_again %>% 
  count(Geography) %>% 
  arrange(-n)

edwina_merge1 %>% 
  inner_join()



edwina_merge <- edwina_merge %>% 
  select(c(ORG_CODE,FEMALE:TOTAL,District_Total:Geography))

edwina_merged_again <- edwina_merge1 %>% 
  full_join(edwina_again,by = "Geography")
  



















































































