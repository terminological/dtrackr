## code to prepare `ILPD` dataset goes here

ILPD = readr::read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv",
                       col_names = c("Age","Gender","Total_Bilirubin","Direct_Bilirubin","Alkaline_Phosphatase",
                                     "Alamine_Aminotransferase","Aspartate_Aminotransferase","Total_Protein","Albumin",
                                     "Albumin_Globulin_Ratio","Case_or_Control"))
ILPD = ILPD %>% mutate(
  Case_or_Control = ordered(Case_or_Control,labels=c("case","control")),
  Gender = as.factor(Gender)
)

usethis::use_data(ILPD, overwrite = TRUE)
