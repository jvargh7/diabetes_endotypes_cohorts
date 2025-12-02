


sidd_coef_classifier <- readxl::read_excel(paste0(path_diabetes_endotypes_cosmos_repo,"/analysis/SIDD Classifier.xlsx"),sheet ="coefs")
# sidd_vcov_classifier <- readxl::read_excel("analysis/SIDD Classifier.xlsx",sheet="vcov") %>% 
#   rename(rownames = "...1") %>% 
#   dplyr::select(-rownames) %>% 
#   as.matrix(.)

sidd_coefs = sidd_coef_classifier$Coefficient
names(sidd_coefs) = sidd_coef_classifier$Variable



mod_coef_classifier <- readxl::read_excel(paste0(path_diabetes_endotypes_cosmos_repo,"/analysis/MOD Classifier.xlsx"),sheet ="coefs")
# mod_vcov_classifier <- readxl::read_excel("analysis/MOD Classifier.xlsx",sheet="vcov") %>% 
#   rename(rownames = "...1") %>% 
#   dplyr::select(-rownames) %>% 
#   as.matrix(.)

mod_coefs = mod_coef_classifier$Coefficient
names(mod_coefs) = mod_coef_classifier$Variable

mard_coef_classifier <- readxl::read_excel(paste0(path_diabetes_endotypes_cosmos_repo,"/analysis/MARD Classifier.xlsx"),sheet ="coefs")
# mard_vcov_classifier <- readxl::read_excel("analysis/MARD Classifier.xlsx",sheet="vcov") %>% 
#   rename(rownames = "...1") %>% 
#   dplyr::select(-rownames) %>% 
#   as.matrix(.)

mard_coefs = mard_coef_classifier$Coefficient
names(mard_coefs) = mard_coef_classifier$Variable
