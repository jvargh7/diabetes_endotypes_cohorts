

data_extract <- function(study_name, vl_column,data_path,df_name = character()){
  
  if(identical(df_name,character(0))){
    df_name = vl_column
  }
  
  
  if(Sys.info()["user"] == "JVARGH7"){
    vl_sheet = readxl::read_excel("data/Phenotypes Variable List.xlsx",sheet = study_name)
  }
  
  if(Sys.info()["user"] == "zhongyuli"){
    vl_sheet = readxl::read_excel("/Users/zhongyuli/Library/CloudStorage/OneDrive-EmoryUniversity/code_files/git/de_repo/diabetes_endotypes_cohorts/data/Phenotypes Variable List.xlsx",
                                  sheet=study_name)
  }
  
  if(Sys.info()["user"] == "JGUO258"){
    vl_sheet = readxl::read_excel("data/Phenotypes Variable List.xlsx",sheet = study_name)
  }
  
  var_names <- vl_sheet %>% 
    dplyr::select(label,harmonized,one_of(vl_column)) %>% 
    rename_at(vars(one_of(vl_column)),~"selected") %>% 
    dplyr::filter(!is.na(selected))
  
  names_to_upper <- c("DPPOS","DPP","JHS","SEARCH","TODAY","ARIC","BHS","CARDIA")
  
  if(study_name %in% names_to_upper){
    
    var_names$selected <- str_to_upper(var_names$selected)
    
    df <- read_csv(paste0(data_path,"/",df_name,".csv")) %>% 
      rename_all(~str_to_upper(.)) %>% 
      dplyr::select(one_of(var_names$selected)) 
  }
  if(!study_name %in% names_to_upper){
      df <- read_csv(paste0(data_path,"/",df_name,".csv")) %>% 
        dplyr::select(one_of(var_names$selected)) 
  
  }
  
  
  harmonized_vars <- var_names %>% 
    dplyr::filter(selected %in% names(df))
  
  df_cleaned <- df %>% 
    rename_at(vars(one_of(harmonized_vars$selected)),~harmonized_vars$harmonized)
  
  return(df_cleaned)
  
  
}
