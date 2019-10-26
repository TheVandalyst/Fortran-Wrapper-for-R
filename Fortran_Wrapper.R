library(rlang)
Fortran_Execute <- function(Fortran_Code)
{
  Fortran_Code_per_Line <- str_split(Fortran_Code, "\n")[[1]]
  routine_name <- Fortran_Code_per_Line[1]
  routine_name
  routine_name <- str_split_fixed(routine_name, " ",3)[2]
  routine_name <- trimws(routine_name)
  routine_name <- gsub("\\s*\\([^\\)]+\\)","",routine_name)
  Variable_def <- str_split_fixed(Fortran_Code_per_Line[1], "[(]",2)[2]
  Variable_def <- str_remove_all(string = Variable_def, pattern = "[)]")
  Variable_def
  Variable_def <- unlist(stringr::str_split(Variable_def,","))
  Variable_def
  Variable_def <- trimws(Variable_def)
  Variable_def
  
  location_of_types <- grep("integer|double",Fortran_Code_per_Line)
  input_Variables <- Variable_def
  
  for(i in 1:length(location_of_types))
  { #i=3
    Variable_def_type <- str_remove_all(str_split(Fortran_Code_per_Line[location_of_types[i]], " ", simplify = TRUE), ",")
    Variable_def_type <- gsub("\\s*\\([^\\)]+\\)","",Variable_def_type)
    Locations <- match(Variable_def_type,input_Variables)[!is.na(match(Variable_def_type,input_Variables))]
    input_Variables[Locations] <- Variable_def_type[1]
  }
  input_Variables
  
  
  current <- getwd()
  setwd("~/")
  writeLines(Fortran_Code, paste0(routine_name,".f90"))
  system(paste0("gfortran -c ",routine_name,".f90"))
  system(paste0("gfortran -shared -o ",routine_name,".so ",routine_name,".o"))
  dyn.load(paste0(routine_name,".so"))
  setwd(current)
  #if(length(Variable_def))
  #{
    #.Fortran("facto",n=as.integer(5),answer=as.integer(1))
  #  Function_Path <- paste0(".Fortran(", "'",routine_name,"'", ")")
  #}
  for(i in 1:length(input_Variables)){
    if(i == 1){
      Variables <- paste0("alist(num",i,"=")
      Function_Path <- 
        paste0(".Fortran(", "'",routine_name,"'",",",Variable_def[i],"=as.", input_Variables[i], "(num",i,")")
    }else{
      Function_Path <- paste0(Function_Path,",",Variable_def[i],"=as.", input_Variables[i], "(num",i,")")
      Variables <- paste0(Variables,",","num",i,"=")
    }
  }
  Function_Path <- paste0(Function_Path, ")")
  Variables <-  paste0(Variables, ")")
  #.Fortran("facto",n=as.integer(5),answer=as.integer(1))
  #Function_Path <- paste0(".Fortran(", "'",routine_name,"'", ")")
  #assign(routine_name,function()
  #{
  #  eval(parse(text = Function_Path))
  #}, envir = .GlobalEnv)
  #return(paste0(".Fortran(", "'",routine_name,"'", ")"))
  assign(routine_name, envir = .GlobalEnv,
         new_function(eval(parse(text = Variables)), quote(eval(parse(text = Function_Path)))))
  return(Function_Path)
}
Fortran_Execute(Fortran_Code)

facto(5,1)
