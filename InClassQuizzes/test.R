# Read in data
dird="~/Desktop/MainFolder/OuClasses/Spring 2023/Applied Statistical Methods/FALL224753wise0046/CourseData/Data-for-the-course/K25936_Downloads/Excel/"
library(readxl)

files = list.files(dird)

### Important Functions
myconvert = function(xl) {
  if(stringr::str_ends(xl, "XLS") | stringr::str_ends(xl, "xls")){
    v=try(readxl::read_xls(paste0(dird, xl)), silent = TRUE)
  }
  else{
    v = NA
  }
  v
}



# eggs <- myconvert("EGGS.XLS")
# head(eggs)
