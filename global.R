icon <- function (name, class = NULL, lib = "font-awesome", type = "solid", ...) 
{
  prefixes <- list(`font-awesome` = "fa", glyphicon = "glyphicon")
  fa_extras <- list(solid = "s", regular = "r", brand = "b", old = "")
  
  prefix <- prefixes[[lib]]
  fa_extra <- fa_extras[[type]]
  
  if (is.null(prefix)) {
    stop("Unknown font library '", lib, "' specified. Must be one of ", 
         paste0("\"", names(prefixes), "\"", collapse = ", "))
  }
  
  iconClass <- ""
  if (!is.null(name)) 
    iconClass <- paste0(prefix, fa_extra, " ", prefix, "-", name)
  if (!is.null(class)) 
    iconClass <- paste(iconClass, class)
  
  iconTag <- tags$i(class = iconClass, ...)
  
  iconTag
}