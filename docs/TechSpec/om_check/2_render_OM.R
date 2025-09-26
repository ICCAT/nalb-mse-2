require(quarto)
require(here)
selected_om = read.csv("docs/TechSpec/tables/selected_OM.csv")
this_path = file.path(here::here(), 'docs/TechSpec/om_check')

# Render OM check html:
setwd(this_path)
for(m in 1:nrow(selected_om)) {
  output_file = paste0(paste('OM', 
                             selected_om$mod_type[m], 
                             selected_om$iter[m], sep = '_'), 
                       '.html')
  # Render document:
  quarto_render(input = 'om_check.qmd', 
                output_format = 'html',
                output_file = output_file,
                execute_params = list(selmod = as.character(selected_om$iter[m]), 
                                      typemod = selected_om$mod_type[m]))
}
setwd(here::here())
