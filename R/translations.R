translate <- function(lang, key) {
# JULIA 18/11/2023
#  translations %>%  
#    filter(en == {{ key }}) %>% 
#    pull({{ lang }})
  if (lang=="es")
    return(translations[translations$en==key,]$es)
  if (lang=="ca")
    return(translations[translations$en==key,]$ca)
  return(key)
}

show_translated_notice <- function(lang, text) {
  showModal(modalDialog(
    title = translate(lang,"Information"),
    translate(lang, text),
    footer = modalButton("Ok")
  ))
}
