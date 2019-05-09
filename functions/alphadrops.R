
alphadrop = function(a, minrdrop = .2) {
  drop = a$item.stats; 
  rdrop = drop$r.drop
  nitem = row.names(drop)
  drop = nitem %>% as_tibble() %>% dplyr::rename(nitem = value) %>% mutate(rdrop = rdrop) %>% arrange(rdrop) %>% filter(rdrop < minrdrop)
  # drop = data.frame(cbind(nitem, rdrop));
  # drop = drop[order(drop$rdrop),]
  # 
  return(drop)
}


# alphadrop.me function ---------------------------------------------------

alphadrop.me = function(df, name_prefix, minrdrop) {
  # crear var. con min. rdrop
  min_rdrop = minrdrop
  
  # limpiar df, dejar solo items
  temp2 = df %>%
    dplyr::select(contains(name_prefix), -contains("Time"))
  # dplyr::select(matches(name_prefix), ignore.case = F)
  
  # aplicar funcion alpha sobre df
  a = psych::alpha(temp2)
  
  # Alphadrop section ******************************************************
  # check if alphadrop function exist.
  ifelse(
    
    # check if alphadrop exists
    exists("alphadrop", where = .GlobalEnv), 
    
    # mensaje si la funcion ya esta definida
    "alphadrop function exists",
    
    # define funcion si no existe
    (alphadrop = function(a) {
      drop = a$item.stats; 
      rdrop = drop$r.drop
      nitem = row.names(drop)
      drop = nitem %>% 
        as_tibble() %>% 
        dplyr::rename(nitem = value) %>% 
        mutate(rdrop = rdrop) %>% 
        arrange(rdrop)
      
      return(drop)
    })
    
  )
  
  # Asignar funcion alphadrop a global enviroment
  assign("alphadrop", alphadrop, envir = .GlobalEnv)
  
  # aplicar funcion alphadrop sobre a
  alpha_drop = alphadrop(a)
  
  # ******************************************************
  
  # Seleccionar items con rdrop bajo minimo
  alpha_drop = alpha_drop %>% 
    filter(rdrop <= min_rdrop)
  
  # extraer nombre de items seleccionados
  drop_items = alpha_drop$nitem
  # guardar mensaje para print final 
  items_mess = paste("Se eliminan los siguientes items: ", paste(drop_items, collapse = " "))
  
  # seleccionar items que no se eliminaran
  drop_items = names(temp2[,!(names(temp2) %in% drop_items)])
  
  # extraer valor alpha raw para print final
  alpha_total = as.tibble(a$total)
  old_alpha = paste0("alpha antes de eliminar items: ", alpha_total$raw_alpha)
  
  # Eliminamos items bajo min. rdrop del df
  a = as.tibble(temp2) %>% 
    dplyr::select_(.dots = drop_items)
  
  # aplicamos alpha a nuevo df con menos items 
  a = alpha(a)
  
  # extraer valor alpha raw para print final
  alpha_total_new = as.tibble(a$total)
  new_alpha = paste0("alpha antes de eliminar items: ", alpha_total_new$raw_alpha)
  
  # Crear nuevo df sin items eliminados
  temp_new = as.tibble(temp2) %>% 
    dplyr::select_(.dots = drop_items)
  
  # asignar nuevo df a Global enviroment
  assign("temp_new", temp_new, envir = .GlobalEnv)
  
  # Print final: items a eliminar, nombre de nuevo df, alpha antes de eliminar y despues de eliminar
  cat("\n\n**************************************", "\n", items_mess, "\n", "New dataframe: temp_new \n", old_alpha, "\n", new_alpha, "\n**************************************")
  
  
}