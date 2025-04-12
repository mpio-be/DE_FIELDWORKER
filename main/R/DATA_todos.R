#' x = NESTS(DB = yr2dbnam(2024), .refdate = "2024-04-25")
#' x = NESTS()
#' z = extract_TODO(x)
extract_TODO <- function(x) {

  #! ALL_RULES
  # "catch M", "catch F", "nest check", "hatch check" # and combinations of it


  o = x[!nest_state %in% c("P", "D", "notA")]
  o[, last_trap := difftime(as.Date( input$refdate), as.Date(trap_on_date), units = "days")|>as.numeric()]
  
  # TODO: add todo rules listing to ui

  # CATCH
    #! RULES
      #! min_days_to_hatch <= 14
      #! nest_state != 'H'
      #! catch or caching attempt with one day break 
      #TODO: hatch_state does not contain S, C, CC 

    # male
    cm = o[min_days_to_hatch <= 14 & nest_state != 'H', .(nest, M_cap, M_nest, last_trap)]
    cm[is.na(M_cap), todo_catch := "catch M"]
    cm[last_trap == 0, todo_catch := NA]
    cm[!is.na(M_nest), todo_catch := NA]
    cm = unique(cm)

    # female
    cf = o[min_days_to_hatch <= 14 & nest_state != 'H', .(nest, F_cap, F_nest,last_trap)]
    cf[is.na(F_cap), todo_catch := "catch F"]
    cf[last_trap == 0, todo_catch := NA]
    cf[!is.na(F_nest), todo_catch := NA]
    cf = unique(cf)

    catch = merge(cm, cf, by = 'nest' )
    catch[, todo_catch := paste(c(todo_catch.x, todo_catch.y) |> na.omit(), collapse = ", ") , by = .I]
    catch = catch[nchar(todo_catch) > 0, .(nest, todo_catch)]
    catch[todo_catch == 'catch M, catch F', todo_catch := "catch any"]

  # NESTS CHECK
    #! RULES
      #! if last check >=7
      #! if nest_state = pD, pP

    nc = o[
      lastCheck >= 7 |
        nest_state %in% c("pD", "pP"),
      .(nest, todo_check = "nest check")
    ] |> unique()


  # HATCH CHECK
    #! RULES
      #! if min_days_to_hatch <= 4
      #TODO: if eggs show no signs of hatch then do not check next day but in 2 days 
      #TODO: if not all chicks hatched (using, hatch_state, brood_size, clutch_size)
      
    hc = o[
      ! collected & 
      min_days_to_hatch <= 4,
      .(nest, todo_check = "hatch check")
    ] |> unique()
    
    check = merge(nc, hc, by = "nest", all = TRUE, suffixes = c("", "_hatch"))
    check[!is.na(todo_check_hatch), todo_check := todo_check_hatch]
    check = check[nchar(todo_check) > 0, .(nest, todo_check)]

    
  # CHICK PROCESSING
    #! RULES
      #TODO: if hatch_state contains CC, C then go process chicks. 

  # prepare final set
  ll = 
  out = merge(catch, check, all = TRUE, , by = "nest")


  out = merge(out, o[,.(nest, lat,lon)] , by = "nest", all = TRUE)



  os = o[, .(nest, last_check_days_ago = lastCheck, last_clutch, last_brood = NA, last_state = nest_state, min_days_to_hatch)]
  out = merge(os, out, by = "nest", all.x = TRUE)
  setcolorder(out, c("todo_catch", "todo_check"), after = "nest")
  
    
  setorder(out, todo_catch, todo_check, na.last=TRUE)
  out


}