#' x = NESTS(DB = yr2dbnam(2024), .refdate = "2024-04-25")
#' x = NESTS()
#' z = extract_TODO(x)
extract_TODO <- function(x) {

  o = x[ !nest_state %in% c("P", "D", "notA")]
  
  # TODO: add todo rules to ui

  # CATCH
    #! RULES
      #! min_days_to_hatch <= 14
      #! nest_state != 'H'
      #TODO: catch or caching attempt with one day break (use NESTS.trap_on) 
      #TODO: hatch_state does not contain S, C, CC 

    # male
    cm = o[min_days_to_hatch <= 14 & nest_state != 'H', .(nest, M_cap, M_nest)]
    cm[is.na(M_cap) , todo := "catch M"]
    cm[!is.na(M_nest), todo := NA]
    cm = unique(cm)

    # female
    cf = o[min_days_to_hatch <= 14 & nest_state != 'H', .(nest, F_cap, F_nest)]
    cf[is.na(F_cap) , todo := "catch F"]
    cf[!is.na(F_nest), todo := NA]
    cf = unique(cf)

    catch = merge(cm, cf, by = 'nest' )
    catch[, todo := paste(c(todo.x, todo.y) |> na.omit(), collapse = ", "), by = .I]
    catch = catch[nchar(todo) > 0, .(nest, todo)]

    catch[, .N, todo]
    catch[todo == 'catch M']

  # NESTS CHECK
    #! RULES
      #! if last check >=7
      #! if nest_state = pD, pP

    nc = o[
      lastCheck >= 7 |
        nest_state %in% c("pD", "pP"),
      .(nest, todo = "nest check")
    ] |> unique()


  # HATCH CHECK
    #! RULES
      #! if min_days_to_hatch <= 4
      #TODO: if eggs show no signs of hatch then do not check next day but in 2 days 
      #TODO: if not all chicks hatched (using, hatch_state, brood_size, clutch_size)
      
    hc = o[
      ! collected & 
      min_days_to_hatch <= 4,
      .(nest, todo_hc = "hatch check")
    ] |> unique()
    
    cc = merge(nc, hc, by = "nest", all = TRUE)
    cc[!is.na(todo_hc), todo := todo_hc]
    cc[, todo_hc := NULL]

    
  # CHICK PROCESSING
    #! RULES
      #TODO: if hatch_state contains CC, C then go process chicks. 

  # prepare final set
  ll = o[,.(nest, lat,lon)] 
  out = merge(catch, cc, all = TRUE, suffixes = c("", "_x"))

  out[, todo := paste(c(todo, todo_x) |> na.omit(), collapse = ", "), by = .I]
  out[, todo_x := NULL]


  out = merge(out, ll, by = "nest", all = TRUE)



  os = o[, .(nest, last_check_days_ago = lastCheck, last_clutch, last_brood = NA, last_state = nest_state, min_days_to_hatch)]
  out = merge(os, out, by = "nest", all.x = TRUE)
  setcolorder(out, "todo", after = "nest")
  
    
  setorder(out, todo, na.last=TRUE)
  out


}