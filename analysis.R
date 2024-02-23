library(tidyverse)

messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

AAT_M_labels <- GAR::GAR_dict %>%
  as_tibble() %>% 
  filter(str_detect(key, "AAT")) %>% 
  filter(str_detect(key, "PROMPT")) %>% 
  filter(str_detect(key, "_M"))%>% 
  select(key, en) %>% 
  arrange(key) %>% 
  mutate(var_name = sprintf("AAT.m%02d", 1:nrow(.))) %>% 
  mutate(label = sprintf("MX.%s", en)) %>% select(-en)

AAT_AT_labels <- tibble(label = c("AT.attention_grabbing", 
                                  "AT.disturbing",
                                  "AT.noticeable",
                                  "AT.spell_casting",
                                  "AT.hard_not_to_listen",
                                  "PG.meaning", 
                                  "PG.evokes_memories",
                                  "PG.like_it",
                                  "PG.know_it",
                                  "PG.know_this_kind",
                                  "PG.makes_me_sing",
                                  "PG.relaxing",
                                  "PG.exciting",
                                  "PG.makes_me_move",
                                  "PG.invites_mind_wandering"
                                  )) %>% 
  mutate(var_name = sprintf("AAT.%s%02d", c(rep("at", 5), rep("pg", 10)), c(1:5, 1:10)))
AAT_labels_tmp <- bind_rows(AAT_M_labels, AAT_AT_labels)
AAT_labels <- AAT_labels_tmp$label %>% set_names(AAT_labels_tmp$var_name)

get_parameters <- function(data, input, keep_pseudo_na = T, var_data){
  
  vars <- c("x" = input$bv_variable1, "y" = input$bv_variable2)
  var_info1 <- var_data %>% filter(variable == input$bv_variable1)
  var_info2 <- var_data %>% filter(variable == input$bv_variable2)
  sub_type <- sprintf("%s-%s", substr(var_info1$type, 1, 3), substr(var_info2$type, 1, 3))  
  list(vars = vars, sub_type = sub_type)
}


split_multi_entry <- function(entry){
  if(length(entry) == 1){
    ret <- str_replace_all(entry, "',", "@") %>% 
      str_replace_all("'", "") %>% 
      str_split("@") %>% 
      unlist()        
  }    
  else{
    ret <- NULL
  }
  ret
}

join_rows <- function(data){
  if(is.null(data[["p_id"]])){
    return(data)
  }
  ids <- data %>% count(p_id) %>% filter(n > 1) %>% pull(p_id)
  ret <- data %>% filter(!(p_id %in% ids))
  fixed_rows <- 
    map_dfr(ids, function(i){
    tmp <- data %>% filter(p_id == i)
    completed <- which(tmp$complete == TRUE)
    if(length(completed) == 0){
      tmp  <- tmp[nrow(tmp),]   
    }
    else{
      tmp <- tmp[max(completed), ]  
    }
    tmp
  })
  ret %>% bind_rows(fixed_rows) 
}



restructe_aat_names <- function(aat_part){
  #browser()
  names(aat_part) <- names(aat_part) %>% 
    substr(9, nchar(.)) %>% 
    str_replace("\\.", "_") %>% 
    tolower() %>% 
    str_remove("_q") %>% 
    sprintf("AAT.%s", .)
  names(aat_part) <- sprintf("%s%02d", 
                             str_remove(names(aat_part), "[0-9]+$"), 
                             as.integer(str_extract(names(aat_part), "[0-9]+$")))
  return(aat_part)  
}
parse_aat <- function(entry){
  #browser()
  group <- entry$results$group
  aat <-  entry %>% pluck("AAT")   
  if(is.null(aat)){
    return(NULL)
  }
  if("AAT_stim_order_random" %in%  names(aat)){
    #browser()
  }
  else{
    return(NULL)
  }
  parts <- list(
    PG =  names(aat)[2:31][str_detect(names(aat)[2:31], "PG$")],
    M =  names(aat)[2:31][str_detect(names(aat)[2:31], "M$")],
    AT = names(aat)[2:31][str_detect(names(aat)[2:31], "AT$")])
  
  ret <- 
    map_dfc(parts, function(entries){
      map_dfr(entries, function(i){
        tmp <- aat[[i]] %>% t() %>% as.data.frame() 
        tmp <- restructe_aat_names(tmp)
        tmp[sort(names(tmp))] %>% as_tibble()
      })
    })
  ret %>% mutate(AAT.quest_type = group, 
                 stimulus = aat[["AAT_stim_order_random"]]) %>% 
    select(stimulus, AAT.quest_type, everything())
  
}
read_data <- function(result_dir = "data/from_server"){
  res_files <- list.files(result_dir, pattern = "*rds", full.names = T)
  #browser()
  
  map_dfr(res_files, function(fname){
    #browser()
    tmp <- readRDS(fname) %>% as.list()
    nms <- tmp %>% names()
    if(!("session" %in% names(tmp))){
      return(NULL)
    }
    if(!any(str_detect(nms, "AAT"))){
      return(NULL)
    }
    #file.copy(fname, to = "e:/projects/science/DOTS/development/shiny_apps/mas_monitor/data/from_server/")    
    aat <- parse_aat(tmp)  
    deg <- NULL
    if("DEG" %in% names(tmp)){
      #browser()
      deg <- tmp$DEG %>% 
        as.data.frame() %>% 
        janitor::clean_names() %>% 
        select(!starts_with("q")) %>% 
        set_names(sprintf("DEG.%s", names(.))) %>% 
        distinct() %>% 
        mutate(DEG.age = DEG.age/12)
    }
    deghi <- NULL
    if("DEGHI" %in% names(tmp)){
      #browser()
      deghi <- tmp$DEGHI %>% 
        as.data.frame() %>% 
        janitor::clean_names() %>% 
        select(!starts_with("q")) %>% 
        set_names(sprintf("DEG.%s", names(.)))
    }
    prof <- NULL
    if("PROF" %in% names(tmp)){
      #browser()
      prof <- tmp$PROF %>% 
        as.data.frame() %>% 
        janitor::clean_names() %>% 
        select(!starts_with("q")) %>% 
        set_names(sprintf("DEG.%s", names(.)))
    }
    gms <- NULL
    if("GMS" %in% names(tmp)){
      gms <- tibble(GMS.general = tmp$GMS$General)
    }
    #browser()
    session <- tmp$session[c("p_id", "time_started", "complete")] %>% as.data.frame()
    bind_cols(session, deg, deghi, prof, gms, aat)
  }) %>% 
    distinct() %>% 
    as_tibble()
}


setup_workspace <- function(results = "data/from_server"){
  #browser()
  master <- read_data(results) 
  master <- master %>% mutate(age = round(DEG.age/12), 
                              gender = factor(DEG.gender, 
                                              levels = 1:4, 
                                              labels = c("female", "male", "other", "rather not say"))) 
  names(master)[names(master) %in% names(AAT_labels)] <- AAT_labels[names(master)[names(master) %in% names(AAT_labels)]]
  assign("master", master, globalenv())
  invisible(master)
}

get_correlations <- function(data, var_x, var_y, method = "pearson"){
  f <- as.formula(sprintf("~ %s + %s", var_x, var_y))
  ct <- cor.test(f, data = data, method = method)
  return(ct %>% broom::tidy())     
}


beta_plot <- function(lm_model, order_by_size = F){
  if("lm" %in% class(lm_model)){
    lm_model <- lm_model %>% 
      broom::tidy()
  }
  lm_model <- lm_model %>% 
    filter(term != "(Intercept)") %>% 
    mutate(y_min = estimate  - 1.96*std.error, y_max = estimate  + 1.96*std.error, 
           sig = y_min > 0 | y_max < 0)
  
  if(order_by_size) 
    lm_model <- lm_model %>% mutate(term = factor(term) %>% fct_reorder(estimate, mean))
  if("N" %in%  names(lm_model)){
    q <- lm_model %>% 
      mutate(N_fact = factor(N)) %>% 
      ggplot(aes(x = term, y = estimate, colour = sig, group = N_fact)) 
    q <- q + geom_point(shape = 2, size = 2, position = position_dodge(width = 1)) 
    q <- q + geom_linerange(aes(ymin = y_min, ymax = y_max, colour = sig, group = N_fact), position = position_dodge(width = 1))
    q <- q + geom_text(aes(y = 2, x = 10 * (N - min(N))/max(N) + 2, label = sprintf("N = %s", N)), 
                       size = 3, colour ="black")                            
    q <- q + ylim(-1, 1)
  }
  else{
    q <- lm_model %>% ggplot(aes(x = term, y = estimate )) 
    q <- q + geom_point(shape = 2, size = 2, color = def_colour1) 
    q <- q + geom_linerange(aes(ymin = y_min, ymax = y_max))
  }
  q <- q + coord_flip()  
  q <- q + geom_hline(yintercept = 0, linetype = "dashed")
  q <- q + theme(legend.position = "none")
  q
}

get_model <- function(data, dv = "SRS.perc_correct", predictors = num_predictors, output_format = "raw", ...){
  output_format <- match.arg(output_format, c("raw","summary", "glance", "tidy", "sj", "jtools_tab", "jtools_plot"))
  predictors <- setdiff(predictors, dv)
  data <- data %>% select(all_of(c(dv, predictors))) %>%  mutate_if(is.numeric, scale)
  f <- sprintf("%s ~ .", dv) %>% as.formula()                      
  lm_tab <- lm(f, data = data)
  if(output_format == "summary"){
    lm_tab <- lm_tab %>% 
      summary()
  }
  if(output_format == "tidy"){
    lm_tab <- lm_tab %>% 
      broom::tidy()
  }
  if(output_format == "glance"){
    lm_tab <- lm_tab %>% 
      broom::glance()
  }
  if(output_format == "sj"){
    lm_tab <- lm_tab %>% 
      sjPlot::tab_model(...)
  }
  if(output_format == "jtools_tab"){
    lm_tab <- lm_tab %>% 
      jtools::summ(...)
  }
  if(output_format == "jtools_plot"){
    lm_tab <- lm_tab %>% 
      jtools::plot_summs(scale = T,  ...)
  }
  lm_tab
}
