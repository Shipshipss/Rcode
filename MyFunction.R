
# ggpacket ----------------------------------------------------------------
pack_geom_box <- function(...) {
  ggpacket() %+%
    geom_point(.id = c('all','point'),...) %+%
    geom_line(.id = c('all','line'),size = 0.5,alpha = 0.3,color = 'gray',...) %+%
    geom_boxplot(.id = 'box',...) 
}

pack_sig <- function(...) {
  #Same geom only use once, otherwise can't find aes
  ggpacket() %+%
    geom_line(.id = c('all','line'),color = 'black', linewidth = 0.5,alpha = 0.5,...) %+%
    geom_text(.id = c('all','text'),size =3,fontface = 'bold',vjust = 1.2,...) 
}

pack_scale <-  function(...) {
  ggpacket() %+%
    scale_x_discrete(.id = c('x', 'all'), ...)%+%
    scale_color_brewer(.id = c('color', 'all'), ...)%+%
    scale_fill_brewer(.id = c('fill', 'all'), ...)
}

pack_theme <-  function(...) {
  ggpacket() %+%
    theme(.id = 'theme',
          
          strip.placement = 'outside',
          strip.background = element_blank(),
          strip.text.x = element_text(face = 'bold',size = 10),
          panel.spacing.x = grid::unit(0.5,'lines'),
          panel.spacing.y = grid::unit(0.5,'lines'),
          
          legend.position = 'bottom',
          
          axis.title = element_text(face = 'bold',size = 13),
          plot.title = element_text(face = 'bold',size = 16),
          ...)
}



# lmer model --------------------------------------------------------------


#lmer multiple datasets
Fun_lme <- function(DF,dv,time) {
  
  #need input: after rbindlist total data & Model vars(DV) 
  
  
  
  
  .[,x:= map_depth(pred,2,'x')] %>% 
    .[,predicted:= map_depth(pred,2,'predicted')] 
  
  useto_plot <- model_result %>% 
    .[,unnest(.SD),.SDcols = c("vars", "p", "p.adj", "x", "predicted"),by = .(Label)]
  useto_plot[,x.lab := map(x,~f2n(.x,level = time))][
    ,c('t1','t2'):=transpose(x.lab)][
      ,c('pred1','pred2'):=transpose(predicted)][
        ,c('x.m','pred.m'):=map(.SD,\(x) map_dbl(x,mean)),.SDcols = c('x.lab','predicted')][
          ,c('pred.m','p.adj'):=map(.SD,\(x) map_dbl(x,~round(.x,3))),.SDcols = c('pred.m','p.adj')][
            ,sig := fcase(                               # add sig star
              p.adj < 0.05 & p.adj > 0.01, '*',
              p.adj < 0.01 & p.adj > 0.001, '**',
              p.adj < 0.001,'***',
              default = 'ns')][
                ,issig := sig != 'ns']
  predline <- useto_plot[,map(.SD,unlist),.SDcols = c('x.lab','predicted'),by = .(Label,vars)] 
  predline %>% setkey(Label)
  
  siglabel <- useto_plot[,.(Label,vars,x.m,pred.m,p.adj,sig,issig)] 
  
  return(list('model' = model_result,'usetoplot' = useto_plot,'predline' = predline,'siglabel' = siglabel))
}
get_model_result <- function(model) {
  
  tidymod <- broom.mixed::tidy(model, effects = "fixed")
  pred <- ggeffects::ggpredict(model,terms = c('Session'))
  correc <- emmeans(model,pairwise ~ Session,
                    adjust = 'bonferroni')$contrasts %>% tidy()
  
  return(list('summary' = tidymod,'pred'=pred,'correc'=correc))
}


f2n <- function(x,level) {
  factor(x,levels = level) %>%
    as.numeric()
  # another way to direct convert number factor to numerical
  # as.character(x) %>%
  #  as.numeric()
}
fun_boxplot <- function(test,var,timelabel) {
  
  test %$%  # magrittr:: Expose the names in lhs to the rhs expression
    {                       # %$% pass on to all layers using {}
      
      ggplot(t_data[var],aes(Session,value))+
        #Do not need D_ttest$t_data,if use %$%
        
        pack_geom_box(line.mapping = aes(group = Number),
                      box.mapping = aes(fill = Session)) %+% #ggplot:: expose
        pack_sig(text.data = t_test[var], 
                 #dont need to D_ttest$t_test,if use %$%
                 
                 text.mapping = aes(x = 1.5,y=Inf,label = str_c('p = ',p.adj)))+
        facet_wrap(~vars,scales = 'free_y')+
        scale_x_discrete(labels = timelabel)+
        scale_fill_brewer(palette = 'Set1',labels = timelabel)+
        # pack_scale(all.labels = c(timelabel),
        #            fill.palette = 'Set1')+
        theme_bw()+theme(axis.title = element_blank())+
        #  labs(title = 'Significant variable PLOT on paired t-test',
        #       subtitle = 'N = 19 (F:10 / M:9)')+
        pack_theme(theme.legend.position='none')
    }
}




# analysis ----------------------------------------------------------------





find_measure <- function(data) {
  # find columns which have no NA
  # then these columns should be the measurements in this session
  map_lgl(data, \(column) !any(is.na(column))) %>%
    data[, .SD,.SDcols = .] %>% 
    names()
}
find_samesub <- function(data) {
  sub <- data[,.(sub = .(Number)),by = Label][
    ,samesub := map2(sub,dplyr::lag(sub),\(x,y) intersect(x, y))][2,unlist(samesub)]
  return(sub)
}

my_cor_pcor <- function(data,vars,control) {
  
  cor <- corr.test(data[,-'Number'])
  
  
  N =  uniqueN(data,by = 'Number')
  # corr.p may be applied to the results of partial.r if n is set to n - s 
  # (where s is the number of variables partialed out)
  pcor <- partial.r(data,c(vars),control)
  pcor.test <- corr.p(pcor,n = N,adjust = 'fdr')
  
  names <- c('cor',"pcor", "pcor.test")
  return(setNames(list(cor,pcor, pcor.test), names))
}

my_lme <- function(data,dv,p.adj.method = 'fdr') {
  #model_result
  model = my_model(data, dv)
  summary = map(model,~ tidy(., effects = 'fixed'))
  # prediction = map(model,~ggpredict(terms = c('Session')))
  
  #model_sig
  p = map(summary,list('p.value',2))
  p.adj = get_corr_p(p) %>% round(2)
  sig.adj =  map(p.adj,~ .<0.05)
  sig.vars.adj = keep(sig.adj,isTRUE) %>% names()
  
  #prediction
  
  
  names = c("model", "summary",'p',map_chr(c('p.adj.','sig.vars.'), ~ str_c(.,p.adj.method)))
  
  return(setNames(list(model, summary, p, p.adj, sig.vars.adj), names))
  
}

my_diff <- function(cleandata,vars) {
  
  
  diff <- cleandata[,map(.SD,\(x) x - shift(x) ),by = Number,.SDcols = vars] %>% 
    na.omit()
  setnames(diff,vars,str_c('Δ',vars))
  
  cleandata[,.SD,.SDcols = c('Number',con.vars)][
    diff, on = "Number", mult = "first"][,Gender:=as.numeric(Gender)]
  
  
}


my_model <- function(data,dv) {
  model <- map(dv,~lmer(paste(.x,'~ Session + (1|Number)',collapse = ' '),data)) %>% 
    set_names(dv) 
  
}
get_corr_p <- function(p,p.adj.method = 'fdr') {
  p.adjust(p,method = p.adj.method,n = length(p)) %>% 
    round(3)
}


my_ttest <- function(data,variable,ref,p.adj.method = 'fdr') {
  
  sub <- find_samesub(data)
  
  cleandata <- data[Number %in% sub]
  
  longdata <-  cleandata %>% 
    melt(id.vars = c('Label','Number','Session'),
         measure.vars = variable,
         variable.name = 'vars') %>% 
    setDT(key = 'vars')
  
  stat.test <- longdata %>%
    group_by(vars) %>%
    t_test(value ~ Label, paired = TRUE,ref.group = ref) %>%
    adjust_pvalue(method = p.adj.method) %>%
    add_significance() %>% 
    setDT(key = 'vars') %>% 
    modify_if(is.numeric,\(x) round(x,3)) 
  
  #correction
  corre.met <- p.adj.method
  sig.vars <- stat.test[p < 0.05,vars] %>% as.character()
  sig.vars.cor <- stat.test[p.adj.signif !='ns',vars] %>% as.character()
  
  # c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
  #   "fdr", "none")

  # tabledata <- cleandata[,.SD,.SDcols = c('Number','Session',variable)][
  #                    ,Session := as.character(Session)]
  # table = my_t_table(tabledata)
  
  names <-  c('sub','cleandata',"longdata", "stat.test", 
              "sig.vars", str_c('sig.vars.',p.adj.method),'corre.met')
  # dont forget to add names and return to save tbl or plot in ttest_result
  return(setNames(list(sub, cleandata, longdata, stat.test, sig.vars, sig.vars.cor,corre.met),names))
  
}

set_dtlist_name <- function(data,label) {
  # set label to factor but 
  # data[['label']] returns a factor with levels, and not a character 
  # so create new label
  
  label <- data[[label]] %>% as.character()
  
  data[,map(.SD,as.list)] %>% names() %>% 
    map(~with(data, get(.x))) %>% 
    map(~setattr(.x,'names',label )) %>% 
    unlist(recursive = F) # unlist map function generate list 
  
}

tbl_ttest <- function(data, variable, by, ...) {
  t.test(data[[variable]] ~ as.factor(data[[by]]),paired = T) %>%
    broom::tidy() %>%
    # ttest default compute order : mean1 - mean2, *-1 reverse, so is m2 - m1 (post - pre)
    mutate(statistic = statistic*(-1)) %>% 
    select(statistic, p.value)
}

my_t_table <- function(data) {
  
  tbl <- tbl_summary(data,by = Session,include = -Number,
                     statistic = list(all_continuous() ~ '{mean} ({sd})')) %>% 
    # add_p(test = all_continuous() ~ "t.test",
    #       test.args = all_tests("t.test") ~ list(paired = TRUE)) %>%
    # add a header to the statistic column, which is hidden by default
    # adding the header will also unhide the column
    add_stat(fns = everything() ~ tbl_ttest) %>%
    add_q() %>%  bold_p(q = T) %>% add_significance_stars() %>% 
    modify_header(list(statistic ~ "**t-statistic**",p.value ~ "**p-value**")) %>%
    modify_fmt_fun(list(statistic ~ style_sigfig,p.value ~ style_pvalue) ) 

  
  return(tbl)
}


my_corplot <- function(cor) {
  
  plot <- corrplot(cor$r,p.mat = cor$p,
                   method="square", type = 'u',
                   insig = 'blank',
                   addCoef.col = 'white',number.cex = 1,
                   tl.pos = 'd',tl.srt = 0,tl.col='black',tl.cex = 0.6,tl.offset = 0.1,
                   # cl.pos = 'n', # remove color bar
                   # bg = 'color1',addgrid.col = 'color2',
                   col = COL2('BrBG'))
  return(plot)
}
my_pcorplot <- function(pcor,pcor.test) {
  
  plot <- corrplot(pcor,p.mat = pcor.test$p,
                   method="square", type = 'l',
                   insig = 'blank',
                   addCoef.col = 'white',number.cex = 1,
                   tl.pos = 'd',tl.srt = 0,tl.col='black',tl.cex = 0.6,tl.offset = 0.1,
                   # cl.pos = 'n', # remove color bar
                   # bg = 'color1',addgrid.col = 'color2',
                   col = COL2('BrBG'))
  return(plot)
}

my_boxplot <- function(longdata,var,test,timelabel,colors,subtitle,all) {
  
  title = if_else(all == T,"Paired t-test for all variables",
                  "Paired t-test for all variables passing multiple corrections")
  
  # strvar <- as_name(quote(var))
  # 
  # dotn <- str_count(strvar, "\\.")
  # 
  # title = case_when(
  #   dotn == 0 ~ "Paired t-test for all variables",
  #   dotn == 1 ~ "Paired t-test for all significant variables (no corrections)",
  #   dotn == 2 ~ "Paired t-test for all variables passing multiple corrections",
  # )
  # 
  # if(dotn == 2){
  #   correct = str_split_1(strvar,'\\.')[3]
  # }
  
  if(length(var) == 0 ){
    NULL
  } else
  {
    ggplot(longdata[var],aes(Session,value))+
      
      pack_geom_box(line.mapping = aes(group = Number),
                    box.mapping = aes(fill = Session)) %+% #ggplot:: expose
      
      pack_sig(text.data = test[var], 
               text.mapping = aes(x = 1.5,y=Inf,label = str_c('p.adj = ',p.adj)),
               text.color = ifelse(test[var]$p.adj < 0.05 & all == T,'red','black'))+
      
      facet_wrap(~vars,scales = 'free_y')+
      
      scale_x_discrete(labels = timelabel)+
      #  scale_fill_brewer(palette = 'Set1')+
      scale_fill_manual(values  = colors)+
      theme_bw()+theme(axis.title = element_blank())+
      labs(title = title,
           subtitle = str_c(subtitle,"Correction method: FDR",sep = ',  '))+
      pack_theme(theme.legend.position='none')
    
    
    
  }
}

getresult <- function(variable) {
  # Each element may have a different length or be of a completely different type, 
  # so it is necessary to wrap them in a list.
  # then convert them to data.table and bind them together to look better
  result <-  with(myresult,get(variable)) %>% 
    map( \(x) map(x,list) %>% as.data.table()) %>% 
    rbindlist(idcol = 'label') %>% .[,label := as.factor(label)]  %>% 
    # merge info
    merge(myresult[,.SD,.SDcols = c(names(info),'var')] , . ,by = 'label') %>% 
    .[,label := fct_drop(label,only = NULL)] %>% 
    # merge subinfo
    merge(sub_t_wide,.,by.x = 'Label',by.y = 't2')%>% 
    # directly assign to global environment named input : 'variable' 
    assign(variable,.,envir = .GlobalEnv)
  # set result column to DT makes result not visual enough 
  # so Transfer this step into the get_result function
  # but this can not get info
  # so that merge info by hand 
  return(result)
}




# Not done ---------------------------------------------------------------------


# data.table function
my_recode <- function(DT,lut_list) {
  for (v in intersect(names(lut_list), colnames(DT))) {
    DT[lut_list[[v]], on = paste0(v, "==from"), (v) := i.to]
  }
}


# 
#  my_boxplot <- function(label) {
# 
#   myresult[label,ttest_result,on = 'label'] %>% unlist(recursive = F) %$%  # magrittr:: Expose the names in lhs to the rhs expression
#     {                       # %$% pass on to all layers using {}
#       
#       ggplot(longdata[sig.vars.fdr],aes(Label,value))+
#         #Do not need D_ttest$t_data,if use %$%
#         
#         pack_geom_box(line.mapping = aes(group = Number),
#                       box.mapping = aes(fill = Label)) %+% #ggplot:: expose
#         pack_sig(text.data = stat.test[sig.vars.fdr], 
#                  #dont need to D_ttest$t_test,if use %$%
#                  
#                  text.mapping = aes(x = 1.5,y=Inf,label = str_c('p = ',p.adj)))+
#         facet_wrap(~vars,scales = 'free_y')+
#         #    scale_x_discrete(labels = timelabel)+
#         #    scale_fill_brewer(palette = 'Set1',labels = timelabel)+
#         # pack_scale(all.labels = c(timelabel),
#         #            fill.palette = 'Set1')+
#         theme_bw()+theme(axis.title = element_blank())+
#         #    labs(title = 'Significant variable PLOT on paired t-test',
#         #         subtitle = 'N = 19 (F:10 / M:9)')+
#         pack_theme(theme.legend.position='none')
#     }
# }

