library(R6)

ModelCompareUnivariate = R6Class(
  classname = "ModelCompareUnivariate",
  cloneable = TRUE,
  lock_objects=F,
  lock_class=F,
  
  #### Public Methods ----
  public=list(
    x = NA,
    #num_models = NA,
    models = NA,
    n.ahead = NA,
    batch_size = NA,
    
    #### Constructor ----
    initialize = function(x = NA, mdl_list, n.ahead = NA, batch_size = NA)
    {
      # Add checks here
      if (all(is.na(x))){ stop("You have not provided the time series data. Please provide to continue.") }
      
      self$set_x(x = x)
      self$add_models(mdl_list)
      private$set_n.ahead(n.ahead)
      private$set_batch_size(batch_size)
      self$compute_metrics(step_n.ahead = TRUE)
      
    },
    
    #### Getters and Setters ----
    
    get_x = function(){return(self$x)},
    set_x = function(x){self$x = x},
    get_len_x = function(){
      return(length(self$get_x()))
    },
    
    get_batch_size = function(){return(self$batch_size)},
    get_n.ahead = function(){return(self$n.ahead)},
    
    
    add_models = function(mdl_list){
      if (length(unique(names(mdl_list))) != length(names(mdl_list))){
        stop("The model names in the provided list contain duplicates. Please fix and rerun.")
      }
      
      existing = names(self$models)
      if (any(names(mdl_list) %in% existing)){
        print(names(mdl_list)[names(mdl_list) %in% existing])
        stop("The model names above already exist in this comparison object. Please provide unique names.")
      }
      
      mdl_list = private$clean_model_input(mdl_list)
      
      if (all(is.na(self$models))){
        self$models = mdl_list
      }
      else{
        self$models = c(self$models, mdl_list)
      }
    },

    remove_models = function(x){
      
    },
    
    get_models = function(){
      return(self$models)
    },
    
    #### General Public Methods ----
    compute_metrics = function(step_n.ahead = FALSE){
      for (name in names(self$get_models())){
        
        if (self$models[[name]][['metric_has_been_computed']] == FALSE){
          cat(paste("\n\n\nComputing metrics for: ", name, "\n"))
          
          res = private$sliding_ase(x = self$get_x(),
                                    phi = self$get_models()[[name]][['phi']],
                                    theta = self$get_models()[[name]][['theta']],
                                    d = self$get_models()[[name]][['d']],
                                    s = self$get_models()[[name]][['s']],
                                    n.ahead = self$get_n.ahead(),
                                    batch_size = self$get_models()[[name]][['batch_size']],
                                    step_n.ahead = step_n.ahead)
          

          ## Inplace
          self$models[[name]][['ASEs']] = res$ASEs  
          self$models[[name]][['time_test_start']] = res$time_test_start
          self$models[[name]][['time_test_end']] = res$time_test_end
          self$models[[name]][['batch_num']] = res$batch_num
          self$models[[name]][['f']] = res$f
          self$models[[name]][['ll']] = res$ll
          self$models[[name]][['ul']] = res$ul
          self$models[[name]][['time.forecasts']] = res$time.forecasts
          
          
          self$models[[name]][['metric_has_been_computed']] = TRUE
          
        }
        else{
          warning(paste("Metrics have already been computed for Model: '", name, "'. These will not be computed again."))
        }
      }
    },
    
    plot_histogram_ases = function(){
      results = self$get_tabular_metrics()
      print(ggplot(results, aes(x = Model, y = ASE, color = Model)) + geom_boxplot() + coord_flip())
    },
    
    plot_forecasts = function(only_sliding = TRUE){
      results.forecasts = self$get_tabular_metrics(ases = FALSE)
      
      model_subset = c("Realization")
      if (only_sliding){
        for (name in names(self$get_models())){
          if (self$models[[name]][['sliding_ase']] == TRUE){
            model_subset = c(model_subset, name)
          }
        }
      }
      else{
        # Add all models
        for (name in names(self$get_models())){
          model_subset = c(model_subset, name)
        }
      }

      
      results.forecasts = results.forecasts %>% 
        filter(Model %in% model_subset)
      
      # https://stackoverflow.com/questions/9968975/make-the-background-of-a-graph-different-colours-in-different-regions
      
      # Get Batch Boundaries
      results.ases = self$get_tabular_metrics(ases = TRUE)
      for (name in names(self$get_models())){
        if (self$models[[name]][['sliding_ase']] == TRUE){
          results.batches = results.ases %>% 
            filter(Model == name)
          break()
        }
      }
      
      rects = data.frame(xstart = results.batches[['Time_Test_Start']],
                         xend = results.batches[['Time_Test_End']],
                         Batch = rep(1, length(results.batches[['Batch']])))
      
      
      p = ggplot() + 
        geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Batch), alpha = 0.1, show.legend = FALSE) +  
        geom_line(results.forecasts, mapping = aes(x = Time, y = f, color = Model)) +
        ylab("Forecasts")
      
      print(p) 
      
      
      p = ggplot() +
        geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Batch), alpha = 0.1, show.legend = FALSE) +  
        geom_line(results.forecasts, mapping = aes(x=Time, y=ll, color = Model)) + 
        geom_line(results.forecasts, mapping = aes(x=Time, y=ul, color = Model)) +
        ylab("Upper and Lower Forecast Limits (95%)")
      
      print(p)
      
    },
    
    statistical_compare = function(){
      results = self$get_tabular_metrics(only_sliding = TRUE)
      
      # Analysis of Variance
      res.aov = aov(ASE ~ Model, data = results)
      print(summary(res.aov))
      cat("\n\n")
      
      # Tukey Adjustment
      print(TukeyHSD(res.aov))
      
      return(res.aov)
      
    },
    
    get_tabular_metrics = function(only_sliding = FALSE, ases = TRUE){
      # ases = TRUE returns the ASE values for each batch
      # ases = FALSE returns the forecasts, and the lower limits and upper limits asscoiated with the forecasts
      
      if (ases == TRUE){
        results = tribble(~Model, ~ASE, ~Time_Test_Start, ~Time_Test_End, ~Batch) 
      }
      else {
        results = tribble(~Model, ~Time, ~f, ~ll, ~ul) 
      }
      
      model_names = c()
      
      for (name in names(self$get_models())){
        if (self$models[[name]][['metric_has_been_computed']] == TRUE){
          if(only_sliding == TRUE){
            if (self$models[[name]][['sliding_ase']] == TRUE){
              model_names = c(model_names, name)
            }
          }
          else{
            model_names = c(model_names, name)
          }
        }
        else{
          warning(paste("Metrics have not been computed for Model: '", name, "'. These will not be plotted."))
        }
      }
          
      for (name in model_names){    
        if (ases == TRUE){
          results = results %>% add_row(Model = name,
                                        ASE = self$models[[name]][['ASEs']],
                                        Time_Test_Start = self$models[[name]][['time_test_start']],
                                        Time_Test_End = self$models[[name]][['time_test_end']],
                                        Batch = self$models[[name]][['batch_num']])
        }
        
        else{
          results = results %>% add_row(Model = name,
                                        Time = self$models[[name]][['time.forecasts']],
                                        f = self$models[[name]][['f']],
                                        ll = self$models[[name]][['ll']],
                                        ul = self$models[[name]][['ul']])

        }
      }
        
      if (ases == FALSE){
        # Add the realization as well
        results = results %>% add_row(Model = "Realization",
                                      Time = seq(1, self$get_len_x(), 1),
                                      f = self$get_x(),
                                      ll = self$get_x(),
                                      ul = self$get_x())
      }
      
      return(results)
    }
  
    
  ),
    
  
  #### Private Methods ----
  private = list(
    clean_model_input = function(mdl_list){
      # If the inputs are missing p, d, q, or s values, this will add 0s to make it consistent
      for (name in names(mdl_list)){
        if (is.null(mdl_list[[name]][['phi']])){
          mdl_list[[name]][['phi']] = 0
        }
        if (is.null(mdl_list[[name]][['d']])){
          mdl_list[[name]][['d']] = 0
        }
        if (is.null(mdl_list[[name]][['theta']])){
          mdl_list[[name]][['theta']] = 0
        }
        if (is.null(mdl_list[[name]][['s']])){
          mdl_list[[name]][['s']] = 0
        }
        if (is.null(mdl_list[[name]][['sliding_ase']])){
          mdl_list[[name]][['sliding_ase']] = FALSE
        }
        
        mdl_list[[name]][['metric_has_been_computed']] = FALSE
        
      }
      
      return(mdl_list)
    },
    
    any_sliding_ase = function(){
      for (name in names(self$get_models())){
        if (self$get_models()[[name]][['sliding_ase']]){
          return(TRUE)
        }
      }
      return(FALSE)
    },
    
    set_n.ahead = function(n.ahead){
      self$n.ahead = n.ahead
    },
    
    set_batch_per_model = function(){
      for (name in names(self$get_models())){
        if (self$get_models()[[name]][['sliding_ase']]){
          self$models[[name]][['batch_size']] = self$get_batch_size()  ## Inplace, hence not using get_models
        }
        else{
          self$models[[name]][['batch_size']] = NA  ## Inplace, hence not using get_models
        }
      }
    },
    
    set_batch_size = function(batch_size){
      if (private$any_sliding_ase() & is.na(batch_size)){
        stop("You have provided models that require sliding ASE calculations, but the batch size has been set to NA. Please provide an appropriate value to proceed.")
      }
      self$batch_size = batch_size
      private$set_batch_per_model()
    },
    
    sliding_ase = function(x,
                           phi = 0, theta = 0, d = 0, s = 0, # ARUMA arguments
                           linear = NA, freq = NA,           # Signal + Noise arguments
                           n.ahead = NA, batch_size = NA,    # Forecasting specific arguments
                           step_n.ahead = FALSE,
                           ...)                              # max.p (sigplusnoise), lambda (ARUMA)      
    {
      # Sliding CV ... batches are mutually exclusive
      
      n = length(x)
      
      if (is.na(batch_size)){
        warning("Batch Size has not been specified. Will assume a single batch")
        cat("\n")
        batch_size = n
      }
      
      if (is.na(n.ahead)){
        stop("Number of points to be used for forecasting has not been specified. Please specify n.ahead")
      }
      
      if (all(phi == 0) & all(theta == 0) & d == 0 & s == 0){
        if (is.na(linear) & is.na(freq)){
          stop("You have specified the arguments for neither an ARMA/ARUMA model or a Signal + Noise Model. Please specify at least one of these to continue")
        }
      }
      
      aruma = FALSE
      if (!(all(phi == 0) & all(theta == 0) & d == 0 & s == 0)){
        aruma = TRUE
      }
      else{
        # Signal + Noise model
      }
      
      forecasts.f = rep(NA, n)
      forecasts.ul = rep(NA, n)
      forecasts.ll = rep(NA, n)
      time.forecasts = seq(1, n, 1)
      
      start = 1
      
      if (step_n.ahead == FALSE){
        # Step Size = 1
        step_size = 1
        num_batches = n-batch_size+1  
      }
      else{
        # Step by n.ahead each time
        step_size = n.ahead
        num_batches = floor((n-batch_size)/n.ahead)  + 1
      }
      
      cat(paste("Number of batches expected: ", num_batches, "\n"))
      
      ASEs = numeric(num_batches)
      time_test_start = numeric(num_batches)
      time_test_end = numeric(num_batches)
      batch_num = numeric(num_batches)
      
      for (i in 0:(num_batches-1))
      {
        # Define the batch
        subset = x[start:(batch_size+i*step_size)]
        # Take last n.ahead observations from the batch and use that to compare with the forecast
        
        test_start = i*step_size + batch_size - n.ahead + 1
        test_end = i*step_size + batch_size
        data_start = start
        data_end = i*step_size + batch_size
        
        time_test_start[i+1] = test_start
        time_test_end[i+1] = test_end
        batch_num[i+1] = i+1
        
        
        test_data = x[test_start:test_end]
        
        if (aruma){
          forecasts = tswge::fore.aruma.wge(x = subset, phi = phi, theta = theta, d = d, s = s,
                                            n.ahead = n.ahead, lastn = TRUE, plot = FALSE, ...)
        }
        else{
          forecasts = tswge::fore.sigplusnoise.wge(x = subset, linear = linear, freq = freq,
                                                   n.ahead = n.ahead, lastn = TRUE, plot = FALSE, ...)
        }
        
        ASEs[i+1] = mean((test_data - forecasts$f)^2)
        start = start + step_size
        
        forecasts.f[test_start: test_end] = forecasts$f 
        forecasts.ll[test_start: test_end] = forecasts$ll
        forecasts.ul[test_start: test_end] = forecasts$ul
        time.forecasts[test_start: test_end] = seq(test_start, test_end, 1)
      }
      
      # print("sliding_ase.4: forecasts")
      # print(forecasts.f)
      
      return(list(ASEs = ASEs,
                  time_test_start = time_test_start,
                  time_test_end = time_test_end,
                  batch_num = batch_num,
                  f = forecasts.f,
                  ll = forecasts.ll,
                  ul = forecasts.ul,
                  time.forecasts = time.forecasts ))
    }
    
    
    
  )
  
)

