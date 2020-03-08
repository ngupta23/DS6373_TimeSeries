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
      self$compute_metrics()
      
    },
    
    #### Getters and Setters ----
    
    get_x = function(){return(self$x)},
    set_x = function(x){self$x = x},
    
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
    compute_metrics = function(){
      for (name in names(self$get_models())){
        
        if (self$models[[name]][['metric_has_been_computed']] == FALSE){
          
          res = sliding_ase(x = self$get_x(),
                            phi = self$get_models()[[name]][['phi']],
                            theta = self$get_models()[[name]][['theta']],
                            d = self$get_models()[[name]][['d']],
                            s = self$get_models()[[name]][['s']],
                            n.ahead = self$get_n.ahead(),
                            batch_size = self$get_models()[[name]][['batch_size']])
          
          ## Inplace
          self$models[[name]][['ASEs']] = res$ASEs  
          self$models[[name]][['Time']] = res$Time
          self$models[[name]][['metric_has_been_computed']] = TRUE
        }
        else{
          warning(paste("Metrics have already been computed for Model: '", name, "'. These will not be computed again."))
        }
      }
    },
    
    compare_forecasts = function(){
      # Visual Comparison of future forecasts
      
    },
    
    plot_histogram_ases = function(){
      # Visual Comparison of future forecasts
      results = tribble(~Model, ~ASE) 
      for (name in names(self$get_models())){
        if (self$models[[name]][['metric_has_been_computed']] == TRUE){
          results = results %>% add_row(Model = name, ASE = self$models[[name]][['ASEs']])   
        }
        else{
          warning(paste("Metrics have not been computed for Model: '", name, "'. These will not be plotted."))
        }
      }
      
      print(ggplot(results, aes(x = Model, y = ASE, color = Model)) + geom_boxplot() + coord_flip())
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
    }
    
    
    
  )
  
)

