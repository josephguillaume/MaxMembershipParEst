library(shiny)
library(hydromad) ##SCEoptim


## f:
##  passed values for all parameters in ranges, returns a vector
##  Can use but not set variables from parent environments
## ranges:
##  Columns Variable,Modeled,Min,Max
## limit.val:
##  Either limits for active then response
##   or limits for every row in ranges
## which.active:
##  Indicate which parameters will be varied
## which.response:
##  Indicate which rows of ranges & limit.val are calculated by f
##  If ranges has column Variable, which.response can be character vector,
##   otherwise needs to be numbers
## ...:
##  passed to f as additional parameters
max.membership <- function(f,ranges,limit.val,
                           which.active,which.response,...){

  ## Convert characters to numbers
  if(is.character(which.response) && !is.null(ranges$Variable))
    which.response <- match(which.response,ranges$Variable)
  if(is.character(which.active) && !is.null(ranges$Variable))
    which.active <- match(which.active,ranges$Variable)
  ## Define start.pos and limit.val
  if(nrow(limit.val)==nrow(ranges)) limit.val <- rbind(limit.val[which.active,],limit.val[which.response,])
  print(limit.val)
  start.pos <- ranges$Modeled[c(which.active,which.response)]
  print(start.pos)

  ## Max distance from start.pos normalised by limit.val
  ## Corresponds to negative of membership function without discontinuities
  get.normalised <- function(x)
    ifelse(abs(x-start.pos)<1e-5,0, ## avoid blow out with large numbers TODO: neater reformulation?
           ifelse(x>start.pos,
                  (x-start.pos)/(limit.val[,2]-start.pos),
                  (start.pos-x)/(start.pos-limit.val[,1])
                  ))

  ##TODO: pass f an optional grouping variable
  f.max.norm.dist <- function(x,...){
    ## Pass f all parameters, keeping non-active at Modeled value
    x2=ranges$Modeled
    x2[which.active]<-x
    ## Remove response variables
    ## TODO: deal with extra that isn't active or response
    x3 <- x2[-which.response]
    ## Run
    tryCatch(y <- f(x3,...),error=function(e) browser())
    x <- c(x,y) ##active & response
    max(get.normalised(x))
  }

  if(require("compiler")){
    f <- cmpfun(f)
    f.max.norm.dist <- cmpfun(f.max.norm.dist)
    get.normalised <- cmpfun(get.normalised)
  }

  st <- proc.time()
  opt <- SCEoptim(f.max.norm.dist,
                  ranges$Modeled[which.active],
                  ## Absolute extreme range
                  lower=ranges$Min[which.active],
                  upper=ranges$Max[which.active],
                  control=list(ncomplex=20,trace=1),
                  ...
                  )
  print(proc.time()-st)
  print(str(opt))
  ## Actual response
  x2 <- ranges$Modeled
  x2[which.active]<-opt$par
  ## Remove response variables
  x3 <- x2[-which.response]
  val=f(x3)
  ## Membership function
  normalised <- 1-get.normalised(c(opt$par,val))
  print(normalised)
  return(c(opt$par,val))
} ##max.membership



## Expects global variables: ranges,which_response

shinyServer(function(input, output, session) {

  output$more_results <- renderTable({
    input$more_update
    which.active <- isolate(input$more)
    if(length(which.active)==0) stop("Select variables to use")
    ## Subset pars based on active_vars
    limit.val <- t(sapply(c(which.active,which.response),function(v) isolate(input[[sprintf("more_slider_%s",v)]])))
    print(limit.val)

    res <- max.membership(f,ranges,limit.val,which.active,which.response)
    cbind(ranges[match(c(which.active,which.response),ranges$Variable),],limit.val,value=res)
  })
})
