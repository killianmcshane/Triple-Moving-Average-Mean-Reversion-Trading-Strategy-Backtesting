getOrders <- function(store, newRowList, currentPos, info, params) {

    allzero  <- rep(0,length(newRowList)) # used for initializing vectors
    pos <- allzero

    if (is.null(store)) 
        store <- initStore(newRowList)
    else
        store <- updateStore(store, newRowList)
    
    if (store$iter > params$lookbacks$long) {
        
        # current_close  & prices retrieved from either from store$cl

        # With these getTMA, getPosSignFromTMA and getPosSize  are used to
        # to assign positions to the vector pos
        
        # Sets the lookbacks and prices parameters.
        lookbacks = params$lookbacks
        prices_list = store$cl
        
        position_signs = vector()
        position_sizes = vector()
        
        # Slices the prices list to the last lookbacks$long-many days to avoid
        # any floating point calculation errors.
        prices_lookback_length <- lapply(prices_list, function(x) x[(length(x) - lookbacks$long):length(x)])
        
        # Computes the positions for each series in the data-set 
        for (list in prices_lookback_length) {
            current_close = as.numeric(unlist(tail(list, 1)))
            position_signs = c(position_signs, getPosSignFromTMA((getTMA(list, lookbacks))))
            position_sizes = c(position_sizes, getPosSize(current_close))
            positions = position_sizes * position_signs
        }
        
        pos = positions
    }
    
    marketOrders <- -currentPos + pos

    return(list(store=store,marketOrders=marketOrders,
	                    limitOrders1=allzero,limitPrices1=allzero,
	                    limitOrders2=allzero,limitPrices2=allzero))
}

###############################################################################

getTMA <- function(prices, lookbacks) {

    # prices should be an xts with one of its columns called "Close"

    # lookbacks should be list with exactly three elements:
    # lookbacks$short  is an integer
    # lookbacks$medium is an integer
    # lookbacks$long   is an integer

    # It should be the case that:
    # lookbacks$short < lookbacks$medium < lookbacks$long

    ###########################################################################
    ## START OF 6 CHECKS 
    if (!("short" %in% names(lookbacks)) | !("medium" %in% names(lookbacks)) | !("long" %in% names(lookbacks)))
        stop("E01: 'short', 'medium', or 'long' is missing from names(lookbacks)")
    if (!is.integer(lookbacks$short) | !is.integer(lookbacks$medium) | !is.integer(lookbacks$long))
        stop("E02: At least one lookback is not an integer according to is.integer()")
    if (!((lookbacks$short < lookbacks$medium) & (lookbacks$medium < lookbacks$long)))
        stop("E03: lookbacks$short < lookbacks$medium < lookbacks$long doesn't hold")
    if (!is.xts(prices))
        stop("E04: prices is not an xts according to is.xts()")
    if (!(nrow(prices) >= lookbacks$long))
        stop("E05: prices does not enough rows")
    if (!("Close" %in% names(prices)))
        stop("E06: prices does not contain a column 'Close'")
    ## END OF 6 CHECKS 
    ###########################################################################

    # Loads in the TTR library and calculates the short, medium and long 
    # simple moving averages. THen combines them into a named list to be returned.
    library(TTR)
    short_sma = as.numeric(tail(SMA(prices$Close, n=lookbacks$short), 1))
    medium_sma = as.numeric(tail(SMA(prices$Close, n=lookbacks$medium), 1))
    long_sma = as.numeric(tail(SMA(prices$Close, n=lookbacks$long), 1))
  
    ret <- list(short = short_sma, medium = medium_sma, long = long_sma)

    return(ret)
}

getPosSignFromTMA <- function(tma_list) {
    # This function takes a list of numbers tma_list with three elements 
    # called short, medium, and long, which correspond to the SMA values for 
    # a short, medium and long lookback, respectively.

    # This function returns a single number that is:
    #       -1 if the short SMA < medium SMA < long SMA
    #        1 if the short SMA > medium SMA > long SMA
    #        0 otherwise
    
    # Checks whether either of the position conditions hold and returns
    # either 1 (long), -1 (short) or 0 (flat) if not.
    position = 0
    if ((tma_list$short < tma_list$medium) && (tma_list$medium < tma_list$long))
        position = -1
    else if ((tma_list$short > tma_list$medium) && (tma_list$medium > tma_list$long))
        position = 1
    
    return(position)
}

getPosSize <- function(current_close,constant=1000) {
    # This function returns (constant divided by current_close) 
    # rounded down to the nearest integer
    
    size = floor(constant / current_close)
    
    return(size)
}

###############################################################################
initClStore  <- function(newRowList) {
  clStore <- lapply(newRowList, function(x) x$Close)
  return(clStore)
}
updateClStore <- function(clStore, newRowList) {
  clStore <- mapply(function(x,y) rbind(x,y$Close),clStore,newRowList,SIMPLIFY=FALSE)
  return(clStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=1,cl=initClStore(newRowList)))
}
updateStore <- function(store, newRowList) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList) 
  return(store)
}
