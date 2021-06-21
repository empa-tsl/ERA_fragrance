#=================================================================================================
#Module: Function
#
#Date of last modification: 12.04.2021
#
# 1. rtriang.perc_a: assign a triangular distribution - when the input value has mode and uncertainty 
# 2. rtriang.perc_b: assign a triangular distribution - when the input value has mode, min and max 
# 3. normalize
# 4. transform marix
#=================================================================================================

#=================================================================================================
# 1.rtriang.perc_a:
#=================================================================================================

# introduce symmetric triangular distribution with percentage (default being 50%)

rtriang.perc_a <- function(mmode, perc = 0.5, N = SIM, do.trunc = FALSE)
{
  require(mc2d)
  
  stopifnot(perc <= 1, perc >= 0)
  
  if(is.na(mmode)){
    return(rep(NA,N))
  }
  
  mmin <- mmode - mmode*perc
  mmax <- mmode + mmode*perc
  
  if(do.trunc){
    # if wished truncate the distribution under 0 and above 1
    return(rtrunc(distr = "rtriang", n = N, linf = 0, lsup = 1, min = mmin, mode = mmode, max = mmax))
  } else {
    return(rtriang(n = N, min = mmin, mode = mmode, max = mmax))
  }
  
  return(out)
}

# introduce function that creates a triangular distribution from a modal value and an uncertainty

triangulize_a <- function(modalvalues, uncertainty, N, do.trunc = FALSE){
  
  # 1. Test the input
  if(!all(is.numeric(modalvalues)) & !all(is.numeric(uncertainty))){
    stop("modalvalues and uncertainty have to be numeric.")
  }
  
  if((is.vector(modalvalues) & !is.vector(uncertainty)) |
     (!is.vector(modalvalues) & is.vector(uncertainty))){
    stop("modalvalues and uncertainty are not both vectors, cannot proceed to calculation.")
  }
  
  if((is.matrix(modalvalues) & !is.matrix(uncertainty)) |
     (!is.matrix(modalvalues) & is.matrix(uncertainty))){
    stop("modalvalues and uncertainty are not both matrices, cannot proceed to calculation.")
  }
  
  if((!is.vector(modalvalues) & !is.matrix(modalvalues)) |
     (!is.vector(uncertainty) & !is.matrix(uncertainty))){
    stop("modalvalues and uncertainty have to be either vectors or matrices.")
  }
  
  if(!(all(N == floor(N)) & length(N) == 1)){
    stop("N needs to be a single integer number.")
  }
  
  # 2. create a triangular distribution for every matrix/vector element of modalvalues
  if(is.vector(modalvalues)){
    
    # create empty matrix
    Distr <- matrix(0, length(modalvalues), N)
    if(!is.null(names(modalvalues))){
      rownames(Distr) <- names(modalvalues)
    }
    
    # fill matrix with distributions
    for(i in 1:length(modalvalues)){
      if(is.na(modalvalues[i])){
        Distr[i,] <- NA
      } else if(modalvalues[i] == 0){
        next
      } else Distr[i,] <- rtriang.perc_a(mmode = modalvalues[i], perc = uncertainty[i], N = N, do.trunc = do.trunc)
    }
    
  } else {
    
    # create empty matrix
    Distr <- array(0, c(dim(modalvalues), N))
    if(any(!is.null(dimnames(modalvalues)))){
      dimnames(Distr) <- list(rownames(modalvalues), colnames(modalvalues), NULL)
    }
    
    # fill matrix with distributions
    for(i in 1:dim(TC)[1]){
      for(j in 1:dim(TC)[2]){
        if(is.na(modalvalues[i,j])){
          Distr[i,j,] <- NA
        } else if(modalvalues[i,j] == 0){
          next
        } else Distr[i,j,] <- rtriang.perc_a(mmode = modalvalues[i,j], perc = uncertainty[i,j], N = N, do.trunc = do.trunc)
      }
    }
    
  }
  
  return(Distr)
}

#=================================================================================================
# 2.rtriang.perc_b:
#=================================================================================================

# introduce symmetric triangular distribution with min and max

rtriang.perc_b <- function(mmode, min, max, N , do.trunc = FALSE)
{
  require(mc2d)
  
  stopifnot(min > 0)
  
  if(is.na(mmode)){
    return(rep(NA,N))
  }
  
  mmin <- min
  mmax <- max
  
  if(do.trunc){
    # if wished truncate the distribution under 0 and above 1
    return(rtrunc(distr = "rtriang", n = N, linf = 0, lsup = 1, min = mmin, mode = mmode, max = mmax))
  } else {
    return(rtriang(n = N, min = mmin, mode = mmode, max = mmax))
  }
  
  return(out)
}

# introduce function that creates a triangular distribution from a modal value and min and max

triangulize_b <- function(modalvalues, min, max, N, do.trunc = FALSE){
  
  # 2. create a triangular distribution for every matrix/vector element of modalvalues
  if(is.vector(modalvalues)){
    
    # create empty matrix
    Distr <- matrix(0, length(modalvalues), N)
    if(!is.null(names(modalvalues))){
      rownames(Distr) <- names(modalvalues)
    }
    
    # fill matrix with distributions
    for(i in 1:length(modalvalues)){
      if(is.na(modalvalues[i])){
        Distr[i,] <- NA
      } else if(modalvalues[i] == 0){
        next
      } else Distr[i,] <- rtriang.perc_b(mmode = modalvalues[i], min = min, max = max, N = N, do.trunc = do.trunc)
    }
    
  } else {
    
    # create empty matrix
    Distr <- array(0, c(dim(modalvalues), N))
    if(any(!is.null(dimnames(modalvalues)))){
      dimnames(Distr) <- list(rownames(modalvalues), colnames(modalvalues), NULL)
    }
    
    # fill matrix with distributions
    for(i in 1:dim(TC)[1]){
      for(j in 1:dim(TC)[2]){
        if(is.na(modalvalues[i,j])){
          Distr[i,j,] <- NA
        } else if(modalvalues[i,j] == 0){
          next
        }else Distr[i,] <- rtriang.perc_b(mmode = modalvalues[i], min = min, max = max, N = N, do.trunc = do.trunc)
      }
    }
    
  }
  
  return(Distr)
}

# =================================================================================================
# 3. Normalize
# =================================================================================================

normalize <- function(Distr){
  
  # 1. Test the input
  if(any(is.na(Distr))){
    stop("There are undefined values in the input.")
  }
  
  if(!is.array(Distr)){
    stop("Distr should be a matrix or a 3D array.")
  }
  
  if(!dim(Distr)[1] == dim(Distr)[2]){
    stop("The first two dimensions of Distr should be equal.")
  }
  
  # 2. Normalization step: make sure that all the flows going out of a compartment sum up to 1
  if(length(dim(Distr)) == 2){ # if it is a matrix
    Distr.Norm <- apply(Distr, 2, function(x) if(sum(x) == 0){ return(rep(0,length(x))) } else { return(x/sum(x)) })
    
  } else if(length(dim(Distr)) == 3){ # if it is an array
    Distr.Norm <- apply(Distr, c(2,3), function(x) if(sum(x) == 0){ return(rep(0,length(x))) } else { return(x/sum(x)) })
    
  } else {
    stop("The Distr matrix needs to have 2 or 3 dimensions.")
  }
  
  return(Distr.Norm)
}

# =================================================================================================
# 4. Matrix transform
# =================================================================================================

# function that transforms the matrix type (TC to -TC, diagonal to 1)
matrix.transform <- function(Distr){
  
  # 1. Test the input
  if(any(is.na(Distr))){
    stop("There are undefined values in the input.")
  }
  
  if(!is.array(Distr)){
    stop("Distr should be a matrix or a 3D array.")
  }
  
  if(!dim(Distr)[1] == dim(Distr)[2]){
    stop("The first two dimensions of Distr should be equal.")
  }
  
  # 2. Transformation step
  if(length(dim(Distr)) == 2){
    # transform all the values to negative
    Distr <- -Distr
    # set the diagonal to 1
    diag(Distr) <- 1
    
  } else if(length(dim(Distr)) == 3){
    # transform all the values to negative
    Distr <- -Distr
    # set the diagonal to 1
    for(k in 1:dim(Distr)[3]){
      diag(Distr[,,k]) <- 1
    }
    
  } else {
    stop("The Distr matrix needs to have 2 or 3 dimensions.")
  }
  
  return(Distr)
}



