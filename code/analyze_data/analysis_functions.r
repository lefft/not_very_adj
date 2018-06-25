###############################################################################
### `analysis_functions.r`: funcs for analyzing expt1 and expt2 data ---------
##############################################################################
### 
### NOTES:
###   - implementation written by Alexandre Cremers, summer 2017
###   - minor edits by Timothy Leffel, spring 2018
###   - non-base functions used: 
###       >> `lme4::lmer()`, `sfsmisc::`, `plyr::ddply()`
### 
### --------------------------------------------------------------------------
##############################################################################




# require(lme4)
# require(sfsmisc)
# require(reshape2) # TODO -- check we dont need this, then remove
# require(plyr)     # TODO -- check we dont need this, then remove


### FUNCTIONS FOR CLUSTER ANALYSIS --------------------------------------------
# -----------------------------------------------------------------------------



# Function to find clusters in a vector of t-values:
find.clusters <- function(vect, threshold){
  
  # example: 
  #                    [4]    [6     7]  [8       9]        [11    12    13]
  # vect <- c(1, 1, 2, 2.1, 1, 2.1, 2.2, -2.2, -2.3, -1.3, -2.2, -2.2, -2.2)
  # threshold <- 2
  # find.clusters(vect, threshold) # returns each idx vector + sum of values
  
  clusters <- list(NULL)
  
  # k will index clusters (both pos and neg)
  k <- 1 
  
  # Check if there are any values above threshold
  if (max(vect) > threshold){ 
    # Extract consecutive values above threshold
    indices <- sfsmisc::inv.seq(which((vect) > threshold))
    
    # Need a separate case for a unique value
    if (length(indices)==1){ 
      
      clusters[[k]] <- list(
        as.vector(eval(indices)), abs(sum(vect[eval(indices)])))
      
      k <- k+1
      
    } else {
      # Save cluster position and size for each
      for (i in 2:length(indices)){
        
        clusters[[k]] <- list(
          as.vector(eval(indices[[i]])),abs(sum(vect[eval(indices[[i]])])))
        
        k <- k+1
      }
    }
    
  }
  
  # Same for values below -threshold
  if (max(-vect) > threshold){ 
    indices <- sfsmisc::inv.seq(which((-vect) > threshold))
    
    if (length(indices)==1){
      clusters[[k]]<-list(
        as.vector(eval(indices)),abs(sum(vect[eval(indices)]))
      )
      k <- k+1
      
    } else {
      for(i in 2:length(indices)){
        clusters[[k]] <- list(
          as.vector(eval(indices[[i]])),abs(sum(vect[eval(indices[[i]])]))
        )
        k <- k+1
      }
    }
    
  }
  return(clusters)
}




### functions to compute a vector of t-values (for each scale point) ----------

### `make.stats.lmer()` -- used in expt1 -----------------------
# Take a DV, COND(ition), Subject and vector of CONT(inuous variable)
# Return COND t-vals from lmer(DV~Cond+(1|Subject)) for each val of CONT
make.stats.lmer <- function(DV_vect, COND_vect, SUBJ_vect, CONT_vect){
  
  stats <- data.frame(Cont_Fact=sort(unique(CONT_vect)), TVAL=NA)
  
  h <- NA
  
  for (h in unique(CONT_vect)){
    
    DV_vect_loc <- DV_vect[CONT_vect==h]
    COND_vect_loc <- COND_vect[CONT_vect==h]
    SUBJ_vect_loc <- SUBJ_vect[CONT_vect==h]
    
    tmp.mod <- lme4::lmer(DV_vect_loc ~ COND_vect_loc + (1|SUBJ_vect_loc))
    t.val <- summary(tmp.mod)$coefficients[2,3]
    
    stats$TVAL[stats$Cont_Fact==h] <- t.val
  }
  
  return(stats)
}


### `make.stats.lm()` -- used in expt2 -------------------------
# Same as `make.stats.lmer()` lm instead of lmer:
make.stats.lm <- function(DV_vect, COND_vect, SUBJ_vect, CONT_vect){
  
  stats <- data.frame(Cont_Fact=sort(unique(CONT_vect)), TVAL=NA)
  
  h <- NA
  
  for (h in unique(CONT_vect)){
    
    DV_vect_loc <- DV_vect[CONT_vect==h]
    COND_vect_loc <- COND_vect[CONT_vect==h]
    SUBJ_vect_loc <- SUBJ_vect[CONT_vect==h]
    
    tmp.mod <- lm(DV_vect_loc ~ COND_vect_loc)
    t.val <- summary(tmp.mod)$coefficients[2,3]
    
    stats$TVAL[stats$Cont_Fact==h] <- t.val
  }
  
  return(stats)
}






### `biggest.cluster()` -- used in both experiments ---------------------------

# Function to extract the size of the biggest cluster:
biggest.cluster <- function(vect, threshold){
  
  C <- find.clusters(vect, threshold)
  
  # If nothing above threshold, just return the largest t-value
  # Otherwise, find the biggest cluster and its "size" (sum of its t-values)
  if (is.null(C[[1]])){
    return(max(abs(vect)))
  } else {
    Size <- C[[1]][[2]]	
  }
  
  for (i in 1:length(C)){
    
    if (C[[i]][[2]] > Size){
      Size <- C[[i]][[2]]
    }
  }
  
  return(Size)
}



### `MCsampling()` -- used in both experiments --------------------------------

# DF=dat.stat[dat.stat$Adj==adj & !dat.stat$Comparative, ]; 
# DV="response_scaled";COND="Pred2";SUBJECT="subj_id";
# CONT="NormUnit";threshold=2;N=n_perms;verbose=TRUE;mixed=FALSE

# Computes the biggest cluster for N random permutations of a given data set.
# Use verbose=T to follow progress, mixed=T for lmer and mixed=F for simple lm.
MCsampling <- function(DF, DV, COND, SUBJECT, CONT, threshold, N, 
                       verbose=FALSE, mixed=TRUE){
  
  # Define an "Index" which will code for repetitions of the same condition:
  # (we need to encode all non-relevant conditions into one index 
  #  to avoid issues with reshape)
  df <- plyr::ddply(.data=DF, .variables=c(SUBJECT, COND, CONT), 
                    .fun=function(x) cbind(x, Index=1:nrow(x)))
  
  # MAY22 -- STYLE EDIT (UNSAFE TO USE subset() INSIDE FUNC)
  df <- df[, c(SUBJECT, COND, CONT, "Index", DV)]
  # df <- subset(df, select=c(SUBJECT, COND, CONT,"Index",DV))
  
  # Create vector to collect the biggest cluster size in each permutation, 
  # where the *size* of a cluster is the sum of its t-values 
  Size <- vector(mode="numeric", length=N)
  
  # Reshape to Wide format to ensure that the n-th repetition of a 
  # given condition at each value of CONT will keep the same COND label
  Wide <- reshape(df, v.names=DV, idvar=c(COND, SUBJECT, "Index"), 
                  timevar=CONT,direction="wide")
  
  n <- nrow(Wide)
  
  # main loop, at each iter: shuffle labels, fit model, extract biggest cluster
  for (i in 1:N){
    
    # MAY22 -- MINOR EDIT (ran_id_vect --> random_id_vect)
    random_id_vect <- sample.int(n, replace=FALSE)
    # ran_id_vect <- sample.int(n, replace=FALSE)
    
    ShuffleWide <- Wide
    
    # Shuffle the labels for COND:
    ShuffleWide[, COND] <- Wide[random_id_vect, COND]
    
    # Reshape back to long format for analyses:
    # (need new row names because original row names may not be unique anymore)
    ShuffleDF <- reshape(ShuffleWide, direction="long", 
                         new.row.names=1:nrow(df))
    
    # MAY22 -- MINOR EDIT (SO WE DONT REPEAT "ShuffleDF[, DV], ...")
    # get the appropriate stats func (mixed for expt1, OLS for expt2)
    stat_maker_func <- ifelse(mixed, make.stats.lmer, 
                              ifelse(!mixed, make.stats.lm, 
                                     stop("param `mixed` must be boolean!")))
    
    stats <- stat_maker_func(ShuffleDF[, DV], ShuffleDF[, COND], 
                             ShuffleDF[, SUBJECT], ShuffleDF[, CONT])
    
    # Save size of the biggest cluster for the current permutation:
    Size[i] <- biggest.cluster(stats$TVAL, threshold=threshold)
    
    # MAY22 -- MINOR EDIT (more informative progress message)
    # if `verbose`, show progress every ten iterations 
    if (verbose && i %% 100 == 0){
      message("done with iter `", i, "` of `", N, "`...")
    }
    # if (verbose & i %% 10 == 0){ cat(i," ") }
  }
  
  # Return vector of sizes for the biggest cluster in N random permutations:
  return(Size)
}



