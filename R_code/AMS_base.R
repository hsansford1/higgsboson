AMS_base <- function(s, b, b_reg=10){

  # Objective function for challenge: approximate median significance
  # (eqn 7 in https://higgsml.lal.in2p3.fr/files/2014/04/documentation_v1.8.pdf)

  # Takes luminosity-normalised true and false positive rates
  # (s and b respectively) and regularisation term b_reg (=10 by default),
  # returns real-valued AMS, or complains

  AMS_sq <- (s+b+b_reg) * log(1 + (s/(b+b_reg))) - s

  if(AMS_sq < 0){
    stop('AMS squared is negative')
  }

  return( sqrt(AMS_sq) )
}

ams_fromWeights <- function(predicted_outcomes, actual_outcomes, weights){

  # # CHECKS ARGUMENTS - Taken this out for now, too unwieldy
  # # takes 3 vectors of equal length and computes AMS
  #
  # # predicted_outcomes and actual_outcomes must be:
  # # - boolean   (signal==TRUE, background==FALSE), or
  # # - numeric   (signal==1,    background==0),     or
  # # - character (signal=="s",  background=="b"),   or
  # # - factor    (signal=="s",  background=="b")
  # #
  # # NB: types are checked,
  # #     but whether elements are (TRUE, FALSE, 1, 0, 's', 'b') is NOT checked
  #
  # # weights must be numeric
  #
  # if(!( length(predicted_outcomes)==length(actual_outcomes)
  #       &
  #       length(predicted_outcomes)==length(weights) )){
  #   stop('Lengths of all arguments must be equal.')
  # }
  #
  # if(!( any(is.logical(predicted_outcomes), is.numeric(predicted_outcomes), is.character(predicted_outcomes), is.factor(predicted_outcomes))
  #       &
  #       any(is.logical(actual_outcomes), is.numeric(actual_outcomes), is.character(actual_outcomes), is.factor(actual_outcomes))
  # )){
  #   stop('predicted_outcomes and actual_outcomes must be logical, numeric, character or factor.')
  # }
  #
  # if(!is.numeric(weights)){
  #   stop('weights must be numeric.')
  # }

  # predictions & outcomes should be binary,
  # throws error  if more than 2 categories present in either field,
  # gives warning if less than 2 categories present in either field
  if(length(unique(predicted_outcomes))>2 | length(unique(actual_outcomes))>2){
    stop('predicted_outcomes and/or actual_outcomes contain more than two categories')
  } else if(length(unique(predicted_outcomes))<2 | length(unique(actual_outcomes))<2){
    warning(paste0('predicted_outcomes and actual_outcomes contain ',
                   length(unique(predicted_outcomes)),
                   ' and ',
                   length(unique(actual_outcomes)),
                   ' categories respectively.')
    )
  }

  # put vectors in tibble
  df <- tibble('pred'=predicted_outcomes, 'act'=actual_outcomes, 'wt'=weights)

  # only interested in observations predicted to be true
  if(is.logical(df$pred) | is.numeric(df$pred)){
    df <- df %>% filter(pred!=0)
  } else if(is.character(df$pred) | is.factor(df$pred)){
    df <- df %>% filter(pred=='s')
  }

  # get sums of weights of true and false positives
  df <- df %>% group_by(act) %>% summarise(wt = sum(wt))

  # extract s (sum of true positive weights) and b (sum of false positive weights)
  if(is.logical(df$act) | is.numeric(df$act)){
    s <- filter(df, act!=0)$wt
    b <- filter(df, act==0)$wt
  } else if(is.character(df$act) | is.factor(df$act)){
    s <- filter(df, act!='s')$wt
    b <- filter(df, act=='b')$wt
  }

  ams <- AMS_base(s, b)
  return(ams)
}
