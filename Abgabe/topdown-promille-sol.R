# The tell_me_how_drunk functions calculates the blood alcohol concentration
# after Widmark and Whatson for human beings. Its inputs are the age in years,
# wheater female or male "drinker", the heigth in cm, the weight in kg. The
# drinking time is specified as a POSIXct time vector with the beginning time
# (first drink) and the end time (last drink). Further the consumed drinks
# should be passed in as a list or a vector. The function knows the drinks
# "massn", "hoibe", "schnaps" and "wein" and their average alcohol density.
# The function outputs an estimation of the drinkers blood alcohol concentration
# for the consumed drinks depending on the pre-mentioned inputs

tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  checkmate::assert_numeric(age,
    lower = 14, upper = 99, any.missing = FALSE,
    len = 1,
  )
  checkmate::assert_numeric(height,
    lower = 100, upper = 220, any.missing = FALSE,
    len = 1
  )
  checkmate::assert_numeric(weight,
    lower = 30, upper = 350, any.missing = FALSE,
    len = 1
  )
  checkmate::assert(checkmate::check_list(drinks),
    checkmate::check_atomic_vector(drinks),
    combine = "or"
  )
  checkmate::assert_posixct(drinking_time)
  checkmate::assert_names(names(drinks), subset.of = c(
    "massn", "hoibe",
    "wein", "schnaps"
  ))

  # unlist to make sure lists will be converted into vectors
  drinks <- unlist(drinks)

  # lower characters to have a unique format
  sex <- tolower(sex)

  # make sex matchable
  sex <- match.arg(sex)

  # give out a warning is drinker is under 16 (illegal to consume any type of
  # alcoholics) or if drinker is under 18 and consuming strong alcoholics (schnaps)
  if (age < 16 | (age < 18 & "schnaps" %in% names(drinks))) {
    warning("illegal")
  }

  # calculate the consumed mass of alcohol out of drinks drunken
  alcohol_mass <- calculate_alcohol_mass(drinks)

  # calculate the total bodywater depending on drinkers sex, age, height and weight
  total_body_water <- calculate_total_body_water(sex, age, height, weight)

  # calculate the expected alcohol density of the drinker after Widmark and
  # Watson forumla
  widmark_watson_estimator <- calculate_widmark_watson_estimator(
    alcohol_mass,
    total_body_water
  )

  # correct the Widmark/Watson estimator for time between first and last drink
  corrected_ww <- calculate_corrected_ww_estimator(
    widmark_watson_estimator,
    drinking_time
  )


  corrected_ww
}


# This function gives out the total body water, which is needed to calculate
# WW estimator.
calculate_total_body_water <- function(sex, age, height, weight) {

  # calcullate total body water in case drinker is male
  if (sex == "male") {
    total_body_water <- 2.447 - 0.09516 * age + 0.1074 * height +
      0.3362 * weight

    return(total_body_water)
  }

  # or in case drinker is female
  total_body_water <- 0.203 - 0.07 * age + 0.1069 * height +
    0.2466 * weight

  total_body_water
}


# This function calculate the alcohol mass take from the drunken drinks

calculate_alcohol_mass <- function(drinks) {

  # rho is a fixed parameter in the calculation
  rho <- 0.8

  # the following will calculate the alcohol mass of the given drinks.
  # in case the drinker didn't have a drink of a certain kind, it is set to 0

  # calcaulte alcohol mass out of drinkers consumed "massn"
  if ("massn" %in% names(drinks)) {
    alcohol_massn <- sum(drinks[names(drinks) == "massn"]) * (1000 * 0.06 * rho)
  } else {
    alcohol_massn <- 0
  }

  # same for "hoibe"
  if ("hoibe" %in% names(drinks)) {
    alcohol_hoibe <- sum(drinks[names(drinks) == "hoibe"]) * (500 * 0.06 * rho)
  } else {
    alcohol_hoibe <- 0
  }

  # same for "wein"
  if ("wein" %in% names(drinks)) {
    alcohol_wein <- sum(drinks[names(drinks) == "wein"]) * (200 * 0.11 * rho)
  } else {
    alcohol_wein <- 0
  }

  # and "schnaps"
  if ("schnaps" %in% names(drinks)) {
    alcohol_schnaps <- sum(drinks[names(drinks) == "schnaps"]) * (40 * 0.4 * rho)
  } else {
    alcohol_schnaps <- 0
  }

  # calculate the total alcohol mass over all drinks consumed
  alcohol_mass <- alcohol_massn + alcohol_hoibe + alcohol_wein +
    alcohol_schnaps

  alcohol_mass
}

# This function will give back the estimator after WW
calculate_widmark_watson_estimator <- function(alcohol_mass, total_body_water) {

  # rho is a fixed parameter
  rho_blood <- 1.055

  # calculate the WW formula
  widmark_watson_estimator <- ((0.8 * alcohol_mass) /
    (rho_blood * total_body_water))

  # give back estimation
  widmark_watson_estimator
}

# This function will give back the WW estimator corrected by time
calculate_corrected_ww_estimator <- function(widmark_watson_estimator, drink_time) {

  # calculate the time difference between last and first drink
  time_difference <- difftime(drink_time[2], drink_time[1], units = "hours")

  # convert the difference into numerical so we can plug it in the formula
  time_difference <- as.numeric(time_difference)

  # in the beginning we set the corrected WW estimator equal to the WW estimator
  # as it is not sure that correction will really happen
  corrected_ww_estimator <- widmark_watson_estimator

  # if the difference in time is more then one hour, the correction is made
  if (time_difference > 1) {
    corrected_ww_estimator <- widmark_watson_estimator - ((time_difference - 1) * 0.15)
  }

  # in case we would obtain a negative blood alcohol density we set the
  # corrected WW estimator to 0, as a negative blood alcohol density is impossible
  if (corrected_ww_estimator < 0) {
    corrected_ww_estimator <- 0
  }

  # give back the corrected WW estimatio for the blood alcohol density
  corrected_ww_estimator
}
