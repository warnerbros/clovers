## code to prepare `customer_lifetimes` dataset

# Generate some fake data for inputs to transform_individual().
#
# INPUTS:
#   a - numeric, the "a" parameter of the beta distribution
#   b - numeric, the "b" parameter of the beta distribution
#   tmax - max number of periods
#   nsamps - number of rows to generate
# OUTPUT:
#   data frame with columns t and is_active
generate_fake_data = function(a=0.7, b=1.2, tmax = 12, nsamps=1000){

  # Sample actual lifetimes from the (hierarchical) distribution
  set.seed(1)
  p_samp_floor = 1e-5
  psamps = rbeta(nsamps, a, b)
  psamps = pmax(psamps, p_samp_floor)
  tsamps = rgeom(nsamps, prob=psamps) + 1

  # Simulate censoring by randomly generating
  # an age of the customer
  age = sample(tmax, nsamps, replace=TRUE)
  is_active = as.integer(tsamps > age)

  df = data.frame(t=tsamps,is_active=is_active, age=age)

  # If the true lifetime is greater than age, truncate
  # the lifetime at age.
  df$t[df$is_active==1] = df$age[df$is_active==1]

  return(df[,c("t","is_active")])
}

customer_lifetimes = generate_fake_data()

usethis::use_data(customer_lifetimes)
