# Load mortality table
tmi <- read.csv("TMI2011L.csv", sep = ";")

# PARAMETERS
age_start <- 35
age_end <- 75
years <- age_end - age_start + 1
v <- 1 / (1 + 0.04)  # discount rate 4%
benefit <- 400000000
premium_years <- 15
fixed_cost <- 1000000 + 50000

# SUBSET MORTALITY TABLE
subset <- tmi[tmi$x >= age_start & tmi$x <= age_end, ]
qx <- subset$qx
px <- subset$px

# FUNCTION: Survival probability
tpx <- function(t) {
  if (t == 0) return(1)
  return(prod(px[1:t]))
}

# FUNCTION: Benefit by year
benefit_at <- function(t) {
  if (t < 5) return(0.2 * benefit)
  else if (t < 10) return(0.4 * benefit)
  else if (t < 15) return(0.6 * benefit)
  else if (t < 20) return(0.8 * benefit)
  else return(benefit)
}

# FUNCTION: Net PV of benefits
pv_benefit <- function() {
  total <- 0
  for (t in 0:(years - 1)) {
    total <- total + v^t * tpx(t) * qx[t + 1] * benefit_at(t)
  }
  return(total)
}

# FUNCTION: Annuity present value
pv_annuity <- function(G, n) {
  total <- 0
  for (t in 0:(n - 1)) {
    total <- total + v^t * tpx(t) * G
  }
  return(total)
}

# FUNCTIONS: admin & commission
pv_admin <- function(G, from_t = 0) {
  total <- 0
  for (t in from_t:(years - 1)) {
    total <- total + v^(t - from_t) * tpx(t - from_t) * G
  }
  return(0.01 * total)
}

pv_commission <- function(G, from_t = 0) {
  if (from_t >= premium_years) return(0)
  total <- 0
  for (t in from_t:(premium_years - 1)) {
    total <- total + v^(t - from_t) * tpx(t - from_t) * G
  }
  return(0.1 * total)
}

# LOSS FUNCTION to solve for gross premium
loss_func <- function(G) {
  inflow <- pv_annuity(G, premium_years)
  outflow <- pv_benefit() + pv_admin(G) + pv_commission(G) + fixed_cost
  return(outflow - inflow)
}

# SOLVE Gross Premium
gross_premium <- uniroot(loss_func, c(1000000, 10000000))$root

# CALCULATE Net Premium
net_premium <- pv_benefit() / pv_annuity(1, premium_years)

# Initialize result table
result <- data.frame(
  Year = 0:(years - 1),
  Inflow = numeric(years),
  Benefit = numeric(years),
  Commission = numeric(years),
  Admin = numeric(years),
  Fixed_Cost = numeric(years),
  Outflow = numeric(years),
  GPV = numeric(years)
)

# LOOP per year t = 0 to 40
for (t in 0:(years - 1)) {
  inflow <- if (t < premium_years) pv_annuity(gross_premium, premium_years - t) else 0
  
  # Future benefit
  benefit_total <- 0
  for (k in t:(years - 1)) {
    benefit_total <- benefit_total + v^(k - t) * tpx(k - t) * qx[k + 1] * benefit_at(k)
  }
  
  # Future admin & commission from year t
  commission <- pv_commission(gross_premium, from_t = t)
  admin <- pv_admin(gross_premium, from_t = t)
  fc <- ifelse(t == 0, fixed_cost, 0)
  
  outflow <- benefit_total + commission + admin + fc
  reserve <- outflow - inflow
  
  result[t + 1, ] <- c(t, round(inflow), round(benefit_total), round(commission),
                       round(admin), round(fc), round(outflow), round(reserve))
}

# DISPLAY RESULTS
cat("Gross Premium: Rp", round(gross_premium), "\n")
cat("Net Premium: Rp", round(net_premium), "\n")
print(result)

