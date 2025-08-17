## =========================================================
## GPV Endowment (Before Premium) - INTERAKTIF (Input WAJIB ISI)
## =========================================================

## ---- Fungsi input wajib ----
ask_num_req <- function(prompt) {
  repeat {
    ans <- readline(sprintf("%s: ", prompt))
    val <- suppressWarnings(as.numeric(gsub("[^0-9.,-]", "", ans)))
    if (!is.na(val)) return(val)
    cat("??? Input tidak valid, silakan masukkan angka.\n")
  }
}

ask_pct_req <- function(prompt) {
  repeat {
    ans <- readline(sprintf("%s (dalam persen): ", prompt))
    ans <- gsub("%", "", ans)
    val <- suppressWarnings(as.numeric(gsub("[^0-9.,-]", "", ans)))
    if (!is.na(val)) return(val/100)
    cat("??? Input tidak valid, silakan masukkan angka.\n")
  }
}

format_rp <- function(x) paste0("Rp ", format(round(x, 0), big.mark=".", decimal.mark=","))

## ---- 0) INPUT PARAMETER ----
cat("=== INPUT PARAMETER PRODUK (WAJIB ISI) ===\n")
x   <- ask_num_req("Usia masuk (tahun)")                     
n   <- ask_num_req("Masa pertanggungan, n (tahun)")           
m   <- ask_num_req("Masa bayar premi, m (tahun)")             
UP  <- ask_num_req("Uang Pertanggungan (rupiah)")             
i   <- ask_pct_req("Bunga efektif tahunan")                   
F   <- ask_num_req("Biaya admin flat F per pembayaran (rupiah)")
benef_maturity_ratio <- ask_pct_req("Rasio manfaat akhir (mis. 20 untuk 20%)")

if (m > n) stop("??? Masa bayar m tidak boleh melebihi masa pertanggungan n.")

v <- 1/(1+i)

## ---- 1) Baca TMI ----
message("Pilih file CSV TMI (kolom minimal: x, qx, px, lx)...")
TMI_path <- file.choose()

read_tmi_auto <- function(path){
  try_read <- function(fn) tryCatch(fn(), error=function(e) NULL)
  cands <- list(
    try_read(function() read.csv(path, stringsAsFactors=FALSE)),
    try_read(function() read.csv(path, sep=";", stringsAsFactors=FALSE)),
    try_read(function() read.delim(path, stringsAsFactors=FALSE))
  )
  df <- NULL
  for (d in cands) if (is.data.frame(d) && ncol(d) >= 2) { df <- d; break }
  if (is.null(df)) stop("Gagal membaca CSV. Cek pemisah (koma/; /tab).")
  
  nm <- tolower(gsub("[^a-z0-9]", "", names(df))); names(df) <- nm
  ren <- function(df, alts, tgt){ hit <- which(names(df) %in% alts); if (length(hit)) names(df)[hit[1]] <- tgt; df }
  df <- ren(df, c("x","age"), "x")
  df <- ren(df, c("qx","q","q_x","qxmale","qxfemale"), "qx")
  df <- ren(df, c("px","p","p_x"), "px")
  df <- ren(df, c("lx","l","l_x"), "lx")
  
  need <- c("x","qx","px","lx")
  if (!all(need %in% names(df))) stop(sprintf("Kolom kurang. Wajib: %s", paste(need, collapse=", ")))
  
  df <- df[order(df$x), ]
  for (nm in c("x","qx","px","lx")) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  if (any(is.na(df$px))) df$px <- 1 - df$qx
  df$qx <- pmin(pmax(df$qx, 0), 1)
  df$px <- pmin(pmax(df$px, 0), 1)
  df
}

TMI <- read_tmi_auto(TMI_path)

## ---- 2) Subset usia relevan ----
ages_needed <- x:(x + n + 1)
if (any(!(ages_needed %in% TMI$x))) {
  missing <- ages_needed[!(ages_needed %in% TMI$x)]
  stop(sprintf("Tabel mortalitas tidak mencakup usia: %s", paste(missing, collapse=", ")))
}
TMI <- TMI[TMI$x %in% ages_needed, ]

## ---- 3) Fungsi aktuaria ----
px_map <- setNames(TMI$px, TMI$x)
qx_map <- setNames(TMI$qx, TMI$x)

npx <- function(x0, k){
  if (k <= 0) return(1)
  prod(px_map[as.character(x0 + 0:(k-1))])
}

a_double <- function(x0, r){
  if (r <= 0) return(0)
  ks <- 0:(r-1)
  sum(v^ks * sapply(ks, function(k) npx(x0, k)))
}

A_temp_discrete <- function(x0, r){
  if (r <= 0) return(0)
  ks <- 0:(r-1)
  sum(v^(ks+1) * sapply(ks, function(k) npx(x0, k)) *
        sapply(ks, function(k) qx_map[as.character(x0 + k)]))
}

PV_endowment_from_t <- function(x0, n_total, t, ratio){
  remain <- n_total - t
  if (remain <= 0) return(0)
  ratio * UP * (v^remain) * npx(x0 + t, remain)
}

## ---- 4) PV komponen @ t=0, NP & GP ----
PV_death_0    <- UP * A_temp_discrete(x, n)
PV_maturity_0 <- PV_endowment_from_t(x, n_total = n, t = 0, ratio = benef_maturity_ratio)
denom_0       <- a_double(x, m)             

NP <- (PV_death_0 + PV_maturity_0) / denom_0 
PV_admin_0 <- F * denom_0
GP <- (PV_death_0 + PV_maturity_0 + PV_admin_0) / denom_0  

## ---- 5) Tabel GPV Year 0..n ----
years <- 0:n
Inflow <- PV_Death <- PV_Endowment <- Nominal_Endowment <- Admin <- Outflow <- V_before <- numeric(length(years))

for (t in years) {
  Inflow[t+1] <- if (t < m) GP * a_double(x + t, m - t) else 0
  Admin[t+1]  <- if (t < m) F  * a_double(x + t, m - t) else 0
  
  remain_cov <- n - t
  PV_Death[t+1] <- if (remain_cov > 0) UP * A_temp_discrete(x + t, remain_cov) else 0
  
  if (t == n) {
    PV_Endowment[t+1]      <- benef_maturity_ratio * UP
    Nominal_Endowment[t+1] <- benef_maturity_ratio * UP
  } else {
    PV_Endowment[t+1]      <- PV_endowment_from_t(x, n_total = n, t = t, ratio = benef_maturity_ratio)
    Nominal_Endowment[t+1] <- 0
  }
  
  Outflow[t+1]  <- PV_Death[t+1] + PV_Endowment[t+1] + Admin[t+1]
  V_before[t+1] <- Outflow[t+1] - Inflow[t+1]
}

result <- data.frame(
  Year = years,
  Inflow = Inflow,
  PV_Death = PV_Death,
  PV_Endowment = PV_Endowment,
  Nominal_Endowment = Nominal_Endowment,
  Admin = Admin,
  Outflow = Outflow,
  `Outflow - Inflow (V_t^-)` = V_before,
  check.names = FALSE
)

## ---- 6) Premi per tahun ----
years_pay <- 0:n
NP_per_year <- ifelse(years_pay < m, NP, 0)
GP_per_year <- ifelse(years_pay < m, GP, 0)

prem_table <- data.frame(
  Year = years_pay,
  NP_per_year = NP_per_year,
  GP_per_year = GP_per_year,
  check.names = FALSE
)

## ---- 7) CETAK HASIL ----
cat("\n==== PARAMETER (HASIL INPUT) ====\n")
cat(sprintf("Usia x=%d, n=%d tahun, m=%d tahun\n", x, n, m))
cat(sprintf("UP=%s, F=%s, i=%.4f (v=%.8f), Endowment=%.0f%%%% UP\n",
            format_rp(UP), format_rp(F), i, v, 100*benef_maturity_ratio))

cat("\n==== PREMI ====\n")
cat("Premi Bersih (NP) : ", format_rp(NP), " per tahun\n", sep = "")
cat("Premi Kotor (GP)  : ", format_rp(GP), " per tahun  (admin flat F di-cover)\n", sep = "")

cat("\n==== TABEL PREMI PER TAHUN (0..", n, ") ====\n", sep = "")
print(
  transform(prem_table,
            NP_per_year = format_rp(NP_per_year),
            GP_per_year = format_rp(GP_per_year)),
  row.names = FALSE
)

cat("\n==== TABEL GPV (Before Premium) ====\n")
print(
  transform(result,
            Inflow = format_rp(Inflow),
            PV_Death = format_rp(PV_Death),
            PV_Endowment = format_rp(PV_Endowment),
            Nominal_Endowment = format_rp(Nominal_Endowment),
            Admin = format_rp(Admin),
            Outflow = format_rp(Outflow),
            `Outflow - Inflow (V_t^-)` = format_rp(`Outflow - Inflow (V_t^-)`)
  ),
  row.names = FALSE
)