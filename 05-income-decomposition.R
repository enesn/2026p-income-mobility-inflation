## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

# Every FKIMLIK contributes exactly four consecutive rows once the frame is sorted, so a
# within-person lag is a whole-column shift with the first k rows of each person blanked out.
# That is what lag_person() below does, and it is why the margin and delta blocks need no
# group_by() at all. Household sums go through rowsum(), which sums the whole matrix of
# person-level deltas in one call. Results are identical to the grouped version.

## ==================================================================================================#

income_decomposed <- income_defined %>%
  group_by(FB010) %>%
  mutate(
    deciles2 = weighted_ntile(equal_split_income, as.numeric(FK060_4), 10),
    deciles1 = weighted_ntile(equal_split_income, as.numeric(FK060_2), 10),
  ) %>%
  ungroup() %>%
  arrange(FKIMLIK, FB010)

# Rows are not reordered, added or dropped anywhere below this point, so the lagger stays valid.
lag_person <- local({
  id  <- as.character(income_decomposed$FKIMLIK)
  n   <- length(id)
  pos <- sequence(rle(id)$lengths)   # row number within person
  function(x, k) {
    out <- x[c(rep(NA_integer_, k), seq_len(n - k))]
    out[pos <= k] <- NA
    out
  }
})

income_decomposed <- income_decomposed %>%
  mutate(
    decile_prev3 = lag_person(deciles2, 3),
    decile_prev1 = lag_person(deciles2, 1),
    decile_prev2 = lag_person(deciles2, 2),

    decile1_prev3 = lag_person(deciles1, 3),
    decile1_prev1 = lag_person(deciles1, 1),

    equal_split_prev1 = lag_person(equal_split_income, 1),
    financial_split_prev1 = lag_person(equal_split_financial, 1),
    rental_split_prev1 = lag_person(equal_split_rental, 1),
    employer_split_prev1 = lag_person(equal_split_employer, 1),
    selfemployer_split_prev1 = lag_person(equal_split_selfemployer, 1),
    imprent_split_prev1 = lag_person(equal_split_imprent, 1),
    labor_split_prev1 = lag_person(equal_split_labor, 1),
    pension_split_prev1 = lag_person(equal_split_pension, 1),
    transfer_split_prev1 = lag_person(equal_split_transfers, 1),

    equal_split_prev3 = lag_person(equal_split_income, 3),
    financial_split_prev3 = lag_person(equal_split_financial, 3),
    rental_split_prev3 = lag_person(equal_split_rental, 3),
    # entrp_split_prev3 = lag_person(equal_split_entrp, 3),
    employer_split_prev3 = lag_person(equal_split_employer, 3),
    selfemployer_split_prev3 = lag_person(equal_split_selfemployer, 3),
    imprent_split_prev3 = lag_person(equal_split_imprent, 3),
    labor_split_prev3 = lag_person(equal_split_labor, 3),
    pension_split_prev3 = lag_person(equal_split_pension, 3),
    transfer_split_prev3 = lag_person(equal_split_transfers, 3)
  ) %>%

  mutate(

    upward_2yr = ifelse(deciles2 > decile_prev1, 1, 0),
    downward_2yr = ifelse(deciles2 < decile_prev1, 1, 0),
    notransition_2yr = ifelse(deciles2 == decile_prev1, 1, 0),

    upward_4yr = ifelse(deciles2 > decile_prev3, 1, 0),
    downward_4yr = ifelse(deciles2 < decile_prev3, 1, 0),
    notransition_4yr = ifelse(deciles2 == decile_prev3, 1, 0)

  ) %>%

  mutate(equal_split_imprent = ifelse(is.na(equal_split_imprent), 0, equal_split_imprent))%>%
  mutate(
    income_year = as.numeric(FB010) - 1
  )

# n_distinct() per household, without paying the per-group call: hash a single key built from
# the household and FI010 codes, then count the surviving rows per household.
income_decomposed$FI010_HH_same <- local({
  hh <- match(income_decomposed$HKIMLIK, unique(income_decomposed$HKIMLIK))
  fi <- match(income_decomposed$FI010, unique(income_decomposed$FI010))
  key <- as.double(hh) * (max(fi) + 1) + fi
  n_uniq <- tabulate(hh[!duplicated(key)], nbins = max(hh))
  n_uniq[hh] == 1
})

income_decomposed <- income_decomposed %>%

  select(ALTORN, FKIMLIK, HKIMLIK, income_year, FK060_4,
         equal_split_income, equivalent_income, deciles2,decile_prev3, upward_4yr,
         hh_size, FI010, FI130, FI140,downward_4yr,


          personal_pension_income, personal_transfer_income, personal_labor_income,
         personal_employer_income, personal_selfemployer_income,
         hh_rental_income, hh_financial_income,


         hh_full_transfers, hh_transfer_income,imputed_rent,



         equal_split_income, equal_split_employer, equal_split_selfemployer,  equal_split_financial, equal_split_imprent,
         equal_split_rental, equal_split_labor, equal_split_pension, equal_split_transfers, everything()

  )  %>%
  mutate(hh_direct_transfers = hh_full_transfers - hh_transfer_income)


## ==================================================================================================#
# Extensive/intensive margins
## ==================================================================================================#

# The four-way classification is a 2x2 on (current > 0, lagged > 0). Encoding it as an index
# replaces the four-deep nested ifelse() chains: NA in either argument propagates through the
# index, and a negative value on either side falls through to NA, exactly as the chains did.
MARGIN_LABELS <- c("No change", "Entry", "Exit", "Intensive")

margin_class <- function(x, x_lag) {
  out <- MARGIN_LABELS[(x > 0) + 2L * (x_lag > 0) + 1L]
  out[which(x < 0 | x_lag < 0)] <- NA_character_
  out
}

delta_at <- function(margin, level, x, x_lag) ifelse(margin == level, x - x_lag, 0)

# Variant in which the "No change" cell is tested against a different lag than the other three
# cells. Only needed to reproduce personal_selfemployer_margin2; see the note at the bottom.
margin_class_nochange_lag <- function(x, x_lag, x_lag_nochange) {
  out <- margin_class(x, x_lag)
  # The original chain falls through to its last branch whenever the first three tests are all
  # FALSE, i.e. x == 0 and x_lag <= 0, and that branch consults x_lag_nochange rather than x_lag.
  final <- which(x == 0 & x_lag <= 0)
  l <- x_lag_nochange[final]
  out[final] <- ifelse(!is.na(l) & l == 0, "No change", NA_character_)
  out
}

# Each lag is computed once here and reused by both the margin and the delta blocks.
margin_vars <- c("personal_labor_income", "personal_pension_income", "personal_employer_income",
                 "personal_selfemployer_income", "personal_transfer_income", "hh_rental_income",
                 "hh_financial_income", "hh_direct_transfers", "imputed_rent")

L1 <- lapply(margin_vars, function(v) lag_person(income_decomposed[[v]], 1))
L3 <- lapply(margin_vars, function(v) lag_person(income_decomposed[[v]], 3))
names(L1) <- names(L3) <- margin_vars

income_decomposed <- income_decomposed %>%
  mutate(
    personal_labor_margin4 = margin_class(personal_labor_income, L3$personal_labor_income),
    personal_labor_margin2 = margin_class(personal_labor_income, L1$personal_labor_income),

    personal_pension_margin4 = margin_class(personal_pension_income, L3$personal_pension_income),
    personal_pension_margin2 = margin_class(personal_pension_income, L1$personal_pension_income),

    personal_employer_margin4 = margin_class(personal_employer_income, L3$personal_employer_income),
    personal_employer_margin2 = margin_class(personal_employer_income, L1$personal_employer_income),

    personal_selfemployer_margin4 = margin_class(personal_selfemployer_income, L3$personal_selfemployer_income),
    # NOTE: the "No change" cell of this one is tested against the 3-period lag rather than the
    # 1-period lag. That is carried over verbatim from the original nested ifelse() chain; see the
    # note at the bottom of this file before changing it.
    personal_selfemployer_margin2 = margin_class_nochange_lag(
      personal_selfemployer_income,
      L1$personal_selfemployer_income,
      L3$personal_selfemployer_income),

    personal_transfer_margin4 = margin_class(personal_transfer_income, L3$personal_transfer_income),
    # NOTE: built on the 3-period lag, i.e. identical to personal_transfer_margin4. Carried over
    # verbatim from the original; see the note at the bottom of this file.
    personal_transfer_margin2 = margin_class(personal_transfer_income, L3$personal_transfer_income),

    hh_rental_margin4 = margin_class(hh_rental_income, L3$hh_rental_income),
    hh_rental_margin2 = margin_class(hh_rental_income, L1$hh_rental_income),

    hh_financial_margin4 = margin_class(hh_financial_income, L3$hh_financial_income),
    hh_financial_margin2 = margin_class(hh_financial_income, L1$hh_financial_income),

    hh_transfers_margin4 = margin_class(hh_direct_transfers, L3$hh_direct_transfers),
    hh_transfers_margin2 = margin_class(hh_direct_transfers, L1$hh_direct_transfers),

    imputed_rent_margin4 = margin_class(imputed_rent, L3$imputed_rent),
    imputed_rent_margin2 = margin_class(imputed_rent, L1$imputed_rent)

  ) %>%

  mutate(
    p_pension_delta_intensive4 = delta_at(personal_pension_margin4, "Intensive", personal_pension_income, L3$personal_pension_income),
    p_labor_delta_intensive4 = delta_at(personal_labor_margin4, "Intensive", personal_labor_income, L3$personal_labor_income),
    p_employer_delta_intensive4 = delta_at(personal_employer_margin4, "Intensive", personal_employer_income, L3$personal_employer_income),
    p_selfemployer_delta_intensive4 = delta_at(personal_selfemployer_margin4, "Intensive", personal_selfemployer_income, L3$personal_selfemployer_income),
    p_transfer_delta_intensive4 = delta_at(personal_transfer_margin4, "Intensive", personal_transfer_income, L3$personal_transfer_income),

    p_pension_delta_intensive2 = delta_at(personal_pension_margin2, "Intensive", personal_pension_income, L1$personal_pension_income),
    p_labor_delta_intensive2 = delta_at(personal_labor_margin2, "Intensive", personal_labor_income, L1$personal_labor_income),
    p_employer_delta_intensive2 = delta_at(personal_employer_margin2, "Intensive", personal_employer_income, L1$personal_employer_income),
    p_selfemployer_delta_intensive2 = delta_at(personal_selfemployer_margin2, "Intensive", personal_selfemployer_income, L1$personal_selfemployer_income),
    p_transfer_delta_intensive2 = delta_at(personal_transfer_margin2, "Intensive", personal_transfer_income, L1$personal_transfer_income),

    hh_rental_delta_intensive4 = delta_at(hh_rental_margin4, "Intensive", hh_rental_income, L3$hh_rental_income),
    hh_financial_delta_intensive4 = delta_at(hh_financial_margin4, "Intensive", hh_financial_income, L3$hh_financial_income),
    hh_transfer_delta_intensive4 = delta_at(hh_transfers_margin4, "Intensive", hh_direct_transfers, L3$hh_direct_transfers),
    hh_imprent_delta_intensive4 = delta_at(imputed_rent_margin4, "Intensive", imputed_rent, L3$imputed_rent),

    hh_rental_delta_intensive2 = delta_at(hh_rental_margin2, "Intensive", hh_rental_income, L1$hh_rental_income),
    hh_financial_delta_intensive2 = delta_at(hh_financial_margin2, "Intensive", hh_financial_income, L1$hh_financial_income),
    hh_transfer_delta_intensive2 = delta_at(hh_transfers_margin2, "Intensive", hh_direct_transfers, L1$hh_direct_transfers),
    hh_imprent_delta_intensive2 = delta_at(imputed_rent_margin2, "Intensive", imputed_rent, L1$imputed_rent),

    p_pension_delta_entry4 = delta_at(personal_pension_margin4, "Entry", personal_pension_income, L3$personal_pension_income),
    p_labor_delta_entry4 = delta_at(personal_labor_margin4, "Entry", personal_labor_income, L3$personal_labor_income),
    p_employer_delta_entry4 = delta_at(personal_employer_margin4, "Entry", personal_employer_income, L3$personal_employer_income),
    p_selfemployer_delta_entry4 = delta_at(personal_selfemployer_margin4, "Entry", personal_selfemployer_income, L3$personal_selfemployer_income),
    p_transfer_delta_entry4 = delta_at(personal_transfer_margin4, "Entry", personal_transfer_income, L3$personal_transfer_income),

    p_pension_delta_entry2 = delta_at(personal_pension_margin2, "Entry", personal_pension_income, L1$personal_pension_income),
    p_labor_delta_entry2 = delta_at(personal_labor_margin2, "Entry", personal_labor_income, L1$personal_labor_income),
    p_employer_delta_entry2 = delta_at(personal_employer_margin2, "Entry", personal_employer_income, L1$personal_employer_income),
    p_selfemployer_delta_entry2 = delta_at(personal_selfemployer_margin2, "Entry", personal_selfemployer_income, L1$personal_selfemployer_income),
    p_transfer_delta_entry2 = delta_at(personal_transfer_margin2, "Entry", personal_transfer_income, L1$personal_transfer_income),

    hh_rental_delta_entry4 = delta_at(hh_rental_margin4, "Entry", hh_rental_income, L3$hh_rental_income),
    hh_financial_delta_entry4 = delta_at(hh_financial_margin4, "Entry", hh_financial_income, L3$hh_financial_income),
    hh_transfer_delta_entry4 = delta_at(hh_transfers_margin4, "Entry", hh_direct_transfers, L3$hh_direct_transfers),
    hh_imprent_delta_entry4 = delta_at(imputed_rent_margin4, "Entry", imputed_rent, L3$imputed_rent),

    hh_rental_delta_entry2 = delta_at(hh_rental_margin2, "Entry", hh_rental_income, L1$hh_rental_income),
    hh_financial_delta_entry2 = delta_at(hh_financial_margin2, "Entry", hh_financial_income, L1$hh_financial_income),
    hh_transfer_delta_entry2 = delta_at(hh_transfers_margin2, "Entry", hh_direct_transfers, L1$hh_direct_transfers),
    hh_imprent_delta_entry2 = delta_at(imputed_rent_margin2, "Entry", imputed_rent, L1$imputed_rent),

    p_pension_delta_exit4 = delta_at(personal_pension_margin4, "Exit", personal_pension_income, L3$personal_pension_income),
    p_labor_delta_exit4 = delta_at(personal_labor_margin4, "Exit", personal_labor_income, L3$personal_labor_income),
    p_employer_delta_exit4 = delta_at(personal_employer_margin4, "Exit", personal_employer_income, L3$personal_employer_income),
    p_selfemployer_delta_exit4 = delta_at(personal_selfemployer_margin4, "Exit", personal_selfemployer_income, L3$personal_selfemployer_income),
    p_transfer_delta_exit4 = delta_at(personal_transfer_margin4, "Exit", personal_transfer_income, L3$personal_transfer_income),

    p_pension_delta_exit2 = delta_at(personal_pension_margin2, "Exit", personal_pension_income, L1$personal_pension_income),
    p_labor_delta_exit2 = delta_at(personal_labor_margin2, "Exit", personal_labor_income, L1$personal_labor_income),
    p_employer_delta_exit2 = delta_at(personal_employer_margin2, "Exit", personal_employer_income, L1$personal_employer_income),
    p_selfemployer_delta_exit2 = delta_at(personal_selfemployer_margin2, "Exit", personal_selfemployer_income, L1$personal_selfemployer_income),
    p_transfer_delta_exit2 = delta_at(personal_transfer_margin2, "Exit", personal_transfer_income, L1$personal_transfer_income),

    hh_rental_delta_exit4 = delta_at(hh_rental_margin4, "Exit", hh_rental_income, L3$hh_rental_income),
    hh_financial_delta_exit4 = delta_at(hh_financial_margin4, "Exit", hh_financial_income, L3$hh_financial_income),
    hh_transfer_delta_exit4 = delta_at(hh_transfers_margin4, "Exit", hh_direct_transfers, L3$hh_direct_transfers),
    hh_imprent_delta_exit4 = delta_at(imputed_rent_margin4, "Exit", imputed_rent, L3$imputed_rent),

    hh_rental_delta_exit2 = delta_at(hh_rental_margin2, "Exit", hh_rental_income, L1$hh_rental_income),
    hh_financial_delta_exit2 = delta_at(hh_financial_margin2, "Exit", hh_financial_income, L1$hh_financial_income),
    hh_transfer_delta_exit2 = delta_at(hh_transfers_margin2, "Exit", hh_direct_transfers, L1$hh_direct_transfers),
    hh_imprent_delta_exit2 = delta_at(imputed_rent_margin2, "Exit", imputed_rent, L1$imputed_rent)

  )

rm(L1, L3)


## ==================================================================================================#
# Equal-split shares of the deltas
## ==================================================================================================#

# The personal deltas are summed within household-year; the household-level ones are already at
# that level and only need dividing. rowsum() does all thirty sums in a single pass, replacing
# thirty grouped sum() calls.
person_delta_cols <- c(
  "p_pension_delta_intensive4", "p_labor_delta_intensive4", "p_employer_delta_intensive4",
  "p_selfemployer_delta_intensive4", "p_transfer_delta_intensive4",
  "p_pension_delta_intensive2", "p_labor_delta_intensive2", "p_employer_delta_intensive2",
  "p_selfemployer_delta_intensive2", "p_transfer_delta_intensive2",
  "p_pension_delta_entry4", "p_labor_delta_entry4", "p_employer_delta_entry4",
  "p_selfemployer_delta_entry4", "p_transfer_delta_entry4",
  "p_pension_delta_entry2", "p_labor_delta_entry2", "p_employer_delta_entry2",
  "p_selfemployer_delta_entry2", "p_transfer_delta_entry2",
  "p_pension_delta_exit4", "p_labor_delta_exit4", "p_employer_delta_exit4",
  "p_selfemployer_delta_exit4", "p_transfer_delta_exit4",
  "p_pension_delta_exit2", "p_labor_delta_exit2", "p_employer_delta_exit2",
  "p_selfemployer_delta_exit2", "p_transfer_delta_exit2"
)

HHY <- local({
  hh <- match(income_decomposed$HKIMLIK, unique(income_decomposed$HKIMLIK))
  yr <- as.integer(income_decomposed$income_year)
  g  <- (yr - min(yr)) * max(hh) + hh
  s  <- rowsum(as.matrix(income_decomposed[person_delta_cols]), group = g, na.rm = TRUE)
  s  <- s[match(g, as.integer(rownames(s))), , drop = FALSE]
  rownames(s) <- NULL   # otherwise the group labels ride along into the new columns
  s
})

income_decomposed <- income_decomposed %>%
  mutate(
    esplit_p_pension_delta_intensive4 = HHY[, "p_pension_delta_intensive4"]/hh_size,
    esplit_p_labor_delta_intensive4 = HHY[, "p_labor_delta_intensive4"]/hh_size,
    esplit_p_employer_delta_intensive4 = HHY[, "p_employer_delta_intensive4"]/hh_size,
    esplit_p_selfemployer_delta_intensive4 = HHY[, "p_selfemployer_delta_intensive4"]/hh_size,
    esplit_p_transfer_delta_intensive4 = HHY[, "p_transfer_delta_intensive4"]/hh_size,

    esplit_p_pension_delta_intensive2 = HHY[, "p_pension_delta_intensive2"]/hh_size,
    esplit_p_labor_delta_intensive2 = HHY[, "p_labor_delta_intensive2"]/hh_size,
    esplit_p_employer_delta_intensive2 = HHY[, "p_employer_delta_intensive2"]/hh_size,
    esplit_p_selfemployer_delta_intensive2 = HHY[, "p_selfemployer_delta_intensive2"]/hh_size,
    esplit_p_transfer_delta_intensive2 = HHY[, "p_transfer_delta_intensive2"]/hh_size,

    esplit_hh_rental_delta_intensive4  = hh_rental_delta_intensive4 /hh_size,
    esplit_hh_financial_delta_intensive4 = hh_financial_delta_intensive4/hh_size,
    esplit_hh_transfer_delta_intensive4 = hh_transfer_delta_intensive4/hh_size,
    esplit_hh_imprent_delta_intensive4 = hh_imprent_delta_intensive4/hh_size,

    esplit_hh_rental_delta_intensive2  = hh_rental_delta_intensive2 /hh_size,
    esplit_hh_financial_delta_intensive2 = hh_financial_delta_intensive2/hh_size,
    esplit_hh_transfer_delta_intensive2 = hh_transfer_delta_intensive2/hh_size,
    esplit_hh_imprent_delta_intensive2 = hh_imprent_delta_intensive2/hh_size,

    esplit_p_pension_delta_entry4  = HHY[, "p_pension_delta_entry4"]/hh_size,
    esplit_p_labor_delta_entry4 = HHY[, "p_labor_delta_entry4"]/hh_size,
    esplit_p_employer_delta_entry4 = HHY[, "p_employer_delta_entry4"]/hh_size,
    esplit_p_selfemployer_delta_entry4 = HHY[, "p_selfemployer_delta_entry4"]/hh_size,
    esplit_p_transfer_delta_entry4 = HHY[, "p_transfer_delta_entry4"]/hh_size,

    esplit_p_pension_delta_entry2 = HHY[, "p_pension_delta_entry2"]/hh_size,
    esplit_p_labor_delta_entry2 = HHY[, "p_labor_delta_entry2"]/hh_size,
    esplit_p_employer_delta_entry2 = HHY[, "p_employer_delta_entry2"]/hh_size,
    esplit_p_selfemployer_delta_entry2 = HHY[, "p_selfemployer_delta_entry2"]/hh_size,
    esplit_p_transfer_delta_entry2 = HHY[, "p_transfer_delta_entry2"]/hh_size,

    esplit_hh_rental_delta_entry4  = hh_rental_delta_entry4/hh_size,
    esplit_hh_financial_delta_entry4 =hh_financial_delta_entry4/hh_size,
    esplit_hh_transfer_delta_entry4 =hh_transfer_delta_entry4/hh_size,
    esplit_hh_imprent_delta_entry4 = hh_imprent_delta_entry4/hh_size,

    esplit_hh_rental_delta_entry2  = hh_rental_delta_entry2/hh_size,
    esplit_hh_financial_delta_entry2 =hh_financial_delta_entry2/hh_size,
    esplit_hh_transfer_delta_entry2 =hh_transfer_delta_entry2/hh_size,
    esplit_hh_imprent_delta_entry2 = hh_imprent_delta_entry2/hh_size,

    esplit_p_pension_delta_exit4  = HHY[, "p_pension_delta_exit4"]/hh_size,
    esplit_p_labor_delta_exit4 = HHY[, "p_labor_delta_exit4"]/hh_size,
    esplit_p_employer_delta_exit4 = HHY[, "p_employer_delta_exit4"]/hh_size,
    esplit_p_selfemployer_delta_exit4 = HHY[, "p_selfemployer_delta_exit4"]/hh_size,
    esplit_p_transfer_delta_exit4 = HHY[, "p_transfer_delta_exit4"]/hh_size,

    esplit_p_pension_delta_exit2  = HHY[, "p_pension_delta_exit2"]/hh_size,
    esplit_p_labor_delta_exit2 = HHY[, "p_labor_delta_exit2"]/hh_size,
    esplit_p_employer_delta_exit2 = HHY[, "p_employer_delta_exit2"]/hh_size,
    esplit_p_selfemployer_delta_exit2 = HHY[, "p_selfemployer_delta_exit2"]/hh_size,
    esplit_p_transfer_delta_exit2 = HHY[, "p_transfer_delta_exit2"]/hh_size,

    esplit_hh_rental_delta_exit4  = hh_rental_delta_exit4/hh_size,
    esplit_hh_financial_delta_exit4 = hh_financial_delta_exit4/hh_size,
    esplit_hh_transfer_delta_exit4 = hh_transfer_delta_exit4/hh_size,
    esplit_hh_imprent_delta_exit4 = hh_imprent_delta_exit4/hh_size,

    esplit_hh_rental_delta_exit2  = hh_rental_delta_exit2/hh_size,
    esplit_hh_financial_delta_exit2 = hh_financial_delta_exit2/hh_size,
    esplit_hh_transfer_delta_exit2 = hh_transfer_delta_exit2/hh_size,
    esplit_hh_imprent_delta_exit2 = hh_imprent_delta_exit2/hh_size

  ) %>%
  mutate(
    total_delta4 =
      abs(esplit_p_pension_delta_intensive4) +
      abs(esplit_p_labor_delta_intensive4) +
      abs(esplit_p_employer_delta_intensive4) +
      abs(esplit_p_selfemployer_delta_intensive4) +
      abs(esplit_p_transfer_delta_intensive4) +


      abs(esplit_hh_rental_delta_intensive4) +
      abs(esplit_hh_financial_delta_intensive4) +
      abs(esplit_hh_transfer_delta_intensive4) +
      abs(esplit_hh_imprent_delta_intensive4) +

      abs(esplit_p_pension_delta_entry4) +
      abs(esplit_p_labor_delta_entry4) +
      abs(esplit_p_employer_delta_entry4) +
      abs(esplit_p_selfemployer_delta_entry4) +
      abs(esplit_p_transfer_delta_entry4) +

      abs(esplit_hh_rental_delta_entry4) +
      abs(esplit_hh_financial_delta_entry4) +
      abs(esplit_hh_transfer_delta_entry4) +
      abs(esplit_hh_imprent_delta_entry4) +

      abs(esplit_p_pension_delta_exit4) +
      abs(esplit_p_labor_delta_exit4) +
      abs(esplit_p_employer_delta_exit4) +
      abs(esplit_p_selfemployer_delta_exit4) +
      abs(esplit_p_transfer_delta_exit4) +


      abs(esplit_hh_rental_delta_exit4) +
      abs(esplit_hh_financial_delta_exit4) +
      abs(esplit_hh_transfer_delta_exit4) +
      abs(esplit_hh_imprent_delta_exit4),




    total_delta2 =
      abs(esplit_p_pension_delta_intensive2) +
      abs(esplit_p_labor_delta_intensive2) +
      abs(esplit_p_employer_delta_intensive2) +
      abs(esplit_p_selfemployer_delta_intensive2) +
      abs(esplit_p_transfer_delta_intensive2) +


      abs(esplit_hh_rental_delta_intensive2) +
      abs(esplit_hh_financial_delta_intensive2) +
      abs(esplit_hh_transfer_delta_intensive2) +
      abs(esplit_hh_imprent_delta_intensive2) +

      abs(esplit_p_pension_delta_entry2) +
      abs(esplit_p_labor_delta_entry2) +
      abs(esplit_p_employer_delta_entry2) +
      abs(esplit_p_selfemployer_delta_entry2) +
      abs(esplit_p_transfer_delta_entry2) +

      abs(esplit_hh_rental_delta_entry2) +
      abs(esplit_hh_financial_delta_entry2) +
      abs(esplit_hh_transfer_delta_entry2) +
      abs(esplit_hh_imprent_delta_entry2) +

      abs(esplit_p_pension_delta_exit2) +
      abs(esplit_p_labor_delta_exit2) +
      abs(esplit_p_employer_delta_exit2) +
      abs(esplit_p_selfemployer_delta_exit2) +
      abs(esplit_p_transfer_delta_exit2) +


      abs(esplit_hh_rental_delta_exit2) +
      abs(esplit_hh_financial_delta_exit2) +
      abs(esplit_hh_transfer_delta_exit2) +
      abs(esplit_hh_imprent_delta_exit2),


    p_pension_intensive4_contribution = esplit_p_pension_delta_intensive4/total_delta4,
    p_labor_intensive4_contribution = esplit_p_labor_delta_intensive4/total_delta4,
    p_employer_intensive4_contribution = esplit_p_employer_delta_intensive4/total_delta4,
    p_selfemployer_intensive4_contribution = esplit_p_selfemployer_delta_intensive4/total_delta4,
    p_transfer_intensive4_contribution = esplit_p_transfer_delta_intensive4/total_delta4,


    p_pension_intensive2_contribution = esplit_p_pension_delta_intensive2/total_delta2,
    p_labor_intensive2_contribution = esplit_p_labor_delta_intensive2/total_delta2,
    p_employer_intensive2_contribution = esplit_p_employer_delta_intensive2/total_delta2,
    p_selfemployer_intensive2_contribution = esplit_p_selfemployer_delta_intensive2/total_delta2,
    p_transfer_intensive2_contribution = esplit_p_transfer_delta_intensive2/total_delta2,


    hh_rental_intensive4_contribution = esplit_hh_rental_delta_intensive4/total_delta4,
    hh_financial_intensive4_contribution = esplit_hh_financial_delta_intensive4/total_delta4,
    hh_transfer_intensive4_contribution = esplit_hh_transfer_delta_intensive4/total_delta4,
    hh_imprent_intensive4_contribution = esplit_hh_imprent_delta_intensive4/total_delta4,


    hh_rental_intensive2_contribution = esplit_hh_rental_delta_intensive2/total_delta2,
    hh_financial_intensive2_contribution = esplit_hh_financial_delta_intensive2/total_delta2,
    hh_transfer_intensive2_contribution = esplit_hh_transfer_delta_intensive2/total_delta2,
    hh_imprent_intensive2_contribution = esplit_hh_imprent_delta_intensive2/total_delta2,


    p_pension_entry4_contribution = esplit_p_pension_delta_entry4/total_delta4,
    p_labor_entry4_contribution = esplit_p_labor_delta_entry4/total_delta4,
    p_employer_entry4_contribution = esplit_p_employer_delta_entry4/total_delta4,
    p_selfemployer_entry4_contribution = esplit_p_selfemployer_delta_entry4/total_delta4,
    p_transfer_entry4_contribution = esplit_p_transfer_delta_entry4/total_delta4,


    p_pension_entry2_contribution = esplit_p_pension_delta_entry2/total_delta2,
    p_labor_entry2_contribution = esplit_p_labor_delta_entry2/total_delta2,
    p_employer_entry2_contribution = esplit_p_employer_delta_entry2/total_delta2,
    p_selfemployer_entry2_contribution = esplit_p_selfemployer_delta_entry2/total_delta2,
    p_transfer_entry2_contribution = esplit_p_transfer_delta_entry2/total_delta2,


    hh_rental_entry4_contribution = esplit_hh_rental_delta_entry4/total_delta4,
    hh_financial_entry4_contribution = esplit_hh_financial_delta_entry4/total_delta4,
    hh_transfer_entry4_contribution = esplit_hh_transfer_delta_entry4/total_delta4,
    hh_imprent_entry4_contribution = esplit_hh_imprent_delta_entry4/total_delta4,

    hh_rental_entry2_contribution = esplit_hh_rental_delta_entry2/total_delta2,
    hh_financial_entry2_contribution = esplit_hh_financial_delta_entry2/total_delta2,
    hh_transfer_entry2_contribution = esplit_hh_transfer_delta_entry2/total_delta2,
    hh_imprent_entry2_contribution = esplit_hh_imprent_delta_entry2/total_delta2,



    p_pension_exit4_contribution = esplit_p_pension_delta_exit4/total_delta4,
    p_labor_exit4_contribution = esplit_p_labor_delta_exit4/total_delta4,
    p_employer_exit4_contribution = esplit_p_employer_delta_exit4/total_delta4,
    p_selfemployer_exit4_contribution = esplit_p_selfemployer_delta_exit4/total_delta4,
    p_transfer_exit4_contribution = esplit_p_transfer_delta_exit4/total_delta4,

    p_pension_exit2_contribution = esplit_p_pension_delta_exit2/total_delta2,
    p_labor_exit2_contribution = esplit_p_labor_delta_exit2/total_delta2,
    p_employer_exit2_contribution = esplit_p_employer_delta_exit2/total_delta2,
    p_selfemployer_exit2_contribution = esplit_p_selfemployer_delta_exit2/total_delta2,
    p_transfer_exit2_contribution = esplit_p_transfer_delta_exit2/total_delta2,

    hh_rental_exit4_contribution = esplit_hh_rental_delta_exit4/total_delta4,
    hh_financial_exit4_contribution = esplit_hh_financial_delta_exit4/total_delta4,
    hh_transfer_exit4_contribution = esplit_hh_transfer_delta_exit4/total_delta4,
    hh_imprent_exit4_contribution = esplit_hh_imprent_delta_exit4/total_delta4,


    hh_rental_exit2_contribution = esplit_hh_rental_delta_exit2/total_delta2,
    hh_financial_exit2_contribution = esplit_hh_financial_delta_exit2/total_delta2,
    hh_transfer_exit2_contribution = esplit_hh_transfer_delta_exit2/total_delta2,
    hh_imprent_exit2_contribution = esplit_hh_imprent_delta_exit2/total_delta2
  ) %>%

  select(FKIMLIK, HKIMLIK, income_year, deciles2, FI010, FI130, FI140, FK060_4,
         personal_employer_income, personal_selfemployer_income, personal_pension_income, personal_transfer_income, personal_labor_income,
         hh_rental_income, hh_financial_income, upward_4yr, downward_4yr, deciles2, decile_prev3,

         equal_split_income, equivalent_income, imputed_rent, hh_imprent_delta_entry4, p_pension_intensive4_contribution:hh_imprent_exit4_contribution,
         esplit_p_pension_delta_intensive4:esplit_hh_imprent_delta_exit4, everything() ) %>%

  mutate(
    real_equal_split_prev3 = lag_person(real_equal_split, 3))

rm(HHY, lag_person, margin_class, margin_class_nochange_lag, delta_at,
   margin_vars, person_delta_cols, MARGIN_LABELS)


