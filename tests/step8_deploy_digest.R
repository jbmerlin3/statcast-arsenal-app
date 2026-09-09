# step8_deploy_digest.R
#
#   Rscript tests/step8_deploy_digest.R
#
# The deploy gate's `content` digest must answer "did any VALUE change" and
# nothing else. It was answering "did the file change", which is noisier in one
# specific direction: update_data.R folds with bind_rows(new_clean, sc), so
# every row in the re-pull window is lifted to the front in Savant's response
# order, and when that order shifts the digest shifts with it.
#
# Measured 2026-09-09 from the run artifacts for 10:15 and 15:05, both carrying
# app_data through 2026-09-08 at a518008: 641,065 x 33, identical up to row
# order, hashing to 3cca5ecdcb88 and 17018e2048e7. The gate redeployed twice for
# a byte-identical dataset. Canonicalised, both hash to 4c02f7dfd270.
#
# Cases 1 and 4 are the regression. Cases 2, 3, 5 and 6 are what stops the fix
# from being "return a constant", which would also pass case 1: a digest that
# ignores row order must still catch a changed value, a value SWAPPED between
# two rows (which leaves every column's multiset intact), an added row, and a
# value going NA.
suppressMessages(library(digest))
source("scripts/deploy.R")

d <- readRDS("tests/fixtures/pl_trim_702070.rds")
stopifnot(is.data.frame(d), nrow(d) > 10)
base <- digest_or_na(d)

# 1. Row order must not move the digest.
set.seed(1)
perm <- d[sample(nrow(d)), , drop = FALSE]
same_perm <- identical(digest_or_na(perm), base)

# 2. A changed value must move it.
# The most-varied numeric column, not the first. This fixture is ONE pitcher,
# so `pitcher` is numeric and constant, and a swap test on a constant column
# silently tests nothing.
nums <- names(d)[vapply(d, is.numeric, TRUE)]
num  <- nums[which.max(vapply(nums, function(k) length(unique(na.omit(d[[k]]))), 1L))]
bump <- d; bump[[num]][1] <- bump[[num]][1] + 1
diff_value <- !identical(digest_or_na(bump), base)

# 3. A value swapped between two rows must move it. This is the case a
#    per-column-sorted digest would miss: the column's multiset is unchanged.
swap <- d
ok_i <- which(!is.na(d[[num]]))
i <- c(ok_i[1], ok_i[d[[num]][ok_i] != d[[num]][ok_i[1]]][1])
stopifnot(!anyNA(i), d[[num]][i[1]] != d[[num]][i[2]])
swap[[num]][i] <- swap[[num]][rev(i)]
diff_swap <- !identical(digest_or_na(swap), base)

# 4. Row names alone must not move it. `[` carries the original positions
#    through as an attribute and digest() hashes attributes, which is why
#    sorting without normalising them still produced two hashes for one dataset.
renamed <- d
attr(renamed, "row.names") <- as.integer(seq_len(nrow(d)) + 1000L)
same_rownames <- identical(digest_or_na(renamed), base)

# 5. An added row must move it.
diff_added <- !identical(digest_or_na(rbind(d, d[1, , drop = FALSE])), base)

# 6. A value going NA must move it. Tier D of verify_traits.R exists because a
#    column can silently go stale; the gate must not be the thing that hides it.
gone <- d; gone[[num]][1] <- NA
diff_na <- !identical(digest_or_na(gone), base)

cat("permuted rows hash the same        :", same_perm,     "\n")
cat("renamed rows hash the same         :", same_rownames, "\n")
cat("changed value hashes differently   :", diff_value,    "\n")
cat("swapped values hash differently    :", diff_swap,     "\n")
cat("added row hashes differently       :", diff_added,    "\n")
cat("value gone NA hashes differently   :", diff_na,       "\n")

ok <- same_perm && same_rownames && diff_value && diff_swap && diff_added && diff_na
cat("STEP 8: ", if (ok) "PASS" else "FAIL", "\n", sep = "")
quit(status = if (ok) 0 else 1)
