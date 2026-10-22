# The deploy gate's content digest changes when a value changes, and only then:
# not on row order or row names; yes on a changed, swapped, added or NA value.
suppressMessages(library(digest))
source("scripts/deploy.R")

d <- readRDS("tests/fixtures/pl_trim_702070.rds")
stopifnot(is.data.frame(d), nrow(d) > 10)
base <- digest_or_na(d)

# 1. Row order doesn't move it
set.seed(1)
perm <- d[sample(nrow(d)), , drop = FALSE]
same_perm <- identical(digest_or_na(perm), base)

# 2. A changed value does (most varied numeric column)
nums <- names(d)[vapply(d, is.numeric, TRUE)]
num  <- nums[which.max(vapply(nums, function(k) length(unique(na.omit(d[[k]]))), 1L))]
bump <- d; bump[[num]][1] <- bump[[num]][1] + 1
diff_value <- !identical(digest_or_na(bump), base)

# 3. A value swapped between rows does
swap <- d
ok_i <- which(!is.na(d[[num]]))
i <- c(ok_i[1], ok_i[d[[num]][ok_i] != d[[num]][ok_i[1]]][1])
stopifnot(!anyNA(i), d[[num]][i[1]] != d[[num]][i[2]])
swap[[num]][i] <- swap[[num]][rev(i)]
diff_swap <- !identical(digest_or_na(swap), base)

# 4. Row names alone don't
renamed <- d
attr(renamed, "row.names") <- as.integer(seq_len(nrow(d)) + 1000L)
same_rownames <- identical(digest_or_na(renamed), base)

# 5. An added row does
diff_added <- !identical(digest_or_na(rbind(d, d[1, , drop = FALSE])), base)

# 6. A value going NA does
gone <- d; gone[[num]][1] <- NA
diff_na <- !identical(digest_or_na(gone), base)

cat("permuted rows hash the same        :", same_perm,     "\n")
cat("renamed rows hash the same         :", same_rownames, "\n")
cat("changed value hashes differently   :", diff_value,    "\n")
cat("swapped values hash differently    :", diff_swap,     "\n")
cat("added row hashes differently       :", diff_added,    "\n")
cat("value gone NA hashes differently   :", diff_na,       "\n")

ok <- same_perm && same_rownames && diff_value && diff_swap && diff_added && diff_na
cat("DEPLOY DIGEST: ", if (ok) "PASS" else "FAIL", "\n", sep = "")
quit(status = if (ok) 0 else 1)
