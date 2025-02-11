benchmarks.1 <- list(
  arc = list(mod = "2PL", est = "MAP", lam = 0.005),
  gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.001),
  hellaswag = list(mod = "3PL", est = "MAP", lam = 0.01),
  mmlu = list(mod = "3PL", est = "MAP", lam = 0.01),
  truthfulqa = list(mod = "2PL", est = "EAPsum", lam = 0.01),
  winogrande = list(mod = "4PL", est = "MAP", lam = 0.005)
)

# ------------------------------------------------------------
# seed 2
# ------------------------------------------------------------
# arc
# - 0.001: 196, 1.527 ! (3PL EAPsum)
# - 0.005: 249, 1.345
# - 0.01: 103, 2.057

# gsm8k
# - 0.001: 191, 2.314
# - 0.005: 237, 2.056 ! (2PL EAPsum)
# - 0.01: 196, 2.358

# hellaswag
# - 0.001: 136, 1.396
# - 0.005: 129, 1.419 ! (3PL MAP)
# - 0.01: 70, 1.710

# mmlu
# - 0.001: 197, 1.629
# - 0.005: 100, 1.954 ! (3PL MAP)
# - 0.01: 111, 1.914

# truthfulqa
# - 0.001: 196, 1.560
# - 0.005: 163, 1.894 ! (2PL EAPsum)
# - 0.01: 109, 2.288

# winogrande
# - 0.001: 129, 2.089 
# - 0.005: 87, 2.193
# - 0.01: 67, 2.217 ! (3PL MAP)

benchmarks.2 <- list(
  arc = list(mod = "3PL", est = "EAPsum", lam = 0.001),
  gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.005),
  hellaswag = list(mod = "3PL", est = "MAP", lam = 0.005),
  mmlu = list(mod = "3PL", est = "MAP", lam = 0.005),
  truthfulqa = list(mod = "2PL", est = "EAPsum", lam = 0.005),
  winogrande = list(mod = "3PL", est = "MAP", lam = 0.01)
)

# ------------------------------------------------------------
# seed 3
# ------------------------------------------------------------
# arc
# - 0.001: 196, 1.587
# - 0.005: 249, 1.203
# - 0.01: 145, 1.708 ! (4PL EAPsum)

# gsm8k
# - 0.001: 198, 2.387
# - 0.005: 198, 2.280 ! (2PL EAPsum)
# - 0.01: 165, 2.512

# hellaswag
# - 0.001: 197, 1.332
# - 0.005: 100, 1.464
# - 0.01: 100, 1.464 ! (3PL MAP)

# mmlu
# - 0.001: 171, 1.594 ! (4PL MAP)
# - 0.005: 92, 2.366
# - 0.01: 82, 2.330

# truthfulqa
# - 0.001: 197, 1.729
# - 0.005: 156, 1.724 ! (3PL EAPsum)
# - 0.01: 66, 2.731

# winogrande
# - 0.001: error
# - 0.005: 87, 2.342 ! (3PL MAP)
# - 0.01: 66, 2.624

benchmarks.3 <- list(
   arc = list(mod = "4PL", est = "EAPsum", lam = 0.01),
   gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.005),
   hellaswag = list(mod = "3PL", est = "MAP", lam = 0.01),
   mmlu = list(mod = "4PL", est = "MAP", lam = 0.001),
   truthfulqa = list(mod = "3PL", est = "EAPsum", lam = 0.005),
   winogrande = list(mod = "3PL", est = "MAP", lam = 0.005)
   )

# ------------------------------------------------------------
# seed 4
# ------------------------------------------------------------
# arc
# - 0.001: 197, 2.115 ! (2PL MAP)
# - 0.005: 249, 1.372
# - 0.01: 86, 2.452

# gsm8k
# - 0.001: 196, 2.543
# - 0.005: 249, 2.093 ! (2PL EAPsum)
# - 0.01: 153, 2.954

# hellaswag
# - 0.001: 141, 1.190
# - 0.005: 160, 1.181
# - 0.01: 93, 1.596 ! (4PL MAP)

# mmlu
# - 0.001: 149, 1.818
# - 0.005: 100, 1.812
# - 0.01: 90, 1.981 ! (3PL MAP)

# truthfulqa
# - 0.001: 181, 1.546 ! (4PL EAPsum)
# - 0.005: 185, 1.558
# - 0.01: 70, 2.413

# winogrande
# - 0.001:, 197, 1.905
# - 0.005: 76, 2.089 ! (3PL MAP)
# - 0.01: 67, 2.136

benchmarks.4 <- list(
  arc = list(mod = "2PL", est = "MAP", lam = 0.001),
  gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.005),
  hellaswag = list(mod = "4PL", est = "MAP", lam = 0.01),
  mmlu = list(mod = "3PL", est = "MAP", lam = 0.01),
  truthfulqa = list(mod = "4PL", est = "EAPsum", lam = 0.001),
  winogrande = list(mod = "3PL", est = "MAP", lam = 0.005)
)

# ------------------------------------------------------------
# seed 5
# ------------------------------------------------------------
# arc
# - 0.001: 162, 2.114 ! (3PL MAP)
# - 0.005: 114, 2.215
# - 0.01: 68, 2.596

# gsm8k
# - 0.001: 187, 2.459
# - 0.005: 228, 2.167 ! (3PL EAPsum)
# - 0.01: 228, 2.167

# hellaswag
# - 0.001: 112, 1.349
# - 0.005: 94, 1.388 ! (3PL MAP)
# - 0.01: 93, 1.452

# mmlu
# - 0.001: 99, 2.392 ! (3PL MAP)
# - 0.005: 79, 2.551
# - 0.01: 79, 2.551

# truthfulqa
# - 0.001: 195, 1.431
# - 0.005: 187, 1.462 ! (4PL EAPsum)
# - 0.01: 120, 1.898

# winogrande
# - 0.001: error
# - 0.005: 77, 2.015
# - 0.01: 68, 2.036 ! (3PL MAP)

benchmarks.5 <- list(
   arc = list(mod = "3PL", est = "MAP", lam = 0.001),
   gsm8k = list(mod = "3PL", est = "EAPsum", lam = 0.005),
   hellaswag = list(mod = "3PL", est = "MAP", lam = 0.005),
   mmlu = list(mod = "3PL", est = "MAP", lam = 0.001),
   truthfulqa = list(mod = "4PL", est = "EAPsum", lam = 0.005),
   winogrande = list(mod = "3PL", est = "MAP", lam = 0.01)
   )

#" @export
all.benchmarks <- list("1"=benchmarks.1,
                       "2"=benchmarks.2,
                       "3"=benchmarks.3,
                       "4"=benchmarks.4,
                       "5"=benchmarks.5)


