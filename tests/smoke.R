# Дымовой тест без UI: Rscript tests/smoke.R   (запускать из корня репозитория)
# Проверяет, что код каждого из 11 модулей исполняется и даёт математически верный результат,
# что CSV читается в обоих форматах, а автопроверка заданий принимает верные ответы.

suppressMessages(source("app.R", encoding = "UTF-8"))   # определяет функции; приложение не запускается

ok <- function(cond, msg) {
  if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
  cat("  ok -", msg, "\n")
}

inp <- list(seed = 42, noise_level = 2, forecast_horizon = 30,
            gbm_mu = 0.05, gbm_sigma = 0.2, gbm_paths = 8,
            lorenz_sigma = 10, lorenz_rho = 28, lorenz_beta = 8 / 3,
            kepler_ecc = 0.5, kepler_periods = 3,
            osc_damping = 0.15, osc_freq = 1.5,
            mat_a11 = 1.5, mat_a12 = 0.8, mat_a22 = 1.0,
            svd_rank = 3, dist_type = "norm", sample_n = 1000,
            reg_corr = 0.75, reg_n = 150, net_nodes = 14, net_prob = 0.25,
            md_k = 24)
md_demo <- list(data_line = "series <- as.numeric(AirPassengers)", freq = 12)
run <- function(m, input = inp, md = md_demo, series = NULL) run_code(build_code(m, input, md), series)

cat("[arima]\n");      e <- run("arima")
ok(length(e$fc$mean) == 30, "прогноз длиной h")
ok(length(e$series) == 200, "длина ряда 200")

cat("[gbm]\n");        e <- run("gbm")
ok(all(dim(e$paths) == c(101, 8)), "размер матрицы траекторий")
ok(all(e$paths[1, ] == 100), "старт S0 = 100")

cat("[lorenz]\n");     e <- run("lorenz")
ok(nrow(e$out) == 3001 && all(is.finite(as.matrix(e$out))), "траектория конечна")

cat("[kepler]\n");     e <- run("kepler")
ok(max(abs(e$E - e$e * sin(e$E) - e$M)) < 1e-9, "уравнение Кеплера решено")
ok(abs(max(e$r) - 1.5) < 1e-3 && abs(min(e$r) - 0.5) < 1e-3, "перицентр и апоцентр при e = 0.5")

cat("[oscillator]\n"); e <- run("oscillator")
ok(abs(e$zeta - 0.1) < 1e-12, "zeta = gamma / omega0")
ok(abs(tail(e$out$x, 1)) < 0.5, "колебания затухают")

cat("[eigen]\n");      e <- run("eigen")
ok(isTRUE(all.equal(as.numeric(e$A %*% e$eg$vectors[, 1]), as.numeric(e$eg$values[1] * e$eg$vectors[, 1]))),
   "A v = lambda v")

cat("[svd]\n")
norms <- sapply(1:8, function(k) { e <- run("svd", modifyList(inp, list(svd_rank = k))); norm(e$resid, "F") })
ok(all(diff(norms) < 0), "остаток убывает с ростом ранга (слайдер реально работает)")

cat("[stats]\n");      e <- run("stats")
ok(length(e$x) == 1000, "размер выборки")
for (d in c("exp", "t", "chisq")) ok(length(run("stats", modifyList(inp, list(dist_type = d)))$x) == 1000, paste("распределение", d))

cat("[regression]\n"); e <- run("regression")
ok(abs(cor(e$x, e$y) - 0.75) < 1e-9, "выборочная корреляция точно равна r")

cat("[network]\n");    e <- run("network")
ok(isSymmetric(e$adj) && all(diag(e$adj) == 0), "матрица смежности симметрична")
ok(all(e$deg == rowSums(e$adj)) && nrow(e$edges) == sum(e$adj) / 2, "степени согласованы с рёбрами")

cat("[mydata: демо]\n"); e <- run("mydata")
ok(e$n == 144 && e$k == 24, "AirPassengers: n = 144, k = 24")
ok(all(dim(e$metrics) == c(2, 3)) && is.finite(e$coverage), "метрики и покрытие посчитаны")
ok(is.finite(e$lb$p.value), "тест Льюнга-Бокса")

cat("[mydata: CSV]\n")
for (f in c("data/demo_monthly_sales.csv", "data/demo_monthly_sales_ru.csv")) {
  cd  <- read_user_csv(f)
  col <- names(cd$df)[vapply(cd$df, is.numeric, logical(1))][1]
  ok(!is.na(col) && nrow(cd$df) == 72, paste("прочитан", basename(f), "(sep =", cd$sep, ", dec =", cd$dec, ")"))
  x   <- cd$df[[col]]
  md_up <- list(data_line = sprintf('series <- as.numeric(read.csv("x.csv", sep = "%s", dec = "%s")[["%s"]])  #@upload', cd$sep, cd$dec, col),
                freq = 12)
  e <- run("mydata", md = md_up, series = x)
  ok(e$n == 72, paste("модуль 11 на", basename(f)))
}

cat("[задания]\n")
ok(tasks$kepler$check(list(kepler_ecc = 0.6))$ok && !tasks$kepler$check(list(kepler_ecc = 0.5))$ok, "Кеплер")
ok(tasks$lorenz$check(list(lorenz_sigma = 10, lorenz_rho = 14, lorenz_beta = 8 / 3))$ok &&
   !tasks$lorenz$check(list(lorenz_sigma = 10, lorenz_rho = 28, lorenz_beta = 8 / 3))$ok, "Лоренц")
ok(tasks$oscillator$check(list(osc_damping = 1, osc_freq = 1))$ok &&
   !tasks$oscillator$check(list(osc_damping = 0.15, osc_freq = 1.5))$ok, "Осциллятор")
ok(tasks$gbm$check(list(gbm_mu = 0.1, gbm_sigma = 0.2))$ok &&
   !tasks$gbm$check(list(gbm_mu = 0.05, gbm_sigma = 0.2))$ok, "GBM")
svd_ok <- sapply(1:8, function(k) tasks$svd$check(modifyList(inp, list(svd_rank = k)))$ok)
ok(sum(svd_ok) == 1, paste("SVD: ровно один верный ранг, k =", which(svd_ok)))

cat("\nALL PASSED\n")
