# ==============================================================================
# IMPULSE DATA LAB v3.1 // 11-MODULE MATHEMATICAL SUITE
# R Core Engine | Shiny Reactive Framework | AI-Assisted Architecture
# Требуется R >= 4.0 (сырые строки r"(...)"), пакеты: shiny, plotly, forecast, deSolve
#
# v3.0: модуль 11 "Свои данные" (CSV + описательная статистика + бэктест ARIMA),
#       режим заданий с автопроверкой, скачивание исполняемого .R-скрипта.
#
# Что изменилось в v2.4 относительно v2.3 (см. CHANGELOG в конце файла):
#   * Code Inspector теперь показывает РОВНО тот код, который выполняется
#     ("what you see is what runs"): расчёт = eval() текста из инспектора.
#   * Нет внешних CDN (шрифты/иконки) -> приложение работает офлайн.
#   * Исправлены SVD, граф, Лоренц, Кеплер, осциллятор, ARIMA, регрессия.
#   * Реальный замер времени расчёта в KPI вместо заявленных "< 10 мс".
#
# Файл сохранять в кодировке UTF-8.
# ==============================================================================

library(shiny)
library(plotly)
library(forecast)
library(deSolve)

# ------------------------------------------------------------------------------
# CSS (без внешних ресурсов: системные шрифты, JetBrains Mono подхватится,
# если установлен локально)
# ------------------------------------------------------------------------------
custom_css <- "
:root {
  --bg-main: #050811;
  --panel-bg: rgba(13, 20, 36, 0.78);
  --border-glass: rgba(56, 189, 248, 0.18);
  --border-glow: rgba(56, 189, 248, 0.45);
  --accent-cyan: #38bdf8;
  --accent-emerald: #10b981;
  --accent-purple: #c084fc;
  --text-primary: #f8fafc;
  --text-muted: #94a3b8;
  --font-mono: 'JetBrains Mono', ui-monospace, 'Cascadia Mono', Consolas, 'Courier New', monospace;
  --font-sans: 'Plus Jakarta Sans', 'Segoe UI', system-ui, -apple-system, Roboto, Arial, sans-serif;
}

body {
  background-color: var(--bg-main) !important;
  color: var(--text-primary) !important;
  font-family: var(--font-sans) !important;
  margin: 0;
  padding: 16px;
  overflow-x: hidden;
}

.equal-height-row { display: flex; flex-wrap: wrap; }
.equal-height-col { display: flex; flex-direction: column; }

.glass-panel {
  background: var(--panel-bg);
  backdrop-filter: blur(14px);
  -webkit-backdrop-filter: blur(14px);
  border: 1px solid var(--border-glass);
  border-radius: 14px;
  padding: 18px;
  margin-bottom: 18px;
  box-shadow: 0 8px 32px 0 rgba(0, 0, 0, 0.37);
  transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
  height: 100%;
  display: flex;
  flex-direction: column;
  box-sizing: border-box;
}
.glass-panel:hover {
  border-color: var(--border-glow);
  box-shadow: 0 0 25px rgba(56, 189, 248, 0.15);
}

.lab-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  border-bottom: 1px solid var(--border-glass);
  padding-bottom: 14px;
  margin-bottom: 20px;
}
.lab-title {
  font-size: 24px;
  font-weight: 800;
  letter-spacing: -0.5px;
  background: linear-gradient(135deg, #38bdf8 0%, #818cf8 50%, #c084fc 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  margin: 0;
}
.header-badges { display: flex; gap: 10px; align-items: center; }

.lab-badge, .ai-badge, .github-badge {
  padding: 5px 12px;
  border-radius: 9999px;
  font-size: 11px;
  font-family: var(--font-mono);
  font-weight: 600;
  display: inline-flex;
  align-items: center;
  gap: 6px;
}
.lab-badge   { background: rgba(16,185,129,.15); border: 1px solid rgba(16,185,129,.4); color: #34d399; }
.ai-badge    { background: rgba(192,132,252,.15); border: 1px solid rgba(192,132,252,.4); color: #c084fc; }
.github-badge{ background: rgba(56,189,248,.15); border: 1px solid rgba(56,189,248,.4); color: #38bdf8; text-decoration: none; }

.status-dot {
  width: 7px; height: 7px; border-radius: 50%;
  background-color: var(--accent-emerald);
  box-shadow: 0 0 8px var(--accent-emerald);
}

.kpi-container {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 14px;
  margin-bottom: 20px;
}
.kpi-card {
  background: rgba(15, 23, 42, 0.6);
  border: 1px solid var(--border-glass);
  border-radius: 12px;
  padding: 12px 16px;
}
.kpi-label {
  font-size: 10px; text-transform: uppercase; letter-spacing: 1px;
  color: var(--text-muted); font-family: var(--font-mono);
}
.kpi-val {
  font-size: 19px; font-weight: 800; color: var(--text-primary);
  margin-top: 3px; font-family: var(--font-mono);
}

.form-control, .selectize-input {
  background: rgba(15, 23, 42, 0.9) !important;
  border: 1px solid var(--border-glass) !important;
  color: #f1f5f9 !important;
  border-radius: 8px !important;
}
.selectize-input > .item { color: #f1f5f9 !important; }
.selectize-dropdown {
  background: #0f172a !important;
  border: 1px solid var(--border-glass) !important;
  color: #f1f5f9 !important;
}
.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .active {
  background: rgba(56, 189, 248, 0.15) !important;
  color: #38bdf8 !important;
}
.selectize-dropdown .optgroup-header { background: #0f172a !important; color: #64748b !important; }

label { color: #cbd5e1 !important; font-weight: 600; font-size: 12px; margin-bottom: 4px; }

.btn-cyber {
  background: rgba(56, 189, 248, 0.15);
  border: 1px solid rgba(56, 189, 248, 0.4);
  color: #38bdf8;
  font-family: var(--font-mono);
  font-size: 11px; font-weight: 600;
  border-radius: 6px;
  padding: 5px 10px; margin-right: 4px; margin-top: 5px;
  transition: all 0.2s;
}
.btn-cyber:hover { background: rgba(56, 189, 248, 0.35); color: #fff; }

/* Вкладки в тёмной теме */
.nav-tabs { border-bottom: 1px solid var(--border-glass) !important; }
.nav-tabs > li > a {
  color: var(--text-muted) !important;
  background: transparent !important;
  border: none !important;
  border-bottom: 2px solid transparent !important;
  margin-right: 6px;
}
.nav-tabs > li > a:hover { color: #e2e8f0 !important; }
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  color: var(--accent-cyan) !important;
  background: rgba(56, 189, 248, 0.10) !important;
  border: none !important;
  border-bottom: 2px solid var(--accent-cyan) !important;
}

/* Таблица расчётов */
.table { color: #cbd5e1; width: auto; min-width: 55%; font-family: var(--font-mono); font-size: 12px; }
.table > thead > tr > th { color: #38bdf8; border-bottom: 1px solid var(--border-glass) !important; }
.table > tbody > tr > td { border-top: 1px solid rgba(255,255,255,0.06) !important; }

/* Терминал */
.terminal-window {
  background: #020617;
  border: 1px solid var(--border-glass);
  border-radius: 12px;
  overflow: hidden;
  margin-bottom: 16px;
}
.terminal-header {
  background: rgba(15, 23, 42, 0.9);
  padding: 8px 14px;
  display: flex; align-items: center; gap: 8px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.06);
}
.term-dot { width: 9px; height: 9px; border-radius: 50%; }
.term-dot.red { background: #ef4444; }
.term-dot.yellow { background: #f59e0b; }
.term-dot.green { background: #10b981; }
.term-title { font-family: var(--font-mono); font-size: 11px; color: var(--text-muted); margin-left: 8px; }

/* verbatimTextOutput по умолчанию белый (bootstrap <pre>) - перекрашиваем */
#code_inspector {
  background: #020617 !important;
  color: #38bdf8 !important;
  border: 0 !important;
  border-radius: 0 !important;
  margin: 0 !important;
  padding: 12px 16px !important;
  font-family: var(--font-mono) !important;
  font-size: 12px;
  line-height: 1.55;
  max-height: 300px;
  overflow: auto;
  white-space: pre;
}

/* Валидация и ошибки Shiny - читаемо на тёмном фоне */
.shiny-output-error-validation { color: #fbbf24 !important; font-family: var(--font-mono); font-size: 13px; padding: 12px; }
.shiny-output-error { color: #f87171; }

/* Кнопки Bootstrap по умолчанию (fileInput, downloadButton) */
.btn-default, .btn-file {
  background: rgba(56, 189, 248, 0.15) !important;
  border: 1px solid rgba(56, 189, 248, 0.4) !important;
  color: #38bdf8 !important;
}
.btn-default:hover { background: rgba(56, 189, 248, 0.35) !important; color: #fff !important; }
.radio label, .radio-inline { color: #cbd5e1 !important; font-weight: 500; }
.progress-bar { background-color: var(--accent-cyan); }

/* Задания */
.task-card { padding: 4px 8px 12px 8px; color: #cbd5e1; }
.task-text { background: rgba(15,23,42,0.6); border: 1px solid var(--border-glass); border-radius: 10px;
             padding: 12px 14px; margin: 10px 0; font-size: 13px; line-height: 1.55; }
.task-ok  { border-left: 3px solid var(--accent-emerald); background: rgba(16,185,129,0.10);
            padding: 10px 14px; border-radius: 6px; margin-top: 12px; }
.task-bad { border-left: 3px solid #f59e0b; background: rgba(245,158,11,0.10);
            padding: 10px 14px; border-radius: 6px; margin-top: 12px; }
.task-progress { font-family: var(--font-mono); font-size: 12px; color: var(--text-muted); margin-bottom: 8px; }

.panel-spacer { flex-grow: 1; }
.core-stat-footer {
  margin-top: 14px; padding-top: 10px;
  border-top: 1px solid rgba(255, 255, 255, 0.06);
  font-size: 10px; font-family: var(--font-mono); color: #64748b;
  display: flex; justify-content: space-between;
}
"

# ==============================================================================
# ГЕНЕРАТОР КОДА: единый источник истины для расчёта и Code Inspector
# ==============================================================================

STOCHASTIC_MODES <- c("arima", "gbm", "svd", "stats", "regression", "network")

# число -> строка для подстановки в код без потери точности
num <- function(x) format(x, digits = 15, trim = TRUE, scientific = FALSE)

# подстановка {{name}} в шаблон
fill <- function(tpl, ...) {
  a <- list(...)
  for (k in names(a)) tpl <- gsub(paste0("{{", k, "}}"), a[[k]], tpl, fixed = TRUE)
  tpl
}

build_code <- function(m, input, md = NULL) {
  seed_line <- if (m %in% STOCHASTIC_MODES) paste0("set.seed(", num(input$seed), ")\n") else ""

  body <- switch(m,

    arima = fill(r"---(
library(forecast)
{{seed}}
# Синтетический ряд: линейный тренд + интегрированный AR(1)-шум.
# Волатильность (sd инноваций) = {{sigma}}
e      <- arima.sim(model = list(order = c(1, 1, 0), ar = 0.6), n = 199, sd = {{sigma}})
series <- 100 + 0.3 * seq_along(e) + as.numeric(e)

# Автоподбор спецификации и прогноз с 95% доверительным интервалом
fit        <- auto.arima(ts(series))
fc         <- forecast(fit, h = {{h}}, level = 95)
ord        <- arimaorder(fit)[1:3]
model_name <- sprintf("ARIMA(%d,%d,%d)", ord[1], ord[2], ord[3])
)---", seed = trimws(seed_line), sigma = num(input$noise_level), h = num(input$forecast_horizon)),

    gbm = fill(r"---(
{{seed}}
mu <- {{mu}}; sigma <- {{sigma}}; n_paths <- {{paths}}
S0 <- 100; steps <- 100; dtime <- 1 / steps
tgrid <- seq(0, 1, length.out = steps + 1)

# Геометрическое броуновское движение (точная дискретизация):
# S(t+dt) = S(t) * exp((mu - sigma^2/2) * dt + sigma * dW)
paths <- sapply(seq_len(n_paths), function(i) {
  dW <- rnorm(steps, 0, sqrt(dtime))
  S0 * c(1, exp(cumsum((mu - 0.5 * sigma^2) * dtime + sigma * dW)))
})
expected <- S0 * exp(mu * tgrid)   # E[S(t)] = S0 * exp(mu * t)
)---", seed = trimws(seed_line), mu = num(input$gbm_mu), sigma = num(input$gbm_sigma), paths = num(input$gbm_paths)),

    lorenz = fill(r"---(
library(deSolve)
lorenz <- function(t, state, p) {
  with(as.list(c(state, p)), list(c(
    s * (Y - X),
    X * (r - Z) - Y,
    X * Y - b * Z
  )))
}
params <- c(s = {{s}}, r = {{r}}, b = {{b}})
out <- as.data.frame(ode(y = c(X = 0.1, Y = 0, Z = 0),
                         times = seq(0, 30, by = 0.01),
                         func = lorenz, parms = params, method = "rk4"))
)---", s = num(input$lorenz_sigma), r = num(input$lorenz_rho), b = num(input$lorenz_beta)),

    kepler = fill(r"---(
e     <- {{e}}          # эксцентриситет
n_orb <- {{n}}          # число витков (a = 1, GM = 1)

# Равномерное время -> средняя аномалия M; уравнение Кеплера E - e*sin(E) = M
# решаем методом Ньютона
M <- seq(0, 2 * pi * n_orb, length.out = 400 * n_orb)
E <- M
for (i in 1:50) E <- E - (E - e * sin(E) - M) / (1 - e * cos(E))

x <- cos(E) - e                    # фокус в начале координат
y <- sqrt(1 - e^2) * sin(E)
r <- 1 - e * cos(E)                # расстояние до фокуса
v <- sqrt(2 / r - 1)               # скорость из интеграла vis-viva
)---", e = num(input$kepler_ecc), n = num(input$kepler_periods)),

    oscillator = fill(r"---(
library(deSolve)
# x'' + 2*damping*x' + omega0^2 * x = 0   (решается численно, не формулой)
damping <- {{g}}
omega0  <- {{w}}
osc <- function(t, state, p) {
  with(as.list(c(state, p)), list(c(v, -2 * damping * v - omega0^2 * x)))
}
out  <- as.data.frame(ode(y = c(x = 1, v = 0), times = seq(0, 25, by = 0.03),
                          func = osc, parms = c(damping = damping, omega0 = omega0)))
zeta <- damping / omega0           # <1 колебательный, =1 критический, >1 апериодический
)---", g = num(input$osc_damping), w = num(input$osc_freq)),

    eigen = fill(r"---(
A  <- matrix(c({{a11}}, {{a12}}, {{a12}}, {{a22}}), 2, 2)   # симметричная матрица
eg <- eigen(A)

# Образ единичной окружности под действием A - эллипс с полуосями |lambda_i|
theta   <- seq(0, 2 * pi, length.out = 200)
circle  <- rbind(cos(theta), sin(theta))
ellipse <- A %*% circle
)---", a11 = num(input$mat_a11), a12 = num(input$mat_a12), a22 = num(input$mat_a22)),

    svd = fill(r"---(
{{seed}}
n <- 30; i <- seq_len(n)
# Структурная часть (ранг 3) + полноранговый шум
signal <- outer(sin(i / 4), cos(i / 5)) +
          0.6 * outer(cos(i / 3), sin(i / 6)) +
          0.3 * outer(i / n, rev(i) / n)
A <- signal + matrix(rnorm(n * n, 0, 0.15), n, n)

s   <- svd(A)
k   <- {{k}}
A_k <- s$u[, 1:k, drop = FALSE] %*% diag(s$d[1:k], k, k) %*% t(s$v[, 1:k, drop = FALSE])
resid  <- A - A_k
energy <- sum(s$d[1:k]^2) / sum(s$d^2)   # доля сохранённой "энергии"
)---", seed = trimws(seed_line), k = num(input$svd_rank)),

    stats = {
      dist <- match.arg(input$dist_type, c("norm", "exp", "t", "chisq"))
      fill(r"---(
{{seed}}
n    <- {{n}}
dist <- "{{dist}}"      # t: df = 5, chisq: df = 4
x <- switch(dist,
            norm  = rnorm(n),
            exp   = rexp(n),
            t     = rt(n, df = 5),
            chisq = rchisq(n, df = 4))

# Теоретическая плотность для сравнения
grid <- seq(min(x), max(x), length.out = 300)
dens <- switch(dist,
               norm  = dnorm(grid),
               exp   = dexp(grid),
               t     = dt(grid, df = 5),
               chisq = dchisq(grid, df = 4))
)---", seed = trimws(seed_line), n = num(input$sample_n), dist = dist)
    },

    regression = fill(r"---(
{{seed}}
n <- {{n}}; r <- {{r}}
# Ортогонализация шума к x -> выборочная корреляция РОВНО равна r
x <- as.numeric(scale(rnorm(n)))
e <- as.numeric(scale(resid(lm(rnorm(n) ~ x))))
y <- r * x + sqrt(1 - r^2) * e
fit <- lm(y ~ x)

# Теоретический 95% эллипс рассеяния двумерного нормального закона
theta <- seq(0, 2 * pi, length.out = 200)
cc <- sqrt(qchisq(0.95, 2))
ex <- cc * cos(theta)
ey <- cc * (r * cos(theta) + sqrt(1 - r^2) * sin(theta))
)---", seed = trimws(seed_line), n = num(input$reg_n), r = num(input$reg_corr)),

    network = fill(r"---(
{{seed}}
n <- {{n}}; p <- {{p}}
# Неориентированный граф Эрдёша-Реньи G(n, p): симметричная матрица смежности
adj <- matrix(0, n, n)
adj[upper.tri(adj)] <- rbinom(n * (n - 1) / 2, 1, p)
adj <- adj + t(adj)

deg    <- rowSums(adj)             # степень узла
dcent  <- deg / (n - 1)            # нормированная degree centrality
ang    <- seq(0, 2 * pi, length.out = n + 1)[1:n]
coords <- data.frame(x = cos(ang), y = sin(ang))
edges  <- which(upper.tri(adj) & adj == 1, arr.ind = TRUE)
)---", seed = trimws(seed_line), n = num(input$net_nodes), p = num(input$net_prob)),

    mydata = fill(r"---(
library(forecast)
{{data}}
freq   <- {{freq}}                       # период сезонности (1 = без сезонности)
series <- series[is.finite(series)]
n      <- length(series)
k      <- min({{k}}, floor(n / 3))       # длина отложенной (тестовой) части

# --- 1. Описательная статистика ---------------------------------------------
m    <- mean(series)
q    <- unname(quantile(series, c(0.25, 0.75)))
desc <- c(N = n, mean = m, sd = sd(series), min = min(series), q1 = q[1],
          median = median(series), q3 = q[2], max = max(series),
          skew    = mean((series - m)^3) / mean((series - m)^2)^1.5,
          ex_kurt = mean((series - m)^4) / mean((series - m)^2)^2 - 3,
          shapiro_p = if (n <= 5000) shapiro.test(series)$p.value else NA_real_)

# --- 2. Бэктест: учим модель на начале ряда, проверяем на отложенном хвосте ---
train <- ts(series[1:(n - k)], frequency = freq)
test  <- series[(n - k + 1):n]
use_bc <- {{bc}}                         # преобразование Бокса-Кокса: стабилизирует растущий размах сезонности
lambda <- if (use_bc && all(series > 0)) "auto" else NULL
fit   <- auto.arima(train, lambda = lambda)
fc    <- forecast(fit, h = k, level = 95)
ord   <- arimaorder(fit)
model_name <- if (length(ord) >= 7) {
  sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)[%d]", ord[1], ord[2], ord[3], ord[4], ord[5], ord[6], ord[7])
} else {
  sprintf("ARIMA(%d,%d,%d)", ord[1], ord[2], ord[3])
}

# --- 3. Сравнение с эталоном (сезонный наивный прогноз) и метрики -------------
naive <- as.numeric(snaive(train, h = k)$mean)
scale <- mean(abs(diff(as.numeric(train), lag = freq)))      # масштаб для MASE
err   <- function(f) c(MASE  = mean(abs(test - f)) / scale,
                       sMAPE = mean(200 * abs(test - f) / (abs(test) + abs(f))),
                       RMSE  = sqrt(mean((test - f)^2)))
metrics  <- rbind(model = err(as.numeric(fc$mean)), naive = err(naive))
coverage <- mean(test >= as.numeric(fc$lower) & test <= as.numeric(fc$upper))

# --- 4. Диагностика остатков ----------------------------------------------------
lb_lag <- min(20, floor(length(train) / 4))
lb     <- Box.test(residuals(fit), lag = lb_lag, type = "Ljung-Box",
                   fitdf = min(length(fit$coef), lb_lag - 1))
d_kpss <- ndiffs(train)                                      # число разностей по KPSS
)---", data = md$data_line, freq = num(md$freq), k = num(input$md_k),
         bc = if (isTRUE(input$md_bc)) "TRUE" else "FALSE")
  )

  trimws(body)
}

# Подсказка по визуализации (только для показа в инспекторе, не исполняется)
plot_hint <- function(m) {
  hint <- switch(m,
    arima      = "plot(fc)",
    gbm        = "matplot(tgrid, paths, type = 'l'); lines(tgrid, expected, lwd = 2)",
    lorenz     = "plot(out$X, out$Z, type = 'l')   # проекция X-Z",
    kepler     = "plot(x, y, type = 'l', asp = 1); points(0, 0, pch = 19)",
    oscillator = "plot(out$x, out$v, type = 'l', xlab = 'x', ylab = 'dx/dt')",
    eigen      = "plot(t(ellipse), type = 'l', asp = 1); abline(h = 0, v = 0)",
    svd        = "image(A_k)",
    stats      = "hist(x, freq = FALSE, breaks = 40); lines(grid, dens)",
    regression = "plot(x, y); abline(fit); summary(fit)",
    network    = "plot(coords, pch = 19, cex = 1 + deg / 2)",
    mydata     = "plot(fc, main = model_name); lines(time(fc$mean), test, col = 'red'); print(metrics)"
  )
  hint
}

# Выполнение кода модуля в изолированном окружении.
# Строки-комментарии и строки с маркером #@upload (отображаемая строка чтения CSV)
# не исполняются; загруженный ряд подставляется в переменную series напрямую.
run_code <- function(code, series = NULL) {
  code_run <- gsub("(?m)^.*#@upload[ \\t]*$", "", code, perl = TRUE)
  code_run <- gsub("(?m)^[ \\t]*#.*$", "", code_run, perl = TRUE)
  env <- new.env(parent = globalenv())
  if (!is.null(series)) env$series <- series
  eval(parse(text = code_run), envir = env)
  env
}

# Чтение пользовательского CSV: авто-определение разделителя, десятичного знака и кодировки
read_user_csv <- function(path) {
  first <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  cnt <- function(ch) nchar(first) - nchar(gsub(ch, "", first, fixed = TRUE))
  sep <- if (length(first) && cnt(";") > cnt(",")) ";" else ","
  rd <- function(dec, enc) read.csv(path, sep = sep, dec = dec, fileEncoding = enc,
                                    check.names = FALSE, stringsAsFactors = FALSE)
  try_dec <- function(dec) tryCatch(rd(dec, "UTF-8-SIG"), error = function(e) rd(dec, "CP1251"))
  dec <- if (sep == ";") "," else "."
  df  <- try_dec(dec)
  if (!any(vapply(df, is.numeric, logical(1)))) {        # запасной вариант десятичного знака
    dec2 <- if (dec == ",") "." else ","
    df2  <- try_dec(dec2)
    if (any(vapply(df2, is.numeric, logical(1)))) { df <- df2; dec <- dec2 }
  }
  list(df = df, sep = sep, dec = dec)
}

# ==============================================================================
# ЗАДАНИЯ С АВТОПРОВЕРКОЙ (проверка читает те же слайдеры, что и модули)
# ==============================================================================
tasks <- list(
  lorenz = list(
    title = "1. Лоренц: от хаоса к покою", module = "lorenz",
    inputs = c("lorenz_sigma", "lorenz_rho", "lorenz_beta"),
    text = paste0("Модуль 3. Подберите параметры (sigma, rho, beta) так, чтобы траектория НЕ была хаотической, ",
                  "а сходилась к точке покоя. Подсказка: неподвижные точки теряют устойчивость при ",
                  "rho_H = sigma (sigma + beta + 3) / (sigma - beta - 1)."),
    check = function(input) {
      s <- input$lorenz_sigma; r <- input$lorenz_rho; b <- input$lorenz_beta
      if (s > b + 1) {
        rH <- s * (s + b + 3) / (s - b - 1)
        ok <- r < rH
        msg <- sprintf("Порог Хопфа rho_H = %.2f, у вас rho = %g. %s", rH, r,
                       if (ok) "Ниже порога: траектория сходится к точке покоя." else "Выше порога: движение остаётся нерегулярным, уменьшите rho.")
        if (ok && r > 0.97 * rH) msg <- paste(msg, "(Вы близко к порогу — сходимость медленная.)")
      } else {
        ok <- TRUE
        msg <- "При sigma <= beta + 1 неподвижные точки устойчивы при любом rho."
      }
      list(ok = ok, msg = msg)
    }),

  kepler = list(
    title = "2. Кеплер: скорость в перицентре", module = "kepler",
    inputs = c("kepler_ecc"),
    text = paste0("Модуль 4. Задайте эксцентриситет так, чтобы скорость тела в перицентре была ровно в 4 раза ",
                  "больше, чем в апоцентре. Подсказка: v_п / v_а = (1 + e) / (1 - e)."),
    check = function(input) {
      e <- input$kepler_ecc; ratio <- (1 + e) / (1 - e)
      list(ok = abs(ratio - 4) < 0.01,
           msg = sprintf("При e = %g отношение скоростей v_п / v_а = %.2f (цель — 4.00).", e, ratio))
    }),

  oscillator = list(
    title = "3. Осциллятор: вернуться без колебаний и быстро", module = "oscillator",
    inputs = c("osc_damping", "osc_freq"),
    text = paste0("Модуль 5. Тело отпущено из x = 1. Подберите gamma и omega0 так, чтобы оно вернулось к равновесию ",
                  "БЕЗ колебаний (смещение x нигде не уходит в минус, на фазовом портрете нет спирали) и БЫСТРО: ",
                  "к моменту t = 25 остаточное смещение |x| < 0.01. Слишком слабое затухание даёт спираль, ",
                  "слишком сильное — медленное «ползущее» возвращение. Ищите режим между ними."),
    check = function(input) {
      env <- run_code(build_code("oscillator", input))
      x <- env$out$x
      n_neg <- sum(x < -1e-4)                 # отсчёты с отрицательным смещением = колебание
      x_end <- abs(tail(x, 1))
      z <- env$zeta
      if (n_neg > 0) {
        list(ok = FALSE, msg = sprintf("Колебания остались: смещение уходит в минус (%d отсчётов из %d), zeta = %.2f. Затухание слабое.",
                                       n_neg, length(x), z))
      } else if (x_end >= 0.01) {
        list(ok = FALSE, msg = sprintf("Колебаний нет, но возврат медленный: при t = 25 |x| = %.3f (нужно < 0.01), zeta = %.2f. Затухание избыточное.",
                                       x_end, z))
      } else {
        list(ok = TRUE, msg = sprintf("Колебаний нет, при t = 25 |x| = %.1e. zeta = %.2f — режим близок к критическому: это самое быстрое возвращение без колебаний.",
                                      x_end, z))
      }
    }),

  svd = list(
    title = "4. SVD: минимальный ранг", module = "svd",
    inputs = c("svd_rank", "seed"),
    text = paste0("Модуль 7. Найдите НАИМЕНЬШИЙ ранг усечения k, при котором сохраняется не менее 90% энергии матрицы ",
                  "(доля суммы квадратов сингулярных чисел). Ориентируйтесь на подпись над средней панелью."),
    check = function(input) {
      env <- run_code(build_code("svd", input))
      en  <- cumsum(env$s$d^2) / sum(env$s$d^2)
      kmin <- which(en >= 0.90)[1]; k <- input$svd_rank
      if (is.na(kmin)) return(list(ok = FALSE, msg = "Порог 90% не достигнут ни при каком ранге."))
      if (k == kmin) list(ok = TRUE, msg = sprintf("k = %d сохраняет %.1f%% энергии, и это минимальный подходящий ранг.", k, en[k] * 100))
      else if (k > kmin) list(ok = FALSE, msg = sprintf("k = %d даёт %.1f%% — достаточно, но ранг не минимален. Попробуйте меньше.", k, en[k] * 100))
      else list(ok = FALSE, msg = sprintf("k = %d сохраняет лишь %.1f%% < 90%%. Увеличьте ранг.", k, en[k] * 100))
    }),

  gbm = list(
    title = "5. GBM: вероятность убытка", module = "gbm",
    inputs = c("gbm_mu", "gbm_sigma"),
    text = paste0("Модуль 2. Подберите mu и sigma так, чтобы вероятность того, что цена через год окажется ниже ",
                  "стартовой, P(S(T) < S0), лежала в диапазоне 30–35%. Подсказка: ln(S(T)/S0) ~ N(mu - sigma^2/2, sigma^2)."),
    check = function(input) {
      mu <- input$gbm_mu; sg <- input$gbm_sigma
      p <- pnorm(-(mu - 0.5 * sg^2) / sg)
      list(ok = p >= 0.30 && p <= 0.35,
           msg = sprintf("P(S(T) < S0) = %.1f%% (цель: 30–35%%). Проверьте глазом: примерно такая доля траекторий должна закончиться ниже 100.", p * 100))
    })
)
task_choices <- setNames(names(tasks), vapply(tasks, function(t) t$title, character(1)))

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML(custom_css))),

  div(class = "lab-header",
      div(
        h1(class = "lab-title", "IMPULSE DATA LAB"),
        p(style = "color: #64748b; font-size: 12px; margin: 3px 0 0 0;",
          "Вычислительный комплекс математического, эконометрического и физического анализа // R Shiny Architecture")
      ),
      div(class = "header-badges",
          div(class = "ai-badge", "AI-Assisted: Gemini + Claude"),
          a(class = "github-badge", href = "https://github.com/VenuK0/r-studio-project", target = "_blank", "GitHub"),
          div(class = "lab-badge", div(class = "status-dot"), span("11 ENGINES: ACTIVE"))
      )
  ),

  div(class = "kpi-container",
      div(class = "kpi-card",
          div(class = "kpi-label", "Вычислительное ядро"),
          div(class = "kpi-val", textOutput("kpi_r", inline = TRUE))
      ),
      div(class = "kpi-card",
          div(class = "kpi-label", "Библиотечный стек"),
          div(class = "kpi-val", style = "color: #38bdf8;", "deSolve + forecast")
      ),
      div(class = "kpi-card",
          div(class = "kpi-label", "Математические блоки"),
          div(class = "kpi-val", style = "color: #c084fc;", "11 Модулей")
      ),
      div(class = "kpi-card",
          div(class = "kpi-label", "Время расчёта (замер)"),
          div(class = "kpi-val", style = "color: #10b981;", textOutput("kpi_latency", inline = TRUE))
      )
  ),

  div(class = "row equal-height-row",
      div(class = "col-sm-3 equal-height-col",
          div(class = "glass-panel",
              h4(style = "color: #f8fafc; font-weight: 700; margin-top: 0; font-size: 15px;",
                 "Конфигуратор ядра"),
              hr(style = "border-color: rgba(255,255,255,0.08); margin: 8px 0 14px 0;"),

              selectInput("model_mode", "Выберите математический модуль:",
                          choices = list(
                            "Эконометрика & Стохастика" = c(
                              "1. Предиктивная аналитика (ARIMA)" = "arima",
                              "2. Монте-Карло броуновское движение" = "gbm"
                            ),
                            "Нелинейная динамика & Физика" = c(
                              "3. 3D-аттрактор Лоренца (Хаос)" = "lorenz",
                              "4. Гравитационные орбиты Кеплера" = "kepler",
                              "5. Гармонический осциллятор (Фазовый портрет)" = "oscillator"
                            ),
                            "Матрицы & Алгебра" = c(
                              "6. Спектральный анализ (Eigenvalues)" = "eigen",
                              "7. Сингулярное разложение (SVD)" = "svd"
                            ),
                            "Статистика & Топология" = c(
                              "8. Распределения вероятностей" = "stats",
                              "9. Корреляция и регрессионный эллипс" = "regression",
                              "10. Сетевая топология (Графы)" = "network"
                            ),
                            "Реальные данные" = c(
                              "11. Свои данные: статистика + бэктест" = "mydata"
                            )
                          )),

              # Seed - только для стохастических модулей
              conditionalPanel(
                condition = "['arima','gbm','svd','stats','regression','network'].indexOf(input.model_mode) >= 0",
                numericInput("seed", "Seed (воспроизводимость):", value = 42, min = 1, step = 1),
                actionButton("reseed", "Новая выборка", class = "btn-cyber")
              ),

              # 1. ARIMA
              conditionalPanel(
                condition = "input.model_mode == 'arima'",
                sliderInput("forecast_horizon", "Горизонт (шагов):", min = 7, max = 60, value = 30, step = 1),
                sliderInput("noise_level", "Волатильность (Sigma):", min = 0.5, max = 5.0, value = 2.0, step = 0.1)
              ),
              # 2. GBM
              conditionalPanel(
                condition = "input.model_mode == 'gbm'",
                sliderInput("gbm_mu", "Параметр дрейфа (Mu):", min = -0.1, max = 0.3, value = 0.05, step = 0.01),
                sliderInput("gbm_sigma", "Диффузия (Sigma):", min = 0.05, max = 0.6, value = 0.2, step = 0.05),
                sliderInput("gbm_paths", "Количество траекторий:", min = 3, max = 25, value = 8, step = 1)
              ),
              # 3. Lorenz
              conditionalPanel(
                condition = "input.model_mode == 'lorenz'",
                sliderInput("lorenz_sigma", "Прандтль (Sigma):", min = 5, max = 25, value = 10, step = 1),
                sliderInput("lorenz_rho", "Рэлей (Rho):", min = 10, max = 50, value = 28, step = 1),
                sliderInput("lorenz_beta", "Геометрия (Beta):", min = 1, max = 5, value = 2.67, step = 0.01),
                actionButton("preset_classic", "Хаос (rho=28)", class = "btn-cyber"),
                actionButton("preset_limit", "Покой (rho=14)", class = "btn-cyber")
              ),
              # 4. Kepler
              conditionalPanel(
                condition = "input.model_mode == 'kepler'",
                sliderInput("kepler_ecc", "Эксцентриситет (e):", min = 0.0, max = 0.85, value = 0.5, step = 0.05),
                sliderInput("kepler_periods", "Витков:", min = 1, max = 8, value = 3, step = 1),
                actionButton("preset_circular", "Круг", class = "btn-cyber"),
                actionButton("preset_comet", "Комета", class = "btn-cyber")
              ),
              # 5. Oscillator
              conditionalPanel(
                condition = "input.model_mode == 'oscillator'",
                sliderInput("osc_damping", "Коэффициент затухания (gamma):", min = 0.0, max = 1.0, value = 0.15, step = 0.05),
                sliderInput("osc_freq", "Собственная частота (omega0):", min = 0.5, max = 4.0, value = 1.5, step = 0.1)
              ),
              # 6. Eigen
              conditionalPanel(
                condition = "input.model_mode == 'eigen'",
                sliderInput("mat_a11", "Элемент матрицы A[1,1]:", min = -3.0, max = 3.0, value = 1.5, step = 0.5),
                sliderInput("mat_a12", "Элемент матрицы A[1,2] = A[2,1]:", min = -3.0, max = 3.0, value = 0.8, step = 0.2),
                sliderInput("mat_a22", "Элемент матрицы A[2,2]:", min = -3.0, max = 3.0, value = 1.0, step = 0.5)
              ),
              # 7. SVD
              conditionalPanel(
                condition = "input.model_mode == 'svd'",
                sliderInput("svd_rank", "Ранг усечения матрицы:", min = 1, max = 8, value = 3, step = 1)
              ),
              # 8. Stats
              conditionalPanel(
                condition = "input.model_mode == 'stats'",
                selectInput("dist_type", "Закон распределения:",
                            choices = c("Гауссово" = "norm", "Экспоненциальное" = "exp",
                                        "t-Стьюдента (df=5)" = "t", "Хи-квадрат (df=4)" = "chisq")),
                sliderInput("sample_n", "Размер выборки (N):", min = 200, max = 5000, value = 1000, step = 100)
              ),
              # 9. Regression
              conditionalPanel(
                condition = "input.model_mode == 'regression'",
                sliderInput("reg_corr", "Целевая корреляция (r):", min = -0.95, max = 0.95, value = 0.75, step = 0.05),
                sliderInput("reg_n", "Число точек наблюдения:", min = 50, max = 500, value = 150, step = 25)
              ),
              # 10. Network
              conditionalPanel(
                condition = "input.model_mode == 'network'",
                sliderInput("net_nodes", "Количество узлов сети:", min = 6, max = 30, value = 14, step = 2),
                sliderInput("net_prob", "Плотность связей (p):", min = 0.1, max = 0.7, value = 0.25, step = 0.05)
              ),
              # 11. Свои данные
              conditionalPanel(
                condition = "input.model_mode == 'mydata'",
                radioButtons("md_src", "Источник данных:",
                             choices = c("Демо-набор" = "demo", "Свой CSV" = "upload"), inline = TRUE),
                conditionalPanel(
                  condition = "input.md_src == 'demo'",
                  selectInput("md_demo", "Набор:",
                              choices = c("AirPassengers: авиапассажиры, месяц" = "AirPassengers",
                                          "Nile: сток реки Нил, год" = "Nile",
                                          "co2: CO2 в атмосфере, месяц" = "co2"))
                ),
                conditionalPanel(
                  condition = "input.md_src == 'upload'",
                  fileInput("csv_file", "CSV-файл (первая строка — заголовки):", accept = c(".csv", ".txt"),
                            buttonLabel = "Выбрать", placeholder = "файл не выбран"),
                  uiOutput("csv_col_ui"),
                  selectInput("md_freq", "Сезонность (период):",
                              choices = c("Нет (1)" = 1, "Квартальная (4)" = 4, "Месячная (12)" = 12, "Недельная (7)" = 7))
                ),
                sliderInput("md_k", "Отложенная часть (шагов):", min = 6, max = 48, value = 24, step = 2),
                checkboxInput("md_bc", "Преобразование Бокса–Кокса (λ авто)", value = FALSE)
              ),

              div(class = "panel-spacer"),
              div(class = "core-stat-footer",
                  span("R EXEC: REALTIME"),
                  span("STATUS: OK")
              )
          )
      ),

      div(class = "col-sm-9 equal-height-col",
          div(class = "glass-panel",
              tabsetPanel(
                id = "main_tabs",
                tabPanel("Аналитический экран", value = "screen",
                         br(),
                         plotlyOutput("main_plot", height = "470px")
                ),
                tabPanel("Матрица расчётов", value = "matrix",
                         br(),
                         div(style = "max-height: 470px; overflow-y: auto;",
                             tableOutput("raw_data_table")
                         )
                ),
                tabPanel("Задания", value = "tasks",
                         br(),
                         div(class = "task-card",
                             uiOutput("task_progress"),
                             selectInput("task_id", "Задание:", choices = task_choices),
                             uiOutput("task_text"),
                             actionButton("task_go", "Перейти к модулю", class = "btn-cyber"),
                             actionButton("task_check", "Проверить", class = "btn-cyber"),
                             uiOutput("task_result")
                         )
                )
              )
          )
      )
  ),

  fluidRow(
    column(width = 12,
           div(class = "terminal-window",
               div(class = "terminal-header",
                   div(class = "term-dot red"),
                   div(class = "term-dot yellow"),
                   div(class = "term-dot green"),
                   span(class = "term-title", "TERMINAL AUDIT // R-CORE CODE INSPECTOR (этот код исполняется в расчёте)"),
                   downloadButton("dl_code", "Скачать .R", class = "btn-cyber",
                                  style = "margin: 0 0 0 auto;")
               ),
               verbatimTextOutput("code_inspector")
           )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # --- Пресеты --------------------------------------------------------------
  observeEvent(input$preset_classic, {
    updateSliderInput(session, "lorenz_sigma", value = 10)
    updateSliderInput(session, "lorenz_rho", value = 28)
    updateSliderInput(session, "lorenz_beta", value = 2.67)
  })
  observeEvent(input$preset_limit, {   # rho < 24.06: траектория сходится к неподвижной точке
    updateSliderInput(session, "lorenz_sigma", value = 10)
    updateSliderInput(session, "lorenz_rho", value = 14)
    updateSliderInput(session, "lorenz_beta", value = 2.67)
  })
  observeEvent(input$preset_circular, { updateSliderInput(session, "kepler_ecc", value = 0.0) })
  observeEvent(input$preset_comet,    { updateSliderInput(session, "kepler_ecc", value = 0.8) })
  observeEvent(input$reseed, { updateNumericInput(session, "seed", value = sample.int(99999, 1)) })

  last_ms <- reactiveVal(NA_real_)

  # --- Единый расчёт: исполняется ТОТ ЖЕ код, что показан в инспекторе -------
  # --- Модуль 11: чтение CSV и контекст данных ---------------------------------
  csv_df <- reactive({
    req(input$csv_file)
    tryCatch(read_user_csv(input$csv_file$datapath),
             error = function(e) validate(need(FALSE, paste("Не удалось прочитать файл:", conditionMessage(e)))))
  })

  output$csv_col_ui <- renderUI({
    cd <- csv_df()
    cols <- names(cd$df)[vapply(cd$df, is.numeric, logical(1))]
    validate(need(length(cols) > 0, "В файле нет числовых столбцов."))
    selectInput("csv_col", "Столбец с рядом:", choices = cols)
  })

  md_ctx <- reactive({
    if (input$md_src == "demo") {
      ds <- input$md_demo; req(ds)
      freq <- if (ds == "Nile") 1 else 12
      list(series = NULL, freq = freq, data_line = sprintf("series <- as.numeric(%s)", ds))
    } else {
      req(input$csv_file, input$csv_col)
      cd <- csv_df()
      validate(need(input$csv_col %in% names(cd$df), "Выберите столбец с рядом."))
      x <- suppressWarnings(as.numeric(cd$df[[input$csv_col]]))
      x <- x[is.finite(x)]
      validate(need(length(x) >= 40, sprintf("Нужно минимум 40 наблюдений, в столбце: %d.", length(x))))
      clean <- function(s) gsub("[\\\\\"]", "_", s)
      line <- sprintf('series <- as.numeric(read.csv("%s", sep = "%s", dec = "%s")[["%s"]])  #@upload',
                      clean(input$csv_file$name), cd$sep, cd$dec, clean(input$csv_col))
      list(series = x, freq = as.numeric(input$md_freq), data_line = line)
    }
  })

  calc_data <- reactive({
    m <- input$model_mode
    req(m)
    if (m %in% STOCHASTIC_MODES) req(input$seed)

    md   <- if (m == "mydata") md_ctx() else NULL
    code <- build_code(m, input, md)

    t0  <- Sys.time()
    env <- tryCatch(run_code(code, series = md$series),
                    error = function(e) validate(need(FALSE, paste("Ошибка расчёта:", conditionMessage(e)))))
    last_ms(round(as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000))

    list(mode = m, v = as.list(env), code = code)
  })

  # --- Скачивание исполняемого скрипта ----------------------------------------
  output$dl_code <- downloadHandler(
    filename = function() paste0("impulse_", input$model_mode, ".R"),
    content = function(file) {
      d <- calc_data()
      hdr <- c("# -*- coding: UTF-8 -*-",
               paste0("# IMPULSE DATA LAB v3.1 // модуль: ", d$mode),
               paste0("# Сформировано: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
               "# Запуск: source('<этот файл>', encoding = 'UTF-8')  # или Rscript <файл>",
               "# Пакеты: forecast, deSolve (зависит от модуля)",
               if (d$mode == "mydata") "# Для режима CSV положите файл рядом со скриптом." else NULL,
               "")
      writeLines(enc2utf8(c(hdr, d$code, "", plot_hint(d$mode))), file, useBytes = TRUE)
    }
  )

  # --- Задания ---------------------------------------------------------------
  solved      <- reactiveVal(character())
  task_answer <- reactiveVal(NULL)

  observeEvent(input$task_id, task_answer(NULL), ignoreInit = TRUE)

  observeEvent(input$task_go, {
    updateSelectInput(session, "model_mode", selected = tasks[[input$task_id]]$module)
    updateTabsetPanel(session, "main_tabs", selected = "screen")
  })

  observeEvent(input$task_check, {
    id <- input$task_id
    res <- tryCatch(tasks[[id]]$check(input),
                    error = function(e) list(ok = FALSE, msg = paste("Не удалось проверить:", conditionMessage(e))))
    # запоминаем, при каких параметрах получен ответ: если их изменить, ответ устареет
    res$id   <- id
    res$snap <- lapply(tasks[[id]]$inputs, function(i) input[[i]])
    task_answer(res)
    if (isTRUE(res$ok)) solved(union(solved(), id))
  })

  output$task_progress <- renderUI({
    div(class = "task-progress",
        sprintf("Решено: %d из %d", length(solved()), length(tasks)))
  })
  output$task_text <- renderUI({
    id <- input$task_id; req(id)
    mark <- if (id %in% solved()) "✓ решено. " else ""
    div(class = "task-text", paste0(mark, tasks[[id]]$text))
  })
  output$task_result <- renderUI({
    a <- task_answer(); req(a)
    cur <- lapply(tasks[[a$id]]$inputs, function(i) input[[i]])
    if (!identical(cur, a$snap))
      return(div(class = "task-progress", "Параметры изменились — нажмите «Проверить» ещё раз."))
    div(class = if (isTRUE(a$ok)) "task-ok" else "task-bad",
        paste(if (isTRUE(a$ok)) "Верно. " else "Пока нет. ", a$msg))
  })

  output$kpi_r <- renderText(paste0("R ", getRversion()))
  output$kpi_latency <- renderText({
    ms <- last_ms()
    if (is.na(ms)) "..." else paste0(ms, " мс")
  })

  # --- Тёмная тема Plotly ----------------------------------------------------
  p_dark <- function(p, x = list(), y = list(), margin = list(l = 45, r = 20, t = 30, b = 40), ...) {
    ax <- list(gridcolor = "rgba(255,255,255,0.06)", zerolinecolor = "rgba(255,255,255,0.12)",
               linecolor = "rgba(255,255,255,0.12)")
    p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font   = list(family = "ui-monospace, Consolas, monospace", color = "#94a3b8"),
      xaxis  = modifyList(ax, x),
      yaxis  = modifyList(ax, y),
      legend = list(font = list(color = "#cbd5e1")),
      margin = margin,
      ...
    )
  }

  # --- Отрисовка ---------------------------------------------------------------
  output$main_plot <- renderPlotly({
    d <- calc_data()
    v <- d$v

    switch(d$mode,

      arima = {
        n_h <- length(v$series)
        h   <- length(v$fc$mean)
        xf  <- c(n_h, n_h + seq_len(h))                       # прогноз стартует от последней точки
        last <- v$series[n_h]
        plot_ly() %>%
          add_lines(x = seq_len(n_h), y = v$series, name = "Факт",
                    line = list(color = "#38bdf8", width = 2)) %>%
          add_ribbons(x = xf, ymin = c(last, as.numeric(v$fc$lower)), ymax = c(last, as.numeric(v$fc$upper)),
                      name = "95% интервал", fillcolor = "rgba(16,185,129,0.15)",
                      line = list(color = "transparent")) %>%
          add_lines(x = xf, y = c(last, as.numeric(v$fc$mean)), name = v$model_name,
                    line = list(color = "#10b981", dash = "dash", width = 2)) %>%
          p_dark(x = list(title = "Шаг"), y = list(title = "Значение"))
      },

      gbm = {
        p <- plot_ly()
        for (i in seq_len(ncol(v$paths))) {
          p <- p %>% add_lines(x = v$tgrid, y = v$paths[, i], showlegend = FALSE,
                               line = list(width = 1.2, color = "rgba(56,189,248,0.55)"))
        }
        p %>%
          add_lines(x = v$tgrid, y = v$expected, name = "E[S(t)] (теория)",
                    line = list(color = "#f59e0b", width = 2.5, dash = "dash")) %>%
          p_dark(x = list(title = "Время"), y = list(title = "Цена"),
                 title = list(text = "Траектории Монте-Карло (GBM)", font = list(size = 12, color = "#cbd5e1")))
      },

      lorenz = {
        ax <- list(backgroundcolor = "rgba(0,0,0,0)", gridcolor = "rgba(255,255,255,0.08)",
                   color = "#94a3b8")
        plot_ly(v$out, x = ~X, y = ~Y, z = ~Z, type = "scatter3d", mode = "lines",
                line = list(width = 3, color = ~time, colorscale = "Viridis")) %>%
          layout(
            scene = list(xaxis = c(list(title = "X"), ax),
                         yaxis = c(list(title = "Y"), ax),
                         zaxis = c(list(title = "Z"), ax)),
            paper_bgcolor = "rgba(0,0,0,0)",
            margin = list(l = 0, r = 0, t = 0, b = 0)
          )
      },

      kepler = {
        plot_ly() %>%
          add_trace(x = v$x, y = v$y, type = "scatter", mode = "lines+markers", name = "Орбита",
                    line = list(color = "rgba(56,189,248,0.3)"),
                    marker = list(size = 4, color = v$v, colorscale = "Plasma", showscale = TRUE,
                                  colorbar = list(title = "v")),
                    text = paste0("v = ", round(v$v, 3)), hoverinfo = "text") %>%
          add_trace(x = 0, y = 0, type = "scatter", mode = "markers", name = "Фокус (масса)",
                    marker = list(size = 13, color = "#f59e0b")) %>%
          p_dark(x = list(title = "x"), y = list(title = "y", scaleanchor = "x", scaleratio = 1))
      },

      oscillator = {
        plot_ly() %>%
          add_lines(x = v$out$x, y = v$out$v, name = "Фазовая траектория",
                    line = list(color = "#c084fc", width = 2)) %>%
          add_markers(x = 1, y = 0, name = "Старт", marker = list(color = "#10b981", size = 9)) %>%
          p_dark(x = list(title = "Смещение (x)"), y = list(title = "Скорость (dx/dt)"))
      },

      eigen = {
        v1 <- v$eg$vectors[, 1] * v$eg$values[1]
        v2 <- v$eg$vectors[, 2] * v$eg$values[2]
        plot_ly() %>%
          add_lines(x = v$circle[1, ], y = v$circle[2, ], name = "Единичная окружность",
                    line = list(color = "rgba(148,163,184,0.5)", dash = "dot", width = 1.5)) %>%
          add_lines(x = v$ellipse[1, ], y = v$ellipse[2, ], name = "Образ под действием A",
                    line = list(color = "#c084fc", width = 2)) %>%
          add_segments(x = 0, y = 0, xend = v1[1], yend = v1[2], name = "lambda1 * v1",
                       line = list(color = "#38bdf8", width = 4)) %>%
          add_segments(x = 0, y = 0, xend = v2[1], yend = v2[2], name = "lambda2 * v2",
                       line = list(color = "#10b981", width = 4)) %>%
          p_dark(x = list(range = c(-6.5, 6.5)), y = list(range = c(-6.5, 6.5), scaleanchor = "x"))
      },

      svd = {
        hm <- function(z) plot_ly(z = z, type = "heatmap", colorscale = "Viridis", showscale = FALSE)
        ann <- function(txt, x) list(text = txt, x = x, y = 1.07, xref = "paper", yref = "paper",
                                     showarrow = FALSE, font = list(color = "#cbd5e1", size = 12))
        k <- v$k
        subplot(hm(v$A), hm(v$A_k), hm(v$resid), nrows = 1, margin = 0.02, shareY = TRUE) %>%
          p_dark(y = list(autorange = "reversed"),
                 annotations = list(
                   ann("Оригинал", 0.15),
                   ann(sprintf("Ранг %d  (%.1f%% энергии)", k, v$energy * 100), 0.5),
                   ann("Остаток", 0.85)
                 ),
                 margin = list(l = 35, r = 15, t = 50, b = 30))
      },

      stats = {
        plot_ly() %>%
          add_histogram(x = v$x, histnorm = "probability density", name = "Выборка",
                        marker = list(color = "rgba(56,189,248,0.6)", line = list(color = "#0284c7", width = 1))) %>%
          add_lines(x = v$grid, y = v$dens, name = "Теор. плотность",
                    line = list(color = "#f59e0b", width = 2.5)) %>%
          p_dark(x = list(title = "x"), y = list(title = "Плотность"))
      },

      regression = {
        o <- order(v$x)
        plot_ly() %>%
          add_markers(x = v$x, y = v$y, name = "Наблюдения",
                      marker = list(color = "#38bdf8", opacity = 0.7)) %>%
          add_lines(x = v$ex, y = v$ey, name = "95% эллипс (теор.)",
                    line = list(color = "rgba(192,132,252,0.8)", dash = "dot", width = 1.5)) %>%
          add_lines(x = v$x[o], y = predict(v$fit)[o], name = "МНК-линия",
                    line = list(color = "#f43f5e", width = 2)) %>%
          p_dark(x = list(title = "x"), y = list(title = "y"))
      },

      mydata = {
        n <- v$n; k <- v$k
        tr_x <- seq_len(n - k); te_x <- (n - k + 1):n
        plot_ly() %>%
          add_lines(x = tr_x, y = v$series[tr_x], name = "Обучающая часть",
                    line = list(color = "#38bdf8", width = 2)) %>%
          add_ribbons(x = te_x, ymin = as.numeric(v$fc$lower), ymax = as.numeric(v$fc$upper),
                      name = "95% интервал", fillcolor = "rgba(16,185,129,0.15)",
                      line = list(color = "transparent")) %>%
          add_lines(x = te_x, y = v$test, name = "Факт (отложен)",
                    line = list(color = "#f8fafc", width = 2)) %>%
          add_lines(x = te_x, y = as.numeric(v$fc$mean), name = v$model_name,
                    line = list(color = "#10b981", dash = "dash", width = 2)) %>%
          add_lines(x = te_x, y = v$naive, name = "Эталон: сезонный наивный",
                    line = list(color = "#f59e0b", dash = "dot", width = 1.5)) %>%
          p_dark(x = list(title = "Номер наблюдения"), y = list(title = "Значение"))
      },

      network = {
        p <- plot_ly()
        ed <- v$edges
        if (nrow(ed) > 0) {            # все рёбра одним trace (NA разрывает линию)
          ex <- as.vector(rbind(v$coords$x[ed[, 1]], v$coords$x[ed[, 2]], NA))
          ey <- as.vector(rbind(v$coords$y[ed[, 1]], v$coords$y[ed[, 2]], NA))
          p <- p %>% add_trace(x = ex, y = ey, type = "scatter", mode = "lines", hoverinfo = "none",
                               showlegend = FALSE, line = list(color = "rgba(56,189,248,0.25)", width = 1))
        }
        p %>%
          add_trace(x = v$coords$x, y = v$coords$y, type = "scatter", mode = "markers", name = "Узлы",
                    text = paste0("Узел ", seq_along(v$deg), ": степень ", v$deg), hoverinfo = "text",
                    marker = list(size = 10 + v$deg * 2.5, color = v$deg, colorscale = "Viridis",
                                  showscale = TRUE, colorbar = list(title = "Степень"),
                                  line = list(color = "rgba(255,255,255,0.4)", width = 1))) %>%
          p_dark(x = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 y = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, scaleanchor = "x"))
      }
    )
  })

  # --- Таблица расчётов (все 10 модулей) -------------------------------------
  kv <- function(a, b) data.frame(Показатель = a, Значение = as.character(b), check.names = FALSE)

  output$raw_data_table <- renderTable({
    d <- calc_data()
    v <- d$v

    switch(d$mode,
      arima = {
        h <- length(v$fc$mean)
        kv(c("Модель", "AIC", "Дисперсия остатков", "Горизонт, шагов", "Последнее наблюдение",
             "Прогноз в конце горизонта", "95% интервал в конце"),
           c(v$model_name, round(v$fit$aic, 2), round(v$fit$sigma2, 3), h,
             round(tail(v$series, 1), 2), round(as.numeric(v$fc$mean)[h], 2),
             sprintf("[%.2f; %.2f]", as.numeric(v$fc$lower)[h], as.numeric(v$fc$upper)[h])))
      },
      gbm = {
        term <- v$paths[nrow(v$paths), ]
        kv(c("Траекторий", "Среднее S(T) (Монте-Карло)", "Теоретическое E[S(T)]",
             "Станд. откл. S(T)", "Минимум S(T)", "Максимум S(T)"),
           c(length(term), round(mean(term), 2), round(tail(v$expected, 1), 2),
             round(sd(term), 2), round(min(term), 2), round(max(term), 2)))
      },
      lorenz = head(round(v$out, 3), 12),
      kepler = {
        e <- v$e
        kv(c("Эксцентриситет e", "Перицентр r_min = 1 - e", "Апоцентр r_max = 1 + e",
             "Скорость в перицентре", "Скорость в апоцентре", "Точек на траектории"),
           c(e, round(1 - e, 3), round(1 + e, 3), round(sqrt((1 + e) / (1 - e)), 3),
             round(sqrt((1 - e) / (1 + e)), 3), length(v$x)))
      },
      oscillator = {
        z <- v$zeta
        regime <- if (abs(z - 1) < 1e-9) "критическое затухание" else if (z < 1) "колебательный режим" else "апериодический режим"
        kv(c("Коэффициент затухания zeta = gamma/omega0", "Режим",
             "Частота затухающих колебаний", "Точек решения"),
           c(round(z, 3), regime,
             if (z < 1) round(sqrt(v$omega0^2 - v$damping^2), 3) else "—", nrow(v$out)))
      },
      eigen = {
        ev <- v$eg$values
        cls <- if (all(ev > 0)) "положительно определена" else if (all(ev < 0)) "отрицательно определена" else "знаконеопределённая"
        data.frame(
          Показатель = c("lambda1", "lambda2", "След", "Определитель", "Тип матрицы"),
          Значение   = c(round(ev, 3), round(sum(diag(v$A)), 3), round(det(v$A), 3), cls),
          Вектор_X   = c(round(v$eg$vectors[1, ], 3), "", "", ""),
          Вектор_Y   = c(round(v$eg$vectors[2, ], 3), "", "", ""),
          check.names = FALSE)
      },
      svd = {
        i <- 1:8
        data.frame(i = i,
                   `σ_i` = round(v$s$d[i], 3),
                   `Накопл. энергия, %` = round(cumsum(v$s$d^2)[i] / sum(v$s$d^2) * 100, 1),
                   Выбран = ifelse(i <= v$k, "✓", ""), check.names = FALSE)
      },
      stats = {
        x <- v$x; m <- mean(x); s <- sqrt(mean((x - m)^2))
        kv(c("N", "Среднее", "Станд. откл.", "Медиана", "Асимметрия", "Минимум", "Максимум"),
           c(length(x), round(m, 3), round(sd(x), 3), round(median(x), 3),
             round(mean((x - m)^3) / s^3, 3), round(min(x), 3), round(max(x), 3)))
      },
      regression = {
        sm <- summary(v$fit)
        kv(c("N", "Выборочная корреляция r", "Наклон", "Свободный член", "R²", "p-value наклона"),
           c(length(v$x), round(cor(v$x, v$y), 3), round(coef(v$fit)[2], 3), round(coef(v$fit)[1], 3),
             round(sm$r.squared, 3), format.pval(sm$coefficients[2, 4], digits = 3)))
      },
      network = {
        data.frame(Узел = seq_along(v$deg), Степень = v$deg,
                   `Центральность` = round(v$dcent, 3), check.names = FALSE)
      },
      mydata = {
        ds <- v$desc; mt <- v$metrics
        better <- mt["model", "MASE"] < mt["naive", "MASE"]
        lbp <- v$lb$p.value
        rbind(
          kv("— Описательная статистика —", ""),
          kv(c("N", "Среднее", "Станд. откл.", "Минимум", "Q1", "Медиана", "Q3", "Максимум",
               "Асимметрия", "Эксцесс", "Shapiro-Wilk, p"),
             c(ds[["N"]],
               round(ds[c("mean", "sd", "min", "q1", "median", "q3", "max", "skew", "ex_kurt")], 3),
               format.pval(ds[["shapiro_p"]], digits = 3))),
          kv(sprintf("— Бэктест на последних %d наблюдениях —", v$k), ""),
          kv(c("Модель (auto.arima)", "Преобразование Бокса–Кокса", "MASE: модель / эталон",
               "sMAPE, %: модель / эталон", "RMSE: модель / эталон", "Покрытие 95% интервала", "Вывод"),
             c(v$model_name,
               if (is.null(v$fit$lambda)) "нет" else sprintf("λ = %.3f", v$fit$lambda),
               sprintf("%.3f / %.3f", mt["model", "MASE"], mt["naive", "MASE"]),
               sprintf("%.2f / %.2f", mt["model", "sMAPE"], mt["naive", "sMAPE"]),
               sprintf("%.2f / %.2f", mt["model", "RMSE"], mt["naive", "RMSE"]),
               sprintf("%.0f%% (ожидание ≈ 95%%)", 100 * v$coverage),
               if (better) "модель точнее эталона (MASE ниже)" else "эталон не побит: ARIMA здесь не даёт выигрыша")),
          kv("— Диагностика остатков —", ""),
          kv(c("Ljung-Box, p", "Интерпретация", "Разностей по KPSS (ndiffs)"),
             c(format.pval(lbp, digits = 3),
               if (is.finite(lbp) && lbp > 0.05) "остатки похожи на белый шум" else "в остатках осталась автокорреляция",
               v$d_kpss))
        )
      }
    )
  }, digits = 4, na = "")

  # --- Code Inspector: показываем исполняемый код + подсказку по визуализации -
  output$code_inspector <- renderText({
    d <- calc_data()
    paste0(d$code, "\n\n# --- визуализация (в приложении рисует Plotly) ---\n", plot_hint(d$mode))
  })
}

shinyApp(ui, server)

# ==============================================================================
# CHANGELOG v3.0 -> v3.1
#  * Модуль 11: переключатель преобразования Бокса-Кокса (lambda = "auto") для рядов
#    с растущим размахом сезонности; значение lambda в таблице расчётов.
#  * Плашка ИИ в шапке: Gemini + Claude (как в README и docs/AI_AUDIT.md).
#
# CHANGELOG v2.4 -> v3.0
#  * Модуль 11 "Свои данные": загрузка CSV (авто-разделитель, десятичный знак,
#    кодировка) или демо-наборы; описательная статистика; бэктест auto.arima
#    на отложенной части с эталоном (сезонный наивный), MASE/sMAPE/RMSE,
#    покрытие 95% интервала, тест Льюнга-Бокса, ndiffs (KPSS).
#  * Вкладка "Задания": 5 заданий с автопроверкой по тем же слайдерам.
#  * Кнопка "Скачать .R": исполняемый скрипт модуля с текущими параметрами.
#  * Исправлено: p_dark() получал margin дважды (падение модуля SVD).
#  * run_code()/read_user_csv() вынесены в функции -> тестируются без UI
#    (tests/smoke.R).
#
# CHANGELOG v2.3 -> v2.4
#  * Code Inspector исполняется: calc_data() делает eval() того же текста.
#  * Убраны CDN (Google Fonts, Font Awesome): офлайн-работа, как заявлено.
#  * Терминал и вкладки перекрашены в тёмную тему.
#  * KPI: реальный замер времени расчёта и версия R из сессии.
#  * Seed вынесен в интерфейс (воспроизводимость + кнопка "Новая выборка").
#  * ARIMA: ряд = тренд + интегрированный AR(1), а не чистое блуждание;
#    прогноз стыкуется с последней точкой; показана выбранная модель.
#  * GBM: добавлена теоретическая E[S(t)] и сверка с Монте-Карло в таблице.
#  * Лоренц: метод rk4, beta = 2.67 (≈ 8/3), пресет "Цикл" заменён на честный "Покой".
#  * Кеплер: точки равномерны по времени (уравнение Кеплера, Ньютон).
#  * Осциллятор: решается ОДУ через deSolve; режим по zeta.
#  * Eigen: добавлен A[2,2], эллипс - образ единичной окружности.
#  * SVD: структурный сигнал + полноранговый шум, 3 панели, доля энергии.
#  * Статистика: поверх гистограммы теоретическая плотность.
#  * Регрессия: корреляция точно равна заданной, 95% эллипс.
#  * Сеть: симметричная матрица смежности, рёбра одним trace, центральность.
#  * "Матрица расчётов" работает для всех 10 модулей.
# ==============================================================================
