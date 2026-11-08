#' Tabel Distribusi Frekuensi, 5 Serangkai & 3 Grafik (Ala Excel)
#'
#' @param x Vektor numerik.
#' @param ub_awal Nilai Ujung Bawah kelas pertama (default: nilai minimum data).
#' @param plot Logical; tampilkan 3 grafik otomatis? (default: TRUE).
#' @param view Logical; buka tabel bergaya spreadsheet di RGui? (default: FALSE).
#' @return List berisi info parameter, dataframe TDF, dan Statistik 5 Serangkai.
#' @export
bikin_tdf <- function(x, ub_awal = NULL, plot = TRUE, view = FALSE) {
  x <- suppressWarnings(stats::na.omit(as.numeric(gsub(",", ".", x))))
  stopifnot("Data harus punya minimal 2 angka berbeda" = length(x) > 1 && min(x) < max(x))

  n <- length(x)
  # ponytail: deteksi 0 vs 1 desimal; ubah ke 10^(-d) jika butuh presisi >1 desimal
  is_int <- all(x %% 1 == 0)
  satuan <- if (is_int) 1 else 0.1

  xmin <- min(x); xmax <- max(x); rng <- xmax - xmin
  k_raw <- 1 + 3.3 * log10(n); k <- ceiling(k_raw)
  i_raw <- rng / k_raw; interval <- ceiling(i_raw)

  ub0 <- if (is.null(ub_awal) || is.na(ub_awal)) xmin else as.numeric(ub_awal)
  if (ub0 + k * interval - satuan < xmax) interval <- interval + 1

  ub <- round(ub0 + (0:(k - 1)) * interval, 2)
  ua <- round(ub + interval - satuan, 2)
  nt <- round((ub + ua) / 2, 2)
  bb <- round(ub - satuan / 2, 2)
  ba <- round(ua + satuan / 2, 2)

  f <- vapply(seq_len(k), function(j) sum(x >= bb[j] & x <= ba[j]), integer(1))
  persen <- round(f / n * 100, 2)

  fmt <- if (is_int) "%d-%d" else "%.1f-%.1f"
  tdf <- data.frame(
    `Rentang Kelas` = sprintf(fmt, ub, ua),
    UB = ub, UA = ua, NT = nt, F = f, BB = bb, BA = ba,
    `FK>=` = rev(cumsum(rev(f))), `FK<=` = cumsum(f), Persentase = persen,
    check.names = FALSE
  )

  if (plot) {
    op <- graphics::par(mfrow = c(1, 3), mar = c(5, 4, 4, 2))
    on.exit(graphics::par(op))
    graphics::barplot(f, names.arg = tdf[["Rentang Kelas"]], col = "steelblue",
                      main = "Distribusi Rentang", ylab = "Frekuensi", las = 2)
    graphics::plot(seq_len(k), f, type = "b", pch = 19, lwd = 2, col = "steelblue",
                   xaxt = "n", xlab = "Rentang Kelas", ylab = "Frekuensi", main = "Tren Distribusi")
    graphics::axis(1, at = seq_len(k), labels = tdf[["Rentang Kelas"]], las = 2)
    graphics::pie(f, labels = paste0(tdf[["Rentang Kelas"]], "\n(", persen, "%)"),
                  main = "Persentase Distribusi")
  }

  if (view) utils::View(tdf, title = "Tabel Distribusi Frekuensi")

  list(
    info = round(c(Max = xmax, Min = xmin, Range = rng, Kelas = k_raw, Interval = i_raw), 4),
    tdf = tdf,
    lima_serangkai = stats::setNames(stats::fivenum(x), c("Min", "Q1", "Median", "Q3", "Max"))
  )
}

#' Buka Jendela GUI Statbantu (Mesin tcltk Bawaan R seperti Rcmdr)
#'
#' @export
bikin_tdf_gui <- function() {
  env <- new.env(parent = emptyenv())
  env$x <- numeric(0)

  win <- tcltk::tktoplevel()
  tcltk::tkwm.title(win, "Statbantu GUI")
  status <- tcltk::tclVar("Status: Belum ada data (0 angka)")
  ub_var <- tcltk::tclVar("")

  update_status <- function(x) {
    env$x <- suppressWarnings(stats::na.omit(as.numeric(gsub(",", ".", x))))
    tcltk::tclvalue(status) <- sprintf(
      "Status: %d angka siap (Min: %s | Max: %s)", length(env$x),
      if (length(env$x)) min(env$x) else "-", if (length(env$x)) max(env$x) else "-"
    )
  }

  # Opsi 1: Blok kolom di Excel -> Ctrl+C -> Klik tombol ini
  btn_paste <- tcltk::tkbutton(win, text = "1. Paste Kolom dari Excel (Ctrl+C)", width = 36, command = function() {
    raw <- if (.Platform$OS.type == "windows") utils::readClipboard() else scan("clipboard", what = "", quiet = TRUE)
    update_status(unlist(strsplit(raw, "[[:space:];]+")))
  })

  # Opsi 2: Ketik/Edit langsung di tabel kotak-kotak bawaan RGui
  btn_edit <- tcltk::tkbutton(win, text = "2. Ketik / Edit di Tabel Spreadsheet", width = 36, command = function() {
    df <- utils::edit(data.frame(Data = if (length(env$x)) env$x else numeric(0)))
    update_status(df[[1]])
  })

  # Opsi 3: Browse file CSV dan pilih kolom via pop-up
  btn_csv <- tcltk::tkbutton(win, text = "3. Pilih File CSV", width = 36, command = function() {
    f <- tcltk::tk_choose.files(caption = "Pilih File CSV")
    if (length(f) && nzchar(f[1])) {
      df <- utils::read.csv(f[1])
      kol <- utils::select.list(names(df), title = "Pilih Kolom", graphics = TRUE)
      if (nzchar(kol)) update_status(df[[kol]])
    }
  })

  ent_ub <- tcltk::tkentry(win, textvariable = ub_var, width = 12)
  btn_run <- tcltk::tkbutton(win, text = "PROSES TABEL & 3 GRAFIK", width = 36, bg = "lightblue", command = function() {
    if (length(env$x) < 2) return(tcltk::tkmessageBox(message = "Input minimal 2 angka dulu!", icon = "warning"))
    ub_txt <- trimws(tcltk::tclvalue(ub_var))
    ub_val <- if (nzchar(ub_txt)) as.numeric(gsub(",", ".", ub_txt)) else NULL
    print(bikin_tdf(env$x, ub_awal = ub_val, plot = TRUE, view = TRUE))
  })

  tcltk::tkpack(tcltk::tklabel(win, textvariable = status), pady = 8, padx = 15)
  tcltk::tkpack(btn_paste, btn_edit, btn_csv, pady = 3, padx = 15)
  tcltk::tkpack(tcltk::tklabel(win, text = "UB Kelas Pertama (Kosongkan = Otomatis Min):"), pady = c(8, 0))
  tcltk::tkpack(ent_ub, pady = 3)
  tcltk::tkpack(btn_run, pady = 12, padx = 15)
  invisible(win)
}
