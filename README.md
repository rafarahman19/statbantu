# statbantu

`statbantu` adalah R package ringan buatan lokal yang dirancang khusus untuk memudahkan mahasiswa statistika membuat **Tabel Distribusi Frekuensi (TDF)**, menghitung **Statistik 5 Serangkai**, dan menampilkan **3 Grafik Sekaligus** (Bar, Line, Pie) secara otomatis—lengkap dengan antarmuka GUI ala `Rcmdr`.

## Cara Install

Kamu bisa meng-install package ini langsung dari GitHub menggunakan *package* `remotes`:

```R
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("rafarahman19/statbantu")
