devtools::check_win_devel()

devtools::check_win_release()

devtools::check_win_oldrelease()

# find Ghostscript-Executable:
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.53.3\\bin\\gswin64c.exe")

# reduce size of pdf-vignette:
library(tools)
tools::compactPDF(
  paths = "D:\\Daten_Dissertation\\vignette_forestinventory\\final_edit\\zeileis_edit_finalversion\\export_to_vignette\\forestinventory_vignette.pdf",
  gs_quality = "printer")

# final
devtools::spell_check()
devtools::check_rhub()

# release package:
devtools::release()
