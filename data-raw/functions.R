## code to prepare `functions` dataset goes here



tibble::tribble(
  ~"short_name", ~"description", ~"shift", ~"aggregate", ~"compare",
  "YoY", "Full Year over Year", "", "", "X",
  "YTD", "Year-to-Date", "", "X", "",
  "PYTD", "Prior Year-to-Date amount", "X", "X", "",
  "YoYTD", "Current Year-to-Date over Prior Year-to-Date", "X", "X", "X",
  "YTDOPY", "Year-to-Date over Full Previous Year", "X", "X", "X",
  "PYMAT", "Previous Year Moving Annual Total", "X", "X", "X",
  "QoQ", "Full Quarter over Quarter", "", "", "X",
  "QTD", "Quarter-to-Date", "", "X", "",
  "PQTD", "Prior Quarter-to-Date", "X", "X", "",
  "QOQTD", "Quarter-over-Quarter-to-Date", "X", "X", "X",
  "QTDOPQ", "Quarter-to-Date over Full Previous Quarter", "X", "X", "X",
  "MTD", "Month-to-Date", "", "X", "",
  "MoM", "Full Month over Full Month", "", "", "X",
  "MoMTD", "Current Month-to-Date over Prior Month-to-Date", "X", "X", "X",
  "PMTD", "Prior Month's MTD amount", "X", "X", "",
  "MTDOPM", "Month-to-Date over Full Previous Month", "X", "X", "X",
  "WTD", "Week-to-Date", "", "X", "",
  "WoW", "Full Week over Full Week", "", "", "X",
  "WoWTD", "Current Week-to-Date over Prior Week-to-Date", "X", "X", "X",
  "PWTD", "Prior Week-to-Date", "X", "X", "",
  "WTDOPW", "Week-to-Date over Full Previous Week", "X", "X", "X",
  "MAT", "Moving Annual Total of Previous X Days", "X", "X", "",
  "ATD","cumlaitve total from inception to date","","x","",
  "DoD", "Full Day over Full Day", "", "", "X",

) |> gt::gt()





