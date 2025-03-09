# doownload 10k file


fp <- "https://github.com/sql-bi/Contoso-Data-Generator-V2-Data/releases/download/ready-to-use-data/csv-10k.7z"


# create file locations

dest <- tempfile()
extract <- here::here("data-raw")

# download 100k files
download.file(fp,destfile = dest,mode="wb")


# extract files to data-raw folder

archive::archive_extract(archive = dest,dir =extract)


