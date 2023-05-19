attributes_catchments_file <- "data_input/attributes/attributes_45catchments_ChileCentral.csv"
attributes_catchments <- fread(attributes_catchments_file)

codigos_cuencas = attributes_catchments$cod_cuenca
