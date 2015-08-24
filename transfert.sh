#!/bin/bash

cp /home/rstudio/DB.R .
cp /home/rstudio/backend.R .
#cp /home/rstudio/vorskript.R .
#cp /home/rstudio/global.R .
cp /home/rstudio/server.R .
cp /home/rstudio/ui.R .

R < DB.R --save
R < backend.R --save
#R < vorskript.R --save
#R < global.R --save
R < server.R --save
R < ui.R --save

restart shiny-server
