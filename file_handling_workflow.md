

upload .zip file

  * unzip .csv's into tmp directory

  * generate "Submission" data

  * write "Submission" table to .parquet file in data lake

  * read in "Client" .csv file
    + keep only desired columns
    + perform data transformations (join to lookup table codes)
    + write out "Client" table to .parquet file in data lake

  * read in "Program" .csv file...
