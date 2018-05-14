# Raw VICo data

This repository documents the raw data extraction from the VICo database.

It was developed under ubuntu 16.04 LTS using R version 3.5.0.

## Database connection

Connection to the VICo database requires on-site access and security credentials.
The connection to the database is established using DBI, with FreeTDS as the odbc connector.

