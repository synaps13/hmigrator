# hmigrator
A lib and cli tool to perform migrations on Postgres database. At the moment it is possible only to apply migrations.

# TODO Plan
- pick ENV variables to get connection information (Use dedicated ones)
- add testcontainers to test to allow test independent execution
- Check & create structure in database to track migrations
- Include checks for DML (Data Manipulation Language) and DDL (Data Definition Language)
