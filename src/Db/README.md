# Db: CRUD functionality

This project implements all CRUD functtions for the system. There are two
reasons for having all database related functionality in a separate
project:

1. Keep the rest of the system database agnostic.
2. Due to current complications with the SQL provider and .NET Core it is
   necessary to have the type provider separated from the rest of the system,
   in order to resolve dependencies correctly.