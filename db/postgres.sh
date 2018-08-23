#!/usr/bin/env sh

# n=$(docker images postgres-jupitodo | wc -l)
# if [ $n = 1 ]; then
#     docker build -t postgres-jupitodo .
# fi
docker run --rm --name jupidb \
    -e POSTGRES_PASSWORD=secret \
    -p 5432:5432 \
    -d \
    postgres:latest
sleep 5
docker cp db.sql jupidb:/
docker cp schema.sql jupidb:/
docker exec jupidb psql -f db.sql postgres postgres
docker exec jupidb psql -f schema.sql jupitodo postgres
