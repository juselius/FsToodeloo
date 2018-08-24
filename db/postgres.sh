#!/usr/bin/env sh
docker run --rm --name toodeloodb \
    -e POSTGRES_PASSWORD=secret \
    -p 5432:5432 \
    -d \
    postgres:latest
docker cp schema.sql toodeloodb:/
sleep 2
docker exec toodeloodb psql -c "create database toodeloo;" postgres postgres
docker exec toodeloodb psql -f schema.sql toodeloo postgres
