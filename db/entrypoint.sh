#!/usr/bin/env bash

/docker-entrypoint.sh &
sleep 5
psql -U postgres -f schema.sql
while true; do
    sleep 3600
done
