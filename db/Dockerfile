FROM postgres:latest
EXPOSE 5432
WORKDIR /sql
COPY ./schema.sql .
COPY ./entrypoint.sh .
CMD sh ./entrypoint.sh
