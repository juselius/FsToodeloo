create schema dbo;

create table if not exists dbo.todo(
    taskid integer primary key not null,
    priority integer not null,
    due timestamp,
    task text not null);

