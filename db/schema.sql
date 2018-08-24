create schema dbo;

create table if not exists dbo.todo(
    taskid serial primary key,
    priority integer not null,
    due timestamp,
    task text not null);

