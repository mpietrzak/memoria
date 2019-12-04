
create table session (
    id varchar(128) not null primary key,
    key varchar(128) not null,
    created_at timestamp not null,
    modified_at timestamp not null
);

create unique index session_key_ui on session (key);
create index session_created_i on session (created_at);
create index session_modified_i on session (modified_at);

