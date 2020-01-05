
create table session (
    id varchar(128) not null primary key,
    key varchar(128) not null,
    created_at timestamp not null,
    modified_at timestamp not null
);

create unique index session_key_ui on session (key);
create index session_created_i on session (created_at);
create index session_modified_i on session (modified_at);


create table account (
    id varchar(128) primary key,
    created_at timestamp not null,
    modified_at timestamp not null
);

create index account_created_i on session (created_at);
create index account_modified_i on session (modified_at);


create table session_value (
    id varchar(128) not null,
    session varchar(128) not null references session,
    name varchar(128) not null,
    value varchar(256) not null,
    created_at timestamp not null,
    modified_at timestamp not null
);

create unique index session_value_ui on session_value (session, name);
create index session_value_created_i on session_value (created_at);
create index session_value_modified_i on session_value (modified_at);
create index session_value_value_i on session_value (value);


create table question_set (
    id varchar(128) not null,
    name varchar(128) not null,
    owner varchar(128) not null references account,
    created_at timestamp not null,
    modified_at timestamp not null
);

create index question_set_ui on question_set (owner, name);
create index question_set_created_i on question_set (created_at);
create index question_set_modified_i on question_set (modified_at);

