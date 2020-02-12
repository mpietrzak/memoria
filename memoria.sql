-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.


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
    id varchar(128) not null primary key,
    name varchar(128) not null,
    owner varchar(128) not null references account,
    is_deleted boolean,
    deleted_at timestamp,
    created_at timestamp not null,
    modified_at timestamp not null
);

create index question_set_ui on question_set (owner, name);
create index question_set_created_i on question_set (created_at);
create index question_set_modified_i on question_set (modified_at);


create table question (
    id varchar(128) not null primary key,
    question_set varchar(128) not null references question_set,
    question varchar(1024) not null,
    answer varchar(1024) not null,
    created_at timestamp not null,
    modified_at timestamp not null
);

create index question_question_set_i on question (question_set);
create index question_created_i on question (created_at);
create index question_modified_i on question (modified_at);


create table account_email (
    id varchar(128) not null primary key,
    account varchar(128) not null references account,
    email varchar(128) not null,
    created_at timestamp not null,
    modified_at timestamp not null
);

create index account_email_account_i on account_email (account);
create index account_email_email_i on account_email (email);
create index account_email_created_i on account_email (created_at);
create index account_email_modified_i on account_email (modified_at);


create table login_token (
    id varchar(128) not null primary key,
    token varchar(128) not null,
    account varchar(128) not null references account,
    expires_at timestamp not null,
    created_at timestamp not null,
    modified_at timestamp not null
);

create unique index login_token_token_ui on login_token (token);
create index login_token_acc_id on login_token (account);
create index login_token_created_i on login_token (created_at);
create index login_token_modified_i on login_token (modified_at);


create table question_answer (
    id varchar(128) not null primary key,
    account varchar(128) not null references account,
    question varchar(128) not null references question,
    answer varchar(1024) not null,
    is_correct boolean not null,
    answered_at timestamp not null,
    created_at timestamp not null,
    modified_at timestamp not null
);

create index question_answer_account_i on question_answer (account);
create index question_answer_question_i on question_answer (question);
create index question_answer_answered_i on question_answer (answered_at);
create index question_answer_created_i on question_answer (created_at);
create index question_answer_modified_i on question_answer (modified_at);
create index question_answer_qccount_question_answered_i
on question_answer (account, question, answered_at);


create table question_set_subscription (
    id varchar(128) not null primary key,
    question_set varchar(128) not null references question_set,
    account varchar(128) not null references account,
    subscribed boolean,
    created_at timestamp not null,
    modified_at timestamp not null
);

create index question_set_subscription_account_i on question_set_subscription (account);
create index question_set_subscription_created_i on question_set_subscription (created_at);
create index question_set_subscription_modified_i on question_set_subscription (modified_at);
create unique index question_set_subscription_ui
on question_set_subscription (account, question_set);
