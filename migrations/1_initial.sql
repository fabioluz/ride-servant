create table clients (
  client_id uuid primary key not null,
  name text not null
);

create table users (
  user_id uuid primary key not null,
  client_id uuid not null references clients(client_id),
  name text not null
);