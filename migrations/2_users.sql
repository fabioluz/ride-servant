create table users (
  user_id uuid primary key not null,
  client_id uuid not null references clients(client_id),
  email text not null,
  password text not null,
  name text not null
);