create table users (
  user_id uuid primary key not null,
  email text not null,
  password text not null,
  name text not null
);