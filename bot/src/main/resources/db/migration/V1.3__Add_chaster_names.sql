CREATE TABLE user_chaster_names
AS
  SELECT id, chaster_name
  FROM users;

ALTER TABLE users
  ADD chaster_id text NULL DEFAULT NULL,
  DROP chaster_name;