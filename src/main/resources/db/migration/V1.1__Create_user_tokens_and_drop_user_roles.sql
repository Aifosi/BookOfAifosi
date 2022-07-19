DROP TABLE user_roles;

CREATE TABLE user_tokens
AS
  SELECT access_token, expires_at, refresh_token, scope, created_at, updated_at
  FROM users;

ALTER TABLE user_tokens
  ADD id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  ADD CONSTRAINT access_token_unique UNIQUE (access_token);

ALTER TABLE users
  ADD token_id uuid NULL DEFAULT NULL;

UPDATE users
SET token_id = (SELECT id FROM user_tokens WHERE users.access_token = access_token);

ALTER TABLE users
  ALTER token_id DROP DEFAULT,
  ALTER token_id SET NOT NULL,
  ADD CONSTRAINT token_id_fkey FOREIGN KEY (token_id) REFERENCES user_tokens (id),
  ADD keyholder_ids text[]  NOT NULL DEFAULT '{}',
  ADD is_locked     boolean NOT NULL DEFAULT FALSE,
  DROP access_token,
  DROP expires_at,
  DROP refresh_token,
  DROP scope;