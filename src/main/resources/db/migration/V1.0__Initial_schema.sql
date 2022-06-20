CREATE TABLE tasks (
  id          uuid PRIMARY KEY   DEFAULT gen_random_uuid(),
  name        text      NOT NULL,
  description text      NOT NULL,
  created_at  timestamp NOT NULL DEFAULT NOW(),
  updated_at  timestamp NOT NULL DEFAULT NOW()
);

CREATE TABLE tags (
  id          uuid PRIMARY KEY   DEFAULT gen_random_uuid(),
  name        text      NOT NULL,
  description text      NULL     DEFAULT NULL,
  created_at  timestamp NOT NULL DEFAULT NOW(),
  updated_at  timestamp NOT NULL DEFAULT NOW()
);

CREATE TABLE task_tags (
  id         uuid PRIMARY KEY   DEFAULT gen_random_uuid(),
  task       uuid      NOT NULL REFERENCES tasks (id),
  tag        uuid      NOT NULL REFERENCES tags (id),
  created_at timestamp NOT NULL DEFAULT NOW(),
  updated_at timestamp NOT NULL DEFAULT NOW()
);

CREATE TABLE users (
  chaster_name  text                     NOT NULL,
  discord_id    bigint                   NOT NULL,
  access_token  text                     NOT NULL,
  expires_at    timestamp WITH TIME ZONE NOT NULL,
  refresh_token text                     NOT NULL,
  scope         text                     NOT NULL,
  created_at    timestamp                NOT NULL DEFAULT NOW(),
  updated_at    timestamp                NOT NULL DEFAULT NOW(),
  PRIMARY KEY (chaster_name, discord_id)
);