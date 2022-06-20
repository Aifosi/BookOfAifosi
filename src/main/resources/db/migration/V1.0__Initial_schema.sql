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
  id            uuid PRIMARY KEY   DEFAULT gen_random_uuid(),
  chaster_name  text      NOT NULL,
  discord_id    bigint    NOT NULL,
  access_token  text      NOT NULL,
  expires_at    timestamp NOT NULL,
  refresh_token text      NOT NULL,
  scope         text      NOT NULL,
  created_at    timestamp NOT NULL DEFAULT NOW(),
  updated_at    timestamp NOT NULL DEFAULT NOW(),
  UNIQUE (chaster_name, discord_id)
);

CREATE TABLE task_subscriptions (
  user_id                uuid      NOT NULL REFERENCES users (id),
  lock_id                text      NOT NULL,
  most_recent_event_time timestamp NULL,
  created_at             timestamp NOT NULL DEFAULT NOW(),
  updated_at             timestamp NOT NULL DEFAULT NOW(),
  PRIMARY KEY (user_id, lock_id)
);

CREATE TABLE pending_tasks (
  id           uuid PRIMARY KEY   DEFAULT gen_random_uuid(),
  user_id      uuid      NOT NULL REFERENCES users (id),
  keyholder_id uuid      NOT NULL REFERENCES users (id),
  deadline     timestamp NOT NULL,
  created_at   timestamp NOT NULL DEFAULT NOW(),
  updated_at   timestamp NOT NULL DEFAULT NOW()
);