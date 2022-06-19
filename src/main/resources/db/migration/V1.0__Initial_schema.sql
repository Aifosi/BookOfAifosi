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
  task       uuid       NOT NULL REFERENCES tasks (id),
  tag        uuid       NOT NULL REFERENCES tags (id),
  created_at timestamp NOT NULL DEFAULT NOW(),
  updated_at timestamp NOT NULL DEFAULT NOW()
);

CREATE TABLE users (
  id           uuid PRIMARY KEY   DEFAULT gen_random_uuid(),
  chaster_name text      NOT NULL,
  discord_name text      NOT NULL,
  created_at   timestamp NOT NULL DEFAULT NOW(),
  updated_at   timestamp NOT NULL DEFAULT NOW()
);