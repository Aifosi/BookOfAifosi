CREATE TABLE tasks (
  id          uuid PRIMARY KEY                  DEFAULT gen_random_uuid(),
  name        text                     NOT NULL,
  description text                     NOT NULL,
  created_at  timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at  timestamp WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE TABLE tags (
  id          uuid PRIMARY KEY                  DEFAULT gen_random_uuid(),
  name        text                     NOT NULL,
  description text                     NULL     DEFAULT NULL,
  created_at  timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at  timestamp WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE TABLE task_tags (
  id           uuid PRIMARY KEY                  DEFAULT gen_random_uuid(),
  task         uuid                     NOT NULL REFERENCES tasks (id),
  tag          uuid                     NOT NULL REFERENCES tags (id),
  is_blacklist boolean                  NOT NULL,
  created_at   timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at   timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  UNIQUE (task, tag)
);

CREATE TABLE users (
  id              uuid PRIMARY KEY                  DEFAULT gen_random_uuid(),
  chaster_name    text UNIQUE              NOT NULL,
  user_discord_id bigint UNIQUE            NOT NULL,
  access_token    text                     NOT NULL,
  expires_at      timestamp WITH TIME ZONE NOT NULL,
  refresh_token   text                     NOT NULL,
  scope           text                     NOT NULL,
  is_wearer       boolean                  NOT NULL,
  is_keyholder    boolean                  NOT NULL,
  created_at      timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at      timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  UNIQUE (chaster_name, user_discord_id)
);

CREATE TABLE task_subscriptions (
  user_id                uuid                     NOT NULL REFERENCES users (id),
  lock_id                text                     NOT NULL,
  most_recent_event_time timestamp WITH TIME ZONE NULL,
  created_at             timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at             timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  PRIMARY KEY (user_id, lock_id)
);

CREATE TABLE pending_tasks (
  id           uuid PRIMARY KEY                  DEFAULT gen_random_uuid(),
  task         text                     NOT NULL,
  user_id      uuid                     NOT NULL REFERENCES users (id),
  keyholder_id uuid                     NOT NULL REFERENCES users (id),
  deadline     timestamp WITH TIME ZONE NOT NULL,
  created_at   timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at   timestamp WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE TABLE lock_task_deadlines (
  lock_id                text                     NOT NULL UNIQUE,
  keyholder_id           uuid                     NOT NULL REFERENCES users (id),
  user_id                uuid                     NOT NULL REFERENCES users (id),
  deadline               bigint                   NOT NULL,
  most_recent_event_time timestamp WITH TIME ZONE NULL,
  created_at             timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at             timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  PRIMARY KEY (lock_id, keyholder_id)
);

CREATE TABLE user_roles (
  guild_discord_id bigint                   NOT NULL,
  role_discord_id  bigint                   NOT NULL,
  user_type        text                     NOT NULL,
  created_at       timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at       timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  PRIMARY KEY (guild_discord_id, role_discord_id, user_type)
);

CREATE TABLE pillory_links (
  user_id          uuid                     NOT NULL REFERENCES users (id),
  guild_discord_id bigint                   NOT NULL,
  post_id          text                     NOT NULL,
  counted          boolean                  NOT NULL DEFAULT FALSE,
  created_at       timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at       timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  PRIMARY KEY (user_id, guild_discord_id, post_id)
);

CREATE TABLE pillory_bitches (
  guild_discord_id   bigint                   NOT NULL PRIMARY KEY,
  channel_discord_id bigint                   NOT NULL,
  created_at         timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at         timestamp WITH TIME ZONE NOT NULL DEFAULT NOW()
);