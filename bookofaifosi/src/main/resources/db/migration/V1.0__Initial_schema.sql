CREATE TABLE user_tokens (
  id            uuid                     DEFAULT gen_random_uuid() NOT NULL PRIMARY KEY,
  expires_at    timestamp WITH TIME ZONE,
  access_token  text UNIQUE,
  refresh_token text,
  scope         text,
  created_at    timestamp WITH TIME ZONE DEFAULT NOW()             NOT NULL,
  updated_at    timestamp WITH TIME ZONE DEFAULT NOW()             NOT NULL
);

CREATE TABLE users (
  id               uuid                     DEFAULT gen_random_uuid() NOT NULL PRIMARY KEY,
  user_discord_id  bigint                                             NOT NULL,
  token_id         uuid                                               NOT NULL REFERENCES user_tokens ON DELETE CASCADE,
  keyholder_ids    text[]                   DEFAULT '{}'::text[]      NOT NULL,
  is_locked        boolean                  DEFAULT FALSE             NOT NULL,
  last_locked      timestamp WITH TIME ZONE,
  last_keyheld     timestamp WITH TIME ZONE,
  chaster_id       text                                               NOT NULL,
  guild_discord_id bigint                                             NOT NULL,
  created_at       timestamp WITH TIME ZONE DEFAULT NOW()             NOT NULL,
  updated_at       timestamp WITH TIME ZONE DEFAULT NOW()             NOT NULL,
  UNIQUE (user_discord_id, guild_discord_id)
);

CREATE TABLE recent_lock_history (
  user_id                uuid                                   NOT NULL REFERENCES users ON DELETE CASCADE,
  lock_id                text                                   NOT NULL,
  most_recent_event_time timestamp WITH TIME ZONE,
  created_at             timestamp WITH TIME ZONE DEFAULT NOW() NOT NULL,
  updated_at             timestamp WITH TIME ZONE DEFAULT NOW() NOT NULL,
  PRIMARY KEY (user_id, lock_id)
);
