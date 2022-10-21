CREATE TABLE pending_tasks (
  id                 uuid PRIMARY KEY                  DEFAULT gen_random_uuid(),
  title              text                     NOT NULL,
  message_discord_id bigint UNIQUE            NOT NULL,
  user_id            uuid                     NOT NULL REFERENCES users (id),
  keyholder_id       uuid                     NOT NULL REFERENCES users (id),
  completed          boolean                  NOT NULL DEFAULT FALSE,
  deadline           timestamp WITH TIME ZONE NULL,
  created_at         timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at         timestamp WITH TIME ZONE NOT NULL DEFAULT NOW()
);