CREATE TABLE recent_lock_history (
  user_id                uuid                     NOT NULL REFERENCES users (id),
  lock_id                text                     NOT NULL,
  most_recent_event_time timestamp WITH TIME ZONE NULL,
  created_at             timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at             timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  PRIMARY KEY (user_id, lock_id)
);