ALTER TABLE users
  ADD last_locked timestamp WITH TIME ZONE NULL DEFAULT NULL,
  ADD last_keyheld timestamp WITH TIME ZONE NULL DEFAULT NULL;