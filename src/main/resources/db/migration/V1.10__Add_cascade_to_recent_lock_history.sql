ALTER TABLE recent_lock_history
  DROP CONSTRAINT recent_lock_history_user_id_fkey,
  ADD CONSTRAINT recent_lock_history_user_id_fkey
    FOREIGN KEY (user_id)
      REFERENCES users (id)
      ON DELETE CASCADE;