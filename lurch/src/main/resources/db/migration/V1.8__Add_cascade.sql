ALTER TABLE pillory_links
  DROP CONSTRAINT pillory_links_user_id_fkey,
  ADD CONSTRAINT pillory_links_user_id_fkey
    FOREIGN KEY (user_id)
      REFERENCES users (id)
      ON DELETE CASCADE;

ALTER TABLE users
  DROP CONSTRAINT token_id_fkey,
  ADD CONSTRAINT token_id_fkey
    FOREIGN KEY (token_id)
      REFERENCES user_tokens (id)
      ON DELETE CASCADE;

