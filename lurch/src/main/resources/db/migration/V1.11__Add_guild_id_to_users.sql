ALTER TABLE users
  DROP CONSTRAINT users_user_discord_id_key,
  ADD guild_discord_id bigint NOT NULL DEFAULT 717739783501250666,
  ADD CONSTRAINT users_user_discord_id_guild_id_uniqueness UNIQUE (user_discord_id, guild_discord_id);
