CREATE TABLE locked_channels (
  guild_discord_id   bigint                   NOT NULL,
  channel_discord_id bigint                   NOT NULL,
  created_at         timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at         timestamp WITH TIME ZONE NOT NULL DEFAULT NOW(),
  PRIMARY KEY (guild_discord_id, channel_discord_id)
);