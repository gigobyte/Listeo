CREATE TABLE users (
  id bigserial PRIMARY KEY,
  username text NOT NULL UNIQUE,
  email text NOT NULL UNIQUE,
  "password" text NOT NULL,
  created_on timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE user_follows (
  PRIMARY KEY (user_id, follower_id),
  user_id bigserial REFERENCES users (id),
  follower_id bigserial REFERENCES users (id),
  followed_on timestamptz NOT NULL DEFAULT now()
);

CREATE TYPE playlist_style AS enum (
  'unordered',
  'ranked'
);

CREATE TYPE playlist_privacy AS enum (
  'public',
  'private'
);

CREATE TABLE playlists (
  id bigserial PRIMARY KEY,
  author_id bigserial REFERENCES users (id),
  "name" text NOT NULL,
  "description" text NOT NULL,
  style playlist_style NOT NULL,
  privacy playlist_privacy NOT NULL,
  created_on timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE playlist_tags (
  id bigserial PRIMARY KEY,
  "name" text NOT NULL UNIQUE
);

CREATE TABLE playlists_playlist_tags (
  PRIMARY KEY (playlist_id, playlist_tag_id),
  playlist_id bigserial REFERENCES playlists (id) ON DELETE CASCADE,
  playlist_tag_id bigserial REFERENCES playlist_tags (id) ON DELETE CASCADE
);

CREATE TABLE playlist_likes (
  PRIMARY KEY (playlist_id, user_id),
  playlist_id bigserial REFERENCES playlists (id) ON DELETE CASCADE,
  user_id bigserial REFERENCES users (id),
  liked_on timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE playlist_comments (
  id bigserial PRIMARY KEY,
  content text NOT NULL,
  playlist_id bigserial REFERENCES playlists (id) ON DELETE CASCADE,
  user_id bigserial REFERENCES users (id),
  created_on timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE playlist_subscriptions (
  PRIMARY KEY (playlist_id, user_id),
  playlist_id bigserial REFERENCES playlists (id) ON DELETE CASCADE,
  user_id bigserial REFERENCES users (id),
  subscribed_on timestamptz NOT NULL DEFAULT now()
);

CREATE TYPE video_source AS enum (
  'youtube',
  'vimeo'
);

CREATE TABLE videos (
  id bigserial PRIMARY KEY,
  "name" text NOT NULL,
  source video_source NOT NULL,
  link text NOT NULL,
  playlist_id bigserial REFERENCES playlists (id) ON DELETE CASCADE,
  note text NOT NULL,
  created_on timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE video_tags (
  id bigserial PRIMARY KEY,
  "name" text NOT NULL,
  video_id bigserial REFERENCES videos (id) ON DELETE CASCADE
);

