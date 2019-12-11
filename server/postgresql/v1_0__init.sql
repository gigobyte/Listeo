CREATE TABLE users (
  id bigserial PRIMARY KEY,
  username text NOT NULL UNIQUE,
  pass text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now()
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
  playlist_id bigserial PRIMARY KEY,
  title text NOT NULL,
  style playlist_style NOT NULL,
  privacy playlist_privacy NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE playlist_tags (
  tag_id bigserial PRIMARY KEY,
  tag text NOT NULL
);

CREATE TABLE playlists_playlist_tags (
  PRIMARY KEY (playlist_id, playlist_tag_id),
  playlist_id bigserial REFERENCES playlists (playlist_id) ON DELETE CASCADE,
  playlist_tag_id bigserial REFERENCES playlist_tags (tag_id) ON DELETE CASCADE
);

