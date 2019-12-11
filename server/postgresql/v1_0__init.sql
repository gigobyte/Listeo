CREATE TABLE users (
  id bigserial PRIMARY KEY,
  username text NOT NULL UNIQUE,
  pass text NOT NULL,
  created_on timestamptz NOT NULL DEFAULT now()
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
  author_id bigserial REFERENCES users (id) ON DELETE CASCADE,
  p_name text NOT NULL,
  p_description text,
  style playlist_style NOT NULL,
  privacy playlist_privacy NOT NULL,
  created_on timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE playlist_tags (
  id bigserial PRIMARY KEY,
  t_name text NOT NULL
);

CREATE TABLE playlists_playlist_tags (
  playlist_id bigserial REFERENCES playlists (id) ON DELETE CASCADE,
  playlist_tag_id bigserial REFERENCES playlist_tags (id) ON DELETE CASCADE
);

