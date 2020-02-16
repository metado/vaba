CREATE TABLE IF NOT EXISTS inbox                (id INTEGER, pubDate TEXT, type TEXT);
CREATE TABLE IF NOT EXISTS friendship_requests  (id INTEGER, requester TEXT, addressee TEXT, requestTime TEXT);
CREATE TABLE IF NOT EXISTS text_messages        (id INTEGER, body TEXT, messageTime TEXT);
CREATE TABLE IF NOT EXISTS actors               (id         INTEGER,
                                                 type       TEXT,
                                                 name       TEXT,
                                                 address    TEXT,
                                                 inbox      TEXT,
                                                 outbox     TEXT,
                                                 following  TEXT,
                                                 followers  TEXT,
                                                 streams    TEXT);

INSERT INTO actors (id, type, name, address, inbox, outbox, following, followers, streams) VALUES (1, 'Person', 'bob', 'localhost:8081', 'http://no', 'http://no', 'http://no', 'http://no', 'http://no');
