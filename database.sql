CREATE TABLE IF NOT EXISTS inbox                (id INTEGER, pubDate TEXT, type TEXT);
CREATE TABLE IF NOT EXISTS friendship_requests  (id INTEGER, requester TEXT, addressee TEXT, requestTime TEXT);
CREATE TABLE IF NOT EXISTS text_messages        (id INTEGER, body TEXT, messageTime TEXT);
CREATE TABLE IF NOT EXISTS actors               (id INTEGER, name TEXT, address TEXT);
