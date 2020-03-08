# Vaba

## Endpoints

### Client/UI-facing endpoints

- `GET /users/:name` - an actor endpoint; every instance right now can host only one user (actor) and it is specified in config
  e.g. for `bob@vaba.org`, an actor endpoint would be `https://vaba.org/users/bob`
- `POST /api/feed` - creates a new post

### Node/backend-facing endpoints

- `GET /api/posts` - returns all posts from your node, e.g. `[{"author": "alice@alice.me", "body": "Hello, world"}]`

## Front-end

- To build client-side run `sh build-fe.sh`

Twitter-killer.
