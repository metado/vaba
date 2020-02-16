# Vaba

## Endpoints

### Client/UI-facing endpointsfe

- `GET /api/feed` - returns all posts from your node and friend-nodes, e.g. `[{"author": "alice@alice.me", "body": "Hello, world"}]`
- `POST /api/feed` - creates a new post

### Node/backend-facing endpoints

- `GET /api/posts` - returns all posts from your node, e.g. `[{"author": "alice@alice.me", "body": "Hello, world"}]`

## Front-end

- To build client-side run `sh build-fe.sh`

Twitter-killer.
