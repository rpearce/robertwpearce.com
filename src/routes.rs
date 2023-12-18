use axum::{routing::get, Json, Router};
use robertwpearce_com::{access, misc, posts};
use serde_json::json;

pub fn build_routes() -> Router {
    let routes = Router::new()
        .route("/", get(misc::handlers::home))
        .nest(
            "/posts",
            Router::new()
                .route("/", get(posts::handlers::find_posts))
                .route("/:id", get(posts::handlers::find_post_by_id)),
        )
        .nest(
            "/access",
            Router::new().route("/", get(access::handlers::find_access_index)),
        );

    Router::new()
        .route("/health", get(|| async { Json(json!({ "status": "ok" })) }))
        .merge(routes)
}
