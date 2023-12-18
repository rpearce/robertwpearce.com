use std::net::SocketAddr;

use tracing::info;

use crate::{memory_db, routes};

pub async fn create_server() {
    let router = routes::build_routes();
    let memory_db = memory_db::build_memory_db();
    let addr = SocketAddr::from(([127, 0, 0, 1], 8000));

    info!("Listening on {addr}");

    axum::Server::bind(&addr)
        .serve(router.into_make_service())
        .await
        .unwrap();
}
