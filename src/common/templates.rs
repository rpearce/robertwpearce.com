use askama::Template;
use axum::{
    http::StatusCode,
    response::{Html, IntoResponse, Response},
};
use tracing::trace;

pub fn render_template(template: impl Template) -> Response {
    match template.render() {
        Ok(rendered) => Html(rendered).into_response(),
        Err(e) => {
            trace!("Failed to render template: {e:?}");

            StatusCode::INTERNAL_SERVER_ERROR.into_response()
        }
    }
}
