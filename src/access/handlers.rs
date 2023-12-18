use askama::Template;
use axum::response::Response;

use crate::common::templates::render_template;

#[derive(Template, Default)]
#[template(path = "access/index.html")]
struct IndexTemplate<'a> {
    lang: &'a str,
    title: &'a str,
    description: &'a str,
    author: &'a str,
    keywords: &'a str,
}

pub async fn find_access_index() -> Response {
    let template = IndexTemplate {
        lang: "en",
        title: "Access List",
        description: "Your special access content",
        author: "",
        keywords: "",
    };

    render_template(template)
}
