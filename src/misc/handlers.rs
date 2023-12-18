use askama::Template;
use axum::response::Response;

use crate::common::templates::render_template;

#[derive(Template, Default)]
#[template(path = "index.html")]
struct IndexTemplate<'a> {
    lang: &'a str,
    title: &'a str,
    description: &'a str,
    author: &'a str,
    keywords: &'a str,
}

pub async fn home() -> Response {
    let template = IndexTemplate {
        lang: "en",
        title: "Robert Pearce",
        description: "Software engineer with over 12 years of experience",
        author: "",
        keywords: "",
    };

    render_template(template)
}
