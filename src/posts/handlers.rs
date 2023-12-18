use askama::Template;
use axum::{
    extract::Path,
    http::StatusCode,
    response::{IntoResponse, Response},
};

use crate::common::templates::render_template;

#[derive(Template, Default)]
#[template(path = "posts/index.html")]
struct IndexTemplate<'a> {
    lang: &'a str,
    title: &'a str,
    description: &'a str,
    author: &'a str,
    keywords: &'a str,
}

pub async fn find_posts() -> Response {
    let template = IndexTemplate {
        lang: "en",
        title: "Posts",
        description: "Heaps of thoughts",
        author: "",
        keywords: "",
    };

    render_template(template)
}

#[derive(Template, Default)]
#[template(path = "posts/post.html")]
struct PostTemplate<'a> {
    lang: &'a str,
    title: &'a str,
    description: &'a str,
    author: &'a str,
    keywords: &'a str,
    published_at: &'a str,
    updated_at: &'a str,
    post_content: &'a str,
}

pub async fn find_post_by_id(Path(id): Path<String>) -> Response {
    let mut template = PostTemplate::default();

    template.title = "Testing 123";

    if &template.title == &"" {
        return (StatusCode::NOT_FOUND, "Post not found").into_response();
    }

    render_template(template)
}
