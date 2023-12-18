use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Default)]
pub struct Post {
    pub content: String,
    pub meta: PostMeta,
}

#[derive(Clone, Debug, Default, Deserialize)]
pub struct PostMeta {
    pub title: String,
    pub description: String,
    pub author: String,
    pub keywords: String,
    pub published_at: String,
    pub updated_at: String,
}
