use robertwpearce_com::posts::models::{Post, PostMeta};

use std::{
    fs, io,
    path::{Path, PathBuf},
    process::exit,
};

pub struct MemoryDb {
    posts: Vec<Post>,
}

pub fn build_memory_db() -> MemoryDb {
    // TODO: error handling
    let posts = build_posts_db().unwrap();

    MemoryDb { posts: posts }
}

fn build_posts_db() -> Result<Vec<Post>, io::Error> {
    let path = Path::new("./posts");
    let mut posts: Vec<Post> = vec![];

    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();

        if !path.is_file() || path.extension().unwrap() != "md" {
            continue;
        }

        // =====================================================================
        // TODO: pull md parsing to own fn

        let file_content = fs::read_to_string(path)?;
        let split_frontmatter_content: Vec<&str> = file_content.split("---").collect();

        // TODO: handle error better `?`
        let post_meta: PostMeta = match serde_yaml::from_str(&split_frontmatter_content[1]) {
            Ok(p) => p,
            Err(err) => {
                println!(
                    "memory_db: failed to parse post frontmatter for file {:#?}: {:#?}",
                    entry.path(),
                    err
                );
                exit(1)
            }
        };

        let post = Post {
            content: split_frontmatter_content[2].to_string(),
            meta: post_meta,
        };

        // =====================================================================

        posts.push(post);
    }

    Ok(posts)
}
