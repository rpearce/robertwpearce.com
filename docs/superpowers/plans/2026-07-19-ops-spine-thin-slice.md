# Ops Spine Thin Slice — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stand up the full deploy pipeline — a minimal Rust/Axum service reachable at `https://lab.robertwpearce.com`, provisioned from code and deployed by a pipeline you can read end-to-end — before adding any auth or content logic.

**Architecture:** OpenTofu declares a single Hetzner Cloud VM and its firewall; cloud-init lays down a hardened base OS; the app runs as a bare musl binary under a hand-written **systemd** unit bound to `127.0.0.1:8000`; **Caddy** reverse-proxies `443 → 8000` using an explicit **Cloudflare Origin CA** certificate; Cloudflare fronts it for DNS/TLS/CDN. Deploys are a plain **build → scp → `systemctl restart`** loop you first run by hand, then wrap in a GitHub Actions workflow. No Docker, no Kamal — every moving part is a file you wrote.

**Tech Stack:** Rust + axum 0.8, OpenTofu + `hetznercloud/hcloud` + `cloudflare/cloudflare` providers, cloud-init, systemd, Caddy, Cloudflare (proxy + Origin CA), GitHub Actions (plain `ssh`/`scp`).

## Global Constraints

- **Minimal-abstraction rule:** prefer primitives you author and can read (systemd unit, Caddyfile, SSH deploy) over tools that hide server mechanics. **Runtime = bare binary under systemd (Option A).** Docker is used *only as a reproducible build environment*, not to run the app. No container-runtime and no Kamal in this slice — those are the planned **Option B** phase 2, and the `Dockerfile` written here is the same one Option B will run.
- **Do not touch the live apex site.** `robertwpearce.com` / `www` stay on GitHub Pages untouched. All work here is on the **new subdomain `lab.robertwpearce.com`**.
- **Secrets never committed.** Hetzner + Cloudflare API tokens via env vars (`HCLOUD_TOKEN`, `CLOUDFLARE_API_TOKEN`); the Origin cert private key and the deploy SSH key live only on the box / in GitHub Secrets. `.gitignore` covers `*.tfstate*`, `.terraform/`, `*.tfvars`, `*.pem`, `*.key`.
- **Pin versions:** axum `0.8.9`, tokio `1.x` (`features=["macros","rt-multi-thread"]`), Rust edition `2021`. OpenTofu `>= 1.7`. hcloud provider `~> 1.48`. cloudflare provider `~> 4.0` (v5 renames `value`→`content` and some resources — if you install v5, adjust the DNS resource accordingly).
- **Box:** Hetzner `cpx21` (AMD, 3 vCPU / 4 GB), image `ubuntu-24.04`, in a **US location** (Ashburn `ash` / Hillsboro `hil`) for eventual NZ-audience latency. **Corrected in review:** the Intel **CX line (incl. `cx22`) is EU-only**, and ARM **CAX is EU-only too** — US locations offer only **CPX (shared AMD)** and **CCX (dedicated)**. So the US box is `cpx21` (nearest 4 GB option; `cpx11` = 2 GB is cheapest). Re-verify current pricing in-console (US CPX rose in Hetzner's June 2026 adjustment; **IPv4 is billed separately, ~€0.50/mo**) and availability with `hcloud server-type list`. Docker handles the Linux/musl build regardless of arch, so the choice is driven purely by location. Build target: `x86_64-unknown-linux-musl` (static, amd64).
- **Work directly on `rewrite-rust`** (same repo). This is a **full replacement of Hakyll**: one Rust/Axum app will serve all current static content *and* the gated content, and `rewrite-rust` becomes the new `main` at cutover. No separate branch or repo. `main` keeps building the Hakyll site and stays live on GitHub Pages **untouched until the deliberate apex cutover**; the in-progress Rust app is staged on `lab.robertwpearce.com` (Hetzner). This slice reduces the crate to a minimal compiling entrypoint: the half-built axum-0.6 modules (`lib.rs`, `server.rs`, `routes.rs`, `memory_db.rs`, `access/`, `common/`, `misc/`, `posts/`) are removed now and rebuilt, modernized to axum 0.8, in the app plan (they remain in git history).
- **URL preservation:** no cutover may 404 an existing URL — reproduce the current scheme or 301 to the new location (see **URL Preservation** below). This slice adds only the `lab.` subdomain and leaves every existing URL untouched.

---

## Design Decisions & Alternatives Considered

These are the "how much tooling" calls, made explicit so you can veto any of them before execution:

1. **Reverse proxy (Caddy) — KEPT.** A reverse proxy is a *fundamental* server concept, not a magic abstraction: it decouples TLS and process lifecycle from the app, and its config here is a 4-line file you read in full. *Alternative considered:* no proxy — have the Rust app terminate TLS itself with `axum-server` + rustls + the Origin cert on `:443`. Fewer parts, but couples cert handling and restarts to the app and skips a core concept worth learning. If you'd rather go proxy-less, say so and Task 5 changes.
2. **Explicit Origin CA cert, not Caddy auto-HTTPS.** Behind Cloudflare's proxy, ACME on the origin is finicky and, more importantly, *hides* the Cloudflare↔origin trust relationship. Installing a Cloudflare Origin cert by hand makes that trust legible and needs no ACME. *Alternative:* Caddy auto-Let's-Encrypt (turned off here on purpose).
3. **Docker for builds; bare binary + systemd for runtime (Option A).** You know Docker, so using it as a *build* environment hides nothing — it gives reproducible builds and kills the macOS→Linux cross-compile friction. The app still deploys as a plain binary supervised by systemd, so you learn process supervision, journald, service users, and sandboxing — the actual "running a server" skills. **Option B** (run the container image on the box, then adopt Kamal) is the deliberate *next* phase once those primitives are understood; the `Dockerfile` here already has a runnable final stage so B reuses it.
4. **Manual first deploy, then CI.** You run the build→ship→restart loop by hand once (Task 4) so you understand each step, then automate the identical steps in Actions (Task 6). Understand the primitive before wrapping it.
5. **DNS in Terraform, SSL/cert setup by hand.** The DNS record is codified (you like IaC and it's clean); the one-time Origin-cert trust setup is done in the dashboard so you see what each SSL knob does. Local Terraform state for now (you learn what state *is*); remote state is a later concern.

---

## URL Preservation (cross-cutting requirement)

**Requirement:** the migration must never break an existing URL. Every path the live Hakyll site serves today must, after any cutover, return the same content at the same URL (route parity) or a **301** to its new location — **never a 404**. This covers post pages, the home page, `/new-zealand/*`, the feeds (`/atom.xml`, `/rss.xml`), `/sitemap.xml`, `/robots.txt`, `/.well-known/*`, and asset URLs.

**This slice is URL-non-disruptive:** it only adds the *new* `lab.robertwpearce.com` DNS record. It does not touch the apex/`www` DNS or routing, so every current URL keeps resolving to GitHub Pages exactly as today. There is nothing to forward *in this slice* — this section is the contract the later app/cutover plan must honor.

**Capture the source of truth now** (cheap; do it before any routing change so the canonical list can't drift):
```bash
mkdir -p docs
curl -s https://robertwpearce.com/sitemap.xml \
  | grep -oE '<loc>[^<]+' | sed 's/<loc>//' \
  | sort > docs/url-inventory.txt
wc -l docs/url-inventory.txt
git add docs/url-inventory.txt && git commit -m "docs: capture current live URL inventory"
```
The sitemap misses feeds, `/robots.txt`, `/.well-known/*`, and any redirected paths — so **also crawl the deployed `gh-pages` output and merge** (mandatory, not optional). Keep this file updated as the authoritative list.

**Approach for the app / cutover plan (NOT this slice):**
- **Parity first.** Reproduce the Hakyll URL scheme in the Rust app so most URLs need no redirect. Read `old/ssg/src/Hakyll/Site/Rules.hs` and `old/ssg/src/Hakyll/Site/Post.hs` for the exact output-path rules (trailing-slash vs `.html`, date-stripped slugs, feed/sitemap paths) before choosing routes.
- **301 where it differs.** Prefer redirects at the **Cloudflare edge** (Redirect Rules / Bulk Redirects) so redirect logic stays out of the app and runs before the origin; the axum app can also carry a fallback redirect map for anything edge rules miss.
- **Verify before cutover.** A link-check that, for every URL in `docs/url-inventory.txt`, **follows redirect chains (bounded hops) to a final 200** — treating a literal 404, any 5xx, *and* soft-404s (200 with placeholder/wrong content) as failures. Block the cutover on any regression.

---

## Prerequisites (one-time, before Task 1)

Do these once; they are accounts/installs, not repo changes:

- **Hetzner Cloud** account + a **project**; create a read/write **API token** (Project → Security → API Tokens). Export it: `export HCLOUD_TOKEN=...`
- **Cloudflare** account with `robertwpearce.com` already on it (it is — the live site is fronted by Cloudflare). Create a scoped **API token** (My Profile → API Tokens → *Edit zone DNS* on this zone). Export it: `export CLOUDFLARE_API_TOKEN=...`
- **OpenTofu** installed: `brew install opentofu` (verify `tofu version` ≥ 1.7).
- **An SSH keypair for this box** (separate from your personal key is cleaner): `ssh-keygen -t ed25519 -f ~/.ssh/rwp_lab -C rwp-lab`. You'll reference `~/.ssh/rwp_lab.pub`.
- Rust toolchain present (`cargo --version`) — used for local `cargo run` during dev (Task 1). Release builds go through Docker, not local `cargo`.
- **Docker Desktop** installed and running (`docker version`) — the reproducible build environment. On Apple Silicon, amd64 release builds run under emulation locally (fine for the occasional manual build) and natively in CI.
- Note your current public IP for the SSH firewall rule: `curl -s ifconfig.me` (you'll paste it into a tfvars variable).

---

## File Structure

```
robertwpearce.com/
├─ Cargo.toml                      # MODIFY: axum 0.8 deps for the thin-slice app
├─ src/main.rs                     # MODIFY: minimal axum 0.8 /health + / service
├─ Dockerfile                      # CREATE: multi-stage build (musl static) + runnable final stage
├─ .dockerignore                   # CREATE: keep the build context small
├─ .gitignore                      # MODIFY: ignore tf state, tfvars, pem/key
├─ infra/                          # CREATE: all OpenTofu
│  ├─ versions.tf                  #   required providers + versions
│  ├─ variables.tf                 #   inputs (token via env, ssh key path, admin_ip, hostname)
│  ├─ main.tf                      #   hcloud ssh key + firewall + server (cloud-init)
│  ├─ cloudflare.tf                #   DNS A record lab.robertwpearce.com (proxied)
│  ├─ outputs.tf                   #   server IPv4
│  ├─ cloud-init.yaml              #   base OS: deploy user, ssh hardening, ufw, caddy
│  └─ terraform.tfvars.example     #   template (real terraform.tfvars is gitignored)
├─ deploy/                         # CREATE: the readable deploy primitives
│  ├─ site.service                 #   systemd unit
│  ├─ Caddyfile                    #   reverse proxy + explicit origin TLS
│  ├─ deploy.sh                    #   build → scp → restart (the manual loop)
│  └─ README.md                    #   what each piece does + manual runbook
└─ .github/workflows/
   └─ deploy-lab.yml               # CREATE: CI wrapper around deploy.sh's steps
```

---

## Task 1: Minimal Axum 0.8 service (the thing we deploy)

**Files:**
- Modify: `Cargo.toml`
- Modify: `src/main.rs`
- Modify: `.gitignore`

**Interfaces:**
- Produces: a binary named `robertwpearce_com` that serves `GET /health` → `200 {"status":"ok"}` and `GET /` → `200` text, bound to `127.0.0.1:8000`.

- [ ] **Step 1: Switch to the `rewrite-rust` branch**

This is the full-rewrite branch (it becomes `main` at cutover); work directly on it. `main`/Pages stay untouched.
```bash
git switch rewrite-rust
```

- [ ] **Step 2: Set minimal dependencies**

Replace `Cargo.toml`'s `[dependencies]` block with exactly:
```toml
[dependencies]
axum = "0.8.9"
tokio = { version = "1", features = ["macros", "rt-multi-thread", "net"] }
serde_json = "1"

[profile.release]
strip = true
```
(`net` is explicit because `main.rs` names `tokio::net::TcpListener` — don't lean on axum's default features enabling it. `strip = true` strips at build time, so the Docker image needs no `strip`/binutils. The full app deps come back in a later plan; this slice is about the pipeline.)

- [ ] **Step 3: Write the minimal service**

Replace the entire contents of `src/main.rs` with:
```rust
use axum::{routing::get, Json, Router};
use serde_json::{json, Value};

async fn health() -> Json<Value> {
    Json(json!({ "status": "ok" }))
}

async fn home() -> &'static str {
    "ops spine: hello from lab.robertwpearce.com\n"
}

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/health", get(health))
        .route("/", get(home));

    // Default to loopback (Option A: bare binary behind Caddy on the host). Option B runs
    // this same image as a container and sets BIND_ADDR=0.0.0.0:8000 to be reachable.
    let addr = std::env::var("BIND_ADDR").unwrap_or_else(|_| "127.0.0.1:8000".into());
    let listener = tokio::net::TcpListener::bind(&addr).await.expect("bind");
    println!("listening on http://{}", listener.local_addr().expect("local_addr"));
    axum::serve(listener, app).await.expect("server error");
}
```

Now reduce the crate to just this entrypoint — remove the half-built modules (they stay safe in `rewrite-rust`'s history and return, modernized, in the next plan). This also drops the old axum-0.6 `lib.rs` so it isn't built:
```bash
git rm src/lib.rs src/server.rs src/routes.rs src/memory_db.rs
git rm -r src/common src/access src/misc src/posts
```
(Leave `templates/` — harmless HTML, reused later.)

- [ ] **Step 4: Ignore secrets and state**

Append to `.gitignore`:
```gitignore
# infra
infra/.terraform/
infra/*.tfstate
infra/*.tfstate.*
infra/*.tfvars
*.pem
*.key
```

- [ ] **Step 5: Run it and verify locally**

Run (in one terminal): `cargo run`
Then in another: `curl -s localhost:8000/health && echo && curl -s localhost:8000/`
Expected:
```
{"status":"ok"}
ops spine: hello from lab.robertwpearce.com
```
Stop the server (Ctrl-C).

- [ ] **Step 6: Commit**

```bash
git add -A src/ Cargo.toml Cargo.lock .gitignore
git commit -m "feat(ops): minimal axum 0.8 health service for deploy slice"
```

---

## Task 2: OpenTofu — providers, server, firewall

**Files:**
- Create: `infra/versions.tf`, `infra/variables.tf`, `infra/main.tf`, `infra/outputs.tf`, `infra/terraform.tfvars.example`

**Interfaces:**
- Consumes: env vars `HCLOUD_TOKEN`, `CLOUDFLARE_API_TOKEN`.
- Produces: a running `cx22` server whose IPv4 is exposed as the `server_ipv4` output; a firewall allowing SSH from your IP and 80/443 from Cloudflare. (cloud-init file is wired here but authored in Task 3.)

- [ ] **Step 1: Declare providers**

Create `infra/versions.tf`:
```hcl
terraform {
  required_version = ">= 1.7"
  required_providers {
    hcloud     = { source = "hetznercloud/hcloud", version = "~> 1.48" }
    cloudflare = { source = "cloudflare/cloudflare", version = "~> 4.0" }
  }
}

# Tokens are read from HCLOUD_TOKEN and CLOUDFLARE_API_TOKEN env vars.
provider "hcloud" {}
provider "cloudflare" {}
```

- [ ] **Step 2: Declare inputs**

Create `infra/variables.tf`:
```hcl
variable "admin_ip" {
  description = "Your current public IP (from `curl ifconfig.me`), for the SSH firewall rule."
  type        = string
}

variable "ssh_public_key_path" {
  description = "Path to the box's SSH public key."
  type        = string
  default     = "~/.ssh/rwp_lab.pub"
}

variable "server_type" {
  type    = string
  default = "cpx21" # AMD 3 vCPU / 4 GB. CX (Intel) and CAX (ARM) are EU-only; US = CPX/CCX. Verify: hcloud server-type list
}

variable "location" {
  type    = string
  default = "ash" # Ashburn, US-East. US-West is "hil" (Hillsboro) — verify with `hcloud location list`.
}

variable "hostname" {
  type    = string
  default = "lab.robertwpearce.com"
}

variable "cloudflare_zone" {
  type    = string
  default = "robertwpearce.com"
}

# Cloudflare's published proxy ranges (https://www.cloudflare.com/ips/).
# Refresh occasionally; only these should reach the origin on 80/443.
variable "cloudflare_ipv4" {
  type = list(string)
  default = [
    "173.245.48.0/20", "103.21.244.0/22", "103.22.200.0/22", "103.31.4.0/22",
    "141.101.64.0/18", "108.162.192.0/18", "190.93.240.0/20", "188.114.96.0/20",
    "197.234.240.0/22", "198.41.128.0/17", "162.158.0.0/15", "104.16.0.0/13",
    "104.24.0.0/14", "172.64.0.0/13", "131.0.72.0/22",
  ]
}
```

- [ ] **Step 3: Declare the SSH key, firewall, and server**

Create `infra/main.tf`:
```hcl
resource "hcloud_ssh_key" "lab" {
  name       = "rwp-lab"
  public_key = file(var.ssh_public_key_path)
}

resource "hcloud_firewall" "lab" {
  name = "rwp-lab-fw"

  # SSH only from you.
  rule {
    direction  = "in"
    protocol   = "tcp"
    port       = "22"
    source_ips = ["${var.admin_ip}/32"]
  }

  # HTTP/HTTPS only from Cloudflare.
  rule {
    direction  = "in"
    protocol   = "tcp"
    port       = "80"
    source_ips = var.cloudflare_ipv4
  }
  rule {
    direction  = "in"
    protocol   = "tcp"
    port       = "443"
    source_ips = var.cloudflare_ipv4
  }
}

resource "hcloud_server" "lab" {
  name         = "rwp-lab"
  image        = "ubuntu-24.04"
  server_type  = var.server_type
  location     = var.location
  ssh_keys     = [hcloud_ssh_key.lab.id]
  firewall_ids = [hcloud_firewall.lab.id]
  user_data    = file("${path.module}/cloud-init.yaml")

  public_net {
    ipv4_enabled = true
    ipv6_enabled = true
  }
}
```

- [ ] **Step 4: Declare the output**

Create `infra/outputs.tf`:
```hcl
output "server_ipv4" {
  value = hcloud_server.lab.ipv4_address
}
```

- [ ] **Step 5: Provide a tfvars template**

Create `infra/terraform.tfvars.example`:
```hcl
# Copy to terraform.tfvars (gitignored) and fill in:
admin_ip = "203.0.113.10"   # your `curl ifconfig.me`
# ssh_public_key_path = "~/.ssh/rwp_lab.pub"
# location = "ash"
```
Then run: `cp infra/terraform.tfvars.example infra/terraform.tfvars` and set `admin_ip` to your real IP.

- [ ] **Step 6: Init and verify the plan (server not created yet — cloud-init lands in Task 3)**

Run:
```bash
cd infra && tofu init && tofu validate
```
Expected: `Success! The configuration is valid.`
Do **not** `apply` yet — `cloud-init.yaml` doesn't exist. `tofu plan` will error on the missing file; that's expected until Task 3.

- [ ] **Step 7: Commit**

```bash
git add infra/versions.tf infra/variables.tf infra/main.tf infra/outputs.tf infra/terraform.tfvars.example infra/.terraform.lock.hcl
git commit -m "feat(ops): opentofu server + firewall (cloud-init pending)"
```

---

## Task 3: cloud-init base OS + apply

**Files:**
- Create: `infra/cloud-init.yaml`

**Interfaces:**
- Consumes: the box's SSH public key is injected by hcloud (root); cloud-init adds a `deploy` user with the same key.
- Produces: a hardened box with a non-root `deploy` user, SSH locked to keys, ufw active, Caddy installed, and `/opt/site/{bin,data}` owned by `deploy`.

- [ ] **Step 1: Author the base image**

Create `infra/cloud-init.yaml`. Paste your **public** key contents where shown (`cat ~/.ssh/rwp_lab.pub`):
```yaml
#cloud-config
package_update: true
package_upgrade: true

users:
  - name: deploy
    groups: [sudo]
    shell: /bin/bash
    sudo: ["ALL=(ALL) NOPASSWD:ALL"]
    ssh_authorized_keys:
      - ssh-ed25519 AAAA...PASTE_YOUR_rwp_lab.pub_HERE... rwp-lab

ssh_pwauth: false
disable_root: true

packages:
  - ufw
  - fail2ban
  - unattended-upgrades
  - debian-keyring
  - debian-archive-keyring
  - apt-transport-https
  - curl

write_files:
  - path: /etc/ssh/sshd_config.d/99-hardening.conf
    content: |
      PermitRootLogin no
      PasswordAuthentication no
      KbdInteractiveAuthentication no

runcmd:
  # Host firewall. NOTE: these open 22/80/443 to the whole internet (v4+v6). The real
  # Cloudflare/SSH source restriction lives in the Hetzner cloud firewall (main.tf) — that
  # is the actual boundary; ufw here is only a local baseline, NOT defense-in-depth.
  - ufw default deny incoming
  - ufw default allow outgoing
  - ufw allow 22/tcp
  - ufw allow 80/tcp
  - ufw allow 443/tcp
  - ufw --force enable
  # Install Caddy fail-LOUD: runcmd is best-effort per line, so a silent apt failure would
  # boot a Caddy-less box you'd only notice at Step 3. `set -e` makes provisioning fail visibly.
  - |
    set -euxo pipefail
    install -d -m 0755 /usr/share/keyrings
    curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' \
      | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
    curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' \
      > /etc/apt/sources.list.d/caddy-stable.list
    apt-get update
    apt-get install -y caddy
    caddy version
  # App layout
  - mkdir -p /opt/site/bin /opt/site/data
  - chown -R deploy:deploy /opt/site
  - systemctl enable --now unattended-upgrades
  - systemctl restart ssh
```

- [ ] **Step 2: Create the box**

Run:
```bash
cd infra && tofu plan && tofu apply
```
Expected: `tofu apply` completes; `Outputs: server_ipv4 = "<IP>"`. Note the IP.

- [ ] **Step 3: Wait for cloud-init to finish, then verify hardening**

Wait for cloud-init to actually complete (don't guess with a fixed sleep):
```bash
ssh -i ~/.ssh/rwp_lab deploy@<IP> 'cloud-init status --wait; sudo tail -n 30 /var/log/cloud-init-output.log'
```
Expected: `status: done`. If `status: error`, read the log — the most likely cause is a failed Caddy apt install (Caddy's Cloudsmith signing key has a history of expiring, breaking `apt-get update`; re-fetch the key or check caddy issue #7411). Then verify hardening:
```bash
ssh -i ~/.ssh/rwp_lab deploy@<IP> 'whoami && sudo ufw status verbose && caddy version && ls -ld /opt/site/bin /opt/site/data'
```
Expected: prints `deploy`; ufw `Status: active` with 22/80/443 allowed; a Caddy version string; both dirs owned by `deploy`.

Verify root login is refused:
```bash
ssh -i ~/.ssh/rwp_lab root@<IP> 'echo should-not-work'
```
Expected: permission denied / connection refused for root.

- [ ] **Step 4: Commit**

```bash
git add infra/cloud-init.yaml
git commit -m "feat(ops): cloud-init base os (deploy user, ssh hardening, ufw, caddy)"
```

---

## Task 4: systemd unit + first deploy BY HAND

This is the hands-on "understand how the process runs" task. You ship the binary and start the service manually before any automation.

**Files:**
- Create: `Dockerfile`, `.dockerignore`, `deploy/site.service`, `deploy/deploy.sh`, `deploy/README.md`

**Interfaces:**
- Consumes: the `robertwpearce_com` source from Task 1; `/opt/site` from Task 3.
- Produces: a running `site.service` answering on `127.0.0.1:8000` on the box, plus a `Dockerfile` (reused by Option B) and a `deploy.sh` that encapsulates build→extract→ship→restart.

- [ ] **Step 1: Write the systemd unit**

Create `deploy/site.service`:
```ini
[Unit]
Description=robertwpearce.com ops-spine service
After=network-online.target
Wants=network-online.target
# Crash-loop guard: give up after 5 failures in 60s instead of restarting forever
StartLimitIntervalSec=60
StartLimitBurst=5

[Service]
Type=exec
User=deploy
Group=deploy
WorkingDirectory=/opt/site
ExecStart=/opt/site/bin/site
Restart=on-failure
RestartSec=2

# Resource caps
MemoryMax=512M
TasksMax=256
LimitNOFILE=65535

# Sandboxing (run `systemd-analyze security site` to see the score)
NoNewPrivileges=true
ProtectSystem=strict
ProtectHome=true
PrivateTmp=true
ReadWritePaths=/opt/site/data
ProtectKernelTunables=true
ProtectKernelModules=true
ProtectControlGroups=true
RestrictAddressFamilies=AF_INET AF_INET6
RestrictNamespaces=true
LockPersonality=true
CapabilityBoundingSet=
SystemCallFilter=@system-service

[Install]
WantedBy=multi-user.target
```

- [ ] **Step 2: Write the Dockerfile and deploy script**

Create `Dockerfile` — a musl builder plus a runnable distroless final stage. Option A *extracts* the binary from the `export` stage; Option B later *runs* the `runtime` stage as-is:
```dockerfile
# syntax=docker/dockerfile:1

# Pin to your rustc (e.g. rust:1.XX-alpine) for reproducible builds.
FROM rust:1-alpine AS builder
RUN apk add --no-cache musl-dev
WORKDIR /app
COPY Cargo.toml Cargo.lock ./
COPY src ./src
RUN cargo build --release --locked

# Option A: `docker build --platform linux/amd64 --target export -o out .` → ./out/site
FROM scratch AS export
COPY --from=builder /app/target/release/robertwpearce_com /site

# Option B (later): `docker build --platform linux/amd64 --target runtime -t site .`
FROM gcr.io/distroless/static-debian12 AS runtime
COPY --from=builder /app/target/release/robertwpearce_com /site
EXPOSE 8000
ENTRYPOINT ["/site"]
```

Create `.dockerignore` so the build context stays tiny (only the crate is needed to compile):
```gitignore
out/
target/
old/
posts/
docs/
infra/
deploy/
.git/
.github/
*.md
```

Create `deploy/deploy.sh` (the whole deploy, readable top to bottom — build in Docker, extract, ship, restart):
```bash
#!/usr/bin/env bash
set -euo pipefail

# Usage: HOST=<ip-or-hostname> ./deploy/deploy.sh
: "${HOST:?set HOST to the server IP}"
SSH_KEY="${SSH_KEY:-$HOME/.ssh/rwp_lab}"

echo "==> building static amd64 binary in Docker"
docker build --platform linux/amd64 --target export -o out .
test -x out/site

echo "==> shipping binary to $HOST:/opt/site/bin/site.new"
scp -i "$SSH_KEY" out/site "deploy@$HOST:/opt/site/bin/site.new"

echo "==> installing + restarting"
ssh -i "$SSH_KEY" "deploy@$HOST" '
  set -e
  mv /opt/site/bin/site.new /opt/site/bin/site
  chmod +x /opt/site/bin/site
  sudo systemctl restart site
  sleep 1
  systemctl is-active site
'
echo "==> deployed."
```
Make it executable: `chmod +x deploy/deploy.sh`

- [ ] **Step 3: Install the systemd unit on the box (one time)**

The Docker build (Step 2) does the Linux/musl compile, so there is no macOS cross-linker to set up. Copy the unit up and enable it:
```bash
IP=<IP>
scp -i ~/.ssh/rwp_lab deploy/site.service deploy@$IP:/tmp/site.service
ssh -i ~/.ssh/rwp_lab deploy@$IP 'sudo mv /tmp/site.service /etc/systemd/system/site.service && sudo systemctl daemon-reload && sudo systemctl enable site'
```

- [ ] **Step 4: Run the deploy and verify the service**

Run:
```bash
HOST=<IP> ./deploy/deploy.sh
```
Expected last lines: `active` then `==> deployed.`

Verify on the box:
```bash
ssh -i ~/.ssh/rwp_lab deploy@<IP> 'systemctl status site --no-pager | head -n 5; curl -s localhost:8000/health; echo; journalctl -u site --no-pager | tail -n 3'
```
Expected: `active (running)`, `{"status":"ok"}`, and a `listening on http://127.0.0.1:8000` log line.

- [ ] **Step 5: Write the runbook and commit**

Create `deploy/README.md` documenting: what `site.service` does (systemd supervises the bare binary as user `deploy`, restarts on failure, sandboxed to `/opt/site/data`), what `deploy.sh` does (Docker build → extract `out/site` → scp → atomic swap → `systemctl restart`), how to read logs (`journalctl -u site -f`), and the note that the same `Dockerfile` `runtime` stage becomes the Option B container image later.

```bash
git add Dockerfile .dockerignore deploy/site.service deploy/deploy.sh deploy/README.md
git commit -m "feat(ops): dockerized build + systemd unit + manual deploy script"
```

---

## Task 5: Caddy reverse proxy + Cloudflare Origin TLS + DNS

**Files:**
- Create: `deploy/Caddyfile`, `infra/cloudflare.tf`

**Interfaces:**
- Consumes: the running app on `127.0.0.1:8000`; a Cloudflare Origin CA cert/key (generated in dashboard, installed on box).
- Produces: `https://lab.robertwpearce.com/health` returning `200` **through Cloudflare** (response carries a `cf-ray` header).

- [ ] **Step 1: Add the DNS record in Terraform**

Create `infra/cloudflare.tf`:
```hcl
data "cloudflare_zone" "main" {
  name = var.cloudflare_zone
}

resource "cloudflare_record" "lab" {
  zone_id = data.cloudflare_zone.main.id
  name    = "lab"
  value   = hcloud_server.lab.ipv4_address # v5 providers: rename `value` -> `content`
  type    = "A"
  proxied = true # orange cloud
  ttl     = 1    # automatic (required when proxied)
}
```
Apply: `cd infra && tofu apply`
Verify: `dig +short lab.robertwpearce.com` returns a **Cloudflare** IP (not your origin IP — proof the proxy is on).

- [ ] **Step 2: Generate + install the Cloudflare Origin CA cert (by hand)**

In the Cloudflare dashboard: **SSL/TLS → Origin Server → Create Certificate** (accept defaults, hostnames `lab.robertwpearce.com` + `robertwpearce.com`). Copy the **certificate** and **private key**. On your machine, save them to files, then install on the box with tight perms (they are secrets — never commit):
```bash
IP=<IP>
# paste cert into origin.pem and key into origin.key locally, then:
scp -i ~/.ssh/rwp_lab origin.pem origin.key deploy@$IP:/tmp/
ssh -i ~/.ssh/rwp_lab deploy@$IP '
  sudo install -o caddy -g caddy -m 600 /tmp/origin.pem /etc/caddy/origin.pem
  sudo install -o caddy -g caddy -m 600 /tmp/origin.key /etc/caddy/origin.key
  rm /tmp/origin.pem /tmp/origin.key
'
rm origin.pem origin.key
```
Also set the zone SSL mode to **Full (strict)** (SSL/TLS → Overview).

- [ ] **Step 3: Write the Caddyfile**

Create `deploy/Caddyfile`:
```caddyfile
lab.robertwpearce.com {
	tls /etc/caddy/origin.pem /etc/caddy/origin.key
	reverse_proxy 127.0.0.1:8000
}
```
Ship it and reload Caddy:
```bash
scp -i ~/.ssh/rwp_lab deploy/Caddyfile deploy@<IP>:/tmp/Caddyfile
ssh -i ~/.ssh/rwp_lab deploy@<IP> 'sudo mv /tmp/Caddyfile /etc/caddy/Caddyfile && sudo systemctl reload caddy && systemctl is-active caddy'
```
Expected: `active`.

- [ ] **Step 4: Verify end-to-end through Cloudflare**

Run (from your laptop, may take a minute for DNS to propagate):
```bash
curl -sI https://lab.robertwpearce.com/health
curl -s  https://lab.robertwpearce.com/health && echo
```
Expected: `HTTP/2 200`, a `cf-ray:` header (proves traffic went through Cloudflare), and body `{"status":"ok"}`.

- [ ] **Step 5: Commit**

```bash
git add deploy/Caddyfile infra/cloudflare.tf
git commit -m "feat(ops): caddy reverse proxy + cloudflare origin tls + dns"
```

---

## Task 6: Automate the deploy with GitHub Actions

Wrap the *exact same* build→ship→restart steps you ran by hand into CI, using plain `ssh`/`scp` (no third-party deploy actions, to keep it legible).

**Files:**
- Create: `.github/workflows/deploy-lab.yml`

**Interfaces:**
- Consumes: GitHub Secrets `LAB_SSH_KEY` (private key contents of `~/.ssh/rwp_lab`) and `LAB_HOST` (the server IP).
- Produces: a push to `ops-spine` that touches app/deploy code auto-deploys and the live `/` response reflects the change.

- [ ] **Step 1: Add the GitHub Secrets**

In the repo settings → Secrets and variables → Actions, add:
- `LAB_SSH_KEY` = contents of `~/.ssh/rwp_lab` (the **private** key).
- `LAB_HOST` = the server IPv4.

- [ ] **Step 2: Write the workflow**

Create `.github/workflows/deploy-lab.yml`:
```yaml
name: deploy-lab

on:
  push:
    branches: [ops-spine]
    paths:
      - "src/**"
      - "Cargo.*"
      - "deploy/**"
      - ".github/workflows/deploy-lab.yml"

concurrency:
  group: deploy-lab
  cancel-in-progress: true

jobs:
  deploy:
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4

      - name: Build static amd64 binary in Docker
        run: |
          docker build --platform linux/amd64 --target export -o out .
          test -x out/site

      - name: Set up SSH
        run: |
          install -m 700 -d ~/.ssh
          echo "${{ secrets.LAB_SSH_KEY }}" > ~/.ssh/id_ed25519
          chmod 600 ~/.ssh/id_ed25519
          ssh-keyscan -H "${{ secrets.LAB_HOST }}" >> ~/.ssh/known_hosts

      - name: Ship + restart
        run: |
          scp -i ~/.ssh/id_ed25519 out/site deploy@${{ secrets.LAB_HOST }}:/opt/site/bin/site.new
          ssh -i ~/.ssh/id_ed25519 deploy@${{ secrets.LAB_HOST }} '
            set -e
            mv /opt/site/bin/site.new /opt/site/bin/site
            chmod +x /opt/site/bin/site
            sudo systemctl restart site
            sleep 1
            systemctl is-active site
          '
```
(Note: the firewall allows SSH only from `admin_ip`. GitHub runners have dynamic IPs, so for CI SSH either (a) add a deploy-only path — e.g. temporarily widen the SSH rule, or better (b) restrict via the SSH key + move CI deploys behind a Cloudflare Tunnel in a later hardening pass. For this slice, simplest is to add the runner step's egress by allowing SSH from anywhere **only if** you accept that tradeoff; document the decision in `deploy/README.md`.)

- [ ] **Step 3: Prove the pipeline**

Change the home response in `src/main.rs` (e.g. append `" v2"` to the string), then:
```bash
git add src/main.rs .github/workflows/deploy-lab.yml
git commit -m "feat(ops): github actions deploy pipeline"
git push -u origin ops-spine
```
Watch the Actions run succeed, then:
```bash
curl -s https://lab.robertwpearce.com/ && echo
```
Expected: the updated string (`...lab.robertwpearce.com v2`). The push→live loop is closed.

---

## Task 7: Reproducibility drill (cattle, not pets)

No stateful data exists yet, so this is the ideal moment to prove the box is fully rebuildable from code — the property that makes learning ops *safe*.

**Files:** none (operational drill).

- [ ] **Step 1: Destroy the box**

Run:
```bash
cd infra && tofu destroy
```
Expected: server + firewall + DNS record destroyed. `curl https://lab.robertwpearce.com/health` now fails.

- [ ] **Step 2: Rebuild from code**

Run:
```bash
cd infra && tofu apply
# wait ~90s for cloud-init, then re-install the unit + cert + deploy (Tasks 4–5 manual steps),
# or simply re-run the CI deploy by pushing an empty commit.
```

- [ ] **Step 3: Verify it's back**

Run:
```bash
curl -s https://lab.robertwpearce.com/health && echo
```
Expected: `{"status":"ok"}`.

- [ ] **Step 4: Record the lesson**

Add a short "Rebuild runbook" section to `deploy/README.md`: the ordered steps to go from `tofu apply` to live (install unit, install origin cert, deploy binary). Note explicitly: **once SQLite lands in a later plan, this drill becomes dangerous without backups — that's when Litestream→R2 becomes mandatory.**

```bash
git add deploy/README.md
git commit -m "docs(ops): rebuild-from-code runbook"
```

---

## Self-Review

- **Spec coverage:** provisioning (T2/T3), hardening (T3), run-as-service (T4), proxy+TLS+Cloudflare (T5), automated pipeline (T6), reproducibility (T7), minimal-tools constraint (Design Decisions + no Docker/Kamal throughout). The one deliberately deferred item is **backups/Litestream** — correct to defer, since this slice has no stateful data; flagged in T7 for the next plan.
- **Known sharp edges called out inline:** US Hetzner locations offer only CPX/CCX — CX & CAX are EU-only — so the box is `cpx21`, not `cx22` (review fix, Global Constraints/T2); cloud-init installs are fail-loud so a bad Caddy apt key can't silently boot a proxy-less box (T3); Docker amd64 builds emulate on Apple-Silicon locally but run native in CI (T4/T6); the CI-runner-vs-SSH-firewall access method is an **open decision** (T6); cloudflare provider v4 vs v5 differences (Global Constraints + T5).
- **Next plan after this:** modernize the real app (axum 0.8 `AppState`, error handling, markdown, static assets, feed/sitemap) **with full URL parity to the current Hakyll scheme + 301s for any changes, verified by a link-check against `docs/url-inventory.txt`** (see URL Preservation), then SQLite + sessions + passkeys + the invite graph, gated onto `/new-zealand/*` at the apex — reusing this exact ops spine.
