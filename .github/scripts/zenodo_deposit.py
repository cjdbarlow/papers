#!/usr/bin/env python3
"""
Mint/update Zenodo DOIs for paper subfolders in a monorepo.

A folder is opted in by containing a `zenodo.yml` sentinel holding its
Zenodo metadata. The folder is (re)archived when its content hash changes.

State (concept recid, last DOI, last hash) is tracked per folder in
`.zenodo-state.json` at the repo root.

Exclusions (applied identically to the hash and the uploaded zip):
  - subfolders whose name is in DATA_DIR_NAMES (default: data, raw, data-raw)
  - all *.md files
  - the zenodo.yml sentinel itself
"""

from __future__ import annotations

import hashlib
import io
import json
import os
import sys
import zipfile
from pathlib import Path

import requests
import yaml

# --- configuration -----------------------------------------------------------

REPO_ROOT = Path(os.environ.get("GITHUB_WORKSPACE", ".")).resolve()
SENTINEL = "zenodo.yml"
STATE_FILE = REPO_ROOT / ".zenodo-state.json"
DOI_FILE = "doi.md"

DATA_DIR_NAMES = {
    n.strip().lower()
    for n in os.environ.get("DATA_DIR_NAMES", "data,raw,data-raw").split(",")
    if n.strip()
}

ENV = os.environ.get("ZENODO_ENV", "sandbox").lower()
BASE_URL = {
    "sandbox": "https://sandbox.zenodo.org",
    "production": "https://zenodo.org",
}.get(ENV)
if BASE_URL is None:
    sys.exit(f"ERROR: ZENODO_ENV must be 'sandbox' or 'production', got {ENV!r}")

TOKEN = os.environ.get("ZENODO_TOKEN")
if not TOKEN:
    sys.exit("ERROR: ZENODO_TOKEN is not set")

API = f"{BASE_URL}/api"
SESSION = requests.Session()
SESSION.headers.update({"Authorization": f"Bearer {TOKEN}"})
TIMEOUT = 60


# --- helpers -----------------------------------------------------------------

def is_excluded(rel: Path) -> bool:
    """True if a path (relative to the paper folder) is excluded from
    both the content hash and the uploaded archive."""
    if any(part.lower() in DATA_DIR_NAMES for part in rel.parts[:-1]):
        return True
    if rel.name == SENTINEL:
        return True
    if rel.suffix.lower() == ".md":
        return True
    return False


def included_files(folder: Path) -> list[Path]:
    """Sorted list of included files (relative to *folder*)."""
    out = []
    for p in folder.rglob("*"):
        if not p.is_file():
            continue
        rel = p.relative_to(folder)
        if is_excluded(rel):
            continue
        out.append(rel)
    return sorted(out, key=lambda r: r.as_posix())


def content_hash(folder: Path, files: list[Path]) -> str:
    """Deterministic hash over included relative paths + their bytes."""
    h = hashlib.sha256()
    for rel in files:
        h.update(rel.as_posix().encode("utf-8"))
        h.update(b"\0")
        h.update((folder / rel).read_bytes())
        h.update(b"\0")
    return h.hexdigest()


def build_zip(folder: Path, files: list[Path]) -> bytes:
    """Zip the included files, keyed under the folder name so the archive
    extracts into a sensibly-named directory."""
    top = folder.name
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w", zipfile.ZIP_DEFLATED) as zf:
        for rel in files:
            zf.write(folder / rel, arcname=f"{top}/{rel.as_posix()}")
    return buf.getvalue()


def load_metadata(folder: Path) -> dict:
    """Translate the folder's zenodo.yml into a Zenodo metadata object."""
    raw = yaml.safe_load((folder / SENTINEL).read_text()) or {}
    meta: dict = {
        "title": raw["title"],
        "upload_type": raw.get("upload_type", "software"),
        "description": raw["description"],
        "creators": raw["creators"],
    }
    if "license" in raw:
        meta["license"] = raw["license"]
    if "keywords" in raw:
        meta["keywords"] = raw["keywords"]
    if "version" in raw:
        meta["version"] = str(raw["version"])
    # passthrough for anything else Zenodo accepts (e.g. related_identifiers)
    for k in ("related_identifiers", "communities", "notes", "language"):
        if k in raw:
            meta[k] = raw[k]
    return meta


def api(method: str, url: str, **kw) -> requests.Response:
    r = SESSION.request(method, url, timeout=TIMEOUT, **kw)
    if not r.ok:
        body = r.text[:2000]
        sys.exit(f"ERROR: Zenodo {method} {url} -> {r.status_code}\n{body}")
    return r


# --- core deposition flow ----------------------------------------------------

def new_draft() -> dict:
    """Create a brand-new empty deposition draft."""
    return api("POST", f"{API}/deposit/depositions",
               json={}, headers={"Content-Type": "application/json"}).json()


def new_version_draft(concept_recid: str) -> dict:
    """Create a new-version draft from the latest published version of a
    concept. Returns the editable draft deposition."""
    # Find the latest deposition for this concept. The recid we stored is the
    # one we last published; newversion off it yields a fresh draft whose id
    # lives in links.latest_draft.
    r = api("POST",
            f"{API}/deposit/depositions/{concept_recid}/actions/newversion")
    latest_draft_url = r.json()["links"]["latest_draft"]
    draft = api("GET", latest_draft_url).json()
    # A new version inherits the previous files; strip them so we upload clean.
    for f in draft.get("files", []):
        file_url = f["links"].get("self") or \
            f"{latest_draft_url}/files/{f['id']}"
        api("DELETE", file_url)
    return api("GET", latest_draft_url).json()


def put_metadata(draft: dict, metadata: dict) -> None:
    url = draft["links"]["self"]
    api("PUT", url,
        json={"metadata": metadata},
        headers={"Content-Type": "application/json"})


def upload_file(draft: dict, filename: str, data: bytes) -> None:
    """Upload via the bucket API (preferred over the deprecated files API)."""
    bucket = draft["links"]["bucket"]
    api("PUT", f"{bucket}/{filename}", data=data)


def publish(draft: dict) -> dict:
    url = draft["links"]["publish"]
    return api("POST", url).json()


def archive_folder(folder: Path, state_entry: dict | None) -> dict:
    """Run the full deposit flow for one folder. Returns the new state entry."""
    files = included_files(folder)
    if not files:
        sys.exit(f"ERROR: {folder.name} has no includable files to archive")

    metadata = load_metadata(folder)
    zip_bytes = build_zip(folder, files)
    zip_name = f"{folder.name}.zip"

    if state_entry and state_entry.get("concept_recid"):
        draft = new_version_draft(state_entry["concept_recid"])
    else:
        draft = new_draft()

    put_metadata(draft, metadata)
    upload_file(draft, zip_name, zip_bytes)
    published = publish(draft)

    doi = published.get("doi") or \
        published.get("metadata", {}).get("doi")
    concept_recid = str(
        published.get("conceptrecid") or published["id"]
    )
    concept_doi = published.get("conceptdoi") or \
        published.get("metadata", {}).get("conceptdoi")
    record_url = published.get("links", {}).get("record_html") or \
        f"{BASE_URL}/records/{published['id']}"

    return {
        "concept_recid": concept_recid,
        "last_version_doi": doi,
        "concept_doi": concept_doi,
        "record_url": record_url,
    }


def write_doi_md(folder: Path, entry: dict, hash_hex: str) -> None:
    concept_doi = entry.get("concept_doi")
    version_doi = entry.get("last_version_doi")
    lines = [f"# DOI", ""]
    if concept_doi:
        lines += [
            "**Cite all versions** (concept DOI — always resolves to the "
            "latest):",
            "",
            f"[{concept_doi}](https://doi.org/{concept_doi})",
            "",
        ]
    lines += [
        "**This version:**",
        "",
        f"[{version_doi}](https://doi.org/{version_doi})",
        "",
        f"Archived on Zenodo: {entry.get('record_url', '')}",
        "",
        "<!-- This file is generated by the Zenodo workflow. "
        "Do not edit by hand. -->",
        f"<!-- content-hash: {hash_hex} -->",
        "",
    ]
    (folder / DOI_FILE).write_text("\n".join(lines))


# --- driver ------------------------------------------------------------------

def load_state() -> dict:
    if STATE_FILE.exists():
        return json.loads(STATE_FILE.read_text())
    return {}


def save_state(state: dict) -> None:
    STATE_FILE.write_text(json.dumps(state, indent=2, sort_keys=True) + "\n")


def find_opted_in_folders() -> list[Path]:
    out = []
    for sentinel in REPO_ROOT.rglob(SENTINEL):
        # Only treat a sentinel as a paper folder marker; skip if it somehow
        # sits inside a data dir.
        rel = sentinel.parent.relative_to(REPO_ROOT)
        if any(part.lower() in DATA_DIR_NAMES for part in rel.parts):
            continue
        out.append(sentinel.parent)
    return sorted(out, key=lambda p: p.as_posix())


def main() -> int:
    state = load_state()
    changed_any = False
    summary = []

    for folder in find_opted_in_folders():
        key = folder.relative_to(REPO_ROOT).as_posix()
        files = included_files(folder)
        if not files:
            summary.append(f"SKIP  {key} (no includable files)")
            continue

        new_hash = content_hash(folder, files)
        prev = state.get(key)
        prev_hash = prev.get("content_hash") if prev else None

        if prev_hash == new_hash:
            summary.append(f"OK    {key} (unchanged)")
            continue

        action = "update" if prev else "create"
        print(f"::group::Archiving {key} ({action})")
        entry = archive_folder(folder, prev)
        entry["content_hash"] = new_hash
        state[key] = entry
        write_doi_md(folder, entry, new_hash)
        save_state(state)  # persist incrementally so a mid-run failure
        # doesn't lose already-minted DOIs
        changed_any = True
        summary.append(
            f"DONE  {key} -> {entry['last_version_doi']} ({action})"
        )
        print(f"::endgroup::")

    print("\n=== Zenodo summary ===")
    for line in summary:
        print(line)

    # Signal to the workflow whether a commit is needed.
    gh_out = os.environ.get("GITHUB_OUTPUT")
    if gh_out:
        with open(gh_out, "a") as fh:
            fh.write(f"changed={'true' if changed_any else 'false'}\n")

    return 0


if __name__ == "__main__":
    sys.exit(main())
