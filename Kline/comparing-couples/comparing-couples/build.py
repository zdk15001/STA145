"""Build index.html from the source files.

Run from this folder:  python3 build.py

It inlines data/hcmst_data.json, src/engine.js and src/app.js into
src/template.html and writes index.html, a single self-contained page.
"""
from pathlib import Path

here = Path(__file__).resolve().parent
template = (here / "src" / "template.html").read_text(encoding="utf-8")
data = (here / "data" / "hcmst_data.json").read_text(encoding="utf-8")
engine = (here / "src" / "engine.js").read_text(encoding="utf-8")
app = (here / "src" / "app.js").read_text(encoding="utf-8")

for name, text in (("data", data), ("engine", engine), ("app", app)):
    if "</script" in text.lower():
        raise SystemExit(f"{name} contains '</script', which would break the page")

page = (template.replace("/*DATA*/", data)
                .replace("/*ENGINE*/", engine)
                .replace("/*APP*/", app))
(here / "index.html").write_text(page, encoding="utf-8")
print(f"Wrote index.html ({len(page):,} bytes)")
