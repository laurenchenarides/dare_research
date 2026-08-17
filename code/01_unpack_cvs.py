#!/usr/bin/env python3
"""
01_unpack_cvs.py - Step 1 of the DARE Section 4 CV pipeline.

The faculty "PDF" packets in the project folder are not PDFs. Each is a ZIP
archive containing one JPEG and one .txt per page, plus a manifest.json.
This script unpacks each archive and assembles the per-page text into a
single text file per faculty member, in page order, with page markers.

Fully deterministic: rerun any time a packet is added or replaced.

Usage:  python3 01_unpack_cvs.py <packet_dir> <out_dir>
"""
import json
import os
import re
import subprocess
import sys
import zipfile

SKIP = {"DARE_Program_Review_2020"}


def main(packet_dir, out_dir):
    os.makedirs(out_dir, exist_ok=True)
    log = []
    for fn in sorted(os.listdir(packet_dir)):
        if not fn.lower().endswith(".pdf"):
            continue
        stem = fn[:-4]
        if stem in SKIP:
            continue
        path = os.path.join(packet_dir, fn)
        if not zipfile.is_zipfile(path):
            # Some packets are genuine PDFs rather than page-image archives.
            out = os.path.join(out_dir, stem + ".txt")
            rc = subprocess.run(["pdftotext", "-layout", path, out],
                                capture_output=True).returncode
            if rc == 0:
                log.append((stem, "ok", "true PDF, pdftotext -layout"))
            else:
                log.append((stem, "ERROR", "neither zip nor readable PDF"))
            continue
        with zipfile.ZipFile(path) as z:
            names = z.namelist()
            if "manifest.json" in names:
                pages = json.loads(z.read("manifest.json"))["pages"]
                order = [p["text"]["path"]
                         for p in sorted(pages, key=lambda p: p["page_number"])]
            else:
                order = sorted([n for n in names if n.endswith(".txt")],
                               key=lambda n: int(re.match(r"(\d+)", n).group(1)))
            with open(os.path.join(out_dir, stem + ".txt"), "w") as fh:
                for i, member in enumerate(order, 1):
                    fh.write("\n===== PAGE %d =====\n" % i)
                    fh.write(z.read(member).decode("utf-8", errors="replace"))
            log.append((stem, "ok", "%d pages" % len(order)))

    for stem, status, note in log:
        print("%-24s %-6s %s" % (stem, status, note))
    print("\n%d packets unpacked -> %s" % (sum(1 for r in log if r[1] == "ok"), out_dir))


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "/mnt/project",
         sys.argv[2] if len(sys.argv) > 2 else "txt")
