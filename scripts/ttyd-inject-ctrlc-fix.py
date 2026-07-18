#!/usr/bin/env python3
"""Inject web-client workarounds into ttyd's embedded HTML.

ttyd compiles its entire web UI into the binary as a single gzip blob declared
in ``src/html.h`` (``unsigned char index_html[]`` + ``index_html_len`` = the
compressed size + ``index_html_size`` = the uncompressed size). This script
decompresses that blob, injects a small ``<script>`` before ``</body>``, and
regenerates ``html.h`` in place so the normal C build compiles the patched
client.

The injected script bundles two runtime fixes:

1. iPad Ctrl-C: reproduces xterm.js PR #5742 (fixes issue #5721) via the
   ``window.term`` global that ttyd exposes. Safari on iPad/iPhone/
   AppleVisionPro with a hardware keyboard reports Ctrl-C as keyCode 13
   (Enter), so bundled xterm 5.x sends CR instead of ETX and never interrupts.

2. Initial sizing: xterm's FitAddon runs its first ``fit()`` before the web
   font finishes loading, so the fallback font's wider glyphs under-size the
   PTY. ttyd only refits on the window ``resize`` event, so a fresh page keeps
   the wrong size until the user manually resizes -- which is why an
   ``emacsclient -nw`` frame opened right after load fills only part of the
   screen. Force a refit once fonts are ready (plus a few short retries) so the
   correct size reaches the PTY before anything is launched in it.

Usage:  ttyd-inject-ctrlc-fix.py path/to/src/html.h
"""

import gzip
import re
import sys

# The gate on keyCode === 13 means only the *buggy* Safari event is remapped;
# normal desktop Ctrl-C (keyCode 67) falls through to xterm's native handling.
SCRIPT = b"""<script>
/* Workaround for xterm.js #5721 / PR #5742: Safari on iPad/iPhone/AppleVisionPro
   with a hardware keyboard reports Ctrl-C as keyCode 13 (Enter), so ttyd's bundled
   xterm 5.x sends CR instead of ETX and never interrupts. Patch it at runtime via
   the window.term global that ttyd exposes. */
(function () {
  function install() {
    var t = window.term;
    if (!t || typeof t.attachCustomKeyEventHandler !== 'function') {
      return setTimeout(install, 200);
    }
    t.attachCustomKeyEventHandler(function (e) {
      if (e.type === 'keydown' && e.ctrlKey && e.key === 'c' &&
          (e.keyCode === 13 || e.which === 13)) {
        t.input('\\x03');   // ETX / Ctrl-C
        return false;       // suppress xterm's buggy CR
      }
      return true;
    });
  }
  install();
})();
</script>
<script>
/* Fix initial terminal sizing: ttyd's xterm FitAddon computes columns/rows
   before the web font finishes loading, so the first fit under-sizes the PTY,
   and ttyd only refits on the window 'resize' event. Force a refit once fonts
   are ready (plus a few short retries) so the correct size reaches the PTY --
   e.g. before an `emacsclient -nw` frame is opened in it. */
(function () {
  function refit() {
    try { window.dispatchEvent(new Event('resize')); } catch (e) {}
  }
  function schedule() {
    refit();
    setTimeout(refit, 200);
    setTimeout(refit, 600);
    setTimeout(refit, 1200);
  }
  if (document.fonts && document.fonts.ready) {
    document.fonts.ready.then(schedule);
  } else {
    schedule();
  }
  window.addEventListener('load', schedule);
})();
</script>
"""

MARKER = b"attachCustomKeyEventHandler"


def render_html_h(gz: bytes, uncompressed_len: int) -> str:
    lines = ["unsigned char index_html[] = {"]
    for i in range(0, len(gz), 12):
        chunk = gz[i:i + 12]
        row = "  " + ", ".join(f"0x{b:02x}" for b in chunk) + ","
        lines.append(row)
    # Drop the trailing comma on the final byte to match xxd's output style.
    lines[-1] = lines[-1].rstrip(",")
    lines.append("};")
    lines.append(f"unsigned int index_html_len = {len(gz)};")
    lines.append(f"unsigned int index_html_size = {uncompressed_len};")
    lines.append("")
    return "\n".join(lines)


def main() -> None:
    if len(sys.argv) != 2:
        sys.exit(f"usage: {sys.argv[0]} path/to/html.h")
    path = sys.argv[1]

    text = open(path).read()
    m = re.search(r"index_html\[\]\s*=\s*\{(.*?)\};", text, re.S)
    if not m:
        sys.exit("error: could not find index_html[] array in html.h")
    hexs = re.findall(r"0x[0-9a-fA-F]{2}", m.group(1))
    blob = bytes(int(h, 16) for h in hexs)

    html = gzip.decompress(blob)

    if MARKER in html and SCRIPT.strip() in html:
        # Already patched (idempotent re-run).
        print("ttyd-inject-ctrlc-fix: already patched, skipping")
        return

    count = html.count(b"</body>")
    if count != 1:
        sys.exit(f"error: expected exactly one </body>, found {count}")

    patched = html.replace(b"</body>", SCRIPT + b"</body>", 1)

    gz = gzip.compress(patched, 9)

    # Sanity: the C side inflates with gzip framing and must reach Z_STREAM_END.
    assert gzip.decompress(gz) == patched, "gzip round-trip mismatch"

    open(path, "w").write(render_html_h(gz, len(patched)))
    print(
        f"ttyd-inject-ctrlc-fix: patched {path} "
        f"(compressed {len(blob)} -> {len(gz)} bytes, "
        f"uncompressed {len(html)} -> {len(patched)} bytes)"
    )


if __name__ == "__main__":
    main()
