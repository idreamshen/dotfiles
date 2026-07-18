#!/usr/bin/env python3
"""Inject the iPad Ctrl-C workaround into ttyd's embedded web client.

ttyd compiles its entire web UI into the binary as a single gzip blob declared
in ``src/html.h`` (``unsigned char index_html[]`` + ``index_html_len`` = the
compressed size + ``index_html_size`` = the uncompressed size). This script
decompresses that blob, injects a small ``<script>`` before ``</body>``, and
regenerates ``html.h`` in place so the normal C build compiles the patched
client.

The injected script reproduces xterm.js PR #5742 (fixes issue #5721) at runtime
via the ``window.term`` global that ttyd exposes: Safari on iPad/iPhone/
AppleVisionPro with a hardware keyboard reports Ctrl-C as keyCode 13 (Enter), so
the bundled xterm 5.x sends CR instead of ETX and never interrupts.

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
