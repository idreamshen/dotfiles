#!/usr/bin/env python3
"""Inject local web-client additions into ttyd's embedded HTML.

ttyd compiles its entire web UI into the binary as a single gzip blob declared
in ``src/html.h`` (``unsigned char index_html[]`` + ``index_html_len`` = the
compressed size + ``index_html_size`` = the uncompressed size). This script
decompresses that blob, injects local ``<style>``/``<script>`` snippets, and
regenerates ``html.h`` in place so the normal C build compiles the patched
client.

The injected snippets provide:

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

3. Emacs drop bridge frontend: a small drag/drop/paste bar above the terminal.
   Files dropped on the bar, or images pasted there, are uploaded to the Emacs
   backend on port 7682. Drops in the terminal itself are left alone so ttyd's
   native trzsz handling still works.

Usage:  ttyd-inject-ctrlc-fix.py path/to/src/html.h
"""

import gzip
import re
import sys

# The gate on keyCode === 13 means only the *buggy* Safari event is remapped;
# normal desktop Ctrl-C (keyCode 67) falls through to xterm's native handling.
LOCAL_FIXES_SCRIPT = b"""<script data-ttyd-local-fixes="ctrlc-fit">
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

DROP_STYLE = """<style data-ttyd-drop="emacs-upload-style">
html, body { height: 100%; margin: 0; overflow: hidden; }
body { background: #1e1e1e; }
#ttyd-dropbar { position: fixed; z-index: 10; top: 0; left: 0; right: 0;
                height: 32px; padding: 0 8px 0 12px;
                color: #9e9e9e; background: #2d2d2d;
                border-bottom: 1px solid #444; box-sizing: border-box;
                display: flex; align-items: center; gap: 8px;
                transition: background .15s, color .15s; outline: none;
                font: 13px/32px -apple-system, "Segoe UI", sans-serif;
                user-select: none; }
#ttyd-dropbar:focus { box-shadow: inset 0 0 0 1px #666; }
#ttyd-dropbar.dragover { background: #264f78; color: #fff; }
#ttyd-dropbar.ok { color: #7ec98f; }
#ttyd-dropbar.err { color: #e07a7a; }
#ttyd-droptext { flex: 1 1 auto; white-space: nowrap; overflow: hidden;
                 text-overflow: ellipsis; }
#ttyd-pastebtn { flex: 0 0 auto; height: 24px; padding: 0 8px; color: #cfcfcf;
                 background: #3a3a3a; border: 1px solid #555; border-radius: 4px;
                 font: inherit; line-height: 22px; cursor: pointer; }
#ttyd-pastebtn:hover, #ttyd-pastebtn:focus { background: #454545; color: #fff; }
#terminal-container { position: fixed !important; top: 32px; left: 0; right: 0; bottom: 0;
                      width: auto !important; height: auto !important; margin: 0 !important;
                      max-width: none !important; box-sizing: border-box; }
</style>
""".encode("utf-8")

DROP_SCRIPT = """<script data-ttyd-drop="emacs-upload-script">
/* ttyd-drop: browser frontend for the Emacs upload backend on port 7682. */
(function () {
  var idleTimer = null;
  var idleText = '拖文件到此处 / 点击这里后 Ctrl+V 粘贴截图 → Emacs（Dired 落盘 / agent-shell 附件）；拖到下方终端 = trzsz';
  var uploadUrl = location.protocol + '//' + location.hostname + ':7682/upload';

  function refit() {
    try { window.dispatchEvent(new Event('resize')); } catch (e) {}
  }

  function status(message, cls) {
    var bar = document.getElementById('ttyd-dropbar');
    var text = document.getElementById('ttyd-droptext');
    if (!bar || !text) return;
    clearTimeout(idleTimer);
    bar.className = cls || '';
    text.textContent = message;
    idleTimer = setTimeout(function () {
      bar.className = '';
      text.textContent = idleText;
    }, 8000);
  }

  function pad(n) { return n < 10 ? '0' + n : '' + n; }

  function timestamp() {
    var d = new Date();
    return d.getFullYear() + pad(d.getMonth() + 1) + pad(d.getDate()) + '-' +
      pad(d.getHours()) + pad(d.getMinutes()) + pad(d.getSeconds());
  }

  function extensionForType(type) {
    if (type === 'image/jpeg') return 'jpg';
    if (type === 'image/webp') return 'webp';
    if (type === 'image/gif') return 'gif';
    if (type === 'image/tiff') return 'tiff';
    return 'png';
  }

  function screenshotName(type, index) {
    return 'screenshot-' + timestamp() + '-' + index + '.' + extensionForType(type || 'image/png');
  }

  function uploadFiles(files, label) {
    if (!files.length) { status('没有可上传的文件', 'err'); return; }
    var form = new FormData();
    for (var i = 0; i < files.length; i++) {
      var item = files[i];
      var blob = item.blob || item;
      var name = item.name || blob.name || ('upload-' + i);
      form.append('file' + i, blob, name);
    }
    status((label || '上传中') + '… (' + files.length + ' 个文件)');
    fetch(uploadUrl, { method: 'POST', body: form })
      .then(function (r) { return r.text(); })
      .then(function (message) {
        status(message, message.indexOf('error') === 0 ? 'err' : 'ok');
      })
      .catch(function (err) { status('上传失败: ' + err, 'err'); });
  }

  function clipboardFilesFromPaste(e) {
    var result = [];
    var items = e.clipboardData && e.clipboardData.items;
    if (!items) return result;
    for (var i = 0; i < items.length; i++) {
      var item = items[i];
      if (item.kind === 'file' && item.type && item.type.indexOf('image/') === 0) {
        var file = item.getAsFile();
        if (file) result.push({ blob: file, name: screenshotName(item.type, result.length + 1) });
      }
    }
    return result;
  }

  function pasteFromClipboardApi() {
    if (!navigator.clipboard || !navigator.clipboard.read) {
      status('浏览器不支持直接读取图片；请点击左侧栏后按 Ctrl+V', 'err');
      return;
    }
    navigator.clipboard.read()
      .then(function (items) {
        var files = [];
        var pending = [];
        for (var i = 0; i < items.length; i++) {
          for (var j = 0; j < items[i].types.length; j++) {
            var type = items[i].types[j];
            if (type.indexOf('image/') === 0) {
              pending.push(items[i].getType(type).then(function (blob) {
                files.push({ blob: blob, name: screenshotName(blob.type, files.length + 1) });
              }));
              break;
            }
          }
        }
        return Promise.all(pending).then(function () { return files; });
      })
      .then(function (files) {
        if (!files.length) { status('剪贴板里没有图片；请先截图或复制图片', 'err'); return; }
        uploadFiles(files, '粘贴截图上传中');
      })
      .catch(function (err) {
        status('读取剪贴板失败；请点击左侧栏后按 Ctrl+V: ' + err, 'err');
      });
  }

  function install() {
    if (document.getElementById('ttyd-dropbar')) return;
    var term = document.getElementById('terminal-container');
    if (!term || !document.body) return setTimeout(install, 100);

    var bar = document.createElement('div');
    bar.id = 'ttyd-dropbar';
    bar.tabIndex = 0;

    var text = document.createElement('span');
    text.id = 'ttyd-droptext';
    text.textContent = idleText;

    var pasteBtn = document.createElement('button');
    pasteBtn.id = 'ttyd-pastebtn';
    pasteBtn.type = 'button';
    pasteBtn.title = 'Read image from clipboard and upload it';
    pasteBtn.textContent = 'Paste image';

    bar.appendChild(text);
    bar.appendChild(pasteBtn);
    document.body.appendChild(bar);

    bar.addEventListener('click', function () { bar.focus(); });
    bar.addEventListener('dragover', function (e) {
      e.preventDefault();
      bar.classList.add('dragover');
    });
    bar.addEventListener('dragleave', function () {
      bar.classList.remove('dragover');
    });
    bar.addEventListener('drop', function (e) {
      e.preventDefault();
      bar.classList.remove('dragover');
      uploadFiles(e.dataTransfer.files, '上传中');
    });
    bar.addEventListener('paste', function (e) {
      var files = clipboardFilesFromPaste(e);
      if (!files.length) { status('剪贴板里没有图片；请先截图或复制图片', 'err'); return; }
      e.preventDefault();
      uploadFiles(files, '粘贴截图上传中');
    });
    pasteBtn.addEventListener('click', function (e) {
      e.stopPropagation();
      pasteFromClipboardApi();
    });

    refit();
    setTimeout(refit, 100);
    setTimeout(refit, 500);
  }

  install();
})();
</script>
""".encode("utf-8")

LOCAL_FIXES_MARKER = b'data-ttyd-local-fixes="ctrlc-fit"'
OLD_LOCAL_FIXES_MARKER = b"Workaround for xterm.js #5721 / PR #5742"
DROP_STYLE_MARKER = b'data-ttyd-drop="emacs-upload-style"'
DROP_SCRIPT_MARKER = b'data-ttyd-drop="emacs-upload-script"'


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


def insert_once(html: bytes, anchor: bytes, snippet: bytes, marker: bytes, label: str) -> tuple[bytes, bool]:
    if marker in html:
        return html, False
    count = html.count(anchor)
    if count != 1:
        sys.exit(f"error: expected exactly one {anchor.decode()}, found {count} while injecting {label}")
    return html.replace(anchor, snippet + anchor, 1), True


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
    patched = html
    changed = []

    patched, inserted = insert_once(patched, b"</head>", DROP_STYLE, DROP_STYLE_MARKER, "ttyd-drop style")
    if inserted:
        changed.append("drop style")

    if LOCAL_FIXES_MARKER not in patched and OLD_LOCAL_FIXES_MARKER not in patched:
        patched, inserted = insert_once(
            patched, b"</body>", LOCAL_FIXES_SCRIPT, LOCAL_FIXES_MARKER, "local ttyd fixes"
        )
        if inserted:
            changed.append("local fixes")
    elif OLD_LOCAL_FIXES_MARKER in patched and LOCAL_FIXES_MARKER not in patched:
        print("ttyd-inject-ctrlc-fix: existing local fixes detected without marker")

    patched, inserted = insert_once(patched, b"</body>", DROP_SCRIPT, DROP_SCRIPT_MARKER, "ttyd-drop script")
    if inserted:
        changed.append("drop script")

    if not changed:
        print("ttyd-inject-ctrlc-fix: already patched, skipping")
        return

    gz = gzip.compress(patched, 9)

    # Sanity: the C side inflates with gzip framing and must reach Z_STREAM_END.
    assert gzip.decompress(gz) == patched, "gzip round-trip mismatch"

    open(path, "w").write(render_html_h(gz, len(patched)))
    print(
        f"ttyd-inject-ctrlc-fix: patched {path} "
        f"({', '.join(changed)}; "
        f"compressed {len(blob)} -> {len(gz)} bytes, "
        f"uncompressed {len(html)} -> {len(patched)} bytes)"
    )


if __name__ == "__main__":
    main()
