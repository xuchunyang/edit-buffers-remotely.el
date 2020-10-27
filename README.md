# Edit Emacs Buffers in a Web browser

![Test](https://github.com/xuchunyang/edit-buffers-remotely.el/workflows/Test/badge.svg)

`M-x edit-buffers-remotely-server-start` starts a HTTP server which exposes
Emacs buffers. User can edit the contents of the buffer in a `<textarea>`
element. The changes will be sync back to Emacs via WebSocket when the
textarea's
[`change`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event)
event fires.

## Depends

- web-server (available on MELPA. GNU ELPA and MELPA provide an old version - 0.1.2, I have not tested it and am not sure if it works)
- Emacs 25.1
