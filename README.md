# Edit Emacs Buffers in a Web browser

`M-x edit-buffers-remotely-server-start` starts a HTTP server which exposes
Emacs buffers. User can edit the contents of the buffer in a `<textarea>`
element. The changes will be sync back to Emacs when the textarea's
[`change`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event)
event fires.

## Depends

- web-server (available on MELPA. GNU ELPA and MELPA provide an old version - 0.1.2, I have not tested it and am not sure if it works)
- Emacs 25.1
