[![MELPA](https://melpa.org/packages/org-emms-badge.svg)](https://melpa.org/#/org-emms)

# org-emms

This package provides an Org link type for playing multimedia files
with EMMS, The Emacs Multimedia System. If the link contains a track
position, playback will start at the specified position. For example:

```
[[emms:/path/to/audio.mp3::2:43]]     Starts playback at 2 min 43 sec.
[[emms:/path/to/audio.mp3::1:10:45]]  Starts playback at 1 hr 10 min 45 sec.
[[emms:/path/to/audio.mp3::49]]       Starts playback at 0 min 49 sec.
```

This is particularly useful for transcription, language study, lecture
notes, interviews, and any Org document that references specific
positions in an audio or video file.

Available commands include `org-emms-insert-link`,
`org-emms-insert-track`, `org-emms-insert-track-position`

It is also possible to store an Org link from an EMMS playlist or
browser buffer with `org-store-link`, then insert it into an Org
buffer with `org-insert-link`.

## Installation

Install `org-emms` from MELPA:

```elisp
M-x package-install RET org-emms RET
```

Then load it in your Emacs configuration:

```elisp
(require 'org-emms)
```

EMMS must also be installed and configured with a player that supports
the media formats you intend to use.

## URL support

`org-emms` currently targets local multimedia files through
`emms-play-file`. Support for URLs through `emms-play-url`, including
services played through MPV, is being considered separately.
