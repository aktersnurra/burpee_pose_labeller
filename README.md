# burpee_pose_labeller

OCaml/Bonsai labelling tool for Burpee Trainer pose export bundles.

This repo is intentionally separate from the Phoenix app. Its stable input boundary is an exported dataset bundle:

```text
pose-export-123/
  manifest.json
  dataset.sqlite3
  traces/
    capture-123-warmup.json.zst
    capture-123-main.json.zst
  labels/
    capture-123-labels.json.zst
```

## Goals

- Load exported pose bundles.
- Replay skeleton traces.
- Scrub and zoom timelines.
- Add/edit/delete manual phase, rep, quality, and tag labels.
- Compare manual labels against HSMM/TCN prediction overlays.
- Write labels back to bundle files for import/training.

## Setup

Install OCaml deps in an opam switch:

```sh
opam install . --deps-only --with-test
```

Build:

```sh
dune build @all
```

The browser entrypoint is scaffolded in `app/` and follows the Jane Street Bonsai examples pattern.
