
## Make a release

1. `make test-all`
2. merge into `stable` (from now on, proceed on branch `stable`)
3. update version in `_oasis`
4. `make update_next_tag` (to update `@since` comments)
5. update `CHANGELOG.md` (see its end to find the right git command)
6. commit, tag, and push both to github
7. new opam package

