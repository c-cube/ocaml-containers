
## Make a release

1. `make test-all`
2. update version in `_oasis`
3. `make update_next_tag` (to update `@since` comments)
4. `git checkout stable`
5. `git merge master`
6. update `CHANGELOG.md` (see its end to find the right git command)
7. commit, tag, and push both to github
8. new opam package

