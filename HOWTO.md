
## Make a release

1. `make test-all`
2. update version in `_oasis`
3. `make update_next_tag` (to update `@since` comments)
4. update `CHANGELOG.md` (see its end to find the right git command)
5. `git checkout stable`
6. `git merge master`
7. commit, tag, and push both to github
8. new opam package

## List Authors

`git log --format='%aN' | sort -u`
