
## Make a release

1. `make test`
2. update version in `_oasis`
3. `make update_next_tag` (to update `@since` comments; be careful not to change symlinks)
4. update `CHANGELOG.md` (see its end to find the right git command)
5. commit the changes
6. `git checkout stable;  oasis setup`
7. `git merge master`
8. tag, and push both to github
9. new opam package

## List Authors

`git log --format='%aN' | sort -u`

## Subtree

If gen is [this remote](https://github.com/c-cube/gen.git):
`git subtree pull --prefix gen gen master --squash`
