[.results[] | {project: (.attr | split(".")[0]), success}]
| group_by(.project) | map(select(all(.success)) | .[0].project)
| if . == [] then empty else error("fixed, remove from 'broken' in flake.nix: \(join(", "))") end
