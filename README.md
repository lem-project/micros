# micros
Micro slime/swank with forked SLIME

## Motivation
### Tight coupling of slime with emacs
https://github.com/lem-project/lem/issues/688

With every version update of SLIME, incompatible changes are being made.
Up to version v2.27, it worked fine with Lem, but there seems to be quite a drastic change from around v2.28, and the behavior has changed.
For example, a significant disruptive change in the past can be found at slime/slime@78ad57b.
If not addressed on the client-side, the REPL will hang up.
Also, the output to the REPL has become a bottleneck, and asdf:load-system has become considerably slow.
Besides, there are other areas where behavior has changed due to minor changes in behavior.

As a policy of SLIME, it is strongly integrated with Emacs, and it seems that it does not consider other implementations much.
Continually keeping up with updates to SLIME is too costly and unrealistic, but if left unattended, it will stop working, so constant response is required.

### lem's own extensions
By separating it from SLIME and making it a separate project,
it will be easier to add custom features to Lem. This is useful,
for example, when adding functionality for the Language Server Protocol.

## License
MIT
