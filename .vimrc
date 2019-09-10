" Enable hlint and GHC via Cabal
let g:ale_linters = {'haskell': ['hlint', 'cabal_new_ghc']}
" ... only
let g:ale_linters_explicit = 1
" Don't lint until I save
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter = 'never'
" Set up Alexis King options
" Have to do it this way because ALE is special
call ale#linter#Define('haskell', {
\   'name': 'cabal_new_ghc',
\   'aliases': ['cabal-new-ghc'],
\   'output_stream': 'stderr',
\   'executable': 'cabal',
\   'command': 'cabal new-exec -- ghc -isrc -itest -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints -fno-code -v0 %t', 
\   'callback': 'ale#handlers#haskell#HandleGHCFormat',
\})
