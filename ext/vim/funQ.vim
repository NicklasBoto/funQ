" Vim syntax file
" Language: funQ
" Maintainer: qfunc@nicbot.xyz
" Latest Revision: 15 March 2021

if exists("b:current_syntax")
        finish
endif

syn match fqGates /\u+/
hi def link fqGates Special

syn keyword fqBuiltin new meas measure
hi def link fqBuiltin Define

syn match fqLambda /\(λ\|\\\)/
hi def link fqLambda Special

syn keyword fqBit 0 1
hi def link fqBit Number
