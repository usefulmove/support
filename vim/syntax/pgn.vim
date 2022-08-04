" Vim syntax file
" Language:  PGN
" Author:  Charles Ford <cford@eudoramail.com>
" Modified:  Duane Edmonds <duane.edmonds@gmail.com>
" Date:  16 April 2018
" Portable Game Notation (PGN) is the standard notation for chess games and
" chess game databases.

:syntax clear

:syntax case ignore

:syntax match pgnMove /[0-9]*\./
:syntax match pgnSymbol /[?!]/
:syntax region pgnString start=/"/ end=/"/ contained
:syntax region pgnAnnotation start=/{/ end=/}/ contains=pgnString
:syntax region pgnTag start=/\[/ end=/\]/ contains=pgnString
:syntax match pgnResult /[0-2]\/*[0-2]*[-][0-2]\/*[0-2]*/

":highlight link pgnTag Comment
:highlight link pgnMove Type
:highlight link pgnString Statement
:highlight link pgnAnnotation Comment
:highlight link pgnSymbol Special
:highlight link pgnResult String
