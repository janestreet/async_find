#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"async_find"
  [ oasis_lib "async_find"
  ; file "META" ~section:"lib"
  ]
