#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let opams = [Pkg.opam_file ~lint_deps_excluding:(Some ["odoc"]) "xi-rope.opam"]

let () =
  Pkg.describe "xi-rope" ~opams @@ fun _c ->
  Ok [
    Pkg.mllib "lib/xi_rope.mllib";
    Pkg.test "tests/tests"
  ]
