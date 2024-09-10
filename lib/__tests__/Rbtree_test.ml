open Jest
open Expect
open Rbtree
module Tree = Make (Int)

let () =
  describe "empty" (fun () ->
      test "doesn't contin 1" (fun () ->
          let m = Tree.empty |> Tree.member 1 in
          expect m |> toEqual true))
