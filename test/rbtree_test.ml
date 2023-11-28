open Core
module Checked_Tree = Rbtree.Make_checked (Int)
module Tree = Rbtree.Make (Int)

let test_empty () =
  Alcotest.(check bool) "same bool" false (Tree.empty |> Tree.member 1)

let test_insert () =
  Alcotest.(check bool)
    "same bool" true
    (Tree.empty |> Tree.insert 1 |> Tree.member 1)

let test_insert_1 () =
  Alcotest.(check bool)
    "same bool" false
    (Tree.empty |> Tree.insert 1 |> Tree.member 0)

let test_insert_2 () =
  Alcotest.(check bool)
    "same bool" false
    (Tree.empty |> Tree.insert 1 |> Tree.member 2)

let swap arr i j =
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp

let shuffled list =
  let arr = Array.of_list list in
  let l = Array.length arr in
  for i = l - 1 downto 1 do
    let r = Random.int (i + 1) in
    swap arr i r
  done;
  arr |> Array.to_list

let rec inserts_deletes_noops i a b c n =
  if i < n then
    match Random.int 3 with
    | 0 -> inserts_deletes_noops (i + 1) (i :: a) b c n
    | 1 -> inserts_deletes_noops (i + 1) a (i :: b) c n
    | 2 -> inserts_deletes_noops (i + 1) a b (i :: c) n
    | _ -> assert false
  else (a, b, c)

let inserts_deletes_noops = inserts_deletes_noops 0 [] [] []

let test_random_permutation size () =
  let inserts, deletes, noops = inserts_deletes_noops size in
  let xs = deletes |> List.append deletes |> List.append inserts |> shuffled in
  let tree =
    xs
    |> List.fold ~init:Tree.empty ~f:(fun tree x ->
           tree |> if Tree.member x tree then Tree.delete x else Tree.insert x)
  in
  let member x = Tree.member x tree in
  let not_member x = x |> member |> not in
  let inserts_good = inserts |> shuffled |> List.for_all ~f:member in
  let deletes_good = deletes |> shuffled |> List.for_all ~f:not_member in
  let noops_good = noops |> shuffled |> List.for_all ~f:not_member in
  Alcotest.(check bool)
    "same bool" true
    (inserts_good && deletes_good && noops_good)

let rec many_randoms tests i n =
  if i < n then
    let tc =
      Alcotest.test_case "insert random" `Quick (test_random_permutation 1000)
    in
    many_randoms (tc :: tests) (i + 1) n
  else tests

let many_randoms = many_randoms [] 0

let () =
  Alcotest.run "Rbtree"
    [
      ( "insert and mem",
        [
          Alcotest.test_case "empty tree" `Quick test_empty;
          Alcotest.test_case "insert 1 test 1" `Quick test_insert;
          Alcotest.test_case "insert 1 test 0" `Quick test_insert_1;
          Alcotest.test_case "insert 1 test 2" `Quick test_insert_2;
          Alcotest.test_case "insert random" `Quick
            (test_random_permutation 5000000);
        ]
        |> List.append (many_randoms 10) );
    ]
