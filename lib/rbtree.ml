type color = R | B | BB

module type Elt = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type t
  type elt

  val empty : t
  val member : elt -> t -> bool
  val insert : elt -> t -> t
  val delete : elt -> t -> t
  val of_list : elt List.t -> t
  val to_list : t -> elt List.t
end

module Make (Elt : Elt) = struct
  type t = T of color * t * Elt.t * t | E | EE

  let empty = E

  let rec member x tree =
    match tree with
    | E | EE -> false
    | T (_, l, y, r) ->
        let c = Elt.compare x y in
        if c < 0 then member x l else if c > 0 then member x r else true

  let redden = function
    | T (B, (T (B, _, _, _) as l), x, (T (B, _, _, _) as r)) -> T (R, l, x, r)
    | x -> x

  let blacken = function
    | T (R, (T (R, _, _, _) as a), x, b) | T (R, a, x, (T (R, _, _, _) as b)) ->
        T (B, a, x, b)
    | t -> t

  let balance color a x b =
    match (color, a, x, b) with
    | B, T (R, T (R, a, x, b), y, c), z, d
    | B, T (R, a, x, T (R, b, y, c)), z, d
    | B, a, x, T (R, T (R, b, y, c), z, d)
    | B, a, x, T (R, b, y, T (R, c, z, d)) ->
        T (R, T (B, a, x, b), y, T (B, c, z, d))
    | BB, a, x, T (R, T (R, b, y, c), z, d)
    | BB, T (R, a, x, T (R, b, y, c)), z, d ->
        T (B, T (B, a, x, b), y, T (B, c, z, d))
    | _ -> T (color, a, x, b)

  let insert x s =
    let rec ins = function
      | E | EE -> T (R, E, x, E)
      | T (color, a, y, b) as t ->
          let cmp = Elt.compare x y in
          if cmp < 0 then balance color (ins a) y b
          else if cmp == 0 then t
          else balance color a y (ins b)
    in

    s |> ins |> blacken

  let rotate color a x b =
    match (color, a, x, b) with
    | R, T (BB, a, x, b), y, T (B, c, z, d) ->
        balance B (T (R, T (B, a, x, b), y, c)) z d
    | R, EE, y, T (B, c, z, d) -> balance B (T (R, E, y, c)) z d
    | R, T (B, a, x, b), y, T (BB, c, z, d) ->
        balance B a x (T (R, b, y, T (B, c, z, d)))
    | R, T (B, a, x, b), y, EE -> balance B a x (T (R, b, y, E))
    (* break *)
    | B, T (BB, a, x, b), y, T (B, c, z, d) ->
        balance BB (T (R, T (B, a, x, b), y, c)) z d
    | B, EE, y, T (B, c, z, d) -> balance BB (T (R, E, y, c)) z d
    | B, T (B, a, x, b), y, T (BB, c, z, d) ->
        balance BB a x (T (R, b, y, T (B, c, z, d)))
    | B, T (B, a, x, b), y, EE -> balance BB a x (T (R, b, y, E))
    (* break *)
    | B, T (BB, a, w, b), x, T (R, T (B, c, y, d), z, e) ->
        T (B, balance B (T (R, T (B, a, w, b), x, c)) y d, z, e)
    | B, EE, x, T (R, T (B, c, y, d), z, e) ->
        T (B, balance B (T (R, E, x, c)) y d, z, e)
    | B, T (R, a, w, T (B, b, x, c)), y, T (BB, d, z, e) ->
        T (B, a, w, balance B b x (T (R, c, y, T (B, d, z, e))))
    | B, T (R, a, w, T (B, b, x, c)), y, EE ->
        T (B, a, w, balance B b x (T (R, c, y, E)))
    (* break *)
    | _ -> T (color, a, x, b)

  let rec min_del = function
    | T (R, E, x, E) -> (x, E)
    | T (B, E, x, E) -> (x, EE)
    | T (B, E, x, T (R, E, y, E)) -> (x, T (B, E, y, E))
    | T (c, a, x, b) ->
        let x', a' = min_del a in
        (x', rotate c a' x b)
    | E | EE -> assert false

  let delete x s =
    let rec del = function
      | E | EE -> E
      | T (R, E, y, E) as t -> if Elt.compare x y == 0 then E else t
      | T (B, E, y, E) as t -> if Elt.compare x y == 0 then EE else t
      | T (B, T (R, E, y, E), z, E) as t ->
          let c = Elt.compare x z in
          if c < 0 then T (B, del (T (R, E, y, E)), z, E)
          else if c == 0 then T (B, E, y, E)
          else t
      | T (c, a, y, b) ->
          let cmp = Elt.compare x y in
          if cmp < 0 then rotate c (del a) y b
          else if cmp == 0 then
            let y', b' = min_del b in
            rotate c a y' b'
          else rotate c a y (del b)
    in
    del (redden s)

  let of_list = List.fold_left (fun t e -> insert e t) E

  let to_list x =
    let rec f items = function
      | [] -> items
      | t :: rest -> (
          match t with
          | EE -> assert false
          | E -> f items rest
          | T (_, a, x, E) -> f (x :: items) (a :: rest)
          | T (color, a, x, b) -> f items (b :: T (color, a, x, E) :: rest))
    in
    f [] [ x ]

  let rec check = function
    | EE -> assert false (* EE is an intermediate state in deletion *)
    | E -> 1
    | T (color, left, _, right) -> (
        let a = check left in
        let b = check right in
        if a <> b then assert false
        else
          match color with
          | R ->
              (* a red node has no red child *)
              let () =
                match left with T (R, _, _, _) -> assert false | _ -> ()
              in
              let () =
                match right with T (R, _, _, _) -> assert false | _ -> ()
              in
              (* a red node contributes 0 to black depth *)
              a
          | B -> a + 1
          | BB -> assert false)
end

module Make_checked (Elt : Elt) = struct
  include Make (Elt)

  let insert x t =
    let t = insert x t in
    let _ = check t in
    t

  let delete x t =
    let t = delete x t in
    let _ = check t in
    t
end
