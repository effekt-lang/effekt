
(* EVIDENCE *)
let lift m k1 k2 = m (fun a -> k1 a k2)
let nested ev1 ev2 m = ev1 (ev2 m)
let here x = x

(* REGIONS *)
(* type region = ((() -> () -> ()) list) ref *)
let newRegion () = ref []
let backup cell () =
  let oldState = !cell in
  fun () -> cell := oldState

let fresh r init =
  let cell = ref init in
  r := (backup cell) :: !r;
  cell

let withRegion body =
  let r = newRegion () in
  let lift m k =
    let fields = List.map (fun f -> f ()) !r in
    m (fun a -> List.iter (fun f -> f ()) fields; k a)
  in
  body lift r

(* SHOW INSTANCES *)

(*
To align with the js backend inf and nan is rewritten
- `nan` -> `NaN`
- `inf` -> `Infinity`
- `~2.1` -> `-2.1`
They do still disagree on the truncating of `2.0` to `2` (ml does not).
*)
let show_float x =
  let s = string_of_float x in
  match s with
  | "nan" -> "NaN"
  | "inf" -> "Infinity"
  | _ ->
      if String.ends_with ~suffix:"." s then
        String.sub s 0 (String.length s - 1)
      else
        s


(* TIMING *)


(* RANDOM *)
let() = Random.self_init ()

(* HOLES *)
exception Hole
