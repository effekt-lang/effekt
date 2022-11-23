fun bind m f k = m (fn a => f a k);

fun pure a k = k a;

fun lift m k1 k2 = m (fn a => k1 a k2);

fun reset m = m pure;

fun run m = m (fn a => a);

fun nested ev1 ev2 m = ev1 (ev2 m);

fun here x = x;

fun effektIntString x =
    let val s = (Int.toString x) in
    case String.sub (s, 0) of
          #"~" => "-" ^ String.extract (s, 1, NONE)
        | _ => s
    end;

(*
To alingn with the js backend inf and nan is rewritten
- `nan` -> `NaN`
- `inf` -> `Infinity`
- `~2.1` -> `-2.1`

They do still disagree on the truncating of `2.0` to `2` (ml does not).
*)
fun effektDoubleString x =
    let val s = (Real.toString x) in
    case s of
          "nan" => "NaN"
        | "inf" => "Infinity"
        | _ => case String.sub (s, 0) of
              #"~" => "-" ^ String.extract (s, 1, NONE)
            | _ => s
    end;

val mlStartTime = Time.toMilliseconds (Time.now ());

fun mlRandomReal () =
   let
      val r = MLton.Random.rand ();
      val rreal = MLton.Real.fromWord r;
      val wsize = Real.fromInt Word.wordSize;
      fun shiftRight r shift = r / (Math.pow(2.0, shift))
   in
      shiftRight rreal wsize
   end;
