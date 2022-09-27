fun bind m f k =
  m (fn (a) => f a k);

fun pure a k =
  k a

fun lift m k1 k2 =
  m (fn a => k1 a k2);

fun reset m =
   m pure;

fun run (m) =
  m (fn a => a);

fun here (x) = x;
