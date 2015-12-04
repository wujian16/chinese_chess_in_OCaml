let rec efix f n =
  f (efix f ) n