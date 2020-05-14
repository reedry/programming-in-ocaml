let rec fr s e body =
  if s <= e then
    begin body (); fr (s+1) e body end
