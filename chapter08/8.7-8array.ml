let array_iter f ar =
  let rec iter i =
    try f ar.(i); iter (i+1) with Invalid_argument _ -> ()
  in iter 0

let array_iteri f ar =
  let rec iter i =
    try f (i+1) ar.(i); iter (i+1) with Invalid_argument _ -> ()
  in iter 0
