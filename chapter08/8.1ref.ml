type 'a ref = { mutable contents: 'a }
let ref x = { contents = x }
let (!) { contents = x } = x
let (:=) l r = l.contents <- r
