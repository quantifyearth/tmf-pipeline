let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.( >>= )
let or_raise = function Ok v -> v | Error (`Msg m) -> failwith m
