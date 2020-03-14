include Stdlib.Option

let value_exn = function None -> assert false | Some x -> x
