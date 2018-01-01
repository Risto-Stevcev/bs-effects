[@bs.val] [@bs.module "fs"]
external read_file_sync :
  (string, ([@bs.string] [ `hex | `utf8 | `ascii ])) => string = "readFileSync";

[@bs.val] [@bs.module "fs"]
external write_file_sync :
  (string, string, ([@bs.string] [ `hex | `utf8 | `ascii ])) => unit = "writeFileSync";

[@bs.val] [@bs.module "fs"]
external read_file_async :
  ( string
  , ([@bs.string] [ `hex | `utf8 | `ascii ])
  , (Js.null(Js.Exn.t), string) => unit
  ) => unit = "readFile";

[@bs.val] [@bs.module "fs"]
external write_file_async :
  ( string
  , string
  , ([@bs.string] [ `hex | `utf8 | `ascii ])
  , Js.null(Js.Exn.t) => unit
  ) => unit = "writeFile";

