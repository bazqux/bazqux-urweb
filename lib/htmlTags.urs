val meta : unit -> tag [Nam = meta, Content = string, Id = id, Charset = string, Itemprop = string, Property = string] head [] [] []
(* blessMeta должен также проверять itemprop и propery? *)

val link : unit -> tag [Data = data_attr, Id = id, Rel = string, Title = string, Typ = string, Href = url, Media = string, Integrity = string, Crossorigin = string, Sizes = string, Color = string, As = string] head [] [] []

val picture : bodyTag boxAttrs
