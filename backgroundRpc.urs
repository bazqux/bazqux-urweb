val addAction : Datatypes.bgAction -> transaction {}
val onError   : transaction {}
val flush     : transaction {}
val flushInBackground : transaction {}
val handleBGActions : list Datatypes.bgAction -> transaction {}

val getRpcErrorsCount : transaction int

val tryWithBGRpcList : a ::: Type -> (list Datatypes.bgAction -> transaction (option a)) -> transaction (option a)

val queueRpcB : a ::: Type -> (list Datatypes.bgAction -> transaction a) -> (a -> transaction {}) -> transaction {}
(* Вызов, который может быть отменен другим queueCRpcB с таким же seqNum
   (или увеличением seqNum), используется для setFeed *)
val queueCRpcB : a ::: Type -> source int -> (list Datatypes.bgAction -> transaction a) -> (a -> transaction {}) -> transaction {} -> transaction {}
(* Вызов, который может быть отменен другим queueRpcB с таким же seqNum
   (или увеличением seqNum), но не отменяет другие queueCRpcB_,
   используется для append и других запросов, зависящих от текущего фида.
 *)
val queueCRpcB_ : a ::: Type -> source int -> (list Datatypes.bgAction -> transaction a) -> (a -> transaction {}) -> transaction {} -> transaction {}
