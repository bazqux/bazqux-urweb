open Datatypes
open Utils

(* Очередь RPC, не позволяет запускать несколько RPC одновременно.
   Позволяет запускать rpc, которые могут отменить друг-друга
   (изпользуется для UI запросов, когда смена просматриваемой подписки
    отменяет запрос дерева сообщений предыдущей подписки, или же
    отменяет его отображение, если запрос уже отправлен серверу,
    для этого в queueRpc два параметра)
 *)

val rpcRunning = Unsafe.boolSource "BackgroundRpc.rpcRunning" False
val rpcQueue = Unsafe.transactionListSource "BackgroundRpc.rpcQueue" []
val rpcFlush = Unsafe.optionTransactionSource "BackgroundRpc.rpcFlush" None

fun loopQueue () =
    q <- get rpcQueue;
    if notNull q then
        set rpcQueue [];
        sequence_ (List.rev q);
        loopQueue ()
    else
        f <- get rpcFlush;
        (* flush запускаем только если нет других RPC, чтобы не делать
           лишних /bgactions (RPC и так передают список bgactions)
         *)
        case f of
          | None => return ()
          | Some fa =>
            set rpcFlush None;
            fa;
            loopQueue ()

val runRpcs =
    r <- get rpcRunning;
    if r then
        return ()
    else
       (set rpcRunning True;
        loopQueue ();
        set rpcRunning False)

fun queueRpc [a] (tr : transaction a) (f : a -> transaction {}) =
    modify rpcQueue (cons (x <- tr; f x));
    runRpcs

(* Запуск отменяемого RPC *)
fun queueCRpc [a] (seqNum : source int) (s:int -> int) (tr : transaction a) (f : a -> transaction {}) (onCancel : transaction {}) =
    modify seqNum s;
    num <- get seqNum;
    modify rpcQueue (cons (n <- get seqNum;
                           if n <> num then
                               onCancel
                           else
                               x <- tr;
                               n <- get seqNum;
                               (* после выполнения rpc также проверяем seqNum *)
                               if n <> num then
                                   onCancel
                               else
                                   f x));
    runRpcs

val rpcErrorsCount = Unsafe.intSource "rpcErrorsCount" 0
val getRpcErrorsCount = get rpcErrorsCount

val msgKeyEq : eq msgKey =
    mkEq (fn a b => a.BlogFeedUrl = b.BlogFeedUrl
                    && a.PostGuid = b.PostGuid
                    && a.CommentGuid = b.CommentGuid)
val msgIdEq : eq msgId =
    mkEq (fn a b => a.FeedId = b.FeedId
                    && a.PostId = b.PostId && a.CommentId = b.CommentId)
(* убирает повторные отмечания одного и того же сообщения прочитанным
   и обращает список
 *)
fun preprocessBGActions l =
    let fun go marks acc l =
            case l of
                [] => acc
              | x :: xs =>
                (case x of
                   | BGMarkMsgRead { MsgId = key, ... } =>
                     if elem key marks then go marks acc xs
                     (* уже есть, оставляем последний *)
                     else go (key :: marks) (x :: acc) xs
                   | _ => List.revAppend xs (x :: acc)
                          (* ничего больше не трогаем, т.к. сложно
                             проанализировать, что можно подчистить, а что нет
                           *)
                )
    in
        go [] [] l
    end

fun handleBGActions (l : list bgAction) =
    rpc (Rpcs.bgactions l)

(* RPC, выполняемые в фоне, через определенное время (5сек) одним махом.
   queueRpcB, queueCRpcB -- запускают rpc как только освободится очередь,
   дополнительно передавая текущий накопленный список фоновый действий
   preprocess получает список в обратном порядке,
   handle -- результат preprocess *)

val num = Unsafe.intSource "BackgroundRpc.num" 0
val list = Unsafe.bgActionListSource "BackgroundRpc.list" []
val activeList = Unsafe.bgActionListSource "BackgroundRpc.activeList" []
val timeoutActive = Unsafe.boolSource "BackgroundRpc.timeoutActive" False

val getList =
    l <- get list;
    al <- get activeList;
    let val r = List.append l al
    in
        set list [];
        set activeList r;
        return (List.mp fst r, preprocessBGActions (List.mp snd r))
    end
fun endGetList r =
    set rpcErrorsCount 0;
    modify activeList (List.filter (fn (n,_) => not (elem n r)))
    (* удаляем только те, что были обработаны заданным rpc *)

fun whenHaveNewActions act =
    l <- get list;
    when (notNull l) act

val flush =
    whenHaveNewActions
    (l <- getList; when (notNull l.2) (handleBGActions l.2); endGetList l.1)

fun setTimeout () =
    a <- get timeoutActive;
    when (not a)
         (set timeoutActive True;
          ec <- get rpcErrorsCount;
          Js.setTimeout "bgActions"
                        (set timeoutActive False;
                         process ())
                        (min 60000 ((if Js.isMobile () then 2000 else 5000)
                                    + 5000*ec)))
and process () =
    set rpcFlush (Some (flush; whenHaveNewActions (setTimeout ())));
    runRpcs

val flushInBackground = Js.setTimeout "flushInBackground" (process ()) 0

val onError =
    set rpcRunning False;
    al <- get activeList;
    set activeList [];
    modify list (fn l => List.append l al);
    modify rpcErrorsCount succ;
    set rpcQueue [];
    setTimeout ()

fun addAction (a : bgAction) =
    n <- get num;
    set num (n+1);
    modify list (cons (n,a));
    setTimeout ()

fun withBGRpcList [a] (f : list bgAction -> transaction a) =
    l <- getList; r <- f l.2; endGetList l.1; return r
fun tryWithBGRpcList [a] (f : list bgAction -> transaction (option a)) =
    l <- getList; r <- f l.2;
    (if Option.isSome r then
         endGetList l.1
         (* чистим список только при успешном tryRpc *)
     else
         modify rpcErrorsCount succ);
    return r

fun queueRpcB [a] (tr : list bgAction -> transaction a) (f : a -> transaction {}) =
    queueRpc (withBGRpcList tr) f

fun queueCRpcB [a] seqNum (tr : list bgAction -> transaction a) (f : a -> transaction {}) onCancel =
    queueCRpc seqNum succ (withBGRpcList tr) f onCancel

fun queueCRpcB_ [a] seqNum (tr : list bgAction -> transaction a) (f : a -> transaction {}) onCancel =
    queueCRpc seqNum id (withBGRpcList tr) f onCancel
