con t :: Type

val new : Basis.id -> transaction Js.rect -> transaction t
val reset : t -> transaction {}
val html : t -> xbody

(* сброс сохраненных размеров и координат при смене масштаба или шрифта *)
val invalidate : t -> transaction {}

val append : t -> xbody -> transaction {}
val appendId : t -> Basis.id -> transaction {}
val appendChildId : t -> Basis.id -> Basis.id -> transaction {}
val newPage : t -> transaction {}
(* true если менялся scrollTop из-за invalidate *)
val scroll : t -> transaction bool
(* вызывает обработку только при смещении scrollTop более чем на reserved *)
val tryScroll : t -> transaction {}

val idPositionTop : t -> Basis.id -> transaction float
val idClientRectTop : t -> Basis.id -> transaction float
(* проверка top/bottom координат страницы относительно boundingClientRect parentDiv.
   координаты полученного id будут >= top && <= bottom *)
type check = Js.rect -> float -> float -> transaction bool
val idPositionTop' : check -> t -> Basis.id -> transaction float
val idClientRectTop' : check -> t -> Basis.id -> transaction float
val idClientRectHeight : t -> Basis.id -> transaction float
(* Учет того, что сворачивание list view item на скрытой странице
   уменьшает ее высоту.
 *)
(* сохраняет высоту перед разворачиванием *)
val saveIdHeight : t -> Basis.id -> transaction {}
(* восстанавливает высоту, если страница скрыта *)
val restoreIdHeight : t -> Basis.id -> transaction {}

val isIdOnVisiblePage : t -> Basis.id -> transaction bool
val isIdOnValidPage : t -> Basis.id -> transaction bool
(* не прячет страницу с заданным id, если она выше видимой области
   -- специально для appendRequest
 *)
val setAppendingId : t -> Basis.id -> bool -> transaction {}

(* При сворачивании/разворачивании элементов сбрасываем min-height *)
val resetPageHeight : t -> Basis.id -> transaction {}

val reserved : float
val test : transaction page
