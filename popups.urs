con popup :: Type

val popupId : popup -> id
val popupXbody : popup -> xbody

con position = {Top : int, Left : int}

(* source, с которым toggle работает по умолчанию. необходимо добавить на страницу *)
val popupSource : source xbody
(* вставляем popupSource в html *)
val embed : source xbody -> xbody

val isVisible : popup -> transaction bool
val isActive : transaction bool
val hide : transaction {}

val isSubPopupActive : transaction bool
val hideSubPopup : transaction {}

val toggleExternalPopup : source bool -> transaction {}

(* toggle' popupSource stopPropagation
   stopPropagation нужен в обработчиках событий, чтобы не закрыть по клику
   только что открытый popup
 *)
val toggle' : source xbody -> bool -> popup -> transaction {}
val toggle : popup -> transaction {}

(* небольшие диалоговые окна *)
val newBox : string -> xbody -> transaction popup
(* небольшие диалоговые окна с дополнительным классом *)
val newBoxC : css_class -> string -> xbody -> transaction popup
(* большие диалоги с заголовком и вертикальной прокруткой: Help, Buy Now *)
val newBigBox : string -> xbody -> transaction popup
(* диалог с вложенным диалогом *)
val newBigBoxWithSubPopup :
    string -> ((popup -> transaction {}) -> xbody) -> transaction popup
(* все меню *)
val newMenu : css_class -> string -> xbody -> transaction popup
val newIdPosMenu : id -> string -> Either.either position position -> css_class -> string -> xbody -> popup

val currentMousePosition : transaction position
val showContextMenu : option (Either.either position position) -> xbody -> transaction {}
val toggleContextMenu : Basis.id -> option (Either.either position position) -> xbody -> transaction {}

(* menu item with action *)
val li : string -> transaction {} -> xbody
(* menu item with link *)
val lli : string -> url -> xbody

(* content indented like it has icon *)
val indentedLine : xbody -> xbody
(* [img] menu item with action *)
val lii : css_class -> string -> transaction {} -> xbody
(* [favicon] menu item with action *)
val lif : url -> string -> transaction {} -> xbody
(* [favicon] menu item with link and pre-link action *)
val llif : transaction {} -> string -> url -> xbody
(* [img] menu item with link *)
val llii : css_class -> string -> url -> xbody
(* TODO: как-то более системно назвать *)

(* sub menu > *)
val liSub : string -> xbody -> xbody
(* [img] sub menu > *)
val liSubI : css_class -> string -> xbody -> xbody

val setup : transaction {}
val mouseMovedAfterMouseDown : transaction bool

val menuLabel : string -> xbody
