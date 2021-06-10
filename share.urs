val shareMenuContents : Uim.uim -> url -> transaction xbody
val toggleShareMenu : Uim.uim -> transaction {}
val preloadShareIcons : transaction {}
val mailLink : Uim.uim -> url -> transaction {}
val translate : (Uim.uim -> url -> transaction {}) -> Uim.uim -> transaction {}
