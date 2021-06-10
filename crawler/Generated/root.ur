fun l p t = <xml><a link={p}>{[t]}</a><br/></xml>

val sitemap = Pages.infoPage "Site map" <xml><p>
  {l (Main.main None) "main"}
  {l (Main.i None) "i"}
  {l (Main.demo "") "demo"}
  {l Main.getUserIdBySession "getUserIdBySession"}

  {l (Pages.r "test_referrer") "referral"}
  {l Pages.whoami "whoami"}
  {l (Pages.login ()) "log in"}
  {l (Pages.signUp ()) "sign up"}
  {l Pages.opml "opml"}

  {l (Pages.activate_account "bad token") "activate_account"}
  {l (Pages.password_reset "bad token") "password_reset"}
  {l (Pages.change_email "bad token") "change_email"}
  {l (Pages.restore_access "bad token") "restore_access"}

  {l Pages.please_log_in_again "please_log_in_again"}
  {l Import.importOPML_ "importOPML_"}
(*   {l Import.activeImports "activeImports"} *)
  {l (Payments.order_notification Hacks.dummyPostBody) "order_notification"}
  {l (Payments.order_notification_new Hacks.dummyPostBody) "order_notification_new"}
  {l (Pages.order_completed "") "order_completed"}
  {l (Pages.check_order "") "check_order"}
  {l (Pages.add None) "add"}
  {l (Pages.renew None) "renew"}
  {l Pages.fetcher "fetcher"}
  {l Pages.media_proxy "media_proxy"}
  {l Pages.switch_to_user "switch_to_user"}



  </p></xml>

