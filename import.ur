open Session
open Utils

fun addSubscriptions src f : transaction page =
    Rpcs.withUser src (fn userId =>
    f userId;
    redirectToMain) []

fun importOPML r : transaction page =
    addSubscriptions "importOPML" (H.opmlSubscriptions (fileData r.OPML))

val importOPML_ : transaction page =
    page "Import OPML" <xml>
      <form>
        <p>Upload OPML<br/>
          <upload{#OPML}/><br/>
          <submit action={importOPML}/>
        </p>
      </form>
    </xml>
(*
fun importingFromGoogleReader (qs : option queryString) : transaction page =
    case qs of
        None => error <xml>Empty query string for import callback</xml>
      | Some qs =>
        h <- getHost;
        addSubscriptions "importingFromGoogleReader"
                         (importFromGoogleReaderCallback h
                              (show (effectfulUrl importingFromGoogleReader))
                              (show qs))

fun importFromGoogleReader (_ : option queryString) : transaction page =
    h <- getHost;
    u <- importFromGoogleReaderGetForwardUrl h
             (effectfulUrl importingFromGoogleReader);
    redirect u

fun importingStarredAndTaggedItemsFromGoogleReader (qs : option queryString) : transaction page =
    case qs of
        None => error <xml>Empty query string for import callback</xml>
      | Some qs =>
        h <- getHost;
        withUser "importingStarredAndTaggedItemsFromGoogleReader" (fn userId =>
        importStarredAndTaggedItemsFromGoogleReaderCallback h
            (show (effectfulUrl importingStarredAndTaggedItemsFromGoogleReader))
            (show qs) userId;
        redirect (bless "/i/starred")) []

fun importStarredAndTaggedItemsFromGoogleReader (_ : option queryString) : transaction page =
    h <- getHost;
    u <- importFromGoogleReaderGetForwardUrl h
             (effectfulUrl importingStarredAndTaggedItemsFromGoogleReader);
    redirect u

val activeImports : transaction page =
    c <- activeGRImportNames;
    page1 "Active imports" <xml>
      <p>Active imports:
        <div class="errorText">{c}</div></p>
    </xml>
*)
