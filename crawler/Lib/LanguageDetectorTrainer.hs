{-# LANGUAGE ViewPatterns, TupleSections, BangPatterns, FlexibleContexts,
             RecordWildCards, OverloadedStrings #-}
module Lib.LanguageDetectorTrainer
    (trainAll)
    where

import Control.Monad
import Control.Arrow
import System.IO.Unsafe
import Data.Maybe
import Data.Ord
import Data.List
import Data.Char
import qualified Text.Hyphenation as Hyph
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IntMap
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray
import Data.Array

import qualified Lib.BArray as BA
import qualified Lib.FastChar as FC
import Generated.DataTypes
import Generated.RiakIO
import Riak
import Preprocess (textToHyphenate)
import Lib.LanguageDetector
import Lib.Log

train f =
    HM.fromList . map (first (nGramToText . NGram)) . IntMap.toList
    . IntMap.fromListWith (+) . map ((,1) . unNGram)
    . concatMap wordTrigrams . filter f . textWords

trainLanguage l
    | null feeds = logS $ "No training data for " <> show l
    | otherwise = do
        logS $ "Training " <> show l
        t <- lt "fetch" $ T.unwords <$> mapM feedText feeds
        lt "print" $ print (T.length t, HS.size $ HS.fromList $ filter (T.all alpha) $ textWords t)
        lt "train"
            $ BL.writeFile ("./Lib/languages/" <> languageCode l <> ".new")
            $ file $ train (T.all alpha) t
        logS "done"
    where feeds = trainFeeds l
          lt = logTime directLogger
          alpha = FC.inAlphabet (alphabet l)
          file t = JSON.encode $ JSON.Object $ HM.singleton "freq" $
              JSON.Object $ HM.map (JSON.Number . fromIntegral) t

trainAll = mapM_ trainLanguage [minBound..maxBound]

trainFeeds l = case l of
    English ->
        ["https://www.keyboardmag.com/.rss/full/"
        ,"http://blog.bazqux.com/feeds/posts/default"
        ,"http://feeds.propublica.org/propublica/main"
--         ,"http://fivethirtyeight.com/features/feed/"
--         ,"https://firstlook.org/theintercept/feed/?rss"
        ,"http://highscalability.com/rss.xml"
        ,"http://techcrunch.com/feed/"
--         ,"http://feeds.feedburner.com/TheAtlanticCities"
        ,"http://feeds.feedburner.com/DigitalPhotographySchool"
        ,"http://feeds.feedburner.com/createdigitalmusic"
        ,"http://pitchfork.com/rss/reviews/best/albums/"
        ,"http://www.shamusyoung.com/twentysidedtale/?feed=rss2"
        ,"http://scienceblogs.com/feed/"
        ,"http://feeds.feedburner.com/smittenkitchen"
        ,"http://feeds.101cookbooks.com/101cookbooks/"
        ,"http://lambda-the-ultimate.org/rss.xml"
        ,"http://rss.nytimes.com/services/xml/rss/nyt/Travel.xml"
        ,"http://rss.nytimes.com/services/xml/rss/nyt/Music.xml"
        ,"http://rss.nytimes.com/services/xml/rss/nyt/Health.xml"
        ,"http://rss.nytimes.com/services/xml/rss/nyt/Arts.xml"
        ,"http://rss.nytimes.com/services/xml/rss/nyt/FashionandStyle.xml"
        ,"http://rss.nytimes.com/services/xml/rss/nyt/Education.xml"
        ,"http://feeds.feedburner.com/AtlanticFood"
        ,"http://krebsonsecurity.com/feed/"
        ,"http://www.schneier.com/blog/index2.rdf"
        ,"http://feeds.feedburner.com/colossal"
        ,"http://feeds.feedburner.com/highsnobiety/rss"
        ,"http://www.thehistoryblog.com/feed"
        ,"http://opinionator.blogs.nytimes.com/category/the-stone/feed/"
        ]
    Russian ->
        ["https://roem.ru/rss/roem-all-news.xml"
        ,"http://lenta.ru/rss"
        ,"https://meduza.io/rss/all"
        ,"http://novayagazeta.livejournal.com/data/rss"
        ,"http://lenta.ru/rss/articles"
        ,"http://www.popmech.ru/rss/"
        ,"http://www.3dnews.ru/news/rss/"
        ,"http://download-cd.livejournal.com/data/rss"
        ,"http://ftr.fivefilters.org/makefulltextfeed.php?url=http%3A%2F%2Fjournal.jazz.ru%2Ffeed%2F&max=3"
        ,"http://feeds.feedburner.com/appleinsider_ru"
        ,"http://lifehacker.ru/feed/"
        ,"http://www.f1news.ru/export/news.xml"
        ,"http://skirun.ru/feed/"
        ,"https://dtf.ru/rss/all"
        ,"http://gamer.livejournal.com/data/rss"
        ,"http://elementy.ru/rss/news"
        ,"http://scinquisitor.livejournal.com/data/rss"
        ,"http://greg-butcher.livejournal.com/data/rss"
        ,"http://ibash.org.ru/rss.xml"
        ,"http://www.anekdot.ru/rss/export20.xml"
        ,"http://feeds.feedburner.com/org/LOR"
        ,"http://www.opennet.ru/opennews/opennews_6.rss"
        ,"http://alenacpp.blogspot.com/feeds/posts/default"
        ,"http://tema.livejournal.com/data/rss"
        ,"http://ru-travel.livejournal.com/data/rss"
        ,"http://maria-kitchen.livejournal.com/data/rss"
        ,"http://mustread.livejournal.com/data/rss"
        ,"http://history-of-art.livejournal.com/data/rss"
        ]
    German ->
        ["https://netzpolitik.org/feed/"
        ,"http://feeds.feedburner.com/Bildblog"
        ,"http://www.heise.de/tp/news-atom.xml"
        ,"http://www.techstage.de/rss.xml"
        ,"http://www.tagesschau.de/xml/rss2"
        ,"http://www.spiegel.de/schlagzeilen/index.rss"
        ,"http://www.faz.net/rss/aktuell/"
        ,"http://newsfeed.zeit.de/index"
        ,"http://www.freitag.de/RSS"
        ,"http://www.nachdenkseiten.de/?feed=rss2"
        ,"http://feeds.feedburner.com/photoshopweblog"
        ,"http://rss.kicker.de/news/aktuell"
        ,"http://spielverlagerung.de/feed/"
        ,"http://feeds.4players.de/Allgemein/news/-/rss.xml"
        ,"http://feeds.feedburner.com/SCEEBlogDE"
        ,"http://newsfeed.zeit.de/wissen/index"
        ,"http://www.spektrum.de/alias/rss/spektrum-de-rss-feed/996406"
        ,"http://lamiacucina.wordpress.com/feed/"
        ,"http://www.chefkoch.de/rss/rezept-des-tages.php"
        ,"http://feeds2.feedburner.com/aktuell/feeds/rss/"
        ,"http://newsfeed.zeit.de/digital/index"
        ,"http://www.zdnet.de/feed/"
        ,"http://www.slanted.de/rss.xml"
        ]
    French ->
        ["http://rss.lemonde.fr/c/205/f/3050/index.rss"
        ,"http://feeds.feedburner.com/KorbensBlog-UpgradeYourMind"
        ,"http://feeds.feedburner.com/KoztoujoursTuMintresses"
        ,"http://seriestv.blog.lemonde.fr/feed/"
        ,"http://huet.blog.lemonde.fr/feed/"
        ,"http://www.lefigaro.fr/rss/figaro_une.xml"
        ,"http://www.mediapart.fr/articles/feed"
        ,"http://www.slate.fr/rss.xml"
        ,"http://feeds.feedburner.com/lesnumeriques/news"
        ,"http://www.commentcamarche.net/rss/"
        ,"http://feeds.feedburner.com/IphonfrBlogIphone"
        ,"http://www.legorafi.fr/feed/"
        ,"http://www.causeur.fr/feed"
        ,"http://www.laboiteverte.fr/feed/"
        ,"http://www.iphone3gsystem.fr/feed/"
        ,"http://www.jaddo.fr/feed/"
        ,"http://feeds.feedburner.com/frandroid"
        ,"http://www.maitre-eolas.fr/feed/atom"
        ,"http://www.bvoltaire.fr/feed"
        ]
    Hungarian ->
        ["http://itcafe.hu/hirfolyam/anyagok/rss.xml"
        ,"http://444.hu/feed/"
        ,"http://www.sg.hu/plain/rss.xml"
        ,"http://hvg.hu/rss"
        ,"http://www.hwsw.hu/xml/latest_news_rss.xml"
        ,"http://prohardver.hu/hirfolyam/anyagok/rss.xml"
        ,"http://magyarnarancs.hu/rss/"
        ,"http://feeds.feedburner.com/lfghu/"
        ,"https://csomakozpont.blogspot.com/feeds/posts/default"
        ,"http://androidportal.hu/feed/"
        ,"https://magyarnemzet.hu/feed/?"
        ,"http://tedeinturkey.com/feed/"
        ,"http://forbes.blog.hu/rss"
        ,"http://late-modern.blogspot.com/feeds/posts/default"]
    Ukrainian ->
        ["http://www.istpravda.com.ua/rss/"
        ,"http://tyzhden.ua/RSS/Publications/"
        ,"http://www.pravda.com.ua/rss/"
        ,"http://www.epravda.com.ua/rss/"
        ,"http://litgazeta.com.ua/feed/"
        ,"http://businessua.com/rss.xml"
        ,"http://football-ukraine.com/rss.xml"
        ,"http://yur-gazeta.com/news/rss/feed.xml"
        ,"http://aspekty.net/feed/"
        ,"https://www.infa.ua/feed/"
        ,"https://uain.press/feed/"
        ]
    Belarusian ->
        ["http://by-mova.livejournal.com/data/rss"
        ,"http://knihi.com/news.rss"
        ,"https://www.racyja.com/feed/"
        ]
--    Serbian что-то не нашел
--     Bulgarian ->
--         ["http://www.dnevnik.bg/rss/"
--         ,"http://www.capital.bg/rss/"
--         ,"http://newsmedia.bg/feed/"
--         ,"http://rss.frognews.bg/0/rss.xml"
--         ]
-- слишком мало, начинает с русским путать
    Polish ->
        ["http://www.dobreprogramy.pl/rss/rss_news.xml"
        ,"http://rss.swiatczytnikow.pl/SwiatCzytnikow"
        ,"http://www.spidersweb.pl/feed"
        ,"http://feeds2.feedburner.com/Antyweb"
        ,"http://feeds.feedburner.com/glowny-di"
--        ,"http://bash.org.pl/rss"
        ]
    Czech ->
        ["http://www.root.cz/rss/clanky/"
        ,"http://www.lupa.cz/rss/clanky/"
        ,"http://www.novinky.cz/rss/"
        ,"http://www.mesec.cz/rss/clanky/"
        ,"http://cdr.cz/cdrrss.php"
        ,"http://www.abclinuxu.cz/auto/abc.rss"
        ,"http://www.svethardware.cz/export.jsp?format=rss2"
        ,"http://www.zive.cz/rss/sc-47/default.aspx"
        ,"http://ihned.cz/?p=000000_rss"
        ,"http://androidaplikace.cz/index.php/feed/"
        ,"http://www.digizone.cz/rss/clanky/"
        ,"http://php.vrana.cz/rss.php"
        ,"http://www.serialzone.cz/rss/"
        ,"http://www.ceskenoviny.cz/sluzby/rss/index.php"
        ,"http://www.blesk.cz/rss"
        ,"http://aktualne.centrum.cz/feeds/"
        ]
    Spanish ->
        ["http://www.abc.es/rss/feeds/abc_ultima.xml"
        ,"http://ep01.epimg.net/rss/elpais/portada.xml"
        ,"http://www.elmundotoday.com/feed/"
        ,"http://www.elespanol.com/rss/"
        ,"http://www.neeo.es/feed/"
        ,"http://es.gizmodo.com/rss"
        ,"https://es.ign.com/feed.xml"
        ,"https://www.caranddriver.es/rss/index"
        ,"http://www.attac.es/feed/"
        ,"https://www.facebook.com/106023406156130" -- UNESCO en español
        ,"http://feeds.feedburner.com/arcadiespada"
        ]
    Italian ->
        ["http://www.byoblu.com/feed"
        ,"http://temi.repubblica.it/micromega-online/feed/"
        ,"http://www.ilfattoquotidiano.it/feed/"
        ,"http://www.corriere.it/rss/homepage.xml"
        ,"http://www.ilpost.it/feed/"
        ,"http://www.ispazio.net/feed"
        ,"http://feeds.blogo.it/autoblog/it"
        ,"http://feeds.feedburner.com/iphoneitalia"
        ,"http://feeds.blogo.it/tvblog/it"
        ,"http://feeds.feedburner.com/webnewsit"
        ,"http://www.androidworld.it/feed/"
        ,"http://feeds.feedburner.com/hd-blog"
        ,"http://www.corriere.it/rss/homepage.xml"
        ,"http://feeds.hwupgrade.it/rss_news.xml"
        ,"http://feeds.feedburner.com/mantellini/feed"
        ,"http://feeds.feedburner.com/minimarketingit"
        ]
    _ ->
        []
--        error $ "No train data for " <> show l

feedText url = do
    Posts {..} <- readPosts' url
    T.unwords . concatMap t . catMaybes <$> join (forkReadPar2 readManyMsgs
        [MsgKey pBlogFeedUrl (Just $ mhGuid mh) Nothing
        |mh <- BA.elems $ mtHeaders pMsgTree])
    where t m = [msgSubject m, textToHyphenate $ msgText m]
