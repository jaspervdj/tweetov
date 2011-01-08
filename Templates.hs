{-# LANGUAGE OverloadedStrings #-}
module Templates where

import Prelude
import Data.Monoid (mempty, mappend)

import Snap.Types (Snap, modifyResponse, addHeader, writeLBS)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text (Text)

import Twitter
import Twitter.Html (linkTweet)

-- | Send blaze output to snap.
--
setBlaze :: Html -> Snap ()
setBlaze response = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    addBlaze response

-- | Send blaze output to snap, assuming 'setBlaze' has already been called.
--
addBlaze :: Html -> Snap ()
addBlaze = writeLBS . renderHtml

-- | The root page of the web app.
--
rootTemplate :: Html  -- ^ Tweet section contents
             -> Html  -- ^ User section contents
             -> Html
rootTemplate tweetSection' userSection' = docTypeHtml $ do
    H.head $ do
        H.title "Tweetov: Markov chain tweets"
        script ! type_ "text/javascript" ! src "jquery-1.4.2.min.js"
               $ mempty
        script ! type_ "text/javascript" ! src "jquery.json-2.2.min.js"
               $ mempty
        script ! type_ "text/javascript" ! src "tweetov.js" $ mempty
        link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
    body $ do
        H.div ! A.id "content" $ do
            h1 "Tweetov: Markov chain tweets"
            inputSection
            H.div ! A.id "tweet" $ tweetSection'
            H.div ! A.id "user" $ userSection'
            H.div ! A.style "clear: both;" $ mempty
        H.div ! A.id "footer" $ do
            "Tweets of insanity by "
            a ! href "http://jaspervdj.be" $ "jaspervdj"
            " ("
            a ! href "http://github.com/jaspervdj/tweetov" $ "source code"
            ")"

-- | The input section of the root page.
--
inputSection :: Html
inputSection = H.div ! A.id "inputsection" $ H.form
    ! onsubmit "return submit_username();"
    $ do input ! type_ "text" ! name "usernamefield"
               ! A.id "usernamefield" ! value "username"
               -- Clear field on click.
               ! onfocus (preEscapedStringValue
                     "if(this.value == this.defaultValue) {\
                     \    $(this).val('');\
                     \};")
         input ! type_ "submit"
               ! A.id "submitfield"
               ! value "Submit"

-- | Section containing a tweet.
--
tweetSection :: TweetInfo -> Integer -> Html
tweetSection tweet id' = H.div ! A.id "tweetsection" $ do
    H.div ! A.id "tweet" $ linkTweet tweet
    H.div ! A.id "tweetlink" $
        a ! href (stringValue $ show id') $ "link (expires)"

-- | Produces a script to set the user
--
setUser :: Text -> Html
setUser user = script ! type_ "text/javascript" $
    "set_user('" >> text user >> "');"

-- | Section containing the requested twitter user.
--
userSection :: UserInfo -> Html
userSection userInfo = H.div ! A.id "usersection" $ do
    img ! src (textValue $ userImageUrl userInfo) ! alt "User image."
    a ! href ("http://twitter.com/" `mappend` textValue (userName userInfo))
      ! A.id "username"
      $ "@" `mappend` text (userName userInfo)
    H.div ! A.id "realname" $ text $ userRealName userInfo
