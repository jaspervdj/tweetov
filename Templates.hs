{-# LANGUAGE OverloadedStrings #-}
module Templates where

import Prelude
import Data.Monoid (mempty, mappend)

import Data.Text (Text)
import Snap.Types (Snap, modifyResponse, addHeader, writeLBS)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

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
rootTemplate :: Html
rootTemplate = docTypeHtml $ do
    H.head $ do
        H.title "Tweetov: Markov chain tweets"
        script ! type_ "text/javascript" ! src "jquery-1.4.2.min.js"
               $ mempty
        link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
    body $ do
        H.div ! A.id "content" $ do
            h1 "Tweetov: Markov chain tweets"
            inputSection
            H.div ! A.id "tweet" $ mempty
            H.div ! A.id "user" $ mempty
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
    ! onsubmit (preEscapedStringValue
        -- Start loading on submit.
        "$('#tweet').html('Getting tweets and generating Markov chain...');\
        \$('#user').html('Getting user...');\
        \var u = $('#usernamefield').val();\
        \$.get('user/' + u, function(data) {\
        \    $('#user').html(data);\
        \});\
        \$.get('tweet/' + u, function(data) {\
        \    $('#tweet').html(data);\
        \});\
        \return false;")
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
tweetSection :: Text -> Html
tweetSection t = H.div ! A.id "tweetsection" $ linkTweet t

-- | Section containing the requested twitter user.
--
userSection :: UserInfo -> Html
userSection userInfo = H.div ! A.id "usersection" $ do
    img ! src (textValue $ userImageUrl userInfo) ! alt "User image."
    a ! href ("http://twitter.com/" `mappend` textValue (userName userInfo))
      ! A.id "username"
      $ "@" `mappend` text (userName userInfo)
    H.div ! A.id "realname" $ text $ userRealName userInfo
