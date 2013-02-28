-- | Contains the blaze-html templates for the site
--
{-# LANGUAGE OverloadedStrings #-}
module Tweetov.Views
    ( root
    , tweet
    , user
    , userAjax
    ) where

import Prelude
import Data.Monoid (mempty, mappend)

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text (Text)

import Tweetov.Twitter
import Tweetov.Twitter.Html (viewTweet)

-- | The root page of the web app.
--
root :: Html  -- ^ Tweet section contents
             -> Html  -- ^ User section contents
             -> Html
root tweet' user' = docTypeHtml $ do
    H.head $ do
        H.title "Tweetov: Markov chain tweets"
        script ! type_ "text/javascript" ! src "jquery-1.4.2.min.js" $ mempty
        script ! type_ "text/javascript" ! src "json2.js" $ mempty
        script ! type_ "text/javascript" ! src "tweetov.js" $ mempty
        link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
    body $ do
        H.div ! A.id "content" $ do
            h1 "Tweetov: Markov chain tweets"
            Tweetov.Views.input
            H.div ! A.id "tweet" $ tweet'
            H.div ! A.id "user"  $ user'
            H.div ! A.style "clear: both;" $ mempty
        H.div ! A.id "footer" $ do
            "Tweets of insanity by "
            a ! href "http://twitter.com/jaspervdj" $ "@jaspervdj"
            " ("
            a ! href "http://github.com/jaspervdj/tweetov" $ "source code"
            ")"

-- | The input section of the root page.
--
input :: Html
input = H.div ! A.id "inputsection" $ H.form
    ! onsubmit "return submit_username();"
    $ do H.input ! type_ "text" ! name "usernamefield"
                 ! A.id "usernamefield" ! value "@DalaiLama"
                 -- Clear field on click.
                 ! onfocus (preEscapedToValue (
                     "if(this.value == this.defaultValue) {\
                     \    $(this).val('');\
                     \};" :: String))
         H.input ! type_ "submit"
                 ! A.id "submitfield"
                 ! value "Submit"

-- | Section containing a tweet.
--
tweet :: Tweet -> Html
tweet tweet' = H.div ! A.id "tweetsection" $
    H.div ! A.id "tweet" $ viewTweet tweet'

-- | Section containing the requested twitter user.
--
user :: User -> Html
user user' = H.div ! A.id "usersection" $ do
    img ! src (toValue $ userImageUrl user') ! alt "User image."
    a ! href ("http://twitter.com/" `mappend` toValue (userName user'))
      ! A.id "username"
      $ "@" `mappend` toHtml (userName user')
    H.div ! A.id "realname" $ toHtml $ userRealName user'

-- | Produces a script to set the user later
--
userAjax :: Text -> Html
userAjax user' = script ! type_ "text/javascript" $
    "set_user('" >> toHtml user' >> "');"
