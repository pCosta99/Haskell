{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

module HelloWorld where

import Yesod

data HelloWorld = HelloWorld

instance Yesod HelloWorld

mkYesod "HelloWorld" [parseRoutes|
  / HomeR GET
|]

getHomeR = defaultLayout [whamlet|
  <h1>Things to Do
  <a href="http://fpcomplete.com">Visit FP Complete web site
  <ol>
    <li>Eat
    <li>Sleep
    <li>Go to 1.
  <img src="file:///E:/UM/Haskell/src/WebDev/HelloWorld/sudo.jpg" alt="Sudo" width="150" height="100">
    |]

getImageR = undefined --sendFile typeJpeg "/path/to/file.jpg"

main = warp 9000 HelloWorld