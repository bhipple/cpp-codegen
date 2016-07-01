{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Gencpp ( genCppHPair ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import System.Environment (getArgs)
import Turtle
import Text.RawString.QQ
import Text.StringTemplate
import Cases (camelize)
import Data.Char (toUpper)

--  ===========================================================================
--                              Templates
--  ===========================================================================
cppTmpl :: StringTemplate Text
cppTmpl = newSTMP [r|$firstline$
#include <$filename$.h>

// Application Includes
// BDE Includes
// System Includes

namespace BloombergLP {
namespace $namespace$ {
namespace {
const char LOG_CATEGORY[] = "$logCategory$";
}

}  // close namespace $namespace$
}  // close enterprise namespace
|]


headerTmpl :: StringTemplate Text
headerTmpl = newSTMP [r|$firstline$
#ifndef $incGuard$
#define $incGuard$

// Application Includes
// BDE Includes
// System Includes

namespace BloombergLP {
namespace $namespace$ {
class $cname$ {
  public:
    //$cname$() { }
    //$cname$(const $cname$&);
    //$cname$& operator=(const $cname$&);

    //~$cname$() { }

  private:

};

}  // close namespace $namespace$
}  // close enterprise namespace

#endif
|]

--  ===========================================================================
--                             Generation
--  ===========================================================================
genCppHPair :: Text -> FilePath -> IO ()
genCppHPair ns fn = do
    let bn = basename fn
    let cpp = setManyAttrib [ ("namespace", ns)
                             , ("firstline", firstLine (bn <.> "cpp"))
                             , ("filename", format fp bn)
                             , ("logCategory", T.toUpper $ getClassName fn)
                             ] cppTmpl

    let h = setManyAttrib [ ("namespace", ns)
                          , ("firstline", firstLine (bn <.> "h"))
                          , ("incGuard", "INCLUDED_" <> (T.toUpper . format fp . basename $ fn))
                          , ("cname", getClassName fn)
                          ] headerTmpl

    let write name tmpl = output name $ pure $ render tmpl
    write (fn <.> "cpp") cpp
    write (fn <.> "h") h

-- Generates the // foo.cpp -*-C++-*- tags
firstLine :: FilePath -> Text
firstLine fn = let pref = "// "
                   fn' = format fp $ basename fn
                   tag = "-*-C++-*-"
                   ws = T.replicate (79 - T.length pref - T.length fn' - T.length tag) " "
               in pref <> fn' <> ws <> tag

getClassName :: FilePath -> Text
getClassName fn = let cc = camelize . format fp . basename $ fn
                  in T.cons (toUpper . T.head $ cc) $ T.tail cc
