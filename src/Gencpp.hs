{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Gencpp ( genCppHPair ) where

import Data.Text hiding (head, drop)
import Prelude hiding (FilePath)
import System.Environment (getArgs)
import Turtle
import Text.RawString.QQ
import Text.StringTemplate

--  ===========================================================================
--                              Templates
--  ===========================================================================
cppTmpl :: StringTemplate String
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


headerTmpl :: StringTemplate String
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
    let cpp = cppTmpl
    let h = headerTmpl
    let cpp' = setManyAttrib [ ("namespace", ns)
                             , ("firstline", "WIP-FL")
                             , ("filename", "WIP-FN")
                             , ("logCategory", "WIP-LC")
                             ] cpp

    let h' = setManyAttrib [ ("namespace", ns)
                           , ("firstline", "WIP-FL")
                           , ("incGuard", "WIP-IG")
                           , ("cname", "WIP-CN")
                           ] h

    print $ render cpp'
    print $ render h'

-- Generates the // foo.cpp -*-C++-*- tags
firstLine :: FilePath -> Text
firstLine = undefined
