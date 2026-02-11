{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserActivity@.
module ObjC.ClassKit.NSUserActivity
  ( NSUserActivity
  , IsNSUserActivity(..)
  , isClassKitDeepLink
  , contextIdentifierPath
  , isClassKitDeepLinkSelector
  , contextIdentifierPathSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ClassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns whether the user activity is a ClassKit deep link.
--
-- ObjC selector: @- isClassKitDeepLink@
isClassKitDeepLink :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
isClassKitDeepLink nsUserActivity  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserActivity (mkSelector "isClassKitDeepLink") retCULong []

-- | Returns the context identifier path you should deep link to.
--
-- For example for the context identifier path @@["swift-programming-book",@ @"chapter1"],@                your app should direct the user to /chapter1/ in /swift-programming-book./
--
-- ObjC selector: @- contextIdentifierPath@
contextIdentifierPath :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSArray)
contextIdentifierPath nsUserActivity  =
  sendMsg nsUserActivity (mkSelector "contextIdentifierPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isClassKitDeepLink@
isClassKitDeepLinkSelector :: Selector
isClassKitDeepLinkSelector = mkSelector "isClassKitDeepLink"

-- | @Selector@ for @contextIdentifierPath@
contextIdentifierPathSelector :: Selector
contextIdentifierPathSelector = mkSelector "contextIdentifierPath"

