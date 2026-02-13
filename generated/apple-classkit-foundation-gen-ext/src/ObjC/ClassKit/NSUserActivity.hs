{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserActivity@.
module ObjC.ClassKit.NSUserActivity
  ( NSUserActivity
  , IsNSUserActivity(..)
  , isClassKitDeepLink
  , contextIdentifierPath
  , contextIdentifierPathSelector
  , isClassKitDeepLinkSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ClassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns whether the user activity is a ClassKit deep link.
--
-- ObjC selector: @- isClassKitDeepLink@
isClassKitDeepLink :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
isClassKitDeepLink nsUserActivity =
  sendMessage nsUserActivity isClassKitDeepLinkSelector

-- | Returns the context identifier path you should deep link to.
--
-- For example for the context identifier path @@["swift-programming-book",@ @"chapter1"],@                your app should direct the user to /chapter1/ in /swift-programming-book./
--
-- ObjC selector: @- contextIdentifierPath@
contextIdentifierPath :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSArray)
contextIdentifierPath nsUserActivity =
  sendMessage nsUserActivity contextIdentifierPathSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isClassKitDeepLink@
isClassKitDeepLinkSelector :: Selector '[] Bool
isClassKitDeepLinkSelector = mkSelector "isClassKitDeepLink"

-- | @Selector@ for @contextIdentifierPath@
contextIdentifierPathSelector :: Selector '[] (Id NSArray)
contextIdentifierPathSelector = mkSelector "contextIdentifierPath"

