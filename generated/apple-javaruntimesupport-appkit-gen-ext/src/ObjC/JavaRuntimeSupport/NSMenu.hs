{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMenu@.
module ObjC.JavaRuntimeSupport.NSMenu
  ( NSMenu
  , IsNSMenu(..)
  , javaMenuWithTitle
  , setJavaMenuDelegate
  , isJavaMenu
  , isJavaMenuSelector
  , javaMenuWithTitleSelector
  , setJavaMenuDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ javaMenuWithTitle:@
javaMenuWithTitle :: IsNSString title => title -> IO (Id NSMenu)
javaMenuWithTitle title =
  do
    cls' <- getRequiredClass "NSMenu"
    sendClassMessage cls' javaMenuWithTitleSelector (toNSString title)

-- | @- setJavaMenuDelegate:@
setJavaMenuDelegate :: IsNSMenu nsMenu => nsMenu -> RawId -> IO ()
setJavaMenuDelegate nsMenu delegate =
  sendMessage nsMenu setJavaMenuDelegateSelector delegate

-- | @- isJavaMenu@
isJavaMenu :: IsNSMenu nsMenu => nsMenu -> IO Bool
isJavaMenu nsMenu =
  sendMessage nsMenu isJavaMenuSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @javaMenuWithTitle:@
javaMenuWithTitleSelector :: Selector '[Id NSString] (Id NSMenu)
javaMenuWithTitleSelector = mkSelector "javaMenuWithTitle:"

-- | @Selector@ for @setJavaMenuDelegate:@
setJavaMenuDelegateSelector :: Selector '[RawId] ()
setJavaMenuDelegateSelector = mkSelector "setJavaMenuDelegate:"

-- | @Selector@ for @isJavaMenu@
isJavaMenuSelector :: Selector '[] Bool
isJavaMenuSelector = mkSelector "isJavaMenu"

