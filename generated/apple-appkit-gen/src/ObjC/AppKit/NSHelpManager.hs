{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSHelpManager@.
module ObjC.AppKit.NSHelpManager
  ( NSHelpManager
  , IsNSHelpManager(..)
  , setContextHelp_forObject
  , removeContextHelpForObject
  , contextHelpForObject
  , showContextHelpForObject_locationHint
  , openHelpAnchor_inBook
  , findString_inBook
  , registerBooksInBundle
  , sharedHelpManager
  , contextHelpModeActive
  , setContextHelpModeActive
  , contextHelpForObjectSelector
  , contextHelpModeActiveSelector
  , findString_inBookSelector
  , openHelpAnchor_inBookSelector
  , registerBooksInBundleSelector
  , removeContextHelpForObjectSelector
  , setContextHelpModeActiveSelector
  , setContextHelp_forObjectSelector
  , sharedHelpManagerSelector
  , showContextHelpForObject_locationHintSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- setContextHelp:forObject:@
setContextHelp_forObject :: (IsNSHelpManager nsHelpManager, IsNSAttributedString attrString) => nsHelpManager -> attrString -> RawId -> IO ()
setContextHelp_forObject nsHelpManager attrString object =
  sendMessage nsHelpManager setContextHelp_forObjectSelector (toNSAttributedString attrString) object

-- | @- removeContextHelpForObject:@
removeContextHelpForObject :: IsNSHelpManager nsHelpManager => nsHelpManager -> RawId -> IO ()
removeContextHelpForObject nsHelpManager object =
  sendMessage nsHelpManager removeContextHelpForObjectSelector object

-- | @- contextHelpForObject:@
contextHelpForObject :: IsNSHelpManager nsHelpManager => nsHelpManager -> RawId -> IO (Id NSAttributedString)
contextHelpForObject nsHelpManager object =
  sendMessage nsHelpManager contextHelpForObjectSelector object

-- | @- showContextHelpForObject:locationHint:@
showContextHelpForObject_locationHint :: IsNSHelpManager nsHelpManager => nsHelpManager -> RawId -> NSPoint -> IO Bool
showContextHelpForObject_locationHint nsHelpManager object pt =
  sendMessage nsHelpManager showContextHelpForObject_locationHintSelector object pt

-- | @- openHelpAnchor:inBook:@
openHelpAnchor_inBook :: (IsNSHelpManager nsHelpManager, IsNSString anchor, IsNSString book) => nsHelpManager -> anchor -> book -> IO ()
openHelpAnchor_inBook nsHelpManager anchor book =
  sendMessage nsHelpManager openHelpAnchor_inBookSelector (toNSString anchor) (toNSString book)

-- | @- findString:inBook:@
findString_inBook :: (IsNSHelpManager nsHelpManager, IsNSString query, IsNSString book) => nsHelpManager -> query -> book -> IO ()
findString_inBook nsHelpManager query book =
  sendMessage nsHelpManager findString_inBookSelector (toNSString query) (toNSString book)

-- | @- registerBooksInBundle:@
registerBooksInBundle :: (IsNSHelpManager nsHelpManager, IsNSBundle bundle) => nsHelpManager -> bundle -> IO Bool
registerBooksInBundle nsHelpManager bundle =
  sendMessage nsHelpManager registerBooksInBundleSelector (toNSBundle bundle)

-- | @+ sharedHelpManager@
sharedHelpManager :: IO (Id NSHelpManager)
sharedHelpManager  =
  do
    cls' <- getRequiredClass "NSHelpManager"
    sendClassMessage cls' sharedHelpManagerSelector

-- | @+ contextHelpModeActive@
contextHelpModeActive :: IO Bool
contextHelpModeActive  =
  do
    cls' <- getRequiredClass "NSHelpManager"
    sendClassMessage cls' contextHelpModeActiveSelector

-- | @+ setContextHelpModeActive:@
setContextHelpModeActive :: Bool -> IO ()
setContextHelpModeActive value =
  do
    cls' <- getRequiredClass "NSHelpManager"
    sendClassMessage cls' setContextHelpModeActiveSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setContextHelp:forObject:@
setContextHelp_forObjectSelector :: Selector '[Id NSAttributedString, RawId] ()
setContextHelp_forObjectSelector = mkSelector "setContextHelp:forObject:"

-- | @Selector@ for @removeContextHelpForObject:@
removeContextHelpForObjectSelector :: Selector '[RawId] ()
removeContextHelpForObjectSelector = mkSelector "removeContextHelpForObject:"

-- | @Selector@ for @contextHelpForObject:@
contextHelpForObjectSelector :: Selector '[RawId] (Id NSAttributedString)
contextHelpForObjectSelector = mkSelector "contextHelpForObject:"

-- | @Selector@ for @showContextHelpForObject:locationHint:@
showContextHelpForObject_locationHintSelector :: Selector '[RawId, NSPoint] Bool
showContextHelpForObject_locationHintSelector = mkSelector "showContextHelpForObject:locationHint:"

-- | @Selector@ for @openHelpAnchor:inBook:@
openHelpAnchor_inBookSelector :: Selector '[Id NSString, Id NSString] ()
openHelpAnchor_inBookSelector = mkSelector "openHelpAnchor:inBook:"

-- | @Selector@ for @findString:inBook:@
findString_inBookSelector :: Selector '[Id NSString, Id NSString] ()
findString_inBookSelector = mkSelector "findString:inBook:"

-- | @Selector@ for @registerBooksInBundle:@
registerBooksInBundleSelector :: Selector '[Id NSBundle] Bool
registerBooksInBundleSelector = mkSelector "registerBooksInBundle:"

-- | @Selector@ for @sharedHelpManager@
sharedHelpManagerSelector :: Selector '[] (Id NSHelpManager)
sharedHelpManagerSelector = mkSelector "sharedHelpManager"

-- | @Selector@ for @contextHelpModeActive@
contextHelpModeActiveSelector :: Selector '[] Bool
contextHelpModeActiveSelector = mkSelector "contextHelpModeActive"

-- | @Selector@ for @setContextHelpModeActive:@
setContextHelpModeActiveSelector :: Selector '[Bool] ()
setContextHelpModeActiveSelector = mkSelector "setContextHelpModeActive:"

