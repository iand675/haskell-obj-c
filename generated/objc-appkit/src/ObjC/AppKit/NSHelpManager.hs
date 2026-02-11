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
  , setContextHelp_forObjectSelector
  , removeContextHelpForObjectSelector
  , contextHelpForObjectSelector
  , showContextHelpForObject_locationHintSelector
  , openHelpAnchor_inBookSelector
  , findString_inBookSelector
  , registerBooksInBundleSelector
  , sharedHelpManagerSelector
  , contextHelpModeActiveSelector
  , setContextHelpModeActiveSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- setContextHelp:forObject:@
setContextHelp_forObject :: (IsNSHelpManager nsHelpManager, IsNSAttributedString attrString) => nsHelpManager -> attrString -> RawId -> IO ()
setContextHelp_forObject nsHelpManager  attrString object =
withObjCPtr attrString $ \raw_attrString ->
    sendMsg nsHelpManager (mkSelector "setContextHelp:forObject:") retVoid [argPtr (castPtr raw_attrString :: Ptr ()), argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- removeContextHelpForObject:@
removeContextHelpForObject :: IsNSHelpManager nsHelpManager => nsHelpManager -> RawId -> IO ()
removeContextHelpForObject nsHelpManager  object =
  sendMsg nsHelpManager (mkSelector "removeContextHelpForObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- contextHelpForObject:@
contextHelpForObject :: IsNSHelpManager nsHelpManager => nsHelpManager -> RawId -> IO (Id NSAttributedString)
contextHelpForObject nsHelpManager  object =
  sendMsg nsHelpManager (mkSelector "contextHelpForObject:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ())] >>= retainedObject . castPtr

-- | @- showContextHelpForObject:locationHint:@
showContextHelpForObject_locationHint :: IsNSHelpManager nsHelpManager => nsHelpManager -> RawId -> NSPoint -> IO Bool
showContextHelpForObject_locationHint nsHelpManager  object pt =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsHelpManager (mkSelector "showContextHelpForObject:locationHint:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ()), argNSPoint pt]

-- | @- openHelpAnchor:inBook:@
openHelpAnchor_inBook :: (IsNSHelpManager nsHelpManager, IsNSString anchor, IsNSString book) => nsHelpManager -> anchor -> book -> IO ()
openHelpAnchor_inBook nsHelpManager  anchor book =
withObjCPtr anchor $ \raw_anchor ->
  withObjCPtr book $ \raw_book ->
      sendMsg nsHelpManager (mkSelector "openHelpAnchor:inBook:") retVoid [argPtr (castPtr raw_anchor :: Ptr ()), argPtr (castPtr raw_book :: Ptr ())]

-- | @- findString:inBook:@
findString_inBook :: (IsNSHelpManager nsHelpManager, IsNSString query, IsNSString book) => nsHelpManager -> query -> book -> IO ()
findString_inBook nsHelpManager  query book =
withObjCPtr query $ \raw_query ->
  withObjCPtr book $ \raw_book ->
      sendMsg nsHelpManager (mkSelector "findString:inBook:") retVoid [argPtr (castPtr raw_query :: Ptr ()), argPtr (castPtr raw_book :: Ptr ())]

-- | @- registerBooksInBundle:@
registerBooksInBundle :: (IsNSHelpManager nsHelpManager, IsNSBundle bundle) => nsHelpManager -> bundle -> IO Bool
registerBooksInBundle nsHelpManager  bundle =
withObjCPtr bundle $ \raw_bundle ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsHelpManager (mkSelector "registerBooksInBundle:") retCULong [argPtr (castPtr raw_bundle :: Ptr ())]

-- | @+ sharedHelpManager@
sharedHelpManager :: IO (Id NSHelpManager)
sharedHelpManager  =
  do
    cls' <- getRequiredClass "NSHelpManager"
    sendClassMsg cls' (mkSelector "sharedHelpManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ contextHelpModeActive@
contextHelpModeActive :: IO Bool
contextHelpModeActive  =
  do
    cls' <- getRequiredClass "NSHelpManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "contextHelpModeActive") retCULong []

-- | @+ setContextHelpModeActive:@
setContextHelpModeActive :: Bool -> IO ()
setContextHelpModeActive value =
  do
    cls' <- getRequiredClass "NSHelpManager"
    sendClassMsg cls' (mkSelector "setContextHelpModeActive:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setContextHelp:forObject:@
setContextHelp_forObjectSelector :: Selector
setContextHelp_forObjectSelector = mkSelector "setContextHelp:forObject:"

-- | @Selector@ for @removeContextHelpForObject:@
removeContextHelpForObjectSelector :: Selector
removeContextHelpForObjectSelector = mkSelector "removeContextHelpForObject:"

-- | @Selector@ for @contextHelpForObject:@
contextHelpForObjectSelector :: Selector
contextHelpForObjectSelector = mkSelector "contextHelpForObject:"

-- | @Selector@ for @showContextHelpForObject:locationHint:@
showContextHelpForObject_locationHintSelector :: Selector
showContextHelpForObject_locationHintSelector = mkSelector "showContextHelpForObject:locationHint:"

-- | @Selector@ for @openHelpAnchor:inBook:@
openHelpAnchor_inBookSelector :: Selector
openHelpAnchor_inBookSelector = mkSelector "openHelpAnchor:inBook:"

-- | @Selector@ for @findString:inBook:@
findString_inBookSelector :: Selector
findString_inBookSelector = mkSelector "findString:inBook:"

-- | @Selector@ for @registerBooksInBundle:@
registerBooksInBundleSelector :: Selector
registerBooksInBundleSelector = mkSelector "registerBooksInBundle:"

-- | @Selector@ for @sharedHelpManager@
sharedHelpManagerSelector :: Selector
sharedHelpManagerSelector = mkSelector "sharedHelpManager"

-- | @Selector@ for @contextHelpModeActive@
contextHelpModeActiveSelector :: Selector
contextHelpModeActiveSelector = mkSelector "contextHelpModeActive"

-- | @Selector@ for @setContextHelpModeActive:@
setContextHelpModeActiveSelector :: Selector
setContextHelpModeActiveSelector = mkSelector "setContextHelpModeActive:"

