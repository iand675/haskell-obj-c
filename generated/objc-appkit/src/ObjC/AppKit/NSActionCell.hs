{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSActionCell@.
module ObjC.AppKit.NSActionCell
  ( NSActionCell
  , IsNSActionCell(..)
  , target
  , setTarget
  , action
  , setAction
  , tag
  , setTag
  , targetSelector
  , setTargetSelector
  , actionSelector
  , setActionSelector
  , tagSelector
  , setTagSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- target@
target :: IsNSActionCell nsActionCell => nsActionCell -> IO RawId
target nsActionCell  =
  fmap (RawId . castPtr) $ sendMsg nsActionCell (mkSelector "target") (retPtr retVoid) []

-- | @- setTarget:@
setTarget :: IsNSActionCell nsActionCell => nsActionCell -> RawId -> IO ()
setTarget nsActionCell  value =
  sendMsg nsActionCell (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- action@
action :: IsNSActionCell nsActionCell => nsActionCell -> IO Selector
action nsActionCell  =
  fmap (Selector . castPtr) $ sendMsg nsActionCell (mkSelector "action") (retPtr retVoid) []

-- | @- setAction:@
setAction :: IsNSActionCell nsActionCell => nsActionCell -> Selector -> IO ()
setAction nsActionCell  value =
  sendMsg nsActionCell (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | @- tag@
tag :: IsNSActionCell nsActionCell => nsActionCell -> IO CLong
tag nsActionCell  =
  sendMsg nsActionCell (mkSelector "tag") retCLong []

-- | @- setTag:@
setTag :: IsNSActionCell nsActionCell => nsActionCell -> CLong -> IO ()
setTag nsActionCell  value =
  sendMsg nsActionCell (mkSelector "setTag:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @tag@
tagSelector :: Selector
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector
setTagSelector = mkSelector "setTag:"

