{-# LANGUAGE DataKinds #-}
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
  , actionSelector
  , setActionSelector
  , setTagSelector
  , setTargetSelector
  , tagSelector
  , targetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- target@
target :: IsNSActionCell nsActionCell => nsActionCell -> IO RawId
target nsActionCell =
  sendMessage nsActionCell targetSelector

-- | @- setTarget:@
setTarget :: IsNSActionCell nsActionCell => nsActionCell -> RawId -> IO ()
setTarget nsActionCell value =
  sendMessage nsActionCell setTargetSelector value

-- | @- action@
action :: IsNSActionCell nsActionCell => nsActionCell -> IO Sel
action nsActionCell =
  sendMessage nsActionCell actionSelector

-- | @- setAction:@
setAction :: IsNSActionCell nsActionCell => nsActionCell -> Sel -> IO ()
setAction nsActionCell value =
  sendMessage nsActionCell setActionSelector value

-- | @- tag@
tag :: IsNSActionCell nsActionCell => nsActionCell -> IO CLong
tag nsActionCell =
  sendMessage nsActionCell tagSelector

-- | @- setTag:@
setTag :: IsNSActionCell nsActionCell => nsActionCell -> CLong -> IO ()
setTag nsActionCell value =
  sendMessage nsActionCell setTagSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @target@
targetSelector :: Selector '[] RawId
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[RawId] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] Sel
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Sel] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @tag@
tagSelector :: Selector '[] CLong
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector '[CLong] ()
setTagSelector = mkSelector "setTag:"

