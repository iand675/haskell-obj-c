{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDockTile@.
module ObjC.AppKit.NSDockTile
  ( NSDockTile
  , IsNSDockTile(..)
  , display
  , size
  , contentView
  , setContentView
  , showsApplicationBadge
  , setShowsApplicationBadge
  , badgeLabel
  , setBadgeLabel
  , owner
  , displaySelector
  , sizeSelector
  , contentViewSelector
  , setContentViewSelector
  , showsApplicationBadgeSelector
  , setShowsApplicationBadgeSelector
  , badgeLabelSelector
  , setBadgeLabelSelector
  , ownerSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- display@
display :: IsNSDockTile nsDockTile => nsDockTile -> IO ()
display nsDockTile  =
  sendMsg nsDockTile (mkSelector "display") retVoid []

-- | @- size@
size :: IsNSDockTile nsDockTile => nsDockTile -> IO NSSize
size nsDockTile  =
  sendMsgStret nsDockTile (mkSelector "size") retNSSize []

-- | @- contentView@
contentView :: IsNSDockTile nsDockTile => nsDockTile -> IO (Id NSView)
contentView nsDockTile  =
  sendMsg nsDockTile (mkSelector "contentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentView:@
setContentView :: (IsNSDockTile nsDockTile, IsNSView value) => nsDockTile -> value -> IO ()
setContentView nsDockTile  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDockTile (mkSelector "setContentView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- showsApplicationBadge@
showsApplicationBadge :: IsNSDockTile nsDockTile => nsDockTile -> IO Bool
showsApplicationBadge nsDockTile  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDockTile (mkSelector "showsApplicationBadge") retCULong []

-- | @- setShowsApplicationBadge:@
setShowsApplicationBadge :: IsNSDockTile nsDockTile => nsDockTile -> Bool -> IO ()
setShowsApplicationBadge nsDockTile  value =
  sendMsg nsDockTile (mkSelector "setShowsApplicationBadge:") retVoid [argCULong (if value then 1 else 0)]

-- | @- badgeLabel@
badgeLabel :: IsNSDockTile nsDockTile => nsDockTile -> IO (Id NSString)
badgeLabel nsDockTile  =
  sendMsg nsDockTile (mkSelector "badgeLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBadgeLabel:@
setBadgeLabel :: (IsNSDockTile nsDockTile, IsNSString value) => nsDockTile -> value -> IO ()
setBadgeLabel nsDockTile  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDockTile (mkSelector "setBadgeLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- owner@
owner :: IsNSDockTile nsDockTile => nsDockTile -> IO RawId
owner nsDockTile  =
  fmap (RawId . castPtr) $ sendMsg nsDockTile (mkSelector "owner") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @display@
displaySelector :: Selector
displaySelector = mkSelector "display"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @contentView@
contentViewSelector :: Selector
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @showsApplicationBadge@
showsApplicationBadgeSelector :: Selector
showsApplicationBadgeSelector = mkSelector "showsApplicationBadge"

-- | @Selector@ for @setShowsApplicationBadge:@
setShowsApplicationBadgeSelector :: Selector
setShowsApplicationBadgeSelector = mkSelector "setShowsApplicationBadge:"

-- | @Selector@ for @badgeLabel@
badgeLabelSelector :: Selector
badgeLabelSelector = mkSelector "badgeLabel"

-- | @Selector@ for @setBadgeLabel:@
setBadgeLabelSelector :: Selector
setBadgeLabelSelector = mkSelector "setBadgeLabel:"

-- | @Selector@ for @owner@
ownerSelector :: Selector
ownerSelector = mkSelector "owner"

