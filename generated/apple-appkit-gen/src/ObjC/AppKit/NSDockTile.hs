{-# LANGUAGE DataKinds #-}
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
  , badgeLabelSelector
  , contentViewSelector
  , displaySelector
  , ownerSelector
  , setBadgeLabelSelector
  , setContentViewSelector
  , setShowsApplicationBadgeSelector
  , showsApplicationBadgeSelector
  , sizeSelector


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

-- | @- display@
display :: IsNSDockTile nsDockTile => nsDockTile -> IO ()
display nsDockTile =
  sendMessage nsDockTile displaySelector

-- | @- size@
size :: IsNSDockTile nsDockTile => nsDockTile -> IO NSSize
size nsDockTile =
  sendMessage nsDockTile sizeSelector

-- | @- contentView@
contentView :: IsNSDockTile nsDockTile => nsDockTile -> IO (Id NSView)
contentView nsDockTile =
  sendMessage nsDockTile contentViewSelector

-- | @- setContentView:@
setContentView :: (IsNSDockTile nsDockTile, IsNSView value) => nsDockTile -> value -> IO ()
setContentView nsDockTile value =
  sendMessage nsDockTile setContentViewSelector (toNSView value)

-- | @- showsApplicationBadge@
showsApplicationBadge :: IsNSDockTile nsDockTile => nsDockTile -> IO Bool
showsApplicationBadge nsDockTile =
  sendMessage nsDockTile showsApplicationBadgeSelector

-- | @- setShowsApplicationBadge:@
setShowsApplicationBadge :: IsNSDockTile nsDockTile => nsDockTile -> Bool -> IO ()
setShowsApplicationBadge nsDockTile value =
  sendMessage nsDockTile setShowsApplicationBadgeSelector value

-- | @- badgeLabel@
badgeLabel :: IsNSDockTile nsDockTile => nsDockTile -> IO (Id NSString)
badgeLabel nsDockTile =
  sendMessage nsDockTile badgeLabelSelector

-- | @- setBadgeLabel:@
setBadgeLabel :: (IsNSDockTile nsDockTile, IsNSString value) => nsDockTile -> value -> IO ()
setBadgeLabel nsDockTile value =
  sendMessage nsDockTile setBadgeLabelSelector (toNSString value)

-- | @- owner@
owner :: IsNSDockTile nsDockTile => nsDockTile -> IO RawId
owner nsDockTile =
  sendMessage nsDockTile ownerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @display@
displaySelector :: Selector '[] ()
displaySelector = mkSelector "display"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] NSSize
sizeSelector = mkSelector "size"

-- | @Selector@ for @contentView@
contentViewSelector :: Selector '[] (Id NSView)
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector '[Id NSView] ()
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @showsApplicationBadge@
showsApplicationBadgeSelector :: Selector '[] Bool
showsApplicationBadgeSelector = mkSelector "showsApplicationBadge"

-- | @Selector@ for @setShowsApplicationBadge:@
setShowsApplicationBadgeSelector :: Selector '[Bool] ()
setShowsApplicationBadgeSelector = mkSelector "setShowsApplicationBadge:"

-- | @Selector@ for @badgeLabel@
badgeLabelSelector :: Selector '[] (Id NSString)
badgeLabelSelector = mkSelector "badgeLabel"

-- | @Selector@ for @setBadgeLabel:@
setBadgeLabelSelector :: Selector '[Id NSString] ()
setBadgeLabelSelector = mkSelector "setBadgeLabel:"

-- | @Selector@ for @owner@
ownerSelector :: Selector '[] RawId
ownerSelector = mkSelector "owner"

