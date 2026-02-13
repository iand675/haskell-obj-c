{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDrawer@.
module ObjC.AppKit.NSDrawer
  ( NSDrawer
  , IsNSDrawer(..)
  , initWithContentSize_preferredEdge
  , open
  , openOnEdge
  , close
  , toggle
  , parentWindow
  , setParentWindow
  , contentView
  , setContentView
  , preferredEdge
  , setPreferredEdge
  , delegate
  , setDelegate
  , state
  , edge
  , contentSize
  , setContentSize
  , minContentSize
  , setMinContentSize
  , maxContentSize
  , setMaxContentSize
  , leadingOffset
  , setLeadingOffset
  , trailingOffset
  , setTrailingOffset
  , closeSelector
  , contentSizeSelector
  , contentViewSelector
  , delegateSelector
  , edgeSelector
  , initWithContentSize_preferredEdgeSelector
  , leadingOffsetSelector
  , maxContentSizeSelector
  , minContentSizeSelector
  , openOnEdgeSelector
  , openSelector
  , parentWindowSelector
  , preferredEdgeSelector
  , setContentSizeSelector
  , setContentViewSelector
  , setDelegateSelector
  , setLeadingOffsetSelector
  , setMaxContentSizeSelector
  , setMinContentSizeSelector
  , setParentWindowSelector
  , setPreferredEdgeSelector
  , setTrailingOffsetSelector
  , stateSelector
  , toggleSelector
  , trailingOffsetSelector

  -- * Enum types
  , NSRectEdge(NSRectEdge)
  , pattern NSRectEdgeMinX
  , pattern NSRectEdgeMinY
  , pattern NSRectEdgeMaxX
  , pattern NSRectEdgeMaxY
  , pattern NSMinXEdge
  , pattern NSMinYEdge
  , pattern NSMaxXEdge
  , pattern NSMaxYEdge

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithContentSize:preferredEdge:@
initWithContentSize_preferredEdge :: IsNSDrawer nsDrawer => nsDrawer -> NSSize -> NSRectEdge -> IO (Id NSDrawer)
initWithContentSize_preferredEdge nsDrawer contentSize edge =
  sendOwnedMessage nsDrawer initWithContentSize_preferredEdgeSelector contentSize edge

-- | @- open@
open :: IsNSDrawer nsDrawer => nsDrawer -> IO ()
open nsDrawer =
  sendMessage nsDrawer openSelector

-- | @- openOnEdge:@
openOnEdge :: IsNSDrawer nsDrawer => nsDrawer -> NSRectEdge -> IO ()
openOnEdge nsDrawer edge =
  sendMessage nsDrawer openOnEdgeSelector edge

-- | @- close@
close :: IsNSDrawer nsDrawer => nsDrawer -> IO ()
close nsDrawer =
  sendMessage nsDrawer closeSelector

-- | @- toggle:@
toggle :: IsNSDrawer nsDrawer => nsDrawer -> RawId -> IO ()
toggle nsDrawer sender =
  sendMessage nsDrawer toggleSelector sender

-- | @- parentWindow@
parentWindow :: IsNSDrawer nsDrawer => nsDrawer -> IO (Id NSWindow)
parentWindow nsDrawer =
  sendMessage nsDrawer parentWindowSelector

-- | @- setParentWindow:@
setParentWindow :: (IsNSDrawer nsDrawer, IsNSWindow value) => nsDrawer -> value -> IO ()
setParentWindow nsDrawer value =
  sendMessage nsDrawer setParentWindowSelector (toNSWindow value)

-- | @- contentView@
contentView :: IsNSDrawer nsDrawer => nsDrawer -> IO (Id NSView)
contentView nsDrawer =
  sendMessage nsDrawer contentViewSelector

-- | @- setContentView:@
setContentView :: (IsNSDrawer nsDrawer, IsNSView value) => nsDrawer -> value -> IO ()
setContentView nsDrawer value =
  sendMessage nsDrawer setContentViewSelector (toNSView value)

-- | @- preferredEdge@
preferredEdge :: IsNSDrawer nsDrawer => nsDrawer -> IO NSRectEdge
preferredEdge nsDrawer =
  sendMessage nsDrawer preferredEdgeSelector

-- | @- setPreferredEdge:@
setPreferredEdge :: IsNSDrawer nsDrawer => nsDrawer -> NSRectEdge -> IO ()
setPreferredEdge nsDrawer value =
  sendMessage nsDrawer setPreferredEdgeSelector value

-- | @- delegate@
delegate :: IsNSDrawer nsDrawer => nsDrawer -> IO RawId
delegate nsDrawer =
  sendMessage nsDrawer delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSDrawer nsDrawer => nsDrawer -> RawId -> IO ()
setDelegate nsDrawer value =
  sendMessage nsDrawer setDelegateSelector value

-- | @- state@
state :: IsNSDrawer nsDrawer => nsDrawer -> IO CLong
state nsDrawer =
  sendMessage nsDrawer stateSelector

-- | @- edge@
edge :: IsNSDrawer nsDrawer => nsDrawer -> IO NSRectEdge
edge nsDrawer =
  sendMessage nsDrawer edgeSelector

-- | @- contentSize@
contentSize :: IsNSDrawer nsDrawer => nsDrawer -> IO NSSize
contentSize nsDrawer =
  sendMessage nsDrawer contentSizeSelector

-- | @- setContentSize:@
setContentSize :: IsNSDrawer nsDrawer => nsDrawer -> NSSize -> IO ()
setContentSize nsDrawer value =
  sendMessage nsDrawer setContentSizeSelector value

-- | @- minContentSize@
minContentSize :: IsNSDrawer nsDrawer => nsDrawer -> IO NSSize
minContentSize nsDrawer =
  sendMessage nsDrawer minContentSizeSelector

-- | @- setMinContentSize:@
setMinContentSize :: IsNSDrawer nsDrawer => nsDrawer -> NSSize -> IO ()
setMinContentSize nsDrawer value =
  sendMessage nsDrawer setMinContentSizeSelector value

-- | @- maxContentSize@
maxContentSize :: IsNSDrawer nsDrawer => nsDrawer -> IO NSSize
maxContentSize nsDrawer =
  sendMessage nsDrawer maxContentSizeSelector

-- | @- setMaxContentSize:@
setMaxContentSize :: IsNSDrawer nsDrawer => nsDrawer -> NSSize -> IO ()
setMaxContentSize nsDrawer value =
  sendMessage nsDrawer setMaxContentSizeSelector value

-- | @- leadingOffset@
leadingOffset :: IsNSDrawer nsDrawer => nsDrawer -> IO CDouble
leadingOffset nsDrawer =
  sendMessage nsDrawer leadingOffsetSelector

-- | @- setLeadingOffset:@
setLeadingOffset :: IsNSDrawer nsDrawer => nsDrawer -> CDouble -> IO ()
setLeadingOffset nsDrawer value =
  sendMessage nsDrawer setLeadingOffsetSelector value

-- | @- trailingOffset@
trailingOffset :: IsNSDrawer nsDrawer => nsDrawer -> IO CDouble
trailingOffset nsDrawer =
  sendMessage nsDrawer trailingOffsetSelector

-- | @- setTrailingOffset:@
setTrailingOffset :: IsNSDrawer nsDrawer => nsDrawer -> CDouble -> IO ()
setTrailingOffset nsDrawer value =
  sendMessage nsDrawer setTrailingOffsetSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentSize:preferredEdge:@
initWithContentSize_preferredEdgeSelector :: Selector '[NSSize, NSRectEdge] (Id NSDrawer)
initWithContentSize_preferredEdgeSelector = mkSelector "initWithContentSize:preferredEdge:"

-- | @Selector@ for @open@
openSelector :: Selector '[] ()
openSelector = mkSelector "open"

-- | @Selector@ for @openOnEdge:@
openOnEdgeSelector :: Selector '[NSRectEdge] ()
openOnEdgeSelector = mkSelector "openOnEdge:"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @toggle:@
toggleSelector :: Selector '[RawId] ()
toggleSelector = mkSelector "toggle:"

-- | @Selector@ for @parentWindow@
parentWindowSelector :: Selector '[] (Id NSWindow)
parentWindowSelector = mkSelector "parentWindow"

-- | @Selector@ for @setParentWindow:@
setParentWindowSelector :: Selector '[Id NSWindow] ()
setParentWindowSelector = mkSelector "setParentWindow:"

-- | @Selector@ for @contentView@
contentViewSelector :: Selector '[] (Id NSView)
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector '[Id NSView] ()
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @preferredEdge@
preferredEdgeSelector :: Selector '[] NSRectEdge
preferredEdgeSelector = mkSelector "preferredEdge"

-- | @Selector@ for @setPreferredEdge:@
setPreferredEdgeSelector :: Selector '[NSRectEdge] ()
setPreferredEdgeSelector = mkSelector "setPreferredEdge:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] CLong
stateSelector = mkSelector "state"

-- | @Selector@ for @edge@
edgeSelector :: Selector '[] NSRectEdge
edgeSelector = mkSelector "edge"

-- | @Selector@ for @contentSize@
contentSizeSelector :: Selector '[] NSSize
contentSizeSelector = mkSelector "contentSize"

-- | @Selector@ for @setContentSize:@
setContentSizeSelector :: Selector '[NSSize] ()
setContentSizeSelector = mkSelector "setContentSize:"

-- | @Selector@ for @minContentSize@
minContentSizeSelector :: Selector '[] NSSize
minContentSizeSelector = mkSelector "minContentSize"

-- | @Selector@ for @setMinContentSize:@
setMinContentSizeSelector :: Selector '[NSSize] ()
setMinContentSizeSelector = mkSelector "setMinContentSize:"

-- | @Selector@ for @maxContentSize@
maxContentSizeSelector :: Selector '[] NSSize
maxContentSizeSelector = mkSelector "maxContentSize"

-- | @Selector@ for @setMaxContentSize:@
setMaxContentSizeSelector :: Selector '[NSSize] ()
setMaxContentSizeSelector = mkSelector "setMaxContentSize:"

-- | @Selector@ for @leadingOffset@
leadingOffsetSelector :: Selector '[] CDouble
leadingOffsetSelector = mkSelector "leadingOffset"

-- | @Selector@ for @setLeadingOffset:@
setLeadingOffsetSelector :: Selector '[CDouble] ()
setLeadingOffsetSelector = mkSelector "setLeadingOffset:"

-- | @Selector@ for @trailingOffset@
trailingOffsetSelector :: Selector '[] CDouble
trailingOffsetSelector = mkSelector "trailingOffset"

-- | @Selector@ for @setTrailingOffset:@
setTrailingOffsetSelector :: Selector '[CDouble] ()
setTrailingOffsetSelector = mkSelector "setTrailingOffset:"

