{-# LANGUAGE PatternSynonyms #-}
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
  , initWithContentSize_preferredEdgeSelector
  , openSelector
  , openOnEdgeSelector
  , closeSelector
  , toggleSelector
  , parentWindowSelector
  , setParentWindowSelector
  , contentViewSelector
  , setContentViewSelector
  , preferredEdgeSelector
  , setPreferredEdgeSelector
  , stateSelector
  , edgeSelector
  , contentSizeSelector
  , setContentSizeSelector
  , minContentSizeSelector
  , setMinContentSizeSelector
  , maxContentSizeSelector
  , setMaxContentSizeSelector
  , leadingOffsetSelector
  , setLeadingOffsetSelector
  , trailingOffsetSelector
  , setTrailingOffsetSelector

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithContentSize:preferredEdge:@
initWithContentSize_preferredEdge :: IsNSDrawer nsDrawer => nsDrawer -> NSSize -> NSRectEdge -> IO (Id NSDrawer)
initWithContentSize_preferredEdge nsDrawer  contentSize edge =
  sendMsg nsDrawer (mkSelector "initWithContentSize:preferredEdge:") (retPtr retVoid) [argNSSize contentSize, argCULong (coerce edge)] >>= ownedObject . castPtr

-- | @- open@
open :: IsNSDrawer nsDrawer => nsDrawer -> IO ()
open nsDrawer  =
  sendMsg nsDrawer (mkSelector "open") retVoid []

-- | @- openOnEdge:@
openOnEdge :: IsNSDrawer nsDrawer => nsDrawer -> NSRectEdge -> IO ()
openOnEdge nsDrawer  edge =
  sendMsg nsDrawer (mkSelector "openOnEdge:") retVoid [argCULong (coerce edge)]

-- | @- close@
close :: IsNSDrawer nsDrawer => nsDrawer -> IO ()
close nsDrawer  =
  sendMsg nsDrawer (mkSelector "close") retVoid []

-- | @- toggle:@
toggle :: IsNSDrawer nsDrawer => nsDrawer -> RawId -> IO ()
toggle nsDrawer  sender =
  sendMsg nsDrawer (mkSelector "toggle:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- parentWindow@
parentWindow :: IsNSDrawer nsDrawer => nsDrawer -> IO (Id NSWindow)
parentWindow nsDrawer  =
  sendMsg nsDrawer (mkSelector "parentWindow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParentWindow:@
setParentWindow :: (IsNSDrawer nsDrawer, IsNSWindow value) => nsDrawer -> value -> IO ()
setParentWindow nsDrawer  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDrawer (mkSelector "setParentWindow:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentView@
contentView :: IsNSDrawer nsDrawer => nsDrawer -> IO (Id NSView)
contentView nsDrawer  =
  sendMsg nsDrawer (mkSelector "contentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentView:@
setContentView :: (IsNSDrawer nsDrawer, IsNSView value) => nsDrawer -> value -> IO ()
setContentView nsDrawer  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDrawer (mkSelector "setContentView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- preferredEdge@
preferredEdge :: IsNSDrawer nsDrawer => nsDrawer -> IO NSRectEdge
preferredEdge nsDrawer  =
  fmap (coerce :: CULong -> NSRectEdge) $ sendMsg nsDrawer (mkSelector "preferredEdge") retCULong []

-- | @- setPreferredEdge:@
setPreferredEdge :: IsNSDrawer nsDrawer => nsDrawer -> NSRectEdge -> IO ()
setPreferredEdge nsDrawer  value =
  sendMsg nsDrawer (mkSelector "setPreferredEdge:") retVoid [argCULong (coerce value)]

-- | @- state@
state :: IsNSDrawer nsDrawer => nsDrawer -> IO CLong
state nsDrawer  =
  sendMsg nsDrawer (mkSelector "state") retCLong []

-- | @- edge@
edge :: IsNSDrawer nsDrawer => nsDrawer -> IO NSRectEdge
edge nsDrawer  =
  fmap (coerce :: CULong -> NSRectEdge) $ sendMsg nsDrawer (mkSelector "edge") retCULong []

-- | @- contentSize@
contentSize :: IsNSDrawer nsDrawer => nsDrawer -> IO NSSize
contentSize nsDrawer  =
  sendMsgStret nsDrawer (mkSelector "contentSize") retNSSize []

-- | @- setContentSize:@
setContentSize :: IsNSDrawer nsDrawer => nsDrawer -> NSSize -> IO ()
setContentSize nsDrawer  value =
  sendMsg nsDrawer (mkSelector "setContentSize:") retVoid [argNSSize value]

-- | @- minContentSize@
minContentSize :: IsNSDrawer nsDrawer => nsDrawer -> IO NSSize
minContentSize nsDrawer  =
  sendMsgStret nsDrawer (mkSelector "minContentSize") retNSSize []

-- | @- setMinContentSize:@
setMinContentSize :: IsNSDrawer nsDrawer => nsDrawer -> NSSize -> IO ()
setMinContentSize nsDrawer  value =
  sendMsg nsDrawer (mkSelector "setMinContentSize:") retVoid [argNSSize value]

-- | @- maxContentSize@
maxContentSize :: IsNSDrawer nsDrawer => nsDrawer -> IO NSSize
maxContentSize nsDrawer  =
  sendMsgStret nsDrawer (mkSelector "maxContentSize") retNSSize []

-- | @- setMaxContentSize:@
setMaxContentSize :: IsNSDrawer nsDrawer => nsDrawer -> NSSize -> IO ()
setMaxContentSize nsDrawer  value =
  sendMsg nsDrawer (mkSelector "setMaxContentSize:") retVoid [argNSSize value]

-- | @- leadingOffset@
leadingOffset :: IsNSDrawer nsDrawer => nsDrawer -> IO CDouble
leadingOffset nsDrawer  =
  sendMsg nsDrawer (mkSelector "leadingOffset") retCDouble []

-- | @- setLeadingOffset:@
setLeadingOffset :: IsNSDrawer nsDrawer => nsDrawer -> CDouble -> IO ()
setLeadingOffset nsDrawer  value =
  sendMsg nsDrawer (mkSelector "setLeadingOffset:") retVoid [argCDouble (fromIntegral value)]

-- | @- trailingOffset@
trailingOffset :: IsNSDrawer nsDrawer => nsDrawer -> IO CDouble
trailingOffset nsDrawer  =
  sendMsg nsDrawer (mkSelector "trailingOffset") retCDouble []

-- | @- setTrailingOffset:@
setTrailingOffset :: IsNSDrawer nsDrawer => nsDrawer -> CDouble -> IO ()
setTrailingOffset nsDrawer  value =
  sendMsg nsDrawer (mkSelector "setTrailingOffset:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentSize:preferredEdge:@
initWithContentSize_preferredEdgeSelector :: Selector
initWithContentSize_preferredEdgeSelector = mkSelector "initWithContentSize:preferredEdge:"

-- | @Selector@ for @open@
openSelector :: Selector
openSelector = mkSelector "open"

-- | @Selector@ for @openOnEdge:@
openOnEdgeSelector :: Selector
openOnEdgeSelector = mkSelector "openOnEdge:"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @toggle:@
toggleSelector :: Selector
toggleSelector = mkSelector "toggle:"

-- | @Selector@ for @parentWindow@
parentWindowSelector :: Selector
parentWindowSelector = mkSelector "parentWindow"

-- | @Selector@ for @setParentWindow:@
setParentWindowSelector :: Selector
setParentWindowSelector = mkSelector "setParentWindow:"

-- | @Selector@ for @contentView@
contentViewSelector :: Selector
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @preferredEdge@
preferredEdgeSelector :: Selector
preferredEdgeSelector = mkSelector "preferredEdge"

-- | @Selector@ for @setPreferredEdge:@
setPreferredEdgeSelector :: Selector
setPreferredEdgeSelector = mkSelector "setPreferredEdge:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @edge@
edgeSelector :: Selector
edgeSelector = mkSelector "edge"

-- | @Selector@ for @contentSize@
contentSizeSelector :: Selector
contentSizeSelector = mkSelector "contentSize"

-- | @Selector@ for @setContentSize:@
setContentSizeSelector :: Selector
setContentSizeSelector = mkSelector "setContentSize:"

-- | @Selector@ for @minContentSize@
minContentSizeSelector :: Selector
minContentSizeSelector = mkSelector "minContentSize"

-- | @Selector@ for @setMinContentSize:@
setMinContentSizeSelector :: Selector
setMinContentSizeSelector = mkSelector "setMinContentSize:"

-- | @Selector@ for @maxContentSize@
maxContentSizeSelector :: Selector
maxContentSizeSelector = mkSelector "maxContentSize"

-- | @Selector@ for @setMaxContentSize:@
setMaxContentSizeSelector :: Selector
setMaxContentSizeSelector = mkSelector "setMaxContentSize:"

-- | @Selector@ for @leadingOffset@
leadingOffsetSelector :: Selector
leadingOffsetSelector = mkSelector "leadingOffset"

-- | @Selector@ for @setLeadingOffset:@
setLeadingOffsetSelector :: Selector
setLeadingOffsetSelector = mkSelector "setLeadingOffset:"

-- | @Selector@ for @trailingOffset@
trailingOffsetSelector :: Selector
trailingOffsetSelector = mkSelector "trailingOffset"

-- | @Selector@ for @setTrailingOffset:@
setTrailingOffsetSelector :: Selector
setTrailingOffsetSelector = mkSelector "setTrailingOffset:"

