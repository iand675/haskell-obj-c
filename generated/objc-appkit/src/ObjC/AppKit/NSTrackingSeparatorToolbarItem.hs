{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTrackingSeparatorToolbarItem@.
module ObjC.AppKit.NSTrackingSeparatorToolbarItem
  ( NSTrackingSeparatorToolbarItem
  , IsNSTrackingSeparatorToolbarItem(..)
  , trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndex
  , dividerIndex
  , setDividerIndex
  , trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndexSelector
  , dividerIndexSelector
  , setDividerIndexSelector


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

-- | Creates a new @NSTrackingSeparatorToolbarItem@ and configures it to attempt to visually align with the specified divider of the @splitView@. The @splitView@ must be in the same window as the toolbar containing this item by the time the toolbar is shown. Only vertical @splitViews@ are supported.
--
-- ObjC selector: @+ trackingSeparatorToolbarItemWithIdentifier:splitView:dividerIndex:@
trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndex :: (IsNSString identifier, IsNSSplitView splitView) => identifier -> splitView -> CLong -> IO (Id NSTrackingSeparatorToolbarItem)
trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndex identifier splitView dividerIndex =
  do
    cls' <- getRequiredClass "NSTrackingSeparatorToolbarItem"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr splitView $ \raw_splitView ->
        sendClassMsg cls' (mkSelector "trackingSeparatorToolbarItemWithIdentifier:splitView:dividerIndex:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_splitView :: Ptr ()), argCLong (fromIntegral dividerIndex)] >>= retainedObject . castPtr

-- | The specific divider of the @splitView@ which will be tracked.
--
-- ObjC selector: @- dividerIndex@
dividerIndex :: IsNSTrackingSeparatorToolbarItem nsTrackingSeparatorToolbarItem => nsTrackingSeparatorToolbarItem -> IO CLong
dividerIndex nsTrackingSeparatorToolbarItem  =
  sendMsg nsTrackingSeparatorToolbarItem (mkSelector "dividerIndex") retCLong []

-- | The specific divider of the @splitView@ which will be tracked.
--
-- ObjC selector: @- setDividerIndex:@
setDividerIndex :: IsNSTrackingSeparatorToolbarItem nsTrackingSeparatorToolbarItem => nsTrackingSeparatorToolbarItem -> CLong -> IO ()
setDividerIndex nsTrackingSeparatorToolbarItem  value =
  sendMsg nsTrackingSeparatorToolbarItem (mkSelector "setDividerIndex:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @trackingSeparatorToolbarItemWithIdentifier:splitView:dividerIndex:@
trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndexSelector :: Selector
trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndexSelector = mkSelector "trackingSeparatorToolbarItemWithIdentifier:splitView:dividerIndex:"

-- | @Selector@ for @dividerIndex@
dividerIndexSelector :: Selector
dividerIndexSelector = mkSelector "dividerIndex"

-- | @Selector@ for @setDividerIndex:@
setDividerIndexSelector :: Selector
setDividerIndexSelector = mkSelector "setDividerIndex:"

