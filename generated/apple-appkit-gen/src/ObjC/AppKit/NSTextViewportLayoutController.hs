{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextViewportLayoutController@.
module ObjC.AppKit.NSTextViewportLayoutController
  ( NSTextViewportLayoutController
  , IsNSTextViewportLayoutController(..)
  , initWithTextLayoutManager
  , new
  , init_
  , layoutViewport
  , relocateViewportToTextLocation
  , adjustViewportByVerticalOffset
  , delegate
  , setDelegate
  , textLayoutManager
  , viewportRange
  , adjustViewportByVerticalOffsetSelector
  , delegateSelector
  , initSelector
  , initWithTextLayoutManagerSelector
  , layoutViewportSelector
  , newSelector
  , relocateViewportToTextLocationSelector
  , setDelegateSelector
  , textLayoutManagerSelector
  , viewportRangeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTextLayoutManager:@
initWithTextLayoutManager :: (IsNSTextViewportLayoutController nsTextViewportLayoutController, IsNSTextLayoutManager textLayoutManager) => nsTextViewportLayoutController -> textLayoutManager -> IO (Id NSTextViewportLayoutController)
initWithTextLayoutManager nsTextViewportLayoutController textLayoutManager =
  sendOwnedMessage nsTextViewportLayoutController initWithTextLayoutManagerSelector (toNSTextLayoutManager textLayoutManager)

-- | @+ new@
new :: IO (Id NSTextViewportLayoutController)
new  =
  do
    cls' <- getRequiredClass "NSTextViewportLayoutController"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> IO (Id NSTextViewportLayoutController)
init_ nsTextViewportLayoutController =
  sendOwnedMessage nsTextViewportLayoutController initSelector

-- | @- layoutViewport@
layoutViewport :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> IO ()
layoutViewport nsTextViewportLayoutController =
  sendMessage nsTextViewportLayoutController layoutViewportSelector

-- | @- relocateViewportToTextLocation:@
relocateViewportToTextLocation :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> RawId -> IO CDouble
relocateViewportToTextLocation nsTextViewportLayoutController textLocation =
  sendMessage nsTextViewportLayoutController relocateViewportToTextLocationSelector textLocation

-- | @- adjustViewportByVerticalOffset:@
adjustViewportByVerticalOffset :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> CDouble -> IO ()
adjustViewportByVerticalOffset nsTextViewportLayoutController verticalOffset =
  sendMessage nsTextViewportLayoutController adjustViewportByVerticalOffsetSelector verticalOffset

-- | @- delegate@
delegate :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> IO RawId
delegate nsTextViewportLayoutController =
  sendMessage nsTextViewportLayoutController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> RawId -> IO ()
setDelegate nsTextViewportLayoutController value =
  sendMessage nsTextViewportLayoutController setDelegateSelector value

-- | @- textLayoutManager@
textLayoutManager :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> IO (Id NSTextLayoutManager)
textLayoutManager nsTextViewportLayoutController =
  sendMessage nsTextViewportLayoutController textLayoutManagerSelector

-- | @- viewportRange@
viewportRange :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> IO (Id NSTextRange)
viewportRange nsTextViewportLayoutController =
  sendMessage nsTextViewportLayoutController viewportRangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTextLayoutManager:@
initWithTextLayoutManagerSelector :: Selector '[Id NSTextLayoutManager] (Id NSTextViewportLayoutController)
initWithTextLayoutManagerSelector = mkSelector "initWithTextLayoutManager:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSTextViewportLayoutController)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextViewportLayoutController)
initSelector = mkSelector "init"

-- | @Selector@ for @layoutViewport@
layoutViewportSelector :: Selector '[] ()
layoutViewportSelector = mkSelector "layoutViewport"

-- | @Selector@ for @relocateViewportToTextLocation:@
relocateViewportToTextLocationSelector :: Selector '[RawId] CDouble
relocateViewportToTextLocationSelector = mkSelector "relocateViewportToTextLocation:"

-- | @Selector@ for @adjustViewportByVerticalOffset:@
adjustViewportByVerticalOffsetSelector :: Selector '[CDouble] ()
adjustViewportByVerticalOffsetSelector = mkSelector "adjustViewportByVerticalOffset:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @textLayoutManager@
textLayoutManagerSelector :: Selector '[] (Id NSTextLayoutManager)
textLayoutManagerSelector = mkSelector "textLayoutManager"

-- | @Selector@ for @viewportRange@
viewportRangeSelector :: Selector '[] (Id NSTextRange)
viewportRangeSelector = mkSelector "viewportRange"

