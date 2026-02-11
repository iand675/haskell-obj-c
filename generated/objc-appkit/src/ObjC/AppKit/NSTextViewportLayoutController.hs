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
  , textLayoutManager
  , viewportRange
  , initWithTextLayoutManagerSelector
  , newSelector
  , initSelector
  , layoutViewportSelector
  , relocateViewportToTextLocationSelector
  , adjustViewportByVerticalOffsetSelector
  , textLayoutManagerSelector
  , viewportRangeSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithTextLayoutManager:@
initWithTextLayoutManager :: (IsNSTextViewportLayoutController nsTextViewportLayoutController, IsNSTextLayoutManager textLayoutManager) => nsTextViewportLayoutController -> textLayoutManager -> IO (Id NSTextViewportLayoutController)
initWithTextLayoutManager nsTextViewportLayoutController  textLayoutManager =
withObjCPtr textLayoutManager $ \raw_textLayoutManager ->
    sendMsg nsTextViewportLayoutController (mkSelector "initWithTextLayoutManager:") (retPtr retVoid) [argPtr (castPtr raw_textLayoutManager :: Ptr ())] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSTextViewportLayoutController)
new  =
  do
    cls' <- getRequiredClass "NSTextViewportLayoutController"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> IO (Id NSTextViewportLayoutController)
init_ nsTextViewportLayoutController  =
  sendMsg nsTextViewportLayoutController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- layoutViewport@
layoutViewport :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> IO ()
layoutViewport nsTextViewportLayoutController  =
  sendMsg nsTextViewportLayoutController (mkSelector "layoutViewport") retVoid []

-- | @- relocateViewportToTextLocation:@
relocateViewportToTextLocation :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> RawId -> IO CDouble
relocateViewportToTextLocation nsTextViewportLayoutController  textLocation =
  sendMsg nsTextViewportLayoutController (mkSelector "relocateViewportToTextLocation:") retCDouble [argPtr (castPtr (unRawId textLocation) :: Ptr ())]

-- | @- adjustViewportByVerticalOffset:@
adjustViewportByVerticalOffset :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> CDouble -> IO ()
adjustViewportByVerticalOffset nsTextViewportLayoutController  verticalOffset =
  sendMsg nsTextViewportLayoutController (mkSelector "adjustViewportByVerticalOffset:") retVoid [argCDouble (fromIntegral verticalOffset)]

-- | @- textLayoutManager@
textLayoutManager :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> IO (Id NSTextLayoutManager)
textLayoutManager nsTextViewportLayoutController  =
  sendMsg nsTextViewportLayoutController (mkSelector "textLayoutManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- viewportRange@
viewportRange :: IsNSTextViewportLayoutController nsTextViewportLayoutController => nsTextViewportLayoutController -> IO (Id NSTextRange)
viewportRange nsTextViewportLayoutController  =
  sendMsg nsTextViewportLayoutController (mkSelector "viewportRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTextLayoutManager:@
initWithTextLayoutManagerSelector :: Selector
initWithTextLayoutManagerSelector = mkSelector "initWithTextLayoutManager:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @layoutViewport@
layoutViewportSelector :: Selector
layoutViewportSelector = mkSelector "layoutViewport"

-- | @Selector@ for @relocateViewportToTextLocation:@
relocateViewportToTextLocationSelector :: Selector
relocateViewportToTextLocationSelector = mkSelector "relocateViewportToTextLocation:"

-- | @Selector@ for @adjustViewportByVerticalOffset:@
adjustViewportByVerticalOffsetSelector :: Selector
adjustViewportByVerticalOffsetSelector = mkSelector "adjustViewportByVerticalOffset:"

-- | @Selector@ for @textLayoutManager@
textLayoutManagerSelector :: Selector
textLayoutManagerSelector = mkSelector "textLayoutManager"

-- | @Selector@ for @viewportRange@
viewportRangeSelector :: Selector
viewportRangeSelector = mkSelector "viewportRange"

