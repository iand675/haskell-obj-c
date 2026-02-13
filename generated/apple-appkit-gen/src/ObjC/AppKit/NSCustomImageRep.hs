{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCustomImageRep@.
module ObjC.AppKit.NSCustomImageRep
  ( NSCustomImageRep
  , IsNSCustomImageRep(..)
  , initWithSize_flipped_drawingHandler
  , initWithDrawSelector_delegate
  , drawingHandler
  , drawSelector
  , delegate
  , delegateSelector
  , drawSelectorSelector
  , drawingHandlerSelector
  , initWithDrawSelector_delegateSelector
  , initWithSize_flipped_drawingHandlerSelector


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

-- | @- initWithSize:flipped:drawingHandler:@
initWithSize_flipped_drawingHandler :: IsNSCustomImageRep nsCustomImageRep => nsCustomImageRep -> NSSize -> Bool -> Ptr () -> IO (Id NSCustomImageRep)
initWithSize_flipped_drawingHandler nsCustomImageRep size drawingHandlerShouldBeCalledWithFlippedContext drawingHandler =
  sendOwnedMessage nsCustomImageRep initWithSize_flipped_drawingHandlerSelector size drawingHandlerShouldBeCalledWithFlippedContext drawingHandler

-- | @- initWithDrawSelector:delegate:@
initWithDrawSelector_delegate :: IsNSCustomImageRep nsCustomImageRep => nsCustomImageRep -> Sel -> RawId -> IO (Id NSCustomImageRep)
initWithDrawSelector_delegate nsCustomImageRep selector delegate =
  sendOwnedMessage nsCustomImageRep initWithDrawSelector_delegateSelector selector delegate

-- | @- drawingHandler@
drawingHandler :: IsNSCustomImageRep nsCustomImageRep => nsCustomImageRep -> IO (Ptr ())
drawingHandler nsCustomImageRep =
  sendMessage nsCustomImageRep drawingHandlerSelector

-- | @- drawSelector@
drawSelector :: IsNSCustomImageRep nsCustomImageRep => nsCustomImageRep -> IO Sel
drawSelector nsCustomImageRep =
  sendMessage nsCustomImageRep drawSelectorSelector

-- | @- delegate@
delegate :: IsNSCustomImageRep nsCustomImageRep => nsCustomImageRep -> IO RawId
delegate nsCustomImageRep =
  sendMessage nsCustomImageRep delegateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSize:flipped:drawingHandler:@
initWithSize_flipped_drawingHandlerSelector :: Selector '[NSSize, Bool, Ptr ()] (Id NSCustomImageRep)
initWithSize_flipped_drawingHandlerSelector = mkSelector "initWithSize:flipped:drawingHandler:"

-- | @Selector@ for @initWithDrawSelector:delegate:@
initWithDrawSelector_delegateSelector :: Selector '[Sel, RawId] (Id NSCustomImageRep)
initWithDrawSelector_delegateSelector = mkSelector "initWithDrawSelector:delegate:"

-- | @Selector@ for @drawingHandler@
drawingHandlerSelector :: Selector '[] (Ptr ())
drawingHandlerSelector = mkSelector "drawingHandler"

-- | @Selector@ for @drawSelector@
drawSelectorSelector :: Selector '[] Sel
drawSelectorSelector = mkSelector "drawSelector"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

