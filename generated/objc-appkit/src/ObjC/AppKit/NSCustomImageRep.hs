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
  , initWithSize_flipped_drawingHandlerSelector
  , initWithDrawSelector_delegateSelector
  , drawingHandlerSelector
  , drawSelectorSelector
  , delegateSelector


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
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithSize:flipped:drawingHandler:@
initWithSize_flipped_drawingHandler :: IsNSCustomImageRep nsCustomImageRep => nsCustomImageRep -> NSSize -> Bool -> Ptr () -> IO (Id NSCustomImageRep)
initWithSize_flipped_drawingHandler nsCustomImageRep  size drawingHandlerShouldBeCalledWithFlippedContext drawingHandler =
  sendMsg nsCustomImageRep (mkSelector "initWithSize:flipped:drawingHandler:") (retPtr retVoid) [argNSSize size, argCULong (if drawingHandlerShouldBeCalledWithFlippedContext then 1 else 0), argPtr (castPtr drawingHandler :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDrawSelector:delegate:@
initWithDrawSelector_delegate :: IsNSCustomImageRep nsCustomImageRep => nsCustomImageRep -> Selector -> RawId -> IO (Id NSCustomImageRep)
initWithDrawSelector_delegate nsCustomImageRep  selector delegate =
  sendMsg nsCustomImageRep (mkSelector "initWithDrawSelector:delegate:") (retPtr retVoid) [argPtr (unSelector selector), argPtr (castPtr (unRawId delegate) :: Ptr ())] >>= ownedObject . castPtr

-- | @- drawingHandler@
drawingHandler :: IsNSCustomImageRep nsCustomImageRep => nsCustomImageRep -> IO (Ptr ())
drawingHandler nsCustomImageRep  =
  fmap castPtr $ sendMsg nsCustomImageRep (mkSelector "drawingHandler") (retPtr retVoid) []

-- | @- drawSelector@
drawSelector :: IsNSCustomImageRep nsCustomImageRep => nsCustomImageRep -> IO Selector
drawSelector nsCustomImageRep  =
  fmap (Selector . castPtr) $ sendMsg nsCustomImageRep (mkSelector "drawSelector") (retPtr retVoid) []

-- | @- delegate@
delegate :: IsNSCustomImageRep nsCustomImageRep => nsCustomImageRep -> IO RawId
delegate nsCustomImageRep  =
  fmap (RawId . castPtr) $ sendMsg nsCustomImageRep (mkSelector "delegate") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSize:flipped:drawingHandler:@
initWithSize_flipped_drawingHandlerSelector :: Selector
initWithSize_flipped_drawingHandlerSelector = mkSelector "initWithSize:flipped:drawingHandler:"

-- | @Selector@ for @initWithDrawSelector:delegate:@
initWithDrawSelector_delegateSelector :: Selector
initWithDrawSelector_delegateSelector = mkSelector "initWithDrawSelector:delegate:"

-- | @Selector@ for @drawingHandler@
drawingHandlerSelector :: Selector
drawingHandlerSelector = mkSelector "drawingHandler"

-- | @Selector@ for @drawSelector@
drawSelectorSelector :: Selector
drawSelectorSelector = mkSelector "drawSelector"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

