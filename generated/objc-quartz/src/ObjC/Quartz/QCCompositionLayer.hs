{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QCCompositionLayer@.
module ObjC.Quartz.QCCompositionLayer
  ( QCCompositionLayer
  , IsQCCompositionLayer(..)
  , compositionLayerWithFile
  , compositionLayerWithComposition
  , initWithFile
  , initWithComposition
  , composition
  , compositionLayerWithFileSelector
  , compositionLayerWithCompositionSelector
  , initWithFileSelector
  , initWithCompositionSelector
  , compositionSelector


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

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | @+ compositionLayerWithFile:@
compositionLayerWithFile :: IsNSString path => path -> IO (Id QCCompositionLayer)
compositionLayerWithFile path =
  do
    cls' <- getRequiredClass "QCCompositionLayer"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "compositionLayerWithFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @+ compositionLayerWithComposition:@
compositionLayerWithComposition :: IsQCComposition composition => composition -> IO (Id QCCompositionLayer)
compositionLayerWithComposition composition =
  do
    cls' <- getRequiredClass "QCCompositionLayer"
    withObjCPtr composition $ \raw_composition ->
      sendClassMsg cls' (mkSelector "compositionLayerWithComposition:") (retPtr retVoid) [argPtr (castPtr raw_composition :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithFile:@
initWithFile :: (IsQCCompositionLayer qcCompositionLayer, IsNSString path) => qcCompositionLayer -> path -> IO RawId
initWithFile qcCompositionLayer  path =
withObjCPtr path $ \raw_path ->
    fmap (RawId . castPtr) $ sendMsg qcCompositionLayer (mkSelector "initWithFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | @- initWithComposition:@
initWithComposition :: (IsQCCompositionLayer qcCompositionLayer, IsQCComposition composition) => qcCompositionLayer -> composition -> IO RawId
initWithComposition qcCompositionLayer  composition =
withObjCPtr composition $ \raw_composition ->
    fmap (RawId . castPtr) $ sendMsg qcCompositionLayer (mkSelector "initWithComposition:") (retPtr retVoid) [argPtr (castPtr raw_composition :: Ptr ())]

-- | @- composition@
composition :: IsQCCompositionLayer qcCompositionLayer => qcCompositionLayer -> IO (Id QCComposition)
composition qcCompositionLayer  =
  sendMsg qcCompositionLayer (mkSelector "composition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compositionLayerWithFile:@
compositionLayerWithFileSelector :: Selector
compositionLayerWithFileSelector = mkSelector "compositionLayerWithFile:"

-- | @Selector@ for @compositionLayerWithComposition:@
compositionLayerWithCompositionSelector :: Selector
compositionLayerWithCompositionSelector = mkSelector "compositionLayerWithComposition:"

-- | @Selector@ for @initWithFile:@
initWithFileSelector :: Selector
initWithFileSelector = mkSelector "initWithFile:"

-- | @Selector@ for @initWithComposition:@
initWithCompositionSelector :: Selector
initWithCompositionSelector = mkSelector "initWithComposition:"

-- | @Selector@ for @composition@
compositionSelector :: Selector
compositionSelector = mkSelector "composition"

