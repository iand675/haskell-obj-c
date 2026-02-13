{-# LANGUAGE DataKinds #-}
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
  , compositionLayerWithCompositionSelector
  , compositionLayerWithFileSelector
  , compositionSelector
  , initWithCompositionSelector
  , initWithFileSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' compositionLayerWithFileSelector (toNSString path)

-- | @+ compositionLayerWithComposition:@
compositionLayerWithComposition :: IsQCComposition composition => composition -> IO (Id QCCompositionLayer)
compositionLayerWithComposition composition =
  do
    cls' <- getRequiredClass "QCCompositionLayer"
    sendClassMessage cls' compositionLayerWithCompositionSelector (toQCComposition composition)

-- | @- initWithFile:@
initWithFile :: (IsQCCompositionLayer qcCompositionLayer, IsNSString path) => qcCompositionLayer -> path -> IO RawId
initWithFile qcCompositionLayer path =
  sendOwnedMessage qcCompositionLayer initWithFileSelector (toNSString path)

-- | @- initWithComposition:@
initWithComposition :: (IsQCCompositionLayer qcCompositionLayer, IsQCComposition composition) => qcCompositionLayer -> composition -> IO RawId
initWithComposition qcCompositionLayer composition =
  sendOwnedMessage qcCompositionLayer initWithCompositionSelector (toQCComposition composition)

-- | @- composition@
composition :: IsQCCompositionLayer qcCompositionLayer => qcCompositionLayer -> IO (Id QCComposition)
composition qcCompositionLayer =
  sendMessage qcCompositionLayer compositionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compositionLayerWithFile:@
compositionLayerWithFileSelector :: Selector '[Id NSString] (Id QCCompositionLayer)
compositionLayerWithFileSelector = mkSelector "compositionLayerWithFile:"

-- | @Selector@ for @compositionLayerWithComposition:@
compositionLayerWithCompositionSelector :: Selector '[Id QCComposition] (Id QCCompositionLayer)
compositionLayerWithCompositionSelector = mkSelector "compositionLayerWithComposition:"

-- | @Selector@ for @initWithFile:@
initWithFileSelector :: Selector '[Id NSString] RawId
initWithFileSelector = mkSelector "initWithFile:"

-- | @Selector@ for @initWithComposition:@
initWithCompositionSelector :: Selector '[Id QCComposition] RawId
initWithCompositionSelector = mkSelector "initWithComposition:"

-- | @Selector@ for @composition@
compositionSelector :: Selector '[] (Id QCComposition)
compositionSelector = mkSelector "composition"

