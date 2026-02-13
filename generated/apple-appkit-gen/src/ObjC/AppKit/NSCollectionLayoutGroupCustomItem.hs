{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutGroupCustomItem@.
module ObjC.AppKit.NSCollectionLayoutGroupCustomItem
  ( NSCollectionLayoutGroupCustomItem
  , IsNSCollectionLayoutGroupCustomItem(..)
  , customItemWithFrame
  , customItemWithFrame_zIndex
  , init_
  , new
  , frame
  , zIndex
  , customItemWithFrameSelector
  , customItemWithFrame_zIndexSelector
  , frameSelector
  , initSelector
  , newSelector
  , zIndexSelector


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

-- | @+ customItemWithFrame:@
customItemWithFrame :: NSRect -> IO (Id NSCollectionLayoutGroupCustomItem)
customItemWithFrame frame =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroupCustomItem"
    sendClassMessage cls' customItemWithFrameSelector frame

-- | @+ customItemWithFrame:zIndex:@
customItemWithFrame_zIndex :: NSRect -> CLong -> IO (Id NSCollectionLayoutGroupCustomItem)
customItemWithFrame_zIndex frame zIndex =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroupCustomItem"
    sendClassMessage cls' customItemWithFrame_zIndexSelector frame zIndex

-- | @- init@
init_ :: IsNSCollectionLayoutGroupCustomItem nsCollectionLayoutGroupCustomItem => nsCollectionLayoutGroupCustomItem -> IO (Id NSCollectionLayoutGroupCustomItem)
init_ nsCollectionLayoutGroupCustomItem =
  sendOwnedMessage nsCollectionLayoutGroupCustomItem initSelector

-- | @+ new@
new :: IO (Id NSCollectionLayoutGroupCustomItem)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroupCustomItem"
    sendOwnedClassMessage cls' newSelector

-- | @- frame@
frame :: IsNSCollectionLayoutGroupCustomItem nsCollectionLayoutGroupCustomItem => nsCollectionLayoutGroupCustomItem -> IO NSRect
frame nsCollectionLayoutGroupCustomItem =
  sendMessage nsCollectionLayoutGroupCustomItem frameSelector

-- | @- zIndex@
zIndex :: IsNSCollectionLayoutGroupCustomItem nsCollectionLayoutGroupCustomItem => nsCollectionLayoutGroupCustomItem -> IO CLong
zIndex nsCollectionLayoutGroupCustomItem =
  sendMessage nsCollectionLayoutGroupCustomItem zIndexSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @customItemWithFrame:@
customItemWithFrameSelector :: Selector '[NSRect] (Id NSCollectionLayoutGroupCustomItem)
customItemWithFrameSelector = mkSelector "customItemWithFrame:"

-- | @Selector@ for @customItemWithFrame:zIndex:@
customItemWithFrame_zIndexSelector :: Selector '[NSRect, CLong] (Id NSCollectionLayoutGroupCustomItem)
customItemWithFrame_zIndexSelector = mkSelector "customItemWithFrame:zIndex:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionLayoutGroupCustomItem)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionLayoutGroupCustomItem)
newSelector = mkSelector "new"

-- | @Selector@ for @frame@
frameSelector :: Selector '[] NSRect
frameSelector = mkSelector "frame"

-- | @Selector@ for @zIndex@
zIndexSelector :: Selector '[] CLong
zIndexSelector = mkSelector "zIndex"

