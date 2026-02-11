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
  , initSelector
  , newSelector
  , frameSelector
  , zIndexSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ customItemWithFrame:@
customItemWithFrame :: NSRect -> IO (Id NSCollectionLayoutGroupCustomItem)
customItemWithFrame frame =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroupCustomItem"
    sendClassMsg cls' (mkSelector "customItemWithFrame:") (retPtr retVoid) [argNSRect frame] >>= retainedObject . castPtr

-- | @+ customItemWithFrame:zIndex:@
customItemWithFrame_zIndex :: NSRect -> CLong -> IO (Id NSCollectionLayoutGroupCustomItem)
customItemWithFrame_zIndex frame zIndex =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroupCustomItem"
    sendClassMsg cls' (mkSelector "customItemWithFrame:zIndex:") (retPtr retVoid) [argNSRect frame, argCLong (fromIntegral zIndex)] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionLayoutGroupCustomItem nsCollectionLayoutGroupCustomItem => nsCollectionLayoutGroupCustomItem -> IO (Id NSCollectionLayoutGroupCustomItem)
init_ nsCollectionLayoutGroupCustomItem  =
  sendMsg nsCollectionLayoutGroupCustomItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionLayoutGroupCustomItem)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroupCustomItem"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- frame@
frame :: IsNSCollectionLayoutGroupCustomItem nsCollectionLayoutGroupCustomItem => nsCollectionLayoutGroupCustomItem -> IO NSRect
frame nsCollectionLayoutGroupCustomItem  =
  sendMsgStret nsCollectionLayoutGroupCustomItem (mkSelector "frame") retNSRect []

-- | @- zIndex@
zIndex :: IsNSCollectionLayoutGroupCustomItem nsCollectionLayoutGroupCustomItem => nsCollectionLayoutGroupCustomItem -> IO CLong
zIndex nsCollectionLayoutGroupCustomItem  =
  sendMsg nsCollectionLayoutGroupCustomItem (mkSelector "zIndex") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @customItemWithFrame:@
customItemWithFrameSelector :: Selector
customItemWithFrameSelector = mkSelector "customItemWithFrame:"

-- | @Selector@ for @customItemWithFrame:zIndex:@
customItemWithFrame_zIndexSelector :: Selector
customItemWithFrame_zIndexSelector = mkSelector "customItemWithFrame:zIndex:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @frame@
frameSelector :: Selector
frameSelector = mkSelector "frame"

-- | @Selector@ for @zIndex@
zIndexSelector :: Selector
zIndexSelector = mkSelector "zIndex"

