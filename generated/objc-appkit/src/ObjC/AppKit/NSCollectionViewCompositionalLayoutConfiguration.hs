{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewCompositionalLayoutConfiguration@.
module ObjC.AppKit.NSCollectionViewCompositionalLayoutConfiguration
  ( NSCollectionViewCompositionalLayoutConfiguration
  , IsNSCollectionViewCompositionalLayoutConfiguration(..)
  , scrollDirection
  , setScrollDirection
  , interSectionSpacing
  , setInterSectionSpacing
  , boundarySupplementaryItems
  , setBoundarySupplementaryItems
  , scrollDirectionSelector
  , setScrollDirectionSelector
  , interSectionSpacingSelector
  , setInterSectionSpacingSelector
  , boundarySupplementaryItemsSelector
  , setBoundarySupplementaryItemsSelector

  -- * Enum types
  , NSCollectionViewScrollDirection(NSCollectionViewScrollDirection)
  , pattern NSCollectionViewScrollDirectionVertical
  , pattern NSCollectionViewScrollDirectionHorizontal

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- scrollDirection@
scrollDirection :: IsNSCollectionViewCompositionalLayoutConfiguration nsCollectionViewCompositionalLayoutConfiguration => nsCollectionViewCompositionalLayoutConfiguration -> IO NSCollectionViewScrollDirection
scrollDirection nsCollectionViewCompositionalLayoutConfiguration  =
  fmap (coerce :: CLong -> NSCollectionViewScrollDirection) $ sendMsg nsCollectionViewCompositionalLayoutConfiguration (mkSelector "scrollDirection") retCLong []

-- | @- setScrollDirection:@
setScrollDirection :: IsNSCollectionViewCompositionalLayoutConfiguration nsCollectionViewCompositionalLayoutConfiguration => nsCollectionViewCompositionalLayoutConfiguration -> NSCollectionViewScrollDirection -> IO ()
setScrollDirection nsCollectionViewCompositionalLayoutConfiguration  value =
  sendMsg nsCollectionViewCompositionalLayoutConfiguration (mkSelector "setScrollDirection:") retVoid [argCLong (coerce value)]

-- | @- interSectionSpacing@
interSectionSpacing :: IsNSCollectionViewCompositionalLayoutConfiguration nsCollectionViewCompositionalLayoutConfiguration => nsCollectionViewCompositionalLayoutConfiguration -> IO CDouble
interSectionSpacing nsCollectionViewCompositionalLayoutConfiguration  =
  sendMsg nsCollectionViewCompositionalLayoutConfiguration (mkSelector "interSectionSpacing") retCDouble []

-- | @- setInterSectionSpacing:@
setInterSectionSpacing :: IsNSCollectionViewCompositionalLayoutConfiguration nsCollectionViewCompositionalLayoutConfiguration => nsCollectionViewCompositionalLayoutConfiguration -> CDouble -> IO ()
setInterSectionSpacing nsCollectionViewCompositionalLayoutConfiguration  value =
  sendMsg nsCollectionViewCompositionalLayoutConfiguration (mkSelector "setInterSectionSpacing:") retVoid [argCDouble (fromIntegral value)]

-- | @- boundarySupplementaryItems@
boundarySupplementaryItems :: IsNSCollectionViewCompositionalLayoutConfiguration nsCollectionViewCompositionalLayoutConfiguration => nsCollectionViewCompositionalLayoutConfiguration -> IO (Id NSArray)
boundarySupplementaryItems nsCollectionViewCompositionalLayoutConfiguration  =
  sendMsg nsCollectionViewCompositionalLayoutConfiguration (mkSelector "boundarySupplementaryItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBoundarySupplementaryItems:@
setBoundarySupplementaryItems :: (IsNSCollectionViewCompositionalLayoutConfiguration nsCollectionViewCompositionalLayoutConfiguration, IsNSArray value) => nsCollectionViewCompositionalLayoutConfiguration -> value -> IO ()
setBoundarySupplementaryItems nsCollectionViewCompositionalLayoutConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCollectionViewCompositionalLayoutConfiguration (mkSelector "setBoundarySupplementaryItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scrollDirection@
scrollDirectionSelector :: Selector
scrollDirectionSelector = mkSelector "scrollDirection"

-- | @Selector@ for @setScrollDirection:@
setScrollDirectionSelector :: Selector
setScrollDirectionSelector = mkSelector "setScrollDirection:"

-- | @Selector@ for @interSectionSpacing@
interSectionSpacingSelector :: Selector
interSectionSpacingSelector = mkSelector "interSectionSpacing"

-- | @Selector@ for @setInterSectionSpacing:@
setInterSectionSpacingSelector :: Selector
setInterSectionSpacingSelector = mkSelector "setInterSectionSpacing:"

-- | @Selector@ for @boundarySupplementaryItems@
boundarySupplementaryItemsSelector :: Selector
boundarySupplementaryItemsSelector = mkSelector "boundarySupplementaryItems"

-- | @Selector@ for @setBoundarySupplementaryItems:@
setBoundarySupplementaryItemsSelector :: Selector
setBoundarySupplementaryItemsSelector = mkSelector "setBoundarySupplementaryItems:"

