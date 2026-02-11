{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutSection@.
module ObjC.AppKit.NSCollectionLayoutSection
  ( NSCollectionLayoutSection
  , IsNSCollectionLayoutSection(..)
  , sectionWithGroup
  , init_
  , new
  , contentInsets
  , setContentInsets
  , interGroupSpacing
  , setInterGroupSpacing
  , orthogonalScrollingBehavior
  , setOrthogonalScrollingBehavior
  , boundarySupplementaryItems
  , setBoundarySupplementaryItems
  , supplementariesFollowContentInsets
  , setSupplementariesFollowContentInsets
  , visibleItemsInvalidationHandler
  , setVisibleItemsInvalidationHandler
  , decorationItems
  , setDecorationItems
  , sectionWithGroupSelector
  , initSelector
  , newSelector
  , contentInsetsSelector
  , setContentInsetsSelector
  , interGroupSpacingSelector
  , setInterGroupSpacingSelector
  , orthogonalScrollingBehaviorSelector
  , setOrthogonalScrollingBehaviorSelector
  , boundarySupplementaryItemsSelector
  , setBoundarySupplementaryItemsSelector
  , supplementariesFollowContentInsetsSelector
  , setSupplementariesFollowContentInsetsSelector
  , visibleItemsInvalidationHandlerSelector
  , setVisibleItemsInvalidationHandlerSelector
  , decorationItemsSelector
  , setDecorationItemsSelector

  -- * Enum types
  , NSCollectionLayoutSectionOrthogonalScrollingBehavior(NSCollectionLayoutSectionOrthogonalScrollingBehavior)
  , pattern NSCollectionLayoutSectionOrthogonalScrollingBehaviorNone
  , pattern NSCollectionLayoutSectionOrthogonalScrollingBehaviorContinuous
  , pattern NSCollectionLayoutSectionOrthogonalScrollingBehaviorContinuousGroupLeadingBoundary
  , pattern NSCollectionLayoutSectionOrthogonalScrollingBehaviorPaging
  , pattern NSCollectionLayoutSectionOrthogonalScrollingBehaviorGroupPaging
  , pattern NSCollectionLayoutSectionOrthogonalScrollingBehaviorGroupPagingCentered

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
import ObjC.AppKit.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ sectionWithGroup:@
sectionWithGroup :: IsNSCollectionLayoutGroup group => group -> IO (Id NSCollectionLayoutSection)
sectionWithGroup group =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSection"
    withObjCPtr group $ \raw_group ->
      sendClassMsg cls' (mkSelector "sectionWithGroup:") (retPtr retVoid) [argPtr (castPtr raw_group :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO (Id NSCollectionLayoutSection)
init_ nsCollectionLayoutSection  =
  sendMsg nsCollectionLayoutSection (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionLayoutSection)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSection"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- contentInsets@
contentInsets :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO NSDirectionalEdgeInsets
contentInsets nsCollectionLayoutSection  =
  sendMsgStret nsCollectionLayoutSection (mkSelector "contentInsets") retNSDirectionalEdgeInsets []

-- | @- setContentInsets:@
setContentInsets :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> NSDirectionalEdgeInsets -> IO ()
setContentInsets nsCollectionLayoutSection  value =
  sendMsg nsCollectionLayoutSection (mkSelector "setContentInsets:") retVoid [argNSDirectionalEdgeInsets value]

-- | @- interGroupSpacing@
interGroupSpacing :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO CDouble
interGroupSpacing nsCollectionLayoutSection  =
  sendMsg nsCollectionLayoutSection (mkSelector "interGroupSpacing") retCDouble []

-- | @- setInterGroupSpacing:@
setInterGroupSpacing :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> CDouble -> IO ()
setInterGroupSpacing nsCollectionLayoutSection  value =
  sendMsg nsCollectionLayoutSection (mkSelector "setInterGroupSpacing:") retVoid [argCDouble (fromIntegral value)]

-- | @- orthogonalScrollingBehavior@
orthogonalScrollingBehavior :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO NSCollectionLayoutSectionOrthogonalScrollingBehavior
orthogonalScrollingBehavior nsCollectionLayoutSection  =
  fmap (coerce :: CLong -> NSCollectionLayoutSectionOrthogonalScrollingBehavior) $ sendMsg nsCollectionLayoutSection (mkSelector "orthogonalScrollingBehavior") retCLong []

-- | @- setOrthogonalScrollingBehavior:@
setOrthogonalScrollingBehavior :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> NSCollectionLayoutSectionOrthogonalScrollingBehavior -> IO ()
setOrthogonalScrollingBehavior nsCollectionLayoutSection  value =
  sendMsg nsCollectionLayoutSection (mkSelector "setOrthogonalScrollingBehavior:") retVoid [argCLong (coerce value)]

-- | @- boundarySupplementaryItems@
boundarySupplementaryItems :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO (Id NSArray)
boundarySupplementaryItems nsCollectionLayoutSection  =
  sendMsg nsCollectionLayoutSection (mkSelector "boundarySupplementaryItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBoundarySupplementaryItems:@
setBoundarySupplementaryItems :: (IsNSCollectionLayoutSection nsCollectionLayoutSection, IsNSArray value) => nsCollectionLayoutSection -> value -> IO ()
setBoundarySupplementaryItems nsCollectionLayoutSection  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCollectionLayoutSection (mkSelector "setBoundarySupplementaryItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supplementariesFollowContentInsets@
supplementariesFollowContentInsets :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO Bool
supplementariesFollowContentInsets nsCollectionLayoutSection  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionLayoutSection (mkSelector "supplementariesFollowContentInsets") retCULong []

-- | @- setSupplementariesFollowContentInsets:@
setSupplementariesFollowContentInsets :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> Bool -> IO ()
setSupplementariesFollowContentInsets nsCollectionLayoutSection  value =
  sendMsg nsCollectionLayoutSection (mkSelector "setSupplementariesFollowContentInsets:") retVoid [argCULong (if value then 1 else 0)]

-- | @- visibleItemsInvalidationHandler@
visibleItemsInvalidationHandler :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO (Ptr ())
visibleItemsInvalidationHandler nsCollectionLayoutSection  =
  fmap castPtr $ sendMsg nsCollectionLayoutSection (mkSelector "visibleItemsInvalidationHandler") (retPtr retVoid) []

-- | @- setVisibleItemsInvalidationHandler:@
setVisibleItemsInvalidationHandler :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> Ptr () -> IO ()
setVisibleItemsInvalidationHandler nsCollectionLayoutSection  value =
  sendMsg nsCollectionLayoutSection (mkSelector "setVisibleItemsInvalidationHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- decorationItems@
decorationItems :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO (Id NSArray)
decorationItems nsCollectionLayoutSection  =
  sendMsg nsCollectionLayoutSection (mkSelector "decorationItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDecorationItems:@
setDecorationItems :: (IsNSCollectionLayoutSection nsCollectionLayoutSection, IsNSArray value) => nsCollectionLayoutSection -> value -> IO ()
setDecorationItems nsCollectionLayoutSection  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCollectionLayoutSection (mkSelector "setDecorationItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sectionWithGroup:@
sectionWithGroupSelector :: Selector
sectionWithGroupSelector = mkSelector "sectionWithGroup:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @contentInsets@
contentInsetsSelector :: Selector
contentInsetsSelector = mkSelector "contentInsets"

-- | @Selector@ for @setContentInsets:@
setContentInsetsSelector :: Selector
setContentInsetsSelector = mkSelector "setContentInsets:"

-- | @Selector@ for @interGroupSpacing@
interGroupSpacingSelector :: Selector
interGroupSpacingSelector = mkSelector "interGroupSpacing"

-- | @Selector@ for @setInterGroupSpacing:@
setInterGroupSpacingSelector :: Selector
setInterGroupSpacingSelector = mkSelector "setInterGroupSpacing:"

-- | @Selector@ for @orthogonalScrollingBehavior@
orthogonalScrollingBehaviorSelector :: Selector
orthogonalScrollingBehaviorSelector = mkSelector "orthogonalScrollingBehavior"

-- | @Selector@ for @setOrthogonalScrollingBehavior:@
setOrthogonalScrollingBehaviorSelector :: Selector
setOrthogonalScrollingBehaviorSelector = mkSelector "setOrthogonalScrollingBehavior:"

-- | @Selector@ for @boundarySupplementaryItems@
boundarySupplementaryItemsSelector :: Selector
boundarySupplementaryItemsSelector = mkSelector "boundarySupplementaryItems"

-- | @Selector@ for @setBoundarySupplementaryItems:@
setBoundarySupplementaryItemsSelector :: Selector
setBoundarySupplementaryItemsSelector = mkSelector "setBoundarySupplementaryItems:"

-- | @Selector@ for @supplementariesFollowContentInsets@
supplementariesFollowContentInsetsSelector :: Selector
supplementariesFollowContentInsetsSelector = mkSelector "supplementariesFollowContentInsets"

-- | @Selector@ for @setSupplementariesFollowContentInsets:@
setSupplementariesFollowContentInsetsSelector :: Selector
setSupplementariesFollowContentInsetsSelector = mkSelector "setSupplementariesFollowContentInsets:"

-- | @Selector@ for @visibleItemsInvalidationHandler@
visibleItemsInvalidationHandlerSelector :: Selector
visibleItemsInvalidationHandlerSelector = mkSelector "visibleItemsInvalidationHandler"

-- | @Selector@ for @setVisibleItemsInvalidationHandler:@
setVisibleItemsInvalidationHandlerSelector :: Selector
setVisibleItemsInvalidationHandlerSelector = mkSelector "setVisibleItemsInvalidationHandler:"

-- | @Selector@ for @decorationItems@
decorationItemsSelector :: Selector
decorationItemsSelector = mkSelector "decorationItems"

-- | @Selector@ for @setDecorationItems:@
setDecorationItemsSelector :: Selector
setDecorationItemsSelector = mkSelector "setDecorationItems:"

