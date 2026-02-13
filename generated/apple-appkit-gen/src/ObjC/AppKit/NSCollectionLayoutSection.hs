{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , boundarySupplementaryItemsSelector
  , contentInsetsSelector
  , decorationItemsSelector
  , initSelector
  , interGroupSpacingSelector
  , newSelector
  , orthogonalScrollingBehaviorSelector
  , sectionWithGroupSelector
  , setBoundarySupplementaryItemsSelector
  , setContentInsetsSelector
  , setDecorationItemsSelector
  , setInterGroupSpacingSelector
  , setOrthogonalScrollingBehaviorSelector
  , setSupplementariesFollowContentInsetsSelector
  , setVisibleItemsInvalidationHandlerSelector
  , supplementariesFollowContentInsetsSelector
  , visibleItemsInvalidationHandlerSelector

  -- * Enum types
  , NSCollectionLayoutSectionOrthogonalScrollingBehavior(NSCollectionLayoutSectionOrthogonalScrollingBehavior)
  , pattern NSCollectionLayoutSectionOrthogonalScrollingBehaviorNone
  , pattern NSCollectionLayoutSectionOrthogonalScrollingBehaviorContinuous
  , pattern NSCollectionLayoutSectionOrthogonalScrollingBehaviorContinuousGroupLeadingBoundary
  , pattern NSCollectionLayoutSectionOrthogonalScrollingBehaviorPaging
  , pattern NSCollectionLayoutSectionOrthogonalScrollingBehaviorGroupPaging
  , pattern NSCollectionLayoutSectionOrthogonalScrollingBehaviorGroupPagingCentered

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sectionWithGroupSelector (toNSCollectionLayoutGroup group)

-- | @- init@
init_ :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO (Id NSCollectionLayoutSection)
init_ nsCollectionLayoutSection =
  sendOwnedMessage nsCollectionLayoutSection initSelector

-- | @+ new@
new :: IO (Id NSCollectionLayoutSection)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSection"
    sendOwnedClassMessage cls' newSelector

-- | @- contentInsets@
contentInsets :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO NSDirectionalEdgeInsets
contentInsets nsCollectionLayoutSection =
  sendMessage nsCollectionLayoutSection contentInsetsSelector

-- | @- setContentInsets:@
setContentInsets :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> NSDirectionalEdgeInsets -> IO ()
setContentInsets nsCollectionLayoutSection value =
  sendMessage nsCollectionLayoutSection setContentInsetsSelector value

-- | @- interGroupSpacing@
interGroupSpacing :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO CDouble
interGroupSpacing nsCollectionLayoutSection =
  sendMessage nsCollectionLayoutSection interGroupSpacingSelector

-- | @- setInterGroupSpacing:@
setInterGroupSpacing :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> CDouble -> IO ()
setInterGroupSpacing nsCollectionLayoutSection value =
  sendMessage nsCollectionLayoutSection setInterGroupSpacingSelector value

-- | @- orthogonalScrollingBehavior@
orthogonalScrollingBehavior :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO NSCollectionLayoutSectionOrthogonalScrollingBehavior
orthogonalScrollingBehavior nsCollectionLayoutSection =
  sendMessage nsCollectionLayoutSection orthogonalScrollingBehaviorSelector

-- | @- setOrthogonalScrollingBehavior:@
setOrthogonalScrollingBehavior :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> NSCollectionLayoutSectionOrthogonalScrollingBehavior -> IO ()
setOrthogonalScrollingBehavior nsCollectionLayoutSection value =
  sendMessage nsCollectionLayoutSection setOrthogonalScrollingBehaviorSelector value

-- | @- boundarySupplementaryItems@
boundarySupplementaryItems :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO (Id NSArray)
boundarySupplementaryItems nsCollectionLayoutSection =
  sendMessage nsCollectionLayoutSection boundarySupplementaryItemsSelector

-- | @- setBoundarySupplementaryItems:@
setBoundarySupplementaryItems :: (IsNSCollectionLayoutSection nsCollectionLayoutSection, IsNSArray value) => nsCollectionLayoutSection -> value -> IO ()
setBoundarySupplementaryItems nsCollectionLayoutSection value =
  sendMessage nsCollectionLayoutSection setBoundarySupplementaryItemsSelector (toNSArray value)

-- | @- supplementariesFollowContentInsets@
supplementariesFollowContentInsets :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO Bool
supplementariesFollowContentInsets nsCollectionLayoutSection =
  sendMessage nsCollectionLayoutSection supplementariesFollowContentInsetsSelector

-- | @- setSupplementariesFollowContentInsets:@
setSupplementariesFollowContentInsets :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> Bool -> IO ()
setSupplementariesFollowContentInsets nsCollectionLayoutSection value =
  sendMessage nsCollectionLayoutSection setSupplementariesFollowContentInsetsSelector value

-- | @- visibleItemsInvalidationHandler@
visibleItemsInvalidationHandler :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO (Ptr ())
visibleItemsInvalidationHandler nsCollectionLayoutSection =
  sendMessage nsCollectionLayoutSection visibleItemsInvalidationHandlerSelector

-- | @- setVisibleItemsInvalidationHandler:@
setVisibleItemsInvalidationHandler :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> Ptr () -> IO ()
setVisibleItemsInvalidationHandler nsCollectionLayoutSection value =
  sendMessage nsCollectionLayoutSection setVisibleItemsInvalidationHandlerSelector value

-- | @- decorationItems@
decorationItems :: IsNSCollectionLayoutSection nsCollectionLayoutSection => nsCollectionLayoutSection -> IO (Id NSArray)
decorationItems nsCollectionLayoutSection =
  sendMessage nsCollectionLayoutSection decorationItemsSelector

-- | @- setDecorationItems:@
setDecorationItems :: (IsNSCollectionLayoutSection nsCollectionLayoutSection, IsNSArray value) => nsCollectionLayoutSection -> value -> IO ()
setDecorationItems nsCollectionLayoutSection value =
  sendMessage nsCollectionLayoutSection setDecorationItemsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sectionWithGroup:@
sectionWithGroupSelector :: Selector '[Id NSCollectionLayoutGroup] (Id NSCollectionLayoutSection)
sectionWithGroupSelector = mkSelector "sectionWithGroup:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionLayoutSection)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionLayoutSection)
newSelector = mkSelector "new"

-- | @Selector@ for @contentInsets@
contentInsetsSelector :: Selector '[] NSDirectionalEdgeInsets
contentInsetsSelector = mkSelector "contentInsets"

-- | @Selector@ for @setContentInsets:@
setContentInsetsSelector :: Selector '[NSDirectionalEdgeInsets] ()
setContentInsetsSelector = mkSelector "setContentInsets:"

-- | @Selector@ for @interGroupSpacing@
interGroupSpacingSelector :: Selector '[] CDouble
interGroupSpacingSelector = mkSelector "interGroupSpacing"

-- | @Selector@ for @setInterGroupSpacing:@
setInterGroupSpacingSelector :: Selector '[CDouble] ()
setInterGroupSpacingSelector = mkSelector "setInterGroupSpacing:"

-- | @Selector@ for @orthogonalScrollingBehavior@
orthogonalScrollingBehaviorSelector :: Selector '[] NSCollectionLayoutSectionOrthogonalScrollingBehavior
orthogonalScrollingBehaviorSelector = mkSelector "orthogonalScrollingBehavior"

-- | @Selector@ for @setOrthogonalScrollingBehavior:@
setOrthogonalScrollingBehaviorSelector :: Selector '[NSCollectionLayoutSectionOrthogonalScrollingBehavior] ()
setOrthogonalScrollingBehaviorSelector = mkSelector "setOrthogonalScrollingBehavior:"

-- | @Selector@ for @boundarySupplementaryItems@
boundarySupplementaryItemsSelector :: Selector '[] (Id NSArray)
boundarySupplementaryItemsSelector = mkSelector "boundarySupplementaryItems"

-- | @Selector@ for @setBoundarySupplementaryItems:@
setBoundarySupplementaryItemsSelector :: Selector '[Id NSArray] ()
setBoundarySupplementaryItemsSelector = mkSelector "setBoundarySupplementaryItems:"

-- | @Selector@ for @supplementariesFollowContentInsets@
supplementariesFollowContentInsetsSelector :: Selector '[] Bool
supplementariesFollowContentInsetsSelector = mkSelector "supplementariesFollowContentInsets"

-- | @Selector@ for @setSupplementariesFollowContentInsets:@
setSupplementariesFollowContentInsetsSelector :: Selector '[Bool] ()
setSupplementariesFollowContentInsetsSelector = mkSelector "setSupplementariesFollowContentInsets:"

-- | @Selector@ for @visibleItemsInvalidationHandler@
visibleItemsInvalidationHandlerSelector :: Selector '[] (Ptr ())
visibleItemsInvalidationHandlerSelector = mkSelector "visibleItemsInvalidationHandler"

-- | @Selector@ for @setVisibleItemsInvalidationHandler:@
setVisibleItemsInvalidationHandlerSelector :: Selector '[Ptr ()] ()
setVisibleItemsInvalidationHandlerSelector = mkSelector "setVisibleItemsInvalidationHandler:"

-- | @Selector@ for @decorationItems@
decorationItemsSelector :: Selector '[] (Id NSArray)
decorationItemsSelector = mkSelector "decorationItems"

-- | @Selector@ for @setDecorationItems:@
setDecorationItemsSelector :: Selector '[Id NSArray] ()
setDecorationItemsSelector = mkSelector "setDecorationItems:"

