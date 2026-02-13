{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutAnchor@.
module ObjC.AppKit.NSCollectionLayoutAnchor
  ( NSCollectionLayoutAnchor
  , IsNSCollectionLayoutAnchor(..)
  , layoutAnchorWithEdges
  , layoutAnchorWithEdges_absoluteOffset
  , layoutAnchorWithEdges_fractionalOffset
  , init_
  , new
  , edges
  , offset
  , isAbsoluteOffset
  , isFractionalOffset
  , edgesSelector
  , initSelector
  , isAbsoluteOffsetSelector
  , isFractionalOffsetSelector
  , layoutAnchorWithEdgesSelector
  , layoutAnchorWithEdges_absoluteOffsetSelector
  , layoutAnchorWithEdges_fractionalOffsetSelector
  , newSelector
  , offsetSelector

  -- * Enum types
  , NSDirectionalRectEdge(NSDirectionalRectEdge)
  , pattern NSDirectionalRectEdgeNone
  , pattern NSDirectionalRectEdgeTop
  , pattern NSDirectionalRectEdgeLeading
  , pattern NSDirectionalRectEdgeBottom
  , pattern NSDirectionalRectEdgeTrailing
  , pattern NSDirectionalRectEdgeAll

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ layoutAnchorWithEdges:@
layoutAnchorWithEdges :: NSDirectionalRectEdge -> IO (Id NSCollectionLayoutAnchor)
layoutAnchorWithEdges edges =
  do
    cls' <- getRequiredClass "NSCollectionLayoutAnchor"
    sendClassMessage cls' layoutAnchorWithEdgesSelector edges

-- | @+ layoutAnchorWithEdges:absoluteOffset:@
layoutAnchorWithEdges_absoluteOffset :: NSDirectionalRectEdge -> NSPoint -> IO (Id NSCollectionLayoutAnchor)
layoutAnchorWithEdges_absoluteOffset edges absoluteOffset =
  do
    cls' <- getRequiredClass "NSCollectionLayoutAnchor"
    sendClassMessage cls' layoutAnchorWithEdges_absoluteOffsetSelector edges absoluteOffset

-- | @+ layoutAnchorWithEdges:fractionalOffset:@
layoutAnchorWithEdges_fractionalOffset :: NSDirectionalRectEdge -> NSPoint -> IO (Id NSCollectionLayoutAnchor)
layoutAnchorWithEdges_fractionalOffset edges fractionalOffset =
  do
    cls' <- getRequiredClass "NSCollectionLayoutAnchor"
    sendClassMessage cls' layoutAnchorWithEdges_fractionalOffsetSelector edges fractionalOffset

-- | @- init@
init_ :: IsNSCollectionLayoutAnchor nsCollectionLayoutAnchor => nsCollectionLayoutAnchor -> IO (Id NSCollectionLayoutAnchor)
init_ nsCollectionLayoutAnchor =
  sendOwnedMessage nsCollectionLayoutAnchor initSelector

-- | @+ new@
new :: IO (Id NSCollectionLayoutAnchor)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutAnchor"
    sendOwnedClassMessage cls' newSelector

-- | @- edges@
edges :: IsNSCollectionLayoutAnchor nsCollectionLayoutAnchor => nsCollectionLayoutAnchor -> IO NSDirectionalRectEdge
edges nsCollectionLayoutAnchor =
  sendMessage nsCollectionLayoutAnchor edgesSelector

-- | @- offset@
offset :: IsNSCollectionLayoutAnchor nsCollectionLayoutAnchor => nsCollectionLayoutAnchor -> IO NSPoint
offset nsCollectionLayoutAnchor =
  sendMessage nsCollectionLayoutAnchor offsetSelector

-- | @- isAbsoluteOffset@
isAbsoluteOffset :: IsNSCollectionLayoutAnchor nsCollectionLayoutAnchor => nsCollectionLayoutAnchor -> IO Bool
isAbsoluteOffset nsCollectionLayoutAnchor =
  sendMessage nsCollectionLayoutAnchor isAbsoluteOffsetSelector

-- | @- isFractionalOffset@
isFractionalOffset :: IsNSCollectionLayoutAnchor nsCollectionLayoutAnchor => nsCollectionLayoutAnchor -> IO Bool
isFractionalOffset nsCollectionLayoutAnchor =
  sendMessage nsCollectionLayoutAnchor isFractionalOffsetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layoutAnchorWithEdges:@
layoutAnchorWithEdgesSelector :: Selector '[NSDirectionalRectEdge] (Id NSCollectionLayoutAnchor)
layoutAnchorWithEdgesSelector = mkSelector "layoutAnchorWithEdges:"

-- | @Selector@ for @layoutAnchorWithEdges:absoluteOffset:@
layoutAnchorWithEdges_absoluteOffsetSelector :: Selector '[NSDirectionalRectEdge, NSPoint] (Id NSCollectionLayoutAnchor)
layoutAnchorWithEdges_absoluteOffsetSelector = mkSelector "layoutAnchorWithEdges:absoluteOffset:"

-- | @Selector@ for @layoutAnchorWithEdges:fractionalOffset:@
layoutAnchorWithEdges_fractionalOffsetSelector :: Selector '[NSDirectionalRectEdge, NSPoint] (Id NSCollectionLayoutAnchor)
layoutAnchorWithEdges_fractionalOffsetSelector = mkSelector "layoutAnchorWithEdges:fractionalOffset:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionLayoutAnchor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionLayoutAnchor)
newSelector = mkSelector "new"

-- | @Selector@ for @edges@
edgesSelector :: Selector '[] NSDirectionalRectEdge
edgesSelector = mkSelector "edges"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] NSPoint
offsetSelector = mkSelector "offset"

-- | @Selector@ for @isAbsoluteOffset@
isAbsoluteOffsetSelector :: Selector '[] Bool
isAbsoluteOffsetSelector = mkSelector "isAbsoluteOffset"

-- | @Selector@ for @isFractionalOffset@
isFractionalOffsetSelector :: Selector '[] Bool
isFractionalOffsetSelector = mkSelector "isFractionalOffset"

