{-# LANGUAGE PatternSynonyms #-}
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
  , layoutAnchorWithEdgesSelector
  , layoutAnchorWithEdges_absoluteOffsetSelector
  , layoutAnchorWithEdges_fractionalOffsetSelector
  , initSelector
  , newSelector
  , edgesSelector
  , offsetSelector
  , isAbsoluteOffsetSelector
  , isFractionalOffsetSelector

  -- * Enum types
  , NSDirectionalRectEdge(NSDirectionalRectEdge)
  , pattern NSDirectionalRectEdgeNone
  , pattern NSDirectionalRectEdgeTop
  , pattern NSDirectionalRectEdgeLeading
  , pattern NSDirectionalRectEdgeBottom
  , pattern NSDirectionalRectEdgeTrailing
  , pattern NSDirectionalRectEdgeAll

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ layoutAnchorWithEdges:@
layoutAnchorWithEdges :: NSDirectionalRectEdge -> IO (Id NSCollectionLayoutAnchor)
layoutAnchorWithEdges edges =
  do
    cls' <- getRequiredClass "NSCollectionLayoutAnchor"
    sendClassMsg cls' (mkSelector "layoutAnchorWithEdges:") (retPtr retVoid) [argCULong (coerce edges)] >>= retainedObject . castPtr

-- | @+ layoutAnchorWithEdges:absoluteOffset:@
layoutAnchorWithEdges_absoluteOffset :: NSDirectionalRectEdge -> NSPoint -> IO (Id NSCollectionLayoutAnchor)
layoutAnchorWithEdges_absoluteOffset edges absoluteOffset =
  do
    cls' <- getRequiredClass "NSCollectionLayoutAnchor"
    sendClassMsg cls' (mkSelector "layoutAnchorWithEdges:absoluteOffset:") (retPtr retVoid) [argCULong (coerce edges), argNSPoint absoluteOffset] >>= retainedObject . castPtr

-- | @+ layoutAnchorWithEdges:fractionalOffset:@
layoutAnchorWithEdges_fractionalOffset :: NSDirectionalRectEdge -> NSPoint -> IO (Id NSCollectionLayoutAnchor)
layoutAnchorWithEdges_fractionalOffset edges fractionalOffset =
  do
    cls' <- getRequiredClass "NSCollectionLayoutAnchor"
    sendClassMsg cls' (mkSelector "layoutAnchorWithEdges:fractionalOffset:") (retPtr retVoid) [argCULong (coerce edges), argNSPoint fractionalOffset] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionLayoutAnchor nsCollectionLayoutAnchor => nsCollectionLayoutAnchor -> IO (Id NSCollectionLayoutAnchor)
init_ nsCollectionLayoutAnchor  =
  sendMsg nsCollectionLayoutAnchor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionLayoutAnchor)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutAnchor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- edges@
edges :: IsNSCollectionLayoutAnchor nsCollectionLayoutAnchor => nsCollectionLayoutAnchor -> IO NSDirectionalRectEdge
edges nsCollectionLayoutAnchor  =
  fmap (coerce :: CULong -> NSDirectionalRectEdge) $ sendMsg nsCollectionLayoutAnchor (mkSelector "edges") retCULong []

-- | @- offset@
offset :: IsNSCollectionLayoutAnchor nsCollectionLayoutAnchor => nsCollectionLayoutAnchor -> IO NSPoint
offset nsCollectionLayoutAnchor  =
  sendMsgStret nsCollectionLayoutAnchor (mkSelector "offset") retNSPoint []

-- | @- isAbsoluteOffset@
isAbsoluteOffset :: IsNSCollectionLayoutAnchor nsCollectionLayoutAnchor => nsCollectionLayoutAnchor -> IO Bool
isAbsoluteOffset nsCollectionLayoutAnchor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionLayoutAnchor (mkSelector "isAbsoluteOffset") retCULong []

-- | @- isFractionalOffset@
isFractionalOffset :: IsNSCollectionLayoutAnchor nsCollectionLayoutAnchor => nsCollectionLayoutAnchor -> IO Bool
isFractionalOffset nsCollectionLayoutAnchor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionLayoutAnchor (mkSelector "isFractionalOffset") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layoutAnchorWithEdges:@
layoutAnchorWithEdgesSelector :: Selector
layoutAnchorWithEdgesSelector = mkSelector "layoutAnchorWithEdges:"

-- | @Selector@ for @layoutAnchorWithEdges:absoluteOffset:@
layoutAnchorWithEdges_absoluteOffsetSelector :: Selector
layoutAnchorWithEdges_absoluteOffsetSelector = mkSelector "layoutAnchorWithEdges:absoluteOffset:"

-- | @Selector@ for @layoutAnchorWithEdges:fractionalOffset:@
layoutAnchorWithEdges_fractionalOffsetSelector :: Selector
layoutAnchorWithEdges_fractionalOffsetSelector = mkSelector "layoutAnchorWithEdges:fractionalOffset:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @edges@
edgesSelector :: Selector
edgesSelector = mkSelector "edges"

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @isAbsoluteOffset@
isAbsoluteOffsetSelector :: Selector
isAbsoluteOffsetSelector = mkSelector "isAbsoluteOffset"

-- | @Selector@ for @isFractionalOffset@
isFractionalOffsetSelector :: Selector
isFractionalOffsetSelector = mkSelector "isFractionalOffset"

