{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A PHProjectSectionContent object contains all the elements and suggested layout information for a specific level of curation within a PHProjectSection. A section can provide multiple content objects, but only one is intended to be used in a project based on the amount of content detail desired.
--
-- Generated bindings for @PHProjectSectionContent@.
module ObjC.PhotosUI.PHProjectSectionContent
  ( PHProjectSectionContent
  , IsPHProjectSectionContent(..)
  , init_
  , new
  , elements
  , numberOfColumns
  , aspectRatio
  , cloudAssetIdentifiers
  , initSelector
  , newSelector
  , elementsSelector
  , numberOfColumnsSelector
  , aspectRatioSelector
  , cloudAssetIdentifiersSelector


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

import ObjC.PhotosUI.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Photos.Internal.Classes

-- | @- init@
init_ :: IsPHProjectSectionContent phProjectSectionContent => phProjectSectionContent -> IO (Id PHProjectSectionContent)
init_ phProjectSectionContent  =
  sendMsg phProjectSectionContent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHProjectSectionContent)
new  =
  do
    cls' <- getRequiredClass "PHProjectSectionContent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Array of asset, text, or journal entry elements contained in the content.
--
-- ObjC selector: @- elements@
elements :: IsPHProjectSectionContent phProjectSectionContent => phProjectSectionContent -> IO (Id NSArray)
elements phProjectSectionContent  =
  sendMsg phProjectSectionContent (mkSelector "elements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The suggested layout of the content is provided in resolution-independent "grid space" units where one grid space is the width of the defined project canvas divided by numberOfColumns. If a project represents a "fixed layout" (e.g., it was created from an existing Apple Book, Card, or Calendar) the specified numberOfColumns will always be 1.
--
-- ObjC selector: @- numberOfColumns@
numberOfColumns :: IsPHProjectSectionContent phProjectSectionContent => phProjectSectionContent -> IO CLong
numberOfColumns phProjectSectionContent  =
  sendMsg phProjectSectionContent (mkSelector "numberOfColumns") retCLong []

-- | Overall aspect ratio of the full content layout (width/height) to enable faithful replication in the project's layout.
--
-- ObjC selector: @- aspectRatio@
aspectRatio :: IsPHProjectSectionContent phProjectSectionContent => phProjectSectionContent -> IO CDouble
aspectRatio phProjectSectionContent  =
  sendMsg phProjectSectionContent (mkSelector "aspectRatio") retCDouble []

-- | Convenience for getting a single array of all cloud asset identifiers referenced in the content without needing to enumerate elements.
--
-- ObjC selector: @- cloudAssetIdentifiers@
cloudAssetIdentifiers :: IsPHProjectSectionContent phProjectSectionContent => phProjectSectionContent -> IO (Id NSArray)
cloudAssetIdentifiers phProjectSectionContent  =
  sendMsg phProjectSectionContent (mkSelector "cloudAssetIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @elements@
elementsSelector :: Selector
elementsSelector = mkSelector "elements"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @aspectRatio@
aspectRatioSelector :: Selector
aspectRatioSelector = mkSelector "aspectRatio"

-- | @Selector@ for @cloudAssetIdentifiers@
cloudAssetIdentifiersSelector :: Selector
cloudAssetIdentifiersSelector = mkSelector "cloudAssetIdentifiers"

