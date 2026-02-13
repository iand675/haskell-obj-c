{-# LANGUAGE DataKinds #-}
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
  , backgroundColor
  , aspectRatioSelector
  , backgroundColorSelector
  , cloudAssetIdentifiersSelector
  , elementsSelector
  , initSelector
  , newSelector
  , numberOfColumnsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PhotosUI.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Photos.Internal.Classes

-- | @- init@
init_ :: IsPHProjectSectionContent phProjectSectionContent => phProjectSectionContent -> IO (Id PHProjectSectionContent)
init_ phProjectSectionContent =
  sendOwnedMessage phProjectSectionContent initSelector

-- | @+ new@
new :: IO (Id PHProjectSectionContent)
new  =
  do
    cls' <- getRequiredClass "PHProjectSectionContent"
    sendOwnedClassMessage cls' newSelector

-- | Array of asset, text, or journal entry elements contained in the content.
--
-- ObjC selector: @- elements@
elements :: IsPHProjectSectionContent phProjectSectionContent => phProjectSectionContent -> IO (Id NSArray)
elements phProjectSectionContent =
  sendMessage phProjectSectionContent elementsSelector

-- | The suggested layout of the content is provided in resolution-independent "grid space" units where one grid space is the width of the defined project canvas divided by numberOfColumns. If a project represents a "fixed layout" (e.g., it was created from an existing Apple Book, Card, or Calendar) the specified numberOfColumns will always be 1.
--
-- ObjC selector: @- numberOfColumns@
numberOfColumns :: IsPHProjectSectionContent phProjectSectionContent => phProjectSectionContent -> IO CLong
numberOfColumns phProjectSectionContent =
  sendMessage phProjectSectionContent numberOfColumnsSelector

-- | Overall aspect ratio of the full content layout (width/height) to enable faithful replication in the project's layout.
--
-- ObjC selector: @- aspectRatio@
aspectRatio :: IsPHProjectSectionContent phProjectSectionContent => phProjectSectionContent -> IO CDouble
aspectRatio phProjectSectionContent =
  sendMessage phProjectSectionContent aspectRatioSelector

-- | Convenience for getting a single array of all cloud asset identifiers referenced in the content without needing to enumerate elements.
--
-- ObjC selector: @- cloudAssetIdentifiers@
cloudAssetIdentifiers :: IsPHProjectSectionContent phProjectSectionContent => phProjectSectionContent -> IO (Id NSArray)
cloudAssetIdentifiers phProjectSectionContent =
  sendMessage phProjectSectionContent cloudAssetIdentifiersSelector

-- | Background color of the section content. This property is only used when the user creates a new project from an existing Apple Print Product
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsPHProjectSectionContent phProjectSectionContent => phProjectSectionContent -> IO (Id NSColor)
backgroundColor phProjectSectionContent =
  sendMessage phProjectSectionContent backgroundColorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHProjectSectionContent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHProjectSectionContent)
newSelector = mkSelector "new"

-- | @Selector@ for @elements@
elementsSelector :: Selector '[] (Id NSArray)
elementsSelector = mkSelector "elements"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector '[] CLong
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @aspectRatio@
aspectRatioSelector :: Selector '[] CDouble
aspectRatioSelector = mkSelector "aspectRatio"

-- | @Selector@ for @cloudAssetIdentifiers@
cloudAssetIdentifiersSelector :: Selector '[] (Id NSArray)
cloudAssetIdentifiersSelector = mkSelector "cloudAssetIdentifiers"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

