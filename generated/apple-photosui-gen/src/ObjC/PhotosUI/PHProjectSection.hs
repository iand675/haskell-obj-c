{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A PHProjectSection object represents a collection of content for the project including asset elements and text elements. Each section contains one or more PHProjectSectionContent objects which provide suggested levels of "curation" for the content contained in the section. The number of sections included in PHProjectInfo will vary depending on the creation source at the time of the project initiation. For example: - if user creates a project from a Memory, there will be one cover section with a key asset element and titling, plus one section containing multiple levels of curation that mirror the "Show Summary" and "Show More" options of the Memory - if user creates a project from a single Album, the project info may only contain one section unless the album contains a large quantity of photos in which case, Photos may suggest section breaks based on Moments in the user's library - if user creates a project from an existing Apple Book, Card, or Calendar, the sections provided in the project info will exactly match the pagination in that project (e.g., one section per page in a book).
--
-- Generated bindings for @PHProjectSection@.
module ObjC.PhotosUI.PHProjectSection
  ( PHProjectSection
  , IsPHProjectSection(..)
  , init_
  , new
  , sectionContents
  , sectionType
  , title
  , initSelector
  , newSelector
  , sectionContentsSelector
  , sectionTypeSelector
  , titleSelector

  -- * Enum types
  , PHProjectSectionType(PHProjectSectionType)
  , pattern PHProjectSectionTypeUndefined
  , pattern PHProjectSectionTypeCover
  , pattern PHProjectSectionTypeContent
  , pattern PHProjectSectionTypeAuxiliary

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PhotosUI.Internal.Classes
import ObjC.PhotosUI.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHProjectSection phProjectSection => phProjectSection -> IO (Id PHProjectSection)
init_ phProjectSection =
  sendOwnedMessage phProjectSection initSelector

-- | @+ new@
new :: IO (Id PHProjectSection)
new  =
  do
    cls' <- getRequiredClass "PHProjectSection"
    sendOwnedClassMessage cls' newSelector

-- | Array containing one or more PHProjectSectionContent objects. Ordered by number of elements from least to most. Projects should only present one level of content to the user at a time as assets will be reused within individual content objects.
--
-- ObjC selector: @- sectionContents@
sectionContents :: IsPHProjectSection phProjectSection => phProjectSection -> IO (Id NSArray)
sectionContents phProjectSection =
  sendMessage phProjectSection sectionContentsSelector

-- | The intended usage of the section (e.g., cover, content, auxiliary)
--
-- ObjC selector: @- sectionType@
sectionType :: IsPHProjectSection phProjectSection => phProjectSection -> IO PHProjectSectionType
sectionType phProjectSection =
  sendMessage phProjectSection sectionTypeSelector

-- | Title for the section (e.g., a Moment name or a general geographical location), might be an empty string.
--
-- ObjC selector: @- title@
title :: IsPHProjectSection phProjectSection => phProjectSection -> IO (Id NSString)
title phProjectSection =
  sendMessage phProjectSection titleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHProjectSection)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHProjectSection)
newSelector = mkSelector "new"

-- | @Selector@ for @sectionContents@
sectionContentsSelector :: Selector '[] (Id NSArray)
sectionContentsSelector = mkSelector "sectionContents"

-- | @Selector@ for @sectionType@
sectionTypeSelector :: Selector '[] PHProjectSectionType
sectionTypeSelector = mkSelector "sectionType"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

