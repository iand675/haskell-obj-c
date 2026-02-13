{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A PHProjectInfo object is created by Photos and passed along with a PHProjectExtensionContext any time Photos creates a new project. It comprises the complete content description which a Photos Project Extension can leverage to influence things like project layout, auto-flow, or theme selection. The properties in this class are inmutable and the class cannot be instatiated by an extension directly.
--
-- Generated bindings for @PHProjectInfo@.
module ObjC.PhotosUI.PHProjectInfo
  ( PHProjectInfo
  , IsPHProjectInfo(..)
  , init_
  , new
  , creationSource
  , projectType
  , sections
  , brandingEnabled
  , pageNumbersEnabled
  , productIdentifier
  , themeIdentifier
  , brandingEnabledSelector
  , creationSourceSelector
  , initSelector
  , newSelector
  , pageNumbersEnabledSelector
  , productIdentifierSelector
  , projectTypeSelector
  , sectionsSelector
  , themeIdentifierSelector

  -- * Enum types
  , PHProjectCreationSource(PHProjectCreationSource)
  , pattern PHProjectCreationSourceUndefined
  , pattern PHProjectCreationSourceUserSelection
  , pattern PHProjectCreationSourceAlbum
  , pattern PHProjectCreationSourceMemory
  , pattern PHProjectCreationSourceMoment
  , pattern PHProjectCreationSourceProject
  , pattern PHProjectCreationSourceProjectBook
  , pattern PHProjectCreationSourceProjectCalendar
  , pattern PHProjectCreationSourceProjectCard
  , pattern PHProjectCreationSourceProjectPrintOrder
  , pattern PHProjectCreationSourceProjectSlideshow
  , pattern PHProjectCreationSourceProjectExtension

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
init_ :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO (Id PHProjectInfo)
init_ phProjectInfo =
  sendOwnedMessage phProjectInfo initSelector

-- | @+ new@
new :: IO (Id PHProjectInfo)
new  =
  do
    cls' <- getRequiredClass "PHProjectInfo"
    sendOwnedClassMessage cls' newSelector

-- | Source from which the project was created.
--
-- ObjC selector: @- creationSource@
creationSource :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO PHProjectCreationSource
creationSource phProjectInfo =
  sendMessage phProjectInfo creationSourceSelector

-- | Selected projectType value from the extensions options as defined in -[PHProjectExtensionController supportedProjectTypes]. See PHProjectExtensionController.h for more information on configuring the options.
--
-- ObjC selector: @- projectType@
projectType :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO (Id NSString)
projectType phProjectInfo =
  sendMessage phProjectInfo projectTypeSelector

-- | Array of project sections each containing one or more PHProjectSectionContent objects.
--
-- ObjC selector: @- sections@
sections :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO (Id NSArray)
sections phProjectInfo =
  sendMessage phProjectInfo sectionsSelector

-- | The following properties are only used when the user creates a new project from an existing Apple Print Product.
--
-- YES if the source project had branding enabled.
--
-- ObjC selector: @- brandingEnabled@
brandingEnabled :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO Bool
brandingEnabled phProjectInfo =
  sendMessage phProjectInfo brandingEnabledSelector

-- | YES if the source project had page numbers enabled.
--
-- ObjC selector: @- pageNumbersEnabled@
pageNumbersEnabled :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO Bool
pageNumbersEnabled phProjectInfo =
  sendMessage phProjectInfo pageNumbersEnabledSelector

-- | The product identifier of the originating Apple Print Product.
--
-- ObjC selector: @- productIdentifier@
productIdentifier :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO (Id NSString)
productIdentifier phProjectInfo =
  sendMessage phProjectInfo productIdentifierSelector

-- | The product theme identifier of the originating Apple Print Product.
--
-- ObjC selector: @- themeIdentifier@
themeIdentifier :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO (Id NSString)
themeIdentifier phProjectInfo =
  sendMessage phProjectInfo themeIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHProjectInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHProjectInfo)
newSelector = mkSelector "new"

-- | @Selector@ for @creationSource@
creationSourceSelector :: Selector '[] PHProjectCreationSource
creationSourceSelector = mkSelector "creationSource"

-- | @Selector@ for @projectType@
projectTypeSelector :: Selector '[] (Id NSString)
projectTypeSelector = mkSelector "projectType"

-- | @Selector@ for @sections@
sectionsSelector :: Selector '[] (Id NSArray)
sectionsSelector = mkSelector "sections"

-- | @Selector@ for @brandingEnabled@
brandingEnabledSelector :: Selector '[] Bool
brandingEnabledSelector = mkSelector "brandingEnabled"

-- | @Selector@ for @pageNumbersEnabled@
pageNumbersEnabledSelector :: Selector '[] Bool
pageNumbersEnabledSelector = mkSelector "pageNumbersEnabled"

-- | @Selector@ for @productIdentifier@
productIdentifierSelector :: Selector '[] (Id NSString)
productIdentifierSelector = mkSelector "productIdentifier"

-- | @Selector@ for @themeIdentifier@
themeIdentifierSelector :: Selector '[] (Id NSString)
themeIdentifierSelector = mkSelector "themeIdentifier"

