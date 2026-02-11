{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , creationSourceSelector
  , projectTypeSelector
  , sectionsSelector
  , brandingEnabledSelector
  , pageNumbersEnabledSelector
  , productIdentifierSelector
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
import ObjC.PhotosUI.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO (Id PHProjectInfo)
init_ phProjectInfo  =
    sendMsg phProjectInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHProjectInfo)
new  =
  do
    cls' <- getRequiredClass "PHProjectInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Source from which the project was created.
--
-- ObjC selector: @- creationSource@
creationSource :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO PHProjectCreationSource
creationSource phProjectInfo  =
    fmap (coerce :: CLong -> PHProjectCreationSource) $ sendMsg phProjectInfo (mkSelector "creationSource") retCLong []

-- | Selected projectType value from the extensions options as defined in -[PHProjectExtensionController supportedProjectTypes]. See PHProjectExtensionController.h for more information on configuring the options.
--
-- ObjC selector: @- projectType@
projectType :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO (Id NSString)
projectType phProjectInfo  =
    sendMsg phProjectInfo (mkSelector "projectType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of project sections each containing one or more PHProjectSectionContent objects.
--
-- ObjC selector: @- sections@
sections :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO (Id NSArray)
sections phProjectInfo  =
    sendMsg phProjectInfo (mkSelector "sections") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The following properties are only used when the user creates a new project from an existing Apple Print Product.
--
-- YES if the source project had branding enabled.
--
-- ObjC selector: @- brandingEnabled@
brandingEnabled :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO Bool
brandingEnabled phProjectInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg phProjectInfo (mkSelector "brandingEnabled") retCULong []

-- | YES if the source project had page numbers enabled.
--
-- ObjC selector: @- pageNumbersEnabled@
pageNumbersEnabled :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO Bool
pageNumbersEnabled phProjectInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg phProjectInfo (mkSelector "pageNumbersEnabled") retCULong []

-- | The product identifier of the originating Apple Print Product.
--
-- ObjC selector: @- productIdentifier@
productIdentifier :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO (Id NSString)
productIdentifier phProjectInfo  =
    sendMsg phProjectInfo (mkSelector "productIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The product theme identifier of the originating Apple Print Product.
--
-- ObjC selector: @- themeIdentifier@
themeIdentifier :: IsPHProjectInfo phProjectInfo => phProjectInfo -> IO (Id NSString)
themeIdentifier phProjectInfo  =
    sendMsg phProjectInfo (mkSelector "themeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @creationSource@
creationSourceSelector :: Selector
creationSourceSelector = mkSelector "creationSource"

-- | @Selector@ for @projectType@
projectTypeSelector :: Selector
projectTypeSelector = mkSelector "projectType"

-- | @Selector@ for @sections@
sectionsSelector :: Selector
sectionsSelector = mkSelector "sections"

-- | @Selector@ for @brandingEnabled@
brandingEnabledSelector :: Selector
brandingEnabledSelector = mkSelector "brandingEnabled"

-- | @Selector@ for @pageNumbersEnabled@
pageNumbersEnabledSelector :: Selector
pageNumbersEnabledSelector = mkSelector "pageNumbersEnabled"

-- | @Selector@ for @productIdentifier@
productIdentifierSelector :: Selector
productIdentifierSelector = mkSelector "productIdentifier"

-- | @Selector@ for @themeIdentifier@
themeIdentifierSelector :: Selector
themeIdentifierSelector = mkSelector "themeIdentifier"

