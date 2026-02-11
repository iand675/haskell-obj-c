{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A PHProjectTypeDescription object represents one project type choice in the project picker that is presented to a user when creating a project via a project extension. These objects are returned from the PHProjectTypeDescriptionDataSource object returned from -[PHProjectExtensionController typeDescriptionDataSourceForCategory:invalidator:]. The info includes a type, localized title, localized (attributed) description, image and optional subtype descriptions.
--
-- Generated bindings for @PHProjectTypeDescription@.
module ObjC.PhotosUI.PHProjectTypeDescription
  ( PHProjectTypeDescription
  , IsPHProjectTypeDescription(..)
  , initWithProjectType_title_description_image_subtypeDescriptions
  , initWithProjectType_title_attributedDescription_image_subtypeDescriptions
  , initWithProjectType_title_description_image
  , initWithProjectType_title_description_image_canProvideSubtypes
  , initWithProjectType_title_attributedDescription_image_canProvideSubtypes
  , init_
  , new
  , projectType
  , localizedTitle
  , localizedDescription
  , localizedAttributedDescription
  , image
  , subtypeDescriptions
  , canProvideSubtypes
  , initWithProjectType_title_description_image_subtypeDescriptionsSelector
  , initWithProjectType_title_attributedDescription_image_subtypeDescriptionsSelector
  , initWithProjectType_title_description_imageSelector
  , initWithProjectType_title_description_image_canProvideSubtypesSelector
  , initWithProjectType_title_attributedDescription_image_canProvideSubtypesSelector
  , initSelector
  , newSelector
  , projectTypeSelector
  , localizedTitleSelector
  , localizedDescriptionSelector
  , localizedAttributedDescriptionSelector
  , imageSelector
  , subtypeDescriptionsSelector
  , canProvideSubtypesSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Designated initalizer for instances with the full subtype hierarchy upfront.
--
-- ObjC selector: @- initWithProjectType:title:description:image:subtypeDescriptions:@
initWithProjectType_title_description_image_subtypeDescriptions :: (IsPHProjectTypeDescription phProjectTypeDescription, IsNSString projectType, IsNSString localizedTitle, IsNSString localizedDescription, IsNSImage image, IsNSArray subtypeDescriptions) => phProjectTypeDescription -> projectType -> localizedTitle -> localizedDescription -> image -> subtypeDescriptions -> IO (Id PHProjectTypeDescription)
initWithProjectType_title_description_image_subtypeDescriptions phProjectTypeDescription  projectType localizedTitle localizedDescription image subtypeDescriptions =
  withObjCPtr projectType $ \raw_projectType ->
    withObjCPtr localizedTitle $ \raw_localizedTitle ->
      withObjCPtr localizedDescription $ \raw_localizedDescription ->
        withObjCPtr image $ \raw_image ->
          withObjCPtr subtypeDescriptions $ \raw_subtypeDescriptions ->
              sendMsg phProjectTypeDescription (mkSelector "initWithProjectType:title:description:image:subtypeDescriptions:") (retPtr retVoid) [argPtr (castPtr raw_projectType :: Ptr ()), argPtr (castPtr raw_localizedTitle :: Ptr ()), argPtr (castPtr raw_localizedDescription :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_subtypeDescriptions :: Ptr ())] >>= ownedObject . castPtr

-- | Designated initalizer for instances with the full subtype hierarchy upfront and an attributed string for the description text.
--
-- ObjC selector: @- initWithProjectType:title:attributedDescription:image:subtypeDescriptions:@
initWithProjectType_title_attributedDescription_image_subtypeDescriptions :: (IsPHProjectTypeDescription phProjectTypeDescription, IsNSString projectType, IsNSString localizedTitle, IsNSAttributedString localizedAttributedDescription, IsNSImage image, IsNSArray subtypeDescriptions) => phProjectTypeDescription -> projectType -> localizedTitle -> localizedAttributedDescription -> image -> subtypeDescriptions -> IO (Id PHProjectTypeDescription)
initWithProjectType_title_attributedDescription_image_subtypeDescriptions phProjectTypeDescription  projectType localizedTitle localizedAttributedDescription image subtypeDescriptions =
  withObjCPtr projectType $ \raw_projectType ->
    withObjCPtr localizedTitle $ \raw_localizedTitle ->
      withObjCPtr localizedAttributedDescription $ \raw_localizedAttributedDescription ->
        withObjCPtr image $ \raw_image ->
          withObjCPtr subtypeDescriptions $ \raw_subtypeDescriptions ->
              sendMsg phProjectTypeDescription (mkSelector "initWithProjectType:title:attributedDescription:image:subtypeDescriptions:") (retPtr retVoid) [argPtr (castPtr raw_projectType :: Ptr ()), argPtr (castPtr raw_localizedTitle :: Ptr ()), argPtr (castPtr raw_localizedAttributedDescription :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_subtypeDescriptions :: Ptr ())] >>= ownedObject . castPtr

-- | Convenience initializer without subtype descriptions.
--
-- ObjC selector: @- initWithProjectType:title:description:image:@
initWithProjectType_title_description_image :: (IsPHProjectTypeDescription phProjectTypeDescription, IsNSString projectType, IsNSString localizedTitle, IsNSString localizedDescription, IsNSImage image) => phProjectTypeDescription -> projectType -> localizedTitle -> localizedDescription -> image -> IO (Id PHProjectTypeDescription)
initWithProjectType_title_description_image phProjectTypeDescription  projectType localizedTitle localizedDescription image =
  withObjCPtr projectType $ \raw_projectType ->
    withObjCPtr localizedTitle $ \raw_localizedTitle ->
      withObjCPtr localizedDescription $ \raw_localizedDescription ->
        withObjCPtr image $ \raw_image ->
            sendMsg phProjectTypeDescription (mkSelector "initWithProjectType:title:description:image:") (retPtr retVoid) [argPtr (castPtr raw_projectType :: Ptr ()), argPtr (castPtr raw_localizedTitle :: Ptr ()), argPtr (castPtr raw_localizedDescription :: Ptr ()), argPtr (castPtr raw_image :: Ptr ())] >>= ownedObject . castPtr

-- | Designated initalizer for instances with lazily fetched subtypes.
--
-- ObjC selector: @- initWithProjectType:title:description:image:canProvideSubtypes:@
initWithProjectType_title_description_image_canProvideSubtypes :: (IsPHProjectTypeDescription phProjectTypeDescription, IsNSString projectType, IsNSString localizedTitle, IsNSString localizedDescription, IsNSImage image) => phProjectTypeDescription -> projectType -> localizedTitle -> localizedDescription -> image -> Bool -> IO (Id PHProjectTypeDescription)
initWithProjectType_title_description_image_canProvideSubtypes phProjectTypeDescription  projectType localizedTitle localizedDescription image canProvideSubtypes =
  withObjCPtr projectType $ \raw_projectType ->
    withObjCPtr localizedTitle $ \raw_localizedTitle ->
      withObjCPtr localizedDescription $ \raw_localizedDescription ->
        withObjCPtr image $ \raw_image ->
            sendMsg phProjectTypeDescription (mkSelector "initWithProjectType:title:description:image:canProvideSubtypes:") (retPtr retVoid) [argPtr (castPtr raw_projectType :: Ptr ()), argPtr (castPtr raw_localizedTitle :: Ptr ()), argPtr (castPtr raw_localizedDescription :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argCULong (if canProvideSubtypes then 1 else 0)] >>= ownedObject . castPtr

-- | Designated initalizer for instances with lazily fetched subtypes and an attributed string for the description text.
--
-- ObjC selector: @- initWithProjectType:title:attributedDescription:image:canProvideSubtypes:@
initWithProjectType_title_attributedDescription_image_canProvideSubtypes :: (IsPHProjectTypeDescription phProjectTypeDescription, IsNSString projectType, IsNSString localizedTitle, IsNSAttributedString localizedAttributedDescription, IsNSImage image) => phProjectTypeDescription -> projectType -> localizedTitle -> localizedAttributedDescription -> image -> Bool -> IO (Id PHProjectTypeDescription)
initWithProjectType_title_attributedDescription_image_canProvideSubtypes phProjectTypeDescription  projectType localizedTitle localizedAttributedDescription image canProvideSubtypes =
  withObjCPtr projectType $ \raw_projectType ->
    withObjCPtr localizedTitle $ \raw_localizedTitle ->
      withObjCPtr localizedAttributedDescription $ \raw_localizedAttributedDescription ->
        withObjCPtr image $ \raw_image ->
            sendMsg phProjectTypeDescription (mkSelector "initWithProjectType:title:attributedDescription:image:canProvideSubtypes:") (retPtr retVoid) [argPtr (castPtr raw_projectType :: Ptr ()), argPtr (castPtr raw_localizedTitle :: Ptr ()), argPtr (castPtr raw_localizedAttributedDescription :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argCULong (if canProvideSubtypes then 1 else 0)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsPHProjectTypeDescription phProjectTypeDescription => phProjectTypeDescription -> IO (Id PHProjectTypeDescription)
init_ phProjectTypeDescription  =
    sendMsg phProjectTypeDescription (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHProjectTypeDescription)
new  =
  do
    cls' <- getRequiredClass "PHProjectTypeDescription"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Identifier for the project type info. These should be added to the extensible string enum defined in PhotosUITypes.h.
--
-- ObjC selector: @- projectType@
projectType :: IsPHProjectTypeDescription phProjectTypeDescription => phProjectTypeDescription -> IO (Id NSString)
projectType phProjectTypeDescription  =
    sendMsg phProjectTypeDescription (mkSelector "projectType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Localized title and description of the project type to be displayed to the user. The title is required, but description is optional.
--
-- ObjC selector: @- localizedTitle@
localizedTitle :: IsPHProjectTypeDescription phProjectTypeDescription => phProjectTypeDescription -> IO (Id NSString)
localizedTitle phProjectTypeDescription  =
    sendMsg phProjectTypeDescription (mkSelector "localizedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedDescription@
localizedDescription :: IsPHProjectTypeDescription phProjectTypeDescription => phProjectTypeDescription -> IO (Id NSString)
localizedDescription phProjectTypeDescription  =
    sendMsg phProjectTypeDescription (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedAttributedDescription@
localizedAttributedDescription :: IsPHProjectTypeDescription phProjectTypeDescription => phProjectTypeDescription -> IO (Id NSAttributedString)
localizedAttributedDescription phProjectTypeDescription  =
    sendMsg phProjectTypeDescription (mkSelector "localizedAttributedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional image to be associated with the project type in the picker; PNG images are recommended.
--
-- ObjC selector: @- image@
image :: IsPHProjectTypeDescription phProjectTypeDescription => phProjectTypeDescription -> IO (Id NSImage)
image phProjectTypeDescription  =
    sendMsg phProjectTypeDescription (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of type descriptions for subtype descriptions, may be empty.
--
-- ObjC selector: @- subtypeDescriptions@
subtypeDescriptions :: IsPHProjectTypeDescription phProjectTypeDescription => phProjectTypeDescription -> IO (Id NSArray)
subtypeDescriptions phProjectTypeDescription  =
    sendMsg phProjectTypeDescription (mkSelector "subtypeDescriptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | For spase instances canProvideSubtypes is an indicator if subtypes can be fetched from the data source. If subtypeDescriptions is not empty it will also return YES.
--
-- ObjC selector: @- canProvideSubtypes@
canProvideSubtypes :: IsPHProjectTypeDescription phProjectTypeDescription => phProjectTypeDescription -> IO Bool
canProvideSubtypes phProjectTypeDescription  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg phProjectTypeDescription (mkSelector "canProvideSubtypes") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProjectType:title:description:image:subtypeDescriptions:@
initWithProjectType_title_description_image_subtypeDescriptionsSelector :: Selector
initWithProjectType_title_description_image_subtypeDescriptionsSelector = mkSelector "initWithProjectType:title:description:image:subtypeDescriptions:"

-- | @Selector@ for @initWithProjectType:title:attributedDescription:image:subtypeDescriptions:@
initWithProjectType_title_attributedDescription_image_subtypeDescriptionsSelector :: Selector
initWithProjectType_title_attributedDescription_image_subtypeDescriptionsSelector = mkSelector "initWithProjectType:title:attributedDescription:image:subtypeDescriptions:"

-- | @Selector@ for @initWithProjectType:title:description:image:@
initWithProjectType_title_description_imageSelector :: Selector
initWithProjectType_title_description_imageSelector = mkSelector "initWithProjectType:title:description:image:"

-- | @Selector@ for @initWithProjectType:title:description:image:canProvideSubtypes:@
initWithProjectType_title_description_image_canProvideSubtypesSelector :: Selector
initWithProjectType_title_description_image_canProvideSubtypesSelector = mkSelector "initWithProjectType:title:description:image:canProvideSubtypes:"

-- | @Selector@ for @initWithProjectType:title:attributedDescription:image:canProvideSubtypes:@
initWithProjectType_title_attributedDescription_image_canProvideSubtypesSelector :: Selector
initWithProjectType_title_attributedDescription_image_canProvideSubtypesSelector = mkSelector "initWithProjectType:title:attributedDescription:image:canProvideSubtypes:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @projectType@
projectTypeSelector :: Selector
projectTypeSelector = mkSelector "projectType"

-- | @Selector@ for @localizedTitle@
localizedTitleSelector :: Selector
localizedTitleSelector = mkSelector "localizedTitle"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @localizedAttributedDescription@
localizedAttributedDescriptionSelector :: Selector
localizedAttributedDescriptionSelector = mkSelector "localizedAttributedDescription"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @subtypeDescriptions@
subtypeDescriptionsSelector :: Selector
subtypeDescriptionsSelector = mkSelector "subtypeDescriptions"

-- | @Selector@ for @canProvideSubtypes@
canProvideSubtypesSelector :: Selector
canProvideSubtypesSelector = mkSelector "canProvideSubtypes"

