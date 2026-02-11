{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.PhotosUI.Internal.Classes (
    module ObjC.PhotosUI.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.Photos.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Photos.Internal.Classes

-- ---------- PHPickerConfiguration ----------

-- | A configuration for @PHPickerViewController.@
-- 
-- Phantom type for @PHPickerConfiguration@.
data PHPickerConfiguration

instance IsObjCObject (Id PHPickerConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHPickerConfiguration"

class IsNSObject a => IsPHPickerConfiguration a where
  toPHPickerConfiguration :: a -> Id PHPickerConfiguration

instance IsPHPickerConfiguration (Id PHPickerConfiguration) where
  toPHPickerConfiguration = unsafeCastId

instance IsNSObject (Id PHPickerConfiguration) where
  toNSObject = unsafeCastId

-- ---------- PHPickerFilter ----------

-- | A filter that restricts which types of assets @PHPickerViewController@ can show.
-- 
-- Phantom type for @PHPickerFilter@.
data PHPickerFilter

instance IsObjCObject (Id PHPickerFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHPickerFilter"

class IsNSObject a => IsPHPickerFilter a where
  toPHPickerFilter :: a -> Id PHPickerFilter

instance IsPHPickerFilter (Id PHPickerFilter) where
  toPHPickerFilter = unsafeCastId

instance IsNSObject (Id PHPickerFilter) where
  toNSObject = unsafeCastId

-- ---------- PHPickerResult ----------

-- | A user selected asset from @PHPickerViewController.@
-- 
-- Phantom type for @PHPickerResult@.
data PHPickerResult

instance IsObjCObject (Id PHPickerResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHPickerResult"

class IsNSObject a => IsPHPickerResult a where
  toPHPickerResult :: a -> Id PHPickerResult

instance IsPHPickerResult (Id PHPickerResult) where
  toPHPickerResult = unsafeCastId

instance IsNSObject (Id PHPickerResult) where
  toNSObject = unsafeCastId

-- ---------- PHPickerUpdateConfiguration ----------

-- | An update configuration for @PHPickerViewController.@
-- 
-- Phantom type for @PHPickerUpdateConfiguration@.
data PHPickerUpdateConfiguration

instance IsObjCObject (Id PHPickerUpdateConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHPickerUpdateConfiguration"

class IsNSObject a => IsPHPickerUpdateConfiguration a where
  toPHPickerUpdateConfiguration :: a -> Id PHPickerUpdateConfiguration

instance IsPHPickerUpdateConfiguration (Id PHPickerUpdateConfiguration) where
  toPHPickerUpdateConfiguration = unsafeCastId

instance IsNSObject (Id PHPickerUpdateConfiguration) where
  toNSObject = unsafeCastId

-- ---------- PHProjectElement ----------

-- | PHProjectElement is the superclass for all element objects. It is never directly used, but defines the shared properties of any element in an instance of PHProjectSectionContent.
-- 
-- Phantom type for @PHProjectElement@.
data PHProjectElement

instance IsObjCObject (Id PHProjectElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProjectElement"

class IsNSObject a => IsPHProjectElement a where
  toPHProjectElement :: a -> Id PHProjectElement

instance IsPHProjectElement (Id PHProjectElement) where
  toPHProjectElement = unsafeCastId

instance IsNSObject (Id PHProjectElement) where
  toNSObject = unsafeCastId

-- ---------- PHProjectInfo ----------

-- | A PHProjectInfo object is created by Photos and passed along with a PHProjectExtensionContext any time Photos creates a new project. It comprises the complete content description which a Photos Project Extension can leverage to influence things like project layout, auto-flow, or theme selection. The properties in this class are inmutable and the class cannot be instatiated by an extension directly.
-- 
-- Phantom type for @PHProjectInfo@.
data PHProjectInfo

instance IsObjCObject (Id PHProjectInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProjectInfo"

class IsNSObject a => IsPHProjectInfo a where
  toPHProjectInfo :: a -> Id PHProjectInfo

instance IsPHProjectInfo (Id PHProjectInfo) where
  toPHProjectInfo = unsafeCastId

instance IsNSObject (Id PHProjectInfo) where
  toNSObject = unsafeCastId

-- ---------- PHProjectRegionOfInterest ----------

-- | In PHProjectAssetElement objects, an array of PHProjectRegionOfInterest objects may be provided. These regions represent specific areas in an asset that have signficant meaning. For example, faces that are relevant to the user (as opposed to faces in a crowd) will be highlighted in the asset to help with things like auto-pan, auto-zoom, or focusing on specific areas in the asset during animations or  transitions. Regions representing the same person or object across multiple assets are cross-referenced through the use of the identifier.
-- 
-- Phantom type for @PHProjectRegionOfInterest@.
data PHProjectRegionOfInterest

instance IsObjCObject (Id PHProjectRegionOfInterest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProjectRegionOfInterest"

class IsNSObject a => IsPHProjectRegionOfInterest a where
  toPHProjectRegionOfInterest :: a -> Id PHProjectRegionOfInterest

instance IsPHProjectRegionOfInterest (Id PHProjectRegionOfInterest) where
  toPHProjectRegionOfInterest = unsafeCastId

instance IsNSObject (Id PHProjectRegionOfInterest) where
  toNSObject = unsafeCastId

-- ---------- PHProjectSection ----------

-- | A PHProjectSection object represents a collection of content for the project including asset elements and text elements. Each section contains one or more PHProjectSectionContent objects which provide suggested levels of "curation" for the content contained in the section. The number of sections included in PHProjectInfo will vary depending on the creation source at the time of the project initiation. For example: - if user creates a project from a Memory, there will be one cover section with a key asset element and titling, plus one section containing multiple levels of curation that mirror the "Show Summary" and "Show More" options of the Memory - if user creates a project from a single Album, the project info may only contain one section unless the album contains a large quantity of photos in which case, Photos may suggest section breaks based on Moments in the user's library - if user creates a project from an existing Apple Book, Card, or Calendar, the sections provided in the project info will exactly match the pagination in that project (e.g., one section per page in a book).
-- 
-- Phantom type for @PHProjectSection@.
data PHProjectSection

instance IsObjCObject (Id PHProjectSection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProjectSection"

class IsNSObject a => IsPHProjectSection a where
  toPHProjectSection :: a -> Id PHProjectSection

instance IsPHProjectSection (Id PHProjectSection) where
  toPHProjectSection = unsafeCastId

instance IsNSObject (Id PHProjectSection) where
  toNSObject = unsafeCastId

-- ---------- PHProjectSectionContent ----------

-- | A PHProjectSectionContent object contains all the elements and suggested layout information for a specific level of curation within a PHProjectSection. A section can provide multiple content objects, but only one is intended to be used in a project based on the amount of content detail desired.
-- 
-- Phantom type for @PHProjectSectionContent@.
data PHProjectSectionContent

instance IsObjCObject (Id PHProjectSectionContent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProjectSectionContent"

class IsNSObject a => IsPHProjectSectionContent a where
  toPHProjectSectionContent :: a -> Id PHProjectSectionContent

instance IsPHProjectSectionContent (Id PHProjectSectionContent) where
  toPHProjectSectionContent = unsafeCastId

instance IsNSObject (Id PHProjectSectionContent) where
  toNSObject = unsafeCastId

-- ---------- PHProjectTypeDescription ----------

-- | A PHProjectTypeDescription object represents one project type choice in the project picker that is presented to a user when creating a project via a project extension. These objects are returned from the PHProjectTypeDescriptionDataSource object returned from -[PHProjectExtensionController typeDescriptionDataSourceForCategory:invalidator:]. The info includes a type, localized title, localized (attributed) description, image and optional subtype descriptions.
-- 
-- Phantom type for @PHProjectTypeDescription@.
data PHProjectTypeDescription

instance IsObjCObject (Id PHProjectTypeDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProjectTypeDescription"

class IsNSObject a => IsPHProjectTypeDescription a where
  toPHProjectTypeDescription :: a -> Id PHProjectTypeDescription

instance IsPHProjectTypeDescription (Id PHProjectTypeDescription) where
  toPHProjectTypeDescription = unsafeCastId

instance IsNSObject (Id PHProjectTypeDescription) where
  toNSObject = unsafeCastId

-- ---------- PHProjectExtensionContext ----------

-- | When a Photos project extension is initialized, it is handed a PHProjectExtensionContext object. This object provides the extension access to the underlying project as well as the photo library from which assets can be fetched.
-- 
-- Phantom type for @PHProjectExtensionContext@.
data PHProjectExtensionContext

instance IsObjCObject (Id PHProjectExtensionContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProjectExtensionContext"

class IsNSExtensionContext a => IsPHProjectExtensionContext a where
  toPHProjectExtensionContext :: a -> Id PHProjectExtensionContext

instance IsPHProjectExtensionContext (Id PHProjectExtensionContext) where
  toPHProjectExtensionContext = unsafeCastId

instance IsNSExtensionContext (Id PHProjectExtensionContext) where
  toNSExtensionContext = unsafeCastId

instance IsNSObject (Id PHProjectExtensionContext) where
  toNSObject = unsafeCastId

-- ---------- PHProjectAssetElement ----------

-- | A PHProjectAssetElement object represents a media asset within a PHProjectSectionContent. The underlying PHAsset can be accessed by converting the provided cloudAssetIdentifier to a localIdentifier, then using the fetchAssetsWithLocalIdentifiers:options: class method defined in PHAsset.h.
-- 
-- Phantom type for @PHProjectAssetElement@.
data PHProjectAssetElement

instance IsObjCObject (Id PHProjectAssetElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProjectAssetElement"

class IsPHProjectElement a => IsPHProjectAssetElement a where
  toPHProjectAssetElement :: a -> Id PHProjectAssetElement

instance IsPHProjectAssetElement (Id PHProjectAssetElement) where
  toPHProjectAssetElement = unsafeCastId

instance IsNSObject (Id PHProjectAssetElement) where
  toNSObject = unsafeCastId

instance IsPHProjectElement (Id PHProjectAssetElement) where
  toPHProjectElement = unsafeCastId

-- ---------- PHProjectJournalEntryElement ----------

-- | A PHProjectJournalEntryElement object represents auxilary, date specific information that may be interesting to include in a project. For example, callouts for specific birthdays or holidays. In general, these will only be included for projects created from existing Apple Calendar projects.
-- 
-- Phantom type for @PHProjectJournalEntryElement@.
data PHProjectJournalEntryElement

instance IsObjCObject (Id PHProjectJournalEntryElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProjectJournalEntryElement"

class IsPHProjectElement a => IsPHProjectJournalEntryElement a where
  toPHProjectJournalEntryElement :: a -> Id PHProjectJournalEntryElement

instance IsPHProjectJournalEntryElement (Id PHProjectJournalEntryElement) where
  toPHProjectJournalEntryElement = unsafeCastId

instance IsNSObject (Id PHProjectJournalEntryElement) where
  toNSObject = unsafeCastId

instance IsPHProjectElement (Id PHProjectJournalEntryElement) where
  toPHProjectElement = unsafeCastId

-- ---------- PHProjectMapElement ----------

-- | A PHProjectMapElement object representing a map with annotations. In general, these will only be included for projects created from existing Apple Print Product projects.
-- 
-- Phantom type for @PHProjectMapElement@.
data PHProjectMapElement

instance IsObjCObject (Id PHProjectMapElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProjectMapElement"

class IsPHProjectElement a => IsPHProjectMapElement a where
  toPHProjectMapElement :: a -> Id PHProjectMapElement

instance IsPHProjectMapElement (Id PHProjectMapElement) where
  toPHProjectMapElement = unsafeCastId

instance IsNSObject (Id PHProjectMapElement) where
  toNSObject = unsafeCastId

instance IsPHProjectElement (Id PHProjectMapElement) where
  toPHProjectElement = unsafeCastId

-- ---------- PHProjectTextElement ----------

-- | A PHProjectTextElement object represents formatted, positioned text that should be considered for inclusion in a project. In this case of a Memory, this will always be the Title and Subtitle show in the Memory header view. For projects created from Apple Book, Card, and Calendar projects, text appearing on any page.
-- 
-- Phantom type for @PHProjectTextElement@.
data PHProjectTextElement

instance IsObjCObject (Id PHProjectTextElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProjectTextElement"

class IsPHProjectElement a => IsPHProjectTextElement a where
  toPHProjectTextElement :: a -> Id PHProjectTextElement

instance IsPHProjectTextElement (Id PHProjectTextElement) where
  toPHProjectTextElement = unsafeCastId

instance IsNSObject (Id PHProjectTextElement) where
  toNSObject = unsafeCastId

instance IsPHProjectElement (Id PHProjectTextElement) where
  toPHProjectElement = unsafeCastId

-- ---------- PHLivePhotoView ----------

-- | Phantom type for @PHLivePhotoView@.
data PHLivePhotoView

instance IsObjCObject (Id PHLivePhotoView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHLivePhotoView"

class IsNSView a => IsPHLivePhotoView a where
  toPHLivePhotoView :: a -> Id PHLivePhotoView

instance IsPHLivePhotoView (Id PHLivePhotoView) where
  toPHLivePhotoView = unsafeCastId

instance IsNSObject (Id PHLivePhotoView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id PHLivePhotoView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id PHLivePhotoView) where
  toNSView = unsafeCastId

-- ---------- PHPickerViewController ----------

-- | Phantom type for @PHPickerViewController@.
data PHPickerViewController

instance IsObjCObject (Id PHPickerViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHPickerViewController"

class IsNSViewController a => IsPHPickerViewController a where
  toPHPickerViewController :: a -> Id PHPickerViewController

instance IsPHPickerViewController (Id PHPickerViewController) where
  toPHPickerViewController = unsafeCastId

instance IsNSObject (Id PHPickerViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id PHPickerViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id PHPickerViewController) where
  toNSViewController = unsafeCastId
