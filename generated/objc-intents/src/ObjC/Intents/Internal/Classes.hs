{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Intents.Internal.Classes (
    module ObjC.Intents.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.CoreLocation.Internal.Classes,
    module ObjC.EventKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- INAirline ----------

-- | Phantom type for @INAirline@.
data INAirline

instance IsObjCObject (Id INAirline) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAirline"

class IsNSObject a => IsINAirline a where
  toINAirline :: a -> Id INAirline

instance IsINAirline (Id INAirline) where
  toINAirline = unsafeCastId

instance IsNSObject (Id INAirline) where
  toNSObject = unsafeCastId

-- ---------- INAirport ----------

-- | Phantom type for @INAirport@.
data INAirport

instance IsObjCObject (Id INAirport) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAirport"

class IsNSObject a => IsINAirport a where
  toINAirport :: a -> Id INAirport

instance IsINAirport (Id INAirport) where
  toINAirport = unsafeCastId

instance IsNSObject (Id INAirport) where
  toNSObject = unsafeCastId

-- ---------- INAirportGate ----------

-- | Phantom type for @INAirportGate@.
data INAirportGate

instance IsObjCObject (Id INAirportGate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAirportGate"

class IsNSObject a => IsINAirportGate a where
  toINAirportGate :: a -> Id INAirportGate

instance IsINAirportGate (Id INAirportGate) where
  toINAirportGate = unsafeCastId

instance IsNSObject (Id INAirportGate) where
  toNSObject = unsafeCastId

-- ---------- INBalanceAmount ----------

-- | Phantom type for @INBalanceAmount@.
data INBalanceAmount

instance IsObjCObject (Id INBalanceAmount) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBalanceAmount"

class IsNSObject a => IsINBalanceAmount a where
  toINBalanceAmount :: a -> Id INBalanceAmount

instance IsINBalanceAmount (Id INBalanceAmount) where
  toINBalanceAmount = unsafeCastId

instance IsNSObject (Id INBalanceAmount) where
  toNSObject = unsafeCastId

-- ---------- INBillDetails ----------

-- | Phantom type for @INBillDetails@.
data INBillDetails

instance IsObjCObject (Id INBillDetails) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBillDetails"

class IsNSObject a => IsINBillDetails a where
  toINBillDetails :: a -> Id INBillDetails

instance IsINBillDetails (Id INBillDetails) where
  toINBillDetails = unsafeCastId

instance IsNSObject (Id INBillDetails) where
  toNSObject = unsafeCastId

-- ---------- INBillPayee ----------

-- | Phantom type for @INBillPayee@.
data INBillPayee

instance IsObjCObject (Id INBillPayee) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBillPayee"

class IsNSObject a => IsINBillPayee a where
  toINBillPayee :: a -> Id INBillPayee

instance IsINBillPayee (Id INBillPayee) where
  toINBillPayee = unsafeCastId

instance IsNSObject (Id INBillPayee) where
  toNSObject = unsafeCastId

-- ---------- INBoatTrip ----------

-- | Phantom type for @INBoatTrip@.
data INBoatTrip

instance IsObjCObject (Id INBoatTrip) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBoatTrip"

class IsNSObject a => IsINBoatTrip a where
  toINBoatTrip :: a -> Id INBoatTrip

instance IsINBoatTrip (Id INBoatTrip) where
  toINBoatTrip = unsafeCastId

instance IsNSObject (Id INBoatTrip) where
  toNSObject = unsafeCastId

-- ---------- INBusTrip ----------

-- | Phantom type for @INBusTrip@.
data INBusTrip

instance IsObjCObject (Id INBusTrip) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBusTrip"

class IsNSObject a => IsINBusTrip a where
  toINBusTrip :: a -> Id INBusTrip

instance IsINBusTrip (Id INBusTrip) where
  toINBusTrip = unsafeCastId

instance IsNSObject (Id INBusTrip) where
  toNSObject = unsafeCastId

-- ---------- INCallGroup ----------

-- | Phantom type for @INCallGroup@.
data INCallGroup

instance IsObjCObject (Id INCallGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCallGroup"

class IsNSObject a => IsINCallGroup a where
  toINCallGroup :: a -> Id INCallGroup

instance IsINCallGroup (Id INCallGroup) where
  toINCallGroup = unsafeCastId

instance IsNSObject (Id INCallGroup) where
  toNSObject = unsafeCastId

-- ---------- INCallRecord ----------

-- | Phantom type for @INCallRecord@.
data INCallRecord

instance IsObjCObject (Id INCallRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCallRecord"

class IsNSObject a => IsINCallRecord a where
  toINCallRecord :: a -> Id INCallRecord

instance IsINCallRecord (Id INCallRecord) where
  toINCallRecord = unsafeCastId

instance IsNSObject (Id INCallRecord) where
  toNSObject = unsafeCastId

-- ---------- INCallRecordFilter ----------

-- | Phantom type for @INCallRecordFilter@.
data INCallRecordFilter

instance IsObjCObject (Id INCallRecordFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCallRecordFilter"

class IsNSObject a => IsINCallRecordFilter a where
  toINCallRecordFilter :: a -> Id INCallRecordFilter

instance IsINCallRecordFilter (Id INCallRecordFilter) where
  toINCallRecordFilter = unsafeCastId

instance IsNSObject (Id INCallRecordFilter) where
  toNSObject = unsafeCastId

-- ---------- INCar ----------

-- | Phantom type for @INCar@.
data INCar

instance IsObjCObject (Id INCar) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCar"

class IsNSObject a => IsINCar a where
  toINCar :: a -> Id INCar

instance IsINCar (Id INCar) where
  toINCar = unsafeCastId

instance IsNSObject (Id INCar) where
  toNSObject = unsafeCastId

-- ---------- INCarHeadUnit ----------

-- | Phantom type for @INCarHeadUnit@.
data INCarHeadUnit

instance IsObjCObject (Id INCarHeadUnit) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCarHeadUnit"

class IsNSObject a => IsINCarHeadUnit a where
  toINCarHeadUnit :: a -> Id INCarHeadUnit

instance IsINCarHeadUnit (Id INCarHeadUnit) where
  toINCarHeadUnit = unsafeCastId

instance IsNSObject (Id INCarHeadUnit) where
  toNSObject = unsafeCastId

-- ---------- INCurrencyAmount ----------

-- | Phantom type for @INCurrencyAmount@.
data INCurrencyAmount

instance IsObjCObject (Id INCurrencyAmount) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCurrencyAmount"

class IsNSObject a => IsINCurrencyAmount a where
  toINCurrencyAmount :: a -> Id INCurrencyAmount

instance IsINCurrencyAmount (Id INCurrencyAmount) where
  toINCurrencyAmount = unsafeCastId

instance IsNSObject (Id INCurrencyAmount) where
  toNSObject = unsafeCastId

-- ---------- INDateComponentsRange ----------

-- | Phantom type for @INDateComponentsRange@.
data INDateComponentsRange

instance IsObjCObject (Id INDateComponentsRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INDateComponentsRange"

class IsNSObject a => IsINDateComponentsRange a where
  toINDateComponentsRange :: a -> Id INDateComponentsRange

instance IsINDateComponentsRange (Id INDateComponentsRange) where
  toINDateComponentsRange = unsafeCastId

instance IsNSObject (Id INDateComponentsRange) where
  toNSObject = unsafeCastId

-- ---------- INDefaultCardTemplate ----------

-- | A template for customizing the display of relevant shortcuts.
--
-- INRelevantShortcut
-- 
-- Phantom type for @INDefaultCardTemplate@.
data INDefaultCardTemplate

instance IsObjCObject (Id INDefaultCardTemplate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INDefaultCardTemplate"

class IsNSObject a => IsINDefaultCardTemplate a where
  toINDefaultCardTemplate :: a -> Id INDefaultCardTemplate

instance IsINDefaultCardTemplate (Id INDefaultCardTemplate) where
  toINDefaultCardTemplate = unsafeCastId

instance IsNSObject (Id INDefaultCardTemplate) where
  toNSObject = unsafeCastId

-- ---------- INExtension ----------

-- | Phantom type for @INExtension@.
data INExtension

instance IsObjCObject (Id INExtension) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INExtension"

class IsNSObject a => IsINExtension a where
  toINExtension :: a -> Id INExtension

instance IsINExtension (Id INExtension) where
  toINExtension = unsafeCastId

instance IsNSObject (Id INExtension) where
  toNSObject = unsafeCastId

-- ---------- INFile ----------

-- | An object that describes a piece of data and its associated name and uniform type identifier. This data can either be stored in a file on disk, or in memory.
-- 
-- Phantom type for @INFile@.
data INFile

instance IsObjCObject (Id INFile) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INFile"

class IsNSObject a => IsINFile a where
  toINFile :: a -> Id INFile

instance IsINFile (Id INFile) where
  toINFile = unsafeCastId

instance IsNSObject (Id INFile) where
  toNSObject = unsafeCastId

-- ---------- INFlight ----------

-- | Phantom type for @INFlight@.
data INFlight

instance IsObjCObject (Id INFlight) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INFlight"

class IsNSObject a => IsINFlight a where
  toINFlight :: a -> Id INFlight

instance IsINFlight (Id INFlight) where
  toINFlight = unsafeCastId

instance IsNSObject (Id INFlight) where
  toNSObject = unsafeCastId

-- ---------- INFocusStatus ----------

-- | Phantom type for @INFocusStatus@.
data INFocusStatus

instance IsObjCObject (Id INFocusStatus) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INFocusStatus"

class IsNSObject a => IsINFocusStatus a where
  toINFocusStatus :: a -> Id INFocusStatus

instance IsINFocusStatus (Id INFocusStatus) where
  toINFocusStatus = unsafeCastId

instance IsNSObject (Id INFocusStatus) where
  toNSObject = unsafeCastId

-- ---------- INFocusStatusCenter ----------

-- | Phantom type for @INFocusStatusCenter@.
data INFocusStatusCenter

instance IsObjCObject (Id INFocusStatusCenter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INFocusStatusCenter"

class IsNSObject a => IsINFocusStatusCenter a where
  toINFocusStatusCenter :: a -> Id INFocusStatusCenter

instance IsINFocusStatusCenter (Id INFocusStatusCenter) where
  toINFocusStatusCenter = unsafeCastId

instance IsNSObject (Id INFocusStatusCenter) where
  toNSObject = unsafeCastId

-- ---------- INImage ----------

-- | Phantom type for @INImage@.
data INImage

instance IsObjCObject (Id INImage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INImage"

class IsNSObject a => IsINImage a where
  toINImage :: a -> Id INImage

instance IsINImage (Id INImage) where
  toINImage = unsafeCastId

instance IsNSObject (Id INImage) where
  toNSObject = unsafeCastId

-- ---------- INIntent ----------

-- | Phantom type for @INIntent@.
data INIntent

instance IsObjCObject (Id INIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INIntent"

class IsNSObject a => IsINIntent a where
  toINIntent :: a -> Id INIntent

instance IsINIntent (Id INIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INIntent) where
  toNSObject = unsafeCastId

-- ---------- INIntentDonationMetadata ----------

-- | Phantom type for @INIntentDonationMetadata@.
data INIntentDonationMetadata

instance IsObjCObject (Id INIntentDonationMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INIntentDonationMetadata"

class IsNSObject a => IsINIntentDonationMetadata a where
  toINIntentDonationMetadata :: a -> Id INIntentDonationMetadata

instance IsINIntentDonationMetadata (Id INIntentDonationMetadata) where
  toINIntentDonationMetadata = unsafeCastId

instance IsNSObject (Id INIntentDonationMetadata) where
  toNSObject = unsafeCastId

-- ---------- INIntentResolutionResult ----------

-- | Phantom type for @INIntentResolutionResult@.
data INIntentResolutionResult

instance IsObjCObject (Id INIntentResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INIntentResolutionResult"

class IsNSObject a => IsINIntentResolutionResult a where
  toINIntentResolutionResult :: a -> Id INIntentResolutionResult

instance IsINIntentResolutionResult (Id INIntentResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INIntentResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INIntentResponse ----------

-- | Phantom type for @INIntentResponse@.
data INIntentResponse

instance IsObjCObject (Id INIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INIntentResponse"

class IsNSObject a => IsINIntentResponse a where
  toINIntentResponse :: a -> Id INIntentResponse

instance IsINIntentResponse (Id INIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INInteraction ----------

-- | Phantom type for @INInteraction@.
data INInteraction

instance IsObjCObject (Id INInteraction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INInteraction"

class IsNSObject a => IsINInteraction a where
  toINInteraction :: a -> Id INInteraction

instance IsINInteraction (Id INInteraction) where
  toINInteraction = unsafeCastId

instance IsNSObject (Id INInteraction) where
  toNSObject = unsafeCastId

-- ---------- INMediaDestination ----------

-- | Phantom type for @INMediaDestination@.
data INMediaDestination

instance IsObjCObject (Id INMediaDestination) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMediaDestination"

class IsNSObject a => IsINMediaDestination a where
  toINMediaDestination :: a -> Id INMediaDestination

instance IsINMediaDestination (Id INMediaDestination) where
  toINMediaDestination = unsafeCastId

instance IsNSObject (Id INMediaDestination) where
  toNSObject = unsafeCastId

-- ---------- INMediaItem ----------

-- | Phantom type for @INMediaItem@.
data INMediaItem

instance IsObjCObject (Id INMediaItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMediaItem"

class IsNSObject a => IsINMediaItem a where
  toINMediaItem :: a -> Id INMediaItem

instance IsINMediaItem (Id INMediaItem) where
  toINMediaItem = unsafeCastId

instance IsNSObject (Id INMediaItem) where
  toNSObject = unsafeCastId

-- ---------- INMediaSearch ----------

-- | Phantom type for @INMediaSearch@.
data INMediaSearch

instance IsObjCObject (Id INMediaSearch) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMediaSearch"

class IsNSObject a => IsINMediaSearch a where
  toINMediaSearch :: a -> Id INMediaSearch

instance IsINMediaSearch (Id INMediaSearch) where
  toINMediaSearch = unsafeCastId

instance IsNSObject (Id INMediaSearch) where
  toNSObject = unsafeCastId

-- ---------- INMessage ----------

-- | Phantom type for @INMessage@.
data INMessage

instance IsObjCObject (Id INMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMessage"

class IsNSObject a => IsINMessage a where
  toINMessage :: a -> Id INMessage

instance IsINMessage (Id INMessage) where
  toINMessage = unsafeCastId

instance IsNSObject (Id INMessage) where
  toNSObject = unsafeCastId

-- ---------- INMessageLinkMetadata ----------

-- | Phantom type for @INMessageLinkMetadata@.
data INMessageLinkMetadata

instance IsObjCObject (Id INMessageLinkMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMessageLinkMetadata"

class IsNSObject a => IsINMessageLinkMetadata a where
  toINMessageLinkMetadata :: a -> Id INMessageLinkMetadata

instance IsINMessageLinkMetadata (Id INMessageLinkMetadata) where
  toINMessageLinkMetadata = unsafeCastId

instance IsNSObject (Id INMessageLinkMetadata) where
  toNSObject = unsafeCastId

-- ---------- INMessageReaction ----------

-- | Phantom type for @INMessageReaction@.
data INMessageReaction

instance IsObjCObject (Id INMessageReaction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMessageReaction"

class IsNSObject a => IsINMessageReaction a where
  toINMessageReaction :: a -> Id INMessageReaction

instance IsINMessageReaction (Id INMessageReaction) where
  toINMessageReaction = unsafeCastId

instance IsNSObject (Id INMessageReaction) where
  toNSObject = unsafeCastId

-- ---------- INNote ----------

-- | Phantom type for @INNote@.
data INNote

instance IsObjCObject (Id INNote) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INNote"

class IsNSObject a => IsINNote a where
  toINNote :: a -> Id INNote

instance IsINNote (Id INNote) where
  toINNote = unsafeCastId

instance IsNSObject (Id INNote) where
  toNSObject = unsafeCastId

-- ---------- INNoteContent ----------

-- | Phantom type for @INNoteContent@.
data INNoteContent

instance IsObjCObject (Id INNoteContent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INNoteContent"

class IsNSObject a => IsINNoteContent a where
  toINNoteContent :: a -> Id INNoteContent

instance IsINNoteContent (Id INNoteContent) where
  toINNoteContent = unsafeCastId

instance IsNSObject (Id INNoteContent) where
  toNSObject = unsafeCastId

-- ---------- INObject ----------

-- | Phantom type for @INObject@.
data INObject

instance IsObjCObject (Id INObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INObject"

class IsNSObject a => IsINObject a where
  toINObject :: a -> Id INObject

instance IsINObject (Id INObject) where
  toINObject = unsafeCastId

instance IsNSObject (Id INObject) where
  toNSObject = unsafeCastId

-- ---------- INObjectCollection ----------

-- | Phantom type for @INObjectCollection@.
data INObjectCollection

instance IsObjCObject (Id INObjectCollection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INObjectCollection"

class IsNSObject a => IsINObjectCollection a where
  toINObjectCollection :: a -> Id INObjectCollection

instance IsINObjectCollection (Id INObjectCollection) where
  toINObjectCollection = unsafeCastId

instance IsNSObject (Id INObjectCollection) where
  toNSObject = unsafeCastId

-- ---------- INObjectSection ----------

-- | Phantom type for @INObjectSection@.
data INObjectSection

instance IsObjCObject (Id INObjectSection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INObjectSection"

class IsNSObject a => IsINObjectSection a where
  toINObjectSection :: a -> Id INObjectSection

instance IsINObjectSection (Id INObjectSection) where
  toINObjectSection = unsafeCastId

instance IsNSObject (Id INObjectSection) where
  toNSObject = unsafeCastId

-- ---------- INParameter ----------

-- | Phantom type for @INParameter@.
data INParameter

instance IsObjCObject (Id INParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INParameter"

class IsNSObject a => IsINParameter a where
  toINParameter :: a -> Id INParameter

instance IsINParameter (Id INParameter) where
  toINParameter = unsafeCastId

instance IsNSObject (Id INParameter) where
  toNSObject = unsafeCastId

-- ---------- INPaymentAccount ----------

-- | Phantom type for @INPaymentAccount@.
data INPaymentAccount

instance IsObjCObject (Id INPaymentAccount) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPaymentAccount"

class IsNSObject a => IsINPaymentAccount a where
  toINPaymentAccount :: a -> Id INPaymentAccount

instance IsINPaymentAccount (Id INPaymentAccount) where
  toINPaymentAccount = unsafeCastId

instance IsNSObject (Id INPaymentAccount) where
  toNSObject = unsafeCastId

-- ---------- INPaymentAmount ----------

-- | Phantom type for @INPaymentAmount@.
data INPaymentAmount

instance IsObjCObject (Id INPaymentAmount) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPaymentAmount"

class IsNSObject a => IsINPaymentAmount a where
  toINPaymentAmount :: a -> Id INPaymentAmount

instance IsINPaymentAmount (Id INPaymentAmount) where
  toINPaymentAmount = unsafeCastId

instance IsNSObject (Id INPaymentAmount) where
  toNSObject = unsafeCastId

-- ---------- INPaymentMethod ----------

-- | Phantom type for @INPaymentMethod@.
data INPaymentMethod

instance IsObjCObject (Id INPaymentMethod) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPaymentMethod"

class IsNSObject a => IsINPaymentMethod a where
  toINPaymentMethod :: a -> Id INPaymentMethod

instance IsINPaymentMethod (Id INPaymentMethod) where
  toINPaymentMethod = unsafeCastId

instance IsNSObject (Id INPaymentMethod) where
  toNSObject = unsafeCastId

-- ---------- INPaymentRecord ----------

-- | Phantom type for @INPaymentRecord@.
data INPaymentRecord

instance IsObjCObject (Id INPaymentRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPaymentRecord"

class IsNSObject a => IsINPaymentRecord a where
  toINPaymentRecord :: a -> Id INPaymentRecord

instance IsINPaymentRecord (Id INPaymentRecord) where
  toINPaymentRecord = unsafeCastId

instance IsNSObject (Id INPaymentRecord) where
  toNSObject = unsafeCastId

-- ---------- INPerson ----------

-- | Phantom type for @INPerson@.
data INPerson

instance IsObjCObject (Id INPerson) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPerson"

class IsNSObject a => IsINPerson a where
  toINPerson :: a -> Id INPerson

instance IsINPerson (Id INPerson) where
  toINPerson = unsafeCastId

instance IsNSObject (Id INPerson) where
  toNSObject = unsafeCastId

-- ---------- INPersonHandle ----------

-- | Phantom type for @INPersonHandle@.
data INPersonHandle

instance IsObjCObject (Id INPersonHandle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPersonHandle"

class IsNSObject a => IsINPersonHandle a where
  toINPersonHandle :: a -> Id INPersonHandle

instance IsINPersonHandle (Id INPersonHandle) where
  toINPersonHandle = unsafeCastId

instance IsNSObject (Id INPersonHandle) where
  toNSObject = unsafeCastId

-- ---------- INPreferences ----------

-- | Phantom type for @INPreferences@.
data INPreferences

instance IsObjCObject (Id INPreferences) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPreferences"

class IsNSObject a => IsINPreferences a where
  toINPreferences :: a -> Id INPreferences

instance IsINPreferences (Id INPreferences) where
  toINPreferences = unsafeCastId

instance IsNSObject (Id INPreferences) where
  toNSObject = unsafeCastId

-- ---------- INPriceRange ----------

-- | Phantom type for @INPriceRange@.
data INPriceRange

instance IsObjCObject (Id INPriceRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPriceRange"

class IsNSObject a => IsINPriceRange a where
  toINPriceRange :: a -> Id INPriceRange

instance IsINPriceRange (Id INPriceRange) where
  toINPriceRange = unsafeCastId

instance IsNSObject (Id INPriceRange) where
  toNSObject = unsafeCastId

-- ---------- INRecurrenceRule ----------

-- | Phantom type for @INRecurrenceRule@.
data INRecurrenceRule

instance IsObjCObject (Id INRecurrenceRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRecurrenceRule"

class IsNSObject a => IsINRecurrenceRule a where
  toINRecurrenceRule :: a -> Id INRecurrenceRule

instance IsINRecurrenceRule (Id INRecurrenceRule) where
  toINRecurrenceRule = unsafeCastId

instance IsNSObject (Id INRecurrenceRule) where
  toNSObject = unsafeCastId

-- ---------- INRelevanceProvider ----------

-- | A relevance provider represents a piece of relevance information that can be used by Siri when predicting relevant shortcuts.
-- 
-- Phantom type for @INRelevanceProvider@.
data INRelevanceProvider

instance IsObjCObject (Id INRelevanceProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRelevanceProvider"

class IsNSObject a => IsINRelevanceProvider a where
  toINRelevanceProvider :: a -> Id INRelevanceProvider

instance IsINRelevanceProvider (Id INRelevanceProvider) where
  toINRelevanceProvider = unsafeCastId

instance IsNSObject (Id INRelevanceProvider) where
  toNSObject = unsafeCastId

-- ---------- INRelevantShortcut ----------

-- | Lets you provide relevant shortcut to Siri, for display on the Siri Watch Face.
--
-- Including relevance information allows Siri to make suggestions for shortcuts that the user might be interested in but has not previously performed.
-- 
-- Phantom type for @INRelevantShortcut@.
data INRelevantShortcut

instance IsObjCObject (Id INRelevantShortcut) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRelevantShortcut"

class IsNSObject a => IsINRelevantShortcut a where
  toINRelevantShortcut :: a -> Id INRelevantShortcut

instance IsINRelevantShortcut (Id INRelevantShortcut) where
  toINRelevantShortcut = unsafeCastId

instance IsNSObject (Id INRelevantShortcut) where
  toNSObject = unsafeCastId

-- ---------- INRelevantShortcutStore ----------

-- | Where relevant shortcuts are provided to Siri.
--
-- INRelevantShortcut
-- 
-- Phantom type for @INRelevantShortcutStore@.
data INRelevantShortcutStore

instance IsObjCObject (Id INRelevantShortcutStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRelevantShortcutStore"

class IsNSObject a => IsINRelevantShortcutStore a where
  toINRelevantShortcutStore :: a -> Id INRelevantShortcutStore

instance IsINRelevantShortcutStore (Id INRelevantShortcutStore) where
  toINRelevantShortcutStore = unsafeCastId

instance IsNSObject (Id INRelevantShortcutStore) where
  toNSObject = unsafeCastId

-- ---------- INRentalCar ----------

-- | Phantom type for @INRentalCar@.
data INRentalCar

instance IsObjCObject (Id INRentalCar) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRentalCar"

class IsNSObject a => IsINRentalCar a where
  toINRentalCar :: a -> Id INRentalCar

instance IsINRentalCar (Id INRentalCar) where
  toINRentalCar = unsafeCastId

instance IsNSObject (Id INRentalCar) where
  toNSObject = unsafeCastId

-- ---------- INReservation ----------

-- | Phantom type for @INReservation@.
data INReservation

instance IsObjCObject (Id INReservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INReservation"

class IsNSObject a => IsINReservation a where
  toINReservation :: a -> Id INReservation

instance IsINReservation (Id INReservation) where
  toINReservation = unsafeCastId

instance IsNSObject (Id INReservation) where
  toNSObject = unsafeCastId

-- ---------- INReservationAction ----------

-- | Phantom type for @INReservationAction@.
data INReservationAction

instance IsObjCObject (Id INReservationAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INReservationAction"

class IsNSObject a => IsINReservationAction a where
  toINReservationAction :: a -> Id INReservationAction

instance IsINReservationAction (Id INReservationAction) where
  toINReservationAction = unsafeCastId

instance IsNSObject (Id INReservationAction) where
  toNSObject = unsafeCastId

-- ---------- INRestaurant ----------

-- | Phantom type for @INRestaurant@.
data INRestaurant

instance IsObjCObject (Id INRestaurant) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRestaurant"

class IsNSObject a => IsINRestaurant a where
  toINRestaurant :: a -> Id INRestaurant

instance IsINRestaurant (Id INRestaurant) where
  toINRestaurant = unsafeCastId

instance IsNSObject (Id INRestaurant) where
  toNSObject = unsafeCastId

-- ---------- INRestaurantGuestDisplayPreferences ----------

-- | Phantom type for @INRestaurantGuestDisplayPreferences@.
data INRestaurantGuestDisplayPreferences

instance IsObjCObject (Id INRestaurantGuestDisplayPreferences) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRestaurantGuestDisplayPreferences"

class IsNSObject a => IsINRestaurantGuestDisplayPreferences a where
  toINRestaurantGuestDisplayPreferences :: a -> Id INRestaurantGuestDisplayPreferences

instance IsINRestaurantGuestDisplayPreferences (Id INRestaurantGuestDisplayPreferences) where
  toINRestaurantGuestDisplayPreferences = unsafeCastId

instance IsNSObject (Id INRestaurantGuestDisplayPreferences) where
  toNSObject = unsafeCastId

-- ---------- INRestaurantOffer ----------

-- | Phantom type for @INRestaurantOffer@.
data INRestaurantOffer

instance IsObjCObject (Id INRestaurantOffer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRestaurantOffer"

class IsNSObject a => IsINRestaurantOffer a where
  toINRestaurantOffer :: a -> Id INRestaurantOffer

instance IsINRestaurantOffer (Id INRestaurantOffer) where
  toINRestaurantOffer = unsafeCastId

instance IsNSObject (Id INRestaurantOffer) where
  toNSObject = unsafeCastId

-- ---------- INRestaurantReservationBooking ----------

-- | Phantom type for @INRestaurantReservationBooking@.
data INRestaurantReservationBooking

instance IsObjCObject (Id INRestaurantReservationBooking) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRestaurantReservationBooking"

class IsNSObject a => IsINRestaurantReservationBooking a where
  toINRestaurantReservationBooking :: a -> Id INRestaurantReservationBooking

instance IsINRestaurantReservationBooking (Id INRestaurantReservationBooking) where
  toINRestaurantReservationBooking = unsafeCastId

instance IsNSObject (Id INRestaurantReservationBooking) where
  toNSObject = unsafeCastId

-- ---------- INRideCompletionStatus ----------

-- | Phantom type for @INRideCompletionStatus@.
data INRideCompletionStatus

instance IsObjCObject (Id INRideCompletionStatus) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRideCompletionStatus"

class IsNSObject a => IsINRideCompletionStatus a where
  toINRideCompletionStatus :: a -> Id INRideCompletionStatus

instance IsINRideCompletionStatus (Id INRideCompletionStatus) where
  toINRideCompletionStatus = unsafeCastId

instance IsNSObject (Id INRideCompletionStatus) where
  toNSObject = unsafeCastId

-- ---------- INRideFareLineItem ----------

-- | Phantom type for @INRideFareLineItem@.
data INRideFareLineItem

instance IsObjCObject (Id INRideFareLineItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRideFareLineItem"

class IsNSObject a => IsINRideFareLineItem a where
  toINRideFareLineItem :: a -> Id INRideFareLineItem

instance IsINRideFareLineItem (Id INRideFareLineItem) where
  toINRideFareLineItem = unsafeCastId

instance IsNSObject (Id INRideFareLineItem) where
  toNSObject = unsafeCastId

-- ---------- INRideOption ----------

-- | Phantom type for @INRideOption@.
data INRideOption

instance IsObjCObject (Id INRideOption) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRideOption"

class IsNSObject a => IsINRideOption a where
  toINRideOption :: a -> Id INRideOption

instance IsINRideOption (Id INRideOption) where
  toINRideOption = unsafeCastId

instance IsNSObject (Id INRideOption) where
  toNSObject = unsafeCastId

-- ---------- INRidePartySizeOption ----------

-- | Phantom type for @INRidePartySizeOption@.
data INRidePartySizeOption

instance IsObjCObject (Id INRidePartySizeOption) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRidePartySizeOption"

class IsNSObject a => IsINRidePartySizeOption a where
  toINRidePartySizeOption :: a -> Id INRidePartySizeOption

instance IsINRidePartySizeOption (Id INRidePartySizeOption) where
  toINRidePartySizeOption = unsafeCastId

instance IsNSObject (Id INRidePartySizeOption) where
  toNSObject = unsafeCastId

-- ---------- INRideStatus ----------

-- | Phantom type for @INRideStatus@.
data INRideStatus

instance IsObjCObject (Id INRideStatus) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRideStatus"

class IsNSObject a => IsINRideStatus a where
  toINRideStatus :: a -> Id INRideStatus

instance IsINRideStatus (Id INRideStatus) where
  toINRideStatus = unsafeCastId

instance IsNSObject (Id INRideStatus) where
  toNSObject = unsafeCastId

-- ---------- INRideVehicle ----------

-- | Phantom type for @INRideVehicle@.
data INRideVehicle

instance IsObjCObject (Id INRideVehicle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRideVehicle"

class IsNSObject a => IsINRideVehicle a where
  toINRideVehicle :: a -> Id INRideVehicle

instance IsINRideVehicle (Id INRideVehicle) where
  toINRideVehicle = unsafeCastId

instance IsNSObject (Id INRideVehicle) where
  toNSObject = unsafeCastId

-- ---------- INSeat ----------

-- | Phantom type for @INSeat@.
data INSeat

instance IsObjCObject (Id INSeat) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSeat"

class IsNSObject a => IsINSeat a where
  toINSeat :: a -> Id INSeat

instance IsINSeat (Id INSeat) where
  toINSeat = unsafeCastId

instance IsNSObject (Id INSeat) where
  toNSObject = unsafeCastId

-- ---------- INSendMessageAttachment ----------

-- | Phantom type for @INSendMessageAttachment@.
data INSendMessageAttachment

instance IsObjCObject (Id INSendMessageAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSendMessageAttachment"

class IsNSObject a => IsINSendMessageAttachment a where
  toINSendMessageAttachment :: a -> Id INSendMessageAttachment

instance IsINSendMessageAttachment (Id INSendMessageAttachment) where
  toINSendMessageAttachment = unsafeCastId

instance IsNSObject (Id INSendMessageAttachment) where
  toNSObject = unsafeCastId

-- ---------- INShortcut ----------

-- | A shortcut is an action that can be suggested by the system or added to Siri.
-- 
-- Phantom type for @INShortcut@.
data INShortcut

instance IsObjCObject (Id INShortcut) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INShortcut"

class IsNSObject a => IsINShortcut a where
  toINShortcut :: a -> Id INShortcut

instance IsINShortcut (Id INShortcut) where
  toINShortcut = unsafeCastId

instance IsNSObject (Id INShortcut) where
  toNSObject = unsafeCastId

-- ---------- INSpatialEventTrigger ----------

-- | Phantom type for @INSpatialEventTrigger@.
data INSpatialEventTrigger

instance IsObjCObject (Id INSpatialEventTrigger) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSpatialEventTrigger"

class IsNSObject a => IsINSpatialEventTrigger a where
  toINSpatialEventTrigger :: a -> Id INSpatialEventTrigger

instance IsINSpatialEventTrigger (Id INSpatialEventTrigger) where
  toINSpatialEventTrigger = unsafeCastId

instance IsNSObject (Id INSpatialEventTrigger) where
  toNSObject = unsafeCastId

-- ---------- INSpeakableString ----------

-- | Phantom type for @INSpeakableString@.
data INSpeakableString

instance IsObjCObject (Id INSpeakableString) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSpeakableString"

class IsNSObject a => IsINSpeakableString a where
  toINSpeakableString :: a -> Id INSpeakableString

instance IsINSpeakableString (Id INSpeakableString) where
  toINSpeakableString = unsafeCastId

instance IsNSObject (Id INSpeakableString) where
  toNSObject = unsafeCastId

-- ---------- INSticker ----------

-- | An object that describes a sticker someone sends in a message.
-- 
-- Phantom type for @INSticker@.
data INSticker

instance IsObjCObject (Id INSticker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSticker"

class IsNSObject a => IsINSticker a where
  toINSticker :: a -> Id INSticker

instance IsINSticker (Id INSticker) where
  toINSticker = unsafeCastId

instance IsNSObject (Id INSticker) where
  toNSObject = unsafeCastId

-- ---------- INTask ----------

-- | Phantom type for @INTask@.
data INTask

instance IsObjCObject (Id INTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTask"

class IsNSObject a => IsINTask a where
  toINTask :: a -> Id INTask

instance IsINTask (Id INTask) where
  toINTask = unsafeCastId

instance IsNSObject (Id INTask) where
  toNSObject = unsafeCastId

-- ---------- INTaskList ----------

-- | Phantom type for @INTaskList@.
data INTaskList

instance IsObjCObject (Id INTaskList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTaskList"

class IsNSObject a => IsINTaskList a where
  toINTaskList :: a -> Id INTaskList

instance IsINTaskList (Id INTaskList) where
  toINTaskList = unsafeCastId

instance IsNSObject (Id INTaskList) where
  toNSObject = unsafeCastId

-- ---------- INTemporalEventTrigger ----------

-- | Phantom type for @INTemporalEventTrigger@.
data INTemporalEventTrigger

instance IsObjCObject (Id INTemporalEventTrigger) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTemporalEventTrigger"

class IsNSObject a => IsINTemporalEventTrigger a where
  toINTemporalEventTrigger :: a -> Id INTemporalEventTrigger

instance IsINTemporalEventTrigger (Id INTemporalEventTrigger) where
  toINTemporalEventTrigger = unsafeCastId

instance IsNSObject (Id INTemporalEventTrigger) where
  toNSObject = unsafeCastId

-- ---------- INTermsAndConditions ----------

-- | Phantom type for @INTermsAndConditions@.
data INTermsAndConditions

instance IsObjCObject (Id INTermsAndConditions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTermsAndConditions"

class IsNSObject a => IsINTermsAndConditions a where
  toINTermsAndConditions :: a -> Id INTermsAndConditions

instance IsINTermsAndConditions (Id INTermsAndConditions) where
  toINTermsAndConditions = unsafeCastId

instance IsNSObject (Id INTermsAndConditions) where
  toNSObject = unsafeCastId

-- ---------- INTicketedEvent ----------

-- | Phantom type for @INTicketedEvent@.
data INTicketedEvent

instance IsObjCObject (Id INTicketedEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTicketedEvent"

class IsNSObject a => IsINTicketedEvent a where
  toINTicketedEvent :: a -> Id INTicketedEvent

instance IsINTicketedEvent (Id INTicketedEvent) where
  toINTicketedEvent = unsafeCastId

instance IsNSObject (Id INTicketedEvent) where
  toNSObject = unsafeCastId

-- ---------- INTrainTrip ----------

-- | Phantom type for @INTrainTrip@.
data INTrainTrip

instance IsObjCObject (Id INTrainTrip) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTrainTrip"

class IsNSObject a => IsINTrainTrip a where
  toINTrainTrip :: a -> Id INTrainTrip

instance IsINTrainTrip (Id INTrainTrip) where
  toINTrainTrip = unsafeCastId

instance IsNSObject (Id INTrainTrip) where
  toNSObject = unsafeCastId

-- ---------- INUpcomingMediaManager ----------

-- | Phantom type for @INUpcomingMediaManager@.
data INUpcomingMediaManager

instance IsObjCObject (Id INUpcomingMediaManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INUpcomingMediaManager"

class IsNSObject a => IsINUpcomingMediaManager a where
  toINUpcomingMediaManager :: a -> Id INUpcomingMediaManager

instance IsINUpcomingMediaManager (Id INUpcomingMediaManager) where
  toINUpcomingMediaManager = unsafeCastId

instance IsNSObject (Id INUpcomingMediaManager) where
  toNSObject = unsafeCastId

-- ---------- INUserContext ----------

-- | Phantom type for @INUserContext@.
data INUserContext

instance IsObjCObject (Id INUserContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INUserContext"

class IsNSObject a => IsINUserContext a where
  toINUserContext :: a -> Id INUserContext

instance IsINUserContext (Id INUserContext) where
  toINUserContext = unsafeCastId

instance IsNSObject (Id INUserContext) where
  toNSObject = unsafeCastId

-- ---------- INVocabulary ----------

-- | Phantom type for @INVocabulary@.
data INVocabulary

instance IsObjCObject (Id INVocabulary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INVocabulary"

class IsNSObject a => IsINVocabulary a where
  toINVocabulary :: a -> Id INVocabulary

instance IsINVocabulary (Id INVocabulary) where
  toINVocabulary = unsafeCastId

instance IsNSObject (Id INVocabulary) where
  toNSObject = unsafeCastId

-- ---------- INVoiceShortcut ----------

-- | A shortcut that has been added to Siri
-- 
-- Phantom type for @INVoiceShortcut@.
data INVoiceShortcut

instance IsObjCObject (Id INVoiceShortcut) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INVoiceShortcut"

class IsNSObject a => IsINVoiceShortcut a where
  toINVoiceShortcut :: a -> Id INVoiceShortcut

instance IsINVoiceShortcut (Id INVoiceShortcut) where
  toINVoiceShortcut = unsafeCastId

instance IsNSObject (Id INVoiceShortcut) where
  toNSObject = unsafeCastId

-- ---------- INVoiceShortcutCenter ----------

-- | Lets you access shortcuts that have been added to Siri
--
-- INVoiceShortcut
-- 
-- Phantom type for @INVoiceShortcutCenter@.
data INVoiceShortcutCenter

instance IsObjCObject (Id INVoiceShortcutCenter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INVoiceShortcutCenter"

class IsNSObject a => IsINVoiceShortcutCenter a where
  toINVoiceShortcutCenter :: a -> Id INVoiceShortcutCenter

instance IsINVoiceShortcutCenter (Id INVoiceShortcutCenter) where
  toINVoiceShortcutCenter = unsafeCastId

instance IsNSObject (Id INVoiceShortcutCenter) where
  toNSObject = unsafeCastId

-- ---------- UNNotificationContent ----------

-- | Phantom type for @UNNotificationContent@.
data UNNotificationContent

instance IsObjCObject (Id UNNotificationContent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotificationContent"

class IsNSObject a => IsUNNotificationContent a where
  toUNNotificationContent :: a -> Id UNNotificationContent

instance IsUNNotificationContent (Id UNNotificationContent) where
  toUNNotificationContent = unsafeCastId

instance IsNSObject (Id UNNotificationContent) where
  toNSObject = unsafeCastId

-- ---------- INActivateCarSignalIntent ----------

-- | Phantom type for @INActivateCarSignalIntent@.
data INActivateCarSignalIntent

instance IsObjCObject (Id INActivateCarSignalIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INActivateCarSignalIntent"

class IsINIntent a => IsINActivateCarSignalIntent a where
  toINActivateCarSignalIntent :: a -> Id INActivateCarSignalIntent

instance IsINActivateCarSignalIntent (Id INActivateCarSignalIntent) where
  toINActivateCarSignalIntent = unsafeCastId

instance IsINIntent (Id INActivateCarSignalIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INActivateCarSignalIntent) where
  toNSObject = unsafeCastId

-- ---------- INAddMediaIntent ----------

-- | Phantom type for @INAddMediaIntent@.
data INAddMediaIntent

instance IsObjCObject (Id INAddMediaIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAddMediaIntent"

class IsINIntent a => IsINAddMediaIntent a where
  toINAddMediaIntent :: a -> Id INAddMediaIntent

instance IsINAddMediaIntent (Id INAddMediaIntent) where
  toINAddMediaIntent = unsafeCastId

instance IsINIntent (Id INAddMediaIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INAddMediaIntent) where
  toNSObject = unsafeCastId

-- ---------- INAddTasksIntent ----------

-- | Phantom type for @INAddTasksIntent@.
data INAddTasksIntent

instance IsObjCObject (Id INAddTasksIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAddTasksIntent"

class IsINIntent a => IsINAddTasksIntent a where
  toINAddTasksIntent :: a -> Id INAddTasksIntent

instance IsINAddTasksIntent (Id INAddTasksIntent) where
  toINAddTasksIntent = unsafeCastId

instance IsINIntent (Id INAddTasksIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INAddTasksIntent) where
  toNSObject = unsafeCastId

-- ---------- INAnswerCallIntent ----------

-- | Phantom type for @INAnswerCallIntent@.
data INAnswerCallIntent

instance IsObjCObject (Id INAnswerCallIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAnswerCallIntent"

class IsINIntent a => IsINAnswerCallIntent a where
  toINAnswerCallIntent :: a -> Id INAnswerCallIntent

instance IsINAnswerCallIntent (Id INAnswerCallIntent) where
  toINAnswerCallIntent = unsafeCastId

instance IsINIntent (Id INAnswerCallIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INAnswerCallIntent) where
  toNSObject = unsafeCastId

-- ---------- INAppendToNoteIntent ----------

-- | Phantom type for @INAppendToNoteIntent@.
data INAppendToNoteIntent

instance IsObjCObject (Id INAppendToNoteIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAppendToNoteIntent"

class IsINIntent a => IsINAppendToNoteIntent a where
  toINAppendToNoteIntent :: a -> Id INAppendToNoteIntent

instance IsINAppendToNoteIntent (Id INAppendToNoteIntent) where
  toINAppendToNoteIntent = unsafeCastId

instance IsINIntent (Id INAppendToNoteIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INAppendToNoteIntent) where
  toNSObject = unsafeCastId

-- ---------- INBookRestaurantReservationIntent ----------

-- | Phantom type for @INBookRestaurantReservationIntent@.
data INBookRestaurantReservationIntent

instance IsObjCObject (Id INBookRestaurantReservationIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBookRestaurantReservationIntent"

class IsINIntent a => IsINBookRestaurantReservationIntent a where
  toINBookRestaurantReservationIntent :: a -> Id INBookRestaurantReservationIntent

instance IsINBookRestaurantReservationIntent (Id INBookRestaurantReservationIntent) where
  toINBookRestaurantReservationIntent = unsafeCastId

instance IsINIntent (Id INBookRestaurantReservationIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INBookRestaurantReservationIntent) where
  toNSObject = unsafeCastId

-- ---------- INCancelRideIntent ----------

-- | Phantom type for @INCancelRideIntent@.
data INCancelRideIntent

instance IsObjCObject (Id INCancelRideIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCancelRideIntent"

class IsINIntent a => IsINCancelRideIntent a where
  toINCancelRideIntent :: a -> Id INCancelRideIntent

instance IsINCancelRideIntent (Id INCancelRideIntent) where
  toINCancelRideIntent = unsafeCastId

instance IsINIntent (Id INCancelRideIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INCancelRideIntent) where
  toNSObject = unsafeCastId

-- ---------- INCancelWorkoutIntent ----------

-- | Phantom type for @INCancelWorkoutIntent@.
data INCancelWorkoutIntent

instance IsObjCObject (Id INCancelWorkoutIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCancelWorkoutIntent"

class IsINIntent a => IsINCancelWorkoutIntent a where
  toINCancelWorkoutIntent :: a -> Id INCancelWorkoutIntent

instance IsINCancelWorkoutIntent (Id INCancelWorkoutIntent) where
  toINCancelWorkoutIntent = unsafeCastId

instance IsINIntent (Id INCancelWorkoutIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INCancelWorkoutIntent) where
  toNSObject = unsafeCastId

-- ---------- INCreateNoteIntent ----------

-- | Phantom type for @INCreateNoteIntent@.
data INCreateNoteIntent

instance IsObjCObject (Id INCreateNoteIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCreateNoteIntent"

class IsINIntent a => IsINCreateNoteIntent a where
  toINCreateNoteIntent :: a -> Id INCreateNoteIntent

instance IsINCreateNoteIntent (Id INCreateNoteIntent) where
  toINCreateNoteIntent = unsafeCastId

instance IsINIntent (Id INCreateNoteIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INCreateNoteIntent) where
  toNSObject = unsafeCastId

-- ---------- INCreateTaskListIntent ----------

-- | Phantom type for @INCreateTaskListIntent@.
data INCreateTaskListIntent

instance IsObjCObject (Id INCreateTaskListIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCreateTaskListIntent"

class IsINIntent a => IsINCreateTaskListIntent a where
  toINCreateTaskListIntent :: a -> Id INCreateTaskListIntent

instance IsINCreateTaskListIntent (Id INCreateTaskListIntent) where
  toINCreateTaskListIntent = unsafeCastId

instance IsINIntent (Id INCreateTaskListIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INCreateTaskListIntent) where
  toNSObject = unsafeCastId

-- ---------- INDeleteTasksIntent ----------

-- | Phantom type for @INDeleteTasksIntent@.
data INDeleteTasksIntent

instance IsObjCObject (Id INDeleteTasksIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INDeleteTasksIntent"

class IsINIntent a => IsINDeleteTasksIntent a where
  toINDeleteTasksIntent :: a -> Id INDeleteTasksIntent

instance IsINDeleteTasksIntent (Id INDeleteTasksIntent) where
  toINDeleteTasksIntent = unsafeCastId

instance IsINIntent (Id INDeleteTasksIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INDeleteTasksIntent) where
  toNSObject = unsafeCastId

-- ---------- INEditMessageIntent ----------

-- | Phantom type for @INEditMessageIntent@.
data INEditMessageIntent

instance IsObjCObject (Id INEditMessageIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INEditMessageIntent"

class IsINIntent a => IsINEditMessageIntent a where
  toINEditMessageIntent :: a -> Id INEditMessageIntent

instance IsINEditMessageIntent (Id INEditMessageIntent) where
  toINEditMessageIntent = unsafeCastId

instance IsINIntent (Id INEditMessageIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INEditMessageIntent) where
  toNSObject = unsafeCastId

-- ---------- INEndWorkoutIntent ----------

-- | Phantom type for @INEndWorkoutIntent@.
data INEndWorkoutIntent

instance IsObjCObject (Id INEndWorkoutIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INEndWorkoutIntent"

class IsINIntent a => IsINEndWorkoutIntent a where
  toINEndWorkoutIntent :: a -> Id INEndWorkoutIntent

instance IsINEndWorkoutIntent (Id INEndWorkoutIntent) where
  toINEndWorkoutIntent = unsafeCastId

instance IsINIntent (Id INEndWorkoutIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INEndWorkoutIntent) where
  toNSObject = unsafeCastId

-- ---------- INGetAvailableRestaurantReservationBookingDefaultsIntent ----------

-- | Phantom type for @INGetAvailableRestaurantReservationBookingDefaultsIntent@.
data INGetAvailableRestaurantReservationBookingDefaultsIntent

instance IsObjCObject (Id INGetAvailableRestaurantReservationBookingDefaultsIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetAvailableRestaurantReservationBookingDefaultsIntent"

class IsINIntent a => IsINGetAvailableRestaurantReservationBookingDefaultsIntent a where
  toINGetAvailableRestaurantReservationBookingDefaultsIntent :: a -> Id INGetAvailableRestaurantReservationBookingDefaultsIntent

instance IsINGetAvailableRestaurantReservationBookingDefaultsIntent (Id INGetAvailableRestaurantReservationBookingDefaultsIntent) where
  toINGetAvailableRestaurantReservationBookingDefaultsIntent = unsafeCastId

instance IsINIntent (Id INGetAvailableRestaurantReservationBookingDefaultsIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INGetAvailableRestaurantReservationBookingDefaultsIntent) where
  toNSObject = unsafeCastId

-- ---------- INGetAvailableRestaurantReservationBookingsIntent ----------

-- | Phantom type for @INGetAvailableRestaurantReservationBookingsIntent@.
data INGetAvailableRestaurantReservationBookingsIntent

instance IsObjCObject (Id INGetAvailableRestaurantReservationBookingsIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetAvailableRestaurantReservationBookingsIntent"

class IsINIntent a => IsINGetAvailableRestaurantReservationBookingsIntent a where
  toINGetAvailableRestaurantReservationBookingsIntent :: a -> Id INGetAvailableRestaurantReservationBookingsIntent

instance IsINGetAvailableRestaurantReservationBookingsIntent (Id INGetAvailableRestaurantReservationBookingsIntent) where
  toINGetAvailableRestaurantReservationBookingsIntent = unsafeCastId

instance IsINIntent (Id INGetAvailableRestaurantReservationBookingsIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INGetAvailableRestaurantReservationBookingsIntent) where
  toNSObject = unsafeCastId

-- ---------- INGetCarLockStatusIntent ----------

-- | Phantom type for @INGetCarLockStatusIntent@.
data INGetCarLockStatusIntent

instance IsObjCObject (Id INGetCarLockStatusIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetCarLockStatusIntent"

class IsINIntent a => IsINGetCarLockStatusIntent a where
  toINGetCarLockStatusIntent :: a -> Id INGetCarLockStatusIntent

instance IsINGetCarLockStatusIntent (Id INGetCarLockStatusIntent) where
  toINGetCarLockStatusIntent = unsafeCastId

instance IsINIntent (Id INGetCarLockStatusIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INGetCarLockStatusIntent) where
  toNSObject = unsafeCastId

-- ---------- INGetCarPowerLevelStatusIntent ----------

-- | Phantom type for @INGetCarPowerLevelStatusIntent@.
data INGetCarPowerLevelStatusIntent

instance IsObjCObject (Id INGetCarPowerLevelStatusIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetCarPowerLevelStatusIntent"

class IsINIntent a => IsINGetCarPowerLevelStatusIntent a where
  toINGetCarPowerLevelStatusIntent :: a -> Id INGetCarPowerLevelStatusIntent

instance IsINGetCarPowerLevelStatusIntent (Id INGetCarPowerLevelStatusIntent) where
  toINGetCarPowerLevelStatusIntent = unsafeCastId

instance IsINIntent (Id INGetCarPowerLevelStatusIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INGetCarPowerLevelStatusIntent) where
  toNSObject = unsafeCastId

-- ---------- INGetReservationDetailsIntent ----------

-- | Phantom type for @INGetReservationDetailsIntent@.
data INGetReservationDetailsIntent

instance IsObjCObject (Id INGetReservationDetailsIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetReservationDetailsIntent"

class IsINIntent a => IsINGetReservationDetailsIntent a where
  toINGetReservationDetailsIntent :: a -> Id INGetReservationDetailsIntent

instance IsINGetReservationDetailsIntent (Id INGetReservationDetailsIntent) where
  toINGetReservationDetailsIntent = unsafeCastId

instance IsINIntent (Id INGetReservationDetailsIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INGetReservationDetailsIntent) where
  toNSObject = unsafeCastId

-- ---------- INGetRestaurantGuestIntent ----------

-- | Phantom type for @INGetRestaurantGuestIntent@.
data INGetRestaurantGuestIntent

instance IsObjCObject (Id INGetRestaurantGuestIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetRestaurantGuestIntent"

class IsINIntent a => IsINGetRestaurantGuestIntent a where
  toINGetRestaurantGuestIntent :: a -> Id INGetRestaurantGuestIntent

instance IsINGetRestaurantGuestIntent (Id INGetRestaurantGuestIntent) where
  toINGetRestaurantGuestIntent = unsafeCastId

instance IsINIntent (Id INGetRestaurantGuestIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INGetRestaurantGuestIntent) where
  toNSObject = unsafeCastId

-- ---------- INGetRideStatusIntent ----------

-- | Phantom type for @INGetRideStatusIntent@.
data INGetRideStatusIntent

instance IsObjCObject (Id INGetRideStatusIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetRideStatusIntent"

class IsINIntent a => IsINGetRideStatusIntent a where
  toINGetRideStatusIntent :: a -> Id INGetRideStatusIntent

instance IsINGetRideStatusIntent (Id INGetRideStatusIntent) where
  toINGetRideStatusIntent = unsafeCastId

instance IsINIntent (Id INGetRideStatusIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INGetRideStatusIntent) where
  toNSObject = unsafeCastId

-- ---------- INGetUserCurrentRestaurantReservationBookingsIntent ----------

-- | Phantom type for @INGetUserCurrentRestaurantReservationBookingsIntent@.
data INGetUserCurrentRestaurantReservationBookingsIntent

instance IsObjCObject (Id INGetUserCurrentRestaurantReservationBookingsIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetUserCurrentRestaurantReservationBookingsIntent"

class IsINIntent a => IsINGetUserCurrentRestaurantReservationBookingsIntent a where
  toINGetUserCurrentRestaurantReservationBookingsIntent :: a -> Id INGetUserCurrentRestaurantReservationBookingsIntent

instance IsINGetUserCurrentRestaurantReservationBookingsIntent (Id INGetUserCurrentRestaurantReservationBookingsIntent) where
  toINGetUserCurrentRestaurantReservationBookingsIntent = unsafeCastId

instance IsINIntent (Id INGetUserCurrentRestaurantReservationBookingsIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INGetUserCurrentRestaurantReservationBookingsIntent) where
  toNSObject = unsafeCastId

-- ---------- INGetVisualCodeIntent ----------

-- | Phantom type for @INGetVisualCodeIntent@.
data INGetVisualCodeIntent

instance IsObjCObject (Id INGetVisualCodeIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetVisualCodeIntent"

class IsINIntent a => IsINGetVisualCodeIntent a where
  toINGetVisualCodeIntent :: a -> Id INGetVisualCodeIntent

instance IsINGetVisualCodeIntent (Id INGetVisualCodeIntent) where
  toINGetVisualCodeIntent = unsafeCastId

instance IsINIntent (Id INGetVisualCodeIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INGetVisualCodeIntent) where
  toNSObject = unsafeCastId

-- ---------- INHangUpCallIntent ----------

-- | Phantom type for @INHangUpCallIntent@.
data INHangUpCallIntent

instance IsObjCObject (Id INHangUpCallIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INHangUpCallIntent"

class IsINIntent a => IsINHangUpCallIntent a where
  toINHangUpCallIntent :: a -> Id INHangUpCallIntent

instance IsINHangUpCallIntent (Id INHangUpCallIntent) where
  toINHangUpCallIntent = unsafeCastId

instance IsINIntent (Id INHangUpCallIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INHangUpCallIntent) where
  toNSObject = unsafeCastId

-- ---------- INListCarsIntent ----------

-- | Phantom type for @INListCarsIntent@.
data INListCarsIntent

instance IsObjCObject (Id INListCarsIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INListCarsIntent"

class IsINIntent a => IsINListCarsIntent a where
  toINListCarsIntent :: a -> Id INListCarsIntent

instance IsINListCarsIntent (Id INListCarsIntent) where
  toINListCarsIntent = unsafeCastId

instance IsINIntent (Id INListCarsIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INListCarsIntent) where
  toNSObject = unsafeCastId

-- ---------- INListRideOptionsIntent ----------

-- | Phantom type for @INListRideOptionsIntent@.
data INListRideOptionsIntent

instance IsObjCObject (Id INListRideOptionsIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INListRideOptionsIntent"

class IsINIntent a => IsINListRideOptionsIntent a where
  toINListRideOptionsIntent :: a -> Id INListRideOptionsIntent

instance IsINListRideOptionsIntent (Id INListRideOptionsIntent) where
  toINListRideOptionsIntent = unsafeCastId

instance IsINIntent (Id INListRideOptionsIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INListRideOptionsIntent) where
  toNSObject = unsafeCastId

-- ---------- INPauseWorkoutIntent ----------

-- | Phantom type for @INPauseWorkoutIntent@.
data INPauseWorkoutIntent

instance IsObjCObject (Id INPauseWorkoutIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPauseWorkoutIntent"

class IsINIntent a => IsINPauseWorkoutIntent a where
  toINPauseWorkoutIntent :: a -> Id INPauseWorkoutIntent

instance IsINPauseWorkoutIntent (Id INPauseWorkoutIntent) where
  toINPauseWorkoutIntent = unsafeCastId

instance IsINIntent (Id INPauseWorkoutIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INPauseWorkoutIntent) where
  toNSObject = unsafeCastId

-- ---------- INPayBillIntent ----------

-- | Phantom type for @INPayBillIntent@.
data INPayBillIntent

instance IsObjCObject (Id INPayBillIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPayBillIntent"

class IsINIntent a => IsINPayBillIntent a where
  toINPayBillIntent :: a -> Id INPayBillIntent

instance IsINPayBillIntent (Id INPayBillIntent) where
  toINPayBillIntent = unsafeCastId

instance IsINIntent (Id INPayBillIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INPayBillIntent) where
  toNSObject = unsafeCastId

-- ---------- INPlayMediaIntent ----------

-- | Phantom type for @INPlayMediaIntent@.
data INPlayMediaIntent

instance IsObjCObject (Id INPlayMediaIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPlayMediaIntent"

class IsINIntent a => IsINPlayMediaIntent a where
  toINPlayMediaIntent :: a -> Id INPlayMediaIntent

instance IsINPlayMediaIntent (Id INPlayMediaIntent) where
  toINPlayMediaIntent = unsafeCastId

instance IsINIntent (Id INPlayMediaIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INPlayMediaIntent) where
  toNSObject = unsafeCastId

-- ---------- INRequestPaymentIntent ----------

-- | Phantom type for @INRequestPaymentIntent@.
data INRequestPaymentIntent

instance IsObjCObject (Id INRequestPaymentIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRequestPaymentIntent"

class IsINIntent a => IsINRequestPaymentIntent a where
  toINRequestPaymentIntent :: a -> Id INRequestPaymentIntent

instance IsINRequestPaymentIntent (Id INRequestPaymentIntent) where
  toINRequestPaymentIntent = unsafeCastId

instance IsINIntent (Id INRequestPaymentIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INRequestPaymentIntent) where
  toNSObject = unsafeCastId

-- ---------- INRequestRideIntent ----------

-- | Phantom type for @INRequestRideIntent@.
data INRequestRideIntent

instance IsObjCObject (Id INRequestRideIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRequestRideIntent"

class IsINIntent a => IsINRequestRideIntent a where
  toINRequestRideIntent :: a -> Id INRequestRideIntent

instance IsINRequestRideIntent (Id INRequestRideIntent) where
  toINRequestRideIntent = unsafeCastId

instance IsINIntent (Id INRequestRideIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INRequestRideIntent) where
  toNSObject = unsafeCastId

-- ---------- INResumeWorkoutIntent ----------

-- | Phantom type for @INResumeWorkoutIntent@.
data INResumeWorkoutIntent

instance IsObjCObject (Id INResumeWorkoutIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INResumeWorkoutIntent"

class IsINIntent a => IsINResumeWorkoutIntent a where
  toINResumeWorkoutIntent :: a -> Id INResumeWorkoutIntent

instance IsINResumeWorkoutIntent (Id INResumeWorkoutIntent) where
  toINResumeWorkoutIntent = unsafeCastId

instance IsINIntent (Id INResumeWorkoutIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INResumeWorkoutIntent) where
  toNSObject = unsafeCastId

-- ---------- INSaveProfileInCarIntent ----------

-- | Phantom type for @INSaveProfileInCarIntent@.
data INSaveProfileInCarIntent

instance IsObjCObject (Id INSaveProfileInCarIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSaveProfileInCarIntent"

class IsINIntent a => IsINSaveProfileInCarIntent a where
  toINSaveProfileInCarIntent :: a -> Id INSaveProfileInCarIntent

instance IsINSaveProfileInCarIntent (Id INSaveProfileInCarIntent) where
  toINSaveProfileInCarIntent = unsafeCastId

instance IsINIntent (Id INSaveProfileInCarIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSaveProfileInCarIntent) where
  toNSObject = unsafeCastId

-- ---------- INSearchCallHistoryIntent ----------

-- | Phantom type for @INSearchCallHistoryIntent@.
data INSearchCallHistoryIntent

instance IsObjCObject (Id INSearchCallHistoryIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchCallHistoryIntent"

class IsINIntent a => IsINSearchCallHistoryIntent a where
  toINSearchCallHistoryIntent :: a -> Id INSearchCallHistoryIntent

instance IsINSearchCallHistoryIntent (Id INSearchCallHistoryIntent) where
  toINSearchCallHistoryIntent = unsafeCastId

instance IsINIntent (Id INSearchCallHistoryIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSearchCallHistoryIntent) where
  toNSObject = unsafeCastId

-- ---------- INSearchForAccountsIntent ----------

-- | Phantom type for @INSearchForAccountsIntent@.
data INSearchForAccountsIntent

instance IsObjCObject (Id INSearchForAccountsIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForAccountsIntent"

class IsINIntent a => IsINSearchForAccountsIntent a where
  toINSearchForAccountsIntent :: a -> Id INSearchForAccountsIntent

instance IsINSearchForAccountsIntent (Id INSearchForAccountsIntent) where
  toINSearchForAccountsIntent = unsafeCastId

instance IsINIntent (Id INSearchForAccountsIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSearchForAccountsIntent) where
  toNSObject = unsafeCastId

-- ---------- INSearchForBillsIntent ----------

-- | Phantom type for @INSearchForBillsIntent@.
data INSearchForBillsIntent

instance IsObjCObject (Id INSearchForBillsIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForBillsIntent"

class IsINIntent a => IsINSearchForBillsIntent a where
  toINSearchForBillsIntent :: a -> Id INSearchForBillsIntent

instance IsINSearchForBillsIntent (Id INSearchForBillsIntent) where
  toINSearchForBillsIntent = unsafeCastId

instance IsINIntent (Id INSearchForBillsIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSearchForBillsIntent) where
  toNSObject = unsafeCastId

-- ---------- INSearchForMediaIntent ----------

-- | Phantom type for @INSearchForMediaIntent@.
data INSearchForMediaIntent

instance IsObjCObject (Id INSearchForMediaIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForMediaIntent"

class IsINIntent a => IsINSearchForMediaIntent a where
  toINSearchForMediaIntent :: a -> Id INSearchForMediaIntent

instance IsINSearchForMediaIntent (Id INSearchForMediaIntent) where
  toINSearchForMediaIntent = unsafeCastId

instance IsINIntent (Id INSearchForMediaIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSearchForMediaIntent) where
  toNSObject = unsafeCastId

-- ---------- INSearchForMessagesIntent ----------

-- | Phantom type for @INSearchForMessagesIntent@.
data INSearchForMessagesIntent

instance IsObjCObject (Id INSearchForMessagesIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForMessagesIntent"

class IsINIntent a => IsINSearchForMessagesIntent a where
  toINSearchForMessagesIntent :: a -> Id INSearchForMessagesIntent

instance IsINSearchForMessagesIntent (Id INSearchForMessagesIntent) where
  toINSearchForMessagesIntent = unsafeCastId

instance IsINIntent (Id INSearchForMessagesIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSearchForMessagesIntent) where
  toNSObject = unsafeCastId

-- ---------- INSearchForNotebookItemsIntent ----------

-- | Phantom type for @INSearchForNotebookItemsIntent@.
data INSearchForNotebookItemsIntent

instance IsObjCObject (Id INSearchForNotebookItemsIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForNotebookItemsIntent"

class IsINIntent a => IsINSearchForNotebookItemsIntent a where
  toINSearchForNotebookItemsIntent :: a -> Id INSearchForNotebookItemsIntent

instance IsINSearchForNotebookItemsIntent (Id INSearchForNotebookItemsIntent) where
  toINSearchForNotebookItemsIntent = unsafeCastId

instance IsINIntent (Id INSearchForNotebookItemsIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSearchForNotebookItemsIntent) where
  toNSObject = unsafeCastId

-- ---------- INSearchForPhotosIntent ----------

-- | Phantom type for @INSearchForPhotosIntent@.
data INSearchForPhotosIntent

instance IsObjCObject (Id INSearchForPhotosIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForPhotosIntent"

class IsINIntent a => IsINSearchForPhotosIntent a where
  toINSearchForPhotosIntent :: a -> Id INSearchForPhotosIntent

instance IsINSearchForPhotosIntent (Id INSearchForPhotosIntent) where
  toINSearchForPhotosIntent = unsafeCastId

instance IsINIntent (Id INSearchForPhotosIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSearchForPhotosIntent) where
  toNSObject = unsafeCastId

-- ---------- INSendMessageIntent ----------

-- | Phantom type for @INSendMessageIntent@.
data INSendMessageIntent

instance IsObjCObject (Id INSendMessageIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSendMessageIntent"

class IsINIntent a => IsINSendMessageIntent a where
  toINSendMessageIntent :: a -> Id INSendMessageIntent

instance IsINSendMessageIntent (Id INSendMessageIntent) where
  toINSendMessageIntent = unsafeCastId

instance IsINIntent (Id INSendMessageIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSendMessageIntent) where
  toNSObject = unsafeCastId

-- ---------- INSendPaymentIntent ----------

-- | Phantom type for @INSendPaymentIntent@.
data INSendPaymentIntent

instance IsObjCObject (Id INSendPaymentIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSendPaymentIntent"

class IsINIntent a => IsINSendPaymentIntent a where
  toINSendPaymentIntent :: a -> Id INSendPaymentIntent

instance IsINSendPaymentIntent (Id INSendPaymentIntent) where
  toINSendPaymentIntent = unsafeCastId

instance IsINIntent (Id INSendPaymentIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSendPaymentIntent) where
  toNSObject = unsafeCastId

-- ---------- INSendRideFeedbackIntent ----------

-- | Phantom type for @INSendRideFeedbackIntent@.
data INSendRideFeedbackIntent

instance IsObjCObject (Id INSendRideFeedbackIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSendRideFeedbackIntent"

class IsINIntent a => IsINSendRideFeedbackIntent a where
  toINSendRideFeedbackIntent :: a -> Id INSendRideFeedbackIntent

instance IsINSendRideFeedbackIntent (Id INSendRideFeedbackIntent) where
  toINSendRideFeedbackIntent = unsafeCastId

instance IsINIntent (Id INSendRideFeedbackIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSendRideFeedbackIntent) where
  toNSObject = unsafeCastId

-- ---------- INSetAudioSourceInCarIntent ----------

-- | Phantom type for @INSetAudioSourceInCarIntent@.
data INSetAudioSourceInCarIntent

instance IsObjCObject (Id INSetAudioSourceInCarIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetAudioSourceInCarIntent"

class IsINIntent a => IsINSetAudioSourceInCarIntent a where
  toINSetAudioSourceInCarIntent :: a -> Id INSetAudioSourceInCarIntent

instance IsINSetAudioSourceInCarIntent (Id INSetAudioSourceInCarIntent) where
  toINSetAudioSourceInCarIntent = unsafeCastId

instance IsINIntent (Id INSetAudioSourceInCarIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSetAudioSourceInCarIntent) where
  toNSObject = unsafeCastId

-- ---------- INSetCarLockStatusIntent ----------

-- | Phantom type for @INSetCarLockStatusIntent@.
data INSetCarLockStatusIntent

instance IsObjCObject (Id INSetCarLockStatusIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetCarLockStatusIntent"

class IsINIntent a => IsINSetCarLockStatusIntent a where
  toINSetCarLockStatusIntent :: a -> Id INSetCarLockStatusIntent

instance IsINSetCarLockStatusIntent (Id INSetCarLockStatusIntent) where
  toINSetCarLockStatusIntent = unsafeCastId

instance IsINIntent (Id INSetCarLockStatusIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSetCarLockStatusIntent) where
  toNSObject = unsafeCastId

-- ---------- INSetClimateSettingsInCarIntent ----------

-- | Phantom type for @INSetClimateSettingsInCarIntent@.
data INSetClimateSettingsInCarIntent

instance IsObjCObject (Id INSetClimateSettingsInCarIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetClimateSettingsInCarIntent"

class IsINIntent a => IsINSetClimateSettingsInCarIntent a where
  toINSetClimateSettingsInCarIntent :: a -> Id INSetClimateSettingsInCarIntent

instance IsINSetClimateSettingsInCarIntent (Id INSetClimateSettingsInCarIntent) where
  toINSetClimateSettingsInCarIntent = unsafeCastId

instance IsINIntent (Id INSetClimateSettingsInCarIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSetClimateSettingsInCarIntent) where
  toNSObject = unsafeCastId

-- ---------- INSetDefrosterSettingsInCarIntent ----------

-- | Phantom type for @INSetDefrosterSettingsInCarIntent@.
data INSetDefrosterSettingsInCarIntent

instance IsObjCObject (Id INSetDefrosterSettingsInCarIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetDefrosterSettingsInCarIntent"

class IsINIntent a => IsINSetDefrosterSettingsInCarIntent a where
  toINSetDefrosterSettingsInCarIntent :: a -> Id INSetDefrosterSettingsInCarIntent

instance IsINSetDefrosterSettingsInCarIntent (Id INSetDefrosterSettingsInCarIntent) where
  toINSetDefrosterSettingsInCarIntent = unsafeCastId

instance IsINIntent (Id INSetDefrosterSettingsInCarIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSetDefrosterSettingsInCarIntent) where
  toNSObject = unsafeCastId

-- ---------- INSetMessageAttributeIntent ----------

-- | Phantom type for @INSetMessageAttributeIntent@.
data INSetMessageAttributeIntent

instance IsObjCObject (Id INSetMessageAttributeIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetMessageAttributeIntent"

class IsINIntent a => IsINSetMessageAttributeIntent a where
  toINSetMessageAttributeIntent :: a -> Id INSetMessageAttributeIntent

instance IsINSetMessageAttributeIntent (Id INSetMessageAttributeIntent) where
  toINSetMessageAttributeIntent = unsafeCastId

instance IsINIntent (Id INSetMessageAttributeIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSetMessageAttributeIntent) where
  toNSObject = unsafeCastId

-- ---------- INSetProfileInCarIntent ----------

-- | Phantom type for @INSetProfileInCarIntent@.
data INSetProfileInCarIntent

instance IsObjCObject (Id INSetProfileInCarIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetProfileInCarIntent"

class IsINIntent a => IsINSetProfileInCarIntent a where
  toINSetProfileInCarIntent :: a -> Id INSetProfileInCarIntent

instance IsINSetProfileInCarIntent (Id INSetProfileInCarIntent) where
  toINSetProfileInCarIntent = unsafeCastId

instance IsINIntent (Id INSetProfileInCarIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSetProfileInCarIntent) where
  toNSObject = unsafeCastId

-- ---------- INSetRadioStationIntent ----------

-- | Phantom type for @INSetRadioStationIntent@.
data INSetRadioStationIntent

instance IsObjCObject (Id INSetRadioStationIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetRadioStationIntent"

class IsINIntent a => IsINSetRadioStationIntent a where
  toINSetRadioStationIntent :: a -> Id INSetRadioStationIntent

instance IsINSetRadioStationIntent (Id INSetRadioStationIntent) where
  toINSetRadioStationIntent = unsafeCastId

instance IsINIntent (Id INSetRadioStationIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSetRadioStationIntent) where
  toNSObject = unsafeCastId

-- ---------- INSetSeatSettingsInCarIntent ----------

-- | Phantom type for @INSetSeatSettingsInCarIntent@.
data INSetSeatSettingsInCarIntent

instance IsObjCObject (Id INSetSeatSettingsInCarIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetSeatSettingsInCarIntent"

class IsINIntent a => IsINSetSeatSettingsInCarIntent a where
  toINSetSeatSettingsInCarIntent :: a -> Id INSetSeatSettingsInCarIntent

instance IsINSetSeatSettingsInCarIntent (Id INSetSeatSettingsInCarIntent) where
  toINSetSeatSettingsInCarIntent = unsafeCastId

instance IsINIntent (Id INSetSeatSettingsInCarIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSetSeatSettingsInCarIntent) where
  toNSObject = unsafeCastId

-- ---------- INSetTaskAttributeIntent ----------

-- | Phantom type for @INSetTaskAttributeIntent@.
data INSetTaskAttributeIntent

instance IsObjCObject (Id INSetTaskAttributeIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetTaskAttributeIntent"

class IsINIntent a => IsINSetTaskAttributeIntent a where
  toINSetTaskAttributeIntent :: a -> Id INSetTaskAttributeIntent

instance IsINSetTaskAttributeIntent (Id INSetTaskAttributeIntent) where
  toINSetTaskAttributeIntent = unsafeCastId

instance IsINIntent (Id INSetTaskAttributeIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSetTaskAttributeIntent) where
  toNSObject = unsafeCastId

-- ---------- INShareFocusStatusIntent ----------

-- | Phantom type for @INShareFocusStatusIntent@.
data INShareFocusStatusIntent

instance IsObjCObject (Id INShareFocusStatusIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INShareFocusStatusIntent"

class IsINIntent a => IsINShareFocusStatusIntent a where
  toINShareFocusStatusIntent :: a -> Id INShareFocusStatusIntent

instance IsINShareFocusStatusIntent (Id INShareFocusStatusIntent) where
  toINShareFocusStatusIntent = unsafeCastId

instance IsINIntent (Id INShareFocusStatusIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INShareFocusStatusIntent) where
  toNSObject = unsafeCastId

-- ---------- INSnoozeTasksIntent ----------

-- | Phantom type for @INSnoozeTasksIntent@.
data INSnoozeTasksIntent

instance IsObjCObject (Id INSnoozeTasksIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSnoozeTasksIntent"

class IsINIntent a => IsINSnoozeTasksIntent a where
  toINSnoozeTasksIntent :: a -> Id INSnoozeTasksIntent

instance IsINSnoozeTasksIntent (Id INSnoozeTasksIntent) where
  toINSnoozeTasksIntent = unsafeCastId

instance IsINIntent (Id INSnoozeTasksIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INSnoozeTasksIntent) where
  toNSObject = unsafeCastId

-- ---------- INStartAudioCallIntent ----------

-- | Phantom type for @INStartAudioCallIntent@.
data INStartAudioCallIntent

instance IsObjCObject (Id INStartAudioCallIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartAudioCallIntent"

class IsINIntent a => IsINStartAudioCallIntent a where
  toINStartAudioCallIntent :: a -> Id INStartAudioCallIntent

instance IsINStartAudioCallIntent (Id INStartAudioCallIntent) where
  toINStartAudioCallIntent = unsafeCastId

instance IsINIntent (Id INStartAudioCallIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INStartAudioCallIntent) where
  toNSObject = unsafeCastId

-- ---------- INStartCallIntent ----------

-- | Phantom type for @INStartCallIntent@.
data INStartCallIntent

instance IsObjCObject (Id INStartCallIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartCallIntent"

class IsINIntent a => IsINStartCallIntent a where
  toINStartCallIntent :: a -> Id INStartCallIntent

instance IsINStartCallIntent (Id INStartCallIntent) where
  toINStartCallIntent = unsafeCastId

instance IsINIntent (Id INStartCallIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INStartCallIntent) where
  toNSObject = unsafeCastId

-- ---------- INStartPhotoPlaybackIntent ----------

-- | Phantom type for @INStartPhotoPlaybackIntent@.
data INStartPhotoPlaybackIntent

instance IsObjCObject (Id INStartPhotoPlaybackIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartPhotoPlaybackIntent"

class IsINIntent a => IsINStartPhotoPlaybackIntent a where
  toINStartPhotoPlaybackIntent :: a -> Id INStartPhotoPlaybackIntent

instance IsINStartPhotoPlaybackIntent (Id INStartPhotoPlaybackIntent) where
  toINStartPhotoPlaybackIntent = unsafeCastId

instance IsINIntent (Id INStartPhotoPlaybackIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INStartPhotoPlaybackIntent) where
  toNSObject = unsafeCastId

-- ---------- INStartVideoCallIntent ----------

-- | Phantom type for @INStartVideoCallIntent@.
data INStartVideoCallIntent

instance IsObjCObject (Id INStartVideoCallIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartVideoCallIntent"

class IsINIntent a => IsINStartVideoCallIntent a where
  toINStartVideoCallIntent :: a -> Id INStartVideoCallIntent

instance IsINStartVideoCallIntent (Id INStartVideoCallIntent) where
  toINStartVideoCallIntent = unsafeCastId

instance IsINIntent (Id INStartVideoCallIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INStartVideoCallIntent) where
  toNSObject = unsafeCastId

-- ---------- INStartWorkoutIntent ----------

-- | Phantom type for @INStartWorkoutIntent@.
data INStartWorkoutIntent

instance IsObjCObject (Id INStartWorkoutIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartWorkoutIntent"

class IsINIntent a => IsINStartWorkoutIntent a where
  toINStartWorkoutIntent :: a -> Id INStartWorkoutIntent

instance IsINStartWorkoutIntent (Id INStartWorkoutIntent) where
  toINStartWorkoutIntent = unsafeCastId

instance IsINIntent (Id INStartWorkoutIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INStartWorkoutIntent) where
  toNSObject = unsafeCastId

-- ---------- INTransferMoneyIntent ----------

-- | Phantom type for @INTransferMoneyIntent@.
data INTransferMoneyIntent

instance IsObjCObject (Id INTransferMoneyIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTransferMoneyIntent"

class IsINIntent a => IsINTransferMoneyIntent a where
  toINTransferMoneyIntent :: a -> Id INTransferMoneyIntent

instance IsINTransferMoneyIntent (Id INTransferMoneyIntent) where
  toINTransferMoneyIntent = unsafeCastId

instance IsINIntent (Id INTransferMoneyIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INTransferMoneyIntent) where
  toNSObject = unsafeCastId

-- ---------- INUnsendMessagesIntent ----------

-- | Phantom type for @INUnsendMessagesIntent@.
data INUnsendMessagesIntent

instance IsObjCObject (Id INUnsendMessagesIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INUnsendMessagesIntent"

class IsINIntent a => IsINUnsendMessagesIntent a where
  toINUnsendMessagesIntent :: a -> Id INUnsendMessagesIntent

instance IsINUnsendMessagesIntent (Id INUnsendMessagesIntent) where
  toINUnsendMessagesIntent = unsafeCastId

instance IsINIntent (Id INUnsendMessagesIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INUnsendMessagesIntent) where
  toNSObject = unsafeCastId

-- ---------- INUpdateMediaAffinityIntent ----------

-- | Phantom type for @INUpdateMediaAffinityIntent@.
data INUpdateMediaAffinityIntent

instance IsObjCObject (Id INUpdateMediaAffinityIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INUpdateMediaAffinityIntent"

class IsINIntent a => IsINUpdateMediaAffinityIntent a where
  toINUpdateMediaAffinityIntent :: a -> Id INUpdateMediaAffinityIntent

instance IsINUpdateMediaAffinityIntent (Id INUpdateMediaAffinityIntent) where
  toINUpdateMediaAffinityIntent = unsafeCastId

instance IsINIntent (Id INUpdateMediaAffinityIntent) where
  toINIntent = unsafeCastId

instance IsNSObject (Id INUpdateMediaAffinityIntent) where
  toNSObject = unsafeCastId

-- ---------- INSendMessageIntentDonationMetadata ----------

-- | Phantom type for @INSendMessageIntentDonationMetadata@.
data INSendMessageIntentDonationMetadata

instance IsObjCObject (Id INSendMessageIntentDonationMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSendMessageIntentDonationMetadata"

class IsINIntentDonationMetadata a => IsINSendMessageIntentDonationMetadata a where
  toINSendMessageIntentDonationMetadata :: a -> Id INSendMessageIntentDonationMetadata

instance IsINSendMessageIntentDonationMetadata (Id INSendMessageIntentDonationMetadata) where
  toINSendMessageIntentDonationMetadata = unsafeCastId

instance IsINIntentDonationMetadata (Id INSendMessageIntentDonationMetadata) where
  toINIntentDonationMetadata = unsafeCastId

instance IsNSObject (Id INSendMessageIntentDonationMetadata) where
  toNSObject = unsafeCastId

-- ---------- INAccountTypeResolutionResult ----------

-- | Phantom type for @INAccountTypeResolutionResult@.
data INAccountTypeResolutionResult

instance IsObjCObject (Id INAccountTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAccountTypeResolutionResult"

class IsINIntentResolutionResult a => IsINAccountTypeResolutionResult a where
  toINAccountTypeResolutionResult :: a -> Id INAccountTypeResolutionResult

instance IsINAccountTypeResolutionResult (Id INAccountTypeResolutionResult) where
  toINAccountTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INAccountTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INAccountTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INBalanceTypeResolutionResult ----------

-- | Phantom type for @INBalanceTypeResolutionResult@.
data INBalanceTypeResolutionResult

instance IsObjCObject (Id INBalanceTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBalanceTypeResolutionResult"

class IsINIntentResolutionResult a => IsINBalanceTypeResolutionResult a where
  toINBalanceTypeResolutionResult :: a -> Id INBalanceTypeResolutionResult

instance IsINBalanceTypeResolutionResult (Id INBalanceTypeResolutionResult) where
  toINBalanceTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INBalanceTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INBalanceTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INBillPayeeResolutionResult ----------

-- | Phantom type for @INBillPayeeResolutionResult@.
data INBillPayeeResolutionResult

instance IsObjCObject (Id INBillPayeeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBillPayeeResolutionResult"

class IsINIntentResolutionResult a => IsINBillPayeeResolutionResult a where
  toINBillPayeeResolutionResult :: a -> Id INBillPayeeResolutionResult

instance IsINBillPayeeResolutionResult (Id INBillPayeeResolutionResult) where
  toINBillPayeeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INBillPayeeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INBillPayeeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INBillTypeResolutionResult ----------

-- | Phantom type for @INBillTypeResolutionResult@.
data INBillTypeResolutionResult

instance IsObjCObject (Id INBillTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBillTypeResolutionResult"

class IsINIntentResolutionResult a => IsINBillTypeResolutionResult a where
  toINBillTypeResolutionResult :: a -> Id INBillTypeResolutionResult

instance IsINBillTypeResolutionResult (Id INBillTypeResolutionResult) where
  toINBillTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INBillTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INBillTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INBooleanResolutionResult ----------

-- | Phantom type for @INBooleanResolutionResult@.
data INBooleanResolutionResult

instance IsObjCObject (Id INBooleanResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBooleanResolutionResult"

class IsINIntentResolutionResult a => IsINBooleanResolutionResult a where
  toINBooleanResolutionResult :: a -> Id INBooleanResolutionResult

instance IsINBooleanResolutionResult (Id INBooleanResolutionResult) where
  toINBooleanResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INBooleanResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INBooleanResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INCallCapabilityResolutionResult ----------

-- | Phantom type for @INCallCapabilityResolutionResult@.
data INCallCapabilityResolutionResult

instance IsObjCObject (Id INCallCapabilityResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCallCapabilityResolutionResult"

class IsINIntentResolutionResult a => IsINCallCapabilityResolutionResult a where
  toINCallCapabilityResolutionResult :: a -> Id INCallCapabilityResolutionResult

instance IsINCallCapabilityResolutionResult (Id INCallCapabilityResolutionResult) where
  toINCallCapabilityResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INCallCapabilityResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INCallCapabilityResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INCallDestinationTypeResolutionResult ----------

-- | Phantom type for @INCallDestinationTypeResolutionResult@.
data INCallDestinationTypeResolutionResult

instance IsObjCObject (Id INCallDestinationTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCallDestinationTypeResolutionResult"

class IsINIntentResolutionResult a => IsINCallDestinationTypeResolutionResult a where
  toINCallDestinationTypeResolutionResult :: a -> Id INCallDestinationTypeResolutionResult

instance IsINCallDestinationTypeResolutionResult (Id INCallDestinationTypeResolutionResult) where
  toINCallDestinationTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INCallDestinationTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INCallDestinationTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INCallRecordResolutionResult ----------

-- | Phantom type for @INCallRecordResolutionResult@.
data INCallRecordResolutionResult

instance IsObjCObject (Id INCallRecordResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCallRecordResolutionResult"

class IsINIntentResolutionResult a => IsINCallRecordResolutionResult a where
  toINCallRecordResolutionResult :: a -> Id INCallRecordResolutionResult

instance IsINCallRecordResolutionResult (Id INCallRecordResolutionResult) where
  toINCallRecordResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INCallRecordResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INCallRecordResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INCallRecordTypeOptionsResolutionResult ----------

-- | Phantom type for @INCallRecordTypeOptionsResolutionResult@.
data INCallRecordTypeOptionsResolutionResult

instance IsObjCObject (Id INCallRecordTypeOptionsResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCallRecordTypeOptionsResolutionResult"

class IsINIntentResolutionResult a => IsINCallRecordTypeOptionsResolutionResult a where
  toINCallRecordTypeOptionsResolutionResult :: a -> Id INCallRecordTypeOptionsResolutionResult

instance IsINCallRecordTypeOptionsResolutionResult (Id INCallRecordTypeOptionsResolutionResult) where
  toINCallRecordTypeOptionsResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INCallRecordTypeOptionsResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INCallRecordTypeOptionsResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INCallRecordTypeResolutionResult ----------

-- | Phantom type for @INCallRecordTypeResolutionResult@.
data INCallRecordTypeResolutionResult

instance IsObjCObject (Id INCallRecordTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCallRecordTypeResolutionResult"

class IsINIntentResolutionResult a => IsINCallRecordTypeResolutionResult a where
  toINCallRecordTypeResolutionResult :: a -> Id INCallRecordTypeResolutionResult

instance IsINCallRecordTypeResolutionResult (Id INCallRecordTypeResolutionResult) where
  toINCallRecordTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INCallRecordTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INCallRecordTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INCarAirCirculationModeResolutionResult ----------

-- | Phantom type for @INCarAirCirculationModeResolutionResult@.
data INCarAirCirculationModeResolutionResult

instance IsObjCObject (Id INCarAirCirculationModeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCarAirCirculationModeResolutionResult"

class IsINIntentResolutionResult a => IsINCarAirCirculationModeResolutionResult a where
  toINCarAirCirculationModeResolutionResult :: a -> Id INCarAirCirculationModeResolutionResult

instance IsINCarAirCirculationModeResolutionResult (Id INCarAirCirculationModeResolutionResult) where
  toINCarAirCirculationModeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INCarAirCirculationModeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INCarAirCirculationModeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INCarAudioSourceResolutionResult ----------

-- | Phantom type for @INCarAudioSourceResolutionResult@.
data INCarAudioSourceResolutionResult

instance IsObjCObject (Id INCarAudioSourceResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCarAudioSourceResolutionResult"

class IsINIntentResolutionResult a => IsINCarAudioSourceResolutionResult a where
  toINCarAudioSourceResolutionResult :: a -> Id INCarAudioSourceResolutionResult

instance IsINCarAudioSourceResolutionResult (Id INCarAudioSourceResolutionResult) where
  toINCarAudioSourceResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INCarAudioSourceResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INCarAudioSourceResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INCarDefrosterResolutionResult ----------

-- | Phantom type for @INCarDefrosterResolutionResult@.
data INCarDefrosterResolutionResult

instance IsObjCObject (Id INCarDefrosterResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCarDefrosterResolutionResult"

class IsINIntentResolutionResult a => IsINCarDefrosterResolutionResult a where
  toINCarDefrosterResolutionResult :: a -> Id INCarDefrosterResolutionResult

instance IsINCarDefrosterResolutionResult (Id INCarDefrosterResolutionResult) where
  toINCarDefrosterResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INCarDefrosterResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INCarDefrosterResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INCarSeatResolutionResult ----------

-- | Phantom type for @INCarSeatResolutionResult@.
data INCarSeatResolutionResult

instance IsObjCObject (Id INCarSeatResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCarSeatResolutionResult"

class IsINIntentResolutionResult a => IsINCarSeatResolutionResult a where
  toINCarSeatResolutionResult :: a -> Id INCarSeatResolutionResult

instance IsINCarSeatResolutionResult (Id INCarSeatResolutionResult) where
  toINCarSeatResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INCarSeatResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INCarSeatResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INCarSignalOptionsResolutionResult ----------

-- | Phantom type for @INCarSignalOptionsResolutionResult@.
data INCarSignalOptionsResolutionResult

instance IsObjCObject (Id INCarSignalOptionsResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCarSignalOptionsResolutionResult"

class IsINIntentResolutionResult a => IsINCarSignalOptionsResolutionResult a where
  toINCarSignalOptionsResolutionResult :: a -> Id INCarSignalOptionsResolutionResult

instance IsINCarSignalOptionsResolutionResult (Id INCarSignalOptionsResolutionResult) where
  toINCarSignalOptionsResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INCarSignalOptionsResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INCarSignalOptionsResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INCurrencyAmountResolutionResult ----------

-- | Phantom type for @INCurrencyAmountResolutionResult@.
data INCurrencyAmountResolutionResult

instance IsObjCObject (Id INCurrencyAmountResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCurrencyAmountResolutionResult"

class IsINIntentResolutionResult a => IsINCurrencyAmountResolutionResult a where
  toINCurrencyAmountResolutionResult :: a -> Id INCurrencyAmountResolutionResult

instance IsINCurrencyAmountResolutionResult (Id INCurrencyAmountResolutionResult) where
  toINCurrencyAmountResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INCurrencyAmountResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INCurrencyAmountResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INDateComponentsRangeResolutionResult ----------

-- | Phantom type for @INDateComponentsRangeResolutionResult@.
data INDateComponentsRangeResolutionResult

instance IsObjCObject (Id INDateComponentsRangeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INDateComponentsRangeResolutionResult"

class IsINIntentResolutionResult a => IsINDateComponentsRangeResolutionResult a where
  toINDateComponentsRangeResolutionResult :: a -> Id INDateComponentsRangeResolutionResult

instance IsINDateComponentsRangeResolutionResult (Id INDateComponentsRangeResolutionResult) where
  toINDateComponentsRangeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INDateComponentsRangeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INDateComponentsRangeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INDateComponentsResolutionResult ----------

-- | Phantom type for @INDateComponentsResolutionResult@.
data INDateComponentsResolutionResult

instance IsObjCObject (Id INDateComponentsResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INDateComponentsResolutionResult"

class IsINIntentResolutionResult a => IsINDateComponentsResolutionResult a where
  toINDateComponentsResolutionResult :: a -> Id INDateComponentsResolutionResult

instance IsINDateComponentsResolutionResult (Id INDateComponentsResolutionResult) where
  toINDateComponentsResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INDateComponentsResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INDateComponentsResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INDateSearchTypeResolutionResult ----------

-- | Phantom type for @INDateSearchTypeResolutionResult@.
data INDateSearchTypeResolutionResult

instance IsObjCObject (Id INDateSearchTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INDateSearchTypeResolutionResult"

class IsINIntentResolutionResult a => IsINDateSearchTypeResolutionResult a where
  toINDateSearchTypeResolutionResult :: a -> Id INDateSearchTypeResolutionResult

instance IsINDateSearchTypeResolutionResult (Id INDateSearchTypeResolutionResult) where
  toINDateSearchTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INDateSearchTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INDateSearchTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INDoubleResolutionResult ----------

-- | Phantom type for @INDoubleResolutionResult@.
data INDoubleResolutionResult

instance IsObjCObject (Id INDoubleResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INDoubleResolutionResult"

class IsINIntentResolutionResult a => IsINDoubleResolutionResult a where
  toINDoubleResolutionResult :: a -> Id INDoubleResolutionResult

instance IsINDoubleResolutionResult (Id INDoubleResolutionResult) where
  toINDoubleResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INDoubleResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INDoubleResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INEnergyResolutionResult ----------

-- | Phantom type for @INEnergyResolutionResult@.
data INEnergyResolutionResult

instance IsObjCObject (Id INEnergyResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INEnergyResolutionResult"

class IsINIntentResolutionResult a => IsINEnergyResolutionResult a where
  toINEnergyResolutionResult :: a -> Id INEnergyResolutionResult

instance IsINEnergyResolutionResult (Id INEnergyResolutionResult) where
  toINEnergyResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INEnergyResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INEnergyResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INEnumResolutionResult ----------

-- | Phantom type for @INEnumResolutionResult@.
data INEnumResolutionResult

instance IsObjCObject (Id INEnumResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INEnumResolutionResult"

class IsINIntentResolutionResult a => IsINEnumResolutionResult a where
  toINEnumResolutionResult :: a -> Id INEnumResolutionResult

instance IsINEnumResolutionResult (Id INEnumResolutionResult) where
  toINEnumResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INEnumResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INEnumResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INFileResolutionResult ----------

-- | Phantom type for @INFileResolutionResult@.
data INFileResolutionResult

instance IsObjCObject (Id INFileResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INFileResolutionResult"

class IsINIntentResolutionResult a => IsINFileResolutionResult a where
  toINFileResolutionResult :: a -> Id INFileResolutionResult

instance IsINFileResolutionResult (Id INFileResolutionResult) where
  toINFileResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INFileResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INFileResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INIntegerResolutionResult ----------

-- | Phantom type for @INIntegerResolutionResult@.
data INIntegerResolutionResult

instance IsObjCObject (Id INIntegerResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INIntegerResolutionResult"

class IsINIntentResolutionResult a => IsINIntegerResolutionResult a where
  toINIntegerResolutionResult :: a -> Id INIntegerResolutionResult

instance IsINIntegerResolutionResult (Id INIntegerResolutionResult) where
  toINIntegerResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INIntegerResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INIntegerResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INLengthResolutionResult ----------

-- | Phantom type for @INLengthResolutionResult@.
data INLengthResolutionResult

instance IsObjCObject (Id INLengthResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INLengthResolutionResult"

class IsINIntentResolutionResult a => IsINLengthResolutionResult a where
  toINLengthResolutionResult :: a -> Id INLengthResolutionResult

instance IsINLengthResolutionResult (Id INLengthResolutionResult) where
  toINLengthResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INLengthResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INLengthResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INLocationSearchTypeResolutionResult ----------

-- | Phantom type for @INLocationSearchTypeResolutionResult@.
data INLocationSearchTypeResolutionResult

instance IsObjCObject (Id INLocationSearchTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INLocationSearchTypeResolutionResult"

class IsINIntentResolutionResult a => IsINLocationSearchTypeResolutionResult a where
  toINLocationSearchTypeResolutionResult :: a -> Id INLocationSearchTypeResolutionResult

instance IsINLocationSearchTypeResolutionResult (Id INLocationSearchTypeResolutionResult) where
  toINLocationSearchTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INLocationSearchTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INLocationSearchTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INMassResolutionResult ----------

-- | Phantom type for @INMassResolutionResult@.
data INMassResolutionResult

instance IsObjCObject (Id INMassResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMassResolutionResult"

class IsINIntentResolutionResult a => IsINMassResolutionResult a where
  toINMassResolutionResult :: a -> Id INMassResolutionResult

instance IsINMassResolutionResult (Id INMassResolutionResult) where
  toINMassResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INMassResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INMassResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INMediaAffinityTypeResolutionResult ----------

-- | Phantom type for @INMediaAffinityTypeResolutionResult@.
data INMediaAffinityTypeResolutionResult

instance IsObjCObject (Id INMediaAffinityTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMediaAffinityTypeResolutionResult"

class IsINIntentResolutionResult a => IsINMediaAffinityTypeResolutionResult a where
  toINMediaAffinityTypeResolutionResult :: a -> Id INMediaAffinityTypeResolutionResult

instance IsINMediaAffinityTypeResolutionResult (Id INMediaAffinityTypeResolutionResult) where
  toINMediaAffinityTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INMediaAffinityTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INMediaAffinityTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INMediaDestinationResolutionResult ----------

-- | Phantom type for @INMediaDestinationResolutionResult@.
data INMediaDestinationResolutionResult

instance IsObjCObject (Id INMediaDestinationResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMediaDestinationResolutionResult"

class IsINIntentResolutionResult a => IsINMediaDestinationResolutionResult a where
  toINMediaDestinationResolutionResult :: a -> Id INMediaDestinationResolutionResult

instance IsINMediaDestinationResolutionResult (Id INMediaDestinationResolutionResult) where
  toINMediaDestinationResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INMediaDestinationResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INMediaDestinationResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INMediaItemResolutionResult ----------

-- | Phantom type for @INMediaItemResolutionResult@.
data INMediaItemResolutionResult

instance IsObjCObject (Id INMediaItemResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMediaItemResolutionResult"

class IsINIntentResolutionResult a => IsINMediaItemResolutionResult a where
  toINMediaItemResolutionResult :: a -> Id INMediaItemResolutionResult

instance IsINMediaItemResolutionResult (Id INMediaItemResolutionResult) where
  toINMediaItemResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INMediaItemResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INMediaItemResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INMessageAttributeOptionsResolutionResult ----------

-- | Phantom type for @INMessageAttributeOptionsResolutionResult@.
data INMessageAttributeOptionsResolutionResult

instance IsObjCObject (Id INMessageAttributeOptionsResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMessageAttributeOptionsResolutionResult"

class IsINIntentResolutionResult a => IsINMessageAttributeOptionsResolutionResult a where
  toINMessageAttributeOptionsResolutionResult :: a -> Id INMessageAttributeOptionsResolutionResult

instance IsINMessageAttributeOptionsResolutionResult (Id INMessageAttributeOptionsResolutionResult) where
  toINMessageAttributeOptionsResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INMessageAttributeOptionsResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INMessageAttributeOptionsResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INMessageAttributeResolutionResult ----------

-- | Phantom type for @INMessageAttributeResolutionResult@.
data INMessageAttributeResolutionResult

instance IsObjCObject (Id INMessageAttributeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMessageAttributeResolutionResult"

class IsINIntentResolutionResult a => IsINMessageAttributeResolutionResult a where
  toINMessageAttributeResolutionResult :: a -> Id INMessageAttributeResolutionResult

instance IsINMessageAttributeResolutionResult (Id INMessageAttributeResolutionResult) where
  toINMessageAttributeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INMessageAttributeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INMessageAttributeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INNoteContentResolutionResult ----------

-- | Phantom type for @INNoteContentResolutionResult@.
data INNoteContentResolutionResult

instance IsObjCObject (Id INNoteContentResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INNoteContentResolutionResult"

class IsINIntentResolutionResult a => IsINNoteContentResolutionResult a where
  toINNoteContentResolutionResult :: a -> Id INNoteContentResolutionResult

instance IsINNoteContentResolutionResult (Id INNoteContentResolutionResult) where
  toINNoteContentResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INNoteContentResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INNoteContentResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INNoteContentTypeResolutionResult ----------

-- | Phantom type for @INNoteContentTypeResolutionResult@.
data INNoteContentTypeResolutionResult

instance IsObjCObject (Id INNoteContentTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INNoteContentTypeResolutionResult"

class IsINIntentResolutionResult a => IsINNoteContentTypeResolutionResult a where
  toINNoteContentTypeResolutionResult :: a -> Id INNoteContentTypeResolutionResult

instance IsINNoteContentTypeResolutionResult (Id INNoteContentTypeResolutionResult) where
  toINNoteContentTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INNoteContentTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INNoteContentTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INNoteResolutionResult ----------

-- | Phantom type for @INNoteResolutionResult@.
data INNoteResolutionResult

instance IsObjCObject (Id INNoteResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INNoteResolutionResult"

class IsINIntentResolutionResult a => IsINNoteResolutionResult a where
  toINNoteResolutionResult :: a -> Id INNoteResolutionResult

instance IsINNoteResolutionResult (Id INNoteResolutionResult) where
  toINNoteResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INNoteResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INNoteResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INNotebookItemTypeResolutionResult ----------

-- | Phantom type for @INNotebookItemTypeResolutionResult@.
data INNotebookItemTypeResolutionResult

instance IsObjCObject (Id INNotebookItemTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INNotebookItemTypeResolutionResult"

class IsINIntentResolutionResult a => IsINNotebookItemTypeResolutionResult a where
  toINNotebookItemTypeResolutionResult :: a -> Id INNotebookItemTypeResolutionResult

instance IsINNotebookItemTypeResolutionResult (Id INNotebookItemTypeResolutionResult) where
  toINNotebookItemTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INNotebookItemTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INNotebookItemTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INObjectResolutionResult ----------

-- | Phantom type for @INObjectResolutionResult@.
data INObjectResolutionResult

instance IsObjCObject (Id INObjectResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INObjectResolutionResult"

class IsINIntentResolutionResult a => IsINObjectResolutionResult a where
  toINObjectResolutionResult :: a -> Id INObjectResolutionResult

instance IsINObjectResolutionResult (Id INObjectResolutionResult) where
  toINObjectResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INObjectResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INObjectResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INOutgoingMessageTypeResolutionResult ----------

-- | Phantom type for @INOutgoingMessageTypeResolutionResult@.
data INOutgoingMessageTypeResolutionResult

instance IsObjCObject (Id INOutgoingMessageTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INOutgoingMessageTypeResolutionResult"

class IsINIntentResolutionResult a => IsINOutgoingMessageTypeResolutionResult a where
  toINOutgoingMessageTypeResolutionResult :: a -> Id INOutgoingMessageTypeResolutionResult

instance IsINOutgoingMessageTypeResolutionResult (Id INOutgoingMessageTypeResolutionResult) where
  toINOutgoingMessageTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INOutgoingMessageTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INOutgoingMessageTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INPaymentAccountResolutionResult ----------

-- | Phantom type for @INPaymentAccountResolutionResult@.
data INPaymentAccountResolutionResult

instance IsObjCObject (Id INPaymentAccountResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPaymentAccountResolutionResult"

class IsINIntentResolutionResult a => IsINPaymentAccountResolutionResult a where
  toINPaymentAccountResolutionResult :: a -> Id INPaymentAccountResolutionResult

instance IsINPaymentAccountResolutionResult (Id INPaymentAccountResolutionResult) where
  toINPaymentAccountResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INPaymentAccountResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INPaymentAccountResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INPaymentAmountResolutionResult ----------

-- | Phantom type for @INPaymentAmountResolutionResult@.
data INPaymentAmountResolutionResult

instance IsObjCObject (Id INPaymentAmountResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPaymentAmountResolutionResult"

class IsINIntentResolutionResult a => IsINPaymentAmountResolutionResult a where
  toINPaymentAmountResolutionResult :: a -> Id INPaymentAmountResolutionResult

instance IsINPaymentAmountResolutionResult (Id INPaymentAmountResolutionResult) where
  toINPaymentAmountResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INPaymentAmountResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INPaymentAmountResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INPaymentMethodResolutionResult ----------

-- | Phantom type for @INPaymentMethodResolutionResult@.
data INPaymentMethodResolutionResult

instance IsObjCObject (Id INPaymentMethodResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPaymentMethodResolutionResult"

class IsINIntentResolutionResult a => IsINPaymentMethodResolutionResult a where
  toINPaymentMethodResolutionResult :: a -> Id INPaymentMethodResolutionResult

instance IsINPaymentMethodResolutionResult (Id INPaymentMethodResolutionResult) where
  toINPaymentMethodResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INPaymentMethodResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INPaymentMethodResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INPaymentStatusResolutionResult ----------

-- | Phantom type for @INPaymentStatusResolutionResult@.
data INPaymentStatusResolutionResult

instance IsObjCObject (Id INPaymentStatusResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPaymentStatusResolutionResult"

class IsINIntentResolutionResult a => IsINPaymentStatusResolutionResult a where
  toINPaymentStatusResolutionResult :: a -> Id INPaymentStatusResolutionResult

instance IsINPaymentStatusResolutionResult (Id INPaymentStatusResolutionResult) where
  toINPaymentStatusResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INPaymentStatusResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INPaymentStatusResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INPersonResolutionResult ----------

-- | Phantom type for @INPersonResolutionResult@.
data INPersonResolutionResult

instance IsObjCObject (Id INPersonResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPersonResolutionResult"

class IsINIntentResolutionResult a => IsINPersonResolutionResult a where
  toINPersonResolutionResult :: a -> Id INPersonResolutionResult

instance IsINPersonResolutionResult (Id INPersonResolutionResult) where
  toINPersonResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INPersonResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INPersonResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INPlacemarkResolutionResult ----------

-- | Phantom type for @INPlacemarkResolutionResult@.
data INPlacemarkResolutionResult

instance IsObjCObject (Id INPlacemarkResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPlacemarkResolutionResult"

class IsINIntentResolutionResult a => IsINPlacemarkResolutionResult a where
  toINPlacemarkResolutionResult :: a -> Id INPlacemarkResolutionResult

instance IsINPlacemarkResolutionResult (Id INPlacemarkResolutionResult) where
  toINPlacemarkResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INPlacemarkResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INPlacemarkResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INPlaybackQueueLocationResolutionResult ----------

-- | Phantom type for @INPlaybackQueueLocationResolutionResult@.
data INPlaybackQueueLocationResolutionResult

instance IsObjCObject (Id INPlaybackQueueLocationResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPlaybackQueueLocationResolutionResult"

class IsINIntentResolutionResult a => IsINPlaybackQueueLocationResolutionResult a where
  toINPlaybackQueueLocationResolutionResult :: a -> Id INPlaybackQueueLocationResolutionResult

instance IsINPlaybackQueueLocationResolutionResult (Id INPlaybackQueueLocationResolutionResult) where
  toINPlaybackQueueLocationResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INPlaybackQueueLocationResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INPlaybackQueueLocationResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INPlaybackRepeatModeResolutionResult ----------

-- | Phantom type for @INPlaybackRepeatModeResolutionResult@.
data INPlaybackRepeatModeResolutionResult

instance IsObjCObject (Id INPlaybackRepeatModeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPlaybackRepeatModeResolutionResult"

class IsINIntentResolutionResult a => IsINPlaybackRepeatModeResolutionResult a where
  toINPlaybackRepeatModeResolutionResult :: a -> Id INPlaybackRepeatModeResolutionResult

instance IsINPlaybackRepeatModeResolutionResult (Id INPlaybackRepeatModeResolutionResult) where
  toINPlaybackRepeatModeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INPlaybackRepeatModeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INPlaybackRepeatModeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INRadioTypeResolutionResult ----------

-- | Phantom type for @INRadioTypeResolutionResult@.
data INRadioTypeResolutionResult

instance IsObjCObject (Id INRadioTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRadioTypeResolutionResult"

class IsINIntentResolutionResult a => IsINRadioTypeResolutionResult a where
  toINRadioTypeResolutionResult :: a -> Id INRadioTypeResolutionResult

instance IsINRadioTypeResolutionResult (Id INRadioTypeResolutionResult) where
  toINRadioTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INRadioTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INRadioTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INRelativeReferenceResolutionResult ----------

-- | Phantom type for @INRelativeReferenceResolutionResult@.
data INRelativeReferenceResolutionResult

instance IsObjCObject (Id INRelativeReferenceResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRelativeReferenceResolutionResult"

class IsINIntentResolutionResult a => IsINRelativeReferenceResolutionResult a where
  toINRelativeReferenceResolutionResult :: a -> Id INRelativeReferenceResolutionResult

instance IsINRelativeReferenceResolutionResult (Id INRelativeReferenceResolutionResult) where
  toINRelativeReferenceResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INRelativeReferenceResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INRelativeReferenceResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INRelativeSettingResolutionResult ----------

-- | Phantom type for @INRelativeSettingResolutionResult@.
data INRelativeSettingResolutionResult

instance IsObjCObject (Id INRelativeSettingResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRelativeSettingResolutionResult"

class IsINIntentResolutionResult a => IsINRelativeSettingResolutionResult a where
  toINRelativeSettingResolutionResult :: a -> Id INRelativeSettingResolutionResult

instance IsINRelativeSettingResolutionResult (Id INRelativeSettingResolutionResult) where
  toINRelativeSettingResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INRelativeSettingResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INRelativeSettingResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INRestaurantGuestResolutionResult ----------

-- | Phantom type for @INRestaurantGuestResolutionResult@.
data INRestaurantGuestResolutionResult

instance IsObjCObject (Id INRestaurantGuestResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRestaurantGuestResolutionResult"

class IsINIntentResolutionResult a => IsINRestaurantGuestResolutionResult a where
  toINRestaurantGuestResolutionResult :: a -> Id INRestaurantGuestResolutionResult

instance IsINRestaurantGuestResolutionResult (Id INRestaurantGuestResolutionResult) where
  toINRestaurantGuestResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INRestaurantGuestResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INRestaurantGuestResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INRestaurantResolutionResult ----------

-- | Phantom type for @INRestaurantResolutionResult@.
data INRestaurantResolutionResult

instance IsObjCObject (Id INRestaurantResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRestaurantResolutionResult"

class IsINIntentResolutionResult a => IsINRestaurantResolutionResult a where
  toINRestaurantResolutionResult :: a -> Id INRestaurantResolutionResult

instance IsINRestaurantResolutionResult (Id INRestaurantResolutionResult) where
  toINRestaurantResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INRestaurantResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INRestaurantResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INSpatialEventTriggerResolutionResult ----------

-- | Phantom type for @INSpatialEventTriggerResolutionResult@.
data INSpatialEventTriggerResolutionResult

instance IsObjCObject (Id INSpatialEventTriggerResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSpatialEventTriggerResolutionResult"

class IsINIntentResolutionResult a => IsINSpatialEventTriggerResolutionResult a where
  toINSpatialEventTriggerResolutionResult :: a -> Id INSpatialEventTriggerResolutionResult

instance IsINSpatialEventTriggerResolutionResult (Id INSpatialEventTriggerResolutionResult) where
  toINSpatialEventTriggerResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INSpatialEventTriggerResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INSpatialEventTriggerResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INSpeakableStringResolutionResult ----------

-- | Phantom type for @INSpeakableStringResolutionResult@.
data INSpeakableStringResolutionResult

instance IsObjCObject (Id INSpeakableStringResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSpeakableStringResolutionResult"

class IsINIntentResolutionResult a => IsINSpeakableStringResolutionResult a where
  toINSpeakableStringResolutionResult :: a -> Id INSpeakableStringResolutionResult

instance IsINSpeakableStringResolutionResult (Id INSpeakableStringResolutionResult) where
  toINSpeakableStringResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INSpeakableStringResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INSpeakableStringResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INSpeedResolutionResult ----------

-- | Phantom type for @INSpeedResolutionResult@.
data INSpeedResolutionResult

instance IsObjCObject (Id INSpeedResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSpeedResolutionResult"

class IsINIntentResolutionResult a => IsINSpeedResolutionResult a where
  toINSpeedResolutionResult :: a -> Id INSpeedResolutionResult

instance IsINSpeedResolutionResult (Id INSpeedResolutionResult) where
  toINSpeedResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INSpeedResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INSpeedResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INStringResolutionResult ----------

-- | Phantom type for @INStringResolutionResult@.
data INStringResolutionResult

instance IsObjCObject (Id INStringResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStringResolutionResult"

class IsINIntentResolutionResult a => IsINStringResolutionResult a where
  toINStringResolutionResult :: a -> Id INStringResolutionResult

instance IsINStringResolutionResult (Id INStringResolutionResult) where
  toINStringResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INStringResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INStringResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INTaskListResolutionResult ----------

-- | Phantom type for @INTaskListResolutionResult@.
data INTaskListResolutionResult

instance IsObjCObject (Id INTaskListResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTaskListResolutionResult"

class IsINIntentResolutionResult a => IsINTaskListResolutionResult a where
  toINTaskListResolutionResult :: a -> Id INTaskListResolutionResult

instance IsINTaskListResolutionResult (Id INTaskListResolutionResult) where
  toINTaskListResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INTaskListResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INTaskListResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INTaskPriorityResolutionResult ----------

-- | Phantom type for @INTaskPriorityResolutionResult@.
data INTaskPriorityResolutionResult

instance IsObjCObject (Id INTaskPriorityResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTaskPriorityResolutionResult"

class IsINIntentResolutionResult a => IsINTaskPriorityResolutionResult a where
  toINTaskPriorityResolutionResult :: a -> Id INTaskPriorityResolutionResult

instance IsINTaskPriorityResolutionResult (Id INTaskPriorityResolutionResult) where
  toINTaskPriorityResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INTaskPriorityResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INTaskPriorityResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INTaskResolutionResult ----------

-- | Phantom type for @INTaskResolutionResult@.
data INTaskResolutionResult

instance IsObjCObject (Id INTaskResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTaskResolutionResult"

class IsINIntentResolutionResult a => IsINTaskResolutionResult a where
  toINTaskResolutionResult :: a -> Id INTaskResolutionResult

instance IsINTaskResolutionResult (Id INTaskResolutionResult) where
  toINTaskResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INTaskResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INTaskResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INTaskStatusResolutionResult ----------

-- | Phantom type for @INTaskStatusResolutionResult@.
data INTaskStatusResolutionResult

instance IsObjCObject (Id INTaskStatusResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTaskStatusResolutionResult"

class IsINIntentResolutionResult a => IsINTaskStatusResolutionResult a where
  toINTaskStatusResolutionResult :: a -> Id INTaskStatusResolutionResult

instance IsINTaskStatusResolutionResult (Id INTaskStatusResolutionResult) where
  toINTaskStatusResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INTaskStatusResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INTaskStatusResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INTemperatureResolutionResult ----------

-- | Phantom type for @INTemperatureResolutionResult@.
data INTemperatureResolutionResult

instance IsObjCObject (Id INTemperatureResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTemperatureResolutionResult"

class IsINIntentResolutionResult a => IsINTemperatureResolutionResult a where
  toINTemperatureResolutionResult :: a -> Id INTemperatureResolutionResult

instance IsINTemperatureResolutionResult (Id INTemperatureResolutionResult) where
  toINTemperatureResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INTemperatureResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INTemperatureResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INTemporalEventTriggerResolutionResult ----------

-- | Phantom type for @INTemporalEventTriggerResolutionResult@.
data INTemporalEventTriggerResolutionResult

instance IsObjCObject (Id INTemporalEventTriggerResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTemporalEventTriggerResolutionResult"

class IsINIntentResolutionResult a => IsINTemporalEventTriggerResolutionResult a where
  toINTemporalEventTriggerResolutionResult :: a -> Id INTemporalEventTriggerResolutionResult

instance IsINTemporalEventTriggerResolutionResult (Id INTemporalEventTriggerResolutionResult) where
  toINTemporalEventTriggerResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INTemporalEventTriggerResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INTemporalEventTriggerResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INTemporalEventTriggerTypeOptionsResolutionResult ----------

-- | Phantom type for @INTemporalEventTriggerTypeOptionsResolutionResult@.
data INTemporalEventTriggerTypeOptionsResolutionResult

instance IsObjCObject (Id INTemporalEventTriggerTypeOptionsResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTemporalEventTriggerTypeOptionsResolutionResult"

class IsINIntentResolutionResult a => IsINTemporalEventTriggerTypeOptionsResolutionResult a where
  toINTemporalEventTriggerTypeOptionsResolutionResult :: a -> Id INTemporalEventTriggerTypeOptionsResolutionResult

instance IsINTemporalEventTriggerTypeOptionsResolutionResult (Id INTemporalEventTriggerTypeOptionsResolutionResult) where
  toINTemporalEventTriggerTypeOptionsResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INTemporalEventTriggerTypeOptionsResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INTemporalEventTriggerTypeOptionsResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INTimeIntervalResolutionResult ----------

-- | Phantom type for @INTimeIntervalResolutionResult@.
data INTimeIntervalResolutionResult

instance IsObjCObject (Id INTimeIntervalResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTimeIntervalResolutionResult"

class IsINIntentResolutionResult a => IsINTimeIntervalResolutionResult a where
  toINTimeIntervalResolutionResult :: a -> Id INTimeIntervalResolutionResult

instance IsINTimeIntervalResolutionResult (Id INTimeIntervalResolutionResult) where
  toINTimeIntervalResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INTimeIntervalResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INTimeIntervalResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INURLResolutionResult ----------

-- | Phantom type for @INURLResolutionResult@.
data INURLResolutionResult

instance IsObjCObject (Id INURLResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INURLResolutionResult"

class IsINIntentResolutionResult a => IsINURLResolutionResult a where
  toINURLResolutionResult :: a -> Id INURLResolutionResult

instance IsINURLResolutionResult (Id INURLResolutionResult) where
  toINURLResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INURLResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INURLResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INVisualCodeTypeResolutionResult ----------

-- | Phantom type for @INVisualCodeTypeResolutionResult@.
data INVisualCodeTypeResolutionResult

instance IsObjCObject (Id INVisualCodeTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INVisualCodeTypeResolutionResult"

class IsINIntentResolutionResult a => IsINVisualCodeTypeResolutionResult a where
  toINVisualCodeTypeResolutionResult :: a -> Id INVisualCodeTypeResolutionResult

instance IsINVisualCodeTypeResolutionResult (Id INVisualCodeTypeResolutionResult) where
  toINVisualCodeTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INVisualCodeTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INVisualCodeTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INVolumeResolutionResult ----------

-- | Phantom type for @INVolumeResolutionResult@.
data INVolumeResolutionResult

instance IsObjCObject (Id INVolumeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INVolumeResolutionResult"

class IsINIntentResolutionResult a => IsINVolumeResolutionResult a where
  toINVolumeResolutionResult :: a -> Id INVolumeResolutionResult

instance IsINVolumeResolutionResult (Id INVolumeResolutionResult) where
  toINVolumeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INVolumeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INVolumeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INWorkoutGoalUnitTypeResolutionResult ----------

-- | Phantom type for @INWorkoutGoalUnitTypeResolutionResult@.
data INWorkoutGoalUnitTypeResolutionResult

instance IsObjCObject (Id INWorkoutGoalUnitTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INWorkoutGoalUnitTypeResolutionResult"

class IsINIntentResolutionResult a => IsINWorkoutGoalUnitTypeResolutionResult a where
  toINWorkoutGoalUnitTypeResolutionResult :: a -> Id INWorkoutGoalUnitTypeResolutionResult

instance IsINWorkoutGoalUnitTypeResolutionResult (Id INWorkoutGoalUnitTypeResolutionResult) where
  toINWorkoutGoalUnitTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INWorkoutGoalUnitTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INWorkoutGoalUnitTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INWorkoutLocationTypeResolutionResult ----------

-- | Phantom type for @INWorkoutLocationTypeResolutionResult@.
data INWorkoutLocationTypeResolutionResult

instance IsObjCObject (Id INWorkoutLocationTypeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INWorkoutLocationTypeResolutionResult"

class IsINIntentResolutionResult a => IsINWorkoutLocationTypeResolutionResult a where
  toINWorkoutLocationTypeResolutionResult :: a -> Id INWorkoutLocationTypeResolutionResult

instance IsINWorkoutLocationTypeResolutionResult (Id INWorkoutLocationTypeResolutionResult) where
  toINWorkoutLocationTypeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INWorkoutLocationTypeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INWorkoutLocationTypeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INActivateCarSignalIntentResponse ----------

-- | Phantom type for @INActivateCarSignalIntentResponse@.
data INActivateCarSignalIntentResponse

instance IsObjCObject (Id INActivateCarSignalIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INActivateCarSignalIntentResponse"

class IsINIntentResponse a => IsINActivateCarSignalIntentResponse a where
  toINActivateCarSignalIntentResponse :: a -> Id INActivateCarSignalIntentResponse

instance IsINActivateCarSignalIntentResponse (Id INActivateCarSignalIntentResponse) where
  toINActivateCarSignalIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INActivateCarSignalIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INActivateCarSignalIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INAddMediaIntentResponse ----------

-- | Phantom type for @INAddMediaIntentResponse@.
data INAddMediaIntentResponse

instance IsObjCObject (Id INAddMediaIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAddMediaIntentResponse"

class IsINIntentResponse a => IsINAddMediaIntentResponse a where
  toINAddMediaIntentResponse :: a -> Id INAddMediaIntentResponse

instance IsINAddMediaIntentResponse (Id INAddMediaIntentResponse) where
  toINAddMediaIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INAddMediaIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INAddMediaIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INAddTasksIntentResponse ----------

-- | Phantom type for @INAddTasksIntentResponse@.
data INAddTasksIntentResponse

instance IsObjCObject (Id INAddTasksIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAddTasksIntentResponse"

class IsINIntentResponse a => IsINAddTasksIntentResponse a where
  toINAddTasksIntentResponse :: a -> Id INAddTasksIntentResponse

instance IsINAddTasksIntentResponse (Id INAddTasksIntentResponse) where
  toINAddTasksIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INAddTasksIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INAddTasksIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INAnswerCallIntentResponse ----------

-- | Phantom type for @INAnswerCallIntentResponse@.
data INAnswerCallIntentResponse

instance IsObjCObject (Id INAnswerCallIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAnswerCallIntentResponse"

class IsINIntentResponse a => IsINAnswerCallIntentResponse a where
  toINAnswerCallIntentResponse :: a -> Id INAnswerCallIntentResponse

instance IsINAnswerCallIntentResponse (Id INAnswerCallIntentResponse) where
  toINAnswerCallIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INAnswerCallIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INAnswerCallIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INAppendToNoteIntentResponse ----------

-- | Phantom type for @INAppendToNoteIntentResponse@.
data INAppendToNoteIntentResponse

instance IsObjCObject (Id INAppendToNoteIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAppendToNoteIntentResponse"

class IsINIntentResponse a => IsINAppendToNoteIntentResponse a where
  toINAppendToNoteIntentResponse :: a -> Id INAppendToNoteIntentResponse

instance IsINAppendToNoteIntentResponse (Id INAppendToNoteIntentResponse) where
  toINAppendToNoteIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INAppendToNoteIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INAppendToNoteIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INBookRestaurantReservationIntentResponse ----------

-- | Phantom type for @INBookRestaurantReservationIntentResponse@.
data INBookRestaurantReservationIntentResponse

instance IsObjCObject (Id INBookRestaurantReservationIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBookRestaurantReservationIntentResponse"

class IsINIntentResponse a => IsINBookRestaurantReservationIntentResponse a where
  toINBookRestaurantReservationIntentResponse :: a -> Id INBookRestaurantReservationIntentResponse

instance IsINBookRestaurantReservationIntentResponse (Id INBookRestaurantReservationIntentResponse) where
  toINBookRestaurantReservationIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INBookRestaurantReservationIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INBookRestaurantReservationIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INCancelRideIntentResponse ----------

-- | Phantom type for @INCancelRideIntentResponse@.
data INCancelRideIntentResponse

instance IsObjCObject (Id INCancelRideIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCancelRideIntentResponse"

class IsINIntentResponse a => IsINCancelRideIntentResponse a where
  toINCancelRideIntentResponse :: a -> Id INCancelRideIntentResponse

instance IsINCancelRideIntentResponse (Id INCancelRideIntentResponse) where
  toINCancelRideIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INCancelRideIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INCancelRideIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INCancelWorkoutIntentResponse ----------

-- | Phantom type for @INCancelWorkoutIntentResponse@.
data INCancelWorkoutIntentResponse

instance IsObjCObject (Id INCancelWorkoutIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCancelWorkoutIntentResponse"

class IsINIntentResponse a => IsINCancelWorkoutIntentResponse a where
  toINCancelWorkoutIntentResponse :: a -> Id INCancelWorkoutIntentResponse

instance IsINCancelWorkoutIntentResponse (Id INCancelWorkoutIntentResponse) where
  toINCancelWorkoutIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INCancelWorkoutIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INCancelWorkoutIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INCreateNoteIntentResponse ----------

-- | Phantom type for @INCreateNoteIntentResponse@.
data INCreateNoteIntentResponse

instance IsObjCObject (Id INCreateNoteIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCreateNoteIntentResponse"

class IsINIntentResponse a => IsINCreateNoteIntentResponse a where
  toINCreateNoteIntentResponse :: a -> Id INCreateNoteIntentResponse

instance IsINCreateNoteIntentResponse (Id INCreateNoteIntentResponse) where
  toINCreateNoteIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INCreateNoteIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INCreateNoteIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INCreateTaskListIntentResponse ----------

-- | Phantom type for @INCreateTaskListIntentResponse@.
data INCreateTaskListIntentResponse

instance IsObjCObject (Id INCreateTaskListIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INCreateTaskListIntentResponse"

class IsINIntentResponse a => IsINCreateTaskListIntentResponse a where
  toINCreateTaskListIntentResponse :: a -> Id INCreateTaskListIntentResponse

instance IsINCreateTaskListIntentResponse (Id INCreateTaskListIntentResponse) where
  toINCreateTaskListIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INCreateTaskListIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INCreateTaskListIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INDeleteTasksIntentResponse ----------

-- | Phantom type for @INDeleteTasksIntentResponse@.
data INDeleteTasksIntentResponse

instance IsObjCObject (Id INDeleteTasksIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INDeleteTasksIntentResponse"

class IsINIntentResponse a => IsINDeleteTasksIntentResponse a where
  toINDeleteTasksIntentResponse :: a -> Id INDeleteTasksIntentResponse

instance IsINDeleteTasksIntentResponse (Id INDeleteTasksIntentResponse) where
  toINDeleteTasksIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INDeleteTasksIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INDeleteTasksIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INEditMessageIntentResponse ----------

-- | Phantom type for @INEditMessageIntentResponse@.
data INEditMessageIntentResponse

instance IsObjCObject (Id INEditMessageIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INEditMessageIntentResponse"

class IsINIntentResponse a => IsINEditMessageIntentResponse a where
  toINEditMessageIntentResponse :: a -> Id INEditMessageIntentResponse

instance IsINEditMessageIntentResponse (Id INEditMessageIntentResponse) where
  toINEditMessageIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INEditMessageIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INEditMessageIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INEndWorkoutIntentResponse ----------

-- | Phantom type for @INEndWorkoutIntentResponse@.
data INEndWorkoutIntentResponse

instance IsObjCObject (Id INEndWorkoutIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INEndWorkoutIntentResponse"

class IsINIntentResponse a => IsINEndWorkoutIntentResponse a where
  toINEndWorkoutIntentResponse :: a -> Id INEndWorkoutIntentResponse

instance IsINEndWorkoutIntentResponse (Id INEndWorkoutIntentResponse) where
  toINEndWorkoutIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INEndWorkoutIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INEndWorkoutIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INGetAvailableRestaurantReservationBookingDefaultsIntentResponse ----------

-- | Phantom type for @INGetAvailableRestaurantReservationBookingDefaultsIntentResponse@.
data INGetAvailableRestaurantReservationBookingDefaultsIntentResponse

instance IsObjCObject (Id INGetAvailableRestaurantReservationBookingDefaultsIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetAvailableRestaurantReservationBookingDefaultsIntentResponse"

class IsINIntentResponse a => IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse a where
  toINGetAvailableRestaurantReservationBookingDefaultsIntentResponse :: a -> Id INGetAvailableRestaurantReservationBookingDefaultsIntentResponse

instance IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse (Id INGetAvailableRestaurantReservationBookingDefaultsIntentResponse) where
  toINGetAvailableRestaurantReservationBookingDefaultsIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INGetAvailableRestaurantReservationBookingDefaultsIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INGetAvailableRestaurantReservationBookingDefaultsIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INGetAvailableRestaurantReservationBookingsIntentResponse ----------

-- | Phantom type for @INGetAvailableRestaurantReservationBookingsIntentResponse@.
data INGetAvailableRestaurantReservationBookingsIntentResponse

instance IsObjCObject (Id INGetAvailableRestaurantReservationBookingsIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetAvailableRestaurantReservationBookingsIntentResponse"

class IsINIntentResponse a => IsINGetAvailableRestaurantReservationBookingsIntentResponse a where
  toINGetAvailableRestaurantReservationBookingsIntentResponse :: a -> Id INGetAvailableRestaurantReservationBookingsIntentResponse

instance IsINGetAvailableRestaurantReservationBookingsIntentResponse (Id INGetAvailableRestaurantReservationBookingsIntentResponse) where
  toINGetAvailableRestaurantReservationBookingsIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INGetAvailableRestaurantReservationBookingsIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INGetAvailableRestaurantReservationBookingsIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INGetCarLockStatusIntentResponse ----------

-- | Phantom type for @INGetCarLockStatusIntentResponse@.
data INGetCarLockStatusIntentResponse

instance IsObjCObject (Id INGetCarLockStatusIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetCarLockStatusIntentResponse"

class IsINIntentResponse a => IsINGetCarLockStatusIntentResponse a where
  toINGetCarLockStatusIntentResponse :: a -> Id INGetCarLockStatusIntentResponse

instance IsINGetCarLockStatusIntentResponse (Id INGetCarLockStatusIntentResponse) where
  toINGetCarLockStatusIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INGetCarLockStatusIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INGetCarLockStatusIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INGetCarPowerLevelStatusIntentResponse ----------

-- | Phantom type for @INGetCarPowerLevelStatusIntentResponse@.
data INGetCarPowerLevelStatusIntentResponse

instance IsObjCObject (Id INGetCarPowerLevelStatusIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetCarPowerLevelStatusIntentResponse"

class IsINIntentResponse a => IsINGetCarPowerLevelStatusIntentResponse a where
  toINGetCarPowerLevelStatusIntentResponse :: a -> Id INGetCarPowerLevelStatusIntentResponse

instance IsINGetCarPowerLevelStatusIntentResponse (Id INGetCarPowerLevelStatusIntentResponse) where
  toINGetCarPowerLevelStatusIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INGetCarPowerLevelStatusIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INGetCarPowerLevelStatusIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INGetReservationDetailsIntentResponse ----------

-- | Phantom type for @INGetReservationDetailsIntentResponse@.
data INGetReservationDetailsIntentResponse

instance IsObjCObject (Id INGetReservationDetailsIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetReservationDetailsIntentResponse"

class IsINIntentResponse a => IsINGetReservationDetailsIntentResponse a where
  toINGetReservationDetailsIntentResponse :: a -> Id INGetReservationDetailsIntentResponse

instance IsINGetReservationDetailsIntentResponse (Id INGetReservationDetailsIntentResponse) where
  toINGetReservationDetailsIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INGetReservationDetailsIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INGetReservationDetailsIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INGetRestaurantGuestIntentResponse ----------

-- | Phantom type for @INGetRestaurantGuestIntentResponse@.
data INGetRestaurantGuestIntentResponse

instance IsObjCObject (Id INGetRestaurantGuestIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetRestaurantGuestIntentResponse"

class IsINIntentResponse a => IsINGetRestaurantGuestIntentResponse a where
  toINGetRestaurantGuestIntentResponse :: a -> Id INGetRestaurantGuestIntentResponse

instance IsINGetRestaurantGuestIntentResponse (Id INGetRestaurantGuestIntentResponse) where
  toINGetRestaurantGuestIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INGetRestaurantGuestIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INGetRestaurantGuestIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INGetRideStatusIntentResponse ----------

-- | Phantom type for @INGetRideStatusIntentResponse@.
data INGetRideStatusIntentResponse

instance IsObjCObject (Id INGetRideStatusIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetRideStatusIntentResponse"

class IsINIntentResponse a => IsINGetRideStatusIntentResponse a where
  toINGetRideStatusIntentResponse :: a -> Id INGetRideStatusIntentResponse

instance IsINGetRideStatusIntentResponse (Id INGetRideStatusIntentResponse) where
  toINGetRideStatusIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INGetRideStatusIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INGetRideStatusIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INGetUserCurrentRestaurantReservationBookingsIntentResponse ----------

-- | Phantom type for @INGetUserCurrentRestaurantReservationBookingsIntentResponse@.
data INGetUserCurrentRestaurantReservationBookingsIntentResponse

instance IsObjCObject (Id INGetUserCurrentRestaurantReservationBookingsIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetUserCurrentRestaurantReservationBookingsIntentResponse"

class IsINIntentResponse a => IsINGetUserCurrentRestaurantReservationBookingsIntentResponse a where
  toINGetUserCurrentRestaurantReservationBookingsIntentResponse :: a -> Id INGetUserCurrentRestaurantReservationBookingsIntentResponse

instance IsINGetUserCurrentRestaurantReservationBookingsIntentResponse (Id INGetUserCurrentRestaurantReservationBookingsIntentResponse) where
  toINGetUserCurrentRestaurantReservationBookingsIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INGetUserCurrentRestaurantReservationBookingsIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INGetUserCurrentRestaurantReservationBookingsIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INGetVisualCodeIntentResponse ----------

-- | Phantom type for @INGetVisualCodeIntentResponse@.
data INGetVisualCodeIntentResponse

instance IsObjCObject (Id INGetVisualCodeIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INGetVisualCodeIntentResponse"

class IsINIntentResponse a => IsINGetVisualCodeIntentResponse a where
  toINGetVisualCodeIntentResponse :: a -> Id INGetVisualCodeIntentResponse

instance IsINGetVisualCodeIntentResponse (Id INGetVisualCodeIntentResponse) where
  toINGetVisualCodeIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INGetVisualCodeIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INGetVisualCodeIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INHangUpCallIntentResponse ----------

-- | Phantom type for @INHangUpCallIntentResponse@.
data INHangUpCallIntentResponse

instance IsObjCObject (Id INHangUpCallIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INHangUpCallIntentResponse"

class IsINIntentResponse a => IsINHangUpCallIntentResponse a where
  toINHangUpCallIntentResponse :: a -> Id INHangUpCallIntentResponse

instance IsINHangUpCallIntentResponse (Id INHangUpCallIntentResponse) where
  toINHangUpCallIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INHangUpCallIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INHangUpCallIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INListCarsIntentResponse ----------

-- | Phantom type for @INListCarsIntentResponse@.
data INListCarsIntentResponse

instance IsObjCObject (Id INListCarsIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INListCarsIntentResponse"

class IsINIntentResponse a => IsINListCarsIntentResponse a where
  toINListCarsIntentResponse :: a -> Id INListCarsIntentResponse

instance IsINListCarsIntentResponse (Id INListCarsIntentResponse) where
  toINListCarsIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INListCarsIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INListCarsIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INListRideOptionsIntentResponse ----------

-- | Phantom type for @INListRideOptionsIntentResponse@.
data INListRideOptionsIntentResponse

instance IsObjCObject (Id INListRideOptionsIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INListRideOptionsIntentResponse"

class IsINIntentResponse a => IsINListRideOptionsIntentResponse a where
  toINListRideOptionsIntentResponse :: a -> Id INListRideOptionsIntentResponse

instance IsINListRideOptionsIntentResponse (Id INListRideOptionsIntentResponse) where
  toINListRideOptionsIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INListRideOptionsIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INListRideOptionsIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INPauseWorkoutIntentResponse ----------

-- | Phantom type for @INPauseWorkoutIntentResponse@.
data INPauseWorkoutIntentResponse

instance IsObjCObject (Id INPauseWorkoutIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPauseWorkoutIntentResponse"

class IsINIntentResponse a => IsINPauseWorkoutIntentResponse a where
  toINPauseWorkoutIntentResponse :: a -> Id INPauseWorkoutIntentResponse

instance IsINPauseWorkoutIntentResponse (Id INPauseWorkoutIntentResponse) where
  toINPauseWorkoutIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INPauseWorkoutIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INPauseWorkoutIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INPayBillIntentResponse ----------

-- | Phantom type for @INPayBillIntentResponse@.
data INPayBillIntentResponse

instance IsObjCObject (Id INPayBillIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPayBillIntentResponse"

class IsINIntentResponse a => IsINPayBillIntentResponse a where
  toINPayBillIntentResponse :: a -> Id INPayBillIntentResponse

instance IsINPayBillIntentResponse (Id INPayBillIntentResponse) where
  toINPayBillIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INPayBillIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INPayBillIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INPlayMediaIntentResponse ----------

-- | Phantom type for @INPlayMediaIntentResponse@.
data INPlayMediaIntentResponse

instance IsObjCObject (Id INPlayMediaIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPlayMediaIntentResponse"

class IsINIntentResponse a => IsINPlayMediaIntentResponse a where
  toINPlayMediaIntentResponse :: a -> Id INPlayMediaIntentResponse

instance IsINPlayMediaIntentResponse (Id INPlayMediaIntentResponse) where
  toINPlayMediaIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INPlayMediaIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INPlayMediaIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INRequestPaymentIntentResponse ----------

-- | Phantom type for @INRequestPaymentIntentResponse@.
data INRequestPaymentIntentResponse

instance IsObjCObject (Id INRequestPaymentIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRequestPaymentIntentResponse"

class IsINIntentResponse a => IsINRequestPaymentIntentResponse a where
  toINRequestPaymentIntentResponse :: a -> Id INRequestPaymentIntentResponse

instance IsINRequestPaymentIntentResponse (Id INRequestPaymentIntentResponse) where
  toINRequestPaymentIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INRequestPaymentIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INRequestPaymentIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INRequestRideIntentResponse ----------

-- | Phantom type for @INRequestRideIntentResponse@.
data INRequestRideIntentResponse

instance IsObjCObject (Id INRequestRideIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRequestRideIntentResponse"

class IsINIntentResponse a => IsINRequestRideIntentResponse a where
  toINRequestRideIntentResponse :: a -> Id INRequestRideIntentResponse

instance IsINRequestRideIntentResponse (Id INRequestRideIntentResponse) where
  toINRequestRideIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INRequestRideIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INRequestRideIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INResumeWorkoutIntentResponse ----------

-- | Phantom type for @INResumeWorkoutIntentResponse@.
data INResumeWorkoutIntentResponse

instance IsObjCObject (Id INResumeWorkoutIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INResumeWorkoutIntentResponse"

class IsINIntentResponse a => IsINResumeWorkoutIntentResponse a where
  toINResumeWorkoutIntentResponse :: a -> Id INResumeWorkoutIntentResponse

instance IsINResumeWorkoutIntentResponse (Id INResumeWorkoutIntentResponse) where
  toINResumeWorkoutIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INResumeWorkoutIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INResumeWorkoutIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSaveProfileInCarIntentResponse ----------

-- | Phantom type for @INSaveProfileInCarIntentResponse@.
data INSaveProfileInCarIntentResponse

instance IsObjCObject (Id INSaveProfileInCarIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSaveProfileInCarIntentResponse"

class IsINIntentResponse a => IsINSaveProfileInCarIntentResponse a where
  toINSaveProfileInCarIntentResponse :: a -> Id INSaveProfileInCarIntentResponse

instance IsINSaveProfileInCarIntentResponse (Id INSaveProfileInCarIntentResponse) where
  toINSaveProfileInCarIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSaveProfileInCarIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSaveProfileInCarIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSearchCallHistoryIntentResponse ----------

-- | Phantom type for @INSearchCallHistoryIntentResponse@.
data INSearchCallHistoryIntentResponse

instance IsObjCObject (Id INSearchCallHistoryIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchCallHistoryIntentResponse"

class IsINIntentResponse a => IsINSearchCallHistoryIntentResponse a where
  toINSearchCallHistoryIntentResponse :: a -> Id INSearchCallHistoryIntentResponse

instance IsINSearchCallHistoryIntentResponse (Id INSearchCallHistoryIntentResponse) where
  toINSearchCallHistoryIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSearchCallHistoryIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSearchCallHistoryIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSearchForAccountsIntentResponse ----------

-- | Phantom type for @INSearchForAccountsIntentResponse@.
data INSearchForAccountsIntentResponse

instance IsObjCObject (Id INSearchForAccountsIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForAccountsIntentResponse"

class IsINIntentResponse a => IsINSearchForAccountsIntentResponse a where
  toINSearchForAccountsIntentResponse :: a -> Id INSearchForAccountsIntentResponse

instance IsINSearchForAccountsIntentResponse (Id INSearchForAccountsIntentResponse) where
  toINSearchForAccountsIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSearchForAccountsIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSearchForAccountsIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSearchForBillsIntentResponse ----------

-- | Phantom type for @INSearchForBillsIntentResponse@.
data INSearchForBillsIntentResponse

instance IsObjCObject (Id INSearchForBillsIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForBillsIntentResponse"

class IsINIntentResponse a => IsINSearchForBillsIntentResponse a where
  toINSearchForBillsIntentResponse :: a -> Id INSearchForBillsIntentResponse

instance IsINSearchForBillsIntentResponse (Id INSearchForBillsIntentResponse) where
  toINSearchForBillsIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSearchForBillsIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSearchForBillsIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSearchForMediaIntentResponse ----------

-- | Phantom type for @INSearchForMediaIntentResponse@.
data INSearchForMediaIntentResponse

instance IsObjCObject (Id INSearchForMediaIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForMediaIntentResponse"

class IsINIntentResponse a => IsINSearchForMediaIntentResponse a where
  toINSearchForMediaIntentResponse :: a -> Id INSearchForMediaIntentResponse

instance IsINSearchForMediaIntentResponse (Id INSearchForMediaIntentResponse) where
  toINSearchForMediaIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSearchForMediaIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSearchForMediaIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSearchForMessagesIntentResponse ----------

-- | Phantom type for @INSearchForMessagesIntentResponse@.
data INSearchForMessagesIntentResponse

instance IsObjCObject (Id INSearchForMessagesIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForMessagesIntentResponse"

class IsINIntentResponse a => IsINSearchForMessagesIntentResponse a where
  toINSearchForMessagesIntentResponse :: a -> Id INSearchForMessagesIntentResponse

instance IsINSearchForMessagesIntentResponse (Id INSearchForMessagesIntentResponse) where
  toINSearchForMessagesIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSearchForMessagesIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSearchForMessagesIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSearchForNotebookItemsIntentResponse ----------

-- | Phantom type for @INSearchForNotebookItemsIntentResponse@.
data INSearchForNotebookItemsIntentResponse

instance IsObjCObject (Id INSearchForNotebookItemsIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForNotebookItemsIntentResponse"

class IsINIntentResponse a => IsINSearchForNotebookItemsIntentResponse a where
  toINSearchForNotebookItemsIntentResponse :: a -> Id INSearchForNotebookItemsIntentResponse

instance IsINSearchForNotebookItemsIntentResponse (Id INSearchForNotebookItemsIntentResponse) where
  toINSearchForNotebookItemsIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSearchForNotebookItemsIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSearchForNotebookItemsIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSearchForPhotosIntentResponse ----------

-- | Phantom type for @INSearchForPhotosIntentResponse@.
data INSearchForPhotosIntentResponse

instance IsObjCObject (Id INSearchForPhotosIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForPhotosIntentResponse"

class IsINIntentResponse a => IsINSearchForPhotosIntentResponse a where
  toINSearchForPhotosIntentResponse :: a -> Id INSearchForPhotosIntentResponse

instance IsINSearchForPhotosIntentResponse (Id INSearchForPhotosIntentResponse) where
  toINSearchForPhotosIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSearchForPhotosIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSearchForPhotosIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSendMessageIntentResponse ----------

-- | Phantom type for @INSendMessageIntentResponse@.
data INSendMessageIntentResponse

instance IsObjCObject (Id INSendMessageIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSendMessageIntentResponse"

class IsINIntentResponse a => IsINSendMessageIntentResponse a where
  toINSendMessageIntentResponse :: a -> Id INSendMessageIntentResponse

instance IsINSendMessageIntentResponse (Id INSendMessageIntentResponse) where
  toINSendMessageIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSendMessageIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSendMessageIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSendPaymentIntentResponse ----------

-- | Phantom type for @INSendPaymentIntentResponse@.
data INSendPaymentIntentResponse

instance IsObjCObject (Id INSendPaymentIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSendPaymentIntentResponse"

class IsINIntentResponse a => IsINSendPaymentIntentResponse a where
  toINSendPaymentIntentResponse :: a -> Id INSendPaymentIntentResponse

instance IsINSendPaymentIntentResponse (Id INSendPaymentIntentResponse) where
  toINSendPaymentIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSendPaymentIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSendPaymentIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSendRideFeedbackIntentResponse ----------

-- | Phantom type for @INSendRideFeedbackIntentResponse@.
data INSendRideFeedbackIntentResponse

instance IsObjCObject (Id INSendRideFeedbackIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSendRideFeedbackIntentResponse"

class IsINIntentResponse a => IsINSendRideFeedbackIntentResponse a where
  toINSendRideFeedbackIntentResponse :: a -> Id INSendRideFeedbackIntentResponse

instance IsINSendRideFeedbackIntentResponse (Id INSendRideFeedbackIntentResponse) where
  toINSendRideFeedbackIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSendRideFeedbackIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSendRideFeedbackIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSetAudioSourceInCarIntentResponse ----------

-- | Phantom type for @INSetAudioSourceInCarIntentResponse@.
data INSetAudioSourceInCarIntentResponse

instance IsObjCObject (Id INSetAudioSourceInCarIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetAudioSourceInCarIntentResponse"

class IsINIntentResponse a => IsINSetAudioSourceInCarIntentResponse a where
  toINSetAudioSourceInCarIntentResponse :: a -> Id INSetAudioSourceInCarIntentResponse

instance IsINSetAudioSourceInCarIntentResponse (Id INSetAudioSourceInCarIntentResponse) where
  toINSetAudioSourceInCarIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSetAudioSourceInCarIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSetAudioSourceInCarIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSetCarLockStatusIntentResponse ----------

-- | Phantom type for @INSetCarLockStatusIntentResponse@.
data INSetCarLockStatusIntentResponse

instance IsObjCObject (Id INSetCarLockStatusIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetCarLockStatusIntentResponse"

class IsINIntentResponse a => IsINSetCarLockStatusIntentResponse a where
  toINSetCarLockStatusIntentResponse :: a -> Id INSetCarLockStatusIntentResponse

instance IsINSetCarLockStatusIntentResponse (Id INSetCarLockStatusIntentResponse) where
  toINSetCarLockStatusIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSetCarLockStatusIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSetCarLockStatusIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSetClimateSettingsInCarIntentResponse ----------

-- | Phantom type for @INSetClimateSettingsInCarIntentResponse@.
data INSetClimateSettingsInCarIntentResponse

instance IsObjCObject (Id INSetClimateSettingsInCarIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetClimateSettingsInCarIntentResponse"

class IsINIntentResponse a => IsINSetClimateSettingsInCarIntentResponse a where
  toINSetClimateSettingsInCarIntentResponse :: a -> Id INSetClimateSettingsInCarIntentResponse

instance IsINSetClimateSettingsInCarIntentResponse (Id INSetClimateSettingsInCarIntentResponse) where
  toINSetClimateSettingsInCarIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSetClimateSettingsInCarIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSetClimateSettingsInCarIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSetDefrosterSettingsInCarIntentResponse ----------

-- | Phantom type for @INSetDefrosterSettingsInCarIntentResponse@.
data INSetDefrosterSettingsInCarIntentResponse

instance IsObjCObject (Id INSetDefrosterSettingsInCarIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetDefrosterSettingsInCarIntentResponse"

class IsINIntentResponse a => IsINSetDefrosterSettingsInCarIntentResponse a where
  toINSetDefrosterSettingsInCarIntentResponse :: a -> Id INSetDefrosterSettingsInCarIntentResponse

instance IsINSetDefrosterSettingsInCarIntentResponse (Id INSetDefrosterSettingsInCarIntentResponse) where
  toINSetDefrosterSettingsInCarIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSetDefrosterSettingsInCarIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSetDefrosterSettingsInCarIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSetMessageAttributeIntentResponse ----------

-- | Phantom type for @INSetMessageAttributeIntentResponse@.
data INSetMessageAttributeIntentResponse

instance IsObjCObject (Id INSetMessageAttributeIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetMessageAttributeIntentResponse"

class IsINIntentResponse a => IsINSetMessageAttributeIntentResponse a where
  toINSetMessageAttributeIntentResponse :: a -> Id INSetMessageAttributeIntentResponse

instance IsINSetMessageAttributeIntentResponse (Id INSetMessageAttributeIntentResponse) where
  toINSetMessageAttributeIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSetMessageAttributeIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSetMessageAttributeIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSetProfileInCarIntentResponse ----------

-- | Phantom type for @INSetProfileInCarIntentResponse@.
data INSetProfileInCarIntentResponse

instance IsObjCObject (Id INSetProfileInCarIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetProfileInCarIntentResponse"

class IsINIntentResponse a => IsINSetProfileInCarIntentResponse a where
  toINSetProfileInCarIntentResponse :: a -> Id INSetProfileInCarIntentResponse

instance IsINSetProfileInCarIntentResponse (Id INSetProfileInCarIntentResponse) where
  toINSetProfileInCarIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSetProfileInCarIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSetProfileInCarIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSetRadioStationIntentResponse ----------

-- | Phantom type for @INSetRadioStationIntentResponse@.
data INSetRadioStationIntentResponse

instance IsObjCObject (Id INSetRadioStationIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetRadioStationIntentResponse"

class IsINIntentResponse a => IsINSetRadioStationIntentResponse a where
  toINSetRadioStationIntentResponse :: a -> Id INSetRadioStationIntentResponse

instance IsINSetRadioStationIntentResponse (Id INSetRadioStationIntentResponse) where
  toINSetRadioStationIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSetRadioStationIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSetRadioStationIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSetSeatSettingsInCarIntentResponse ----------

-- | Phantom type for @INSetSeatSettingsInCarIntentResponse@.
data INSetSeatSettingsInCarIntentResponse

instance IsObjCObject (Id INSetSeatSettingsInCarIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetSeatSettingsInCarIntentResponse"

class IsINIntentResponse a => IsINSetSeatSettingsInCarIntentResponse a where
  toINSetSeatSettingsInCarIntentResponse :: a -> Id INSetSeatSettingsInCarIntentResponse

instance IsINSetSeatSettingsInCarIntentResponse (Id INSetSeatSettingsInCarIntentResponse) where
  toINSetSeatSettingsInCarIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSetSeatSettingsInCarIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSetSeatSettingsInCarIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSetTaskAttributeIntentResponse ----------

-- | Phantom type for @INSetTaskAttributeIntentResponse@.
data INSetTaskAttributeIntentResponse

instance IsObjCObject (Id INSetTaskAttributeIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetTaskAttributeIntentResponse"

class IsINIntentResponse a => IsINSetTaskAttributeIntentResponse a where
  toINSetTaskAttributeIntentResponse :: a -> Id INSetTaskAttributeIntentResponse

instance IsINSetTaskAttributeIntentResponse (Id INSetTaskAttributeIntentResponse) where
  toINSetTaskAttributeIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSetTaskAttributeIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSetTaskAttributeIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INShareFocusStatusIntentResponse ----------

-- | Phantom type for @INShareFocusStatusIntentResponse@.
data INShareFocusStatusIntentResponse

instance IsObjCObject (Id INShareFocusStatusIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INShareFocusStatusIntentResponse"

class IsINIntentResponse a => IsINShareFocusStatusIntentResponse a where
  toINShareFocusStatusIntentResponse :: a -> Id INShareFocusStatusIntentResponse

instance IsINShareFocusStatusIntentResponse (Id INShareFocusStatusIntentResponse) where
  toINShareFocusStatusIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INShareFocusStatusIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INShareFocusStatusIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INSnoozeTasksIntentResponse ----------

-- | Phantom type for @INSnoozeTasksIntentResponse@.
data INSnoozeTasksIntentResponse

instance IsObjCObject (Id INSnoozeTasksIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSnoozeTasksIntentResponse"

class IsINIntentResponse a => IsINSnoozeTasksIntentResponse a where
  toINSnoozeTasksIntentResponse :: a -> Id INSnoozeTasksIntentResponse

instance IsINSnoozeTasksIntentResponse (Id INSnoozeTasksIntentResponse) where
  toINSnoozeTasksIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INSnoozeTasksIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INSnoozeTasksIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INStartAudioCallIntentResponse ----------

-- | Phantom type for @INStartAudioCallIntentResponse@.
data INStartAudioCallIntentResponse

instance IsObjCObject (Id INStartAudioCallIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartAudioCallIntentResponse"

class IsINIntentResponse a => IsINStartAudioCallIntentResponse a where
  toINStartAudioCallIntentResponse :: a -> Id INStartAudioCallIntentResponse

instance IsINStartAudioCallIntentResponse (Id INStartAudioCallIntentResponse) where
  toINStartAudioCallIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INStartAudioCallIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INStartAudioCallIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INStartCallIntentResponse ----------

-- | Phantom type for @INStartCallIntentResponse@.
data INStartCallIntentResponse

instance IsObjCObject (Id INStartCallIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartCallIntentResponse"

class IsINIntentResponse a => IsINStartCallIntentResponse a where
  toINStartCallIntentResponse :: a -> Id INStartCallIntentResponse

instance IsINStartCallIntentResponse (Id INStartCallIntentResponse) where
  toINStartCallIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INStartCallIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INStartCallIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INStartPhotoPlaybackIntentResponse ----------

-- | Phantom type for @INStartPhotoPlaybackIntentResponse@.
data INStartPhotoPlaybackIntentResponse

instance IsObjCObject (Id INStartPhotoPlaybackIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartPhotoPlaybackIntentResponse"

class IsINIntentResponse a => IsINStartPhotoPlaybackIntentResponse a where
  toINStartPhotoPlaybackIntentResponse :: a -> Id INStartPhotoPlaybackIntentResponse

instance IsINStartPhotoPlaybackIntentResponse (Id INStartPhotoPlaybackIntentResponse) where
  toINStartPhotoPlaybackIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INStartPhotoPlaybackIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INStartPhotoPlaybackIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INStartVideoCallIntentResponse ----------

-- | Phantom type for @INStartVideoCallIntentResponse@.
data INStartVideoCallIntentResponse

instance IsObjCObject (Id INStartVideoCallIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartVideoCallIntentResponse"

class IsINIntentResponse a => IsINStartVideoCallIntentResponse a where
  toINStartVideoCallIntentResponse :: a -> Id INStartVideoCallIntentResponse

instance IsINStartVideoCallIntentResponse (Id INStartVideoCallIntentResponse) where
  toINStartVideoCallIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INStartVideoCallIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INStartVideoCallIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INStartWorkoutIntentResponse ----------

-- | Phantom type for @INStartWorkoutIntentResponse@.
data INStartWorkoutIntentResponse

instance IsObjCObject (Id INStartWorkoutIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartWorkoutIntentResponse"

class IsINIntentResponse a => IsINStartWorkoutIntentResponse a where
  toINStartWorkoutIntentResponse :: a -> Id INStartWorkoutIntentResponse

instance IsINStartWorkoutIntentResponse (Id INStartWorkoutIntentResponse) where
  toINStartWorkoutIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INStartWorkoutIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INStartWorkoutIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INTransferMoneyIntentResponse ----------

-- | Phantom type for @INTransferMoneyIntentResponse@.
data INTransferMoneyIntentResponse

instance IsObjCObject (Id INTransferMoneyIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTransferMoneyIntentResponse"

class IsINIntentResponse a => IsINTransferMoneyIntentResponse a where
  toINTransferMoneyIntentResponse :: a -> Id INTransferMoneyIntentResponse

instance IsINTransferMoneyIntentResponse (Id INTransferMoneyIntentResponse) where
  toINTransferMoneyIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INTransferMoneyIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INTransferMoneyIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INUnsendMessagesIntentResponse ----------

-- | Phantom type for @INUnsendMessagesIntentResponse@.
data INUnsendMessagesIntentResponse

instance IsObjCObject (Id INUnsendMessagesIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INUnsendMessagesIntentResponse"

class IsINIntentResponse a => IsINUnsendMessagesIntentResponse a where
  toINUnsendMessagesIntentResponse :: a -> Id INUnsendMessagesIntentResponse

instance IsINUnsendMessagesIntentResponse (Id INUnsendMessagesIntentResponse) where
  toINUnsendMessagesIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INUnsendMessagesIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INUnsendMessagesIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INUpdateMediaAffinityIntentResponse ----------

-- | Phantom type for @INUpdateMediaAffinityIntentResponse@.
data INUpdateMediaAffinityIntentResponse

instance IsObjCObject (Id INUpdateMediaAffinityIntentResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INUpdateMediaAffinityIntentResponse"

class IsINIntentResponse a => IsINUpdateMediaAffinityIntentResponse a where
  toINUpdateMediaAffinityIntentResponse :: a -> Id INUpdateMediaAffinityIntentResponse

instance IsINUpdateMediaAffinityIntentResponse (Id INUpdateMediaAffinityIntentResponse) where
  toINUpdateMediaAffinityIntentResponse = unsafeCastId

instance IsINIntentResponse (Id INUpdateMediaAffinityIntentResponse) where
  toINIntentResponse = unsafeCastId

instance IsNSObject (Id INUpdateMediaAffinityIntentResponse) where
  toNSObject = unsafeCastId

-- ---------- INImageNoteContent ----------

-- | Phantom type for @INImageNoteContent@.
data INImageNoteContent

instance IsObjCObject (Id INImageNoteContent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INImageNoteContent"

class IsINNoteContent a => IsINImageNoteContent a where
  toINImageNoteContent :: a -> Id INImageNoteContent

instance IsINImageNoteContent (Id INImageNoteContent) where
  toINImageNoteContent = unsafeCastId

instance IsINNoteContent (Id INImageNoteContent) where
  toINNoteContent = unsafeCastId

instance IsNSObject (Id INImageNoteContent) where
  toNSObject = unsafeCastId

-- ---------- INTextNoteContent ----------

-- | Phantom type for @INTextNoteContent@.
data INTextNoteContent

instance IsObjCObject (Id INTextNoteContent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTextNoteContent"

class IsINNoteContent a => IsINTextNoteContent a where
  toINTextNoteContent :: a -> Id INTextNoteContent

instance IsINTextNoteContent (Id INTextNoteContent) where
  toINTextNoteContent = unsafeCastId

instance IsINNoteContent (Id INTextNoteContent) where
  toINNoteContent = unsafeCastId

instance IsNSObject (Id INTextNoteContent) where
  toNSObject = unsafeCastId

-- ---------- INRestaurantGuest ----------

-- | Phantom type for @INRestaurantGuest@.
data INRestaurantGuest

instance IsObjCObject (Id INRestaurantGuest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRestaurantGuest"

class IsINPerson a => IsINRestaurantGuest a where
  toINRestaurantGuest :: a -> Id INRestaurantGuest

instance IsINRestaurantGuest (Id INRestaurantGuest) where
  toINRestaurantGuest = unsafeCastId

instance IsINPerson (Id INRestaurantGuest) where
  toINPerson = unsafeCastId

instance IsNSObject (Id INRestaurantGuest) where
  toNSObject = unsafeCastId

-- ---------- INRideDriver ----------

-- | Phantom type for @INRideDriver@.
data INRideDriver

instance IsObjCObject (Id INRideDriver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRideDriver"

class IsINPerson a => IsINRideDriver a where
  toINRideDriver :: a -> Id INRideDriver

instance IsINRideDriver (Id INRideDriver) where
  toINRideDriver = unsafeCastId

instance IsINPerson (Id INRideDriver) where
  toINPerson = unsafeCastId

instance IsNSObject (Id INRideDriver) where
  toNSObject = unsafeCastId

-- ---------- INDailyRoutineRelevanceProvider ----------

-- | A relevance provider that specifies relevance during a specific situation.
--
-- INDailyRoutineSituation
-- 
-- Phantom type for @INDailyRoutineRelevanceProvider@.
data INDailyRoutineRelevanceProvider

instance IsObjCObject (Id INDailyRoutineRelevanceProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INDailyRoutineRelevanceProvider"

class IsINRelevanceProvider a => IsINDailyRoutineRelevanceProvider a where
  toINDailyRoutineRelevanceProvider :: a -> Id INDailyRoutineRelevanceProvider

instance IsINDailyRoutineRelevanceProvider (Id INDailyRoutineRelevanceProvider) where
  toINDailyRoutineRelevanceProvider = unsafeCastId

instance IsINRelevanceProvider (Id INDailyRoutineRelevanceProvider) where
  toINRelevanceProvider = unsafeCastId

instance IsNSObject (Id INDailyRoutineRelevanceProvider) where
  toNSObject = unsafeCastId

-- ---------- INDateRelevanceProvider ----------

-- | A relevance provider to indicate relevance at a date or date interval.
-- 
-- Phantom type for @INDateRelevanceProvider@.
data INDateRelevanceProvider

instance IsObjCObject (Id INDateRelevanceProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INDateRelevanceProvider"

class IsINRelevanceProvider a => IsINDateRelevanceProvider a where
  toINDateRelevanceProvider :: a -> Id INDateRelevanceProvider

instance IsINDateRelevanceProvider (Id INDateRelevanceProvider) where
  toINDateRelevanceProvider = unsafeCastId

instance IsINRelevanceProvider (Id INDateRelevanceProvider) where
  toINRelevanceProvider = unsafeCastId

instance IsNSObject (Id INDateRelevanceProvider) where
  toNSObject = unsafeCastId

-- ---------- INLocationRelevanceProvider ----------

-- | Phantom type for @INLocationRelevanceProvider@.
data INLocationRelevanceProvider

instance IsObjCObject (Id INLocationRelevanceProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INLocationRelevanceProvider"

class IsINRelevanceProvider a => IsINLocationRelevanceProvider a where
  toINLocationRelevanceProvider :: a -> Id INLocationRelevanceProvider

instance IsINLocationRelevanceProvider (Id INLocationRelevanceProvider) where
  toINLocationRelevanceProvider = unsafeCastId

instance IsINRelevanceProvider (Id INLocationRelevanceProvider) where
  toINRelevanceProvider = unsafeCastId

instance IsNSObject (Id INLocationRelevanceProvider) where
  toNSObject = unsafeCastId

-- ---------- INBoatReservation ----------

-- | Phantom type for @INBoatReservation@.
data INBoatReservation

instance IsObjCObject (Id INBoatReservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBoatReservation"

class IsINReservation a => IsINBoatReservation a where
  toINBoatReservation :: a -> Id INBoatReservation

instance IsINBoatReservation (Id INBoatReservation) where
  toINBoatReservation = unsafeCastId

instance IsINReservation (Id INBoatReservation) where
  toINReservation = unsafeCastId

instance IsNSObject (Id INBoatReservation) where
  toNSObject = unsafeCastId

-- ---------- INBusReservation ----------

-- | Phantom type for @INBusReservation@.
data INBusReservation

instance IsObjCObject (Id INBusReservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INBusReservation"

class IsINReservation a => IsINBusReservation a where
  toINBusReservation :: a -> Id INBusReservation

instance IsINBusReservation (Id INBusReservation) where
  toINBusReservation = unsafeCastId

instance IsINReservation (Id INBusReservation) where
  toINReservation = unsafeCastId

instance IsNSObject (Id INBusReservation) where
  toNSObject = unsafeCastId

-- ---------- INFlightReservation ----------

-- | Phantom type for @INFlightReservation@.
data INFlightReservation

instance IsObjCObject (Id INFlightReservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INFlightReservation"

class IsINReservation a => IsINFlightReservation a where
  toINFlightReservation :: a -> Id INFlightReservation

instance IsINFlightReservation (Id INFlightReservation) where
  toINFlightReservation = unsafeCastId

instance IsINReservation (Id INFlightReservation) where
  toINReservation = unsafeCastId

instance IsNSObject (Id INFlightReservation) where
  toNSObject = unsafeCastId

-- ---------- INLodgingReservation ----------

-- | Phantom type for @INLodgingReservation@.
data INLodgingReservation

instance IsObjCObject (Id INLodgingReservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INLodgingReservation"

class IsINReservation a => IsINLodgingReservation a where
  toINLodgingReservation :: a -> Id INLodgingReservation

instance IsINLodgingReservation (Id INLodgingReservation) where
  toINLodgingReservation = unsafeCastId

instance IsINReservation (Id INLodgingReservation) where
  toINReservation = unsafeCastId

instance IsNSObject (Id INLodgingReservation) where
  toNSObject = unsafeCastId

-- ---------- INRentalCarReservation ----------

-- | Phantom type for @INRentalCarReservation@.
data INRentalCarReservation

instance IsObjCObject (Id INRentalCarReservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRentalCarReservation"

class IsINReservation a => IsINRentalCarReservation a where
  toINRentalCarReservation :: a -> Id INRentalCarReservation

instance IsINRentalCarReservation (Id INRentalCarReservation) where
  toINRentalCarReservation = unsafeCastId

instance IsINReservation (Id INRentalCarReservation) where
  toINReservation = unsafeCastId

instance IsNSObject (Id INRentalCarReservation) where
  toNSObject = unsafeCastId

-- ---------- INRestaurantReservation ----------

-- | Phantom type for @INRestaurantReservation@.
data INRestaurantReservation

instance IsObjCObject (Id INRestaurantReservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRestaurantReservation"

class IsINReservation a => IsINRestaurantReservation a where
  toINRestaurantReservation :: a -> Id INRestaurantReservation

instance IsINRestaurantReservation (Id INRestaurantReservation) where
  toINRestaurantReservation = unsafeCastId

instance IsINReservation (Id INRestaurantReservation) where
  toINReservation = unsafeCastId

instance IsNSObject (Id INRestaurantReservation) where
  toNSObject = unsafeCastId

-- ---------- INTicketedEventReservation ----------

-- | Phantom type for @INTicketedEventReservation@.
data INTicketedEventReservation

instance IsObjCObject (Id INTicketedEventReservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTicketedEventReservation"

class IsINReservation a => IsINTicketedEventReservation a where
  toINTicketedEventReservation :: a -> Id INTicketedEventReservation

instance IsINTicketedEventReservation (Id INTicketedEventReservation) where
  toINTicketedEventReservation = unsafeCastId

instance IsINReservation (Id INTicketedEventReservation) where
  toINReservation = unsafeCastId

instance IsNSObject (Id INTicketedEventReservation) where
  toNSObject = unsafeCastId

-- ---------- INTrainReservation ----------

-- | Phantom type for @INTrainReservation@.
data INTrainReservation

instance IsObjCObject (Id INTrainReservation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INTrainReservation"

class IsINReservation a => IsINTrainReservation a where
  toINTrainReservation :: a -> Id INTrainReservation

instance IsINTrainReservation (Id INTrainReservation) where
  toINTrainReservation = unsafeCastId

instance IsINReservation (Id INTrainReservation) where
  toINReservation = unsafeCastId

instance IsNSObject (Id INTrainReservation) where
  toNSObject = unsafeCastId

-- ---------- INRestaurantReservationUserBooking ----------

-- | Phantom type for @INRestaurantReservationUserBooking@.
data INRestaurantReservationUserBooking

instance IsObjCObject (Id INRestaurantReservationUserBooking) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRestaurantReservationUserBooking"

class IsINRestaurantReservationBooking a => IsINRestaurantReservationUserBooking a where
  toINRestaurantReservationUserBooking :: a -> Id INRestaurantReservationUserBooking

instance IsINRestaurantReservationUserBooking (Id INRestaurantReservationUserBooking) where
  toINRestaurantReservationUserBooking = unsafeCastId

instance IsINRestaurantReservationBooking (Id INRestaurantReservationUserBooking) where
  toINRestaurantReservationBooking = unsafeCastId

instance IsNSObject (Id INRestaurantReservationUserBooking) where
  toNSObject = unsafeCastId

-- ---------- INMediaUserContext ----------

-- | Phantom type for @INMediaUserContext@.
data INMediaUserContext

instance IsObjCObject (Id INMediaUserContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INMediaUserContext"

class IsINUserContext a => IsINMediaUserContext a where
  toINMediaUserContext :: a -> Id INMediaUserContext

instance IsINMediaUserContext (Id INMediaUserContext) where
  toINMediaUserContext = unsafeCastId

instance IsINUserContext (Id INMediaUserContext) where
  toINUserContext = unsafeCastId

instance IsNSObject (Id INMediaUserContext) where
  toNSObject = unsafeCastId

-- ---------- UNMutableNotificationContent ----------

-- | Phantom type for @UNMutableNotificationContent@.
data UNMutableNotificationContent

instance IsObjCObject (Id UNMutableNotificationContent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNMutableNotificationContent"

class IsUNNotificationContent a => IsUNMutableNotificationContent a where
  toUNMutableNotificationContent :: a -> Id UNMutableNotificationContent

instance IsUNMutableNotificationContent (Id UNMutableNotificationContent) where
  toUNMutableNotificationContent = unsafeCastId

instance IsNSObject (Id UNMutableNotificationContent) where
  toNSObject = unsafeCastId

instance IsUNNotificationContent (Id UNMutableNotificationContent) where
  toUNNotificationContent = unsafeCastId

-- ---------- INStartCallCallCapabilityResolutionResult ----------

-- | Phantom type for @INStartCallCallCapabilityResolutionResult@.
data INStartCallCallCapabilityResolutionResult

instance IsObjCObject (Id INStartCallCallCapabilityResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartCallCallCapabilityResolutionResult"

class IsINCallCapabilityResolutionResult a => IsINStartCallCallCapabilityResolutionResult a where
  toINStartCallCallCapabilityResolutionResult :: a -> Id INStartCallCallCapabilityResolutionResult

instance IsINStartCallCallCapabilityResolutionResult (Id INStartCallCallCapabilityResolutionResult) where
  toINStartCallCallCapabilityResolutionResult = unsafeCastId

instance IsINCallCapabilityResolutionResult (Id INStartCallCallCapabilityResolutionResult) where
  toINCallCapabilityResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INStartCallCallCapabilityResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INStartCallCallCapabilityResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INStartCallCallRecordToCallBackResolutionResult ----------

-- | Phantom type for @INStartCallCallRecordToCallBackResolutionResult@.
data INStartCallCallRecordToCallBackResolutionResult

instance IsObjCObject (Id INStartCallCallRecordToCallBackResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartCallCallRecordToCallBackResolutionResult"

class IsINCallRecordResolutionResult a => IsINStartCallCallRecordToCallBackResolutionResult a where
  toINStartCallCallRecordToCallBackResolutionResult :: a -> Id INStartCallCallRecordToCallBackResolutionResult

instance IsINStartCallCallRecordToCallBackResolutionResult (Id INStartCallCallRecordToCallBackResolutionResult) where
  toINStartCallCallRecordToCallBackResolutionResult = unsafeCastId

instance IsINCallRecordResolutionResult (Id INStartCallCallRecordToCallBackResolutionResult) where
  toINCallRecordResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INStartCallCallRecordToCallBackResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INStartCallCallRecordToCallBackResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INRequestPaymentCurrencyAmountResolutionResult ----------

-- | Phantom type for @INRequestPaymentCurrencyAmountResolutionResult@.
data INRequestPaymentCurrencyAmountResolutionResult

instance IsObjCObject (Id INRequestPaymentCurrencyAmountResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRequestPaymentCurrencyAmountResolutionResult"

class IsINCurrencyAmountResolutionResult a => IsINRequestPaymentCurrencyAmountResolutionResult a where
  toINRequestPaymentCurrencyAmountResolutionResult :: a -> Id INRequestPaymentCurrencyAmountResolutionResult

instance IsINRequestPaymentCurrencyAmountResolutionResult (Id INRequestPaymentCurrencyAmountResolutionResult) where
  toINRequestPaymentCurrencyAmountResolutionResult = unsafeCastId

instance IsINCurrencyAmountResolutionResult (Id INRequestPaymentCurrencyAmountResolutionResult) where
  toINCurrencyAmountResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INRequestPaymentCurrencyAmountResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INRequestPaymentCurrencyAmountResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INSendPaymentCurrencyAmountResolutionResult ----------

-- | Phantom type for @INSendPaymentCurrencyAmountResolutionResult@.
data INSendPaymentCurrencyAmountResolutionResult

instance IsObjCObject (Id INSendPaymentCurrencyAmountResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSendPaymentCurrencyAmountResolutionResult"

class IsINCurrencyAmountResolutionResult a => IsINSendPaymentCurrencyAmountResolutionResult a where
  toINSendPaymentCurrencyAmountResolutionResult :: a -> Id INSendPaymentCurrencyAmountResolutionResult

instance IsINSendPaymentCurrencyAmountResolutionResult (Id INSendPaymentCurrencyAmountResolutionResult) where
  toINSendPaymentCurrencyAmountResolutionResult = unsafeCastId

instance IsINCurrencyAmountResolutionResult (Id INSendPaymentCurrencyAmountResolutionResult) where
  toINCurrencyAmountResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INSendPaymentCurrencyAmountResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INSendPaymentCurrencyAmountResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INPlayMediaPlaybackSpeedResolutionResult ----------

-- | Phantom type for @INPlayMediaPlaybackSpeedResolutionResult@.
data INPlayMediaPlaybackSpeedResolutionResult

instance IsObjCObject (Id INPlayMediaPlaybackSpeedResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPlayMediaPlaybackSpeedResolutionResult"

class IsINDoubleResolutionResult a => IsINPlayMediaPlaybackSpeedResolutionResult a where
  toINPlayMediaPlaybackSpeedResolutionResult :: a -> Id INPlayMediaPlaybackSpeedResolutionResult

instance IsINPlayMediaPlaybackSpeedResolutionResult (Id INPlayMediaPlaybackSpeedResolutionResult) where
  toINPlayMediaPlaybackSpeedResolutionResult = unsafeCastId

instance IsINDoubleResolutionResult (Id INPlayMediaPlaybackSpeedResolutionResult) where
  toINDoubleResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INPlayMediaPlaybackSpeedResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsNSObject (Id INPlayMediaPlaybackSpeedResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INAddMediaMediaDestinationResolutionResult ----------

-- | Phantom type for @INAddMediaMediaDestinationResolutionResult@.
data INAddMediaMediaDestinationResolutionResult

instance IsObjCObject (Id INAddMediaMediaDestinationResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAddMediaMediaDestinationResolutionResult"

class IsINMediaDestinationResolutionResult a => IsINAddMediaMediaDestinationResolutionResult a where
  toINAddMediaMediaDestinationResolutionResult :: a -> Id INAddMediaMediaDestinationResolutionResult

instance IsINAddMediaMediaDestinationResolutionResult (Id INAddMediaMediaDestinationResolutionResult) where
  toINAddMediaMediaDestinationResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INAddMediaMediaDestinationResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINMediaDestinationResolutionResult (Id INAddMediaMediaDestinationResolutionResult) where
  toINMediaDestinationResolutionResult = unsafeCastId

instance IsNSObject (Id INAddMediaMediaDestinationResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INAddMediaMediaItemResolutionResult ----------

-- | Phantom type for @INAddMediaMediaItemResolutionResult@.
data INAddMediaMediaItemResolutionResult

instance IsObjCObject (Id INAddMediaMediaItemResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAddMediaMediaItemResolutionResult"

class IsINMediaItemResolutionResult a => IsINAddMediaMediaItemResolutionResult a where
  toINAddMediaMediaItemResolutionResult :: a -> Id INAddMediaMediaItemResolutionResult

instance IsINAddMediaMediaItemResolutionResult (Id INAddMediaMediaItemResolutionResult) where
  toINAddMediaMediaItemResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INAddMediaMediaItemResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINMediaItemResolutionResult (Id INAddMediaMediaItemResolutionResult) where
  toINMediaItemResolutionResult = unsafeCastId

instance IsNSObject (Id INAddMediaMediaItemResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INPlayMediaMediaItemResolutionResult ----------

-- | Phantom type for @INPlayMediaMediaItemResolutionResult@.
data INPlayMediaMediaItemResolutionResult

instance IsObjCObject (Id INPlayMediaMediaItemResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INPlayMediaMediaItemResolutionResult"

class IsINMediaItemResolutionResult a => IsINPlayMediaMediaItemResolutionResult a where
  toINPlayMediaMediaItemResolutionResult :: a -> Id INPlayMediaMediaItemResolutionResult

instance IsINPlayMediaMediaItemResolutionResult (Id INPlayMediaMediaItemResolutionResult) where
  toINPlayMediaMediaItemResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INPlayMediaMediaItemResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINMediaItemResolutionResult (Id INPlayMediaMediaItemResolutionResult) where
  toINMediaItemResolutionResult = unsafeCastId

instance IsNSObject (Id INPlayMediaMediaItemResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INSearchForMediaMediaItemResolutionResult ----------

-- | Phantom type for @INSearchForMediaMediaItemResolutionResult@.
data INSearchForMediaMediaItemResolutionResult

instance IsObjCObject (Id INSearchForMediaMediaItemResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSearchForMediaMediaItemResolutionResult"

class IsINMediaItemResolutionResult a => IsINSearchForMediaMediaItemResolutionResult a where
  toINSearchForMediaMediaItemResolutionResult :: a -> Id INSearchForMediaMediaItemResolutionResult

instance IsINSearchForMediaMediaItemResolutionResult (Id INSearchForMediaMediaItemResolutionResult) where
  toINSearchForMediaMediaItemResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INSearchForMediaMediaItemResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINMediaItemResolutionResult (Id INSearchForMediaMediaItemResolutionResult) where
  toINMediaItemResolutionResult = unsafeCastId

instance IsNSObject (Id INSearchForMediaMediaItemResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INUpdateMediaAffinityMediaItemResolutionResult ----------

-- | Phantom type for @INUpdateMediaAffinityMediaItemResolutionResult@.
data INUpdateMediaAffinityMediaItemResolutionResult

instance IsObjCObject (Id INUpdateMediaAffinityMediaItemResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INUpdateMediaAffinityMediaItemResolutionResult"

class IsINMediaItemResolutionResult a => IsINUpdateMediaAffinityMediaItemResolutionResult a where
  toINUpdateMediaAffinityMediaItemResolutionResult :: a -> Id INUpdateMediaAffinityMediaItemResolutionResult

instance IsINUpdateMediaAffinityMediaItemResolutionResult (Id INUpdateMediaAffinityMediaItemResolutionResult) where
  toINUpdateMediaAffinityMediaItemResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INUpdateMediaAffinityMediaItemResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINMediaItemResolutionResult (Id INUpdateMediaAffinityMediaItemResolutionResult) where
  toINMediaItemResolutionResult = unsafeCastId

instance IsNSObject (Id INUpdateMediaAffinityMediaItemResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INRequestPaymentPayerResolutionResult ----------

-- | Phantom type for @INRequestPaymentPayerResolutionResult@.
data INRequestPaymentPayerResolutionResult

instance IsObjCObject (Id INRequestPaymentPayerResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INRequestPaymentPayerResolutionResult"

class IsINPersonResolutionResult a => IsINRequestPaymentPayerResolutionResult a where
  toINRequestPaymentPayerResolutionResult :: a -> Id INRequestPaymentPayerResolutionResult

instance IsINRequestPaymentPayerResolutionResult (Id INRequestPaymentPayerResolutionResult) where
  toINRequestPaymentPayerResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INRequestPaymentPayerResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINPersonResolutionResult (Id INRequestPaymentPayerResolutionResult) where
  toINPersonResolutionResult = unsafeCastId

instance IsNSObject (Id INRequestPaymentPayerResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INSendMessageRecipientResolutionResult ----------

-- | Phantom type for @INSendMessageRecipientResolutionResult@.
data INSendMessageRecipientResolutionResult

instance IsObjCObject (Id INSendMessageRecipientResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSendMessageRecipientResolutionResult"

class IsINPersonResolutionResult a => IsINSendMessageRecipientResolutionResult a where
  toINSendMessageRecipientResolutionResult :: a -> Id INSendMessageRecipientResolutionResult

instance IsINSendMessageRecipientResolutionResult (Id INSendMessageRecipientResolutionResult) where
  toINSendMessageRecipientResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INSendMessageRecipientResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINPersonResolutionResult (Id INSendMessageRecipientResolutionResult) where
  toINPersonResolutionResult = unsafeCastId

instance IsNSObject (Id INSendMessageRecipientResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INSendPaymentPayeeResolutionResult ----------

-- | Phantom type for @INSendPaymentPayeeResolutionResult@.
data INSendPaymentPayeeResolutionResult

instance IsObjCObject (Id INSendPaymentPayeeResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSendPaymentPayeeResolutionResult"

class IsINPersonResolutionResult a => IsINSendPaymentPayeeResolutionResult a where
  toINSendPaymentPayeeResolutionResult :: a -> Id INSendPaymentPayeeResolutionResult

instance IsINSendPaymentPayeeResolutionResult (Id INSendPaymentPayeeResolutionResult) where
  toINSendPaymentPayeeResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INSendPaymentPayeeResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINPersonResolutionResult (Id INSendPaymentPayeeResolutionResult) where
  toINPersonResolutionResult = unsafeCastId

instance IsNSObject (Id INSendPaymentPayeeResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INStartCallContactResolutionResult ----------

-- | Phantom type for @INStartCallContactResolutionResult@.
data INStartCallContactResolutionResult

instance IsObjCObject (Id INStartCallContactResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INStartCallContactResolutionResult"

class IsINPersonResolutionResult a => IsINStartCallContactResolutionResult a where
  toINStartCallContactResolutionResult :: a -> Id INStartCallContactResolutionResult

instance IsINStartCallContactResolutionResult (Id INStartCallContactResolutionResult) where
  toINStartCallContactResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INStartCallContactResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINPersonResolutionResult (Id INStartCallContactResolutionResult) where
  toINPersonResolutionResult = unsafeCastId

instance IsNSObject (Id INStartCallContactResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INAddTasksTargetTaskListResolutionResult ----------

-- | Phantom type for @INAddTasksTargetTaskListResolutionResult@.
data INAddTasksTargetTaskListResolutionResult

instance IsObjCObject (Id INAddTasksTargetTaskListResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAddTasksTargetTaskListResolutionResult"

class IsINTaskListResolutionResult a => IsINAddTasksTargetTaskListResolutionResult a where
  toINAddTasksTargetTaskListResolutionResult :: a -> Id INAddTasksTargetTaskListResolutionResult

instance IsINAddTasksTargetTaskListResolutionResult (Id INAddTasksTargetTaskListResolutionResult) where
  toINAddTasksTargetTaskListResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INAddTasksTargetTaskListResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINTaskListResolutionResult (Id INAddTasksTargetTaskListResolutionResult) where
  toINTaskListResolutionResult = unsafeCastId

instance IsNSObject (Id INAddTasksTargetTaskListResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INDeleteTasksTaskListResolutionResult ----------

-- | Phantom type for @INDeleteTasksTaskListResolutionResult@.
data INDeleteTasksTaskListResolutionResult

instance IsObjCObject (Id INDeleteTasksTaskListResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INDeleteTasksTaskListResolutionResult"

class IsINTaskListResolutionResult a => IsINDeleteTasksTaskListResolutionResult a where
  toINDeleteTasksTaskListResolutionResult :: a -> Id INDeleteTasksTaskListResolutionResult

instance IsINDeleteTasksTaskListResolutionResult (Id INDeleteTasksTaskListResolutionResult) where
  toINDeleteTasksTaskListResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INDeleteTasksTaskListResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINTaskListResolutionResult (Id INDeleteTasksTaskListResolutionResult) where
  toINTaskListResolutionResult = unsafeCastId

instance IsNSObject (Id INDeleteTasksTaskListResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INDeleteTasksTaskResolutionResult ----------

-- | Phantom type for @INDeleteTasksTaskResolutionResult@.
data INDeleteTasksTaskResolutionResult

instance IsObjCObject (Id INDeleteTasksTaskResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INDeleteTasksTaskResolutionResult"

class IsINTaskResolutionResult a => IsINDeleteTasksTaskResolutionResult a where
  toINDeleteTasksTaskResolutionResult :: a -> Id INDeleteTasksTaskResolutionResult

instance IsINDeleteTasksTaskResolutionResult (Id INDeleteTasksTaskResolutionResult) where
  toINDeleteTasksTaskResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INDeleteTasksTaskResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINTaskResolutionResult (Id INDeleteTasksTaskResolutionResult) where
  toINTaskResolutionResult = unsafeCastId

instance IsNSObject (Id INDeleteTasksTaskResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INSnoozeTasksTaskResolutionResult ----------

-- | Phantom type for @INSnoozeTasksTaskResolutionResult@.
data INSnoozeTasksTaskResolutionResult

instance IsObjCObject (Id INSnoozeTasksTaskResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSnoozeTasksTaskResolutionResult"

class IsINTaskResolutionResult a => IsINSnoozeTasksTaskResolutionResult a where
  toINSnoozeTasksTaskResolutionResult :: a -> Id INSnoozeTasksTaskResolutionResult

instance IsINSnoozeTasksTaskResolutionResult (Id INSnoozeTasksTaskResolutionResult) where
  toINSnoozeTasksTaskResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INSnoozeTasksTaskResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINTaskResolutionResult (Id INSnoozeTasksTaskResolutionResult) where
  toINTaskResolutionResult = unsafeCastId

instance IsNSObject (Id INSnoozeTasksTaskResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INAddTasksTemporalEventTriggerResolutionResult ----------

-- | Phantom type for @INAddTasksTemporalEventTriggerResolutionResult@.
data INAddTasksTemporalEventTriggerResolutionResult

instance IsObjCObject (Id INAddTasksTemporalEventTriggerResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INAddTasksTemporalEventTriggerResolutionResult"

class IsINTemporalEventTriggerResolutionResult a => IsINAddTasksTemporalEventTriggerResolutionResult a where
  toINAddTasksTemporalEventTriggerResolutionResult :: a -> Id INAddTasksTemporalEventTriggerResolutionResult

instance IsINAddTasksTemporalEventTriggerResolutionResult (Id INAddTasksTemporalEventTriggerResolutionResult) where
  toINAddTasksTemporalEventTriggerResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INAddTasksTemporalEventTriggerResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINTemporalEventTriggerResolutionResult (Id INAddTasksTemporalEventTriggerResolutionResult) where
  toINTemporalEventTriggerResolutionResult = unsafeCastId

instance IsNSObject (Id INAddTasksTemporalEventTriggerResolutionResult) where
  toNSObject = unsafeCastId

-- ---------- INSetTaskAttributeTemporalEventTriggerResolutionResult ----------

-- | Phantom type for @INSetTaskAttributeTemporalEventTriggerResolutionResult@.
data INSetTaskAttributeTemporalEventTriggerResolutionResult

instance IsObjCObject (Id INSetTaskAttributeTemporalEventTriggerResolutionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INSetTaskAttributeTemporalEventTriggerResolutionResult"

class IsINTemporalEventTriggerResolutionResult a => IsINSetTaskAttributeTemporalEventTriggerResolutionResult a where
  toINSetTaskAttributeTemporalEventTriggerResolutionResult :: a -> Id INSetTaskAttributeTemporalEventTriggerResolutionResult

instance IsINSetTaskAttributeTemporalEventTriggerResolutionResult (Id INSetTaskAttributeTemporalEventTriggerResolutionResult) where
  toINSetTaskAttributeTemporalEventTriggerResolutionResult = unsafeCastId

instance IsINIntentResolutionResult (Id INSetTaskAttributeTemporalEventTriggerResolutionResult) where
  toINIntentResolutionResult = unsafeCastId

instance IsINTemporalEventTriggerResolutionResult (Id INSetTaskAttributeTemporalEventTriggerResolutionResult) where
  toINTemporalEventTriggerResolutionResult = unsafeCastId

instance IsNSObject (Id INSetTaskAttributeTemporalEventTriggerResolutionResult) where
  toNSObject = unsafeCastId
