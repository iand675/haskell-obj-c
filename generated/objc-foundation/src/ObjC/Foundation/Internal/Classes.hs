{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Foundation.Internal.Classes (
    module ObjC.Foundation.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Automator.Internal.Classes,
    module ObjC.CloudKit.Internal.Classes,
    module ObjC.DiscRecording.Internal.Classes,
    module ObjC.DiscRecordingUI.Internal.Classes,
    module ObjC.ExceptionHandling.Internal.Classes,
    module ObjC.GameplayKit.Internal.Classes,
    module ObjC.IOBluetooth.Internal.Classes,
    module ObjC.NetworkExtension.Internal.Classes,
    module ObjC.Quartz.Internal.Classes,
    module ObjC.QuartzCore.Internal.Classes,
    module ObjC.QuickLookUI.Internal.Classes,
    module ObjC.SecurityInterface.Internal.Classes,
    module ObjC.SyncServices.Internal.Classes,
    module ObjC.UniformTypeIdentifiers.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Automator.Internal.Classes
import ObjC.CloudKit.Internal.Classes
import ObjC.DiscRecording.Internal.Classes
import ObjC.DiscRecordingUI.Internal.Classes
import ObjC.ExceptionHandling.Internal.Classes
import ObjC.GameplayKit.Internal.Classes
import ObjC.IOBluetooth.Internal.Classes
import ObjC.NetworkExtension.Internal.Classes
import ObjC.Quartz.Internal.Classes
import ObjC.QuartzCore.Internal.Classes
import ObjC.QuickLookUI.Internal.Classes
import ObjC.SecurityInterface.Internal.Classes
import ObjC.SyncServices.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- ---------- NSObject ----------

-- | Phantom type for @NSObject@.
data NSObject

instance IsObjCObject (Id NSObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSObject"

class IsObjCObject a => IsNSObject a where
  toNSObject :: a -> Id NSObject

instance IsNSObject (Id NSObject) where
  toNSObject = unsafeCastId

-- ---------- NSProxy ----------

-- | Phantom type for @NSProxy@.
data NSProxy

instance IsObjCObject (Id NSProxy) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSProxy"

class IsObjCObject a => IsNSProxy a where
  toNSProxy :: a -> Id NSProxy

instance IsNSProxy (Id NSProxy) where
  toNSProxy = unsafeCastId

-- ---------- NSAffineTransform ----------

-- | Phantom type for @NSAffineTransform@.
data NSAffineTransform

instance IsObjCObject (Id NSAffineTransform) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAffineTransform"

class IsNSObject a => IsNSAffineTransform a where
  toNSAffineTransform :: a -> Id NSAffineTransform

instance IsNSAffineTransform (Id NSAffineTransform) where
  toNSAffineTransform = unsafeCastId

instance IsNSObject (Id NSAffineTransform) where
  toNSObject = unsafeCastId

-- ---------- NSAppleEventDescriptor ----------

-- | Phantom type for @NSAppleEventDescriptor@.
data NSAppleEventDescriptor

instance IsObjCObject (Id NSAppleEventDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAppleEventDescriptor"

class IsNSObject a => IsNSAppleEventDescriptor a where
  toNSAppleEventDescriptor :: a -> Id NSAppleEventDescriptor

instance IsNSAppleEventDescriptor (Id NSAppleEventDescriptor) where
  toNSAppleEventDescriptor = unsafeCastId

instance IsNSObject (Id NSAppleEventDescriptor) where
  toNSObject = unsafeCastId

-- ---------- NSAppleEventManager ----------

-- | Phantom type for @NSAppleEventManager@.
data NSAppleEventManager

instance IsObjCObject (Id NSAppleEventManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAppleEventManager"

class IsNSObject a => IsNSAppleEventManager a where
  toNSAppleEventManager :: a -> Id NSAppleEventManager

instance IsNSAppleEventManager (Id NSAppleEventManager) where
  toNSAppleEventManager = unsafeCastId

instance IsNSObject (Id NSAppleEventManager) where
  toNSObject = unsafeCastId

-- ---------- NSAppleScript ----------

-- | Phantom type for @NSAppleScript@.
data NSAppleScript

instance IsObjCObject (Id NSAppleScript) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAppleScript"

class IsNSObject a => IsNSAppleScript a where
  toNSAppleScript :: a -> Id NSAppleScript

instance IsNSAppleScript (Id NSAppleScript) where
  toNSAppleScript = unsafeCastId

instance IsNSObject (Id NSAppleScript) where
  toNSObject = unsafeCastId

-- ---------- NSArray ----------

-- | **************	Immutable Array		***************
-- 
-- Phantom type for @NSArray@.
data NSArray

instance IsObjCObject (Id NSArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSArray"

class IsNSObject a => IsNSArray a where
  toNSArray :: a -> Id NSArray

instance IsNSArray (Id NSArray) where
  toNSArray = unsafeCastId

instance IsNSObject (Id NSArray) where
  toNSObject = unsafeCastId

-- ---------- NSAssertionHandler ----------

-- | Phantom type for @NSAssertionHandler@.
data NSAssertionHandler

instance IsObjCObject (Id NSAssertionHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAssertionHandler"

class IsNSObject a => IsNSAssertionHandler a where
  toNSAssertionHandler :: a -> Id NSAssertionHandler

instance IsNSAssertionHandler (Id NSAssertionHandler) where
  toNSAssertionHandler = unsafeCastId

instance IsNSObject (Id NSAssertionHandler) where
  toNSObject = unsafeCastId

-- ---------- NSAttributedString ----------

-- | Phantom type for @NSAttributedString@.
data NSAttributedString

instance IsObjCObject (Id NSAttributedString) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAttributedString"

class IsNSObject a => IsNSAttributedString a where
  toNSAttributedString :: a -> Id NSAttributedString

instance IsNSAttributedString (Id NSAttributedString) where
  toNSAttributedString = unsafeCastId

instance IsNSObject (Id NSAttributedString) where
  toNSObject = unsafeCastId

-- ---------- NSAttributedStringMarkdownParsingOptions ----------

-- | Phantom type for @NSAttributedStringMarkdownParsingOptions@.
data NSAttributedStringMarkdownParsingOptions

instance IsObjCObject (Id NSAttributedStringMarkdownParsingOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAttributedStringMarkdownParsingOptions"

class IsNSObject a => IsNSAttributedStringMarkdownParsingOptions a where
  toNSAttributedStringMarkdownParsingOptions :: a -> Id NSAttributedStringMarkdownParsingOptions

instance IsNSAttributedStringMarkdownParsingOptions (Id NSAttributedStringMarkdownParsingOptions) where
  toNSAttributedStringMarkdownParsingOptions = unsafeCastId

instance IsNSObject (Id NSAttributedStringMarkdownParsingOptions) where
  toNSObject = unsafeCastId

-- ---------- NSAttributedStringMarkdownSourcePosition ----------

-- | Phantom type for @NSAttributedStringMarkdownSourcePosition@.
data NSAttributedStringMarkdownSourcePosition

instance IsObjCObject (Id NSAttributedStringMarkdownSourcePosition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAttributedStringMarkdownSourcePosition"

class IsNSObject a => IsNSAttributedStringMarkdownSourcePosition a where
  toNSAttributedStringMarkdownSourcePosition :: a -> Id NSAttributedStringMarkdownSourcePosition

instance IsNSAttributedStringMarkdownSourcePosition (Id NSAttributedStringMarkdownSourcePosition) where
  toNSAttributedStringMarkdownSourcePosition = unsafeCastId

instance IsNSObject (Id NSAttributedStringMarkdownSourcePosition) where
  toNSObject = unsafeCastId

-- ---------- NSAutoreleasePool ----------

-- | Phantom type for @NSAutoreleasePool@.
data NSAutoreleasePool

instance IsObjCObject (Id NSAutoreleasePool) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAutoreleasePool"

class IsNSObject a => IsNSAutoreleasePool a where
  toNSAutoreleasePool :: a -> Id NSAutoreleasePool

instance IsNSAutoreleasePool (Id NSAutoreleasePool) where
  toNSAutoreleasePool = unsafeCastId

instance IsNSObject (Id NSAutoreleasePool) where
  toNSObject = unsafeCastId

-- ---------- NSBackgroundActivityScheduler ----------

-- | Phantom type for @NSBackgroundActivityScheduler@.
data NSBackgroundActivityScheduler

instance IsObjCObject (Id NSBackgroundActivityScheduler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBackgroundActivityScheduler"

class IsNSObject a => IsNSBackgroundActivityScheduler a where
  toNSBackgroundActivityScheduler :: a -> Id NSBackgroundActivityScheduler

instance IsNSBackgroundActivityScheduler (Id NSBackgroundActivityScheduler) where
  toNSBackgroundActivityScheduler = unsafeCastId

instance IsNSObject (Id NSBackgroundActivityScheduler) where
  toNSObject = unsafeCastId

-- ---------- NSBundle ----------

-- | Phantom type for @NSBundle@.
data NSBundle

instance IsObjCObject (Id NSBundle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBundle"

class IsNSObject a => IsNSBundle a where
  toNSBundle :: a -> Id NSBundle

instance IsNSBundle (Id NSBundle) where
  toNSBundle = unsafeCastId

instance IsNSObject (Id NSBundle) where
  toNSObject = unsafeCastId

-- ---------- NSBundleResourceRequest ----------

-- | Phantom type for @NSBundleResourceRequest@.
data NSBundleResourceRequest

instance IsObjCObject (Id NSBundleResourceRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBundleResourceRequest"

class IsNSObject a => IsNSBundleResourceRequest a where
  toNSBundleResourceRequest :: a -> Id NSBundleResourceRequest

instance IsNSBundleResourceRequest (Id NSBundleResourceRequest) where
  toNSBundleResourceRequest = unsafeCastId

instance IsNSObject (Id NSBundleResourceRequest) where
  toNSObject = unsafeCastId

-- ---------- NSCache ----------

-- | Phantom type for @NSCache@.
data NSCache

instance IsObjCObject (Id NSCache) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCache"

class IsNSObject a => IsNSCache a where
  toNSCache :: a -> Id NSCache

instance IsNSCache (Id NSCache) where
  toNSCache = unsafeCastId

instance IsNSObject (Id NSCache) where
  toNSObject = unsafeCastId

-- ---------- NSCachedURLResponse ----------

-- | NSCachedURLResponse
--
-- NSCachedURLResponse is a class whose objects functions as a wrapper for    objects that are stored in the framework's caching system.     It is used to maintain characteristics and attributes of a cached     object.
-- 
-- Phantom type for @NSCachedURLResponse@.
data NSCachedURLResponse

instance IsObjCObject (Id NSCachedURLResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCachedURLResponse"

class IsNSObject a => IsNSCachedURLResponse a where
  toNSCachedURLResponse :: a -> Id NSCachedURLResponse

instance IsNSCachedURLResponse (Id NSCachedURLResponse) where
  toNSCachedURLResponse = unsafeCastId

instance IsNSObject (Id NSCachedURLResponse) where
  toNSObject = unsafeCastId

-- ---------- NSCalendar ----------

-- | Phantom type for @NSCalendar@.
data NSCalendar

instance IsObjCObject (Id NSCalendar) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCalendar"

class IsNSObject a => IsNSCalendar a where
  toNSCalendar :: a -> Id NSCalendar

instance IsNSCalendar (Id NSCalendar) where
  toNSCalendar = unsafeCastId

instance IsNSObject (Id NSCalendar) where
  toNSObject = unsafeCastId

-- ---------- NSCharacterSet ----------

-- | Phantom type for @NSCharacterSet@.
data NSCharacterSet

instance IsObjCObject (Id NSCharacterSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCharacterSet"

class IsNSObject a => IsNSCharacterSet a where
  toNSCharacterSet :: a -> Id NSCharacterSet

instance IsNSCharacterSet (Id NSCharacterSet) where
  toNSCharacterSet = unsafeCastId

instance IsNSObject (Id NSCharacterSet) where
  toNSObject = unsafeCastId

-- ---------- NSClassDescription ----------

-- | Phantom type for @NSClassDescription@.
data NSClassDescription

instance IsObjCObject (Id NSClassDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSClassDescription"

class IsNSObject a => IsNSClassDescription a where
  toNSClassDescription :: a -> Id NSClassDescription

instance IsNSClassDescription (Id NSClassDescription) where
  toNSClassDescription = unsafeCastId

instance IsNSObject (Id NSClassDescription) where
  toNSObject = unsafeCastId

-- ---------- NSCoder ----------

-- | Phantom type for @NSCoder@.
data NSCoder

instance IsObjCObject (Id NSCoder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCoder"

class IsNSObject a => IsNSCoder a where
  toNSCoder :: a -> Id NSCoder

instance IsNSCoder (Id NSCoder) where
  toNSCoder = unsafeCastId

instance IsNSObject (Id NSCoder) where
  toNSObject = unsafeCastId

-- ---------- NSCondition ----------

-- | Phantom type for @NSCondition@.
data NSCondition

instance IsObjCObject (Id NSCondition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCondition"

class IsNSObject a => IsNSCondition a where
  toNSCondition :: a -> Id NSCondition

instance IsNSCondition (Id NSCondition) where
  toNSCondition = unsafeCastId

instance IsNSObject (Id NSCondition) where
  toNSObject = unsafeCastId

-- ---------- NSConditionLock ----------

-- | Phantom type for @NSConditionLock@.
data NSConditionLock

instance IsObjCObject (Id NSConditionLock) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSConditionLock"

class IsNSObject a => IsNSConditionLock a where
  toNSConditionLock :: a -> Id NSConditionLock

instance IsNSConditionLock (Id NSConditionLock) where
  toNSConditionLock = unsafeCastId

instance IsNSObject (Id NSConditionLock) where
  toNSObject = unsafeCastId

-- ---------- NSConnection ----------

-- | Phantom type for @NSConnection@.
data NSConnection

instance IsObjCObject (Id NSConnection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSConnection"

class IsNSObject a => IsNSConnection a where
  toNSConnection :: a -> Id NSConnection

instance IsNSConnection (Id NSConnection) where
  toNSConnection = unsafeCastId

instance IsNSObject (Id NSConnection) where
  toNSObject = unsafeCastId

-- ---------- NSData ----------

-- | **************	Immutable Data		***************
-- 
-- Phantom type for @NSData@.
data NSData

instance IsObjCObject (Id NSData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSData"

class IsNSObject a => IsNSData a where
  toNSData :: a -> Id NSData

instance IsNSData (Id NSData) where
  toNSData = unsafeCastId

instance IsNSObject (Id NSData) where
  toNSObject = unsafeCastId

-- ---------- NSDate ----------

-- | Phantom type for @NSDate@.
data NSDate

instance IsObjCObject (Id NSDate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDate"

class IsNSObject a => IsNSDate a where
  toNSDate :: a -> Id NSDate

instance IsNSDate (Id NSDate) where
  toNSDate = unsafeCastId

instance IsNSObject (Id NSDate) where
  toNSObject = unsafeCastId

-- ---------- NSDateComponents ----------

-- | Phantom type for @NSDateComponents@.
data NSDateComponents

instance IsObjCObject (Id NSDateComponents) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDateComponents"

class IsNSObject a => IsNSDateComponents a where
  toNSDateComponents :: a -> Id NSDateComponents

instance IsNSDateComponents (Id NSDateComponents) where
  toNSDateComponents = unsafeCastId

instance IsNSObject (Id NSDateComponents) where
  toNSObject = unsafeCastId

-- ---------- NSDateInterval ----------

-- | Phantom type for @NSDateInterval@.
data NSDateInterval

instance IsObjCObject (Id NSDateInterval) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDateInterval"

class IsNSObject a => IsNSDateInterval a where
  toNSDateInterval :: a -> Id NSDateInterval

instance IsNSDateInterval (Id NSDateInterval) where
  toNSDateInterval = unsafeCastId

instance IsNSObject (Id NSDateInterval) where
  toNSObject = unsafeCastId

-- ---------- NSDecimalNumberHandler ----------

-- | *********	A class for defining common behaviors		******
-- 
-- Phantom type for @NSDecimalNumberHandler@.
data NSDecimalNumberHandler

instance IsObjCObject (Id NSDecimalNumberHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDecimalNumberHandler"

class IsNSObject a => IsNSDecimalNumberHandler a where
  toNSDecimalNumberHandler :: a -> Id NSDecimalNumberHandler

instance IsNSDecimalNumberHandler (Id NSDecimalNumberHandler) where
  toNSDecimalNumberHandler = unsafeCastId

instance IsNSObject (Id NSDecimalNumberHandler) where
  toNSObject = unsafeCastId

-- ---------- NSDictionary ----------

-- | **************	Immutable Dictionary	***************
-- 
-- Phantom type for @NSDictionary@.
data NSDictionary

instance IsObjCObject (Id NSDictionary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDictionary"

class IsNSObject a => IsNSDictionary a where
  toNSDictionary :: a -> Id NSDictionary

instance IsNSDictionary (Id NSDictionary) where
  toNSDictionary = unsafeCastId

instance IsNSObject (Id NSDictionary) where
  toNSObject = unsafeCastId

-- ---------- NSDistantObjectRequest ----------

-- | Phantom type for @NSDistantObjectRequest@.
data NSDistantObjectRequest

instance IsObjCObject (Id NSDistantObjectRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDistantObjectRequest"

class IsNSObject a => IsNSDistantObjectRequest a where
  toNSDistantObjectRequest :: a -> Id NSDistantObjectRequest

instance IsNSDistantObjectRequest (Id NSDistantObjectRequest) where
  toNSDistantObjectRequest = unsafeCastId

instance IsNSObject (Id NSDistantObjectRequest) where
  toNSObject = unsafeCastId

-- ---------- NSDistributedLock ----------

-- | Phantom type for @NSDistributedLock@.
data NSDistributedLock

instance IsObjCObject (Id NSDistributedLock) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDistributedLock"

class IsNSObject a => IsNSDistributedLock a where
  toNSDistributedLock :: a -> Id NSDistributedLock

instance IsNSDistributedLock (Id NSDistributedLock) where
  toNSDistributedLock = unsafeCastId

instance IsNSObject (Id NSDistributedLock) where
  toNSObject = unsafeCastId

-- ---------- NSEnumerator ----------

-- | Phantom type for @NSEnumerator@.
data NSEnumerator

instance IsObjCObject (Id NSEnumerator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSEnumerator"

class IsNSObject a => IsNSEnumerator a where
  toNSEnumerator :: a -> Id NSEnumerator

instance IsNSEnumerator (Id NSEnumerator) where
  toNSEnumerator = unsafeCastId

instance IsNSObject (Id NSEnumerator) where
  toNSObject = unsafeCastId

-- ---------- NSError ----------

-- | Phantom type for @NSError@.
data NSError

instance IsObjCObject (Id NSError) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSError"

class IsNSObject a => IsNSError a where
  toNSError :: a -> Id NSError

instance IsNSError (Id NSError) where
  toNSError = unsafeCastId

instance IsNSObject (Id NSError) where
  toNSObject = unsafeCastId

-- ---------- NSException ----------

-- | Phantom type for @NSException@.
data NSException

instance IsObjCObject (Id NSException) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSException"

class IsNSObject a => IsNSException a where
  toNSException :: a -> Id NSException

instance IsNSException (Id NSException) where
  toNSException = unsafeCastId

instance IsNSObject (Id NSException) where
  toNSObject = unsafeCastId

-- ---------- NSExpression ----------

-- | Phantom type for @NSExpression@.
data NSExpression

instance IsObjCObject (Id NSExpression) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSExpression"

class IsNSObject a => IsNSExpression a where
  toNSExpression :: a -> Id NSExpression

instance IsNSExpression (Id NSExpression) where
  toNSExpression = unsafeCastId

instance IsNSObject (Id NSExpression) where
  toNSObject = unsafeCastId

-- ---------- NSExtensionContext ----------

-- | Phantom type for @NSExtensionContext@.
data NSExtensionContext

instance IsObjCObject (Id NSExtensionContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSExtensionContext"

class IsNSObject a => IsNSExtensionContext a where
  toNSExtensionContext :: a -> Id NSExtensionContext

instance IsNSExtensionContext (Id NSExtensionContext) where
  toNSExtensionContext = unsafeCastId

instance IsNSObject (Id NSExtensionContext) where
  toNSObject = unsafeCastId

-- ---------- NSExtensionItem ----------

-- | Phantom type for @NSExtensionItem@.
data NSExtensionItem

instance IsObjCObject (Id NSExtensionItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSExtensionItem"

class IsNSObject a => IsNSExtensionItem a where
  toNSExtensionItem :: a -> Id NSExtensionItem

instance IsNSExtensionItem (Id NSExtensionItem) where
  toNSExtensionItem = unsafeCastId

instance IsNSObject (Id NSExtensionItem) where
  toNSObject = unsafeCastId

-- ---------- NSFileAccessIntent ----------

-- | Phantom type for @NSFileAccessIntent@.
data NSFileAccessIntent

instance IsObjCObject (Id NSFileAccessIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileAccessIntent"

class IsNSObject a => IsNSFileAccessIntent a where
  toNSFileAccessIntent :: a -> Id NSFileAccessIntent

instance IsNSFileAccessIntent (Id NSFileAccessIntent) where
  toNSFileAccessIntent = unsafeCastId

instance IsNSObject (Id NSFileAccessIntent) where
  toNSObject = unsafeCastId

-- ---------- NSFileCoordinator ----------

-- | Phantom type for @NSFileCoordinator@.
data NSFileCoordinator

instance IsObjCObject (Id NSFileCoordinator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileCoordinator"

class IsNSObject a => IsNSFileCoordinator a where
  toNSFileCoordinator :: a -> Id NSFileCoordinator

instance IsNSFileCoordinator (Id NSFileCoordinator) where
  toNSFileCoordinator = unsafeCastId

instance IsNSObject (Id NSFileCoordinator) where
  toNSObject = unsafeCastId

-- ---------- NSFileHandle ----------

-- | Phantom type for @NSFileHandle@.
data NSFileHandle

instance IsObjCObject (Id NSFileHandle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileHandle"

class IsNSObject a => IsNSFileHandle a where
  toNSFileHandle :: a -> Id NSFileHandle

instance IsNSFileHandle (Id NSFileHandle) where
  toNSFileHandle = unsafeCastId

instance IsNSObject (Id NSFileHandle) where
  toNSObject = unsafeCastId

-- ---------- NSFileManager ----------

-- | Phantom type for @NSFileManager@.
data NSFileManager

instance IsObjCObject (Id NSFileManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileManager"

class IsNSObject a => IsNSFileManager a where
  toNSFileManager :: a -> Id NSFileManager

instance IsNSFileManager (Id NSFileManager) where
  toNSFileManager = unsafeCastId

instance IsNSObject (Id NSFileManager) where
  toNSObject = unsafeCastId

-- ---------- NSFileProviderService ----------

-- | Phantom type for @NSFileProviderService@.
data NSFileProviderService

instance IsObjCObject (Id NSFileProviderService) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileProviderService"

class IsNSObject a => IsNSFileProviderService a where
  toNSFileProviderService :: a -> Id NSFileProviderService

instance IsNSFileProviderService (Id NSFileProviderService) where
  toNSFileProviderService = unsafeCastId

instance IsNSObject (Id NSFileProviderService) where
  toNSObject = unsafeCastId

-- ---------- NSFileSecurity ----------

-- | Phantom type for @NSFileSecurity@.
data NSFileSecurity

instance IsObjCObject (Id NSFileSecurity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileSecurity"

class IsNSObject a => IsNSFileSecurity a where
  toNSFileSecurity :: a -> Id NSFileSecurity

instance IsNSFileSecurity (Id NSFileSecurity) where
  toNSFileSecurity = unsafeCastId

instance IsNSObject (Id NSFileSecurity) where
  toNSObject = unsafeCastId

-- ---------- NSFileVersion ----------

-- | Phantom type for @NSFileVersion@.
data NSFileVersion

instance IsObjCObject (Id NSFileVersion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileVersion"

class IsNSObject a => IsNSFileVersion a where
  toNSFileVersion :: a -> Id NSFileVersion

instance IsNSFileVersion (Id NSFileVersion) where
  toNSFileVersion = unsafeCastId

instance IsNSObject (Id NSFileVersion) where
  toNSObject = unsafeCastId

-- ---------- NSFileWrapper ----------

-- | Phantom type for @NSFileWrapper@.
data NSFileWrapper

instance IsObjCObject (Id NSFileWrapper) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFileWrapper"

class IsNSObject a => IsNSFileWrapper a where
  toNSFileWrapper :: a -> Id NSFileWrapper

instance IsNSFileWrapper (Id NSFileWrapper) where
  toNSFileWrapper = unsafeCastId

instance IsNSObject (Id NSFileWrapper) where
  toNSObject = unsafeCastId

-- ---------- NSFormatter ----------

-- | Phantom type for @NSFormatter@.
data NSFormatter

instance IsObjCObject (Id NSFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFormatter"

class IsNSObject a => IsNSFormatter a where
  toNSFormatter :: a -> Id NSFormatter

instance IsNSFormatter (Id NSFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSGarbageCollector ----------

-- | Phantom type for @NSGarbageCollector@.
data NSGarbageCollector

instance IsObjCObject (Id NSGarbageCollector) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGarbageCollector"

class IsNSObject a => IsNSGarbageCollector a where
  toNSGarbageCollector :: a -> Id NSGarbageCollector

instance IsNSGarbageCollector (Id NSGarbageCollector) where
  toNSGarbageCollector = unsafeCastId

instance IsNSObject (Id NSGarbageCollector) where
  toNSObject = unsafeCastId

-- ---------- NSHTTPCookie ----------

-- | NSHTTPCookie
--
-- NSHTTPCookie represents an http cookie.
--
-- A NSHTTPCookie instance represents a single http cookie. It is    an immutable object initialized from a dictionary that contains    the various cookie attributes. It has accessors to get the various    attributes of a cookie.
-- 
-- Phantom type for @NSHTTPCookie@.
data NSHTTPCookie

instance IsObjCObject (Id NSHTTPCookie) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSHTTPCookie"

class IsNSObject a => IsNSHTTPCookie a where
  toNSHTTPCookie :: a -> Id NSHTTPCookie

instance IsNSHTTPCookie (Id NSHTTPCookie) where
  toNSHTTPCookie = unsafeCastId

instance IsNSObject (Id NSHTTPCookie) where
  toNSObject = unsafeCastId

-- ---------- NSHTTPCookieStorage ----------

-- | NSHTTPCookieStorage
--
-- NSHTTPCookieStorage implements a singleton object (shared    instance) which manages the shared cookie store.  It has methods    to allow clients to set and remove cookies, and get the current    set of cookies.  It also has convenience methods to parse and    generate cookie-related HTTP header fields.
-- 
-- Phantom type for @NSHTTPCookieStorage@.
data NSHTTPCookieStorage

instance IsObjCObject (Id NSHTTPCookieStorage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSHTTPCookieStorage"

class IsNSObject a => IsNSHTTPCookieStorage a where
  toNSHTTPCookieStorage :: a -> Id NSHTTPCookieStorage

instance IsNSHTTPCookieStorage (Id NSHTTPCookieStorage) where
  toNSHTTPCookieStorage = unsafeCastId

instance IsNSObject (Id NSHTTPCookieStorage) where
  toNSObject = unsafeCastId

-- ---------- NSHashTable ----------

-- | Phantom type for @NSHashTable@.
data NSHashTable

instance IsObjCObject (Id NSHashTable) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSHashTable"

class IsNSObject a => IsNSHashTable a where
  toNSHashTable :: a -> Id NSHashTable

instance IsNSHashTable (Id NSHashTable) where
  toNSHashTable = unsafeCastId

instance IsNSObject (Id NSHashTable) where
  toNSObject = unsafeCastId

-- ---------- NSHost ----------

-- | DEPRECATION NOTICE
--
-- If you’re using @NSHost@ to resolve DNS names so that you can connect to a service, switch to a connect-by-name API, for example, @nw_connection@.
--
-- If you have other DNS resolution needs, switch to <dns_sd.h>.
-- 
-- Phantom type for @NSHost@.
data NSHost

instance IsObjCObject (Id NSHost) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSHost"

class IsNSObject a => IsNSHost a where
  toNSHost :: a -> Id NSHost

instance IsNSHost (Id NSHost) where
  toNSHost = unsafeCastId

instance IsNSObject (Id NSHost) where
  toNSObject = unsafeCastId

-- ---------- NSIndexPath ----------

-- | Phantom type for @NSIndexPath@.
data NSIndexPath

instance IsObjCObject (Id NSIndexPath) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSIndexPath"

class IsNSObject a => IsNSIndexPath a where
  toNSIndexPath :: a -> Id NSIndexPath

instance IsNSIndexPath (Id NSIndexPath) where
  toNSIndexPath = unsafeCastId

instance IsNSObject (Id NSIndexPath) where
  toNSObject = unsafeCastId

-- ---------- NSIndexSet ----------

-- | Phantom type for @NSIndexSet@.
data NSIndexSet

instance IsObjCObject (Id NSIndexSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSIndexSet"

class IsNSObject a => IsNSIndexSet a where
  toNSIndexSet :: a -> Id NSIndexSet

instance IsNSIndexSet (Id NSIndexSet) where
  toNSIndexSet = unsafeCastId

instance IsNSObject (Id NSIndexSet) where
  toNSObject = unsafeCastId

-- ---------- NSInflectionRule ----------

-- | Phantom type for @NSInflectionRule@.
data NSInflectionRule

instance IsObjCObject (Id NSInflectionRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSInflectionRule"

class IsNSObject a => IsNSInflectionRule a where
  toNSInflectionRule :: a -> Id NSInflectionRule

instance IsNSInflectionRule (Id NSInflectionRule) where
  toNSInflectionRule = unsafeCastId

instance IsNSObject (Id NSInflectionRule) where
  toNSObject = unsafeCastId

-- ---------- NSInvocation ----------

-- | Phantom type for @NSInvocation@.
data NSInvocation

instance IsObjCObject (Id NSInvocation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSInvocation"

class IsNSObject a => IsNSInvocation a where
  toNSInvocation :: a -> Id NSInvocation

instance IsNSInvocation (Id NSInvocation) where
  toNSInvocation = unsafeCastId

instance IsNSObject (Id NSInvocation) where
  toNSObject = unsafeCastId

-- ---------- NSItemProvider ----------

-- | Phantom type for @NSItemProvider@.
data NSItemProvider

instance IsObjCObject (Id NSItemProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSItemProvider"

class IsNSObject a => IsNSItemProvider a where
  toNSItemProvider :: a -> Id NSItemProvider

instance IsNSItemProvider (Id NSItemProvider) where
  toNSItemProvider = unsafeCastId

instance IsNSObject (Id NSItemProvider) where
  toNSObject = unsafeCastId

-- ---------- NSJSONSerialization ----------

-- | Phantom type for @NSJSONSerialization@.
data NSJSONSerialization

instance IsObjCObject (Id NSJSONSerialization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSJSONSerialization"

class IsNSObject a => IsNSJSONSerialization a where
  toNSJSONSerialization :: a -> Id NSJSONSerialization

instance IsNSJSONSerialization (Id NSJSONSerialization) where
  toNSJSONSerialization = unsafeCastId

instance IsNSObject (Id NSJSONSerialization) where
  toNSObject = unsafeCastId

-- ---------- NSKeyValueSharedObservers ----------

-- | A collection of key-value observations which may be registered with multiple observable objects
-- 
-- Phantom type for @NSKeyValueSharedObservers@.
data NSKeyValueSharedObservers

instance IsObjCObject (Id NSKeyValueSharedObservers) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSKeyValueSharedObservers"

class IsNSObject a => IsNSKeyValueSharedObservers a where
  toNSKeyValueSharedObservers :: a -> Id NSKeyValueSharedObservers

instance IsNSKeyValueSharedObservers (Id NSKeyValueSharedObservers) where
  toNSKeyValueSharedObservers = unsafeCastId

instance IsNSObject (Id NSKeyValueSharedObservers) where
  toNSObject = unsafeCastId

-- ---------- NSKeyValueSharedObserversSnapshot ----------

-- | A collection of key-value observations which may be registered with multiple observable objects. Create using ``-[NSKeyValueSharedObservers snapshot]``
-- 
-- Phantom type for @NSKeyValueSharedObserversSnapshot@.
data NSKeyValueSharedObserversSnapshot

instance IsObjCObject (Id NSKeyValueSharedObserversSnapshot) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSKeyValueSharedObserversSnapshot"

class IsNSObject a => IsNSKeyValueSharedObserversSnapshot a where
  toNSKeyValueSharedObserversSnapshot :: a -> Id NSKeyValueSharedObserversSnapshot

instance IsNSKeyValueSharedObserversSnapshot (Id NSKeyValueSharedObserversSnapshot) where
  toNSKeyValueSharedObserversSnapshot = unsafeCastId

instance IsNSObject (Id NSKeyValueSharedObserversSnapshot) where
  toNSObject = unsafeCastId

-- ---------- NSLinguisticTagger ----------

-- | Phantom type for @NSLinguisticTagger@.
data NSLinguisticTagger

instance IsObjCObject (Id NSLinguisticTagger) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLinguisticTagger"

class IsNSObject a => IsNSLinguisticTagger a where
  toNSLinguisticTagger :: a -> Id NSLinguisticTagger

instance IsNSLinguisticTagger (Id NSLinguisticTagger) where
  toNSLinguisticTagger = unsafeCastId

instance IsNSObject (Id NSLinguisticTagger) where
  toNSObject = unsafeCastId

-- ---------- NSLocale ----------

-- | Phantom type for @NSLocale@.
data NSLocale

instance IsObjCObject (Id NSLocale) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLocale"

class IsNSObject a => IsNSLocale a where
  toNSLocale :: a -> Id NSLocale

instance IsNSLocale (Id NSLocale) where
  toNSLocale = unsafeCastId

instance IsNSObject (Id NSLocale) where
  toNSObject = unsafeCastId

-- ---------- NSLocalizedNumberFormatRule ----------

-- | Phantom type for @NSLocalizedNumberFormatRule@.
data NSLocalizedNumberFormatRule

instance IsObjCObject (Id NSLocalizedNumberFormatRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLocalizedNumberFormatRule"

class IsNSObject a => IsNSLocalizedNumberFormatRule a where
  toNSLocalizedNumberFormatRule :: a -> Id NSLocalizedNumberFormatRule

instance IsNSLocalizedNumberFormatRule (Id NSLocalizedNumberFormatRule) where
  toNSLocalizedNumberFormatRule = unsafeCastId

instance IsNSObject (Id NSLocalizedNumberFormatRule) where
  toNSObject = unsafeCastId

-- ---------- NSLock ----------

-- | Phantom type for @NSLock@.
data NSLock

instance IsObjCObject (Id NSLock) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLock"

class IsNSObject a => IsNSLock a where
  toNSLock :: a -> Id NSLock

instance IsNSLock (Id NSLock) where
  toNSLock = unsafeCastId

instance IsNSObject (Id NSLock) where
  toNSObject = unsafeCastId

-- ---------- NSMapTable ----------

-- | Phantom type for @NSMapTable@.
data NSMapTable

instance IsObjCObject (Id NSMapTable) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMapTable"

class IsNSObject a => IsNSMapTable a where
  toNSMapTable :: a -> Id NSMapTable

instance IsNSMapTable (Id NSMapTable) where
  toNSMapTable = unsafeCastId

instance IsNSObject (Id NSMapTable) where
  toNSObject = unsafeCastId

-- ---------- NSMeasurement ----------

-- | Phantom type for @NSMeasurement@.
data NSMeasurement

instance IsObjCObject (Id NSMeasurement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMeasurement"

class IsNSObject a => IsNSMeasurement a where
  toNSMeasurement :: a -> Id NSMeasurement

instance IsNSMeasurement (Id NSMeasurement) where
  toNSMeasurement = unsafeCastId

instance IsNSObject (Id NSMeasurement) where
  toNSObject = unsafeCastId

-- ---------- NSMetadataItem ----------

-- | Phantom type for @NSMetadataItem@.
data NSMetadataItem

instance IsObjCObject (Id NSMetadataItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMetadataItem"

class IsNSObject a => IsNSMetadataItem a where
  toNSMetadataItem :: a -> Id NSMetadataItem

instance IsNSMetadataItem (Id NSMetadataItem) where
  toNSMetadataItem = unsafeCastId

instance IsNSObject (Id NSMetadataItem) where
  toNSObject = unsafeCastId

-- ---------- NSMetadataQuery ----------

-- | Phantom type for @NSMetadataQuery@.
data NSMetadataQuery

instance IsObjCObject (Id NSMetadataQuery) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMetadataQuery"

class IsNSObject a => IsNSMetadataQuery a where
  toNSMetadataQuery :: a -> Id NSMetadataQuery

instance IsNSMetadataQuery (Id NSMetadataQuery) where
  toNSMetadataQuery = unsafeCastId

instance IsNSObject (Id NSMetadataQuery) where
  toNSObject = unsafeCastId

-- ---------- NSMetadataQueryAttributeValueTuple ----------

-- | Phantom type for @NSMetadataQueryAttributeValueTuple@.
data NSMetadataQueryAttributeValueTuple

instance IsObjCObject (Id NSMetadataQueryAttributeValueTuple) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMetadataQueryAttributeValueTuple"

class IsNSObject a => IsNSMetadataQueryAttributeValueTuple a where
  toNSMetadataQueryAttributeValueTuple :: a -> Id NSMetadataQueryAttributeValueTuple

instance IsNSMetadataQueryAttributeValueTuple (Id NSMetadataQueryAttributeValueTuple) where
  toNSMetadataQueryAttributeValueTuple = unsafeCastId

instance IsNSObject (Id NSMetadataQueryAttributeValueTuple) where
  toNSObject = unsafeCastId

-- ---------- NSMetadataQueryResultGroup ----------

-- | Phantom type for @NSMetadataQueryResultGroup@.
data NSMetadataQueryResultGroup

instance IsObjCObject (Id NSMetadataQueryResultGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMetadataQueryResultGroup"

class IsNSObject a => IsNSMetadataQueryResultGroup a where
  toNSMetadataQueryResultGroup :: a -> Id NSMetadataQueryResultGroup

instance IsNSMetadataQueryResultGroup (Id NSMetadataQueryResultGroup) where
  toNSMetadataQueryResultGroup = unsafeCastId

instance IsNSObject (Id NSMetadataQueryResultGroup) where
  toNSObject = unsafeCastId

-- ---------- NSMethodSignature ----------

-- | Phantom type for @NSMethodSignature@.
data NSMethodSignature

instance IsObjCObject (Id NSMethodSignature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMethodSignature"

class IsNSObject a => IsNSMethodSignature a where
  toNSMethodSignature :: a -> Id NSMethodSignature

instance IsNSMethodSignature (Id NSMethodSignature) where
  toNSMethodSignature = unsafeCastId

instance IsNSObject (Id NSMethodSignature) where
  toNSObject = unsafeCastId

-- ---------- NSMorphology ----------

-- | Phantom type for @NSMorphology@.
data NSMorphology

instance IsObjCObject (Id NSMorphology) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMorphology"

class IsNSObject a => IsNSMorphology a where
  toNSMorphology :: a -> Id NSMorphology

instance IsNSMorphology (Id NSMorphology) where
  toNSMorphology = unsafeCastId

instance IsNSObject (Id NSMorphology) where
  toNSObject = unsafeCastId

-- ---------- NSMorphologyCustomPronoun ----------

-- | Phantom type for @NSMorphologyCustomPronoun@.
data NSMorphologyCustomPronoun

instance IsObjCObject (Id NSMorphologyCustomPronoun) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMorphologyCustomPronoun"

class IsNSObject a => IsNSMorphologyCustomPronoun a where
  toNSMorphologyCustomPronoun :: a -> Id NSMorphologyCustomPronoun

instance IsNSMorphologyCustomPronoun (Id NSMorphologyCustomPronoun) where
  toNSMorphologyCustomPronoun = unsafeCastId

instance IsNSObject (Id NSMorphologyCustomPronoun) where
  toNSObject = unsafeCastId

-- ---------- NSMorphologyPronoun ----------

-- | Phantom type for @NSMorphologyPronoun@.
data NSMorphologyPronoun

instance IsObjCObject (Id NSMorphologyPronoun) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMorphologyPronoun"

class IsNSObject a => IsNSMorphologyPronoun a where
  toNSMorphologyPronoun :: a -> Id NSMorphologyPronoun

instance IsNSMorphologyPronoun (Id NSMorphologyPronoun) where
  toNSMorphologyPronoun = unsafeCastId

instance IsNSObject (Id NSMorphologyPronoun) where
  toNSObject = unsafeCastId

-- ---------- NSNetService ----------

-- | Phantom type for @NSNetService@.
data NSNetService

instance IsObjCObject (Id NSNetService) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSNetService"

class IsNSObject a => IsNSNetService a where
  toNSNetService :: a -> Id NSNetService

instance IsNSNetService (Id NSNetService) where
  toNSNetService = unsafeCastId

instance IsNSObject (Id NSNetService) where
  toNSObject = unsafeCastId

-- ---------- NSNetServiceBrowser ----------

-- | Phantom type for @NSNetServiceBrowser@.
data NSNetServiceBrowser

instance IsObjCObject (Id NSNetServiceBrowser) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSNetServiceBrowser"

class IsNSObject a => IsNSNetServiceBrowser a where
  toNSNetServiceBrowser :: a -> Id NSNetServiceBrowser

instance IsNSNetServiceBrowser (Id NSNetServiceBrowser) where
  toNSNetServiceBrowser = unsafeCastId

instance IsNSObject (Id NSNetServiceBrowser) where
  toNSObject = unsafeCastId

-- ---------- NSNotification ----------

-- | **************	Notifications	***************
-- 
-- Phantom type for @NSNotification@.
data NSNotification

instance IsObjCObject (Id NSNotification) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSNotification"

class IsNSObject a => IsNSNotification a where
  toNSNotification :: a -> Id NSNotification

instance IsNSNotification (Id NSNotification) where
  toNSNotification = unsafeCastId

instance IsNSObject (Id NSNotification) where
  toNSObject = unsafeCastId

-- ---------- NSNotificationCenter ----------

-- | **************	Notification Center	***************
-- 
-- Phantom type for @NSNotificationCenter@.
data NSNotificationCenter

instance IsObjCObject (Id NSNotificationCenter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSNotificationCenter"

class IsNSObject a => IsNSNotificationCenter a where
  toNSNotificationCenter :: a -> Id NSNotificationCenter

instance IsNSNotificationCenter (Id NSNotificationCenter) where
  toNSNotificationCenter = unsafeCastId

instance IsNSObject (Id NSNotificationCenter) where
  toNSObject = unsafeCastId

-- ---------- NSNotificationQueue ----------

-- | Phantom type for @NSNotificationQueue@.
data NSNotificationQueue

instance IsObjCObject (Id NSNotificationQueue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSNotificationQueue"

class IsNSObject a => IsNSNotificationQueue a where
  toNSNotificationQueue :: a -> Id NSNotificationQueue

instance IsNSNotificationQueue (Id NSNotificationQueue) where
  toNSNotificationQueue = unsafeCastId

instance IsNSObject (Id NSNotificationQueue) where
  toNSObject = unsafeCastId

-- ---------- NSNull ----------

-- | Phantom type for @NSNull@.
data NSNull

instance IsObjCObject (Id NSNull) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSNull"

class IsNSObject a => IsNSNull a where
  toNSNull :: a -> Id NSNull

instance IsNSNull (Id NSNull) where
  toNSNull = unsafeCastId

instance IsNSObject (Id NSNull) where
  toNSObject = unsafeCastId

-- ---------- NSOperation ----------

-- | Phantom type for @NSOperation@.
data NSOperation

instance IsObjCObject (Id NSOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOperation"

class IsNSObject a => IsNSOperation a where
  toNSOperation :: a -> Id NSOperation

instance IsNSOperation (Id NSOperation) where
  toNSOperation = unsafeCastId

instance IsNSObject (Id NSOperation) where
  toNSObject = unsafeCastId

-- ---------- NSOperationQueue ----------

-- | Phantom type for @NSOperationQueue@.
data NSOperationQueue

instance IsObjCObject (Id NSOperationQueue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOperationQueue"

class IsNSObject a => IsNSOperationQueue a where
  toNSOperationQueue :: a -> Id NSOperationQueue

instance IsNSOperationQueue (Id NSOperationQueue) where
  toNSOperationQueue = unsafeCastId

instance IsNSObject (Id NSOperationQueue) where
  toNSObject = unsafeCastId

-- ---------- NSOrderedCollectionChange ----------

-- | Phantom type for @NSOrderedCollectionChange@.
data NSOrderedCollectionChange

instance IsObjCObject (Id NSOrderedCollectionChange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOrderedCollectionChange"

class IsNSObject a => IsNSOrderedCollectionChange a where
  toNSOrderedCollectionChange :: a -> Id NSOrderedCollectionChange

instance IsNSOrderedCollectionChange (Id NSOrderedCollectionChange) where
  toNSOrderedCollectionChange = unsafeCastId

instance IsNSObject (Id NSOrderedCollectionChange) where
  toNSObject = unsafeCastId

-- ---------- NSOrderedCollectionDifference ----------

-- | Phantom type for @NSOrderedCollectionDifference@.
data NSOrderedCollectionDifference

instance IsObjCObject (Id NSOrderedCollectionDifference) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOrderedCollectionDifference"

class IsNSObject a => IsNSOrderedCollectionDifference a where
  toNSOrderedCollectionDifference :: a -> Id NSOrderedCollectionDifference

instance IsNSOrderedCollectionDifference (Id NSOrderedCollectionDifference) where
  toNSOrderedCollectionDifference = unsafeCastId

instance IsNSObject (Id NSOrderedCollectionDifference) where
  toNSObject = unsafeCastId

-- ---------- NSOrderedSet ----------

-- | **************       Immutable Ordered Set   ***************
-- 
-- Phantom type for @NSOrderedSet@.
data NSOrderedSet

instance IsObjCObject (Id NSOrderedSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOrderedSet"

class IsNSObject a => IsNSOrderedSet a where
  toNSOrderedSet :: a -> Id NSOrderedSet

instance IsNSOrderedSet (Id NSOrderedSet) where
  toNSOrderedSet = unsafeCastId

instance IsNSObject (Id NSOrderedSet) where
  toNSObject = unsafeCastId

-- ---------- NSOrthography ----------

-- | Phantom type for @NSOrthography@.
data NSOrthography

instance IsObjCObject (Id NSOrthography) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOrthography"

class IsNSObject a => IsNSOrthography a where
  toNSOrthography :: a -> Id NSOrthography

instance IsNSOrthography (Id NSOrthography) where
  toNSOrthography = unsafeCastId

instance IsNSObject (Id NSOrthography) where
  toNSObject = unsafeCastId

-- ---------- NSPersonNameComponents ----------

-- | Phantom type for @NSPersonNameComponents@.
data NSPersonNameComponents

instance IsObjCObject (Id NSPersonNameComponents) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersonNameComponents"

class IsNSObject a => IsNSPersonNameComponents a where
  toNSPersonNameComponents :: a -> Id NSPersonNameComponents

instance IsNSPersonNameComponents (Id NSPersonNameComponents) where
  toNSPersonNameComponents = unsafeCastId

instance IsNSObject (Id NSPersonNameComponents) where
  toNSObject = unsafeCastId

-- ---------- NSPipe ----------

-- | Phantom type for @NSPipe@.
data NSPipe

instance IsObjCObject (Id NSPipe) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPipe"

class IsNSObject a => IsNSPipe a where
  toNSPipe :: a -> Id NSPipe

instance IsNSPipe (Id NSPipe) where
  toNSPipe = unsafeCastId

instance IsNSObject (Id NSPipe) where
  toNSObject = unsafeCastId

-- ---------- NSPointerArray ----------

-- | Phantom type for @NSPointerArray@.
data NSPointerArray

instance IsObjCObject (Id NSPointerArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPointerArray"

class IsNSObject a => IsNSPointerArray a where
  toNSPointerArray :: a -> Id NSPointerArray

instance IsNSPointerArray (Id NSPointerArray) where
  toNSPointerArray = unsafeCastId

instance IsNSObject (Id NSPointerArray) where
  toNSObject = unsafeCastId

-- ---------- NSPointerFunctions ----------

-- | Phantom type for @NSPointerFunctions@.
data NSPointerFunctions

instance IsObjCObject (Id NSPointerFunctions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPointerFunctions"

class IsNSObject a => IsNSPointerFunctions a where
  toNSPointerFunctions :: a -> Id NSPointerFunctions

instance IsNSPointerFunctions (Id NSPointerFunctions) where
  toNSPointerFunctions = unsafeCastId

instance IsNSObject (Id NSPointerFunctions) where
  toNSObject = unsafeCastId

-- ---------- NSPort ----------

-- | Phantom type for @NSPort@.
data NSPort

instance IsObjCObject (Id NSPort) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPort"

class IsNSObject a => IsNSPort a where
  toNSPort :: a -> Id NSPort

instance IsNSPort (Id NSPort) where
  toNSPort = unsafeCastId

instance IsNSObject (Id NSPort) where
  toNSObject = unsafeCastId

-- ---------- NSPortMessage ----------

-- | Phantom type for @NSPortMessage@.
data NSPortMessage

instance IsObjCObject (Id NSPortMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPortMessage"

class IsNSObject a => IsNSPortMessage a where
  toNSPortMessage :: a -> Id NSPortMessage

instance IsNSPortMessage (Id NSPortMessage) where
  toNSPortMessage = unsafeCastId

instance IsNSObject (Id NSPortMessage) where
  toNSObject = unsafeCastId

-- ---------- NSPortNameServer ----------

-- | Phantom type for @NSPortNameServer@.
data NSPortNameServer

instance IsObjCObject (Id NSPortNameServer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPortNameServer"

class IsNSObject a => IsNSPortNameServer a where
  toNSPortNameServer :: a -> Id NSPortNameServer

instance IsNSPortNameServer (Id NSPortNameServer) where
  toNSPortNameServer = unsafeCastId

instance IsNSObject (Id NSPortNameServer) where
  toNSObject = unsafeCastId

-- ---------- NSPositionalSpecifier ----------

-- | Phantom type for @NSPositionalSpecifier@.
data NSPositionalSpecifier

instance IsObjCObject (Id NSPositionalSpecifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPositionalSpecifier"

class IsNSObject a => IsNSPositionalSpecifier a where
  toNSPositionalSpecifier :: a -> Id NSPositionalSpecifier

instance IsNSPositionalSpecifier (Id NSPositionalSpecifier) where
  toNSPositionalSpecifier = unsafeCastId

instance IsNSObject (Id NSPositionalSpecifier) where
  toNSObject = unsafeCastId

-- ---------- NSPredicate ----------

-- | Phantom type for @NSPredicate@.
data NSPredicate

instance IsObjCObject (Id NSPredicate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPredicate"

class IsNSObject a => IsNSPredicate a where
  toNSPredicate :: a -> Id NSPredicate

instance IsNSPredicate (Id NSPredicate) where
  toNSPredicate = unsafeCastId

instance IsNSObject (Id NSPredicate) where
  toNSObject = unsafeCastId

-- ---------- NSPresentationIntent ----------

-- | Phantom type for @NSPresentationIntent@.
data NSPresentationIntent

instance IsObjCObject (Id NSPresentationIntent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPresentationIntent"

class IsNSObject a => IsNSPresentationIntent a where
  toNSPresentationIntent :: a -> Id NSPresentationIntent

instance IsNSPresentationIntent (Id NSPresentationIntent) where
  toNSPresentationIntent = unsafeCastId

instance IsNSObject (Id NSPresentationIntent) where
  toNSObject = unsafeCastId

-- ---------- NSProcessInfo ----------

-- | Phantom type for @NSProcessInfo@.
data NSProcessInfo

instance IsObjCObject (Id NSProcessInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSProcessInfo"

class IsNSObject a => IsNSProcessInfo a where
  toNSProcessInfo :: a -> Id NSProcessInfo

instance IsNSProcessInfo (Id NSProcessInfo) where
  toNSProcessInfo = unsafeCastId

instance IsNSObject (Id NSProcessInfo) where
  toNSObject = unsafeCastId

-- ---------- NSProgress ----------

-- | Phantom type for @NSProgress@.
data NSProgress

instance IsObjCObject (Id NSProgress) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSProgress"

class IsNSObject a => IsNSProgress a where
  toNSProgress :: a -> Id NSProgress

instance IsNSProgress (Id NSProgress) where
  toNSProgress = unsafeCastId

instance IsNSObject (Id NSProgress) where
  toNSObject = unsafeCastId

-- ---------- NSPropertyListSerialization ----------

-- | Phantom type for @NSPropertyListSerialization@.
data NSPropertyListSerialization

instance IsObjCObject (Id NSPropertyListSerialization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPropertyListSerialization"

class IsNSObject a => IsNSPropertyListSerialization a where
  toNSPropertyListSerialization :: a -> Id NSPropertyListSerialization

instance IsNSPropertyListSerialization (Id NSPropertyListSerialization) where
  toNSPropertyListSerialization = unsafeCastId

instance IsNSObject (Id NSPropertyListSerialization) where
  toNSObject = unsafeCastId

-- ---------- NSRecursiveLock ----------

-- | Phantom type for @NSRecursiveLock@.
data NSRecursiveLock

instance IsObjCObject (Id NSRecursiveLock) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSRecursiveLock"

class IsNSObject a => IsNSRecursiveLock a where
  toNSRecursiveLock :: a -> Id NSRecursiveLock

instance IsNSRecursiveLock (Id NSRecursiveLock) where
  toNSRecursiveLock = unsafeCastId

instance IsNSObject (Id NSRecursiveLock) where
  toNSObject = unsafeCastId

-- ---------- NSRegularExpression ----------

-- | Phantom type for @NSRegularExpression@.
data NSRegularExpression

instance IsObjCObject (Id NSRegularExpression) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSRegularExpression"

class IsNSObject a => IsNSRegularExpression a where
  toNSRegularExpression :: a -> Id NSRegularExpression

instance IsNSRegularExpression (Id NSRegularExpression) where
  toNSRegularExpression = unsafeCastId

instance IsNSObject (Id NSRegularExpression) where
  toNSObject = unsafeCastId

-- ---------- NSRunLoop ----------

-- | Phantom type for @NSRunLoop@.
data NSRunLoop

instance IsObjCObject (Id NSRunLoop) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSRunLoop"

class IsNSObject a => IsNSRunLoop a where
  toNSRunLoop :: a -> Id NSRunLoop

instance IsNSRunLoop (Id NSRunLoop) where
  toNSRunLoop = unsafeCastId

instance IsNSObject (Id NSRunLoop) where
  toNSObject = unsafeCastId

-- ---------- NSScanner ----------

-- | Phantom type for @NSScanner@.
data NSScanner

instance IsObjCObject (Id NSScanner) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScanner"

class IsNSObject a => IsNSScanner a where
  toNSScanner :: a -> Id NSScanner

instance IsNSScanner (Id NSScanner) where
  toNSScanner = unsafeCastId

instance IsNSObject (Id NSScanner) where
  toNSObject = unsafeCastId

-- ---------- NSScriptCoercionHandler ----------

-- | Phantom type for @NSScriptCoercionHandler@.
data NSScriptCoercionHandler

instance IsObjCObject (Id NSScriptCoercionHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScriptCoercionHandler"

class IsNSObject a => IsNSScriptCoercionHandler a where
  toNSScriptCoercionHandler :: a -> Id NSScriptCoercionHandler

instance IsNSScriptCoercionHandler (Id NSScriptCoercionHandler) where
  toNSScriptCoercionHandler = unsafeCastId

instance IsNSObject (Id NSScriptCoercionHandler) where
  toNSObject = unsafeCastId

-- ---------- NSScriptCommand ----------

-- | Phantom type for @NSScriptCommand@.
data NSScriptCommand

instance IsObjCObject (Id NSScriptCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScriptCommand"

class IsNSObject a => IsNSScriptCommand a where
  toNSScriptCommand :: a -> Id NSScriptCommand

instance IsNSScriptCommand (Id NSScriptCommand) where
  toNSScriptCommand = unsafeCastId

instance IsNSObject (Id NSScriptCommand) where
  toNSObject = unsafeCastId

-- ---------- NSScriptCommandDescription ----------

-- | Phantom type for @NSScriptCommandDescription@.
data NSScriptCommandDescription

instance IsObjCObject (Id NSScriptCommandDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScriptCommandDescription"

class IsNSObject a => IsNSScriptCommandDescription a where
  toNSScriptCommandDescription :: a -> Id NSScriptCommandDescription

instance IsNSScriptCommandDescription (Id NSScriptCommandDescription) where
  toNSScriptCommandDescription = unsafeCastId

instance IsNSObject (Id NSScriptCommandDescription) where
  toNSObject = unsafeCastId

-- ---------- NSScriptExecutionContext ----------

-- | Phantom type for @NSScriptExecutionContext@.
data NSScriptExecutionContext

instance IsObjCObject (Id NSScriptExecutionContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScriptExecutionContext"

class IsNSObject a => IsNSScriptExecutionContext a where
  toNSScriptExecutionContext :: a -> Id NSScriptExecutionContext

instance IsNSScriptExecutionContext (Id NSScriptExecutionContext) where
  toNSScriptExecutionContext = unsafeCastId

instance IsNSObject (Id NSScriptExecutionContext) where
  toNSObject = unsafeCastId

-- ---------- NSScriptObjectSpecifier ----------

-- | Phantom type for @NSScriptObjectSpecifier@.
data NSScriptObjectSpecifier

instance IsObjCObject (Id NSScriptObjectSpecifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScriptObjectSpecifier"

class IsNSObject a => IsNSScriptObjectSpecifier a where
  toNSScriptObjectSpecifier :: a -> Id NSScriptObjectSpecifier

instance IsNSScriptObjectSpecifier (Id NSScriptObjectSpecifier) where
  toNSScriptObjectSpecifier = unsafeCastId

instance IsNSObject (Id NSScriptObjectSpecifier) where
  toNSObject = unsafeCastId

-- ---------- NSScriptSuiteRegistry ----------

-- | Phantom type for @NSScriptSuiteRegistry@.
data NSScriptSuiteRegistry

instance IsObjCObject (Id NSScriptSuiteRegistry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScriptSuiteRegistry"

class IsNSObject a => IsNSScriptSuiteRegistry a where
  toNSScriptSuiteRegistry :: a -> Id NSScriptSuiteRegistry

instance IsNSScriptSuiteRegistry (Id NSScriptSuiteRegistry) where
  toNSScriptSuiteRegistry = unsafeCastId

instance IsNSObject (Id NSScriptSuiteRegistry) where
  toNSObject = unsafeCastId

-- ---------- NSScriptWhoseTest ----------

-- | Phantom type for @NSScriptWhoseTest@.
data NSScriptWhoseTest

instance IsObjCObject (Id NSScriptWhoseTest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScriptWhoseTest"

class IsNSObject a => IsNSScriptWhoseTest a where
  toNSScriptWhoseTest :: a -> Id NSScriptWhoseTest

instance IsNSScriptWhoseTest (Id NSScriptWhoseTest) where
  toNSScriptWhoseTest = unsafeCastId

instance IsNSObject (Id NSScriptWhoseTest) where
  toNSObject = unsafeCastId

-- ---------- NSSet ----------

-- | **************	Immutable Set	***************
-- 
-- Phantom type for @NSSet@.
data NSSet

instance IsObjCObject (Id NSSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSet"

class IsNSObject a => IsNSSet a where
  toNSSet :: a -> Id NSSet

instance IsNSSet (Id NSSet) where
  toNSSet = unsafeCastId

instance IsNSObject (Id NSSet) where
  toNSObject = unsafeCastId

-- ---------- NSSortDescriptor ----------

-- | Phantom type for @NSSortDescriptor@.
data NSSortDescriptor

instance IsObjCObject (Id NSSortDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSortDescriptor"

class IsNSObject a => IsNSSortDescriptor a where
  toNSSortDescriptor :: a -> Id NSSortDescriptor

instance IsNSSortDescriptor (Id NSSortDescriptor) where
  toNSSortDescriptor = unsafeCastId

instance IsNSObject (Id NSSortDescriptor) where
  toNSObject = unsafeCastId

-- ---------- NSSpellServer ----------

-- | Phantom type for @NSSpellServer@.
data NSSpellServer

instance IsObjCObject (Id NSSpellServer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSpellServer"

class IsNSObject a => IsNSSpellServer a where
  toNSSpellServer :: a -> Id NSSpellServer

instance IsNSSpellServer (Id NSSpellServer) where
  toNSSpellServer = unsafeCastId

instance IsNSObject (Id NSSpellServer) where
  toNSObject = unsafeCastId

-- ---------- NSStream ----------

-- | Phantom type for @NSStream@.
data NSStream

instance IsObjCObject (Id NSStream) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSStream"

class IsNSObject a => IsNSStream a where
  toNSStream :: a -> Id NSStream

instance IsNSStream (Id NSStream) where
  toNSStream = unsafeCastId

instance IsNSObject (Id NSStream) where
  toNSObject = unsafeCastId

-- ---------- NSString ----------

-- | Phantom type for @NSString@.
data NSString

instance IsObjCObject (Id NSString) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSString"

class IsNSObject a => IsNSString a where
  toNSString :: a -> Id NSString

instance IsNSString (Id NSString) where
  toNSString = unsafeCastId

instance IsNSObject (Id NSString) where
  toNSObject = unsafeCastId

-- ---------- NSTask ----------

-- | Phantom type for @NSTask@.
data NSTask

instance IsObjCObject (Id NSTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTask"

class IsNSObject a => IsNSTask a where
  toNSTask :: a -> Id NSTask

instance IsNSTask (Id NSTask) where
  toNSTask = unsafeCastId

instance IsNSObject (Id NSTask) where
  toNSObject = unsafeCastId

-- ---------- NSTermOfAddress ----------

-- | Phantom type for @NSTermOfAddress@.
data NSTermOfAddress

instance IsObjCObject (Id NSTermOfAddress) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTermOfAddress"

class IsNSObject a => IsNSTermOfAddress a where
  toNSTermOfAddress :: a -> Id NSTermOfAddress

instance IsNSTermOfAddress (Id NSTermOfAddress) where
  toNSTermOfAddress = unsafeCastId

instance IsNSObject (Id NSTermOfAddress) where
  toNSObject = unsafeCastId

-- ---------- NSTextCheckingResult ----------

-- | Phantom type for @NSTextCheckingResult@.
data NSTextCheckingResult

instance IsObjCObject (Id NSTextCheckingResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextCheckingResult"

class IsNSObject a => IsNSTextCheckingResult a where
  toNSTextCheckingResult :: a -> Id NSTextCheckingResult

instance IsNSTextCheckingResult (Id NSTextCheckingResult) where
  toNSTextCheckingResult = unsafeCastId

instance IsNSObject (Id NSTextCheckingResult) where
  toNSObject = unsafeCastId

-- ---------- NSThread ----------

-- | Phantom type for @NSThread@.
data NSThread

instance IsObjCObject (Id NSThread) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSThread"

class IsNSObject a => IsNSThread a where
  toNSThread :: a -> Id NSThread

instance IsNSThread (Id NSThread) where
  toNSThread = unsafeCastId

instance IsNSObject (Id NSThread) where
  toNSObject = unsafeCastId

-- ---------- NSTimeZone ----------

-- | Phantom type for @NSTimeZone@.
data NSTimeZone

instance IsObjCObject (Id NSTimeZone) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTimeZone"

class IsNSObject a => IsNSTimeZone a where
  toNSTimeZone :: a -> Id NSTimeZone

instance IsNSTimeZone (Id NSTimeZone) where
  toNSTimeZone = unsafeCastId

instance IsNSObject (Id NSTimeZone) where
  toNSObject = unsafeCastId

-- ---------- NSTimer ----------

-- | Phantom type for @NSTimer@.
data NSTimer

instance IsObjCObject (Id NSTimer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTimer"

class IsNSObject a => IsNSTimer a where
  toNSTimer :: a -> Id NSTimer

instance IsNSTimer (Id NSTimer) where
  toNSTimer = unsafeCastId

instance IsNSObject (Id NSTimer) where
  toNSObject = unsafeCastId

-- ---------- NSURL ----------

-- | Phantom type for @NSURL@.
data NSURL

instance IsObjCObject (Id NSURL) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURL"

class IsNSObject a => IsNSURL a where
  toNSURL :: a -> Id NSURL

instance IsNSURL (Id NSURL) where
  toNSURL = unsafeCastId

instance IsNSObject (Id NSURL) where
  toNSObject = unsafeCastId

-- ---------- NSURLAuthenticationChallenge ----------

-- | NSURLAuthenticationChallenge
--
-- This class represents an authentication challenge. It    provides all the information about the challenge, and has a method    to indicate when it's done.
-- 
-- Phantom type for @NSURLAuthenticationChallenge@.
data NSURLAuthenticationChallenge

instance IsObjCObject (Id NSURLAuthenticationChallenge) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLAuthenticationChallenge"

class IsNSObject a => IsNSURLAuthenticationChallenge a where
  toNSURLAuthenticationChallenge :: a -> Id NSURLAuthenticationChallenge

instance IsNSURLAuthenticationChallenge (Id NSURLAuthenticationChallenge) where
  toNSURLAuthenticationChallenge = unsafeCastId

instance IsNSObject (Id NSURLAuthenticationChallenge) where
  toNSObject = unsafeCastId

-- ---------- NSURLCache ----------

-- | Phantom type for @NSURLCache@.
data NSURLCache

instance IsObjCObject (Id NSURLCache) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLCache"

class IsNSObject a => IsNSURLCache a where
  toNSURLCache :: a -> Id NSURLCache

instance IsNSURLCache (Id NSURLCache) where
  toNSURLCache = unsafeCastId

instance IsNSObject (Id NSURLCache) where
  toNSObject = unsafeCastId

-- ---------- NSURLComponents ----------

-- | Phantom type for @NSURLComponents@.
data NSURLComponents

instance IsObjCObject (Id NSURLComponents) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLComponents"

class IsNSObject a => IsNSURLComponents a where
  toNSURLComponents :: a -> Id NSURLComponents

instance IsNSURLComponents (Id NSURLComponents) where
  toNSURLComponents = unsafeCastId

instance IsNSObject (Id NSURLComponents) where
  toNSObject = unsafeCastId

-- ---------- NSURLConnection ----------

-- | NSURLConnection
--
-- An NSURLConnection object provides support to perform        asynchronous loads of a URL request, providing data to a        client supplied delegate.
--
-- The interface for NSURLConnection is very sparse, providing        only the controls to start and cancel asynchronous loads of a        URL request.
--
-- An NSURLConnection may be used for loading of resource data        directly to memory, in which case an        NSURLConnectionDataDelegate should be supplied, or for        downloading of resource data directly to a file, in which case        an NSURLConnectionDownloadDelegate is used.  The delegate is        retained by the NSURLConnection until a terminal condition is        encountered.  These two delegates are logically subclasses of        the base protocol, NSURLConnectionDelegate.
--
-- A terminal condition produced by the loader will result in a        connection:didFailWithError: in the case of an error, or        connectionDidFinishLoading: or connectionDidFinishDownloading:        delegate message.
--
-- The -cancel message hints to the loader that a resource load        should be abandoned but does not guarantee that more delegate        messages will not be delivered.  If -cancel does cause the        load to be abandoned, the delegate will be released without        further messages.  In general, a caller should be prepared for        -cancel to have no effect, and internally ignore any delegate        callbacks until the delegate is released.
--
-- Scheduling of an NSURLConnection specifies the context in        which delegate callbacks will be made, but the actual IO may        occur on a separate thread and should be considered an        implementation detail.
--
-- When created, an NSURLConnection performs a deep-copy of the        NSURLRequest.  This copy is available through the        -originalRequest method.  As the connection performs the load,        this request may change as a result of protocol        canonicalization or due to following redirects.        -currentRequest can be used to retrieve this value.
--
-- An NSURLConnections created with the        +connectionWithRequest:delegate: or -initWithRequest:delegate:        methods are scheduled on the current runloop immediately, and        it is not necessary to send the -start message to begin the        resource load.
--
-- NSURLConnections created with        -initWithRequest:delegate:startImmediately: are not        automatically scheduled.  Use -scheduleWithRunLoop:forMode: or        -setDelegateQueue: to specify the context for delegate        callbacks, and -start to begin the load.  If you do not        explicitly schedule the connection before -start, it will be        scheduled on the current runloop and mode automatically.
--
-- The NSURLConnectionSynchronousLoading category adds        +sendSynchronousRequest:returningResponse:error, which blocks        the current thread until the resource data is available or an        error occurs.  It should be noted that using this method on an        applications main run loop may result in an unacceptably long        delay in a user interface and its use is strongly        discourage.
--
-- The NSURLConnectionQueuedLoading category implements        +sendAsynchronousRequest:queue:completionHandler, providing        similar simplicity but provides a mechanism where the current        runloop is not blocked.
--
-- Both of the immediate loading categories do not provide for        customization of resource load, and do not allow the caller to        respond to, e.g., authentication challenges.
-- 
-- Phantom type for @NSURLConnection@.
data NSURLConnection

instance IsObjCObject (Id NSURLConnection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLConnection"

class IsNSObject a => IsNSURLConnection a where
  toNSURLConnection :: a -> Id NSURLConnection

instance IsNSURLConnection (Id NSURLConnection) where
  toNSURLConnection = unsafeCastId

instance IsNSObject (Id NSURLConnection) where
  toNSObject = unsafeCastId

-- ---------- NSURLCredential ----------

-- | NSURLCredential
--
-- This class is an immutable object representing an authentication credential.  The actual type of the credential is determined by the constructor called in the categories declared below.
-- 
-- Phantom type for @NSURLCredential@.
data NSURLCredential

instance IsObjCObject (Id NSURLCredential) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLCredential"

class IsNSObject a => IsNSURLCredential a where
  toNSURLCredential :: a -> Id NSURLCredential

instance IsNSURLCredential (Id NSURLCredential) where
  toNSURLCredential = unsafeCastId

instance IsNSObject (Id NSURLCredential) where
  toNSObject = unsafeCastId

-- ---------- NSURLCredentialStorage ----------

-- | NSURLCredentialStorage
--
-- NSURLCredentialStorage implements a singleton object (shared instance) which manages the shared credentials cache. Note: Whereas in Mac OS X any application can access any credential with a persistence of NSURLCredentialPersistencePermanent provided the user gives permission, in iPhone OS an application can access only its own credentials.
-- 
-- Phantom type for @NSURLCredentialStorage@.
data NSURLCredentialStorage

instance IsObjCObject (Id NSURLCredentialStorage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLCredentialStorage"

class IsNSObject a => IsNSURLCredentialStorage a where
  toNSURLCredentialStorage :: a -> Id NSURLCredentialStorage

instance IsNSURLCredentialStorage (Id NSURLCredentialStorage) where
  toNSURLCredentialStorage = unsafeCastId

instance IsNSObject (Id NSURLCredentialStorage) where
  toNSObject = unsafeCastId

-- ---------- NSURLDownload ----------

-- | NSURLDownload
--
-- A NSURLDownload loads a request and saves the downloaded data to a file. The progress of the download    is reported via the NSURLDownloadDelegate protocol. Note: The word "download" is used to refer to the process    of loading data off a network, decoding the data if necessary and saving the data to a file.
-- 
-- Phantom type for @NSURLDownload@.
data NSURLDownload

instance IsObjCObject (Id NSURLDownload) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLDownload"

class IsNSObject a => IsNSURLDownload a where
  toNSURLDownload :: a -> Id NSURLDownload

instance IsNSURLDownload (Id NSURLDownload) where
  toNSURLDownload = unsafeCastId

instance IsNSObject (Id NSURLDownload) where
  toNSObject = unsafeCastId

-- ---------- NSURLHandle ----------

-- | Phantom type for @NSURLHandle@.
data NSURLHandle

instance IsObjCObject (Id NSURLHandle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLHandle"

class IsNSObject a => IsNSURLHandle a where
  toNSURLHandle :: a -> Id NSURLHandle

instance IsNSURLHandle (Id NSURLHandle) where
  toNSURLHandle = unsafeCastId

instance IsNSObject (Id NSURLHandle) where
  toNSObject = unsafeCastId

-- ---------- NSURLProtectionSpace ----------

-- | NSURLProtectionSpace
--
-- This class represents a protection space requiring authentication.
-- 
-- Phantom type for @NSURLProtectionSpace@.
data NSURLProtectionSpace

instance IsObjCObject (Id NSURLProtectionSpace) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLProtectionSpace"

class IsNSObject a => IsNSURLProtectionSpace a where
  toNSURLProtectionSpace :: a -> Id NSURLProtectionSpace

instance IsNSURLProtectionSpace (Id NSURLProtectionSpace) where
  toNSURLProtectionSpace = unsafeCastId

instance IsNSObject (Id NSURLProtectionSpace) where
  toNSObject = unsafeCastId

-- ---------- NSURLProtocol ----------

-- | NSURLProtocol
--
-- NSURLProtocol is an abstract class which provides the    basic structure for performing protocol-specific loading of URL    data. Concrete subclasses handle the specifics associated with one    or more protocols or URL schemes.
-- 
-- Phantom type for @NSURLProtocol@.
data NSURLProtocol

instance IsObjCObject (Id NSURLProtocol) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLProtocol"

class IsNSObject a => IsNSURLProtocol a where
  toNSURLProtocol :: a -> Id NSURLProtocol

instance IsNSURLProtocol (Id NSURLProtocol) where
  toNSURLProtocol = unsafeCastId

instance IsNSObject (Id NSURLProtocol) where
  toNSObject = unsafeCastId

-- ---------- NSURLQueryItem ----------

-- | Phantom type for @NSURLQueryItem@.
data NSURLQueryItem

instance IsObjCObject (Id NSURLQueryItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLQueryItem"

class IsNSObject a => IsNSURLQueryItem a where
  toNSURLQueryItem :: a -> Id NSURLQueryItem

instance IsNSURLQueryItem (Id NSURLQueryItem) where
  toNSURLQueryItem = unsafeCastId

instance IsNSObject (Id NSURLQueryItem) where
  toNSObject = unsafeCastId

-- ---------- NSURLRequest ----------

-- | NSURLRequest
--
-- An NSURLRequest object represents a URL load request in a    manner independent of protocol and URL scheme.
--
-- NSURLRequest encapsulates two basic data elements about    a URL load request:        The URL to load.    The policy to use when consulting the URL content cache made    available by the implementation.        In addition, NSURLRequest is designed to be extended to support    protocol-specific data by adding categories to access a property    object provided in an interface targeted at protocol implementors.        Protocol implementors should direct their attention to the    NSURLRequestExtensibility category on NSURLRequest for more    information on how to provide extensions on NSURLRequest to    support protocol-specific request information.    Clients of this API who wish to create NSURLRequest objects to    load URL content should consult the protocol-specific NSURLRequest    categories that are available. The NSHTTPURLRequest category on    NSURLRequest is an example.            Objects of this class are used to create NSURLConnection instances,    which can are used to perform the load of a URL, or as input to the    NSURLConnection class method which performs synchronous loads.
-- 
-- Phantom type for @NSURLRequest@.
data NSURLRequest

instance IsObjCObject (Id NSURLRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLRequest"

class IsNSObject a => IsNSURLRequest a where
  toNSURLRequest :: a -> Id NSURLRequest

instance IsNSURLRequest (Id NSURLRequest) where
  toNSURLRequest = unsafeCastId

instance IsNSObject (Id NSURLRequest) where
  toNSObject = unsafeCastId

-- ---------- NSURLResponse ----------

-- | NSURLResponse
--
-- An NSURLResponse object represents a URL load response in a    manner independent of protocol and URL scheme.
--
-- NSURLResponse encapsulates the metadata associated    with a URL load. Note that NSURLResponse objects do not contain    the actual bytes representing the content of a URL. See    NSURLConnection and NSURLConnectionDelegate for more information    about receiving the content data for a URL load.
-- 
-- Phantom type for @NSURLResponse@.
data NSURLResponse

instance IsObjCObject (Id NSURLResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLResponse"

class IsNSObject a => IsNSURLResponse a where
  toNSURLResponse :: a -> Id NSURLResponse

instance IsNSURLResponse (Id NSURLResponse) where
  toNSURLResponse = unsafeCastId

instance IsNSObject (Id NSURLResponse) where
  toNSObject = unsafeCastId

-- ---------- NSURLSession ----------

-- | Phantom type for @NSURLSession@.
data NSURLSession

instance IsObjCObject (Id NSURLSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLSession"

class IsNSObject a => IsNSURLSession a where
  toNSURLSession :: a -> Id NSURLSession

instance IsNSURLSession (Id NSURLSession) where
  toNSURLSession = unsafeCastId

instance IsNSObject (Id NSURLSession) where
  toNSObject = unsafeCastId

-- ---------- NSURLSessionConfiguration ----------

-- | Phantom type for @NSURLSessionConfiguration@.
data NSURLSessionConfiguration

instance IsObjCObject (Id NSURLSessionConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLSessionConfiguration"

class IsNSObject a => IsNSURLSessionConfiguration a where
  toNSURLSessionConfiguration :: a -> Id NSURLSessionConfiguration

instance IsNSURLSessionConfiguration (Id NSURLSessionConfiguration) where
  toNSURLSessionConfiguration = unsafeCastId

instance IsNSObject (Id NSURLSessionConfiguration) where
  toNSObject = unsafeCastId

-- ---------- NSURLSessionTask ----------

-- | Phantom type for @NSURLSessionTask@.
data NSURLSessionTask

instance IsObjCObject (Id NSURLSessionTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLSessionTask"

class IsNSObject a => IsNSURLSessionTask a where
  toNSURLSessionTask :: a -> Id NSURLSessionTask

instance IsNSURLSessionTask (Id NSURLSessionTask) where
  toNSURLSessionTask = unsafeCastId

instance IsNSObject (Id NSURLSessionTask) where
  toNSObject = unsafeCastId

-- ---------- NSURLSessionTaskMetrics ----------

-- | Phantom type for @NSURLSessionTaskMetrics@.
data NSURLSessionTaskMetrics

instance IsObjCObject (Id NSURLSessionTaskMetrics) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLSessionTaskMetrics"

class IsNSObject a => IsNSURLSessionTaskMetrics a where
  toNSURLSessionTaskMetrics :: a -> Id NSURLSessionTaskMetrics

instance IsNSURLSessionTaskMetrics (Id NSURLSessionTaskMetrics) where
  toNSURLSessionTaskMetrics = unsafeCastId

instance IsNSObject (Id NSURLSessionTaskMetrics) where
  toNSObject = unsafeCastId

-- ---------- NSURLSessionTaskTransactionMetrics ----------

-- | Phantom type for @NSURLSessionTaskTransactionMetrics@.
data NSURLSessionTaskTransactionMetrics

instance IsObjCObject (Id NSURLSessionTaskTransactionMetrics) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLSessionTaskTransactionMetrics"

class IsNSObject a => IsNSURLSessionTaskTransactionMetrics a where
  toNSURLSessionTaskTransactionMetrics :: a -> Id NSURLSessionTaskTransactionMetrics

instance IsNSURLSessionTaskTransactionMetrics (Id NSURLSessionTaskTransactionMetrics) where
  toNSURLSessionTaskTransactionMetrics = unsafeCastId

instance IsNSObject (Id NSURLSessionTaskTransactionMetrics) where
  toNSObject = unsafeCastId

-- ---------- NSURLSessionWebSocketMessage ----------

-- | Phantom type for @NSURLSessionWebSocketMessage@.
data NSURLSessionWebSocketMessage

instance IsObjCObject (Id NSURLSessionWebSocketMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLSessionWebSocketMessage"

class IsNSObject a => IsNSURLSessionWebSocketMessage a where
  toNSURLSessionWebSocketMessage :: a -> Id NSURLSessionWebSocketMessage

instance IsNSURLSessionWebSocketMessage (Id NSURLSessionWebSocketMessage) where
  toNSURLSessionWebSocketMessage = unsafeCastId

instance IsNSObject (Id NSURLSessionWebSocketMessage) where
  toNSObject = unsafeCastId

-- ---------- NSUUID ----------

-- | Phantom type for @NSUUID@.
data NSUUID

instance IsObjCObject (Id NSUUID) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUUID"

class IsNSObject a => IsNSUUID a where
  toNSUUID :: a -> Id NSUUID

instance IsNSUUID (Id NSUUID) where
  toNSUUID = unsafeCastId

instance IsNSObject (Id NSUUID) where
  toNSObject = unsafeCastId

-- ---------- NSUbiquitousKeyValueStore ----------

-- | Phantom type for @NSUbiquitousKeyValueStore@.
data NSUbiquitousKeyValueStore

instance IsObjCObject (Id NSUbiquitousKeyValueStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUbiquitousKeyValueStore"

class IsNSObject a => IsNSUbiquitousKeyValueStore a where
  toNSUbiquitousKeyValueStore :: a -> Id NSUbiquitousKeyValueStore

instance IsNSUbiquitousKeyValueStore (Id NSUbiquitousKeyValueStore) where
  toNSUbiquitousKeyValueStore = unsafeCastId

instance IsNSObject (Id NSUbiquitousKeyValueStore) where
  toNSObject = unsafeCastId

-- ---------- NSUndoManager ----------

-- | Phantom type for @NSUndoManager@.
data NSUndoManager

instance IsObjCObject (Id NSUndoManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUndoManager"

class IsNSObject a => IsNSUndoManager a where
  toNSUndoManager :: a -> Id NSUndoManager

instance IsNSUndoManager (Id NSUndoManager) where
  toNSUndoManager = unsafeCastId

instance IsNSObject (Id NSUndoManager) where
  toNSObject = unsafeCastId

-- ---------- NSUnit ----------

-- | Phantom type for @NSUnit@.
data NSUnit

instance IsObjCObject (Id NSUnit) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnit"

class IsNSObject a => IsNSUnit a where
  toNSUnit :: a -> Id NSUnit

instance IsNSUnit (Id NSUnit) where
  toNSUnit = unsafeCastId

instance IsNSObject (Id NSUnit) where
  toNSObject = unsafeCastId

-- ---------- NSUnitConverter ----------

-- | Phantom type for @NSUnitConverter@.
data NSUnitConverter

instance IsObjCObject (Id NSUnitConverter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitConverter"

class IsNSObject a => IsNSUnitConverter a where
  toNSUnitConverter :: a -> Id NSUnitConverter

instance IsNSUnitConverter (Id NSUnitConverter) where
  toNSUnitConverter = unsafeCastId

instance IsNSObject (Id NSUnitConverter) where
  toNSObject = unsafeCastId

-- ---------- NSUserActivity ----------

-- | Phantom type for @NSUserActivity@.
data NSUserActivity

instance IsObjCObject (Id NSUserActivity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUserActivity"

class IsNSObject a => IsNSUserActivity a where
  toNSUserActivity :: a -> Id NSUserActivity

instance IsNSUserActivity (Id NSUserActivity) where
  toNSUserActivity = unsafeCastId

instance IsNSObject (Id NSUserActivity) where
  toNSObject = unsafeCastId

-- ---------- NSUserDefaults ----------

-- | NSUserDefaults is a hierarchical persistent interprocess (optionally distributed) key-value store, optimized for storing user settings.
--
-- Hierarchical: NSUserDefaults has a list of places to look for data called the "search list". A search list is referred to by an arbitrary string called the "suite identifier" or "domain identifier". When queried, NSUserDefaults checks each entry of its search list until it finds one that contains the key in question, or has searched the whole list. The list is (note: "current host + current user" preferences are unimplemented on iOS, watchOS, and tvOS, and "any user" preferences are not generally useful for applications on those operating systems): - Managed ("forced") preferences, set by a configuration profile or via mcx from a network administrator - Commandline arguments - Preferences for the current domain, in the cloud - Preferences for the current domain, the current user, in the current host - Preferences for the current domain, the current user, in any host - Preferences added via -addSuiteNamed: - Preferences global to all apps for the current user, in the current host - Preferences global to all apps for the current user, in any host - Preferences for the current domain, for all users, in the current host - Preferences global to all apps for all users, in the current host - Preferences registered with -registerDefaults:
--
-- Persistent: Preferences stored in NSUserDefaults persist across reboots and relaunches of apps unless otherwise specified.
--
-- Interprocess: Preferences may be accessible to and modified from multiple processes simultaneously (for example between an application and an extension).
--
-- Optionally distributed (Currently only supported in Shared iPad for Students mode):  Data stored in user defaults can be made "ubiqitous", i.e. synchronized between devices via the cloud.  Ubiquitous user defaults are automatically propagated to all devices logged into the same iCloud account. When reading defaults (via -*ForKey: methods on NSUserDefaults), ubiquitous defaults are searched before local defaults. All operations on ubiquitous defaults are asynchronous, so registered defaults may be returned in place of ubiquitous defaults if downloading from iCloud hasn't finished. Ubiquitous defaults are specified in the Defaults Configuration File for an application.
--
-- Key-Value Store: NSUserDefaults stores Property List objects (NSString, NSData, NSNumber, NSDate, NSArray, and NSDictionary) identified by NSString keys, similar to an NSMutableDictionary.
--
-- Optimized for storing user settings: NSUserDefaults is intended for relatively small amounts of data, queried very frequently, and modified occasionally. Using it in other ways may be slow or use more memory than solutions more suited to those uses.
--
-- The 'App' CFPreferences functions in CoreFoundation act on the same search lists that NSUserDefaults does.
--
-- NSUserDefaults can be observed using Key-Value Observing for any key stored in it. Using NSKeyValueObservingOptionPrior to observe changes from other processes or devices will behave as though NSKeyValueObservingOptionPrior was not specified.
-- 
-- Phantom type for @NSUserDefaults@.
data NSUserDefaults

instance IsObjCObject (Id NSUserDefaults) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUserDefaults"

class IsNSObject a => IsNSUserDefaults a where
  toNSUserDefaults :: a -> Id NSUserDefaults

instance IsNSUserDefaults (Id NSUserDefaults) where
  toNSUserDefaults = unsafeCastId

instance IsNSObject (Id NSUserDefaults) where
  toNSObject = unsafeCastId

-- ---------- NSUserNotification ----------

-- | Phantom type for @NSUserNotification@.
data NSUserNotification

instance IsObjCObject (Id NSUserNotification) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUserNotification"

class IsNSObject a => IsNSUserNotification a where
  toNSUserNotification :: a -> Id NSUserNotification

instance IsNSUserNotification (Id NSUserNotification) where
  toNSUserNotification = unsafeCastId

instance IsNSObject (Id NSUserNotification) where
  toNSObject = unsafeCastId

-- ---------- NSUserNotificationAction ----------

-- | Phantom type for @NSUserNotificationAction@.
data NSUserNotificationAction

instance IsObjCObject (Id NSUserNotificationAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUserNotificationAction"

class IsNSObject a => IsNSUserNotificationAction a where
  toNSUserNotificationAction :: a -> Id NSUserNotificationAction

instance IsNSUserNotificationAction (Id NSUserNotificationAction) where
  toNSUserNotificationAction = unsafeCastId

instance IsNSObject (Id NSUserNotificationAction) where
  toNSObject = unsafeCastId

-- ---------- NSUserNotificationCenter ----------

-- | Phantom type for @NSUserNotificationCenter@.
data NSUserNotificationCenter

instance IsObjCObject (Id NSUserNotificationCenter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUserNotificationCenter"

class IsNSObject a => IsNSUserNotificationCenter a where
  toNSUserNotificationCenter :: a -> Id NSUserNotificationCenter

instance IsNSUserNotificationCenter (Id NSUserNotificationCenter) where
  toNSUserNotificationCenter = unsafeCastId

instance IsNSObject (Id NSUserNotificationCenter) where
  toNSObject = unsafeCastId

-- ---------- NSUserScriptTask ----------

-- | Phantom type for @NSUserScriptTask@.
data NSUserScriptTask

instance IsObjCObject (Id NSUserScriptTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUserScriptTask"

class IsNSObject a => IsNSUserScriptTask a where
  toNSUserScriptTask :: a -> Id NSUserScriptTask

instance IsNSUserScriptTask (Id NSUserScriptTask) where
  toNSUserScriptTask = unsafeCastId

instance IsNSObject (Id NSUserScriptTask) where
  toNSObject = unsafeCastId

-- ---------- NSValue ----------

-- | Phantom type for @NSValue@.
data NSValue

instance IsObjCObject (Id NSValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSValue"

class IsNSObject a => IsNSValue a where
  toNSValue :: a -> Id NSValue

instance IsNSValue (Id NSValue) where
  toNSValue = unsafeCastId

instance IsNSObject (Id NSValue) where
  toNSObject = unsafeCastId

-- ---------- NSValueTransformer ----------

-- | Phantom type for @NSValueTransformer@.
data NSValueTransformer

instance IsObjCObject (Id NSValueTransformer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSValueTransformer"

class IsNSObject a => IsNSValueTransformer a where
  toNSValueTransformer :: a -> Id NSValueTransformer

instance IsNSValueTransformer (Id NSValueTransformer) where
  toNSValueTransformer = unsafeCastId

instance IsNSObject (Id NSValueTransformer) where
  toNSObject = unsafeCastId

-- ---------- NSXMLNode ----------

-- | NSXMLNode
--
-- The basic unit of an XML document.
-- 
-- Phantom type for @NSXMLNode@.
data NSXMLNode

instance IsObjCObject (Id NSXMLNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSXMLNode"

class IsNSObject a => IsNSXMLNode a where
  toNSXMLNode :: a -> Id NSXMLNode

instance IsNSXMLNode (Id NSXMLNode) where
  toNSXMLNode = unsafeCastId

instance IsNSObject (Id NSXMLNode) where
  toNSObject = unsafeCastId

-- ---------- NSXMLParser ----------

-- | Phantom type for @NSXMLParser@.
data NSXMLParser

instance IsObjCObject (Id NSXMLParser) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSXMLParser"

class IsNSObject a => IsNSXMLParser a where
  toNSXMLParser :: a -> Id NSXMLParser

instance IsNSXMLParser (Id NSXMLParser) where
  toNSXMLParser = unsafeCastId

instance IsNSObject (Id NSXMLParser) where
  toNSObject = unsafeCastId

-- ---------- NSXPCConnection ----------

-- | Phantom type for @NSXPCConnection@.
data NSXPCConnection

instance IsObjCObject (Id NSXPCConnection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSXPCConnection"

class IsNSObject a => IsNSXPCConnection a where
  toNSXPCConnection :: a -> Id NSXPCConnection

instance IsNSXPCConnection (Id NSXPCConnection) where
  toNSXPCConnection = unsafeCastId

instance IsNSObject (Id NSXPCConnection) where
  toNSObject = unsafeCastId

-- ---------- NSXPCInterface ----------

-- | Phantom type for @NSXPCInterface@.
data NSXPCInterface

instance IsObjCObject (Id NSXPCInterface) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSXPCInterface"

class IsNSObject a => IsNSXPCInterface a where
  toNSXPCInterface :: a -> Id NSXPCInterface

instance IsNSXPCInterface (Id NSXPCInterface) where
  toNSXPCInterface = unsafeCastId

instance IsNSObject (Id NSXPCInterface) where
  toNSObject = unsafeCastId

-- ---------- NSXPCListener ----------

-- | Phantom type for @NSXPCListener@.
data NSXPCListener

instance IsObjCObject (Id NSXPCListener) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSXPCListener"

class IsNSObject a => IsNSXPCListener a where
  toNSXPCListener :: a -> Id NSXPCListener

instance IsNSXPCListener (Id NSXPCListener) where
  toNSXPCListener = unsafeCastId

instance IsNSObject (Id NSXPCListener) where
  toNSObject = unsafeCastId

-- ---------- NSXPCListenerEndpoint ----------

-- | Phantom type for @NSXPCListenerEndpoint@.
data NSXPCListenerEndpoint

instance IsObjCObject (Id NSXPCListenerEndpoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSXPCListenerEndpoint"

class IsNSObject a => IsNSXPCListenerEndpoint a where
  toNSXPCListenerEndpoint :: a -> Id NSXPCListenerEndpoint

instance IsNSXPCListenerEndpoint (Id NSXPCListenerEndpoint) where
  toNSXPCListenerEndpoint = unsafeCastId

instance IsNSObject (Id NSXPCListenerEndpoint) where
  toNSObject = unsafeCastId

-- ---------- OS_object ----------

-- | Phantom type for @OS_object@.
data OS_object

instance IsObjCObject (Id OS_object) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "OS_object"

class IsNSObject a => IsOS_object a where
  toOS_object :: a -> Id OS_object

instance IsOS_object (Id OS_object) where
  toOS_object = unsafeCastId

instance IsNSObject (Id OS_object) where
  toNSObject = unsafeCastId

-- ---------- NSDistantObject ----------

-- | Phantom type for @NSDistantObject@.
data NSDistantObject

instance IsObjCObject (Id NSDistantObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDistantObject"

class IsNSProxy a => IsNSDistantObject a where
  toNSDistantObject :: a -> Id NSDistantObject

instance IsNSDistantObject (Id NSDistantObject) where
  toNSDistantObject = unsafeCastId

instance IsNSProxy (Id NSDistantObject) where
  toNSProxy = unsafeCastId

-- ---------- NSProtocolChecker ----------

-- | Phantom type for @NSProtocolChecker@.
data NSProtocolChecker

instance IsObjCObject (Id NSProtocolChecker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSProtocolChecker"

class IsNSProxy a => IsNSProtocolChecker a where
  toNSProtocolChecker :: a -> Id NSProtocolChecker

instance IsNSProtocolChecker (Id NSProtocolChecker) where
  toNSProtocolChecker = unsafeCastId

instance IsNSProxy (Id NSProtocolChecker) where
  toNSProxy = unsafeCastId

-- ---------- NSMutableArray ----------

-- | **************	Mutable Array		***************
-- 
-- Phantom type for @NSMutableArray@.
data NSMutableArray

instance IsObjCObject (Id NSMutableArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMutableArray"

class IsNSArray a => IsNSMutableArray a where
  toNSMutableArray :: a -> Id NSMutableArray

instance IsNSMutableArray (Id NSMutableArray) where
  toNSMutableArray = unsafeCastId

instance IsNSArray (Id NSMutableArray) where
  toNSArray = unsafeCastId

instance IsNSObject (Id NSMutableArray) where
  toNSObject = unsafeCastId

-- ---------- NSMutableAttributedString ----------

-- | Phantom type for @NSMutableAttributedString@.
data NSMutableAttributedString

instance IsObjCObject (Id NSMutableAttributedString) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMutableAttributedString"

class IsNSAttributedString a => IsNSMutableAttributedString a where
  toNSMutableAttributedString :: a -> Id NSMutableAttributedString

instance IsNSMutableAttributedString (Id NSMutableAttributedString) where
  toNSMutableAttributedString = unsafeCastId

instance IsNSAttributedString (Id NSMutableAttributedString) where
  toNSAttributedString = unsafeCastId

instance IsNSObject (Id NSMutableAttributedString) where
  toNSObject = unsafeCastId

-- ---------- NSMutableCharacterSet ----------

-- | Phantom type for @NSMutableCharacterSet@.
data NSMutableCharacterSet

instance IsObjCObject (Id NSMutableCharacterSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMutableCharacterSet"

class IsNSCharacterSet a => IsNSMutableCharacterSet a where
  toNSMutableCharacterSet :: a -> Id NSMutableCharacterSet

instance IsNSMutableCharacterSet (Id NSMutableCharacterSet) where
  toNSMutableCharacterSet = unsafeCastId

instance IsNSCharacterSet (Id NSMutableCharacterSet) where
  toNSCharacterSet = unsafeCastId

instance IsNSObject (Id NSMutableCharacterSet) where
  toNSObject = unsafeCastId

-- ---------- NSScriptClassDescription ----------

-- | Phantom type for @NSScriptClassDescription@.
data NSScriptClassDescription

instance IsObjCObject (Id NSScriptClassDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScriptClassDescription"

class IsNSClassDescription a => IsNSScriptClassDescription a where
  toNSScriptClassDescription :: a -> Id NSScriptClassDescription

instance IsNSScriptClassDescription (Id NSScriptClassDescription) where
  toNSScriptClassDescription = unsafeCastId

instance IsNSClassDescription (Id NSScriptClassDescription) where
  toNSClassDescription = unsafeCastId

instance IsNSObject (Id NSScriptClassDescription) where
  toNSObject = unsafeCastId

-- ---------- NSArchiver ----------

-- | **********		Archiving: Writing	***************
-- 
-- Phantom type for @NSArchiver@.
data NSArchiver

instance IsObjCObject (Id NSArchiver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSArchiver"

class IsNSCoder a => IsNSArchiver a where
  toNSArchiver :: a -> Id NSArchiver

instance IsNSArchiver (Id NSArchiver) where
  toNSArchiver = unsafeCastId

instance IsNSCoder (Id NSArchiver) where
  toNSCoder = unsafeCastId

instance IsNSObject (Id NSArchiver) where
  toNSObject = unsafeCastId

-- ---------- NSKeyedArchiver ----------

-- | Phantom type for @NSKeyedArchiver@.
data NSKeyedArchiver

instance IsObjCObject (Id NSKeyedArchiver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSKeyedArchiver"

class IsNSCoder a => IsNSKeyedArchiver a where
  toNSKeyedArchiver :: a -> Id NSKeyedArchiver

instance IsNSKeyedArchiver (Id NSKeyedArchiver) where
  toNSKeyedArchiver = unsafeCastId

instance IsNSCoder (Id NSKeyedArchiver) where
  toNSCoder = unsafeCastId

instance IsNSObject (Id NSKeyedArchiver) where
  toNSObject = unsafeCastId

-- ---------- NSKeyedUnarchiver ----------

-- | Phantom type for @NSKeyedUnarchiver@.
data NSKeyedUnarchiver

instance IsObjCObject (Id NSKeyedUnarchiver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSKeyedUnarchiver"

class IsNSCoder a => IsNSKeyedUnarchiver a where
  toNSKeyedUnarchiver :: a -> Id NSKeyedUnarchiver

instance IsNSKeyedUnarchiver (Id NSKeyedUnarchiver) where
  toNSKeyedUnarchiver = unsafeCastId

instance IsNSCoder (Id NSKeyedUnarchiver) where
  toNSCoder = unsafeCastId

instance IsNSObject (Id NSKeyedUnarchiver) where
  toNSObject = unsafeCastId

-- ---------- NSPortCoder ----------

-- | Phantom type for @NSPortCoder@.
data NSPortCoder

instance IsObjCObject (Id NSPortCoder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPortCoder"

class IsNSCoder a => IsNSPortCoder a where
  toNSPortCoder :: a -> Id NSPortCoder

instance IsNSPortCoder (Id NSPortCoder) where
  toNSPortCoder = unsafeCastId

instance IsNSCoder (Id NSPortCoder) where
  toNSCoder = unsafeCastId

instance IsNSObject (Id NSPortCoder) where
  toNSObject = unsafeCastId

-- ---------- NSUnarchiver ----------

-- | **********		Archiving: Reading		***************
-- 
-- Phantom type for @NSUnarchiver@.
data NSUnarchiver

instance IsObjCObject (Id NSUnarchiver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnarchiver"

class IsNSCoder a => IsNSUnarchiver a where
  toNSUnarchiver :: a -> Id NSUnarchiver

instance IsNSUnarchiver (Id NSUnarchiver) where
  toNSUnarchiver = unsafeCastId

instance IsNSCoder (Id NSUnarchiver) where
  toNSCoder = unsafeCastId

instance IsNSObject (Id NSUnarchiver) where
  toNSObject = unsafeCastId

-- ---------- NSXPCCoder ----------

-- | Phantom type for @NSXPCCoder@.
data NSXPCCoder

instance IsObjCObject (Id NSXPCCoder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSXPCCoder"

class IsNSCoder a => IsNSXPCCoder a where
  toNSXPCCoder :: a -> Id NSXPCCoder

instance IsNSXPCCoder (Id NSXPCCoder) where
  toNSXPCCoder = unsafeCastId

instance IsNSCoder (Id NSXPCCoder) where
  toNSCoder = unsafeCastId

instance IsNSObject (Id NSXPCCoder) where
  toNSObject = unsafeCastId

-- ---------- NSMutableData ----------

-- | **************	Mutable Data		***************
-- 
-- Phantom type for @NSMutableData@.
data NSMutableData

instance IsObjCObject (Id NSMutableData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMutableData"

class IsNSData a => IsNSMutableData a where
  toNSMutableData :: a -> Id NSMutableData

instance IsNSMutableData (Id NSMutableData) where
  toNSMutableData = unsafeCastId

instance IsNSData (Id NSMutableData) where
  toNSData = unsafeCastId

instance IsNSObject (Id NSMutableData) where
  toNSObject = unsafeCastId

-- ---------- NSCalendarDate ----------

-- | Phantom type for @NSCalendarDate@.
data NSCalendarDate

instance IsObjCObject (Id NSCalendarDate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCalendarDate"

class IsNSDate a => IsNSCalendarDate a where
  toNSCalendarDate :: a -> Id NSCalendarDate

instance IsNSCalendarDate (Id NSCalendarDate) where
  toNSCalendarDate = unsafeCastId

instance IsNSDate (Id NSCalendarDate) where
  toNSDate = unsafeCastId

instance IsNSObject (Id NSCalendarDate) where
  toNSObject = unsafeCastId

-- ---------- NSMutableDictionary ----------

-- | **************	Mutable Dictionary	***************
-- 
-- Phantom type for @NSMutableDictionary@.
data NSMutableDictionary

instance IsObjCObject (Id NSMutableDictionary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMutableDictionary"

class IsNSDictionary a => IsNSMutableDictionary a where
  toNSMutableDictionary :: a -> Id NSMutableDictionary

instance IsNSMutableDictionary (Id NSMutableDictionary) where
  toNSMutableDictionary = unsafeCastId

instance IsNSDictionary (Id NSMutableDictionary) where
  toNSDictionary = unsafeCastId

instance IsNSObject (Id NSMutableDictionary) where
  toNSObject = unsafeCastId

-- ---------- NSDirectoryEnumerator ----------

-- | Phantom type for @NSDirectoryEnumerator@.
data NSDirectoryEnumerator

instance IsObjCObject (Id NSDirectoryEnumerator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDirectoryEnumerator"

class IsNSEnumerator a => IsNSDirectoryEnumerator a where
  toNSDirectoryEnumerator :: a -> Id NSDirectoryEnumerator

instance IsNSDirectoryEnumerator (Id NSDirectoryEnumerator) where
  toNSDirectoryEnumerator = unsafeCastId

instance IsNSEnumerator (Id NSDirectoryEnumerator) where
  toNSEnumerator = unsafeCastId

instance IsNSObject (Id NSDirectoryEnumerator) where
  toNSObject = unsafeCastId

-- ---------- NSByteCountFormatter ----------

-- | Phantom type for @NSByteCountFormatter@.
data NSByteCountFormatter

instance IsObjCObject (Id NSByteCountFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSByteCountFormatter"

class IsNSFormatter a => IsNSByteCountFormatter a where
  toNSByteCountFormatter :: a -> Id NSByteCountFormatter

instance IsNSByteCountFormatter (Id NSByteCountFormatter) where
  toNSByteCountFormatter = unsafeCastId

instance IsNSFormatter (Id NSByteCountFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSByteCountFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSDateComponentsFormatter ----------

-- | Phantom type for @NSDateComponentsFormatter@.
data NSDateComponentsFormatter

instance IsObjCObject (Id NSDateComponentsFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDateComponentsFormatter"

class IsNSFormatter a => IsNSDateComponentsFormatter a where
  toNSDateComponentsFormatter :: a -> Id NSDateComponentsFormatter

instance IsNSDateComponentsFormatter (Id NSDateComponentsFormatter) where
  toNSDateComponentsFormatter = unsafeCastId

instance IsNSFormatter (Id NSDateComponentsFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSDateComponentsFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSDateFormatter ----------

-- | Phantom type for @NSDateFormatter@.
data NSDateFormatter

instance IsObjCObject (Id NSDateFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDateFormatter"

class IsNSFormatter a => IsNSDateFormatter a where
  toNSDateFormatter :: a -> Id NSDateFormatter

instance IsNSDateFormatter (Id NSDateFormatter) where
  toNSDateFormatter = unsafeCastId

instance IsNSFormatter (Id NSDateFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSDateFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSDateIntervalFormatter ----------

-- | Phantom type for @NSDateIntervalFormatter@.
data NSDateIntervalFormatter

instance IsObjCObject (Id NSDateIntervalFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDateIntervalFormatter"

class IsNSFormatter a => IsNSDateIntervalFormatter a where
  toNSDateIntervalFormatter :: a -> Id NSDateIntervalFormatter

instance IsNSDateIntervalFormatter (Id NSDateIntervalFormatter) where
  toNSDateIntervalFormatter = unsafeCastId

instance IsNSFormatter (Id NSDateIntervalFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSDateIntervalFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSEnergyFormatter ----------

-- | Phantom type for @NSEnergyFormatter@.
data NSEnergyFormatter

instance IsObjCObject (Id NSEnergyFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSEnergyFormatter"

class IsNSFormatter a => IsNSEnergyFormatter a where
  toNSEnergyFormatter :: a -> Id NSEnergyFormatter

instance IsNSEnergyFormatter (Id NSEnergyFormatter) where
  toNSEnergyFormatter = unsafeCastId

instance IsNSFormatter (Id NSEnergyFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSEnergyFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSISO8601DateFormatter ----------

-- | Phantom type for @NSISO8601DateFormatter@.
data NSISO8601DateFormatter

instance IsObjCObject (Id NSISO8601DateFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSISO8601DateFormatter"

class IsNSFormatter a => IsNSISO8601DateFormatter a where
  toNSISO8601DateFormatter :: a -> Id NSISO8601DateFormatter

instance IsNSISO8601DateFormatter (Id NSISO8601DateFormatter) where
  toNSISO8601DateFormatter = unsafeCastId

instance IsNSFormatter (Id NSISO8601DateFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSISO8601DateFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSLengthFormatter ----------

-- | Phantom type for @NSLengthFormatter@.
data NSLengthFormatter

instance IsObjCObject (Id NSLengthFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLengthFormatter"

class IsNSFormatter a => IsNSLengthFormatter a where
  toNSLengthFormatter :: a -> Id NSLengthFormatter

instance IsNSLengthFormatter (Id NSLengthFormatter) where
  toNSLengthFormatter = unsafeCastId

instance IsNSFormatter (Id NSLengthFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSLengthFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSListFormatter ----------

-- | Phantom type for @NSListFormatter@.
data NSListFormatter

instance IsObjCObject (Id NSListFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSListFormatter"

class IsNSFormatter a => IsNSListFormatter a where
  toNSListFormatter :: a -> Id NSListFormatter

instance IsNSListFormatter (Id NSListFormatter) where
  toNSListFormatter = unsafeCastId

instance IsNSFormatter (Id NSListFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSListFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSMassFormatter ----------

-- | Phantom type for @NSMassFormatter@.
data NSMassFormatter

instance IsObjCObject (Id NSMassFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMassFormatter"

class IsNSFormatter a => IsNSMassFormatter a where
  toNSMassFormatter :: a -> Id NSMassFormatter

instance IsNSMassFormatter (Id NSMassFormatter) where
  toNSMassFormatter = unsafeCastId

instance IsNSFormatter (Id NSMassFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSMassFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSMeasurementFormatter ----------

-- | Phantom type for @NSMeasurementFormatter@.
data NSMeasurementFormatter

instance IsObjCObject (Id NSMeasurementFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMeasurementFormatter"

class IsNSFormatter a => IsNSMeasurementFormatter a where
  toNSMeasurementFormatter :: a -> Id NSMeasurementFormatter

instance IsNSMeasurementFormatter (Id NSMeasurementFormatter) where
  toNSMeasurementFormatter = unsafeCastId

instance IsNSFormatter (Id NSMeasurementFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSMeasurementFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSNumberFormatter ----------

-- | Phantom type for @NSNumberFormatter@.
data NSNumberFormatter

instance IsObjCObject (Id NSNumberFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSNumberFormatter"

class IsNSFormatter a => IsNSNumberFormatter a where
  toNSNumberFormatter :: a -> Id NSNumberFormatter

instance IsNSNumberFormatter (Id NSNumberFormatter) where
  toNSNumberFormatter = unsafeCastId

instance IsNSFormatter (Id NSNumberFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSNumberFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSPersonNameComponentsFormatter ----------

-- | Phantom type for @NSPersonNameComponentsFormatter@.
data NSPersonNameComponentsFormatter

instance IsObjCObject (Id NSPersonNameComponentsFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersonNameComponentsFormatter"

class IsNSFormatter a => IsNSPersonNameComponentsFormatter a where
  toNSPersonNameComponentsFormatter :: a -> Id NSPersonNameComponentsFormatter

instance IsNSPersonNameComponentsFormatter (Id NSPersonNameComponentsFormatter) where
  toNSPersonNameComponentsFormatter = unsafeCastId

instance IsNSFormatter (Id NSPersonNameComponentsFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSPersonNameComponentsFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSRelativeDateTimeFormatter ----------

-- | Phantom type for @NSRelativeDateTimeFormatter@.
data NSRelativeDateTimeFormatter

instance IsObjCObject (Id NSRelativeDateTimeFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSRelativeDateTimeFormatter"

class IsNSFormatter a => IsNSRelativeDateTimeFormatter a where
  toNSRelativeDateTimeFormatter :: a -> Id NSRelativeDateTimeFormatter

instance IsNSRelativeDateTimeFormatter (Id NSRelativeDateTimeFormatter) where
  toNSRelativeDateTimeFormatter = unsafeCastId

instance IsNSFormatter (Id NSRelativeDateTimeFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id NSRelativeDateTimeFormatter) where
  toNSObject = unsafeCastId

-- ---------- NSMutableIndexSet ----------

-- | Phantom type for @NSMutableIndexSet@.
data NSMutableIndexSet

instance IsObjCObject (Id NSMutableIndexSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMutableIndexSet"

class IsNSIndexSet a => IsNSMutableIndexSet a where
  toNSMutableIndexSet :: a -> Id NSMutableIndexSet

instance IsNSMutableIndexSet (Id NSMutableIndexSet) where
  toNSMutableIndexSet = unsafeCastId

instance IsNSIndexSet (Id NSMutableIndexSet) where
  toNSIndexSet = unsafeCastId

instance IsNSObject (Id NSMutableIndexSet) where
  toNSObject = unsafeCastId

-- ---------- NSInflectionRuleExplicit ----------

-- | Phantom type for @NSInflectionRuleExplicit@.
data NSInflectionRuleExplicit

instance IsObjCObject (Id NSInflectionRuleExplicit) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSInflectionRuleExplicit"

class IsNSInflectionRule a => IsNSInflectionRuleExplicit a where
  toNSInflectionRuleExplicit :: a -> Id NSInflectionRuleExplicit

instance IsNSInflectionRuleExplicit (Id NSInflectionRuleExplicit) where
  toNSInflectionRuleExplicit = unsafeCastId

instance IsNSInflectionRule (Id NSInflectionRuleExplicit) where
  toNSInflectionRule = unsafeCastId

instance IsNSObject (Id NSInflectionRuleExplicit) where
  toNSObject = unsafeCastId

-- ---------- NSDistributedNotificationCenter ----------

-- | Phantom type for @NSDistributedNotificationCenter@.
data NSDistributedNotificationCenter

instance IsObjCObject (Id NSDistributedNotificationCenter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDistributedNotificationCenter"

class IsNSNotificationCenter a => IsNSDistributedNotificationCenter a where
  toNSDistributedNotificationCenter :: a -> Id NSDistributedNotificationCenter

instance IsNSDistributedNotificationCenter (Id NSDistributedNotificationCenter) where
  toNSDistributedNotificationCenter = unsafeCastId

instance IsNSNotificationCenter (Id NSDistributedNotificationCenter) where
  toNSNotificationCenter = unsafeCastId

instance IsNSObject (Id NSDistributedNotificationCenter) where
  toNSObject = unsafeCastId

-- ---------- NSBlockOperation ----------

-- | Phantom type for @NSBlockOperation@.
data NSBlockOperation

instance IsObjCObject (Id NSBlockOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBlockOperation"

class IsNSOperation a => IsNSBlockOperation a where
  toNSBlockOperation :: a -> Id NSBlockOperation

instance IsNSBlockOperation (Id NSBlockOperation) where
  toNSBlockOperation = unsafeCastId

instance IsNSObject (Id NSBlockOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id NSBlockOperation) where
  toNSOperation = unsafeCastId

-- ---------- NSInvocationOperation ----------

-- | Phantom type for @NSInvocationOperation@.
data NSInvocationOperation

instance IsObjCObject (Id NSInvocationOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSInvocationOperation"

class IsNSOperation a => IsNSInvocationOperation a where
  toNSInvocationOperation :: a -> Id NSInvocationOperation

instance IsNSInvocationOperation (Id NSInvocationOperation) where
  toNSInvocationOperation = unsafeCastId

instance IsNSObject (Id NSInvocationOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id NSInvocationOperation) where
  toNSOperation = unsafeCastId

-- ---------- NSMutableOrderedSet ----------

-- | **************       Mutable Ordered Set     ***************
-- 
-- Phantom type for @NSMutableOrderedSet@.
data NSMutableOrderedSet

instance IsObjCObject (Id NSMutableOrderedSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMutableOrderedSet"

class IsNSOrderedSet a => IsNSMutableOrderedSet a where
  toNSMutableOrderedSet :: a -> Id NSMutableOrderedSet

instance IsNSMutableOrderedSet (Id NSMutableOrderedSet) where
  toNSMutableOrderedSet = unsafeCastId

instance IsNSObject (Id NSMutableOrderedSet) where
  toNSObject = unsafeCastId

instance IsNSOrderedSet (Id NSMutableOrderedSet) where
  toNSOrderedSet = unsafeCastId

-- ---------- NSMachPort ----------

-- | Phantom type for @NSMachPort@.
data NSMachPort

instance IsObjCObject (Id NSMachPort) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMachPort"

class IsNSPort a => IsNSMachPort a where
  toNSMachPort :: a -> Id NSMachPort

instance IsNSMachPort (Id NSMachPort) where
  toNSMachPort = unsafeCastId

instance IsNSObject (Id NSMachPort) where
  toNSObject = unsafeCastId

instance IsNSPort (Id NSMachPort) where
  toNSPort = unsafeCastId

-- ---------- NSMessagePort ----------

-- | Phantom type for @NSMessagePort@.
data NSMessagePort

instance IsObjCObject (Id NSMessagePort) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMessagePort"

class IsNSPort a => IsNSMessagePort a where
  toNSMessagePort :: a -> Id NSMessagePort

instance IsNSMessagePort (Id NSMessagePort) where
  toNSMessagePort = unsafeCastId

instance IsNSObject (Id NSMessagePort) where
  toNSObject = unsafeCastId

instance IsNSPort (Id NSMessagePort) where
  toNSPort = unsafeCastId

-- ---------- NSSocketPort ----------

-- | Phantom type for @NSSocketPort@.
data NSSocketPort

instance IsObjCObject (Id NSSocketPort) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSocketPort"

class IsNSPort a => IsNSSocketPort a where
  toNSSocketPort :: a -> Id NSSocketPort

instance IsNSSocketPort (Id NSSocketPort) where
  toNSSocketPort = unsafeCastId

instance IsNSObject (Id NSSocketPort) where
  toNSObject = unsafeCastId

instance IsNSPort (Id NSSocketPort) where
  toNSPort = unsafeCastId

-- ---------- NSMachBootstrapServer ----------

-- | Phantom type for @NSMachBootstrapServer@.
data NSMachBootstrapServer

instance IsObjCObject (Id NSMachBootstrapServer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMachBootstrapServer"

class IsNSPortNameServer a => IsNSMachBootstrapServer a where
  toNSMachBootstrapServer :: a -> Id NSMachBootstrapServer

instance IsNSMachBootstrapServer (Id NSMachBootstrapServer) where
  toNSMachBootstrapServer = unsafeCastId

instance IsNSObject (Id NSMachBootstrapServer) where
  toNSObject = unsafeCastId

instance IsNSPortNameServer (Id NSMachBootstrapServer) where
  toNSPortNameServer = unsafeCastId

-- ---------- NSMessagePortNameServer ----------

-- | Phantom type for @NSMessagePortNameServer@.
data NSMessagePortNameServer

instance IsObjCObject (Id NSMessagePortNameServer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMessagePortNameServer"

class IsNSPortNameServer a => IsNSMessagePortNameServer a where
  toNSMessagePortNameServer :: a -> Id NSMessagePortNameServer

instance IsNSMessagePortNameServer (Id NSMessagePortNameServer) where
  toNSMessagePortNameServer = unsafeCastId

instance IsNSObject (Id NSMessagePortNameServer) where
  toNSObject = unsafeCastId

instance IsNSPortNameServer (Id NSMessagePortNameServer) where
  toNSPortNameServer = unsafeCastId

-- ---------- NSSocketPortNameServer ----------

-- | Phantom type for @NSSocketPortNameServer@.
data NSSocketPortNameServer

instance IsObjCObject (Id NSSocketPortNameServer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSocketPortNameServer"

class IsNSPortNameServer a => IsNSSocketPortNameServer a where
  toNSSocketPortNameServer :: a -> Id NSSocketPortNameServer

instance IsNSSocketPortNameServer (Id NSSocketPortNameServer) where
  toNSSocketPortNameServer = unsafeCastId

instance IsNSObject (Id NSSocketPortNameServer) where
  toNSObject = unsafeCastId

instance IsNSPortNameServer (Id NSSocketPortNameServer) where
  toNSPortNameServer = unsafeCastId

-- ---------- NSComparisonPredicate ----------

-- | Phantom type for @NSComparisonPredicate@.
data NSComparisonPredicate

instance IsObjCObject (Id NSComparisonPredicate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSComparisonPredicate"

class IsNSPredicate a => IsNSComparisonPredicate a where
  toNSComparisonPredicate :: a -> Id NSComparisonPredicate

instance IsNSComparisonPredicate (Id NSComparisonPredicate) where
  toNSComparisonPredicate = unsafeCastId

instance IsNSObject (Id NSComparisonPredicate) where
  toNSObject = unsafeCastId

instance IsNSPredicate (Id NSComparisonPredicate) where
  toNSPredicate = unsafeCastId

-- ---------- NSCompoundPredicate ----------

-- | Phantom type for @NSCompoundPredicate@.
data NSCompoundPredicate

instance IsObjCObject (Id NSCompoundPredicate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCompoundPredicate"

class IsNSPredicate a => IsNSCompoundPredicate a where
  toNSCompoundPredicate :: a -> Id NSCompoundPredicate

instance IsNSCompoundPredicate (Id NSCompoundPredicate) where
  toNSCompoundPredicate = unsafeCastId

instance IsNSObject (Id NSCompoundPredicate) where
  toNSObject = unsafeCastId

instance IsNSPredicate (Id NSCompoundPredicate) where
  toNSPredicate = unsafeCastId

-- ---------- NSDataDetector ----------

-- | Phantom type for @NSDataDetector@.
data NSDataDetector

instance IsObjCObject (Id NSDataDetector) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDataDetector"

class IsNSRegularExpression a => IsNSDataDetector a where
  toNSDataDetector :: a -> Id NSDataDetector

instance IsNSDataDetector (Id NSDataDetector) where
  toNSDataDetector = unsafeCastId

instance IsNSObject (Id NSDataDetector) where
  toNSObject = unsafeCastId

instance IsNSRegularExpression (Id NSDataDetector) where
  toNSRegularExpression = unsafeCastId

-- ---------- NSCloneCommand ----------

-- | Phantom type for @NSCloneCommand@.
data NSCloneCommand

instance IsObjCObject (Id NSCloneCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCloneCommand"

class IsNSScriptCommand a => IsNSCloneCommand a where
  toNSCloneCommand :: a -> Id NSCloneCommand

instance IsNSCloneCommand (Id NSCloneCommand) where
  toNSCloneCommand = unsafeCastId

instance IsNSObject (Id NSCloneCommand) where
  toNSObject = unsafeCastId

instance IsNSScriptCommand (Id NSCloneCommand) where
  toNSScriptCommand = unsafeCastId

-- ---------- NSCloseCommand ----------

-- | Phantom type for @NSCloseCommand@.
data NSCloseCommand

instance IsObjCObject (Id NSCloseCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCloseCommand"

class IsNSScriptCommand a => IsNSCloseCommand a where
  toNSCloseCommand :: a -> Id NSCloseCommand

instance IsNSCloseCommand (Id NSCloseCommand) where
  toNSCloseCommand = unsafeCastId

instance IsNSObject (Id NSCloseCommand) where
  toNSObject = unsafeCastId

instance IsNSScriptCommand (Id NSCloseCommand) where
  toNSScriptCommand = unsafeCastId

-- ---------- NSCreateCommand ----------

-- | Phantom type for @NSCreateCommand@.
data NSCreateCommand

instance IsObjCObject (Id NSCreateCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCreateCommand"

class IsNSScriptCommand a => IsNSCreateCommand a where
  toNSCreateCommand :: a -> Id NSCreateCommand

instance IsNSCreateCommand (Id NSCreateCommand) where
  toNSCreateCommand = unsafeCastId

instance IsNSObject (Id NSCreateCommand) where
  toNSObject = unsafeCastId

instance IsNSScriptCommand (Id NSCreateCommand) where
  toNSScriptCommand = unsafeCastId

-- ---------- NSDeleteCommand ----------

-- | Phantom type for @NSDeleteCommand@.
data NSDeleteCommand

instance IsObjCObject (Id NSDeleteCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDeleteCommand"

class IsNSScriptCommand a => IsNSDeleteCommand a where
  toNSDeleteCommand :: a -> Id NSDeleteCommand

instance IsNSDeleteCommand (Id NSDeleteCommand) where
  toNSDeleteCommand = unsafeCastId

instance IsNSObject (Id NSDeleteCommand) where
  toNSObject = unsafeCastId

instance IsNSScriptCommand (Id NSDeleteCommand) where
  toNSScriptCommand = unsafeCastId

-- ---------- NSMoveCommand ----------

-- | Phantom type for @NSMoveCommand@.
data NSMoveCommand

instance IsObjCObject (Id NSMoveCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMoveCommand"

class IsNSScriptCommand a => IsNSMoveCommand a where
  toNSMoveCommand :: a -> Id NSMoveCommand

instance IsNSMoveCommand (Id NSMoveCommand) where
  toNSMoveCommand = unsafeCastId

instance IsNSObject (Id NSMoveCommand) where
  toNSObject = unsafeCastId

instance IsNSScriptCommand (Id NSMoveCommand) where
  toNSScriptCommand = unsafeCastId

-- ---------- NSQuitCommand ----------

-- | Phantom type for @NSQuitCommand@.
data NSQuitCommand

instance IsObjCObject (Id NSQuitCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSQuitCommand"

class IsNSScriptCommand a => IsNSQuitCommand a where
  toNSQuitCommand :: a -> Id NSQuitCommand

instance IsNSQuitCommand (Id NSQuitCommand) where
  toNSQuitCommand = unsafeCastId

instance IsNSObject (Id NSQuitCommand) where
  toNSObject = unsafeCastId

instance IsNSScriptCommand (Id NSQuitCommand) where
  toNSScriptCommand = unsafeCastId

-- ---------- NSSetCommand ----------

-- | Phantom type for @NSSetCommand@.
data NSSetCommand

instance IsObjCObject (Id NSSetCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSetCommand"

class IsNSScriptCommand a => IsNSSetCommand a where
  toNSSetCommand :: a -> Id NSSetCommand

instance IsNSSetCommand (Id NSSetCommand) where
  toNSSetCommand = unsafeCastId

instance IsNSObject (Id NSSetCommand) where
  toNSObject = unsafeCastId

instance IsNSScriptCommand (Id NSSetCommand) where
  toNSScriptCommand = unsafeCastId

-- ---------- NSIndexSpecifier ----------

-- | Phantom type for @NSIndexSpecifier@.
data NSIndexSpecifier

instance IsObjCObject (Id NSIndexSpecifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSIndexSpecifier"

class IsNSScriptObjectSpecifier a => IsNSIndexSpecifier a where
  toNSIndexSpecifier :: a -> Id NSIndexSpecifier

instance IsNSIndexSpecifier (Id NSIndexSpecifier) where
  toNSIndexSpecifier = unsafeCastId

instance IsNSObject (Id NSIndexSpecifier) where
  toNSObject = unsafeCastId

instance IsNSScriptObjectSpecifier (Id NSIndexSpecifier) where
  toNSScriptObjectSpecifier = unsafeCastId

-- ---------- NSNameSpecifier ----------

-- | Phantom type for @NSNameSpecifier@.
data NSNameSpecifier

instance IsObjCObject (Id NSNameSpecifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSNameSpecifier"

class IsNSScriptObjectSpecifier a => IsNSNameSpecifier a where
  toNSNameSpecifier :: a -> Id NSNameSpecifier

instance IsNSNameSpecifier (Id NSNameSpecifier) where
  toNSNameSpecifier = unsafeCastId

instance IsNSObject (Id NSNameSpecifier) where
  toNSObject = unsafeCastId

instance IsNSScriptObjectSpecifier (Id NSNameSpecifier) where
  toNSScriptObjectSpecifier = unsafeCastId

-- ---------- NSRangeSpecifier ----------

-- | Phantom type for @NSRangeSpecifier@.
data NSRangeSpecifier

instance IsObjCObject (Id NSRangeSpecifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSRangeSpecifier"

class IsNSScriptObjectSpecifier a => IsNSRangeSpecifier a where
  toNSRangeSpecifier :: a -> Id NSRangeSpecifier

instance IsNSRangeSpecifier (Id NSRangeSpecifier) where
  toNSRangeSpecifier = unsafeCastId

instance IsNSObject (Id NSRangeSpecifier) where
  toNSObject = unsafeCastId

instance IsNSScriptObjectSpecifier (Id NSRangeSpecifier) where
  toNSScriptObjectSpecifier = unsafeCastId

-- ---------- NSRelativeSpecifier ----------

-- | Phantom type for @NSRelativeSpecifier@.
data NSRelativeSpecifier

instance IsObjCObject (Id NSRelativeSpecifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSRelativeSpecifier"

class IsNSScriptObjectSpecifier a => IsNSRelativeSpecifier a where
  toNSRelativeSpecifier :: a -> Id NSRelativeSpecifier

instance IsNSRelativeSpecifier (Id NSRelativeSpecifier) where
  toNSRelativeSpecifier = unsafeCastId

instance IsNSObject (Id NSRelativeSpecifier) where
  toNSObject = unsafeCastId

instance IsNSScriptObjectSpecifier (Id NSRelativeSpecifier) where
  toNSScriptObjectSpecifier = unsafeCastId

-- ---------- NSUniqueIDSpecifier ----------

-- | Phantom type for @NSUniqueIDSpecifier@.
data NSUniqueIDSpecifier

instance IsObjCObject (Id NSUniqueIDSpecifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUniqueIDSpecifier"

class IsNSScriptObjectSpecifier a => IsNSUniqueIDSpecifier a where
  toNSUniqueIDSpecifier :: a -> Id NSUniqueIDSpecifier

instance IsNSUniqueIDSpecifier (Id NSUniqueIDSpecifier) where
  toNSUniqueIDSpecifier = unsafeCastId

instance IsNSObject (Id NSUniqueIDSpecifier) where
  toNSObject = unsafeCastId

instance IsNSScriptObjectSpecifier (Id NSUniqueIDSpecifier) where
  toNSScriptObjectSpecifier = unsafeCastId

-- ---------- NSWhoseSpecifier ----------

-- | Phantom type for @NSWhoseSpecifier@.
data NSWhoseSpecifier

instance IsObjCObject (Id NSWhoseSpecifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSWhoseSpecifier"

class IsNSScriptObjectSpecifier a => IsNSWhoseSpecifier a where
  toNSWhoseSpecifier :: a -> Id NSWhoseSpecifier

instance IsNSWhoseSpecifier (Id NSWhoseSpecifier) where
  toNSWhoseSpecifier = unsafeCastId

instance IsNSObject (Id NSWhoseSpecifier) where
  toNSObject = unsafeCastId

instance IsNSScriptObjectSpecifier (Id NSWhoseSpecifier) where
  toNSScriptObjectSpecifier = unsafeCastId

-- ---------- NSLogicalTest ----------

-- | Phantom type for @NSLogicalTest@.
data NSLogicalTest

instance IsObjCObject (Id NSLogicalTest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLogicalTest"

class IsNSScriptWhoseTest a => IsNSLogicalTest a where
  toNSLogicalTest :: a -> Id NSLogicalTest

instance IsNSLogicalTest (Id NSLogicalTest) where
  toNSLogicalTest = unsafeCastId

instance IsNSObject (Id NSLogicalTest) where
  toNSObject = unsafeCastId

instance IsNSScriptWhoseTest (Id NSLogicalTest) where
  toNSScriptWhoseTest = unsafeCastId

-- ---------- NSSpecifierTest ----------

-- | Phantom type for @NSSpecifierTest@.
data NSSpecifierTest

instance IsObjCObject (Id NSSpecifierTest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSpecifierTest"

class IsNSScriptWhoseTest a => IsNSSpecifierTest a where
  toNSSpecifierTest :: a -> Id NSSpecifierTest

instance IsNSSpecifierTest (Id NSSpecifierTest) where
  toNSSpecifierTest = unsafeCastId

instance IsNSObject (Id NSSpecifierTest) where
  toNSObject = unsafeCastId

instance IsNSScriptWhoseTest (Id NSSpecifierTest) where
  toNSScriptWhoseTest = unsafeCastId

-- ---------- NSMutableSet ----------

-- | **************	Mutable Set	***************
-- 
-- Phantom type for @NSMutableSet@.
data NSMutableSet

instance IsObjCObject (Id NSMutableSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMutableSet"

class IsNSSet a => IsNSMutableSet a where
  toNSMutableSet :: a -> Id NSMutableSet

instance IsNSMutableSet (Id NSMutableSet) where
  toNSMutableSet = unsafeCastId

instance IsNSObject (Id NSMutableSet) where
  toNSObject = unsafeCastId

instance IsNSSet (Id NSMutableSet) where
  toNSSet = unsafeCastId

-- ---------- NSInputStream ----------

-- | Phantom type for @NSInputStream@.
data NSInputStream

instance IsObjCObject (Id NSInputStream) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSInputStream"

class IsNSStream a => IsNSInputStream a where
  toNSInputStream :: a -> Id NSInputStream

instance IsNSInputStream (Id NSInputStream) where
  toNSInputStream = unsafeCastId

instance IsNSObject (Id NSInputStream) where
  toNSObject = unsafeCastId

instance IsNSStream (Id NSInputStream) where
  toNSStream = unsafeCastId

-- ---------- NSOutputStream ----------

-- | Phantom type for @NSOutputStream@.
data NSOutputStream

instance IsObjCObject (Id NSOutputStream) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOutputStream"

class IsNSStream a => IsNSOutputStream a where
  toNSOutputStream :: a -> Id NSOutputStream

instance IsNSOutputStream (Id NSOutputStream) where
  toNSOutputStream = unsafeCastId

instance IsNSObject (Id NSOutputStream) where
  toNSObject = unsafeCastId

instance IsNSStream (Id NSOutputStream) where
  toNSStream = unsafeCastId

-- ---------- NSMutableString ----------

-- | Phantom type for @NSMutableString@.
data NSMutableString

instance IsObjCObject (Id NSMutableString) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMutableString"

class IsNSString a => IsNSMutableString a where
  toNSMutableString :: a -> Id NSMutableString

instance IsNSMutableString (Id NSMutableString) where
  toNSMutableString = unsafeCastId

instance IsNSObject (Id NSMutableString) where
  toNSObject = unsafeCastId

instance IsNSString (Id NSMutableString) where
  toNSString = unsafeCastId

-- ---------- NSSimpleCString ----------

-- | Phantom type for @NSSimpleCString@.
data NSSimpleCString

instance IsObjCObject (Id NSSimpleCString) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSimpleCString"

class IsNSString a => IsNSSimpleCString a where
  toNSSimpleCString :: a -> Id NSSimpleCString

instance IsNSSimpleCString (Id NSSimpleCString) where
  toNSSimpleCString = unsafeCastId

instance IsNSObject (Id NSSimpleCString) where
  toNSObject = unsafeCastId

instance IsNSString (Id NSSimpleCString) where
  toNSString = unsafeCastId

-- ---------- NSMutableURLRequest ----------

-- | NSMutableURLRequest
--
-- An NSMutableURLRequest object represents a mutable URL load    request in a manner independent of protocol and URL scheme.
--
-- This specialization of NSURLRequest is provided to aid    developers who may find it more convenient to mutate a single request    object for a series of URL loads instead of creating an immutable    NSURLRequest for each load. This programming model is supported by    the following contract stipulation between NSMutableURLRequest and     NSURLConnection: NSURLConnection makes a deep copy of each     NSMutableURLRequest object passed to one of its initializers.        NSMutableURLRequest is designed to be extended to support    protocol-specific data by adding categories to access a property    object provided in an interface targeted at protocol implementors.        Protocol implementors should direct their attention to the    NSMutableURLRequestExtensibility category on    NSMutableURLRequest for more information on how to provide    extensions on NSMutableURLRequest to support protocol-specific    request information.    Clients of this API who wish to create NSMutableURLRequest    objects to load URL content should consult the protocol-specific    NSMutableURLRequest categories that are available. The    NSMutableHTTPURLRequest category on NSMutableURLRequest is an    example.
-- 
-- Phantom type for @NSMutableURLRequest@.
data NSMutableURLRequest

instance IsObjCObject (Id NSMutableURLRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMutableURLRequest"

class IsNSURLRequest a => IsNSMutableURLRequest a where
  toNSMutableURLRequest :: a -> Id NSMutableURLRequest

instance IsNSMutableURLRequest (Id NSMutableURLRequest) where
  toNSMutableURLRequest = unsafeCastId

instance IsNSObject (Id NSMutableURLRequest) where
  toNSObject = unsafeCastId

instance IsNSURLRequest (Id NSMutableURLRequest) where
  toNSURLRequest = unsafeCastId

-- ---------- NSHTTPURLResponse ----------

-- | NSHTTPURLResponse
--
-- An NSHTTPURLResponse object represents a response to an    HTTP URL load. It is a specialization of NSURLResponse which    provides conveniences for accessing information specific to HTTP    protocol responses.
-- 
-- Phantom type for @NSHTTPURLResponse@.
data NSHTTPURLResponse

instance IsObjCObject (Id NSHTTPURLResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSHTTPURLResponse"

class IsNSURLResponse a => IsNSHTTPURLResponse a where
  toNSHTTPURLResponse :: a -> Id NSHTTPURLResponse

instance IsNSHTTPURLResponse (Id NSHTTPURLResponse) where
  toNSHTTPURLResponse = unsafeCastId

instance IsNSObject (Id NSHTTPURLResponse) where
  toNSObject = unsafeCastId

instance IsNSURLResponse (Id NSHTTPURLResponse) where
  toNSURLResponse = unsafeCastId

-- ---------- NSURLSessionDataTask ----------

-- | Phantom type for @NSURLSessionDataTask@.
data NSURLSessionDataTask

instance IsObjCObject (Id NSURLSessionDataTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLSessionDataTask"

class IsNSURLSessionTask a => IsNSURLSessionDataTask a where
  toNSURLSessionDataTask :: a -> Id NSURLSessionDataTask

instance IsNSURLSessionDataTask (Id NSURLSessionDataTask) where
  toNSURLSessionDataTask = unsafeCastId

instance IsNSObject (Id NSURLSessionDataTask) where
  toNSObject = unsafeCastId

instance IsNSURLSessionTask (Id NSURLSessionDataTask) where
  toNSURLSessionTask = unsafeCastId

-- ---------- NSURLSessionDownloadTask ----------

-- | Phantom type for @NSURLSessionDownloadTask@.
data NSURLSessionDownloadTask

instance IsObjCObject (Id NSURLSessionDownloadTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLSessionDownloadTask"

class IsNSURLSessionTask a => IsNSURLSessionDownloadTask a where
  toNSURLSessionDownloadTask :: a -> Id NSURLSessionDownloadTask

instance IsNSURLSessionDownloadTask (Id NSURLSessionDownloadTask) where
  toNSURLSessionDownloadTask = unsafeCastId

instance IsNSObject (Id NSURLSessionDownloadTask) where
  toNSObject = unsafeCastId

instance IsNSURLSessionTask (Id NSURLSessionDownloadTask) where
  toNSURLSessionTask = unsafeCastId

-- ---------- NSURLSessionStreamTask ----------

-- | Phantom type for @NSURLSessionStreamTask@.
data NSURLSessionStreamTask

instance IsObjCObject (Id NSURLSessionStreamTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLSessionStreamTask"

class IsNSURLSessionTask a => IsNSURLSessionStreamTask a where
  toNSURLSessionStreamTask :: a -> Id NSURLSessionStreamTask

instance IsNSURLSessionStreamTask (Id NSURLSessionStreamTask) where
  toNSURLSessionStreamTask = unsafeCastId

instance IsNSObject (Id NSURLSessionStreamTask) where
  toNSObject = unsafeCastId

instance IsNSURLSessionTask (Id NSURLSessionStreamTask) where
  toNSURLSessionTask = unsafeCastId

-- ---------- NSURLSessionWebSocketTask ----------

-- | Phantom type for @NSURLSessionWebSocketTask@.
data NSURLSessionWebSocketTask

instance IsObjCObject (Id NSURLSessionWebSocketTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLSessionWebSocketTask"

class IsNSURLSessionTask a => IsNSURLSessionWebSocketTask a where
  toNSURLSessionWebSocketTask :: a -> Id NSURLSessionWebSocketTask

instance IsNSURLSessionWebSocketTask (Id NSURLSessionWebSocketTask) where
  toNSURLSessionWebSocketTask = unsafeCastId

instance IsNSObject (Id NSURLSessionWebSocketTask) where
  toNSObject = unsafeCastId

instance IsNSURLSessionTask (Id NSURLSessionWebSocketTask) where
  toNSURLSessionTask = unsafeCastId

-- ---------- NSDimension ----------

-- | Phantom type for @NSDimension@.
data NSDimension

instance IsObjCObject (Id NSDimension) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDimension"

class IsNSUnit a => IsNSDimension a where
  toNSDimension :: a -> Id NSDimension

instance IsNSDimension (Id NSDimension) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSDimension) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSDimension) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitConverterLinear ----------

-- | Phantom type for @NSUnitConverterLinear@.
data NSUnitConverterLinear

instance IsObjCObject (Id NSUnitConverterLinear) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitConverterLinear"

class IsNSUnitConverter a => IsNSUnitConverterLinear a where
  toNSUnitConverterLinear :: a -> Id NSUnitConverterLinear

instance IsNSUnitConverterLinear (Id NSUnitConverterLinear) where
  toNSUnitConverterLinear = unsafeCastId

instance IsNSObject (Id NSUnitConverterLinear) where
  toNSObject = unsafeCastId

instance IsNSUnitConverter (Id NSUnitConverterLinear) where
  toNSUnitConverter = unsafeCastId

-- ---------- NSUserAppleScriptTask ----------

-- | Phantom type for @NSUserAppleScriptTask@.
data NSUserAppleScriptTask

instance IsObjCObject (Id NSUserAppleScriptTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUserAppleScriptTask"

class IsNSUserScriptTask a => IsNSUserAppleScriptTask a where
  toNSUserAppleScriptTask :: a -> Id NSUserAppleScriptTask

instance IsNSUserAppleScriptTask (Id NSUserAppleScriptTask) where
  toNSUserAppleScriptTask = unsafeCastId

instance IsNSObject (Id NSUserAppleScriptTask) where
  toNSObject = unsafeCastId

instance IsNSUserScriptTask (Id NSUserAppleScriptTask) where
  toNSUserScriptTask = unsafeCastId

-- ---------- NSUserAutomatorTask ----------

-- | Phantom type for @NSUserAutomatorTask@.
data NSUserAutomatorTask

instance IsObjCObject (Id NSUserAutomatorTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUserAutomatorTask"

class IsNSUserScriptTask a => IsNSUserAutomatorTask a where
  toNSUserAutomatorTask :: a -> Id NSUserAutomatorTask

instance IsNSUserAutomatorTask (Id NSUserAutomatorTask) where
  toNSUserAutomatorTask = unsafeCastId

instance IsNSObject (Id NSUserAutomatorTask) where
  toNSObject = unsafeCastId

instance IsNSUserScriptTask (Id NSUserAutomatorTask) where
  toNSUserScriptTask = unsafeCastId

-- ---------- NSUserUnixTask ----------

-- | Phantom type for @NSUserUnixTask@.
data NSUserUnixTask

instance IsObjCObject (Id NSUserUnixTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUserUnixTask"

class IsNSUserScriptTask a => IsNSUserUnixTask a where
  toNSUserUnixTask :: a -> Id NSUserUnixTask

instance IsNSUserUnixTask (Id NSUserUnixTask) where
  toNSUserUnixTask = unsafeCastId

instance IsNSObject (Id NSUserUnixTask) where
  toNSObject = unsafeCastId

instance IsNSUserScriptTask (Id NSUserUnixTask) where
  toNSUserScriptTask = unsafeCastId

-- ---------- NSNumber ----------

-- | Phantom type for @NSNumber@.
data NSNumber

instance IsObjCObject (Id NSNumber) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSNumber"

class IsNSValue a => IsNSNumber a where
  toNSNumber :: a -> Id NSNumber

instance IsNSNumber (Id NSNumber) where
  toNSNumber = unsafeCastId

instance IsNSObject (Id NSNumber) where
  toNSObject = unsafeCastId

instance IsNSValue (Id NSNumber) where
  toNSValue = unsafeCastId

-- ---------- NSSecureUnarchiveFromDataTransformer ----------

-- | A value transformer which transforms values to and from @NSData@ by archiving and unarchiving using secure coding.
-- 
-- Phantom type for @NSSecureUnarchiveFromDataTransformer@.
data NSSecureUnarchiveFromDataTransformer

instance IsObjCObject (Id NSSecureUnarchiveFromDataTransformer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSecureUnarchiveFromDataTransformer"

class IsNSValueTransformer a => IsNSSecureUnarchiveFromDataTransformer a where
  toNSSecureUnarchiveFromDataTransformer :: a -> Id NSSecureUnarchiveFromDataTransformer

instance IsNSSecureUnarchiveFromDataTransformer (Id NSSecureUnarchiveFromDataTransformer) where
  toNSSecureUnarchiveFromDataTransformer = unsafeCastId

instance IsNSObject (Id NSSecureUnarchiveFromDataTransformer) where
  toNSObject = unsafeCastId

instance IsNSValueTransformer (Id NSSecureUnarchiveFromDataTransformer) where
  toNSValueTransformer = unsafeCastId

-- ---------- NSXMLDTD ----------

-- | NSXMLDTD
--
-- Defines the order, repetition, and allowable values for a document
-- 
-- Phantom type for @NSXMLDTD@.
data NSXMLDTD

instance IsObjCObject (Id NSXMLDTD) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSXMLDTD"

class IsNSXMLNode a => IsNSXMLDTD a where
  toNSXMLDTD :: a -> Id NSXMLDTD

instance IsNSXMLDTD (Id NSXMLDTD) where
  toNSXMLDTD = unsafeCastId

instance IsNSObject (Id NSXMLDTD) where
  toNSObject = unsafeCastId

instance IsNSXMLNode (Id NSXMLDTD) where
  toNSXMLNode = unsafeCastId

-- ---------- NSXMLDTDNode ----------

-- | NSXMLDTDNode
--
-- The nodes that are exclusive to a DTD
--
-- Every DTD node has a name. Object value is defined as follows:		Entity declaration - the string that that entity resolves to eg "&lt;"		Attribute declaration - the default value, if any		Element declaration - the validation string		Notation declaration - no objectValue
-- 
-- Phantom type for @NSXMLDTDNode@.
data NSXMLDTDNode

instance IsObjCObject (Id NSXMLDTDNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSXMLDTDNode"

class IsNSXMLNode a => IsNSXMLDTDNode a where
  toNSXMLDTDNode :: a -> Id NSXMLDTDNode

instance IsNSXMLDTDNode (Id NSXMLDTDNode) where
  toNSXMLDTDNode = unsafeCastId

instance IsNSObject (Id NSXMLDTDNode) where
  toNSObject = unsafeCastId

instance IsNSXMLNode (Id NSXMLDTDNode) where
  toNSXMLNode = unsafeCastId

-- ---------- NSXMLDocument ----------

-- | NSXMLDocument
--
-- An XML Document
--
-- Note: if the application of a method would result in more than one element in the children array, an exception is thrown. Trying to add a document, namespace, attribute, or node with a parent also throws an exception. To add a node with a parent first detach or create a copy of it.
-- 
-- Phantom type for @NSXMLDocument@.
data NSXMLDocument

instance IsObjCObject (Id NSXMLDocument) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSXMLDocument"

class IsNSXMLNode a => IsNSXMLDocument a where
  toNSXMLDocument :: a -> Id NSXMLDocument

instance IsNSXMLDocument (Id NSXMLDocument) where
  toNSXMLDocument = unsafeCastId

instance IsNSObject (Id NSXMLDocument) where
  toNSObject = unsafeCastId

instance IsNSXMLNode (Id NSXMLDocument) where
  toNSXMLNode = unsafeCastId

-- ---------- NSXMLElement ----------

-- | NSXMLElement
--
-- An XML element
--
-- Note: Trying to add a document, namespace, attribute, or node with a parent throws an exception. To add a node with a parent first detach or create a copy of it.
-- 
-- Phantom type for @NSXMLElement@.
data NSXMLElement

instance IsObjCObject (Id NSXMLElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSXMLElement"

class IsNSXMLNode a => IsNSXMLElement a where
  toNSXMLElement :: a -> Id NSXMLElement

instance IsNSXMLElement (Id NSXMLElement) where
  toNSXMLElement = unsafeCastId

instance IsNSObject (Id NSXMLElement) where
  toNSObject = unsafeCastId

instance IsNSXMLNode (Id NSXMLElement) where
  toNSXMLNode = unsafeCastId

-- ---------- OS_os_workgroup ----------

-- | os_workgroup_t
--
-- A reference counted os object representing a workload that needs to be distinctly recognized and tracked by the system.  The workgroup tracks a collection of threads all working cooperatively. An os_workgroup object - when not an instance of a specific os_workgroup_t subclass - represents a generic workload and makes no assumptions about the kind of work done.
--
-- Threads can explicitly join an os_workgroup_t to mark themselves as participants in the workload.
-- 
-- Phantom type for @OS_os_workgroup@.
data OS_os_workgroup

instance IsObjCObject (Id OS_os_workgroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "OS_os_workgroup"

class IsOS_object a => IsOS_os_workgroup a where
  toOS_os_workgroup :: a -> Id OS_os_workgroup

instance IsOS_os_workgroup (Id OS_os_workgroup) where
  toOS_os_workgroup = unsafeCastId

instance IsNSObject (Id OS_os_workgroup) where
  toNSObject = unsafeCastId

instance IsOS_object (Id OS_os_workgroup) where
  toOS_object = unsafeCastId

-- ---------- NSPurgeableData ----------

-- | **************	    Purgeable Data	***************
-- 
-- Phantom type for @NSPurgeableData@.
data NSPurgeableData

instance IsObjCObject (Id NSPurgeableData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPurgeableData"

class IsNSMutableData a => IsNSPurgeableData a where
  toNSPurgeableData :: a -> Id NSPurgeableData

instance IsNSPurgeableData (Id NSPurgeableData) where
  toNSPurgeableData = unsafeCastId

instance IsNSData (Id NSPurgeableData) where
  toNSData = unsafeCastId

instance IsNSMutableData (Id NSPurgeableData) where
  toNSMutableData = unsafeCastId

instance IsNSObject (Id NSPurgeableData) where
  toNSObject = unsafeCastId

-- ---------- NSCountedSet ----------

-- | **************	Counted Set	***************
-- 
-- Phantom type for @NSCountedSet@.
data NSCountedSet

instance IsObjCObject (Id NSCountedSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCountedSet"

class IsNSMutableSet a => IsNSCountedSet a where
  toNSCountedSet :: a -> Id NSCountedSet

instance IsNSCountedSet (Id NSCountedSet) where
  toNSCountedSet = unsafeCastId

instance IsNSMutableSet (Id NSCountedSet) where
  toNSMutableSet = unsafeCastId

instance IsNSObject (Id NSCountedSet) where
  toNSObject = unsafeCastId

instance IsNSSet (Id NSCountedSet) where
  toNSSet = unsafeCastId

-- ---------- NSConstantString ----------

-- | Phantom type for @NSConstantString@.
data NSConstantString

instance IsObjCObject (Id NSConstantString) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSConstantString"

class IsNSSimpleCString a => IsNSConstantString a where
  toNSConstantString :: a -> Id NSConstantString

instance IsNSConstantString (Id NSConstantString) where
  toNSConstantString = unsafeCastId

instance IsNSObject (Id NSConstantString) where
  toNSObject = unsafeCastId

instance IsNSSimpleCString (Id NSConstantString) where
  toNSSimpleCString = unsafeCastId

instance IsNSString (Id NSConstantString) where
  toNSString = unsafeCastId

-- ---------- NSURLSessionUploadTask ----------

-- | Phantom type for @NSURLSessionUploadTask@.
data NSURLSessionUploadTask

instance IsObjCObject (Id NSURLSessionUploadTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSURLSessionUploadTask"

class IsNSURLSessionDataTask a => IsNSURLSessionUploadTask a where
  toNSURLSessionUploadTask :: a -> Id NSURLSessionUploadTask

instance IsNSURLSessionUploadTask (Id NSURLSessionUploadTask) where
  toNSURLSessionUploadTask = unsafeCastId

instance IsNSObject (Id NSURLSessionUploadTask) where
  toNSObject = unsafeCastId

instance IsNSURLSessionDataTask (Id NSURLSessionUploadTask) where
  toNSURLSessionDataTask = unsafeCastId

instance IsNSURLSessionTask (Id NSURLSessionUploadTask) where
  toNSURLSessionTask = unsafeCastId

-- ---------- NSUnitAcceleration ----------

-- | Phantom type for @NSUnitAcceleration@.
data NSUnitAcceleration

instance IsObjCObject (Id NSUnitAcceleration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitAcceleration"

class IsNSDimension a => IsNSUnitAcceleration a where
  toNSUnitAcceleration :: a -> Id NSUnitAcceleration

instance IsNSUnitAcceleration (Id NSUnitAcceleration) where
  toNSUnitAcceleration = unsafeCastId

instance IsNSDimension (Id NSUnitAcceleration) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitAcceleration) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitAcceleration) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitAngle ----------

-- | Phantom type for @NSUnitAngle@.
data NSUnitAngle

instance IsObjCObject (Id NSUnitAngle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitAngle"

class IsNSDimension a => IsNSUnitAngle a where
  toNSUnitAngle :: a -> Id NSUnitAngle

instance IsNSUnitAngle (Id NSUnitAngle) where
  toNSUnitAngle = unsafeCastId

instance IsNSDimension (Id NSUnitAngle) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitAngle) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitAngle) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitArea ----------

-- | Phantom type for @NSUnitArea@.
data NSUnitArea

instance IsObjCObject (Id NSUnitArea) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitArea"

class IsNSDimension a => IsNSUnitArea a where
  toNSUnitArea :: a -> Id NSUnitArea

instance IsNSUnitArea (Id NSUnitArea) where
  toNSUnitArea = unsafeCastId

instance IsNSDimension (Id NSUnitArea) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitArea) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitArea) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitConcentrationMass ----------

-- | Phantom type for @NSUnitConcentrationMass@.
data NSUnitConcentrationMass

instance IsObjCObject (Id NSUnitConcentrationMass) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitConcentrationMass"

class IsNSDimension a => IsNSUnitConcentrationMass a where
  toNSUnitConcentrationMass :: a -> Id NSUnitConcentrationMass

instance IsNSUnitConcentrationMass (Id NSUnitConcentrationMass) where
  toNSUnitConcentrationMass = unsafeCastId

instance IsNSDimension (Id NSUnitConcentrationMass) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitConcentrationMass) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitConcentrationMass) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitDispersion ----------

-- | Phantom type for @NSUnitDispersion@.
data NSUnitDispersion

instance IsObjCObject (Id NSUnitDispersion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitDispersion"

class IsNSDimension a => IsNSUnitDispersion a where
  toNSUnitDispersion :: a -> Id NSUnitDispersion

instance IsNSUnitDispersion (Id NSUnitDispersion) where
  toNSUnitDispersion = unsafeCastId

instance IsNSDimension (Id NSUnitDispersion) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitDispersion) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitDispersion) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitDuration ----------

-- | Phantom type for @NSUnitDuration@.
data NSUnitDuration

instance IsObjCObject (Id NSUnitDuration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitDuration"

class IsNSDimension a => IsNSUnitDuration a where
  toNSUnitDuration :: a -> Id NSUnitDuration

instance IsNSUnitDuration (Id NSUnitDuration) where
  toNSUnitDuration = unsafeCastId

instance IsNSDimension (Id NSUnitDuration) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitDuration) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitDuration) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitElectricCharge ----------

-- | Phantom type for @NSUnitElectricCharge@.
data NSUnitElectricCharge

instance IsObjCObject (Id NSUnitElectricCharge) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitElectricCharge"

class IsNSDimension a => IsNSUnitElectricCharge a where
  toNSUnitElectricCharge :: a -> Id NSUnitElectricCharge

instance IsNSUnitElectricCharge (Id NSUnitElectricCharge) where
  toNSUnitElectricCharge = unsafeCastId

instance IsNSDimension (Id NSUnitElectricCharge) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitElectricCharge) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitElectricCharge) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitElectricCurrent ----------

-- | Phantom type for @NSUnitElectricCurrent@.
data NSUnitElectricCurrent

instance IsObjCObject (Id NSUnitElectricCurrent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitElectricCurrent"

class IsNSDimension a => IsNSUnitElectricCurrent a where
  toNSUnitElectricCurrent :: a -> Id NSUnitElectricCurrent

instance IsNSUnitElectricCurrent (Id NSUnitElectricCurrent) where
  toNSUnitElectricCurrent = unsafeCastId

instance IsNSDimension (Id NSUnitElectricCurrent) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitElectricCurrent) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitElectricCurrent) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitElectricPotentialDifference ----------

-- | Phantom type for @NSUnitElectricPotentialDifference@.
data NSUnitElectricPotentialDifference

instance IsObjCObject (Id NSUnitElectricPotentialDifference) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitElectricPotentialDifference"

class IsNSDimension a => IsNSUnitElectricPotentialDifference a where
  toNSUnitElectricPotentialDifference :: a -> Id NSUnitElectricPotentialDifference

instance IsNSUnitElectricPotentialDifference (Id NSUnitElectricPotentialDifference) where
  toNSUnitElectricPotentialDifference = unsafeCastId

instance IsNSDimension (Id NSUnitElectricPotentialDifference) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitElectricPotentialDifference) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitElectricPotentialDifference) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitElectricResistance ----------

-- | Phantom type for @NSUnitElectricResistance@.
data NSUnitElectricResistance

instance IsObjCObject (Id NSUnitElectricResistance) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitElectricResistance"

class IsNSDimension a => IsNSUnitElectricResistance a where
  toNSUnitElectricResistance :: a -> Id NSUnitElectricResistance

instance IsNSUnitElectricResistance (Id NSUnitElectricResistance) where
  toNSUnitElectricResistance = unsafeCastId

instance IsNSDimension (Id NSUnitElectricResistance) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitElectricResistance) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitElectricResistance) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitEnergy ----------

-- | Phantom type for @NSUnitEnergy@.
data NSUnitEnergy

instance IsObjCObject (Id NSUnitEnergy) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitEnergy"

class IsNSDimension a => IsNSUnitEnergy a where
  toNSUnitEnergy :: a -> Id NSUnitEnergy

instance IsNSUnitEnergy (Id NSUnitEnergy) where
  toNSUnitEnergy = unsafeCastId

instance IsNSDimension (Id NSUnitEnergy) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitEnergy) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitEnergy) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitFrequency ----------

-- | Phantom type for @NSUnitFrequency@.
data NSUnitFrequency

instance IsObjCObject (Id NSUnitFrequency) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitFrequency"

class IsNSDimension a => IsNSUnitFrequency a where
  toNSUnitFrequency :: a -> Id NSUnitFrequency

instance IsNSUnitFrequency (Id NSUnitFrequency) where
  toNSUnitFrequency = unsafeCastId

instance IsNSDimension (Id NSUnitFrequency) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitFrequency) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitFrequency) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitFuelEfficiency ----------

-- | Phantom type for @NSUnitFuelEfficiency@.
data NSUnitFuelEfficiency

instance IsObjCObject (Id NSUnitFuelEfficiency) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitFuelEfficiency"

class IsNSDimension a => IsNSUnitFuelEfficiency a where
  toNSUnitFuelEfficiency :: a -> Id NSUnitFuelEfficiency

instance IsNSUnitFuelEfficiency (Id NSUnitFuelEfficiency) where
  toNSUnitFuelEfficiency = unsafeCastId

instance IsNSDimension (Id NSUnitFuelEfficiency) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitFuelEfficiency) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitFuelEfficiency) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitIlluminance ----------

-- | Phantom type for @NSUnitIlluminance@.
data NSUnitIlluminance

instance IsObjCObject (Id NSUnitIlluminance) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitIlluminance"

class IsNSDimension a => IsNSUnitIlluminance a where
  toNSUnitIlluminance :: a -> Id NSUnitIlluminance

instance IsNSUnitIlluminance (Id NSUnitIlluminance) where
  toNSUnitIlluminance = unsafeCastId

instance IsNSDimension (Id NSUnitIlluminance) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitIlluminance) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitIlluminance) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitInformationStorage ----------

-- | Phantom type for @NSUnitInformationStorage@.
data NSUnitInformationStorage

instance IsObjCObject (Id NSUnitInformationStorage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitInformationStorage"

class IsNSDimension a => IsNSUnitInformationStorage a where
  toNSUnitInformationStorage :: a -> Id NSUnitInformationStorage

instance IsNSUnitInformationStorage (Id NSUnitInformationStorage) where
  toNSUnitInformationStorage = unsafeCastId

instance IsNSDimension (Id NSUnitInformationStorage) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitInformationStorage) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitInformationStorage) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitLength ----------

-- | Phantom type for @NSUnitLength@.
data NSUnitLength

instance IsObjCObject (Id NSUnitLength) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitLength"

class IsNSDimension a => IsNSUnitLength a where
  toNSUnitLength :: a -> Id NSUnitLength

instance IsNSUnitLength (Id NSUnitLength) where
  toNSUnitLength = unsafeCastId

instance IsNSDimension (Id NSUnitLength) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitLength) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitLength) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitMass ----------

-- | Phantom type for @NSUnitMass@.
data NSUnitMass

instance IsObjCObject (Id NSUnitMass) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitMass"

class IsNSDimension a => IsNSUnitMass a where
  toNSUnitMass :: a -> Id NSUnitMass

instance IsNSUnitMass (Id NSUnitMass) where
  toNSUnitMass = unsafeCastId

instance IsNSDimension (Id NSUnitMass) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitMass) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitMass) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitPower ----------

-- | Phantom type for @NSUnitPower@.
data NSUnitPower

instance IsObjCObject (Id NSUnitPower) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitPower"

class IsNSDimension a => IsNSUnitPower a where
  toNSUnitPower :: a -> Id NSUnitPower

instance IsNSUnitPower (Id NSUnitPower) where
  toNSUnitPower = unsafeCastId

instance IsNSDimension (Id NSUnitPower) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitPower) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitPower) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitPressure ----------

-- | Phantom type for @NSUnitPressure@.
data NSUnitPressure

instance IsObjCObject (Id NSUnitPressure) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitPressure"

class IsNSDimension a => IsNSUnitPressure a where
  toNSUnitPressure :: a -> Id NSUnitPressure

instance IsNSUnitPressure (Id NSUnitPressure) where
  toNSUnitPressure = unsafeCastId

instance IsNSDimension (Id NSUnitPressure) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitPressure) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitPressure) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitSpeed ----------

-- | Phantom type for @NSUnitSpeed@.
data NSUnitSpeed

instance IsObjCObject (Id NSUnitSpeed) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitSpeed"

class IsNSDimension a => IsNSUnitSpeed a where
  toNSUnitSpeed :: a -> Id NSUnitSpeed

instance IsNSUnitSpeed (Id NSUnitSpeed) where
  toNSUnitSpeed = unsafeCastId

instance IsNSDimension (Id NSUnitSpeed) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitSpeed) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitSpeed) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitTemperature ----------

-- | Phantom type for @NSUnitTemperature@.
data NSUnitTemperature

instance IsObjCObject (Id NSUnitTemperature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitTemperature"

class IsNSDimension a => IsNSUnitTemperature a where
  toNSUnitTemperature :: a -> Id NSUnitTemperature

instance IsNSUnitTemperature (Id NSUnitTemperature) where
  toNSUnitTemperature = unsafeCastId

instance IsNSDimension (Id NSUnitTemperature) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitTemperature) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitTemperature) where
  toNSUnit = unsafeCastId

-- ---------- NSUnitVolume ----------

-- | Phantom type for @NSUnitVolume@.
data NSUnitVolume

instance IsObjCObject (Id NSUnitVolume) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUnitVolume"

class IsNSDimension a => IsNSUnitVolume a where
  toNSUnitVolume :: a -> Id NSUnitVolume

instance IsNSUnitVolume (Id NSUnitVolume) where
  toNSUnitVolume = unsafeCastId

instance IsNSDimension (Id NSUnitVolume) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id NSUnitVolume) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id NSUnitVolume) where
  toNSUnit = unsafeCastId

-- ---------- NSDecimalNumber ----------

-- | *************	NSDecimalNumber: the class		**********
-- 
-- Phantom type for @NSDecimalNumber@.
data NSDecimalNumber

instance IsObjCObject (Id NSDecimalNumber) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDecimalNumber"

class IsNSNumber a => IsNSDecimalNumber a where
  toNSDecimalNumber :: a -> Id NSDecimalNumber

instance IsNSDecimalNumber (Id NSDecimalNumber) where
  toNSDecimalNumber = unsafeCastId

instance IsNSNumber (Id NSDecimalNumber) where
  toNSNumber = unsafeCastId

instance IsNSObject (Id NSDecimalNumber) where
  toNSObject = unsafeCastId

instance IsNSValue (Id NSDecimalNumber) where
  toNSValue = unsafeCastId

-- ---------- OS_os_workgroup_interval ----------

-- | Phantom type for @OS_os_workgroup_interval@.
data OS_os_workgroup_interval

instance IsObjCObject (Id OS_os_workgroup_interval) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "OS_os_workgroup_interval"

class IsOS_os_workgroup a => IsOS_os_workgroup_interval a where
  toOS_os_workgroup_interval :: a -> Id OS_os_workgroup_interval

instance IsOS_os_workgroup_interval (Id OS_os_workgroup_interval) where
  toOS_os_workgroup_interval = unsafeCastId

instance IsNSObject (Id OS_os_workgroup_interval) where
  toNSObject = unsafeCastId

instance IsOS_object (Id OS_os_workgroup_interval) where
  toOS_object = unsafeCastId

instance IsOS_os_workgroup (Id OS_os_workgroup_interval) where
  toOS_os_workgroup = unsafeCastId

-- ---------- OS_os_workgroup_parallel ----------

-- | Phantom type for @OS_os_workgroup_parallel@.
data OS_os_workgroup_parallel

instance IsObjCObject (Id OS_os_workgroup_parallel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "OS_os_workgroup_parallel"

class IsOS_os_workgroup a => IsOS_os_workgroup_parallel a where
  toOS_os_workgroup_parallel :: a -> Id OS_os_workgroup_parallel

instance IsOS_os_workgroup_parallel (Id OS_os_workgroup_parallel) where
  toOS_os_workgroup_parallel = unsafeCastId

instance IsNSObject (Id OS_os_workgroup_parallel) where
  toNSObject = unsafeCastId

instance IsOS_object (Id OS_os_workgroup_parallel) where
  toOS_object = unsafeCastId

instance IsOS_os_workgroup (Id OS_os_workgroup_parallel) where
  toOS_os_workgroup = unsafeCastId
