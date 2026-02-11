{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ScriptingBridge.Internal.Classes (
    module ObjC.ScriptingBridge.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- SBObject ----------

-- | The @SBObject@ class declares methods that can be invoked on any object in a scriptable application. It defines methods for getting elements and properties of an object, as well as setting a given object to a new value.
--
-- Each @SBObject@ is built around an object specifier, which tells Scripting Bridge how to locate the object. Therefore, you can think of an @SBObject@ as a reference to an object in an target application rather than an object itself. To bypass this reference-based approach and force evaluation, use the ``SBObject/get`` method.
--
-- Typically, rather than create @SBObject@ instances explictly, you receive @SBObject@ objects by calling methods of an ``SBApplication`` subclass. For example, if you wanted to get an @SBObject@ representing the current iTunes track, you would use code like this (where @iTunesTrack@ is a subclass of @SBObject@):
--
-- ```objc iTunesApplication *iTunes = [SBApplication applicationWithBundleIdentifier:"com.apple.iTunes"]; iTunesTrack *track = [iTunes currentTrack]; ```
--
-- You can discover the names of dynamically generated classes such as @iTunesApplication@ and @iTunesTrack@ by examining the header file created by the @sdp@ tool. Alternatively, you give these variables the dynamic Objective-C type @id@.
-- 
-- Phantom type for @SBObject@.
data SBObject

instance IsObjCObject (Id SBObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SBObject"

class IsNSObject a => IsSBObject a where
  toSBObject :: a -> Id SBObject

instance IsSBObject (Id SBObject) where
  toSBObject = unsafeCastId

instance IsNSObject (Id SBObject) where
  toNSObject = unsafeCastId

-- ---------- SBApplication ----------

-- | The @SBApplication@ class provides a mechanism enabling an Objective-C program to send Apple events to a scriptable application and receive Apple events in response. It thereby makes it possible for that program to control the application and exchange data with it. Scripting Bridge works by bridging data types between Apple event descriptors and Cocoa objects.
--
-- Although @SBApplication@ includes methods that manually send and process Apple events, you should never have to call these methods directly. Instead, subclasses of @SBApplication@ implement application-specific methods that handle the sending of Apple events automatically.
--
-- For example, if you wanted to get the current iTunes track, you can simply use the @currentTrack@ method of the dynamically defined subclass for the iTunes application—which handles the details of sending the Apple event for you—rather than figuring out the more complicated, low-level alternative:
--
-- ```objc [iTunes propertyWithCode:'pTrk']; ```
--
-- If you do need to send Apple events manually, consider using the @NSAppleEventDescriptor@ class.
--
-- ## Subclassing Notes
--
-- You rarely instantiate @SBApplication@ objects directly. Instead, you get the shared instance of a application-specific subclass typically by calling one of the @applicationWith...@ class methods, using a bundle identifier, process identifier, or URL to identify the application.
-- 
-- Phantom type for @SBApplication@.
data SBApplication

instance IsObjCObject (Id SBApplication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SBApplication"

class IsSBObject a => IsSBApplication a where
  toSBApplication :: a -> Id SBApplication

instance IsSBApplication (Id SBApplication) where
  toSBApplication = unsafeCastId

instance IsNSObject (Id SBApplication) where
  toNSObject = unsafeCastId

instance IsSBObject (Id SBApplication) where
  toSBObject = unsafeCastId

-- ---------- SBElementArray ----------

-- | @SBElementArray@ is subclass of @NSMutableArray@ that manages collections of related ``SBObject`` objects. For example, when you ask the Finder for a list of disks, or ask iTunes for a list of playlists, you get the result back as an @SBElementArray@ containing Scripting Bridge objects representing those items.
--
-- @SBElementArray@ defines methods beyond those of <doc://com.apple.documentation/documentation/foundation/nsarray> for obtaining individual objects. In addition to <doc://com.apple.documentation/documentation/foundation/nsarray/1417555-objectatindex>, @SBElementArray@ also defines ``SBElementArray/objectWithName:``,  ``SBElementArray/objectWithID:``, and ``SBElementArray/objectAtLocation:``.
--
-- ## Subclassing Notes
--
-- The @SBElementArray@ class is not designed for subclassing.
-- 
-- Phantom type for @SBElementArray@.
data SBElementArray

instance IsObjCObject (Id SBElementArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SBElementArray"

class IsNSMutableArray a => IsSBElementArray a where
  toSBElementArray :: a -> Id SBElementArray

instance IsSBElementArray (Id SBElementArray) where
  toSBElementArray = unsafeCastId

instance IsNSArray (Id SBElementArray) where
  toNSArray = unsafeCastId

instance IsNSMutableArray (Id SBElementArray) where
  toNSMutableArray = unsafeCastId

instance IsNSObject (Id SBElementArray) where
  toNSObject = unsafeCastId
