{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @SBElementArray@ is subclass of @NSMutableArray@ that manages collections of related ``SBObject`` objects. For example, when you ask the Finder for a list of disks, or ask iTunes for a list of playlists, you get the result back as an @SBElementArray@ containing Scripting Bridge objects representing those items.
--
-- @SBElementArray@ defines methods beyond those of <doc://com.apple.documentation/documentation/foundation/nsarray> for obtaining individual objects. In addition to <doc://com.apple.documentation/documentation/foundation/nsarray/1417555-objectatindex>, @SBElementArray@ also defines ``SBElementArray/objectWithName:``,  ``SBElementArray/objectWithID:``, and ``SBElementArray/objectAtLocation:``.
--
-- ## Subclassing Notes
--
-- The @SBElementArray@ class is not designed for subclassing.
--
-- Generated bindings for @SBElementArray@.
module ObjC.ScriptingBridge.SBElementArray
  ( SBElementArray
  , IsSBElementArray(..)
  , objectWithName
  , objectWithID
  , objectAtLocation
  , arrayByApplyingSelector
  , arrayByApplyingSelector_withObject
  , get
  , arrayByApplyingSelectorSelector
  , arrayByApplyingSelector_withObjectSelector
  , getSelector
  , objectAtLocationSelector
  , objectWithIDSelector
  , objectWithNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScriptingBridge.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the object in the array with the given name.
--
-- This method is provided as an alternative to <doc://com.apple.documentation/documentation/foundation/nsarray/1417555-objectatindex> for applications where a name is available instead of (or in addition to) an index. A name is generally more stable than an index. For example, it is typically more useful to identify a mailbox in Mail by its name than by its index in the list of mailboxes.
--
-- - Parameters:   - name: The name of one of the receiver's objects.
--
-- - Returns: A reference to the designated object or @nil@ if the object couldn’t be found.
--
-- ObjC selector: @- objectWithName:@
objectWithName :: (IsSBElementArray sbElementArray, IsNSString name) => sbElementArray -> name -> IO RawId
objectWithName sbElementArray name =
  sendMessage sbElementArray objectWithNameSelector (toNSString name)

-- | Returns the object in the array with the given identifier.
--
-- This method is provided as an alternative to <doc://com.apple.documentation/documentation/foundation/nsarray/1417555-objectatindex> for applications where an identifier is available instead of (or in addition to) an index. A unique ID is generally more stable than an index. For example, it may be more useful to identify a contact in Address Book by its identifier (which doesn't change over time) than by its index in the list of contacts (which can change as contacts are added or removed).
--
-- - Parameters:   - identifier: The identifier of one of the receiver's objects.
--
-- - Returns: A reference to the identified object or @nil@ if could not be found.
--
-- ObjC selector: @- objectWithID:@
objectWithID :: IsSBElementArray sbElementArray => sbElementArray -> RawId -> IO RawId
objectWithID sbElementArray identifier =
  sendMessage sbElementArray objectWithIDSelector identifier

-- | Returns the object at the given location in the receiver.
--
-- This method is a generalization of <doc://com.apple.documentation/documentation/foundation/nsarray/1417555-objectatindex> for applications where the "index" is not simply an integer. For example, Finder can specify objects using a <doc://com.apple.documentation/documentation/foundation/nsurl> object as a location. In OSA this is known as "absolute position," a generalization of the notion of “index” in Foundation—it could be an integer, but it doesn't have to be. A single object may even have a number of different "absolute position" values depending on the container.
--
-- - Parameters:   - loc: An object that specifies the absolute position of the object within the array. It can be an integer index, a list of coordinates, a URL, or other determinant. See the discussion for clarification.
--
-- - Returns: A reference to the ``SBObject`` object identified by @loc@ or @nil@ if the object couldn’t be located.
--
-- ObjC selector: @- objectAtLocation:@
objectAtLocation :: IsSBElementArray sbElementArray => sbElementArray -> RawId -> IO RawId
objectAtLocation sbElementArray location =
  sendMessage sbElementArray objectAtLocationSelector location

-- | Returns an array containing the results of sending the specified message to each object in the receiver.
--
-- The method identified by @selector@ must not take any arguments and must return an Objective-C object. It should not have the side effect of modifying the receiving array. The order of the items in the result array corresponds to the order of the items in the original array.
--
-- - Parameters:   - selector: A selector identifying the message to be sent to each object in the array.
--
-- - Returns: A new array containing the results of sending the @selector@ message to each object in the receiver, starting with the first object and continuing through the element array to the last object.
--
-- ObjC selector: @- arrayByApplyingSelector:@
arrayByApplyingSelector :: IsSBElementArray sbElementArray => sbElementArray -> Sel -> IO (Id NSArray)
arrayByApplyingSelector sbElementArray selector =
  sendMessage sbElementArray arrayByApplyingSelectorSelector selector

-- | Returns an array containing the results of sending the specified message to each object in the receiver.
--
-- The method identified by @selector@ must take a single argument—whose value is provided in @argument@—and must return an object. It should not have the side effect of modifying the receiving array. The order of the items in the result array corresponds to the order of the items in the original array.
--
-- - Parameters:   - selector: A selector identifying the message to be sent to each object in the array.
--
-- - argument: The value for the parameter of the message identified by @selector@.
--
-- - Returns: A new array containing the results of sending the @selector@ message to each object in the receiver, starting with the first object and continuing through the element array to the last object.
--
-- ObjC selector: @- arrayByApplyingSelector:withObject:@
arrayByApplyingSelector_withObject :: IsSBElementArray sbElementArray => sbElementArray -> Sel -> RawId -> IO (Id NSArray)
arrayByApplyingSelector_withObject sbElementArray aSelector argument =
  sendMessage sbElementArray arrayByApplyingSelector_withObjectSelector aSelector argument

-- | Forces evaluation of the receiver, causing the real object to be returned immediately.
--
-- This method forces the evaluation of the current object reference (the receiver), resulting in the return of the referenced object. By default, Scripting Bridge deals with references to objects until you actually request some concrete data from them or until you call the @get@ method.
--
-- - Returns: The object referenced by the receiver.
--
-- ObjC selector: @- get@
get :: IsSBElementArray sbElementArray => sbElementArray -> IO (Id NSArray)
get sbElementArray =
  sendMessage sbElementArray getSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectWithName:@
objectWithNameSelector :: Selector '[Id NSString] RawId
objectWithNameSelector = mkSelector "objectWithName:"

-- | @Selector@ for @objectWithID:@
objectWithIDSelector :: Selector '[RawId] RawId
objectWithIDSelector = mkSelector "objectWithID:"

-- | @Selector@ for @objectAtLocation:@
objectAtLocationSelector :: Selector '[RawId] RawId
objectAtLocationSelector = mkSelector "objectAtLocation:"

-- | @Selector@ for @arrayByApplyingSelector:@
arrayByApplyingSelectorSelector :: Selector '[Sel] (Id NSArray)
arrayByApplyingSelectorSelector = mkSelector "arrayByApplyingSelector:"

-- | @Selector@ for @arrayByApplyingSelector:withObject:@
arrayByApplyingSelector_withObjectSelector :: Selector '[Sel, RawId] (Id NSArray)
arrayByApplyingSelector_withObjectSelector = mkSelector "arrayByApplyingSelector:withObject:"

-- | @Selector@ for @get@
getSelector :: Selector '[] (Id NSArray)
getSelector = mkSelector "get"

